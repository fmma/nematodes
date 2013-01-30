function [sig,yfp,cfp] = calcium_process(thresh, bthresh, radius, im, refframe)
%
% calcium imaging processing code
%
% This routine implements the method described in the WormBook
% found here in section 3.7, protocol 4:
%
%   http://www.wormbook.org/chapters/www_imagingneurons/imagingneurons.html
%
% Extraction of the cells of interest and the non-fluorescent background
% is based on crude threshold segmentation.  Two thresholds are required:
% one for the high end to define bright regions representing cells of
% interest, and one for the low end to define dim regions representing
% background.  We are assuming that the frame is split down the middle
% and rely on a helper function (splitter.m) to do the frame splitting
% and registration of the halves to correct for distortions that result
% from the imaging aparatus.  The distortions are assumed to be
% nothing more than a composition of translations and rotations, with
% length scales being unmodified.  A radius is provided for a circle
% representing the region of interest around the identified bright cells.
%
% input:
%   thresh  : threshold for high end, where pixels above the threshold
%             are considered part of fluorescing cells of interest.
%   bthresh : threshold for low end, where pixels below the threshold
%             are considered background and non-fluorescent.
%   radius  : radius of circle centered on the centroid of the cell
%             of interest to measure.
%   im      : sequence of images as a cell array
%   refframe: reference frame
%
% output:
%   sig     : the signal obtained by computing the ratio of the yellow
%             channel over the cyan channel, with corresponding backgrounds
%             subtracted.
%   yfp     : the signal from the yellow channel with background removed
%   cfp     : the signal from the cyan channel with background removed
%
% Matthew Sottile / November 2012
% mjsottile@gmail.com
%

    % signal to return
    sig = zeros(1,length(im));

    % signal for each side individually
    yfp = zeros(1,length(im));
    cfp = zeros(1,length(im));
    
    % register and reconstruct transform
    [~,~,tform] = splitter(double(im{refframe}));
%    [theta,offsetx,offsety] = reconstruct_transform(rhs);
    
    % assemble transform matrix.  do this in matrix form
    % so we can possibly use the matlab affine transform
    % function that takes a matrix as input in the future.
%     mtx_rotate = [cos(theta),  sin(theta), 0; ...
%                   -sin(theta), cos(theta), 0; ...
%                   0          ,          0, 1];
%               
%     mtx_trans = [1, 0, -offsetx; ...
%                  0, 1, -offsety; ...
%                  0, 0,       1];
%              
%     % rotate then translate
%     transform_matrix = mtx_trans * mtx_rotate;
    
    for i=1:length(im)
        [lhs,rhs] = splitter2(im{i},tform);
%        [~,ncols] = size(im{i});
%        lhs = double(im{i}(:,1:(ncols/2)));
%        rhs = double(im{i}(:,(ncols/2)+1:end));
        
        % maximum intensity
        maxval = max(lhs(:));

        % binary image with all pixels within thresh of the max intensity
        BWmax = lhs>(maxval-thresh);

        % compute connected components of thresholded regions
        CCmax = bwconncomp(BWmax);

        % compute centroid of each CC
        Smax = regionprops(CCmax,'Centroid');

        % if we had more than one CC, pick the biggest
        if (length(Smax) > 1)
            largest_max = 0;
            largest_max_size = 0;
            for j=1:length(CCmax.PixelIdxList)
                len = length(CCmax.PixelIdxList(j));
                if (len > largest_max_size)
                    largest_max_size = len;
                    largest_max = j;
                end
            end
        else
            % if we only had 1, then index 1 is the biggest
            largest_max = 1;
        end
        
        % find all pixels that are within bthresh of the minimum
        % intensity to define the background
        minval = min(lhs(:));
        background_mask = lhs < (minval + bthresh);
        lhs_background = lhs .* double(background_mask);
        rhs_background = rhs .* double(background_mask);
        
        % compute y_bkg and c_bkg as average value of pixels determined
        % to be within what we call the background region
        ybkg = sum(lhs_background(:))/length(find(background_mask));
        cbkg = sum(rhs_background(:))/length(find(background_mask));

        % make the circle mask.  
        [size_rows,size_cols] = size(lhs);
        [nc,nr] = ndgrid(1:size_rows,1:size_cols);
        lhs_circlemask_max = sqrt( ...
                          (nc-Smax(largest_max).Centroid(2)).^2 + ...
                          (nr-Smax(largest_max).Centroid(1)).^2) ...
                         < radius;
        
%         % make a new circle centered at a position that is offset and
%         % rotated based on the transformation we computed from the
%         % splitter results
%         rcx = Smax(largest_max).Centroid(1);
%         rcy = Smax(largest_max).Centroid(2);
%         
%         %%%%%% TODO: make sure to put (0,0) at center of image, not a
%         %%%%%% corner.
%         origx = rcx;
%         origy = rcy;
%         
%         half_size_rows = size_rows/2;
%         half_size_cols = size_cols/2;
%         
%         % shift origin to middle of region
%         rcx = rcx - half_size_cols;
%         rcy = rcy - half_size_rows;
%         
%         newcoords = transform_matrix * [rcx; rcy; 0];
%         
%         rcx = newcoords(1) + half_size_cols;
%         rcy = newcoords(2) + half_size_rows;
%         
%         % make the circle mask for the RHS using the transformed
%         % center.
%         rhs_circlemask_max = sqrt( ...
%                           (nc-rcy).^2 + ...
%                           (nr-rcx).^2) ...
%                          < radius;

        rhs_circlemask_max = lhs_circlemask_max;
        
        % mask the halves
        lhs_masked = lhs.*double(lhs_circlemask_max);
        rhs_masked = rhs.*double(rhs_circlemask_max);

        % count masked pixels in each side
        lhs_nnz = length(find(lhs_masked(:) > 0));
        rhs_nnz = length(find(rhs_masked(:) > 0));

        % do R computation.  Note that we take the mean of only the masked 
        % pixels to get the average intensity.  Subtract from each the
        % average of the background pixels determined above.
        yfp(i) = (sum(lhs_masked(:))/lhs_nnz) - ybkg;
        cfp(i) = (sum(rhs_masked(:))/rhs_nnz) - cbkg;
        sig(i) = yfp(i)/cfp(i);
            
        % correct for R_CFP.  Paper recommends value of 0.6 if it wasn't
        % measured directly.  NOTE: we are not doing the f_bkg correction,
        % which is necessary to remove signal due to background noise versus
        % actual flourescence from the cell.
        sig(i) = sig(i)-0.6;

        % plotting stuff

        % LHS plot
        title(sprintf('%d',i));
        subplot(1,3,1);
        imagesc(lhs_masked);
        colormap gray;
        hold on;
        plot(Smax(largest_max).Centroid(1),Smax(largest_max).Centroid(2),'r+');
        hold off;

        % RHS plot    
        subplot(1,3,2);
        imagesc(rhs_masked);
        colormap gray;
        hold on;
        plot(Smax(largest_max).Centroid(1),Smax(largest_max).Centroid(2),'r+');
% 
%         plot(rcx,rcy,'r+');
%         plot(origx,origy,'g+');
        hold off;

        % signal so far
        subplot(1,3,3);
        plot(sig);

        drawnow;
    end
