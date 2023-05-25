%% bring in the first frame from the file

v = FlirMovieReader('sample-faceNotCovered.ptw')
[frame, metadata] = step(v);
frame1 = im2double(frame);
frame2 = imadjust(frame1);
figure, imshow(frame2); % see why here is a colon and not semicolon (does it cause sth different?)

[height, width] = size(frame2)
position_L = [0, 0, (width/2), height]
position_R = [(width/2), 0, width, height]

position = position_L

%%%%%%%%%%%%%%%%%%%% selecting half of the image
imshow(frame2)
frame3 = imcrop(frame2,[position(:,1) position(:,2) position(:,3) position(:,4)])
imshow(frame3)
%%%%%%%%%%%%%%%%%%%%

%% detect a face on the first frame

faceDetector = vision.CascadeObjectDetector;
bbox = step(faceDetector, frame3);
dispFrame = insertObjectAnnotation(frame3, 'rectangle', bbox, 'ChrisB');
figure, imshow(dispFrame)

%% Detection and Tracking
% Capture and process video frames from the video in a loop to detect and
% track a face. The loop will run until the video player
% window is closed.
% setup video player
videoPlayer = vision.DeployableVideoPlayer('Location', [100 100]);
videoPlayer.FrameRate = 60;
videoPlayer.Size = 'Full-screen';


pointTracker = vision.PointTracker('MaxBidirectionalError', 2);

runLoop = true;
numPts = 0;
frameCount = 0;

while ~isDone(v)
    
    % Get the next frame.
    videoFrame = im2double(step(v));
    videoFrame = imadjust(videoFrame);
    videoFrameGray = videoFrame;
    frameCount = frameCount + 1;
    
    if numPts < 10
        % Detection mode.
        bbox = faceDetector.step(videoFrameGray);
        
        if ~isempty(bbox)
            % Find corner points inside the detected region.
            points = detectMinEigenFeatures(videoFrameGray, 'ROI', bbox(1, :));
            
            % Re-initialize the point tracker.
            xyPoints = points.Location;
            numPts = size(xyPoints,1);
            release(pointTracker);
            initialize(pointTracker, xyPoints, videoFrameGray);
            
            % Save a copy of the points.
            oldPoints = xyPoints;
            
            % Convert the rectangle represented as [x, y, w, h] into an
            % M-by-2 matrix of [x,y] coordinates of the four corners. This
            % is needed to be able to transform the bounding box to display
            % the orientation of the face.
            bboxPoints = bbox2points(bbox(1, :));  
            
            % Convert the box corners into the [x1 y1 x2 y2 x3 y3 x4 y4] 
            % format required by insertShape.
            bboxPolygon = reshape(bboxPoints', 1, []);
            
            % Display a bounding box around the detected face.
            videoFrame = insertShape(videoFrame, 'Polygon', bboxPolygon, 'LineWidth', 3);
            
            % Display detected corners.
            videoFrame = insertMarker(videoFrame, xyPoints, '+', 'Color', 'white');
        end
        
    else
        % Tracking mode.
        [xyPoints, isFound] = step(pointTracker, videoFrameGray);
        visiblePoints = xyPoints(isFound, :);
        oldInliers = oldPoints(isFound, :);
                
        numPts = size(visiblePoints, 1);       
        
        if numPts >= 10
            % Estimate the geometric transformation between the old points
            % and the new points.
            [xform, oldInliers, visiblePoints] = estimateGeometricTransform(...
                oldInliers, visiblePoints, 'similarity', 'MaxDistance', 4);            
            
            % Apply the transformation to the bounding box.
            bboxPoints = transformPointsForward(xform, bboxPoints);
            
            % Convert the box corners into the [x1 y1 x2 y2 x3 y3 x4 y4] 
            % format required by insertShape.
            bboxPolygon = reshape(bboxPoints', 1, []);            
            
            % Display a bounding box around the face being tracked.
            videoFrame = insertShape(videoFrame, 'Polygon', bboxPolygon, 'LineWidth', 3);
            
            % Display tracked points.
            videoFrame = insertMarker(videoFrame, visiblePoints, '+', 'Color', 'white');
            
            % Reset the points.
            oldPoints = visiblePoints;
            setPoints(pointTracker, oldPoints);
        end

    end
        
    % Display the annotated video frame using the video player object.
    step(videoPlayer, videoFrame);

    % Check whether the video player window has been closed.
    runLoop = isOpen(videoPlayer);
end
