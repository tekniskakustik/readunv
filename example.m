

% read data from a test file
[success, datacell] = readunv(['.', filesep, 'testdata', filesep, 'test3.unv']); %#ok<ASGLU> 



% write all data to new file
for setCount = 1:length(datacell)
    if setCount == 1
        writeAction = 1; % replace (default if nargin == 2)
    else
        writeAction = 0; % append
    end
    success = writeunv(['.', filesep, 'example1.unv'], datacell{setCount}, writeAction);
    if ~success
        fprintf('1: set %i failed\n', setCount)
        fprintf('1: error code %i\n', success)
        break
    end
end



% write all data to new file, but keep file open between calls
idx = 3; % index number to use between calls, in order to keep multiple files open simultaneously, any number between 1 and 512 (MAXIDX)
         % include idx to keep file open
for setCount = 1:length(datacell)
    if setCount == 1
        writeAction = 1; % replace file on first call
    else
        writeAction = 2; % add to open file
    end
    success = writeunv(['.', filesep, 'example2.unv'], datacell{setCount}, writeAction, idx);
    if success ~= 1
        fprintf('2: set %i failed\n', setCount)
        fprintf('2: error code %i\n', success)
        break
    end
end
writeunv(); % close all files



% write header data to new file, but keep file open between calls
% add data (58) from one set in chunks (streaming) and terminate when done
idx = 4; % index number to use between calls, in order to keep multiple files open simultaneously, any number between 1 and 512 (MAXIDX)
filepath = ['.', filesep, 'example3.unv'];
writeunv(filepath, datacell{1}, 1, idx); % 151 header
writeunv([], datacell{2}, 2, idx); % 164 header
S = datacell{3};
S.numValues = 0; % remove correct value, for demonstration purposes
data = S.data;
S = rmfield(S, 'data'); % remove data
[success, casenum] = writeunv([], S, 2, idx); % 58 header
L = size(data, 1);
stp = 10;
for ii = 1:floor(L/stp)
    dataToWrite = data(1+(ii-1)*stp:min(ii*stp, end));
    writeunv(casenum, dataToWrite, 3, idx, 58);
end
writeunv(-1, [], 3, idx, 58); % terminate file
% close all files is not needed here


