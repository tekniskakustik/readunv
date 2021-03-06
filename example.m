
% -------------------------------------------------------------------------
% SYNTAX:
% -------------------------------------------------------------------------
%
%
%      [STATE, DATACELL] = READUNV(FILEPATH)
%
%      NB: ALL ARGUMENTS ARE REQUIRED (TWO OUTPUTS AND ONE INPUT)
%
%
%      FILEPATH: CHAR WITH RELATIVE OR ABSOLUTE PATH TO FILE TO READ
%
%      DATACELL: CELL ARRAY FOR EACH DATASET IN FILE
%
%      STATE FLAGS:
%        =  1,  SUCCESFUL
%        =  0,  UNKNOWN ERROR
%        = -1,  EMPTY INPUT
%        = -2,  COULD NOT READ FILEPATH
%        = -3,  FILE DOES NOT EXIST
%        = -4,  TOO FEW INPUTS
%        = -5,  FILEPATH IS NOT A CHAR
%        = -6,  FILE IS ILL-FORMATTED
%        = -7,  TWO OUTPUT ARGUMENTS ARE REQUIRED
%        = -12, FLOATING POINT ARITHMETIC OVERFLOW (FROM IOSTAT)
%        = >1,  ERROR CODE FROM IOSTAT WHEN OPENING FILE
%
%
% -------------------------------------------------------------------------
%
%
%      STATE = WRITEUNV(FILEPATH, STRUCT, WRITEMODE, IDX)
% 
%      WRITEUNV(FILEPATH, STRUCT) -> APPEND DATA (FROM STRUCT), CREATE FILE IF NEEDED
%      WRITEUNV(FILEPATH, STRUCT, WRITEMODE) | WRITEMODE = 0 (APPEND), WRITEMODE = 1 (REPLACE), [WRITEMODE] = UINT8
%      WRITEUNV(FILEPATH, STRUCT, 0/1, IDX) -> CREATE FILE ACCORDING TO WRITEMODE, KEEP FILE OPEN, CURRENT FILE = IDX (IDX MUST <= MAXIDX)
%      WRITEUNV([], STRUCT, 2, IDX) -> WRITE INTO LUN(IDX), FILEPATH IS IGNORED
%      WRITEUNV(CASENUM, DATA, 3, IDX, DATASET) -> STREAM DATA INTO <IDX>, FILEPATH IS REPLACED WITH CASENUM FOR DATA WRITING
%      WRITEUNV(-1, DATA, 3, IDX, DATASET) -> TERMINATE FILE <IDX>, AND WRITE NUMBER OF VALUES INTO HEADER
%      WRITEUNV() -> CLOSE ALL OPEN FILES AND ZERO LUN
%      <STATE IS ALWAYS RETURNED>
%
%      FILEPATH: CHAR WITH RELATIVE OR ABSOLUTE PATH TO FILE TO READ
%
%      STATE FLAGS:
%        =  1,  SUCCESFUL
%        =  0,  UNKNOWN ERROR
%        = -1,  ERROR WRITING FIELDS/DATA
%        = -2,  COULD NOT READ FILEPATH
%        = -3,  UNKNOWN WRITEMODE
%        = -4,  TOO FEW INPUTS
%        = -5,  MISSING DATASET NUMBER
%        = -6,  UNSUPPORTED DATASET NUMBER
%        = -7,  EXPECTED STRUCT INPUT IS NOT A STRUCTURE
%        = -8,  TOO FEW FIELDS, OR BADLY FORMATTED FIELDS, IN STRUCTURE
%        = -9,  FILE LUN IS NOT OPEN
%        = -10, DATA IS NOT AN ARRAY, OR WRONGLY SIZED
%        = -11, DATA CASE TYPE IS UNSUPPORTED
%        = -12, FLOATING POINT ARITHMETIC OVERFLOW (FROM IOSTAT)
%        = >1,  ERROR CODE FROM IOSTAT WHEN OPENING FILE
%
%
% -------------------------------------------------------------------------
% -------------------------------------------------------------------------



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
for ii = 1:ceil(L/stp)
    dataToWrite = data(1+(ii-1)*stp:min(ii*stp, end));
    writeunv(casenum, dataToWrite, 3, idx, 58);
end
writeunv(-1, [], 3, idx, 58); % terminate file
% close all files is not needed here


