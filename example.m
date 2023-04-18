
% -------------------------------------------------------------------------
% SYNTAX:
% -------------------------------------------------------------------------
%
%
%     [STATE, DATACELL] = READUNV(FILEPATH, SKIP_DATA)
%
%     NB: ALL ARGUMENTS ARE REQUIRED (TWO OUTPUTS AND ONE INPUT)
%
%
%     FILEPATH:   [CHAR] RELATIVE OR ABSOLUTE PATH TO FILE TO READ
%
%     SKIP_DATA:  [DOUBLE] ONLY READ HEADER, OPTIONAL (DEFAULT IS 0)
%
%     DATACELL:   [CELL]/[CHAR] CELL ARRAY FOR EACH DATASET IN FILE
%                 IF NOT SUCCESSFUL -> CHAR WITH ERROR DESCRIPTION
%
%     STATE FLAGS:
%       =  1,  SUCCESSFUL
%       =  0,  UNKNOWN ERROR
%       = -1,  EMPTY INPUT
%       = -2,  COULD NOT READ FILEPATH
%       = -3,  FILE DOES NOT EXIST
%       = -4,  TOO FEW INPUTS
%       = -5,  FILEPATH IS NOT A CHAR
%       = -6,  FILE IS ILL-FORMED
%       = -7,  TWO OUTPUT ARGUMENTS ARE REQUIRED
%       = -8,  DEPRECATED, AND NOT THROWN ANY MORE
%       = -9,  ERROR WHILE READING FILE
%       = -12, FLOATING POINT ARITHMETIC OVERFLOW (FROM IOSTAT)
%       = >1,  ERROR CODE FROM IOSTAT WHEN OPENING FILE
%
%
% ----------------------------------------------------------------------
%
%
%     [STATE CASENUM] = WRITEUNV(FILEPATH, STRUCT, WRITEMODE, IDX)
%
%
% --- SYNTAX EXAMPLES --------------------------------------------------
%
%       WRITEUNV(FILEPATH, STRUCT)
%         APPEND DATA (FROM STRUCT), CREATE FILE IF NEEDED
%
%       WRITEUNV(FILEPATH, STRUCT, WRITEMODE)
%         WRITEMODE = 0 (APPEND), WRITEMODE = 1 (REPLACE), [WRITEMODE] = DOUBLE
%
%       WRITEUNV(FILEPATH, STRUCT, 0/1, IDX)
%         CREATE FILE ACCORDING TO WRITEMODE, KEEP FILE OPEN, CURRENT FILE = IDX (IDX MUST <= MAXIDX)
%
%       WRITEUNV([], STRUCT, 2, IDX)
%         WRITE INTO LUN(IDX), FILEPATH IS IGNORED
%
%       WRITEUNV(CASENUM, DATA, 3, IDX)
%         STREAM DATA INTO <IDX>, FILEPATH IS REPLACED WITH CASENUM FOR DATA WRITING, ONLY UNV-58 SUPPORTED/ASSUMED
%
%       WRITEUNV(-1, DATA, 3, IDX)
%         TERMINATE FILE <IDX>, AND WRITE NUMBER OF VALUES INTO HEADER, DATA IS IGNORED
%
%       WRITEUNV()
%         CLOSE ALL OPEN FILES AND ZERO LUN/FILEHANDLES
%
%
% --- INPUTS --------------------------------------------------------------
%
%    FILEPATH:    [CHAR] WITH RELATIVE OR ABSOLUTE PATH TO FILE TO READ
%                 REPLACE FILEPATH WITH CASENUM [DOUBLE] WHEN WRITING STREAMING DATA
%                 SET TO -1 [DOUBLE] TO TERMINATE FILE <IDX>
%
%     STRUCT:     [STRUCT] DATA TO BE WRITTEN
%
%     WRITEMODE:  [DOUBLE] REPLACE (1), APPEND (0) OR ADD TO OPEN FILEHANDLE/LUN (2)
%
%     IDX:        [DOUBLE] FILE INDEX TO FILEHANDLE, EITHER A NEW (TO OPEN), OR OLD ALREADY OPENED
%
%
% --- OUTPUTS -------------------------------------------------------------
%
%     STATE FLAGS: (ALWAYS RETURNED)
%       =  1,  SUCCESFUL
%       =  0,  UNKNOWN ERROR
%       = -1,  ERROR WRITING FIELDS/DATA
%       = -2,  COULD NOT READ FILEPATH
%       = -3,  UNKNOWN WRITEMODE
%       = -4,  TOO FEW INPUTS
%       = -7,  EXPECTED STRUCT INPUT IS NOT A STRUCTURE
%       = -9,  FILE LUN IS NOT OPEN
%       = -10, DATA IS NOT AN ARRAY, OR WRONGLY SIZED
%       = -11, DATA CASE TYPE IS UNSUPPORTED
%       = -12, FLOATING POINT ARITHMETIC OVERFLOW (FROM IOSTAT)
%       = -13, FILEPATH IS INVALID OR INACCESSIBLE
%       = -14, "dataFormatType" IS INVALID (STREAMING MODE)
%       = -15, X-DATA FIELD NOT PRESENT OR EMPTY
%       = -16, MISMATCHING X-DATA- AND DATA SIZES
%       = -17, "ordNumDataType" IS INVALID
%       = -18, INVALID "fileType" IN UNV-151
%       = -19, INVALID "unitsCode" IN UNV-164
%       = -20, INVALID DIRECTION NUMBER IN UNV-58, VALID RANGE: [-6 6]
%       = >1,  ERROR CODE FROM IOSTAT WHEN OPENING FILE
%
%     CASENUM:  CASENUM TO USE WHEN STREAMING DATA [DOUBLE]
%               IF NOT SUCCESSFUL -> CHAR WITH ERROR DESCRIPTION [CHAR]
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



% test to write a file from scratch
C = createTemplate(58);
C.data = rand(3, 1);
C.x = 1:3;
C.evenSpacing = 1;
C.x0 = 1;
C.dx = 1;
success = writeunv('test_template.unv', C, 1);



% write all data to new file, but keep file open between calls
idx = 7; % index number to use between calls, in order to keep multiple files open simultaneously, any number between 1 and 512 (MAXIDX)
         % include idx to keep file open
for setCount = 1:length(datacell)
    if setCount == 1
        writeAction = 1; % replace file on first call
    else
        writeAction = 2; % add to open file
    end
    [success, errstr] = writeunv(['.', filesep, 'example2.unv'], datacell{setCount}, writeAction, idx);
    if success ~= 1
        fprintf('2: set %i failed\n', setCount)
        fprintf('2: error code %i\n', success)
        disp(errstr)
        break
    end
end
writeunv(); % close all files


% single-precision streaming
for stp = 1:42 % try streaming with different step sizes

    % write header data to new file, but keep file open between calls
    % add data (58) from one set in chunks (streaming) and terminate when done
    idx = 5; % index number to use between calls, in order to keep multiple files open simultaneously, any number between 1 and 512 (MAXIDX)
    filepath = ['.', filesep, 'example3.unv'];
    writeunv(filepath, datacell{1}, 1, idx); % 151 header
    writeunv([], datacell{2}, 2, idx); % 164 header
    S = datacell{3};
    S.numValues = 0; % remove correct value, for demonstration purposes
    data = S.data;
    S = rmfield(S, 'data'); % remove data
    [success, casenum] = writeunv([], S, 2, idx); % 58 header
    L = size(data, 1);
    
    for ii = 1:ceil(L/stp)
        dataToWrite = data(1+(ii-1)*stp:min(ii*stp, end)); 
        y = writeunv(casenum, dataToWrite, 3, idx);
    end
    writeunv(-1, [], 3, idx); % terminate file
    % close all files is not needed here
    

    fid = fopen(filepath, 'rb');
    fseek(fid, 0, 'eof');
    fileSize = ftell(fid);
    frewind(fid);
    data = fread(fid, fileSize, 'uint8');
    numLines = sum(data == 10) + 1; % check number of lines
    disp([stp, numLines])
    fclose(fid);

end


% double-precision streaming
for stp = 1:21 % try streaming with different step sizes

    % write header data to new file, but keep file open between calls
    % add data (58) from one set in chunks (streaming) and terminate when done
    idx = 5; % index number to use between calls, in order to keep multiple files open simultaneously, any number between 1 and 512 (MAXIDX)
    filepath = ['.', filesep, 'example4.unv'];
    writeunv(filepath, datacell{1}, 1, idx); % 151 header
    writeunv([], datacell{2}, 2, idx); % 164 header
    S = datacell{3};
    S.numValues = 0; % remove correct value, for demonstration purposes
    data = double(S.data);
    S.dataFormatType = 4; % 4 = double, 2 = single | required since no data is sent when writing header
    S.precision = 'DOUBLE'; % not needed
    S = rmfield(S, 'data'); % remove data
    [success, casenum] = writeunv([], S, 2, idx); % 58 header
    L = size(data, 1);
    
    for ii = 1:ceil(L/stp)
        dataToWrite = data(1+(ii-1)*stp:min(ii*stp, end)); 
        y = writeunv(casenum, dataToWrite, 3, idx);
    end
    writeunv(-1, [], 3, idx); % terminate file
    % close all files is not needed here
    
    
    fid = fopen(filepath, 'rb');
    fseek(fid, 0, 'eof');
    fileSize = ftell(fid);
    frewind(fid);
    data = fread(fid, fileSize, 'uint8');
    numLines = sum(data == 10) + 1; % check number of lines
    disp([stp, numLines])
    fclose(fid);

end

