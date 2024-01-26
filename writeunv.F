#include "fintrf.h"
C
C
C ----------------------------------------------------------------------
C
C     [STATE, CASENUM] = WRITEUNV(FILEPATH, STRUCT, WRITEMODE, IDX)
C
C ----------------------------------------------------------------------
C
C
C --- SYNTAX EXAMPLES --------------------------------------------------
C
C       WRITEUNV(FILEPATH, STRUCT)
C         APPEND DATA (FROM STRUCT), CREATE FILE IF NEEDED
C
C       WRITEUNV(FILEPATH, STRUCT, WRITEMODE)
C         WRITEMODE = 0 (APPEND), WRITEMODE = 1 (REPLACE), [WRITEMODE] = DOUBLE
C
C       WRITEUNV(FILEPATH, STRUCT, 0/1, IDX)
C         CREATE FILE ACCORDING TO WRITEMODE, KEEP FILE OPEN, CURRENT FILE = IDX (IDX MUST <= MAXIDX)
C
C       WRITEUNV([], STRUCT, 2, IDX)
C         WRITE INTO LUN(IDX), FILEPATH IS IGNORED
C
C       WRITEUNV(CASENUM, DATA, 3, IDX)
C         STREAM DATA INTO <IDX>, FILEPATH IS REPLACED WITH CASENUM FOR DATA WRITING, ONLY UNV-58 SUPPORTED/ASSUMED
C
C       WRITEUNV(-1, DATA, 3, IDX)
C         TERMINATE FILE <IDX>, AND WRITE NUMBER OF VALUES INTO HEADER, DATA IS IGNORED
C
C       WRITEUNV()
C         CLOSE ALL OPEN FILES AND ZERO LUN/FILEHANDLES
C
C
C --- INPUTS -----------------------------------------------------------
C
C     FILEPATH:   [CHAR] WITH RELATIVE OR ABSOLUTE PATH TO FILE TO READ
C                 REPLACE FILEPATH WITH CASENUM [DOUBLE] WHEN WRITING STREAMING DATA
C                 SET TO -1 [DOUBLE] TO TERMINATE FILE <IDX>
C
C     STRUCT:     [STRUCT] DATA TO BE WRITTEN
C
C     WRITEMODE:  [DOUBLE] REPLACE (1), APPEND (0) OR ADD TO OPEN FILEHANDLE/LUN (2)
C
C     IDX:        [DOUBLE] FILE INDEX TO FILEHANDLE, EITHER A NEW (TO OPEN), OR OLD ALREADY OPENED
C
C
C --- OUTPUTS ----------------------------------------------------------
C
C     STATE FLAGS: (ALWAYS RETURNED)
C       =  1,  SUCCESFUL
C       =  0,  UNKNOWN ERROR
C       = -1,  ERROR WRITING FIELDS/DATA
C       = -2,  COULD NOT READ FILEPATH
C       = -3,  UNKNOWN WRITEMODE
C       = -4,  TOO FEW INPUTS
C       = -5,  "datasetType" IS INVALID OR MISSING
C       = -6,  UNSUPPORTED DATASET
C       = -7,  EXPECTED STRUCT INPUT IS NOT A STRUCTURE
C       = -9,  FILE LUN IS NOT OPEN
C       = -10, DATA IS NOT AN ARRAY, OR WRONGLY SIZED
C       = -11, DATA CASE TYPE IS UNSUPPORTED
C       = -12, FLOATING POINT ARITHMETIC OVERFLOW (FROM IOSTAT)
C       = -13, FILEPATH IS INVALID OR INACCESSIBLE
C       = -14, "dataFormatType" IS INVALID (STREAMING MODE)
C       = -15, X-DATA FIELD NOT PRESENT OR EMPTY
C       = -16, MISMATCHING X-DATA- AND DATA SIZES
C       = -17, "ordNumDataType" IS INVALID
C       = -18, INVALID "fileType" IN UNV-151
C       = -19, INVALID "unitsCode" IN UNV-164
C       = -20, INVALID DIRECTION NUMBER IN UNV-58, VALID RANGE: [-6, 6]
C       = >1,  ERROR CODE FROM IOSTAT WHEN OPENING FILE
C
C     CASENUM:  CASENUM TO USE WHEN STREAMING DATA [DOUBLE]
C               IF NOT SUCCESSFUL -> CHAR WITH ERROR DESCRIPTION [CHAR]
C
C ----------------------------------------------------------------------
C
C
      SUBROUTINE mexFunction(nlhs, plhs, nrhs, prhs)
C
      IMPLICIT NONE
C
C     GATEWAY VARIABLES
      mwPointer :: plhs(*), prhs(*)
      mwSize    :: nlhs, nrhs
C
C     FUNCTIONS
      mwPointer :: mxCreateDoubleScalar
      mwPointer :: mxCreateString
      mwPointer :: mxGetDoubles
      mwPointer :: mxGetSingles
      mwPointer :: mxGetComplexDoubles
      mwPointer :: mxGetComplexSingles
      mwPointer :: mxGetN
      mwPointer :: mxGetNumberOfElements
      INTEGER*4 :: mxIsEmpty
      INTEGER*4 :: mxIsChar
      INTEGER*4 :: mxIsStruct
      INTEGER*4 :: mxIsDouble
      INTEGER*4 :: mxIsComplex
      INTEGER*4 :: mxGetString
      REAL*8    :: mxGetScalar
      LOGICAL   :: ISEMPTY
      LOGICAL   :: ISVALID_DOUBLE
      LOGICAL   :: ISVALID_CHAR
      mwPointer :: GETPTR
C
C     MEX. DERIVED TYPES
      mwPointer :: PTR
      mwPointer :: XPTR
      mwSize    :: NVAL, NVALX, STRLEN
C
      INTEGER*4, PARAMETER :: MAXIDX = 512
      CHARACTER(LEN=7)     :: FILEACT
      LOGICAL   :: KEEPFILEOPEN, WRITEDATA, ISEVEN, OPENLUN
      LOGICAL*1 :: USE_BINARY, ISSTRUCT
      INTEGER*1 :: WRITEMODE, PTLIM, F03, F04
      INTEGER*4 :: LIDX, OK, NOK, CASENUM, OPENSTAT
      INTEGER*4 :: DATASETNUM, ISCOMPLEX, ISDOUBLE
      INTEGER*4 :: DS151F43, DS151F44, DS151F45
      INTEGER*4 :: DS164F11, DS164F13
      INTEGER*4 :: F05, F61, F62, F63, F64, F66, F67, F69, F610
      INTEGER*4 :: F71, F72, F73
      INTEGER*4 :: F81, F82, F83, F84
      INTEGER*4 :: F91, F92, F93, F94
      INTEGER*4 :: F101, F102, F103, F104
      INTEGER*4 :: F111, F112, F113, F114
      INTEGER*8 :: I, K, N, STEP, F06
      REAL*4    :: F74, F75, F76
      REAL*8    :: S
      REAL*8    :: DS164F21, DS164F22, DS164F23, DS164F24
      INTEGER*1, SAVE :: REMPTS(MAXIDX) = 0
      INTEGER*1, SAVE :: REC0BYTE(MAXIDX) = 0
      INTEGER*2, SAVE :: LUN(MAXIDX) = 0
      INTEGER*8, SAVE :: REC7POS(MAXIDX) = 0
      INTEGER*8, SAVE :: REC0POS(MAXIDX) = 0
      INTEGER*8, SAVE :: NUMPTS(MAXIDX) = 0
      REAL*4, DIMENSION(:), ALLOCATABLE     :: XDATA
      REAL*8, DIMENSION(:), ALLOCATABLE     :: XDATA_D
      REAL*4, DIMENSION(:), ALLOCATABLE     :: Y_RS
      REAL*8, DIMENSION(:), ALLOCATABLE     :: Y_RD
      COMPLEX*8, DIMENSION(:), ALLOCATABLE  :: Y_CS
      COMPLEX*16, DIMENSION(:), ALLOCATABLE :: Y_CD
      CHARACTER(LEN=:), ALLOCATABLE         :: FILEPATH
      CHARACTER(LEN=1)   :: F02
      CHARACTER(LEN=6)   :: DATATYPE_STR
      CHARACTER(LEN=10)  :: DS151F41, DS151F42
      CHARACTER(LEN=10)  :: DS151F51, DS151F52
      CHARACTER(LEN=10)  :: DS151F71, DS151F72
      CHARACTER(LEN=10)  :: F65, F68
      CHARACTER(LEN=10)  :: STR10
      CHARACTER(LEN=12)  :: STR12
      CHARACTER(LEN=20)  :: DS164F12
      CHARACTER(LEN=20)  :: F85, F86
      CHARACTER(LEN=20)  :: F95, F96
      CHARACTER(LEN=20)  :: F105, F106
      CHARACTER(LEN=20)  :: F115, F116
      CHARACTER(LEN=80)  :: ID1, ID2, ID3, ID4, ID5
      CHARACTER(LEN=512) :: FILESTR
C
C
C     ------------------------------------------------------------------
C
C
C     INITIALISE VARIABLES
      S = 0.0
      LIDX = 0
      CASENUM = 0
      KEEPFILEOPEN = .FALSE.
C
C
C
C --- CLOSE ALL OPENED FILES, OR CONTINUE ------------------------------
C ----------------------------------------------------------------------
      IF (nrhs < 1) THEN ! EMPTY CALL
        IF (MAXVAL(LUN) > 0) THEN
          DO K = 1,SIZE(LUN)
            IF (LUN(K) > 0) THEN
              CLOSE(LUN(K), STATUS='KEEP')
              LUN(K) = 0
            END IF
          END DO
          S = 1.0
        END IF
        LUN = 0
        NUMPTS = 0
        REMPTS = 0
C
        GO TO 10 ! GO TO END SECTION
C
      ELSEIF (nrhs < 2) THEN ! TOO FEW INPUTS
        S = -4.0
C
        GO TO 10 ! GO TO END SECTION
C
      ELSE
C       CONTINUE BELOW
C
      END IF
C ----------------------------------------------------------------------
C ----------------------------------------------------------------------
C
C
C
C
C --- DETERMINE DESIRED WRITING MODE -----------------------------------
C ----------------------------------------------------------------------
      OPENLUN = .FALSE.
      IF (nrhs < 3) THEN
        WRITEMODE = 1
      ELSE
        WRITEMODE = mxGetScalar(prhs(3))
      END IF
C
      IF (nrhs > 3) THEN
        LIDX = mxGetScalar(prhs(4))
        IF (LIDX > 0 .AND. LIDX <= MAXIDX) THEN
          INQUIRE(UNIT=LUN(LIDX), OPENED=OK)
          IF (WRITEMODE > 1) THEN
            IF (NOT(OK)) THEN
              S = -9.0
              GO TO 10
            END IF
          ELSE
            OPENLUN = .TRUE.
          END IF
        ELSEIF (LIDX < 0 .OR. LIDX > MAXIDX) THEN
          LIDX = 0
        END IF
      END IF
C ----------------------------------------------------------------------
C ----------------------------------------------------------------------
C
C
C
C
C --- OPEN FILE IF NEEDED/DESIRED --------------------------------------
C ----------------------------------------------------------------------
      IF (WRITEMODE == 1 .OR. LIDX < 1 .OR. OPENLUN) THEN
C       CREATE NEW FILE LUN
C
C       CHECK IF ARGUMENT IS EMPTY
        NOK = mxIsEmpty(prhs(1))
        IF (NOK) THEN ! IF EMPTY -> ERROR
          S = -2.0
          GO TO 10
        END IF
C
C       CHECK IF INPUT ARGUMENT IS OF CHAR TYPE
        OK = mxIsChar(prhs(1))
        IF (NOT(OK)) THEN ! IF NOT CHAR -> ERROR
          S = -2.0
          GO TO 10
        END IF
C
        STRLEN = mxGetN(prhs(1))
        ALLOCATE(CHARACTER(LEN=STRLEN) :: FILEPATH)
C
        NOK = mxGetString(prhs(1), FILEPATH, STRLEN)
C
        IF (NOK) THEN ! COULD NOT GET FILEPATH STRING FROM INPUT ARGUMENT
          S = -2.0
          GO TO 10
        END IF
C
C
C       ASSIGN DEFAULT INDEX IF MISSING OR OUT OF BOUNDS
        IF (LIDX < 1 .OR. LIDX > MAXIDX) THEN
          LIDX = 1
        ELSEIF (LUN(LIDX) == 0) THEN
          LUN(LIDX) = LIDX + 1
        END IF
C
C
C       DETERMINE IF FILE IS TO BE REPLACED OR NOT (APPENDED)
        IF (WRITEMODE == 1) THEN
          FILEACT = 'REPLACE'
        ELSE
          FILEACT = 'OLD'
        END IF
C
C
C       IF DESIRED LUN INDEX ALREADY MAPS TO AN OPEN LUN, CLOSE IT
        INQUIRE(UNIT=LUN(LIDX), OPENED=OK)
        IF (OK) THEN
          CLOSE(LUN(LIDX), STATUS='KEEP')
        END IF
C
C
C       OPEN FILE
        OPEN(
     &    UNIT = LUN(LIDX),
     &    FILE = FILEPATH,
     &    ACCESS = 'STREAM',
     &    STATUS = FILEACT,
     &    ERR = 8,
     &    FORM = 'FORMATTED',
     &    POSITION = 'APPEND',
     &    IOSTAT = OPENSTAT,
     &    ACTION = 'READWRITE')
C
        REMPTS(LIDX) = 0
        NUMPTS(LIDX) = 0
        REC0POS(LIDX) = 0
        REC7POS(LIDX) = 0
        REC0BYTE(LIDX) = 0
C
        IF (nrhs < 4) THEN
          KEEPFILEOPEN = .FALSE. ! CLOSE FILE BEFORE RETURNING
        ELSE
          KEEPFILEOPEN = .TRUE. ! KEEP FILE LUN OPEN FOR NEXT FUNCTION CALL
        END IF
C
C
      ELSE ! DO NOT CREATE NEW FILE LUN
C
        KEEPFILEOPEN = .TRUE.
C
      END IF
C ----------------------------------------------------------------------
C ----------------------------------------------------------------------
C
C
C
C
C
C
C --- ONLY WRITE STREAMING DATA ----------------------------------------
C ----------------------------------------------------------------------
      IF (WRITEMODE == 3) THEN
C
        IF (nrhs < 4) THEN
          S = -4.0
          GO TO 10
        END IF
C
        CASENUM = mxGetScalar(prhs(1))
C
C
        IF (CASENUM == -1) THEN ! FINALISE WRITING
C
          IF (LUN(LIDX) > 0) THEN
C
            INQUIRE(UNIT=LUN(LIDX), OPENED=OK, NAME=FILESTR)
            IF (NOT(OK)) THEN
              S = -9.0
              GO TO 10
            END IF
C
C           FINISH LAST LINE
            WRITE(LUN(LIDX),'(1X)',ERR=9)
C
C           WRITE UNV TERMINATOR, -1
            WRITE(LUN(LIDX),'(I6)') -1
C
C           CLOSE FILE
            CLOSE(LUN(LIDX),STATUS='KEEP')
C
            OPEN(
     &        UNIT = LUN(LIDX),
     &        FILE = FILESTR,
     &        ACCESS = 'STREAM',
     &        STATUS = 'OLD',
     &        ERR = 8,
     &        FORM = 'UNFORMATTED',
     &        ACTION = 'WRITE')
C
            IF (REC0POS(LIDX) /= 0) THEN ! IF NOT ZERO, WRITE NUMBER OF BYTES TO 58b ID RECORD
              WRITE(STR12, '(I12)') NUMPTS(LIDX)*REC0BYTE(LIDX)
              WRITE(LUN(LIDX), ERR=9, POS=REC0POS(LIDX)+31) STR12
            END IF
            WRITE(STR10, '(I10)') NUMPTS(LIDX)
            WRITE(LUN(LIDX), ERR=9, POS=REC7POS(LIDX)+10) STR10
            CLOSE(LUN(LIDX), STATUS='KEEP')
C
            S = 1.0
C
          ELSE
C
            S = -9.0
C
          END IF
C
          LUN(LIDX) = 0
          NUMPTS(LIDX) = 0
          REMPTS(LIDX) = 0
          REC0POS(LIDX) = 0
          REC0BYTE(LIDX) = 0
          REC7POS(LIDX) = 0
C
          GO TO 10 ! RETURN
C
C
        ELSE ! WRITE DATA
C
C
          NOK = mxIsEmpty(prhs(2))
C
          IF (NOK) THEN
            S = -10.0
            GO TO 10
          END IF
C
          NVAL = mxGetNumberOfElements(prhs(2))
C
          NUMPTS(LIDX) = NUMPTS(LIDX) + NVAL
          STEP = REMPTS(LIDX)
C
          IF (REC0POS(LIDX) /= 0) THEN
            USE_BINARY = .TRUE.
          ELSE
            USE_BINARY = .FALSE.
          END IF
C
C
C ------- WRITING STREAMING DATA / CASE 1 ----------------------------
          IF (CASENUM == 1) THEN ! REAL SINGLE, X-EVENLY-SPACED
C
            ALLOCATE(Y_RS(NVAL))
            PTR = mxGetSingles(prhs(2))
            CALL mxCopyPtrToReal4(PTR, Y_RS, NVAL)
C
            IF (USE_BINARY) THEN
C
              WRITE(LUN(LIDX), '(<NVAL>A4)', ERR=9, ADVANCE='NO')
     &          (Y_RS(I), I=1,NVAL)
              REMPTS(LIDX) = 0
C
            ELSE
C
              PTLIM = 6
C
              IF (STEP /= 0) THEN ! IF THERE IS UNFINISHED DATA, 0 -> FINISHED ROW
                IF (STEP >= NVAL) THEN ! IF NUMBER VALUES TO WRITE IS <= REQUIRED AMOUNT TO FINISH ROW
                  WRITE(LUN(LIDX), 1001, ERR=9, ADVANCE='NO')
     &              (Y_RS(I), I=1,NVAL)
                  REMPTS(LIDX) = REMPTS(LIDX)-NVAL
                  S = 1.0 ! SUCCESS
                  GO TO 10 ! RETURN
                ELSE ! MORE DATA AVAILABLE AFTER FINISH OF CURRENT LINE
C                 ADD PARTIAL DATA, REQUIRED AMOUNT TO FINISH ROW
                  WRITE(LUN(LIDX), 1001, ERR=9, ADVANCE='NO')
     &              (Y_RS(I), I = 1,STEP)
                END IF
              END IF
C
              N = STEP
C
              IF (N == NVAL) THEN
                REMPTS(LIDX) = 0
              ELSE
                WRITE(LUN(LIDX), '(1X)',ERR=9) ! IF PREVIOUS WRITE WAS FINISHED, OR END OF HEADER -> ADVANCE
                STEP = (NVAL-N) - MOD(NVAL-N, PTLIM) ! WRITE NO PARTIAL NUMBER OF LINES
                IF (N+STEP == NVAL) THEN
                  STEP = STEP-PTLIM ! ONLY WRITE UP UNTIL THE SECOND-TO-LAST LINE, WITH ADVANCE=YES
                END IF
C
C               WRITE 1:END-1 COMPLETE LINES, WITH ADVANCE
                IF (STEP > 0) THEN
                  WRITE(LUN(LIDX), 1001, ERR=9, ADVANCE='YES')
     &              (Y_RS(I+N), I = 1,STEP)
                END IF
C
                N = N + STEP
C
C               WRITE LAST (PARTIAL/COMPLETE) LINE, WITH NO ADVANCE
                WRITE(LUN(LIDX), 1001, ERR=9, ADVANCE='NO')
     &            (Y_RS(I), I = N+1,NVAL)
C
                REMPTS(LIDX) = PTLIM-(NVAL-N) ! POINTS LEFT TO WRITE
C
              END IF
C
            END IF ! END OF ASCII/BINARY SWITCH
C
            S = 1.0 ! SUCCESS
C
C
C
C
C ------- WRITING STREAMING DATA / CASE 3 ----------------------------
          ELSEIF (CASENUM == 3) THEN ! COMPLEX SINGLE, X-EVEN
C
            ALLOCATE(Y_CS(NVAL))
            PTR = mxGetComplexSingles(prhs(2))
            CALL mxCopyPtrToComplex8(PTR, Y_CS, NVAL)
C
            IF (USE_BINARY) THEN
C
              WRITE(LUN(LIDX), '(<NVAL>(A4,A4))', ERR=9, ADVANCE='NO')
     &          (Y_CS(I), I=1,NVAL)
              REMPTS(LIDX) = 0
C
            ELSE
C
              PTLIM = 3
C
              IF (STEP /= 0) THEN ! IF THERE IS UNFINISHED DATA, 0 -> FINISHED ROW
                IF (STEP >= NVAL) THEN ! IF NUMBER VALUES TO WRITE IS <= REQUIRED AMOUNT TO FINISH ROW
                    WRITE(LUN(LIDX), 1003, ERR=9, ADVANCE='NO')
     &                (Y_CS(I), I=1,NVAL)
                  REMPTS(LIDX) = REMPTS(LIDX)-NVAL
                  S = 1.0 ! SUCCESS
                  GO TO 10 ! RETURN
                ELSE ! MORE DATA AVAILABLE AFTER FINISH OF CURRENT LINE
C                 ADD PARTIAL DATA, REQUIRED AMOUNT TO FINISH ROW
                  WRITE(LUN(LIDX), 1003, ERR=9, ADVANCE='NO')
     &              (Y_CS(I), I = 1,STEP)
                END IF
              END IF
C
              N = STEP
C
              IF (N == NVAL) THEN
                REMPTS(LIDX) = 0
              ELSE
                WRITE(LUN(LIDX), '(1X)', ERR=9) ! IF PREVIOUS WRITE WAS FINISHED, OR END OF HEADER -> ADVANCE
                STEP = (NVAL-N) - MOD(NVAL-N, PTLIM) ! WRITE NO PARTIAL NUMBER OF LINES
                IF (N+STEP == NVAL) THEN
                  STEP = STEP-PTLIM ! ONLY WRITE UP UNTIL THE SECOND-TO-LAST LINE, WITH ADVANCE=YES
                END IF
C
C               WRITE 1:END-1 COMPLETE LINES, WITH ADVANCE
                IF (STEP > 0) THEN
                  WRITE(LUN(LIDX), 1003, ERR=9, ADVANCE='YES')
     &              (Y_CS(I+N), I = 1,STEP)
                END IF
C
                N = N + STEP
C
C               WRITE LAST (PARTIAL/COMPLETE) LINE, WITH NO ADVANCE
                WRITE(LUN(LIDX), 1003, ERR=9, ADVANCE='NO')
     &            (Y_CS(I), I = N+1,NVAL)
C
                REMPTS(LIDX) = PTLIM-(NVAL-N) ! POINTS LEFT TO WRITE
C
              END IF
C
            END IF ! END OF ASCII/BINARY SWITCH
C
            S = 1.0 ! SUCCESS
C
C
C
C
C ------- WRITING STREAMING DATA / CASE 5 ----------------------------
          ELSEIF (CASENUM == 5) THEN ! REAL DOUBLE, X-EVENLY-SPACED
C
            ALLOCATE(Y_RD(NVAL))
C
            PTR = mxGetDoubles(prhs(2))
            CALL mxCopyPtrToReal8(PTR, Y_RD, NVAL)
C
            IF (USE_BINARY) THEN
C
              WRITE(LUN(LIDX), '(<NVAL>(A8))', ERR=9, ADVANCE='NO')
     &          (Y_RD(I), I=1,NVAL)
              REMPTS(LIDX) = 0
C
            ELSE
C
              PTLIM = 4
C
              IF (STEP /= 0) THEN ! IF THERE IS UNFINISHED DATA, 0 -> FINISHED ROW
                IF (STEP >= NVAL) THEN ! IF NUMBER VALUES TO WRITE IS <= REQUIRED AMOUNT TO FINISH ROW
                  WRITE(LUN(LIDX), 1005, ERR=9, ADVANCE='NO')
     &              (Y_RD(I), I=1,NVAL)
                  REMPTS(LIDX) = REMPTS(LIDX)-NVAL
                  S = 1.0 ! SUCCESS
                  GO TO 10 ! RETURN
                ELSE ! MORE DATA AVAILABLE AFTER FINISH OF CURRENT LINE
C                 ADD PARTIAL DATA, REQUIRED AMOUNT TO FINISH ROW
                  WRITE(LUN(LIDX), 1005, ERR=9, ADVANCE='NO')
     &              (Y_RD(I), I = 1,STEP)
                END IF
              END IF
C
              N = STEP
C
              IF (N == NVAL) THEN
                REMPTS(LIDX) = 0
              ELSE
                WRITE(LUN(LIDX), '(1X)', ERR=9) ! IF PREVIOUS WRITE WAS FINISHED, OR END OF HEADER -> ADVANCE
                STEP = (NVAL-N) - MOD(NVAL-N, PTLIM) ! WRITE NO PARTIAL NUMBER OF LINES
                IF (N+STEP == NVAL) THEN
                  STEP = STEP-PTLIM ! ONLY WRITE UP UNTIL THE SECOND-TO-LAST LINE, WITH ADVANCE=YES
                END IF
C
C               WRITE 1:END-1 COMPLETE LINES, WITH ADVANCE
                IF (STEP > 0) THEN
                  WRITE(LUN(LIDX), 1005, ERR=9, ADVANCE='YES')
     &            (Y_RD(I+N), I = 1,STEP)
                END IF
C
                N = N + STEP
C
C               WRITE LAST (PARTIAL/COMPLETE) LINE, WITH NO ADVANCE
                WRITE(LUN(LIDX), 1005, ERR=9, ADVANCE='NO')
     &            (Y_RD(I), I = N+1,NVAL)
C
                REMPTS(LIDX) = PTLIM-(NVAL-N) ! POINTS LEFT TO WRITE
C
              END IF
C
            END IF ! END OF ASCII/BINARY SWITCH
C
            S = 1.0 ! SUCCESS
C
C
C
C
C ------- WRITING STREAMING DATA / CASE 7 ----------------------------
          ELSEIF (CASENUM == 7) THEN ! COMPLEX DOUBLE, X-EVEN
C
            ALLOCATE(Y_CD(NVAL))
            PTR = mxGetComplexDoubles(prhs(2))
            CALL mxCopyPtrToComplex16(PTR, Y_CD, NVAL)
C
            IF (USE_BINARY) THEN
C
              WRITE(LUN(LIDX), '(<NVAL>(A8,A8))', ERR=9, ADVANCE='NO')
     &          (Y_CD(I), I=1,NVAL)
              REMPTS(LIDX) = 0
C
            ELSE
C
              PTLIM = 2
C
              IF (STEP /= 0) THEN ! IF THERE IS UNFINISHED DATA, 0 -> FINISHED ROW
                IF (STEP >= NVAL) THEN ! IF NUMBER VALUES TO WRITE IS <= REQUIRED AMOUNT TO FINISH ROW
                    WRITE(LUN(LIDX), 1007, ERR=9, ADVANCE='NO')
     &              (Y_CD(I), I=1,NVAL)
                  REMPTS(LIDX) = REMPTS(LIDX)-NVAL
                  S = 1.0 ! SUCCESS
                  GO TO 10 ! RETURN
                ELSE ! MORE DATA AVAILABLE AFTER FINISH OF CURRENT LINE
C                   ADD PARTIAL DATA, REQUIRED AMOUNT TO FINISH ROW
                    WRITE(LUN(LIDX), 1007, ERR=9, ADVANCE='NO')
     &                (Y_CD(I), I = 1,STEP)
                END IF
              END IF
C
              N = STEP
C
              IF (N == NVAL) THEN
                REMPTS(LIDX) = 0
              ELSE
                WRITE(LUN(LIDX), '(1X)', ERR=9) ! IF PREVIOUS WRITE WAS FINISHED, OR END OF HEADER -> ADVANCE
                STEP = (NVAL-N) - MOD(NVAL-N, PTLIM) ! WRITE NO PARTIAL NUMBER OF LINES
                IF (N+STEP == NVAL) THEN
                  STEP = STEP-PTLIM ! ONLY WRITE UP UNTIL THE SECOND-TO-LAST LINE, WITH ADVANCE=YES
                END IF
C
C               WRITE 1:END-1 COMPLETE LINES, WITH ADVANCE
                IF (STEP > 0) THEN
                  WRITE(LUN(LIDX), 1007, ERR=9, ADVANCE='YES')
     &              (Y_CD(I+N), I = 1,STEP)
                END IF
C
                N = N + STEP
C
C               WRITE LAST (PARTIAL/COMPLETE) LINE, WITH NO ADVANCE
                WRITE(LUN(LIDX), 1007, ERR=9, ADVANCE='NO')
     &            (Y_CD(I), I = N+1,NVAL)
C
                REMPTS(LIDX) = PTLIM-(NVAL-N) ! POINTS LEFT TO WRITE
C
              END IF
C
            END IF ! END OF ASCII/BINARY SWITCH
C
            S = 1.0 ! SUCCESS
C
C
C
          ELSE
            S = -11.0
            GO TO 10
C
C
          END IF
C
        END IF
C
        GO TO 10
C
C
C
C ----------------------------------------------------------------------
C ----------------------------------------------------------------------
C
C
C
C
C --- NORMAL WRITING MODE ----------------------------------------------
C ----------------------------------------------------------------------
      ELSE
C
C
C --- GET DATASET TYPE TO WRITE
      ISSTRUCT = mxIsStruct(prhs(2))
C
C
      PTR = GETPTR(prhs(2), ISSTRUCT, 'datasetType')
      IF (NOT(ISVALID_DOUBLE(PTR))) THEN
        S = -5.0
        GO TO 10
      END IF
      DATASETNUM = mxGetScalar(PTR)
C
C
C
C --- DATASET 151 ------------------------------------------------------
C ----------------------------------------------------------------------
      IF (DATASETNUM == 151) THEN
C
C
C       RECORD 1, ID LINE 1, model file name
        PTR = GETPTR(prhs(2), ISSTRUCT, 'modelName')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          ID1 = 'NONE'
        ELSE
          NOK = mxGetString(PTR, ID1, 80)
          IF (ADJUSTL(ID1) == ' ') ID1 = 'NONE'
        END IF
C
C       RECORD 2, ID LINE 2, model file description
        PTR = GETPTR(prhs(2), ISSTRUCT, 'modelDescription')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          ID2 = 'NONE'
        ELSE
          NOK = mxGetString(PTR, ID2, 80)
          IF (ADJUSTL(ID2) == ' ') ID2 = 'NONE'
        END IF
C
C       RECORD 3, ID LINE 3, program which created DB
        PTR = GETPTR(prhs(2), ISSTRUCT, 'databaseProgram')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          ID3 = 'WHTA WRITEUNV'
        ELSE
          NOK = mxGetString(PTR, ID3, 80)
          IF (ADJUSTL(ID3) == ' ') ID3 = 'WHTA WRITEUNV'
        END IF
C
C       RECORD 4
C       4.1
        PTR = GETPTR(prhs(2), ISSTRUCT, 'databaseCreatedDate')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          DS151F41 = 'DD-MMM-YY'
        END IF
        NOK = mxGetString(PTR, DS151F41, 10)
C
C       4.2
        PTR = GETPTR(prhs(2), ISSTRUCT, 'databaseCreatedTime')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          DS151F42 = 'HH:MM:SS'
        ELSE
          NOK = mxGetString(PTR, DS151F42, 10)
        END IF
C
C       4.3
        PTR = GETPTR(prhs(2), ISSTRUCT, 'databaseVersion1')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          DS151F43 = 0
        ELSE
          DS151F43 = mxGetScalar(PTR)
        END IF
C
C       4.4
        PTR = GETPTR(prhs(2), ISSTRUCT, 'databaseVersion2')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          DS151F44 = 0
        ELSE
          DS151F44 = mxGetScalar(PTR)
        END IF
C
C       4.5
        PTR = GETPTR(prhs(2), ISSTRUCT, 'fileType')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          DS151F45 = 0
        ELSE
          DS151F45 = mxGetScalar(PTR)
          IF (NOT(ANY(DS151F45 == (/0, 1, 2/)))) THEN
            S = -18.0
            GO TO 10
          END IF
        END IF
C
C       RECORD 5
C       5.1
        PTR = GETPTR(prhs(2), ISSTRUCT, 'databaseLastSavedDate')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          DS151F51 = DS151F41
        ELSE
          NOK = mxGetString(PTR, DS151F51, 10)
        END IF
C
C       5.2
        PTR = GETPTR(prhs(2), ISSTRUCT, 'databaseLastSavedTime')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          DS151F52 = DS151F42
        ELSE
          NOK = mxGetString(PTR, DS151F52, 10)
        END IF
C
C       RECORD 6, ID LINE 6, program which created unv-file
        PTR = GETPTR(prhs(2), ISSTRUCT, 'unvProgram')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          ID4 = 'WHTA WRITEUNV'
        ELSE
          NOK = mxGetString(PTR, ID4, 80)
          IF (ADJUSTL(ID3) == ' ') ID4 = 'WHTA WRITEUNV'
        END IF
C
C       RECORD 7
C       7.1
        PTR = GETPTR(prhs(2), ISSTRUCT, 'fileCreatedDate')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          DS151F71 = DS151F41
        ELSE
          NOK = mxGetString(PTR, DS151F71, 10)
        END IF
C
C       7.2
        PTR = GETPTR(prhs(2), ISSTRUCT, 'fileCreatedTime')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          DS151F72 = DS151F42
        ELSE
          NOK = mxGetString(PTR, DS151F72, 10)
        END IF
C
C
C ----- WRITE ALL FIELDS
C
C       START OF RECORD, -1
        WRITE(LUN(LIDX), '(I6)', ERR=9) -1
C
C       DATASET NUMBER
        WRITE(LUN(LIDX), '(I6)', ERR=9) DATASETNUM
C
C       RECORD 1
        WRITE(LUN(LIDX), '(A80)', ERR=9) ID1
C
C       RECORD 2
        WRITE(LUN(LIDX), '(A80)', ERR=9) ID2
C
C       RECORD 3
        WRITE(LUN(LIDX),'(A80)', ERR=9) ID3
C
C       RECORD 4
        WRITE(LUN(LIDX), 1514, ERR=9)
     &    DS151F41, DS151F42, DS151F43, DS151F44, DS151F45
1514    FORMAT(A10, A10, 3I10)
C
C       RECORD 5
        WRITE(LUN(LIDX), 1515, ERR=9) DS151F51, DS151F52
1515    FORMAT(A10, A10)
C
C       RECORD 6
        WRITE(LUN(LIDX), '(A80)', ERR=9) ID4
C
C       RECORD 7
        WRITE(LUN(LIDX), 1517, ERR=9) DS151F71, DS151F72
1517    FORMAT(A10, A10)
C
C       END OF RECORD, -1
        WRITE(LUN(LIDX), '(I6)', ERR=9) -1
C
C
C ----- WRITIING FINISHED
C
        S = 1.0 ! SUCCESS
        GO TO 10 ! GO TO END
C
C
C
C
C --- DATASET 164 ------------------------------------------------------
C ----------------------------------------------------------------------
      ELSEIF (DATASETNUM == 164) THEN
C
C
C       RECORD 1
C       1.1
        PTR = GETPTR(prhs(2), ISSTRUCT, 'unitsCode')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          DS164F11 = 1
        ELSE
          DS164F11 = mxGetScalar(PTR)
          IF (NOT(ANY(DS164F11 == (/1, 2, 3, 4, 5, 6, 7, 8, 9/)))) THEN
            S = -19.0
            GO TO 10
          END IF
        END IF
C
C       1.2
        PTR = GETPTR(prhs(2), ISSTRUCT, 'unitsDescription')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          SELECT CASE (DS164F11)
            CASE (1)
              DS164F12 = 'Meter (newton)'
            CASE (2)
              DS164F12 = 'Foot (pound f)'
            CASE (3)
              DS164F12 = 'Meter (kilogram f)'
            CASE (4)
              DS164F12 = 'Foot (poundal)'
            CASE (5)
              DS164F12 = 'mm (milli newton)'
            CASE (6)
              DS164F12 = 'cm (centi newton)'
            CASE (7)
              DS164F12 = 'Inch (pound f)'
            CASE (8)
              DS164F12 = 'mm (kilogram f)'
            CASE (9)
              DS164F12 = 'USER_DEFINED'
          END SELECT
        ELSE
          NOK = mxGetString(PTR, DS164F12, 20)
        END IF
C
C       1.3
        PTR = GETPTR(prhs(2), ISSTRUCT, 'temperatureMode')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          DS164F13 = 1
        ELSE
          DS164F13 = mxGetScalar(PTR)
        END IF
C
C       RECORD 2
C       2.1
        PTR = GETPTR(prhs(2), ISSTRUCT, 'length')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          DS164F21 = 1
        ELSE
          DS164F21 = mxGetScalar(PTR)
        END IF
C
C       2.2
        PTR = GETPTR(prhs(2), ISSTRUCT, 'force')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          DS164F22 = 1
        ELSE
          DS164F22 = mxGetScalar(PTR)
        END IF
C
C       2.3
        PTR = GETPTR(prhs(2), ISSTRUCT, 'temperature')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          DS164F23 = 1
        ELSE
          DS164F23 = mxGetScalar(PTR)
        END IF
C
C       2.4
        PTR = GETPTR(prhs(2), ISSTRUCT, 'temperatureOffset')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          DS164F24 = 0
        ELSE
          DS164F24 = mxGetScalar(PTR)
        END IF
C
C
C
C ----- WRITE ALL FIELDS
C
C       START OF RECORD, -1
        WRITE(LUN(LIDX), '(I6)', ERR=9) -1
C
C       DATASET NUMBER
        WRITE(LUN(LIDX), '(I6)', ERR=9) DATASETNUM
C
C       RECORD 1
        WRITE(LUN(LIDX), 1641, ERR=9) DS164F11, DS164F12, DS164F13
1641    FORMAT(I10, A20, I10)
C
C       RECORD 2
        WRITE(LUN(LIDX), 1642, ERR=9)
     &    DS164F21, DS164F22, DS164F23, DS164F24
1642    FORMAT(3ES25.17)
C
C       END OF RECORD, -1
        WRITE(LUN(LIDX), '(I6)', ERR=9) -1
C
C
C ----- WRITIING FINISHED
C
C
C
        S = 1.0 ! SUCCESS
        GO TO 10 ! GO TO END
C
C
C
C --- DATASET 58 -------------------------------------------------------
C ----------------------------------------------------------------------
      ELSEIF (DATASETNUM == 58) THEN
C
C
C       RECORD 1, ID LINE 1
        PTR = GETPTR(prhs(2), ISSTRUCT, 'ID1')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          ID1 = 'NONE'
        ELSE
          NOK = mxGetString(PTR, ID1, 80)
          IF (ADJUSTL(ID1) == ' ') ID1 = 'NONE'
        END IF
C
C       RECORD 2, ID LINE 2
        PTR = GETPTR(prhs(2), ISSTRUCT, 'ID2')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          ID2 = 'NONE'
        ELSE
          NOK = mxGetString(PTR, ID2, 80)
          IF (ADJUSTL(ID2) == ' ') ID2 = 'NONE'
        END IF
C
C       RECORD 3, date
        PTR = GETPTR(prhs(2), ISSTRUCT, 'date')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          ID3 = 'UNKNOWN DATE'
        ELSE
          NOK = mxGetString(PTR, ID3, 80)
        END IF
C
C       RECORD 4, ID LINE 4
        PTR = GETPTR(prhs(2), ISSTRUCT, 'ID4')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          ID4 = 'NONE'
        ELSE
          NOK = mxGetString(PTR, ID4, 80)
          IF (ADJUSTL(ID4) == ' ') ID4 = 'NONE'
        END IF
C
C       RECORD 5, ID LINE 5
        PTR = GETPTR(prhs(2), ISSTRUCT, 'ID5')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          ID5 = 'NONE'
        ELSE
          NOK = mxGetString(PTR, ID5, 80)
          IF (ADJUSTL(ID5) == ' ') ID5 = 'NONE'
        END IF
C
C       RECORD 6
C       6.1
        PTR = GETPTR(prhs(2), ISSTRUCT, 'functionType')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F61 = 0
        ELSE
          F61 = mxGetScalar(PTR)
        END IF
C
C       6.2
        PTR = GETPTR(prhs(2), ISSTRUCT, 'functionID')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F62 = 0
        ELSE
          F62 = mxGetScalar(PTR)
        END IF
C
C       6.3
        PTR = GETPTR(prhs(2), ISSTRUCT, 'versionNumber')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F63 = 0
        ELSE
          F63 = mxGetScalar(PTR)
        END IF
C
C       6.4
        PTR = GETPTR(prhs(2), ISSTRUCT, 'loadCaseID')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F64 = 0
        ELSE
          F64 = mxGetScalar(PTR)
        END IF
C
C       6.5
        PTR = GETPTR(prhs(2), ISSTRUCT, 'rspEntName')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          F65 = 'NONE'
        ELSE
          NOK = mxGetString(PTR, F65, 10)
          IF (ADJUSTL(F65) == ' ') F65 = 'NONE'
        END IF
C
C       6.6
        PTR = GETPTR(prhs(2), ISSTRUCT, 'rspNode')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F66 = 0
        ELSE
          F66 = mxGetScalar(PTR)
        END IF
C
C       6.7
        PTR = GETPTR(prhs(2), ISSTRUCT, 'rspDir')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F67 = 0
        ELSE
          F67 = mxGetScalar(PTR)
          IF (F67 < -6 .OR. F67 > 6) THEN
            S = -20.0
            GO TO 10
          END IF
        END IF
C
C       6.8
        PTR = GETPTR(prhs(2), ISSTRUCT, 'refEntName')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          F68 = 'NONE'
        ELSE
          NOK = mxGetString(PTR, F68, 10)
          IF (ADJUSTL(F68) == ' ') F68 = 'NONE'
        END IF
C
C       6.9
        PTR = GETPTR(prhs(2), ISSTRUCT, 'refNode')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F69 = 0
        ELSE
          F69 = mxGetScalar(PTR)
        END IF
C
C       6.10
        PTR = GETPTR(prhs(2), ISSTRUCT, 'refDir')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F610 = 0
        ELSE
          F610 = mxGetScalar(PTR)
          IF (F610 < -6 .OR. F610 > 6) THEN
            S = -20.0
            GO TO 10
          END IF
        END IF
C
C       RECORD 7
C       7.1
        PTR = GETPTR(prhs(2), ISSTRUCT, 'dataFormatType')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F71 = -1
        ELSE
          F71 = mxGetScalar(PTR)
        END IF
C
C       7.2
        PTR = GETPTR(prhs(2), ISSTRUCT, 'numValues')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F72 = 0
        ELSE
          F72 = mxGetScalar(PTR)
        END IF
C
C       7.3
        PTR = GETPTR(prhs(2), ISSTRUCT, 'evenSpacing')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F73 = 0
        ELSE
          F73 = mxGetScalar(PTR)
        END IF
C
C       7.4
        PTR = GETPTR(prhs(2), ISSTRUCT, 'x0')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F74 = 0
        ELSE
          F74 = mxGetScalar(PTR)
        END IF
C
C       7.5
        PTR = GETPTR(prhs(2), ISSTRUCT, 'dx')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F75 = 0
        ELSE
          F75 = mxGetScalar(PTR)
        END IF
C
C       7.6
        PTR = GETPTR(prhs(2), ISSTRUCT, 'zAxisValue')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F76 = 0
        ELSE
          F76 = mxGetScalar(PTR)
        END IF
C
C       RECORD 8
C       8.1
        PTR = GETPTR(prhs(2), ISSTRUCT, 'abscDataType')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F81 = 0
        ELSE
          F81 = mxGetScalar(PTR)
        END IF
C
C       8.2
        PTR = GETPTR(prhs(2), ISSTRUCT, 'abscLengthUnitsExponent')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F82 = 0
        ELSE
          F82 = mxGetScalar(PTR)
        END IF
C
C       8.3
        PTR = GETPTR(prhs(2), ISSTRUCT, 'abscForceUnitsExponent')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F83 = 0
        ELSE
          F83 = mxGetScalar(PTR)
        END IF
C
C       8.4
        PTR = GETPTR(prhs(2), ISSTRUCT, 'abscTempUnitsExponent')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F84 = 0
        ELSE
          F84 = mxGetScalar(PTR)
        END IF
C
C       8.5
        PTR = GETPTR(prhs(2), ISSTRUCT, 'abscAxisLabel')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          F85 = 'NONE'
        ELSE
          NOK = mxGetString(PTR, F85, 20)
        END IF
C
C       8.6
        PTR = GETPTR(prhs(2), ISSTRUCT, 'abscAxisUnitLabel')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          F86 = 'NONE'
        ELSE
          NOK = mxGetString(PTR, F86, 20)
        END IF
C
C       RECORD 9
C       9.1
        PTR = GETPTR(prhs(2), ISSTRUCT, 'ordNumDataType')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          S = -17.0
          GO TO 10
        ELSE
          F91 = mxGetScalar(PTR)
        END IF
C
C       9.2
        PTR = GETPTR(prhs(2), ISSTRUCT, 'ordNumLengthUnitsExponent')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F92 = 0
        ELSE
          F92 = mxGetScalar(PTR)
        END IF
C
C       9.3
        PTR = GETPTR(prhs(2), ISSTRUCT, 'ordNumForceUnitsExponent')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F93 = 0
        ELSE
          F93 = mxGetScalar(PTR)
        END IF
C
C       9.4
        PTR = GETPTR(prhs(2), ISSTRUCT, 'ordNumTempUnitsExponent')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F94 = 0
        ELSE
          F94 = mxGetScalar(PTR)
        END IF
C
C       9.5
        PTR = GETPTR(prhs(2), ISSTRUCT, 'ordNumAxisLabel')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          F95 = 'NONE'
        ELSE
          NOK = mxGetString(PTR, F95, 20)
        END IF
C
C       9.6
        PTR = GETPTR(prhs(2), ISSTRUCT, 'ordNumAxisUnitLabel')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          F96 = 'NONE'
        ELSE
          NOK = mxGetString(PTR, F96, 20)
        END IF
C
C       RECORD 10
C       10.1
        PTR = GETPTR(prhs(2), ISSTRUCT, 'ordDenomDataType')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F101 = 0
        ELSE
          F101 = mxGetScalar(PTR)
        END IF
C
C       10.2
        PTR = GETPTR(prhs(2), ISSTRUCT, 'ordDenomLengthUnitsExponent')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F102 = 0
        ELSE
          F102 = mxGetScalar(PTR)
        END IF
C
C       10.3
        PTR = GETPTR(prhs(2), ISSTRUCT, 'ordDenomForceUnitsExponent')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F103 = 0
        ELSE
          F103 = mxGetScalar(PTR)
          F103 = mxGetScalar(PTR)
        END IF
C
C       10.4
        PTR = GETPTR(prhs(2), ISSTRUCT, 'ordDenomTempUnitsExponent')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F104 = 0
        ELSE
          F104= mxGetScalar(PTR)
        END IF
C
C       10.5
        PTR = GETPTR(prhs(2), ISSTRUCT, 'ordDenomAxisLabel')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          F105 = 'NONE'
        ELSE
          NOK = mxGetString(PTR, F105, 20)
          IF (ADJUSTL(F105) == ' ') F105 = 'NONE'
        END IF
C
C       10.6
        PTR = GETPTR(prhs(2), ISSTRUCT, 'ordDenomAxisUnitLabel')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          F106 = 'NONE'
        ELSE
          NOK = mxGetString(PTR, F106, 20)
          IF (ADJUSTL(F106) == ' ') F106 = 'NONE'
        END IF
C
C       RECORD 11
C       11.1
        PTR = GETPTR(prhs(2), ISSTRUCT, 'zDataType')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F111 = 0
        ELSE
          F111 = mxGetScalar(PTR)
        END IF
C
C       11.2
        PTR = GETPTR(prhs(2), ISSTRUCT, 'zLengthUnitsExponent')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F112 = 0
        ELSE
          F112 = mxGetScalar(PTR)
        END IF
C
C       11.3
        PTR = GETPTR(prhs(2), ISSTRUCT, 'zForceUnitsExponent')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F113 = 0
        ELSE
          F113 = mxGetScalar(PTR)
        END IF
C
C       11.4
        PTR = GETPTR(prhs(2), ISSTRUCT, 'zTempUnitsExponent')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F114 = 0
        ELSE
          F114 = mxGetScalar(PTR)
        END IF
C
C       11.5
        PTR = GETPTR(prhs(2), ISSTRUCT, 'zAxisLabel')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          F115 = 'NONE'
        ELSE
          NOK = mxGetString(PTR, F115, 20)
          IF (ADJUSTL(F115) == ' ') F115 = 'NONE'
        END IF
C
C       11.6
        PTR = GETPTR(prhs(2), ISSTRUCT, 'zAxisUnitLabel')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          F116 = 'NONE'
        ELSE
          NOK = mxGetString(PTR, F116, 20)
          IF (ADJUSTL(F116) == ' ') F116 = 'NONE'
        END IF
C
        ISEVEN = F73
C
        IF (ISEVEN == .FALSE.) THEN
          F74 = 0
          F75 = 0
        END IF
C
C
C       CHECK IF DATA SHOULD BE WRITTEN
        PTR = GETPTR(prhs(2), ISSTRUCT, 'data')
        IF (ISEMPTY(PTR)) THEN
C
          IF (NOT(ANY(F71 == (/2, 4, 5, 6/) ))) THEN
            S = -14.0
            GO TO 10
          END IF
C
          WRITEDATA = .FALSE.
C
        ELSE
C
          WRITEDATA = .TRUE.
C
C         GET DATATYPE OF DATA: SINGLE/DOUBLE, REAL/COMPLEX ?
          ISDOUBLE = mxIsDouble(PTR)
          ISCOMPLEX = mxIsComplex(PTR)
          NVAL = mxGetNumberOfElements(PTR)
C
C         GET CASENUM FROM DATATYPE AND X-EVENNESS
          CASENUM = 1
          IF (ISEVEN == .FALSE.) THEN
            CASENUM = CASENUM + 1 ! = UNEVEN X-DATA
            XPTR = GETPTR(prhs(2), ISSTRUCT, 'x')
            IF (ISEMPTY(XPTR)) THEN
              S = -15.0
              GO TO 10 ! X-DATA FIELD NOT PRESENT OR EMPTY
            END IF
C
            NVALX = mxGetNumberOfElements(XPTR)
C
            IF (NVALX /= NVAL) THEN
              S = -16.0
              GO TO 10 ! MISMATCHING X-DATA
            END IF
C
            IF (NOT(ALLOCATED(XDATA))) THEN
              ALLOCATE(XDATA(NVAL))
            ELSE IF(SIZE(XDATA) /= NVAL) THEN
              DEALLOCATE(XDATA)
              ALLOCATE(XDATA(NVAL))
            END IF
C
            IF (mxIsDouble(XPTR)) THEN
              IF (NOT(ALLOCATED(XDATA_D))) THEN
                ALLOCATE(XDATA_D(NVAL))
              ELSE IF(SIZE(XDATA_D) /= NVAL) THEN
                DEALLOCATE(XDATA_D)
                ALLOCATE(XDATA_D(NVAL))
              END IF
              CALL mxCopyPtrToReal8(mxGetDoubles(XPTR), XDATA_D, NVAL)
              XDATA = XDATA_D
            ELSE
              CALL mxCopyPtrToReal4(mxGetSingles(XPTR), XDATA, NVAL)
            END IF
          END IF
C
          IF (ISCOMPLEX) THEN
            CASENUM = CASENUM + 2 ! = COMPLEX
          END IF
          IF (ISDOUBLE) THEN
            CASENUM = CASENUM + 4 ! = DOUBLE
          END IF
C
          IF (CASENUM > 6) THEN ! COMPLEX DOUBLE
            IF (NOT(ALLOCATED(Y_CD))) THEN
              ALLOCATE(Y_CD(NVAL))
            ELSE IF(SIZE(Y_CD) /= NVAL) THEN
              DEALLOCATE(Y_CD)
              ALLOCATE(Y_CD(NVAL))
            END IF
            CALL mxCopyPtrToComplex16(mxGetComplexDoubles(PTR), Y_CD,
     &        NVAL)
            F71 = 6
C
          ELSEIF (CASENUM > 4) THEN ! REAL DOUBLE
            IF (NOT(ALLOCATED(Y_RD))) THEN
              ALLOCATE(Y_RD(NVAL))
            ELSE IF(SIZE(Y_RD) /= NVAL) THEN
              DEALLOCATE(Y_RD)
              ALLOCATE(Y_RD(NVAL))
            END IF
            CALL mxCopyPtrToReal8(mxGetDoubles(PTR), Y_RD, NVAL)
            F71 = 4
C
          ELSEIF (CASENUM > 2) THEN ! COMPLEX SINGLE
            IF (NOT(ALLOCATED(Y_CS))) THEN
              ALLOCATE(Y_CS(NVAL))
            ELSE IF(SIZE(Y_CS) /= NVAL) THEN
              DEALLOCATE(Y_CS)
              ALLOCATE(Y_CS(NVAL))
            END IF
            CALL mxCopyPtrToComplex8(mxGetComplexSingles(PTR), Y_CS,
     &        NVAL)
            F71 = 5
C
          ELSE ! REAL SINGLE
            IF (NOT(ALLOCATED(Y_RS))) THEN
              ALLOCATE(Y_RS(NVAL))
            ELSE IF(SIZE(Y_RS) /= NVAL) THEN
              DEALLOCATE(Y_RS)
              ALLOCATE(Y_RS(NVAL))
            END IF
            CALL mxCopyPtrToReal4(mxGetSingles(PTR), Y_RS, NVAL)
            F71 = 2
C
          END IF
C
          F72 = NVAL
C
        END IF
C
C
C       SHOULD BE WRITTEN AS ASCII OR BINARY?
        PTR = GETPTR(prhs(2), ISSTRUCT, 'dataType')
        IF (ISEMPTY(PTR)) THEN
          USE_BINARY = .FALSE.
        ELSE
          NOK = mxGetString(PTR, DATATYPE_STR, 6)
          IF (NOT(NOK)) THEN
            USE_BINARY = (LOWER(DATATYPE_STR) == 'binary')
          ELSE
            USE_BINARY = .FALSE.
          END IF
        END IF
C
C
C
C ----- WRITE ALL FIELDS
C
C       START OF RECORD, -1
        WRITE(LUN(LIDX), '(I6)', ERR=9) -1
C
C       DATASET NUMBER
        IF (NOT(USE_BINARY)) THEN
          WRITE(LUN(LIDX), '(I6)', ERR=9) DATASETNUM
        ELSE
          INQUIRE(LUN(LIDX), POS=REC0POS(LIDX))
          F02 = 'b'
          F03 = 1
          F04 = 2
          F05 = 11
          SELECT CASE (CASENUM)
            CASE (1)
              REC0BYTE(LIDX) = 4
            CASE (2)
              REC0BYTE(LIDX) = 8
            CASE (3)
              REC0BYTE(LIDX) = 8
            CASE (4)
              REC0BYTE(LIDX) = 12
            CASE (5)
              REC0BYTE(LIDX) = 8
            CASE (6)
              REC0BYTE(LIDX) = 12
            CASE (7)
              REC0BYTE(LIDX) = 16
            CASE (8)
              REC0BYTE(LIDX) = 20
          END SELECT
          F06 = NVAL*REC0BYTE(LIDX)
5800      FORMAT(I6, 1A1, I6, I6, I12, I12, I6, I6, I12, I12)
          WRITE(LUN(LIDX), 5800, ERR=9) DATASETNUM, F02, F03, F04, F05,
     &      F06, 0, 0, 0, 0
        END IF ! END OF ASCII/BINARY SWITCH
C
C       RECORD 1
        WRITE(LUN(LIDX), '(A80)', ERR=9) ID1
C
C       RECORD 2
        WRITE(LUN(LIDX), '(A80)', ERR=9) ID2
C
C       RECORD 3
        WRITE(LUN(LIDX), '(A80)', ERR=9) ID3
C
C       RECORD 4
        WRITE(LUN(LIDX), '(A80)', ERR=9) ID4
C
C       RECORD 5
        WRITE(LUN(LIDX), '(A80)', ERR=9) ID5
C
C       RECORD 6
5806    FORMAT(2(I5, I10), 2(1X, A10, I10, I4))
        WRITE(LUN(LIDX), 5806, ERR=9)
     &    F61, F62, F63, F64, F65, F66, F67, F68, F69, F610
C
C       RECORD 7
5807    FORMAT(3I10, 3ES13.5)
        INQUIRE(LUN(LIDX), POS=REC7POS(LIDX))
        WRITE(LUN(LIDX), 5807, ERR=9) F71, F72, F73, F74, F75, F76
C
C       RECORD 8
5808    FORMAT(I10, 3I5, 2(1X,A20))
        WRITE(LUN(LIDX), 5808, ERR=9) F81, F82, F83, F84, F85, F86
C
C       RECORD 9
5809    FORMAT(I10, 3I5, 2(1X, A20))
        WRITE(LUN(LIDX), 5809, ERR=9) F91, F92, F93, F94, F95, F96
C
C       RECORD 10
5810    FORMAT(I10, 3I5, 2(1X, A20))
        WRITE(LUN(LIDX), 5810, ERR=9) F101, F102, F103, F104, F105, F106
C
C       RECORD 11
5811    FORMAT(I10, 3I5, 2(1X, A20))
        IF (WRITEDATA) THEN ! IF DATA IS TO BE WRITTEN IN THIS CALL
          WRITE(LUN(LIDX), 5811, ERR=9, ADVANCE='YES')
     &      F111, F112, F113, F114, F115, F116
        ELSE ! DATA WILL COME IN NEXT FUNCTION CALL
          WRITE(LUN(LIDX), 5811, ERR=9, ADVANCE='NO')
     &      F111, F112, F113, F114, F115, F116
        END IF
C
C       RECORD 12, DATA, IF TO BE WRITTEN
        IF (WRITEDATA) THEN
C
          N = 0 ! INITIALISE NUMBER OF VALUES WRITTEN
          SELECT CASE (CASENUM)
           CASE (1) ! REAL SINGLE DATA, EVEN X-DATA
            IF (NOT(USE_BINARY)) THEN
1001          FORMAT(6ES13.5)
              WRITE(LUN(LIDX),1001,ERR=9) (Y_RS(I), I=1,NVAL)
            ELSE
              WRITE(LUN(LIDX),'(<NVAL>A4)',ERR=9) (Y_RS(I), I=1,NVAL)
            END IF
C
           CASE (2) ! REAL SINGLE DATA, UNEVEN X-DATA
            IF (NOT(USE_BINARY)) THEN
1002          FORMAT(6ES13.5)
              WRITE(LUN(LIDX),1002,ERR=9) (XDATA(I), Y_RS(I), I=1,NVAL)
            ELSE
              WRITE(LUN(LIDX),'(<NVAL>(A4,A4))',ERR=9)
     &          (XDATA(I), Y_RS(I), I=1,NVAL)
            END IF
C
           CASE (3) ! COMPLEX SINGLE DATA, EVEN X-DATA
            IF (NOT(USE_BINARY)) THEN
1003          FORMAT(6ES13.5)
              WRITE(LUN(LIDX),1003,ERR=9) (Y_CS(I), I=1,NVAL)
            ELSE
              WRITE(LUN(LIDX),'(<NVAL>(A4,A4))',ERR=9)
     &          (Y_CS(I), I=1,NVAL)
            END IF
C
           CASE (4) ! COMPLEX SINGLE DATA, UNEVEN X-DATA
            IF (NOT(USE_BINARY)) THEN
1004          FORMAT(6ES13.5)
              WRITE(LUN(LIDX),1004,ERR=9) (XDATA(I), Y_CS(I), I=1,NVAL)
            ELSE
              WRITE(LUN(LIDX),'(<NVAL>(A4,A4,A4))',ERR=9)
     &          (XDATA(I), Y_CS(I), I=1,NVAL)
            END IF
C
           CASE (5) ! REAL DOUBLE DATA, EVEN X-DATA
            IF (NOT(USE_BINARY)) THEN
1005          FORMAT(4ES20.12)
              WRITE(LUN(LIDX), 1005, ERR=9) (Y_RD(I), I=1,NVAL)
            ELSE
              WRITE(LUN(LIDX),'(<NVAL>A8)',ERR=9) (Y_RD(I), I=1,NVAL)
            END IF
C
           CASE (6) ! REAL DOUBLE DATA, UNEVEN X-DATA
            IF (NOT(USE_BINARY)) THEN
1006          FORMAT(2(ES13.5, ES20.12))
              WRITE(LUN(LIDX),1006,ERR=9) (XDATA(I), Y_RD(I), I=1,NVAL)
            ELSE
              WRITE(LUN(LIDX),'(<NVAL>(A4, A8))',ERR=9)
     &          (XDATA(I), Y_RD(I), I=1,NVAL)
            END IF
C
           CASE (7) ! COMPLEX DOUBLE DATA, EVEN X-DATA
            IF (NOT(USE_BINARY)) THEN
1007          FORMAT(4ES20.12)
              WRITE(LUN(LIDX),1007,ERR=9) (Y_CD(I), I=1,NVAL)
            ELSE
              WRITE(LUN(LIDX),'(<NVAL>(A8,A8))',ERR=9)
     &         (Y_CD(I), I=1,NVAL)
            END IF
C
           CASE (8) ! COMPLEX DOUBLE DATA, UNEVEN X-DATA
            IF (NOT(USE_BINARY)) THEN
1008          FORMAT(ES13.5, 2ES20.12)
              WRITE(LUN(LIDX),1008,ERR=9) (XDATA(I), Y_CD(I), I=1,NVAL)
            ELSE
              WRITE(LUN(LIDX),'(<NVAL>(A4,A8,A8))',ERR=9)
     &          (XDATA(I), Y_CD(I), I=1,NVAL)
            END IF
C
           CASE DEFAULT
            GO TO 10 ! ERROR: BAD CASENUM, SHOULD ALREADY BE CHECKED ABOVE
C
          END SELECT
C
C         END OF RECORD, -1, IF DATA IS WRITTEN, OTHERWISE IT IS KEPT UNTERMINATED
          WRITE(LUN(LIDX), '(I6)', ERR=9) -1
C
C
        ELSE ! KEEP UNTERMINATED
C
          IF (USE_BINARY) THEN
            WRITE(LUN(LIDX), '(1X)', ERR=9) ! FINISH LAST LINE, BEFORE BINARY STREAM
          END IF
C
C
C         INIT. CASENUM
          CASENUM = 1
C
C         DETERMINE CASENUM
          ISDOUBLE = (F71 == 4 .OR. F71 == 6) ! IS DATA OF TYPE DOUBLE, OR NOT -> SINGLE
          ISCOMPLEX = (F71 > 4) ! IS DATA COMPLEX- (TRUE) OR REAL-VALUED (FALSE)
C
          IF (NOT(ISEVEN)) THEN
            CASENUM = CASENUM + 1 ! = UN-EVEN
          END IF
          IF (ISCOMPLEX) THEN
            CASENUM = CASENUM + 2 ! = COMPLEX
          END IF
          IF (ISDOUBLE) THEN
            CASENUM = CASENUM + 4 ! = DOUBLE
          END IF
C
          SELECT CASE (CASENUM)
            CASE (1)
              REC0BYTE(LIDX) = 4
            CASE (2)
              REC0BYTE(LIDX) = 8
            CASE (3)
              REC0BYTE(LIDX) = 8
            CASE (4)
              REC0BYTE(LIDX) = 12
            CASE (5)
              REC0BYTE(LIDX) = 8
            CASE (6)
              REC0BYTE(LIDX) = 12
            CASE (7)
              REC0BYTE(LIDX) = 16
            CASE (8)
              REC0BYTE(LIDX) = 20
          END SELECT
C
        END IF
C
C
C ----- WRITIING FINISHED
C
        S = 1.0 ! SUCCESS
        GO TO 10
C
C
      ELSE ! UNSUPPORTED DATASET
C
        S = -6.0
        GO TO 10
C
      END IF
C
      END IF
C
C
C
C --- FINISHED WRITING -------------------------------------------------
C ----------------------------------------------------------------------
C
C
C
C --- RETURN ARGUMENTS AND CLOSE FILES AND VARIABLES
C
      S = -90.0 ! SHOULD NEVER HAPPEN -> ERROR IN CODE
      GO TO 10
C
8     CONTINUE
C
      IF (OPENSTAT == 1) THEN ! REMAP IOSTAT ERROR CODE 1 TO -12
        S = -12.0
      ELSEIF (OPENSTAT == 29) THEN ! REMAP IOSTAT ERROR CODE 29 TO -13
        S = -13.0
      ELSE
        S = OPENSTAT
      END IF
      GO TO 10
C
C     UNKNOWN ERROR EXIT
9     S = -1.0
      KEEPFILEOPEN = .FALSE.
C
C
10    CONTINUE ! NORMAL/ASSIGNED EXIT
C
      plhs(1) = mxCreateDoubleScalar(S)
      IF (nlhs > 1) THEN
        IF (INT(S) == 1) THEN ! SUCCESSFUL
C
          plhs(2) = mxCreateDoubleScalar(DBLE(CASENUM))
C
        ELSE ! NOT SUCCESSFUL -> RETURN ERROR DESCRIPTION
          K = S
          SELECT CASE (K)
            CASE (0)
              ID1 = 'UNKNOWN ERROR.'
C
            CASE (-1)
              ID1 = 'ERROR WRITING FIELDS/DATA'
C
            CASE (-2)
              ID1 = 'COULD NOT READ FILEPATH'
C
            CASE (-3)
              ID1 = 'UNKNOWN WRITEMODE'
C
            CASE (-4)
              ID1 = 'TOO FEW INPUTS'
C
            CASE (-5)
              ID1 = '"datasetType" IS INVALID OR MISSING'
C
            CASE (-6)
              ID1 = 'UNSUPPORTED DATASET'
C
            CASE (-7)
              ID1 = 'EXPECTED STRUCT INPUT IS NOT A STRUCTURE'
C
            CASE (-9)
              ID1 = 'FILE LUN IS NOT OPEN'
C
            CASE (-10)
              ID1 = 'DATA IS NOT AN ARRAY, OR WRONGLY SIZED'
C
            CASE (-11)
              ID1 = 'DATA CASE TYPE IS UNSUPPORTED'
C
            CASE (-12)
              ID1 = 'FLOATING POINT ARITHMETIC OVERFLOW (FROM IOSTAT)'
C
            CASE (-13)
              ID1 = 'FILEPATH IS INVALID OR INACCESSIBLE'
C
            CASE (-14)
              ID1 = '"dataFormatType" IS INVALID (STREAMING MODE)'
C
            CASE (-15)
              ID1 = 'X-DATA FIELD NOT PRESENT OR EMPTY'
C
            CASE (-16)
              ID1 = 'MISMATCHING X-DATA- AND DATA SIZES'
C
            CASE (-17)
              ID1 = '"ordNumDataType" IS INVALID'
C
            CASE (-18)
              ID1 = 'INVALID "fileType" IN UNV-151'
C
            CASE (-19)
              ID1 = 'INVALID "unitsCode" IN UNV-164'
C
            CASE (-20)
              ID1 = 'INVALID DIRECTION NUMBER IN UNV-58, VALID: [-6 6]'
C
            CASE DEFAULT
              ID1 = 'Read error: check IOSTAT code.'
C
          END SELECT
          plhs(2) = mxCreateString(TRIM(ID1))
        END IF
      END IF
C
C
      IF (ALLOCATED(FILEPATH))  DEALLOCATE(FILEPATH)
      IF (ALLOCATED(XDATA))     DEALLOCATE(XDATA)
      IF (ALLOCATED(XDATA_D))   DEALLOCATE(XDATA_D)
      IF (ALLOCATED(Y_RS))      DEALLOCATE(Y_RS)
      IF (ALLOCATED(Y_RD))      DEALLOCATE(Y_RD)
      IF (ALLOCATED(Y_CS))      DEALLOCATE(Y_CS)
      IF (ALLOCATED(Y_CD))      DEALLOCATE(Y_CD)
C
C
C
      IF (NOT(KEEPFILEOPEN) .AND. LIDX > 0) THEN
        INQUIRE(UNIT=LUN(LIDX), OPENED=OK)
        IF (OK) THEN
          CLOSE(LUN(LIDX), STATUS='KEEP')
        END IF
        LUN(LIDX) = 0
      END IF
C
C
C
C
C ----------------------------------------------------------------------
C
      CONTAINS
C
C ----------------------------------------------------------------------
C
C
C       CONVERT A WORD TO LOWER CASE
        ELEMENTAL FUNCTION LOWER(WORD) RESULT(Y)
C       ----------------------------------------------------------------
C
        IMPLICIT NONE
C
        CHARACTER (LEN=*), INTENT(IN) :: WORD
        CHARACTER (LEN=LEN(WORD)) :: Y
        INTEGER :: I, IC, NLEN
C       ----------------------------------------------------------------
C
        NLEN = LEN(WORD)
        DO I = 1,NLEN
          IC = ICHAR(WORD(I:I))
          IF (IC >= 65 .AND. IC < 90) THEN
            Y(I:I) = CHAR(IC+32)
          ELSE
            Y(I:I) = WORD(I:I)
          ENDIF
        ENDDO
C
        END FUNCTION LOWER
C       ----------------------------------------------------------------
C
C
C
C
      END SUBROUTINE
C
C
C
C
C ----------------------------------------------------------------------
C --- END OF MAIN PROGRAM ----------------------------------------------
C ----------------------------------------------------------------------
C
C
C
