#include "fintrf.h"


C ----------------------------------------------------------------------
C
C     [STATE CASENUM] = WRITEUNV(FILEPATH, STRUCT, WRITEMODE, IDX)
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
C       = -20, INVALID DIRECTION NUMBER IN UNV-58, VALID RANGE: [-6 6]
C       = >1,  ERROR CODE FROM IOSTAT WHEN OPENING FILE
C
C     CASENUM:  CASENUM TO USE WHEN STREAMING DATA [DOUBLE]
C               IF NOT SUCCESSFUL -> CHAR WITH ERROR DESCRIPTION [CHAR]
C
C ----------------------------------------------------------------------


      SUBROUTINE mexFunction(nlhs, plhs, nrhs, prhs)

      IMPLICIT NONE

C     GATEWAY VARIABLES
      mwPointer plhs(*), prhs(*)
      mwSize :: nlhs, nrhs

C     FUNCTIONS
      mwPointer :: mxCreateDoubleScalar
      mwPointer :: mxCreateString
      mwPointer :: mxGetDoubles
      mwPointer :: mxGetSingles
      mwPointer :: mxGetComplexDoubles
      mwPointer :: mxGetComplexSingles
      mwPointer :: mxGetField
      mwPointer :: mxGetN
      mwPointer :: mxGetNumberOfElements
      INTEGER*4 :: mxIsEmpty
      INTEGER*4 :: mxIsChar
      INTEGER*4 :: mxIsStruct
      INTEGER*4 :: mxIsDouble
      INTEGER*4 :: mxIsComplex
      INTEGER*4 :: mxGetString
      LOGICAL   :: ISEMPTY
      LOGICAL   :: ISVALID_DOUBLE
      LOGICAL   :: ISVALID_CHAR

C     MEX. DERIVED TYPES
      mwPointer :: PTR
      mwPointer :: XPTR
      mwSize    :: NVAL, NVALX, STRLEN
      mwSize, PARAMETER :: N1 = 1

      INTEGER*4, PARAMETER :: MAXIDX = 512
      CHARACTER(LEN=7)     :: FILEACT
      LOGICAL   :: KEEPFILEOPEN, WRITEDATA, ISEVEN, OPENLUN
      LOGICAL*1 :: USE_BINARY
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
      REAL*8    :: S, T
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


C     ------------------------------------------------------------------


C     INITIALISE VARIABLES
      S = 0
      LIDX = 0
      CASENUM = 0
      KEEPFILEOPEN = .FALSE.



C --- CLOSE ALL OPENED FILES, OR CONTINUE ------------------------------
C ----------------------------------------------------------------------
      IF (nrhs < 1) THEN ! EMPTY CALL
        IF (MAXVAL(LUN) > 0) THEN
          DO K=1,SIZE(LUN)
            IF (LUN(K) > 0) THEN
              CLOSE(LUN(K), STATUS='KEEP')
              LUN(K) = 0
            END IF
          END DO
          S = 1
        END IF
        LUN = 0
        NUMPTS = 0
        REMPTS = 0
C       GO TO END SECTION
        GO TO 10

      ELSEIF (nrhs < 2) THEN ! TOO FEW INPUTS
        S = -4
C       GO TO END SECTION
        GO TO 10

      ELSE
C       CONTINUE BELOW

      END IF
C ----------------------------------------------------------------------
C ----------------------------------------------------------------------




C --- DETERMINE DESIRED WRITING MODE -----------------------------------
C ----------------------------------------------------------------------
      OPENLUN = .FALSE.
      IF (nrhs < 3) THEN
        WRITEMODE = 1
      ELSE
        CALL mxCopyPtrToReal8(mxGetDoubles(prhs(3)), T, N1)
        WRITEMODE = T
      END IF

      IF (nrhs > 3) THEN
        CALL mxCopyPtrToReal8(mxGetDoubles(prhs(4)), T, N1)
        LIDX = T
        IF (LIDX > 0 .AND. LIDX <= MAXIDX) THEN
          INQUIRE(UNIT=LUN(LIDX), OPENED=OK)
          IF (WRITEMODE > 1) THEN
            IF (NOT(OK)) THEN
              S = -9
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




C --- OPEN FILE IF NEEDED/DESIRED --------------------------------------
C ----------------------------------------------------------------------
      IF (WRITEMODE == 1 .OR. LIDX < 1 .OR. OPENLUN) THEN
C       CREATE NEW FILE LUN

C       CHECK IF ARGUMENT IS EMPTY
        NOK = mxIsEmpty(prhs(1))
        IF (NOK) THEN ! IF EMPTY -> ERROR
          S = -2
          GO TO 10
        END IF

C       CHECK IF INPUT ARGUMENT IS OF CHAR TYPE
        OK = mxIsChar(prhs(1))
        IF (NOT(OK)) THEN ! IF NOT CHAR -> ERROR
          S = -2
          GO TO 10
        END IF

        STRLEN = mxGetN(prhs(1))
        ALLOCATE(CHARACTER(LEN=STRLEN) :: FILEPATH)

        NOK = mxGetString(prhs(1), FILEPATH, STRLEN)

        IF (NOK) THEN ! COULD NOT GET FILEPATH STRING FROM INPUT ARGUMENT
          S = -2
          GO TO 10
        END IF


C       ASSIGN DEFAULT INDEX IF MISSING OR OUT OF BOUNDS
        IF (LIDX < 1 .OR. LIDX > MAXIDX) THEN
          LIDX = 1
        ELSEIF (LUN(LIDX) == 0) THEN
          LUN(LIDX) = LIDX + 1
        END IF


C       DETERMINE IF FILE IS TO BE REPLACED OR NOT (APPENDED)
        IF (WRITEMODE == 1) THEN
          FILEACT = 'REPLACE'
        ELSE
          FILEACT = 'OLD'
        END IF


C       IF DESIRED LUN INDEX ALREADY MAPS TO AN OPEN LUN, CLOSE IT
        INQUIRE(UNIT=LUN(LIDX), OPENED=OK)
        IF (OK) THEN
          CLOSE(LUN(LIDX), STATUS='KEEP')
        END IF


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

        REMPTS(LIDX) = 0
        NUMPTS(LIDX) = 0
        REC0POS(LIDX) = 0
        REC7POS(LIDX) = 0
        REC0BYTE(LIDX) = 0

        IF (nrhs < 4) THEN
          KEEPFILEOPEN = .FALSE. ! CLOSE FILE BEFORE RETURNING
        ELSE
          KEEPFILEOPEN = .TRUE. ! KEEP FILE LUN OPEN FOR NEXT FUNCTION CALL
        END IF


      ELSE ! DO NOT CREATE NEW FILE LUN

        KEEPFILEOPEN = .TRUE.

      END IF
C ----------------------------------------------------------------------
C ----------------------------------------------------------------------






C --- ONLY WRITE STREAMING DATA ----------------------------------------
C ----------------------------------------------------------------------
      IF (WRITEMODE == 3) THEN

        IF (nrhs < 4) THEN
          S = -4
          GO TO 10
        END IF

        CALL mxCopyPtrToReal8(mxGetDoubles(prhs(1)), T, N1)
        CASENUM = T


        IF (CASENUM == -1) THEN ! FINALISE WRITING

          IF (LUN(LIDX) > 0) THEN

            INQUIRE(UNIT=LUN(LIDX), OPENED=OK, NAME=FILESTR)
            IF (NOT(OK)) THEN
              S = -9
              GO TO 10
            END IF

C           FINISH LAST LINE
            WRITE(LUN(LIDX),'(1X)',ERR=9)

C           WRITE UNV TERMINATOR, -1
            WRITE(LUN(LIDX),'(I6)') -1

C           CLOSE FILE
            CLOSE(LUN(LIDX),STATUS='KEEP')

            OPEN(
     &        UNIT = LUN(LIDX),
     &        FILE = FILESTR,
     &        ACCESS = 'STREAM',
     &        STATUS = 'OLD',
     &        ERR = 8,
     &        FORM = 'UNFORMATTED',
     &        ACTION = 'WRITE')

            IF (REC0POS(LIDX) /= 0) THEN ! IF NOT ZERO, WRITE NUMBER OF BYTES TO 58b ID RECORD
              WRITE(STR12, '(I12)') NUMPTS(LIDX)*REC0BYTE(LIDX)
              WRITE(LUN(LIDX), ERR=9, POS=REC0POS(LIDX)+31) STR12
            END IF
            WRITE(STR10, '(I10)') NUMPTS(LIDX)
            WRITE(LUN(LIDX), ERR=9, POS=REC7POS(LIDX)+10) STR10
            CLOSE(LUN(LIDX), STATUS='KEEP')

            S = 1

          ELSE

            S = -9

          END IF

          LUN(LIDX) = 0
          NUMPTS(LIDX) = 0
          REMPTS(LIDX) = 0
          REC0POS(LIDX) = 0
          REC0BYTE(LIDX) = 0
          REC7POS(LIDX) = 0

          GO TO 10 ! RETURN


        ELSE ! WRITE DATA


          NOK = mxIsEmpty(prhs(2))

          IF (NOK) THEN
            S = -10
            GO TO 10
          END IF

          NVAL = mxGetNumberOfElements(prhs(2))

          NUMPTS(LIDX) = NUMPTS(LIDX) + NVAL
          STEP = REMPTS(LIDX)

          IF (REC0POS(LIDX) /= 0) THEN
            USE_BINARY = .TRUE.
          ELSE
            USE_BINARY = .FALSE.
          END IF


C ------- WRITING STREAMING DATA / CASE 1 ----------------------------
          IF (CASENUM == 1) THEN ! REAL SINGLE, X-EVENLY-SPACED

            ALLOCATE(Y_RS(NVAL))
            PTR = mxGetSingles(prhs(2))
            CALL mxCopyPtrToReal4(PTR, Y_RS, NVAL)

            IF (USE_BINARY) THEN

              WRITE(LUN(LIDX), '(<NVAL>A4)', ERR=9, ADVANCE='NO')
     &          (Y_RS(I), I=1,NVAL)
              REMPTS(LIDX) = 0

            ELSE

              PTLIM = 6

              IF (STEP /= 0) THEN ! IF THERE IS UNFINISHED DATA, 0 -> FINISHED ROW
                IF (STEP >= NVAL) THEN ! IF NUMBER VALUES TO WRITE IS <= REQUIRED AMOUNT TO FINISH ROW
                  WRITE(LUN(LIDX), 1001, ERR=9, ADVANCE='NO')
     &              (Y_RS(I), I=1,NVAL)
                  REMPTS(LIDX) = REMPTS(LIDX)-NVAL
                  S = 1 ! SUCCESS
                  GO TO 10 ! RETURN
                ELSE ! MORE DATA AVAILABLE AFTER FINISH OF CURRENT LINE
C                 ADD PARTIAL DATA, REQUIRED AMOUNT TO FINISH ROW
                  WRITE(LUN(LIDX), 1001, ERR=9, ADVANCE='NO')
     &              (Y_RS(I), I = 1,STEP)
                END IF
              END IF

              N = STEP

              IF (N == NVAL) THEN
                REMPTS(LIDX) = 0
              ELSE
                WRITE(LUN(LIDX), '(1X)',ERR=9) ! IF PREVIOUS WRITE WAS FINISHED, OR END OF HEADER -> ADVANCE
                STEP = (NVAL-N) - MOD(NVAL-N, PTLIM) ! WRITE NO PARTIAL NUMBER OF LINES
                IF (N+STEP == NVAL) THEN
                  STEP = STEP-PTLIM ! ONLY WRITE UP UNTIL THE SECOND-TO-LAST LINE, WITH ADVANCE=YES
                END IF

C               WRITE 1:END-1 COMPLETE LINES, WITH ADVANCE
                IF (STEP > 0) THEN
                  WRITE(LUN(LIDX), 1001, ERR=9, ADVANCE='YES')
     &              (Y_RS(I+N), I = 1,STEP)
                END IF

                N = N + STEP

C               WRITE LAST (PARTIAL/COMPLETE) LINE, WITH NO ADVANCE
                WRITE(LUN(LIDX), 1001, ERR=9, ADVANCE='NO')
     &            (Y_RS(I), I = N+1,NVAL)

                REMPTS(LIDX) = PTLIM-(NVAL-N) ! POINTS LEFT TO WRITE

              END IF

            END IF ! END OF ASCII/BINARY SWITCH

            S = 1 ! SUCCESS




C ------- WRITING STREAMING DATA / CASE 3 ----------------------------
          ELSEIF (CASENUM == 3) THEN ! COMPLEX SINGLE, X-EVEN

            ALLOCATE(Y_CS(NVAL))
            PTR = mxGetComplexSingles(prhs(2))
            CALL mxCopyPtrToComplex8(PTR, Y_CS, NVAL)

            IF (USE_BINARY) THEN

              WRITE(LUN(LIDX), '(<NVAL>(A4,A4))', ERR=9, ADVANCE='NO')
     &          (Y_CS(I), I=1,NVAL)
              REMPTS(LIDX) = 0

            ELSE

              PTLIM = 3

              IF (STEP /= 0) THEN ! IF THERE IS UNFINISHED DATA, 0 -> FINISHED ROW
                IF (STEP >= NVAL) THEN ! IF NUMBER VALUES TO WRITE IS <= REQUIRED AMOUNT TO FINISH ROW
                    WRITE(LUN(LIDX), 1003, ERR=9, ADVANCE='NO')
     &                (Y_CS(I), I=1,NVAL)
                  REMPTS(LIDX) = REMPTS(LIDX)-NVAL
                  S = 1 ! SUCCESS
                  GO TO 10 ! RETURN
                ELSE ! MORE DATA AVAILABLE AFTER FINISH OF CURRENT LINE
C                 ADD PARTIAL DATA, REQUIRED AMOUNT TO FINISH ROW
                  WRITE(LUN(LIDX), 1003, ERR=9, ADVANCE='NO')
     &              (Y_CS(I), I = 1,STEP)
                END IF
              END IF

              N = STEP

              IF (N == NVAL) THEN
                REMPTS(LIDX) = 0
              ELSE
                WRITE(LUN(LIDX), '(1X)', ERR=9) ! IF PREVIOUS WRITE WAS FINISHED, OR END OF HEADER -> ADVANCE
                STEP = (NVAL-N) - MOD(NVAL-N, PTLIM) ! WRITE NO PARTIAL NUMBER OF LINES
                IF (N+STEP == NVAL) THEN
                  STEP = STEP-PTLIM ! ONLY WRITE UP UNTIL THE SECOND-TO-LAST LINE, WITH ADVANCE=YES
                END IF

C               WRITE 1:END-1 COMPLETE LINES, WITH ADVANCE
                IF (STEP > 0) THEN
                  WRITE(LUN(LIDX), 1003, ERR=9, ADVANCE='YES')
     &              (Y_CS(I+N), I = 1,STEP)
                END IF

                N = N + STEP

C               WRITE LAST (PARTIAL/COMPLETE) LINE, WITH NO ADVANCE
                WRITE(LUN(LIDX), 1003, ERR=9, ADVANCE='NO')
     &            (Y_CS(I), I = N+1,NVAL)

                REMPTS(LIDX) = PTLIM-(NVAL-N) ! POINTS LEFT TO WRITE

              END IF

            END IF ! END OF ASCII/BINARY SWITCH

            S = 1 ! SUCCESS




C ------- WRITING STREAMING DATA / CASE 5 ----------------------------
          ELSEIF (CASENUM == 5) THEN ! REAL DOUBLE, X-EVENLY-SPACED

            ALLOCATE(Y_RD(NVAL))

            PTR = mxGetDoubles(prhs(2))
            CALL mxCopyPtrToReal8(PTR, Y_RD, NVAL)

            IF (USE_BINARY) THEN

              WRITE(LUN(LIDX), '(<NVAL>(A8))', ERR=9, ADVANCE='NO')
     &          (Y_RD(I), I=1,NVAL)
              REMPTS(LIDX) = 0

            ELSE

              PTLIM = 4

              IF (STEP /= 0) THEN ! IF THERE IS UNFINISHED DATA, 0 -> FINISHED ROW
                IF (STEP >= NVAL) THEN ! IF NUMBER VALUES TO WRITE IS <= REQUIRED AMOUNT TO FINISH ROW
                  WRITE(LUN(LIDX), 1005, ERR=9, ADVANCE='NO')
     &              (Y_RD(I), I=1,NVAL)
                  REMPTS(LIDX) = REMPTS(LIDX)-NVAL
                  S = 1 ! SUCCESS
                  GO TO 10 ! RETURN
                ELSE ! MORE DATA AVAILABLE AFTER FINISH OF CURRENT LINE
C                 ADD PARTIAL DATA, REQUIRED AMOUNT TO FINISH ROW
                  WRITE(LUN(LIDX), 1005, ERR=9, ADVANCE='NO')
     &              (Y_RD(I), I = 1,STEP)
                END IF
              END IF

              N = STEP

              IF (N == NVAL) THEN
                REMPTS(LIDX) = 0
              ELSE
                WRITE(LUN(LIDX), '(1X)', ERR=9) ! IF PREVIOUS WRITE WAS FINISHED, OR END OF HEADER -> ADVANCE
                STEP = (NVAL-N) - MOD(NVAL-N, PTLIM) ! WRITE NO PARTIAL NUMBER OF LINES
                IF (N+STEP == NVAL) THEN
                  STEP = STEP-PTLIM ! ONLY WRITE UP UNTIL THE SECOND-TO-LAST LINE, WITH ADVANCE=YES
                END IF

C               WRITE 1:END-1 COMPLETE LINES, WITH ADVANCE
                IF (STEP > 0) THEN
                  WRITE(LUN(LIDX), 1005, ERR=9, ADVANCE='YES')
     &            (Y_RD(I+N), I = 1,STEP)
                END IF

                N = N + STEP

C               WRITE LAST (PARTIAL/COMPLETE) LINE, WITH NO ADVANCE
                WRITE(LUN(LIDX), 1005, ERR=9, ADVANCE='NO')
     &            (Y_RD(I), I = N+1,NVAL)

                REMPTS(LIDX) = PTLIM-(NVAL-N) ! POINTS LEFT TO WRITE

              END IF

            END IF ! END OF ASCII/BINARY SWITCH

            S = 1 ! SUCCESS




C ------- WRITING STREAMING DATA / CASE 7 ----------------------------
          ELSEIF (CASENUM == 7) THEN ! COMPLEX DOUBLE, X-EVEN

            ALLOCATE(Y_CD(NVAL))
            PTR = mxGetComplexDoubles(prhs(2))
            CALL mxCopyPtrToComplex16(PTR, Y_CD, NVAL)

            IF (USE_BINARY) THEN

              WRITE(LUN(LIDX), '(<NVAL>(A8,A8))', ERR=9, ADVANCE='NO')
     &          (Y_CD(I), I=1,NVAL)
              REMPTS(LIDX) = 0

            ELSE

              PTLIM = 2

              IF (STEP /= 0) THEN ! IF THERE IS UNFINISHED DATA, 0 -> FINISHED ROW
                IF (STEP >= NVAL) THEN ! IF NUMBER VALUES TO WRITE IS <= REQUIRED AMOUNT TO FINISH ROW
                    WRITE(LUN(LIDX), 1007, ERR=9, ADVANCE='NO')
     &              (Y_CD(I), I=1,NVAL)
                  REMPTS(LIDX) = REMPTS(LIDX)-NVAL
                  S = 1 ! SUCCESS
                  GO TO 10 ! RETURN
                ELSE ! MORE DATA AVAILABLE AFTER FINISH OF CURRENT LINE
C                   ADD PARTIAL DATA, REQUIRED AMOUNT TO FINISH ROW
                    WRITE(LUN(LIDX), 1007, ERR=9, ADVANCE='NO')
     &                (Y_CD(I), I = 1,STEP)
                END IF
              END IF

              N = STEP

              IF (N == NVAL) THEN
                REMPTS(LIDX) = 0
              ELSE
                WRITE(LUN(LIDX), '(1X)', ERR=9) ! IF PREVIOUS WRITE WAS FINISHED, OR END OF HEADER -> ADVANCE
                STEP = (NVAL-N) - MOD(NVAL-N, PTLIM) ! WRITE NO PARTIAL NUMBER OF LINES
                IF (N+STEP == NVAL) THEN
                  STEP = STEP-PTLIM ! ONLY WRITE UP UNTIL THE SECOND-TO-LAST LINE, WITH ADVANCE=YES
                END IF

C               WRITE 1:END-1 COMPLETE LINES, WITH ADVANCE
                IF (STEP > 0) THEN
                  WRITE(LUN(LIDX), 1007, ERR=9, ADVANCE='YES')
     &              (Y_CD(I+N), I = 1,STEP)
                END IF

                N = N + STEP

C               WRITE LAST (PARTIAL/COMPLETE) LINE, WITH NO ADVANCE
                WRITE(LUN(LIDX), 1007, ERR=9, ADVANCE='NO')
     &            (Y_CD(I), I = N+1,NVAL)

                REMPTS(LIDX) = PTLIM-(NVAL-N) ! POINTS LEFT TO WRITE

              END IF

            END IF ! END OF ASCII/BINARY SWITCH

            S = 1 ! SUCCESS



          ELSE
            S = -11
            GO TO 10


          END IF

        END IF

        GO TO 10



C ----------------------------------------------------------------------
C ----------------------------------------------------------------------




C --- NORMAL WRITING MODE ----------------------------------------------
C ----------------------------------------------------------------------
      ELSE


C --- GET DATASET TYPE TO WRITE
      OK = mxIsStruct(prhs(2))
      IF (NOT(OK)) THEN
        S = -7
        GO TO 10
      END IF


      PTR = mxGetField(prhs(2), 1, 'datasetType')
      IF (NOT(ISVALID_DOUBLE(PTR))) THEN
        S = -5
        GO TO 10
      END IF
      CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
      DATASETNUM = T



C --- DATASET 151 ------------------------------------------------------
C ----------------------------------------------------------------------
      IF (DATASETNUM == 151) THEN


C       RECORD 1, ID LINE 1, model file name
        PTR = mxGetField(prhs(2), 1, 'modelName')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          ID1 = 'NONE'
        ELSE
          NOK = mxGetString(PTR, ID1, 80)
          IF (ADJUSTL(ID1) == ' ') ID1 = 'NONE'
        END IF

C       RECORD 2, ID LINE 2, model file description
        PTR = mxGetField(prhs(2), 1, 'modelDescription')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          ID2 = 'NONE'
        ELSE
          NOK = mxGetString(PTR, ID2, 80)
          IF (ADJUSTL(ID2) == ' ') ID2 = 'NONE'
        END IF

C       RECORD 3, ID LINE 3, program which created DB
        PTR = mxGetField(prhs(2), 1, 'databaseProgram')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          ID3 = 'WHTA WRITEUNV'
        ELSE
          NOK = mxGetString(PTR, ID3, 80)
          IF (ADJUSTL(ID3) == ' ') ID3 = 'WHTA WRITEUNV'
        END IF

C       RECORD 4
C       4.1
        PTR = mxGetField(prhs(2), 1, 'databaseCreatedDate')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          DS151F41 = 'DD-MMM-YY'
        END IF
        NOK = mxGetString(PTR, DS151F41, 10)

C       4.2
        PTR = mxGetField(prhs(2), 1, 'databaseCreatedTime')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          DS151F42 = 'HH:MM:SS'
        ELSE
          NOK = mxGetString(PTR, DS151F42, 10)
        END IF

C       4.3
        PTR = mxGetField(prhs(2), 1, 'databaseVersion1')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          DS151F43 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          DS151F43 = T
        END IF

C       4.4
        PTR = mxGetField(prhs(2), 1, 'databaseVersion2')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          DS151F44 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          DS151F44 = T
        END IF

C       4.5
        PTR = mxGetField(prhs(2), 1, 'fileType')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          DS151F45 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          DS151F45 = T
          IF (NOT(ANY(DS151F45 == (/0,1,2/)))) THEN
            S = -18
            GO TO 10
          END IF
        END IF

C       RECORD 5
C       5.1
        PTR = mxGetField(prhs(2), 1, 'databaseLastSavedDate')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          DS151F51 = DS151F41
        ELSE
          NOK = mxGetString(PTR, DS151F51, 10)
        END IF

C       5.2
        PTR = mxGetField(prhs(2), 1, 'databaseLastSavedTime')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          DS151F52 = DS151F42
        ELSE
          NOK = mxGetString(PTR, DS151F52, 10)
        END IF

C       RECORD 6, ID LINE 6, program which created unv-file
        PTR = mxGetField(prhs(2), 1, 'unvProgram')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          ID4 = 'WHTA WRITEUNV'
        ELSE
          NOK = mxGetString(PTR, ID4, 80)
          IF (ADJUSTL(ID3) == ' ') ID4 = 'WHTA WRITEUNV'
        END IF

C       RECORD 7
C       7.1
        PTR = mxGetField(prhs(2), 1, 'fileCreatedDate')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          DS151F71 = DS151F41
        ELSE
          NOK = mxGetString(PTR, DS151F71, 10)
        END IF

C       7.2
        PTR = mxGetField(prhs(2), 1, 'fileCreatedTime')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          DS151F72 = DS151F42
        ELSE
          NOK = mxGetString(PTR, DS151F72, 10)
        END IF


C ----- WRITE ALL FIELDS
C
C       START OF RECORD, -1
        WRITE(LUN(LIDX), '(I6)', ERR=9) -1

C       DATASET NUMBER
        WRITE(LUN(LIDX), '(I6)', ERR=9) DATASETNUM

C       RECORD 1
        WRITE(LUN(LIDX), '(A80)', ERR=9) ID1

C       RECORD 2
        WRITE(LUN(LIDX), '(A80)', ERR=9) ID2

C       RECORD 3
        WRITE(LUN(LIDX),'(A80)', ERR=9) ID3

C       RECORD 4
        WRITE(LUN(LIDX), 1514, ERR=9)
     &    DS151F41, DS151F42, DS151F43, DS151F44, DS151F45
1514    FORMAT(A10, A10, 3I10)

C       RECORD 5
        WRITE(LUN(LIDX), 1515, ERR=9) DS151F51, DS151F52
1515    FORMAT(A10, A10)

C       RECORD 6
        WRITE(LUN(LIDX), '(A80)', ERR=9) ID4

C       RECORD 7
        WRITE(LUN(LIDX), 1517, ERR=9) DS151F71, DS151F72
1517    FORMAT(A10, A10)

C       END OF RECORD, -1
        WRITE(LUN(LIDX), '(I6)', ERR=9) -1


C ----- WRITIING FINISHED

        S = 1 ! SUCCESS
        GO TO 10 ! GO TO END




C --- DATASET 164 ------------------------------------------------------
C ----------------------------------------------------------------------
      ELSEIF (DATASETNUM == 164) THEN


C       RECORD 1
C       1.1
        PTR = mxGetField(prhs(2), 1, 'unitsCode')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          DS164F11 = 1
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          DS164F11 = T
          IF (NOT(ANY(DS164F11==(/1,2,3,4,5,6,7,8,9/)))) THEN
            S = -19
            GO TO 10
          END IF
        END IF

C       1.2
        PTR = mxGetField(prhs(2), 1, 'unitsDescription')
        IF (NOT(ISVALID_CHAR(PTR))) THEN

          SELECT CASE (DS164F11)
            CASE (1)
              DS164F12 = 'SI: Meter (newton)'
            CASE (2)
              DS164F12 = 'BG: Foot (pound f)'
            CASE (3)
              DS164F12 = 'MG: Meter (kilogram f)'
            CASE (4)
              DS164F12 = 'BA: Foot (poundal)'
            CASE (5)
              DS164F12 = 'MM: mm (milli newton)'
            CASE (6)
              DS164F12 = 'CM: cm (centi newton)'
            CASE (7)
              DS164F12 = 'IN: Inch (pound f)'
            CASE (8)
              DS164F12 = 'GM: mm (kilogram f)'
            CASE (9)
              DS164F12 = 'US: USER_DEFINED'
            CASE DEFAULT
              GO TO 10 ! INVALID UNIT CODE
          END SELECT

        ELSE
          NOK = mxGetString(PTR, DS164F12, 20)

        END IF

C       1.3
        PTR = mxGetField(prhs(2), 1, 'temperatureMode')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          DS164F13 = 1
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          DS164F13 = T
        END IF

C       RECORD 2
C       2.1
        PTR = mxGetField(prhs(2), 1, 'length')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          DS164F21 = 1
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), DS164F21, N1)
        END IF

C       2.2
        PTR = mxGetField(prhs(2), 1, 'force')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          DS164F22 = 1
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), DS164F22, N1)
        END IF

C       2.3
        PTR = mxGetField(prhs(2), 1, 'temperature')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          DS164F23 = 1
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), DS164F23, N1)
        END IF

C       2.4
        PTR = mxGetField(prhs(2), 1, 'temperatureOffset')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          DS164F24 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), DS164F24, N1)
        END IF



C ----- WRITE ALL FIELDS
C
C       START OF RECORD, -1
        WRITE(LUN(LIDX), '(I6)', ERR=9) -1

C       DATASET NUMBER
        WRITE(LUN(LIDX), '(I6)', ERR=9) DATASETNUM

C       RECORD 1
        WRITE(LUN(LIDX), 1641, ERR=9) DS164F11, DS164F12, DS164F13
1641    FORMAT(I10, A20, I10)

C       RECORD 2
        WRITE(LUN(LIDX), 1642, ERR=9)
     &    DS164F21, DS164F22, DS164F23, DS164F24
1642    FORMAT(3ES25.17)

C       END OF RECORD, -1
        WRITE(LUN(LIDX), '(I6)', ERR=9) -1


C ----- WRITIING FINISHED



        S = 1 ! SUCCESS
        GO TO 10 ! GO TO END



C --- DATASET 58 -------------------------------------------------------
C ----------------------------------------------------------------------
      ELSEIF (DATASETNUM == 58) THEN


C       RECORD 1, ID LINE 1
        PTR = mxGetField(prhs(2), 1, 'ID1')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          ID1 = 'NONE'
        ELSE
          NOK = mxGetString(PTR, ID1, 80)
          IF (ADJUSTL(ID1) == ' ') ID1 = 'NONE'
        END IF

C       RECORD 2, ID LINE 2
        PTR = mxGetField(prhs(2), 1, 'ID2')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          ID2 = 'NONE'
        ELSE
          NOK = mxGetString(PTR, ID2, 80)
          IF (ADJUSTL(ID2) == ' ') ID2 = 'NONE'
        END IF

C       RECORD 3, date
        PTR = mxGetField(prhs(2), 1, 'date')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          ID3 = 'UNKNOWN DATE'
        ELSE
          NOK = mxGetString(PTR, ID3, 80)
        END IF

C       RECORD 4, ID LINE 4
        PTR = mxGetField(prhs(2), 1, 'ID4')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          ID4 = 'NONE'
        ELSE
          NOK = mxGetString(PTR, ID4, 80)
          IF (ADJUSTL(ID4) == ' ') ID4 = 'NONE'
        END IF

C       RECORD 5, ID LINE 5
        PTR = mxGetField(prhs(2), 1, 'ID5')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          ID5 = 'NONE'
        ELSE
          NOK = mxGetString(PTR, ID5, 80)
          IF (ADJUSTL(ID5) == ' ') ID5 = 'NONE'
        END IF

C       RECORD 6
C       6.1
        PTR = mxGetField(prhs(2), 1, 'functionType')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F61 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F61 = T
        END IF

C       6.2
        PTR = mxGetField(prhs(2), 1, 'functionID')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F62 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F62 = T
        END IF

C       6.3
        PTR = mxGetField(prhs(2), 1, 'versionNumber')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F63 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F63 = T
        END IF

C       6.4
        PTR = mxGetField(prhs(2), 1, 'loadCaseID')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F64 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F64 = T
        END IF

C       6.5
        PTR = mxGetField(prhs(2), 1, 'rspEntName')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          F65 = 'NONE'
        ELSE
          NOK = mxGetString(PTR, F65, 10)
          IF (ADJUSTL(F65) == ' ') F65 = 'NONE'
        END IF

C       6.6
        PTR = mxGetField(prhs(2), 1, 'rspNode')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F66 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F66 = T
        END IF

C       6.7
        PTR = mxGetField(prhs(2), 1, 'rspDir')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F67 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F67 = T
          IF (F67 < -6 .OR. F67 > 6) THEN
            S = -20
            GO TO 10
          END IF
        END IF

C       6.8
        PTR = mxGetField(prhs(2), 1, 'refEntName')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          F68 = 'NONE'
        ELSE
          NOK = mxGetString(PTR, F68, 10)
          IF (ADJUSTL(F68) == ' ') F68 = 'NONE'
        END IF

C       6.9
        PTR = mxGetField(prhs(2), 1, 'refNode')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F69 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F69 = T
        END IF

C       6.10
        PTR = mxGetField(prhs(2), 1, 'refDir')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F610 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F610 = T
          IF (F610 < -6 .OR. F610 > 6) THEN
            S = -20
            GO TO 10
          END IF
        END IF

C       RECORD 7
C       7.1
        PTR = mxGetField(prhs(2), 1, 'dataFormatType')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F71 = -1
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F71 = T
        END IF

C       7.2
        PTR = mxGetField(prhs(2), 1, 'numValues')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F72 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F72 = T
        END IF

C       7.3
        PTR = mxGetField(prhs(2), 1, 'evenSpacing')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F73 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F73 = T
        END IF

C       7.4
        PTR = mxGetField(prhs(2), 1, 'x0')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F74 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F74 = T
        END IF

C       7.5
        PTR = mxGetField(prhs(2), 1, 'dx')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F75 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F75 = T
        END IF

C       7.6
        PTR = mxGetField(prhs(2), 1, 'zAxisValue')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F76 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F76 = T
        END IF

C       RECORD 8
C       8.1
        PTR = mxGetField(prhs(2), 1, 'abscDataType')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F81 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F81 = T
        END IF

C       8.2
        PTR = mxGetField(prhs(2), 1, 'abscLengthUnitsExponent')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F82 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F82 = T
        END IF

C       8.3
        PTR = mxGetField(prhs(2), 1, 'abscForceUnitsExponent')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F83 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F83 = T
        END IF

C       8.4
        PTR = mxGetField(prhs(2), 1, 'abscTempUnitsExponent')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F84 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F84 = T
        END IF

C       8.5
        PTR = mxGetField(prhs(2), 1, 'abscAxisLabel')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          F85 = 'NONE'
        ELSE
          NOK = mxGetString(PTR, F85, 20)
        END IF

C       8.6
        PTR = mxGetField(prhs(2), 1, 'abscAxisUnitLabel')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          F86 = 'NONE'
        ELSE
          NOK = mxGetString(PTR, F86, 20)
        END IF

C       RECORD 9
C       9.1
        PTR = mxGetField(prhs(2), 1, 'ordNumDataType')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          S = -17
          GO TO 10
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F91 = T
        END IF

C       9.2
        PTR = mxGetField(prhs(2), 1, 'ordNumLengthUnitsExponent')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F92 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F92 = T
        END IF

C       9.3
        PTR = mxGetField(prhs(2), 1, 'ordNumForceUnitsExponent')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F93 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F93 = T
        END IF

C       9.4
        PTR = mxGetField(prhs(2), 1, 'ordNumTempUnitsExponent')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F94 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F94 = T
        END IF

C       9.5
        PTR = mxGetField(prhs(2), 1, 'ordNumAxisLabel')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          F95 = 'NONE'
        ELSE
          NOK = mxGetString(PTR, F95, 20)
        END IF

C       9.6
        PTR = mxGetField(prhs(2), 1, 'ordNumAxisUnitLabel')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          F96 = 'NONE'
        ELSE
          NOK = mxGetString(PTR, F96, 20)
        END IF

C       RECORD 10
C       10.1
        PTR = mxGetField(prhs(2), 1, 'ordDenomDataType')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F101 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F101 = T
        END IF

C       10.2
        PTR = mxGetField(prhs(2), 1, 'ordDenomLengthUnitsExponent')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F102 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F102 = T
        END IF

C       10.3
        PTR = mxGetField(prhs(2), 1, 'ordDenomForceUnitsExponent')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F103 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F103 = T
        END IF

C       10.4
        PTR = mxGetField(prhs(2), 1, 'ordDenomTempUnitsExponent')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F104 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F104 = T
        END IF

C       10.5
        PTR = mxGetField(prhs(2), 1, 'ordDenomAxisLabel')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          F105 = 'NONE'
        ELSE
          NOK = mxGetString(PTR, F105, 20)
          IF (ADJUSTL(F105) == ' ') F105 = 'NONE'
        END IF

C       10.6
        PTR = mxGetField(prhs(2), 1, 'ordDenomAxisUnitLabel')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          F106 = 'NONE'
        ELSE
          NOK = mxGetString(PTR, F106, 20)
          IF (ADJUSTL(F106) == ' ') F106 = 'NONE'
        END IF

C       RECORD 11
C       11.1
        PTR = mxGetField(prhs(2), 1, 'zDataType')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F111 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F111 = T
        END IF

C       11.2
        PTR = mxGetField(prhs(2), 1, 'zLengthUnitsExponent')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F112 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F112 = T
        END IF

C       11.3
        PTR = mxGetField(prhs(2), 1, 'zForceUnitsExponent')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F113 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F113 = T
        END IF

C       11.4
        PTR = mxGetField(prhs(2), 1, 'zTempUnitsExponent')
        IF (NOT(ISVALID_DOUBLE(PTR))) THEN
          F114 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F114 = T
        END IF

C       11.5
        PTR = mxGetField(prhs(2), 1, 'zAxisLabel')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          F115 = 'NONE'
        ELSE
          NOK = mxGetString(PTR, F115, 20)
          IF (ADJUSTL(F115) == ' ') F115 = 'NONE'
        END IF

C       11.6
        PTR = mxGetField(prhs(2), 1, 'zAxisUnitLabel')
        IF (NOT(ISVALID_CHAR(PTR))) THEN
          F116 = 'NONE'
        ELSE
          NOK = mxGetString(PTR, F116, 20)
          IF (ADJUSTL(F116) == ' ') F116 = 'NONE'
        END IF

        ISEVEN = F73

        IF (ISEVEN == .FALSE.) THEN
          F74 = 0
          F75 = 0
        END IF


C       CHECK IF DATA SHOULD BE WRITTEN
        PTR = mxGetField(prhs(2), 1, 'data')
        IF (ISEMPTY(PTR)) THEN

          IF (NOT(ANY(F71 == (/2,4,5,6/) ))) THEN
            S = -14
            GO TO 10
          END IF

          WRITEDATA = .FALSE.

        ELSE

          WRITEDATA = .TRUE.

C         GET DATATYPE OF DATA: SINGLE/DOUBLE, REAL/COMPLEX ?
          ISDOUBLE = mxIsDouble(PTR)
          ISCOMPLEX = mxIsComplex(PTR)
          NVAL = mxGetNumberOfElements(PTR)

C         GET CASENUM FROM DATATYPE AND X-EVENNESS
          CASENUM = 1
          IF (ISEVEN == .FALSE.) THEN
            CASENUM = CASENUM + 1 ! = UNEVEN X-DATA
            XPTR = mxGetField(prhs(2), 1, 'x')
            IF (ISEMPTY(XPTR)) THEN
              S = -15
              GO TO 10 ! X-DATA FIELD NOT PRESENT OR EMPTY
            END IF

            NVALX = mxGetNumberOfElements(XPTR)

            IF (NVALX /= NVAL) THEN
              S = -16
              GO TO 10 ! MISMATCHING X-DATA
            END IF

            IF (NOT(ALLOCATED(XDATA))) THEN
              ALLOCATE(XDATA(NVAL))
            ELSE IF(SIZE(XDATA) /= NVAL) THEN
              DEALLOCATE(XDATA)
              ALLOCATE(XDATA(NVAL))
            END IF

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

          IF (ISCOMPLEX) THEN
            CASENUM = CASENUM + 2 ! = COMPLEX
          END IF
          IF (ISDOUBLE) THEN
            CASENUM = CASENUM + 4 ! = DOUBLE
          END IF

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

          ELSEIF (CASENUM > 4) THEN ! REAL DOUBLE
            IF (NOT(ALLOCATED(Y_RD))) THEN
              ALLOCATE(Y_RD(NVAL))
            ELSE IF(SIZE(Y_RD) /= NVAL) THEN
              DEALLOCATE(Y_RD)
              ALLOCATE(Y_RD(NVAL))
            END IF
            CALL mxCopyPtrToReal8(mxGetDoubles(PTR), Y_RD, NVAL)
            F71 = 4

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

          ELSE ! REAL SINGLE
            IF (NOT(ALLOCATED(Y_RS))) THEN
              ALLOCATE(Y_RS(NVAL))
            ELSE IF(SIZE(Y_RS) /= NVAL) THEN
              DEALLOCATE(Y_RS)
              ALLOCATE(Y_RS(NVAL))
            END IF
            CALL mxCopyPtrToReal4(mxGetSingles(PTR), Y_RS, NVAL)
            F71 = 2

          END IF

          F72 = NVAL

        END IF


C       SHOULD BE WRITTEN AS ASCII OR BINARY?
        PTR = mxGetField(prhs(2), 1, 'dataType')
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



C ----- WRITE ALL FIELDS
C
C       START OF RECORD, -1
        WRITE(LUN(LIDX), '(I6)', ERR=9) -1

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

C       RECORD 1
        WRITE(LUN(LIDX), '(A80)', ERR=9) ID1

C       RECORD 2
        WRITE(LUN(LIDX), '(A80)', ERR=9) ID2

C       RECORD 3
        WRITE(LUN(LIDX), '(A80)', ERR=9) ID3

C       RECORD 4
        WRITE(LUN(LIDX), '(A80)', ERR=9) ID4

C       RECORD 5
        WRITE(LUN(LIDX), '(A80)', ERR=9) ID5

C       RECORD 6
5806    FORMAT(2(I5, I10), 2(1X, A10, I10, I4))
        WRITE(LUN(LIDX), 5806, ERR=9)
     &    F61, F62, F63, F64, F65, F66, F67, F68, F69, F610

C       RECORD 7
5807    FORMAT(3I10, 3ES13.5)
        INQUIRE(LUN(LIDX), POS=REC7POS(LIDX))
        WRITE(LUN(LIDX), 5807, ERR=9) F71, F72, F73, F74, F75, F76

C       RECORD 8
5808    FORMAT(I10, 3I5, 2(1X,A20))
        WRITE(LUN(LIDX), 5808, ERR=9) F81, F82, F83, F84, F85, F86

C       RECORD 9
5809    FORMAT(I10, 3I5, 2(1X, A20))
        WRITE(LUN(LIDX), 5809, ERR=9) F91, F92, F93, F94, F95, F96

C       RECORD 10
5810    FORMAT(I10, 3I5, 2(1X, A20))
        WRITE(LUN(LIDX), 5810, ERR=9) F101, F102, F103, F104, F105, F106

C       RECORD 11
5811    FORMAT(I10, 3I5, 2(1X, A20))
        IF (WRITEDATA) THEN ! IF DATA IS TO BE WRITTEN IN THIS CALL
          WRITE(LUN(LIDX), 5811, ERR=9, ADVANCE='YES')
     &      F111, F112, F113, F114, F115, F116
        ELSE ! DATA WILL COME IN NEXT FUNCTION CALL
          WRITE(LUN(LIDX), 5811, ERR=9, ADVANCE='NO')
     &      F111, F112, F113, F114, F115, F116
        END IF

C       RECORD 12, DATA, IF TO BE WRITTEN
        IF (WRITEDATA) THEN

          N = 0 ! INITIALISE NUMBER OF VALUES WRITTEN
          SELECT CASE (CASENUM)
           CASE (1) ! REAL SINGLE DATA, EVEN X-DATA
            IF (NOT(USE_BINARY)) THEN
1001          FORMAT(6ES13.5)
              WRITE(LUN(LIDX),1001,ERR=9) (Y_RS(I), I=1,NVAL)
            ELSE
              WRITE(LUN(LIDX),'(<NVAL>A4)',ERR=9) (Y_RS(I), I=1,NVAL)
            END IF

           CASE (2) ! REAL SINGLE DATA, UNEVEN X-DATA
            IF (NOT(USE_BINARY)) THEN
1002          FORMAT(6ES13.5)
              WRITE(LUN(LIDX),1002,ERR=9) (XDATA(I), Y_RS(I), I=1,NVAL)
            ELSE
              WRITE(LUN(LIDX),'(<NVAL>(A4,A4))',ERR=9)
     &          (XDATA(I), Y_RS(I), I=1,NVAL)
            END IF

           CASE (3) ! COMPLEX SINGLE DATA, EVEN X-DATA
            IF (NOT(USE_BINARY)) THEN
1003          FORMAT(6ES13.5)
              WRITE(LUN(LIDX),1003,ERR=9) (Y_CS(I), I=1,NVAL)
            ELSE
              WRITE(LUN(LIDX),'(<NVAL>(A4,A4))',ERR=9)
     &          (Y_CS(I), I=1,NVAL)
            END IF

           CASE (4) ! COMPLEX SINGLE DATA, UNEVEN X-DATA
            IF (NOT(USE_BINARY)) THEN
1004          FORMAT(6ES13.5)
              WRITE(LUN(LIDX),1004,ERR=9) (XDATA(I), Y_CS(I), I=1,NVAL)
            ELSE
              WRITE(LUN(LIDX),'(<NVAL>(A4,A4,A4))',ERR=9)
     &          (XDATA(I), Y_CS(I), I=1,NVAL)
            END IF

           CASE (5) ! REAL DOUBLE DATA, EVEN X-DATA
            IF (NOT(USE_BINARY)) THEN
1005          FORMAT(4ES20.12)
              WRITE(LUN(LIDX), 1005, ERR=9) (Y_RD(I), I=1,NVAL)
            ELSE
              WRITE(LUN(LIDX),'(<NVAL>A8)',ERR=9) (Y_RD(I), I=1,NVAL)
            END IF

           CASE (6) ! REAL DOUBLE DATA, UNEVEN X-DATA
            IF (NOT(USE_BINARY)) THEN
1006          FORMAT(2(ES13.5, ES20.12))
              WRITE(LUN(LIDX),1006,ERR=9) (XDATA(I), Y_RD(I), I=1,NVAL)
            ELSE
              WRITE(LUN(LIDX),'(<NVAL>(A4, A8))',ERR=9)
     &          (XDATA(I), Y_RD(I), I=1,NVAL)
            END IF

           CASE (7) ! COMPLEX DOUBLE DATA, EVEN X-DATA
            IF (NOT(USE_BINARY)) THEN
1007          FORMAT(4ES20.12)
              WRITE(LUN(LIDX),1007,ERR=9) (Y_CD(I), I=1,NVAL)
            ELSE
              WRITE(LUN(LIDX),'(<NVAL>(A8,A8))',ERR=9)
     &         (Y_CD(I), I=1,NVAL)
            END IF

           CASE (8) ! COMPLEX DOUBLE DATA, UNEVEN X-DATA
            IF (NOT(USE_BINARY)) THEN
1008          FORMAT(ES13.5, 2ES20.12)
              WRITE(LUN(LIDX),1008,ERR=9) (XDATA(I), Y_CD(I), I=1,NVAL)
            ELSE
              WRITE(LUN(LIDX),'(<NVAL>(A4,A8,A8))',ERR=9)
     &          (XDATA(I), Y_CD(I), I=1,NVAL)
            END IF

           CASE DEFAULT
            GO TO 10 ! ERROR: BAD CASENUM, SHOULD ALREADY BE CHECKED ABOVE

          END SELECT

C         END OF RECORD, -1, IF DATA IS WRITTEN, OTHERWISE IT IS KEPT UNTERMINATED
          WRITE(LUN(LIDX), '(I6)', ERR=9) -1


        ELSE ! KEEP UNTERMINATED

          IF (USE_BINARY) THEN
            WRITE(LUN(LIDX), '(1X)', ERR=9) ! FINISH LAST LINE, BEFORE BINARY STREAM
          END IF


C         INIT. CASENUM
          CASENUM = 1

C         DETERMINE CASENUM
          ISDOUBLE = (F71 == 4 .OR. F71 == 6) ! IS DATA OF TYPE DOUBLE, OR NOT -> SINGLE
          ISCOMPLEX = (F71 > 4) ! IS DATA COMPLEX- (TRUE) OR REAL-VALUED (FALSE)

          IF (NOT(ISEVEN)) THEN
            CASENUM = CASENUM + 1 ! = UN-EVEN
          END IF
          IF (ISCOMPLEX) THEN
            CASENUM = CASENUM + 2 ! = COMPLEX
          END IF
          IF (ISDOUBLE) THEN
            CASENUM = CASENUM + 4 ! = DOUBLE
          END IF

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

        END IF

C
C ----- WRITIING FINISHED

        S = 1 ! SUCCESS
        GO TO 10


      ELSE ! UNKNOWN TYPE

        S = -6
        S = DATASETNUM
        GO TO 10

      END IF

      END IF



C --- FINISHED WRITING -------------------------------------------------
C ----------------------------------------------------------------------



C --- RETURN ARGUMENTS AND CLOSE FILES AND VARIABLES

      S = -90 ! SHOULD NEVER HAPPEN -> ERROR IN CODE
      GO TO 10

8     CONTINUE

      IF (OPENSTAT == 1) THEN ! REMAP IOSTAT ERROR CODE 1 TO -12
        S = -12
      ELSEIF (OPENSTAT == 29) THEN ! REMAP IOSTAT ERROR CODE 29 TO -13
        S = -13
      ELSE
        S = OPENSTAT
      END IF
      GO TO 10

C     UNKNOWN ERROR EXIT
9     S = -1
      KEEPFILEOPEN = .FALSE.


10    CONTINUE ! NORMAL/ASSIGNED EXIT

      plhs(1) = mxCreateDoubleScalar(S)
      IF (nlhs > 1) THEN
        IF (INT(S) == 1) THEN ! SUCCESSFUL

          plhs(2) = mxCreateDoubleScalar(DBLE(CASENUM))

        ELSE ! NOT SUCCESSFUL -> RETURN ERROR DESCRIPTION
          K = S
          SELECT CASE (K)
            CASE (0)
              ID1 = 'Unknown error.'

            CASE (-1)
              ID1 = 'ERROR WRITING FIELDS/DATA'

            CASE (-2)
              ID1 = 'COULD NOT READ FILEPATH'

            CASE (-3)
              ID1 = 'UNKNOWN WRITEMODE'

            CASE (-4)
              ID1 = 'TOO FEW INPUTS'

            CASE (-7)
              ID1 = 'EXPECTED STRUCT INPUT IS NOT A STRUCTURE'

            CASE (-9)
              ID1 = 'FILE LUN IS NOT OPEN'

            CASE (-10)
              ID1 = 'DATA IS NOT AN ARRAY, OR WRONGLY SIZED'

            CASE (-11)
              ID1 = 'DATA CASE TYPE IS UNSUPPORTED'

            CASE (-12)
              ID1 = 'FLOATING POINT ARITHMETIC OVERFLOW (FROM IOSTAT)'

            CASE (-13)
              ID1 = 'FILEPATH IS INVALID OR INACCESSIBLE'

            CASE (-14)
              ID1 = '"dataFormatType" IS INVALID (STREAMING MODE)'

            CASE (-15)
              ID1 = 'X-DATA FIELD NOT PRESENT OR EMPTY'

            CASE (-16)
              ID1 = 'MISMATCHING X-DATA- AND DATA SIZES'

            CASE (-17)
              ID1 = '"ordNumDataType" IS INVALID'

            CASE (-18)
              ID1 = 'INVALID "fileType" IN UNV-151'

            CASE (-19)
              ID1 = 'INVALID "unitsCode" IN UNV-164'

            CASE (-20)
              ID1 = 'INVALID DIRECTION NUMBER IN UNV-58, VALID: [-6 6]'

            CASE DEFAULT
              ID1 = 'Read error: check IOSTAT code.'

          END SELECT
          plhs(2) = mxCreateString(TRIM(ID1))
        END IF
      END IF


      IF (ALLOCATED(FILEPATH))  DEALLOCATE(FILEPATH)
      IF (ALLOCATED(XDATA))     DEALLOCATE(XDATA)
      IF (ALLOCATED(XDATA_D))   DEALLOCATE(XDATA_D)
      IF (ALLOCATED(Y_RS))      DEALLOCATE(Y_RS)
      IF (ALLOCATED(Y_RD))      DEALLOCATE(Y_RD)
      IF (ALLOCATED(Y_CS))      DEALLOCATE(Y_CS)
      IF (ALLOCATED(Y_CD))      DEALLOCATE(Y_CD)



      IF (NOT(KEEPFILEOPEN) .AND. LIDX > 0) THEN
        INQUIRE(UNIT=LUN(LIDX), OPENED=OK)
        IF (OK) THEN
          CLOSE(LUN(LIDX), STATUS='KEEP')
        END IF
        LUN(LIDX) = 0
      END IF




C ----------------------------------------------------------------------

      CONTAINS

C ----------------------------------------------------------------------


C       CONVERT A WORD TO LOWER CASE
        ELEMENTAL FUNCTION LOWER(WORD) result(y)
C       ----------------------------------------------------------------

        IMPLICIT NONE

        CHARACTER (LEN=*) , INTENT(IN) :: WORD
        CHARACTER (LEN=LEN(WORD)) :: y
        INTEGER :: I,IC,NLEN

        NLEN = LEN(WORD)
        DO I=1,NLEN
          IC = ICHAR(WORD(I:I))
          IF (IC >= 65 .AND. IC < 90) THEN
            y(I:I) = CHAR(IC+32)
          ELSE
            y(I:I) = WORD(I:I)
          ENDIF
        ENDDO

        END FUNCTION LOWER
C       ----------------------------------------------------------------




      END SUBROUTINE




C ----------------------------------------------------------------------
C --- END OF MAIN PROGRAM ----------------------------------------------
C ----------------------------------------------------------------------



