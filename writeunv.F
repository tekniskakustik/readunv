#include "fintrf.h"


C ------------------------------------------------------------------------------
C
C     STATE = WRITEUNV(FILEPATH, STRUCT, WRITEMODE, IDX)
C
C     WRITEUNV(FILEPATH, STRUCT) -> APPEND DATA (FROM STRUCT), CREATE FILE IF NEEDED
C     WRITEUNV(FILEPATH, STRUCT, WRITEMODE) | WRITEMODE = 0 (APPEND), WRITEMODE = 1 (REPLACE), [WRITEMODE] = UINT8
C     WRITEUNV(FILEPATH, STRUCT, 0/1, IDX) -> CREATE FILE ACCORDING TO WRITEMODE, KEEP FILE OPEN, CURRENT FILE = IDX (IDX MUST <= MAXIDX)
C     WRITEUNV([], STRUCT, 2, IDX) -> WRITE INTO LUN(IDX), FILEPATH IS IGNORED
C     WRITEUNV(CASENUM, DATA, 3, IDX, DATASET) -> STREAM DATA INTO <IDX>, FILEPATH IS REPLACED WITH CASENUM FOR DATA WRITING
C     WRITEUNV(-1, DATA, 3, IDX, DATASET) -> TERMINATE FILE <IDX>, AND WRITE NUMBER OF VALUES INTO HEADER
C     WRITEUNV() -> CLOSE ALL OPEN FILES AND ZERO LUN
C     <STATE IS ALWAYS RETURNED>
C     STATE FLAGS:
C       =  1,  SUCCESFUL
C       =  0,  UNKNOWN ERROR
C       = -1,  ERROR WRITING FIELDS/DATA
C       = -2,  COULD NOT READ FILEPATH
C       = -3,  UNKNOWN WRITEMODE
C       = -4,  TOO FEW INPUTS
C       = -5,  MISSING DATASET NUMBER
C       = -6,  UNSUPPORTED DATASET NUMBER
C       = -7,  EXPECTED STRUCT INPUT IS NOT A STRUCTURE
C       = -8,  TOO FEW FIELDS, OR BADLY FORMATTED FIELDS, IN STRUCTURE
C       = -9,  FILE LUN IS NOT OPEN
C       = -10, DATA IS NOT AN ARRAY, OR WRONGLY SIZED
C       = -11, DATA CASE TYPE IS UNSUPPORTED
C       = -12, FLOATING POINT ARITHMETIC OVERFLOW (FROM IOSTAT)
C       = >1,  ERROR CODE FROM IOSTAT WHEN OPENING FILE
C
C ------------------------------------------------------------------------------


      SUBROUTINE mexFunction(nlhs, plhs, nrhs, prhs)

      IMPLICIT NONE

      mwPointer plhs(*), prhs(*)
      mwSize :: nlhs, nrhs


      mwPointer :: mxCreateDoubleScalar
      mwPointer :: mxGetDoubles
      mwPointer :: mxGetSingles
      mwPointer :: mxGetComplexDoubles
      mwPointer :: mxGetComplexSingles
      mwPointer :: mxGetNumberOfFields
      mwPointer :: mxGetField
      mwPointer :: mxIsEmpty
      mwPointer :: mxIsChar
      mwPointer :: mxIsStruct
      mwPointer :: mxIsDouble
      mwPointer :: mxIsComplex
      mwPointer :: mxGetN
      mwPointer :: mxGetM
      mwPointer :: mxGetString
      mwPointer :: PTR
      mwPointer :: XPTR
      mwSize :: NVAL, NVALX, STRLEN
      mwSize, PARAMETER :: N1 = 1


      INTEGER*4, PARAMETER :: MAXIDX = 512
      CHARACTER(LEN=7) :: FILEACT
      LOGICAL :: KEEPFILEOPEN, WRITEDATA, ISEVEN, OPENLUN
      INTEGER*1 :: WRITEMODE, PTLIM
      INTEGER*4 :: LIDX, OK, NOK, NINPUTF, CASENUM, OPENSTAT
      INTEGER*4 :: DATASETNUM, ISCOMPLEX, ISDOUBLE(2)
      INTEGER*4 :: DS151F43, DS151F44, DS151F45
      INTEGER*4 :: DS164F11, DS164F13
      INTEGER*4 :: F61, F62, F63, F64, F66, F67, F69, F610
      INTEGER*4 :: F71, F72, F73
      INTEGER*4 :: F81, F82, F83, F84
      INTEGER*4 :: F91, F92, F93, F94
      INTEGER*4 :: F101, F102, F103, F104
      INTEGER*4 :: F111, F112, F113, F114
      INTEGER*8 :: I, K, N, FIELDSTOUSE, STEP
      REAL*4 :: F74, F75, F76
      REAL*8 :: S(1), T
      REAL*8 :: DS164F21, DS164F22, DS164F23, DS164F24
      INTEGER*1, SAVE :: REMPTS(MAXIDX) = 0
      INTEGER*2, SAVE :: LUN(MAXIDX) = 0
      INTEGER*8, SAVE :: REC7POS(MAXIDX) = 0
      INTEGER*8, SAVE :: NUMPTS(MAXIDX) = 0
      REAL*4, DIMENSION(:), ALLOCATABLE :: XDATA
      REAL*4, DIMENSION(:), ALLOCATABLE :: Y_RS
      REAL*8, DIMENSION(:), ALLOCATABLE :: Y_RD
      COMPLEX*8, DIMENSION(:), ALLOCATABLE :: Y_CS
      COMPLEX*16, DIMENSION(:), ALLOCATABLE :: Y_CD
      CHARACTER(LEN=:), ALLOCATABLE :: FILEPATH
      CHARACTER(LEN=10) :: DS151F41, DS151F42
      CHARACTER(LEN=10) :: DS151F51, DS151F52
      CHARACTER(LEN=10) :: DS151F71, DS151F72
      CHARACTER(LEN=10) :: F65, F68
      CHARACTER(LEN=10) :: STR10
      CHARACTER(LEN=20) :: DS164F12
      CHARACTER(LEN=20) :: F85, F86
      CHARACTER(LEN=20) :: F95, F96
      CHARACTER(LEN=20) :: F105, F106
      CHARACTER(LEN=20) :: F115, F116
      CHARACTER(LEN=80) :: ID1, ID2, ID3, ID4, ID5
      CHARACTER(LEN=512) :: FILESTR




      S = 0.0
      LIDX = 0
      CASENUM = 0
      KEEPFILEOPEN = .FALSE.




C --- CLOSE ALL OPENED FILES, OR CONTINUE --------------------------------------
C ------------------------------------------------------------------------------
      IF (nrhs < 1) THEN ! EMPTY CALL
        IF (MAXVAL(LUN) > 0) THEN
          DO K=1,SIZE(LUN)
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
        GO TO 10 ! GO TO END SECTION

      ELSEIF (nrhs < 2) THEN ! TOO FEW INPUTS
        S = -4.0
        GO TO 10  ! GO TO END SECTION

      ELSE
C       CONTINUE BELOW

      END IF
C ------------------------------------------------------------------------------
C ------------------------------------------------------------------------------




C --- DETERMINE DESIRED WRITING MODE -------------------------------------------
C ------------------------------------------------------------------------------
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
        ELSE IF (LIDX < 0 .OR. LIDX > MAXIDX) THEN
          LIDX = 0
        END IF
      END IF
C ------------------------------------------------------------------------------
C ------------------------------------------------------------------------------




C --- OPEN FILE IF NEEDED/DESIRED ----------------------------------------------
C ------------------------------------------------------------------------------
      IF (WRITEMODE == 1 .OR. LIDX < 1 .OR. OPENLUN) THEN ! CREATE NEW FILE LUN

C       CHECK IF ARGUMENT IS EMPTY
        NOK = mxIsEmpty(prhs(1))
        IF (NOK == 1) THEN ! IF EMPTY -> ERROR
          S = -2.0
          GO TO 10
        END IF

C       CHECK IF INPUT ARGUMENT IS OF CHAR TYPE
        OK = mxIsChar(prhs(1))
        IF (NOT(OK)) THEN ! IF NOT CHAR -> ERROR
          S = -2.0
          GO TO 10
        END IF

        STRLEN = mxGetN(prhs(1))
        ALLOCATE(CHARACTER(LEN=STRLEN) :: FILEPATH)

        NOK = mxGetString(prhs(1), FILEPATH, STRLEN)

        IF (NOK /= 0) THEN ! COULD NOT GET FILEPATH STRING FROM INPUT ARGUMENT
          S = -2.0
          GO TO 10
        END IF


C       ASSIGN DEFAULT INDEX IF MISSING OR OUT OF BOUNDS
        IF (LIDX < 1 .OR. LIDX > MAXIDX) THEN
          LIDX = 1
        ELSE IF (LUN(LIDX) == 0) THEN
          LUN(LIDX) = LIDX + 1
        END IF


C       DETERMINE IF FILE IS TO BE REPLACED OR NOT (APPENDED)
        IF (WRITEMODE /= 0) THEN
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
        OPEN(UNIT=LUN(LIDX),
     &    FILE=FILEPATH,
     &    ACCESS='STREAM',
     &    STATUS=FILEACT,
     &    ERR=8,
     &    FORM='FORMATTED',
     &    POSITION='APPEND',
     &    IOSTAT=OPENSTAT,
     &    ACTION='READWRITE')

        REMPTS(LIDX) = 0
        NUMPTS(LIDX) = 0
        REC7POS(LIDX) = 0

        IF (nrhs < 4) THEN
          KEEPFILEOPEN = .FALSE. ! CLOSE FILE BEFORE RETURNING
        ELSE
          KEEPFILEOPEN = .TRUE. ! KEEP FILE LUN OPEN FOR NEXT FUNCTION CALL
        END IF


      ELSE ! DO NOT CREATE NEW FILE LUN

        KEEPFILEOPEN = .TRUE.

      END IF
C ------------------------------------------------------------------------------
C ------------------------------------------------------------------------------






C --- ONLY WRITE STREAMING DATA ------------------------------------------------
C ------------------------------------------------------------------------------
      IF (WRITEMODE == 3) THEN

        IF (nrhs < 5) THEN
          S = -5
          GO TO 10
        END IF

        CALL mxCopyPtrToReal8(mxGetDoubles(prhs(1)), T, N1)
        CASENUM = T

        CALL mxCopyPtrToReal8(mxGetDoubles(prhs(5)), T, N1)
        DATASETNUM = T


        IF (DATASETNUM == 58) THEN

          IF (CASENUM == -1) THEN ! FINALISE WRITING

            IF (LUN(LIDX) > 0) THEN

              INQUIRE(UNIT=LUN(LIDX), OPENED=OK, NAME=FILESTR)
              IF (NOT(OK)) THEN
                S = -9
                GO TO 10
              END IF

C             FINISH LAST LINE IF LEFT UNFILLED
              IF (REMPTS(LIDX) > 0) THEN
C                 WRITE(LUN(LIDX),'(A4)',ERR=9) ''
C                 WRITE(LUN(LIDX),'(1X)',ERR=9)
              END IF
              WRITE(LUN(LIDX),'(1X)',ERR=9)

              WRITE(LUN(LIDX),'(I6)') -1
              CLOSE(LUN(LIDX),STATUS='KEEP')

              OPEN(UNIT=LUN(LIDX),
     &          FILE=FILESTR,
     &          ACCESS='STREAM',
     &          STATUS='OLD',
     &          ERR=8,
     &          FORM='UNFORMATTED',
     &          ACTION='WRITE')

              WRITE(STR10,'(I10)') NUMPTS(LIDX)
              WRITE(LUN(LIDX),ERR=9,POS=REC7POS(LIDX)+10) STR10
              CLOSE(LUN(LIDX),STATUS='KEEP')

              S = REMPTS(LIDX)
C               S = 1.0

            ELSE

              S = -9.0

            END IF

            LUN(LIDX) = 0
            NUMPTS(LIDX) = 0
            REMPTS(LIDX) = 0

            GO TO 10 ! RETURN


          ELSE

            NVAL = mxGetM(prhs(2))
            N = mxGetN(prhs(2))


            IF (N > 1 .OR. NVAL < 1) THEN
              S = -10
              GO TO 10
            END IF

            NUMPTS(LIDX) = NUMPTS(LIDX) + NVAL
            STEP = REMPTS(LIDX)


C --------- WRITING STREAMING DATA / CASE 1 ------------------------------------
            IF (CASENUM == 1) THEN ! REAL SINGLE, X-EVENLY-SPACED

              ALLOCATE(Y_RS(NVAL))

              PTLIM = 6

              PTR = mxGetSingles(prhs(2))
              CALL mxCopyPtrToReal4(PTR, Y_RS, NVAL)

              IF (STEP /= 0) THEN ! IF THERE IS UNFINISHED DATA, 0 -> FINISHED ROW
                IF (STEP >= NVAL) THEN ! IF NUMBER VALUES TO WRITE IS <= REQUIRED AMOUNT TO FINISH ROW
                    WRITE(LUN(LIDX),1001,ERR=9,ADVANCE='NO')
     &                (Y_RS(I), I=1,NVAL)
                  REMPTS(LIDX) = REMPTS(LIDX)-NVAL
                  S = 1.0 ! SUCCESS
                  GO TO 10 ! RETURN
                ELSE ! MORE DATA AVAILABLE AFTER FINISH OF CURRENT LINE
C                   ADD PARTIAL DATA, REQUIRED AMOUNT TO FINISH ROW
                    WRITE(LUN(LIDX),1001,ERR=9,ADVANCE='NO')
     &                (Y_RS(I), I=1,STEP)
                END IF
              END IF

              N = STEP

              IF (N == NVAL) THEN
                REMPTS(LIDX) = 0
              ELSE
                WRITE(LUN(LIDX),'(1X)',ERR=9) ! IF PREVIOUS WRITE WAS FINISHED, OR END OF HEADER -> ADVANCE
                STEP = (NVAL-N) - MOD(NVAL-N, PTLIM) ! WRITE NO PARTIAL NUMBER OF LINES
                IF (N+STEP == NVAL) THEN
                  STEP = STEP-PTLIM ! ONLY WRITE UP UNTIL THE SECOND-TO-LAST LINE, WITH ADVANCE=YES
                END IF

C               WRITE 1:END-1 COMPLETE LINES, WITH ADVANCE
                IF (STEP > 0) THEN
                  WRITE(LUN(LIDX),1001,ERR=9,ADVANCE='YES')
     &              (Y_RS(I+N), I=1,STEP)
                END IF

                N = N + STEP

C               WRITE LAST (PARTIAL/COMPLETE) LINE, WITH NO ADVANCE
                WRITE(LUN(LIDX),1001,ERR=9,ADVANCE='NO')
     &              (Y_RS(I), I=N+1,NVAL)

                REMPTS(LIDX) = PTLIM-(NVAL-N) ! POINTS LEFT TO WRITE

              END IF
              S = 1.0 ! SUCCESS




C --------- WRITING STREAMING DATA / CASE 3 ------------------------------------
            ELSE IF (CASENUM == 3) THEN ! COMPLES SINGLE, X-EVENLY-SPACED

              ALLOCATE(Y_CS(NVAL))

              PTLIM = 3

              PTR = mxGetComplexSingles(prhs(2))
              CALL mxCopyPtrToComplex8(PTR, Y_CS, NVAL)

              IF (STEP /= 0) THEN ! IF THERE IS UNFINISHED DATA, 0 -> FINISHED ROW
                IF (STEP >= NVAL) THEN ! IF NUMBER VALUES TO WRITE IS <= REQUIRED AMOUNT TO FINISH ROW
                    WRITE(LUN(LIDX),1003,ERR=9,ADVANCE='NO')
     &                (Y_CS(I), I=1,NVAL)
                  REMPTS(LIDX) = REMPTS(LIDX)-NVAL
                  S = 1.0 ! SUCCESS
                  GO TO 10 ! RETURN
                ELSE ! MORE DATA AVAILABLE AFTER FINISH OF CURRENT LINE
C                   ADD PARTIAL DATA, REQUIRED AMOUNT TO FINISH ROW
                    WRITE(LUN(LIDX),1003,ERR=9,ADVANCE='NO')
     &                (Y_CS(I), I=1,STEP)
                END IF
              END IF

              N = STEP

              IF (N == NVAL) THEN
                REMPTS(LIDX) = 0
              ELSE
                WRITE(LUN(LIDX),'(1X)',ERR=9) ! IF PREVIOUS WRITE WAS FINISHED, OR END OF HEADER -> ADVANCE
                STEP = (NVAL-N) - MOD(NVAL-N, PTLIM) ! WRITE NO PARTIAL NUMBER OF LINES
                IF (N+STEP == NVAL) THEN
                  STEP = STEP-PTLIM ! ONLY WRITE UP UNTIL THE SECOND-TO-LAST LINE, WITH ADVANCE=YES
                END IF

C               WRITE 1:END-1 COMPLETE LINES, WITH ADVANCE
                IF (STEP > 0) THEN
                  WRITE(LUN(LIDX),1003,ERR=9,ADVANCE='YES')
     &              (Y_CS(I+N), I=1,STEP)
                END IF

                N = N + STEP

C               WRITE LAST (PARTIAL/COMPLETE) LINE, WITH NO ADVANCE
                WRITE(LUN(LIDX),1003,ERR=9,ADVANCE='NO')
     &              (Y_CS(I), I=N+1,NVAL)

                REMPTS(LIDX) = PTLIM-(NVAL-N) ! POINTS LEFT TO WRITE

              END IF
              S = 1.0 ! SUCCESS




C --------- WRITING STREAMING DATA / CASE 5 ------------------------------------
            ELSE IF (CASENUM == 5) THEN ! REAL DOUBLE, X-EVENLY-SPACED

              ALLOCATE(Y_RD(NVAL))

              PTLIM = 4

              PTR = mxGetDoubles(prhs(2))
              CALL mxCopyPtrToReal8(PTR, Y_RD, NVAL)

              IF (STEP /= 0) THEN ! IF THERE IS UNFINISHED DATA, 0 -> FINISHED ROW
                IF (STEP >= NVAL) THEN ! IF NUMBER VALUES TO WRITE IS <= REQUIRED AMOUNT TO FINISH ROW
                    WRITE(LUN(LIDX),1005,ERR=9,ADVANCE='NO')
     &                (Y_RD(I), I=1,NVAL)
                  REMPTS(LIDX) = REMPTS(LIDX)-NVAL
                  S = 1.0 ! SUCCESS
                  GO TO 10 ! RETURN
                ELSE ! MORE DATA AVAILABLE AFTER FINISH OF CURRENT LINE
C                   ADD PARTIAL DATA, REQUIRED AMOUNT TO FINISH ROW
                    WRITE(LUN(LIDX),1005,ERR=9,ADVANCE='NO')
     &                (Y_RD(I), I=1,STEP)
                END IF
              END IF

              N = STEP

              IF (N == NVAL) THEN
                REMPTS(LIDX) = 0
              ELSE
                WRITE(LUN(LIDX),'(1X)',ERR=9) ! IF PREVIOUS WRITE WAS FINISHED, OR END OF HEADER -> ADVANCE
                STEP = (NVAL-N) - MOD(NVAL-N, PTLIM) ! WRITE NO PARTIAL NUMBER OF LINES
                IF (N+STEP == NVAL) THEN
                  STEP = STEP-PTLIM ! ONLY WRITE UP UNTIL THE SECOND-TO-LAST LINE, WITH ADVANCE=YES
                END IF

C               WRITE 1:END-1 COMPLETE LINES, WITH ADVANCE
                IF (STEP > 0) THEN
                  WRITE(LUN(LIDX),1005,ERR=9,ADVANCE='YES')
     &              (Y_RD(I+N), I=1,STEP)
                END IF

                N = N + STEP

C               WRITE LAST (PARTIAL/COMPLETE) LINE, WITH NO ADVANCE
                WRITE(LUN(LIDX),1005,ERR=9,ADVANCE='NO')
     &              (Y_RD(I), I=N+1,NVAL)

                REMPTS(LIDX) = PTLIM-(NVAL-N) ! POINTS LEFT TO WRITE

              END IF
              S = 1.0 ! SUCCESS




C --------- WRITING STREAMING DATA / CASE 7 ------------------------------------
            ELSE IF (CASENUM == 7) THEN ! COMPLES DOUBLE, X-EVENLY-SPACED

              ALLOCATE(Y_CD(NVAL))

              PTLIM = 2

              PTR = mxGetComplexDoubles(prhs(2))
              CALL mxCopyPtrToComplex16(PTR, Y_CD, NVAL)

              IF (STEP /= 0) THEN ! IF THERE IS UNFINISHED DATA, 0 -> FINISHED ROW
                IF (STEP >= NVAL) THEN ! IF NUMBER VALUES TO WRITE IS <= REQUIRED AMOUNT TO FINISH ROW
                    WRITE(LUN(LIDX),1007,ERR=9,ADVANCE='NO')
     &                (Y_CD(I), I=1,NVAL)
                  REMPTS(LIDX) = REMPTS(LIDX)-NVAL
                  S = 1.0 ! SUCCESS
                  GO TO 10 ! RETURN
                ELSE ! MORE DATA AVAILABLE AFTER FINISH OF CURRENT LINE
C                   ADD PARTIAL DATA, REQUIRED AMOUNT TO FINISH ROW
                    WRITE(LUN(LIDX),1007,ERR=9,ADVANCE='NO')
     &                (Y_CD(I), I=1,STEP)
                END IF
              END IF

              N = STEP

              IF (N == NVAL) THEN
                REMPTS(LIDX) = 0
              ELSE
                WRITE(LUN(LIDX),'(1X)',ERR=9) ! IF PREVIOUS WRITE WAS FINISHED, OR END OF HEADER -> ADVANCE
                STEP = (NVAL-N) - MOD(NVAL-N, PTLIM) ! WRITE NO PARTIAL NUMBER OF LINES
                IF (N+STEP == NVAL) THEN
                  STEP = STEP-PTLIM ! ONLY WRITE UP UNTIL THE SECOND-TO-LAST LINE, WITH ADVANCE=YES
                END IF

C               WRITE 1:END-1 COMPLETE LINES, WITH ADVANCE
                IF (STEP > 0) THEN
                  WRITE(LUN(LIDX),1007,ERR=9,ADVANCE='YES')
     &              (Y_CD(I+N), I=1,STEP)
                END IF

                N = N + STEP

C               WRITE LAST (PARTIAL/COMPLETE) LINE, WITH NO ADVANCE
                WRITE(LUN(LIDX),1007,ERR=9,ADVANCE='NO')
     &              (Y_CD(I), I=N+1,NVAL)

                REMPTS(LIDX) = PTLIM-(NVAL-N) ! POINTS LEFT TO WRITE

              END IF
              S = 1.0 ! SUCCESS




            ELSE
              S = -11
              GO TO 10


            END IF

          END IF

          GO TO 10

        ELSE
          S = -6
          GO TO 10

        END IF


C ------------------------------------------------------------------------------
C ------------------------------------------------------------------------------




C --- NORMAL WRITING MODE ------------------------------------------------------
C ------------------------------------------------------------------------------
      ELSE


C --- GET DATASET TYPE TO WRITE
      OK = mxIsStruct(prhs(2))
      IF (NOT(OK)) THEN
        S = -7.0
        GO TO 10
      END IF

      NINPUTF = mxGetNumberOfFields(prhs(2))

      IF (NINPUTF < 2) THEN
        S = -8
        GO TO 10
      END IF


      PTR = mxGetField(prhs(2), 1, 'datasetType')
      IF (PTR == 0) THEN
        S = -5.0
        GO TO 10
      END IF
      CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
      DATASETNUM = T



C --- DATASET 151 --------------------------------------------------------------
C ------------------------------------------------------------------------------
      IF (DATASETNUM == 151) THEN

C       MIN. REQUIRED NUMBER OF FIELDS
        FIELDSTOUSE = 6

        S = -8.0 ! DEFAULT ERROR STATE UNTIL ALL DATA EXTRACTED

        IF (NINPUTF < FIELDSTOUSE) GO TO 10


C       RECORD 1, ID LINE 1, model file name
        PTR = mxGetField(prhs(2), 1, 'modelName')
        IF (PTR == 0) THEN
          ID1 = 'NONE'
        ELSE
          OK = mxGetString(PTR, ID1, 80)
          IF (ADJUSTL(ID1)==' ') ID1 = 'NONE'
        END IF

C       RECORD 2, ID LINE 2, model file description
        PTR = mxGetField(prhs(2), 1, 'modelDescription')
        IF (PTR == 0) THEN
          ID2 = 'NONE'
        ELSE
          OK = mxGetString(PTR, ID2, 80)
          IF (ADJUSTL(ID2)==' ') ID2 = 'NONE'
        END IF

C       RECORD 3, ID LINE 3, program which created DB
        PTR = mxGetField(prhs(2), 1, 'databaseProgram')
        IF (PTR == 0) THEN
          ID3 = 'WHTA WRITEUNV'
        ELSE
          OK = mxGetString(PTR, ID3, 80)
          IF (ADJUSTL(ID3)==' ') ID3 = 'WHTA WRITEUNV'
        END IF

C       RECORD 4
C       4.1
        PTR = mxGetField(prhs(2), 1, 'databaseCreatedDate')
        IF (PTR == 0) GO TO 10
        OK = mxGetString(PTR, DS151F41, 10)
C       4.2
        PTR = mxGetField(prhs(2), 1, 'databaseCreatedTime')
        IF (PTR == 0) GO TO 10
        OK = mxGetString(PTR, DS151F42, 10)
C       4.3
        PTR = mxGetField(prhs(2), 1, 'databaseVersion1')
        IF (PTR == 0) THEN
          DS151F43 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          DS151F43 = T
        END IF
C       4.4
        PTR = mxGetField(prhs(2), 1, 'databaseVersion2')
        IF (PTR == 0) THEN
          DS151F44 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          DS151F44 = T
        END IF
C       4.5
        PTR = mxGetField(prhs(2), 1, 'fileType')
        IF (PTR == 0) THEN
          DS151F45 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          DS151F45 = T
          IF (DS151F45 < 0 .OR. DS151F45>2) GO TO 10
        END IF

C       RECORD 5
C       5.1
        PTR = mxGetField(prhs(2), 1, 'databaseLastSavedDate')
        IF (PTR == 0) THEN
          DS151F51 = DS151F41
        ELSE
          OK = mxGetString(PTR, DS151F51, 10)
        END IF
C       5.2
        PTR = mxGetField(prhs(2), 1, 'databaseLastSavedTime')
        IF (PTR == 0) THEN
          DS151F52 = DS151F42
        ELSE
          OK = mxGetString(PTR, DS151F52, 10)
        END IF

C       RECORD 6, ID LINE 6, program which created unv-file
        PTR = mxGetField(prhs(2), 1, 'unvProgram')
        IF (PTR == 0) THEN
          ID4 = 'WHTA WRITEUNV'
        ELSE
          OK = mxGetString(PTR, ID4, 80)
          IF (ADJUSTL(ID3)==' ') ID4 = 'WHTA WRITEUNV'
        END IF

C       RECORD 7
C       7.1
        PTR = mxGetField(prhs(2), 1, 'fileCreatedDate')
        IF (PTR == 0) THEN
          DS151F71 = DS151F41
        ELSE
          OK = mxGetString(PTR, DS151F71, 10)
        END IF
C       7.2
        PTR = mxGetField(prhs(2), 1, 'fileCreatedTime')
        IF (PTR == 0) THEN
          DS151F72 = DS151F42
        ELSE
          OK = mxGetString(PTR, DS151F72, 10)
        END IF


C ----- WRITE ALL FIELDS
C
C       START OF RECORD, -1
        WRITE(LUN(LIDX),'(I6)',ERR=9) -1
C       DATASET NUMBER
        WRITE(LUN(LIDX),'(I6)',ERR=9) DATASETNUM
C       RECORD 1
        WRITE(LUN(LIDX),'(A80)',ERR=9) ID1
C       RECORD 2
        WRITE(LUN(LIDX),'(A80)',ERR=9) ID2
C       RECORD 3
        WRITE(LUN(LIDX),'(A80)',ERR=9) ID3
C       RECORD 4
        WRITE(LUN(LIDX),1514,ERR=9) DS151F41,DS151F42,DS151F43,
     &    DS151F44,DS151F45
1514    FORMAT(A10,A10,3I10)
C       RECORD 5
        WRITE(LUN(LIDX),1515,ERR=9) DS151F51,DS151F52
1515    FORMAT(A10,A10)
C       RECORD 6
        WRITE(LUN(LIDX),'(A80)',ERR=9) ID4
C       RECORD 7
        WRITE(LUN(LIDX),1517,ERR=9) DS151F71, DS151F72
1517    FORMAT(A10,A10)
C       END OF RECORD, -1
        WRITE(LUN(LIDX),'(I6)',ERR=9) -1
C
C ----- WRITIING FINISHED

        S = 1.0 ! SUCCESS
        GO TO 10




C --- DATASET 164 --------------------------------------------------------------
C ------------------------------------------------------------------------------
      ELSEIF (DATASETNUM == 164) THEN

        S = -8.0 ! DEFAULT ERROR STATE UNTIL ALL DATA EXTRACTED

C       RECORD 1
C       1.1
        PTR = mxGetField(prhs(2), 1, 'unitsCode')
        IF (PTR == 0) THEN
          DS164F11 = 1
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          DS164F11 = T
          IF (DS164F11<0 .OR. DS164F11>9) GO TO 10
        END IF
C       1.2
        PTR = mxGetField(prhs(2), 1, 'unitsDescription')
        IF (PTR == 0) THEN
          IF (DS164F11 == 1) THEN
            DS164F12 = 'SI: Meter (newton)'
          ELSE IF (DS164F11 == 2) THEN
            DS164F12 = 'BG: Foot (pound f)'
          ELSE IF (DS164F11 == 3) THEN
            DS164F12 = 'MG: Meter (kilogram f)'
          ELSE IF (DS164F11 == 4) THEN
            DS164F12 = 'BA: Foot (poundal)'
          ELSE IF (DS164F11 == 5) THEN
            DS164F12 = 'MM: mm (milli newton)'
          ELSE IF (DS164F11 == 6) THEN
            DS164F12 = 'CM: cm (centi newton)'
          ELSE IF (DS164F11 == 7) THEN
            DS164F12 = 'IN: Inch (pound f)'
          ELSE IF (DS164F11 == 8) THEN
            DS164F12 = 'GM: mm (kilogram f)'
          ELSE IF (DS164F11 == 9) THEN
            DS164F12 = 'US: USER_DEFINED'
          ELSE
            GO TO 10
          END IF
        ELSE
          OK = mxGetString(PTR, DS164F12, 20)
        END IF
C       1.3
        PTR = mxGetField(prhs(2), 1, 'temperatureMode')
        IF (PTR == 0) THEN
          DS164F13 = 1
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          DS164F13 = T
        END IF

C       RECORD 2
C       2.1
        PTR = mxGetField(prhs(2), 1, 'length')
        IF (PTR == 0) THEN
          DS164F21 = 1
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), DS164F21, N1)
        END IF
C       2.2
        PTR = mxGetField(prhs(2), 1, 'force')
        IF (PTR == 0) THEN
          DS164F22 = 1
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), DS164F22, N1)
        END IF
C       2.3
        PTR = mxGetField(prhs(2), 1, 'temperature')
        IF (PTR == 0) THEN
          DS164F23 = 1
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), DS164F23, N1)
        END IF
C       2.4
        PTR = mxGetField(prhs(2), 1, 'temperatureOffset')
        IF (PTR == 0) THEN
          DS164F24 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), DS164F24, N1)
        END IF



C ----- WRITE ALL FIELDS
C
C       START OF RECORD, -1
        WRITE(LUN(LIDX),'(I6)',ERR=9) -1
C       DATASET NUMBER
        WRITE(LUN(LIDX),'(I6)',ERR=9) DATASETNUM
C       RECORD 1
        WRITE(LUN(LIDX),1641,ERR=9) DS164F11,DS164F12,DS164F13
1641    FORMAT(I10,A20,I10)
C       RECORD 2
        WRITE(LUN(LIDX),1642,ERR=9) DS164F21,DS164F22,DS164F23,
     &    DS164F24
1642    FORMAT(3ES25.17)
C       END OF RECORD, -1
        WRITE(LUN(LIDX),'(I6)',ERR=9) -1
C
C ----- WRITIING FINISHED



        S = 1.0 ! SUCCESS
        GO TO 10



C --- DATASET 58 --------------------------------------------------------------
C ------------------------------------------------------------------------------
      ELSEIF (DATASETNUM == 58) THEN


        FIELDSTOUSE = 13 ! some data is not strictly needed for writing the header

        S = -8.0 ! default error state until all data extracted

        IF (NINPUTF < FIELDSTOUSE) GO TO 10


C       RECORD 1, ID LINE 1
        PTR = mxGetField(prhs(2), 1, 'ID1')
        IF (PTR == 0) THEN
          ID1 = 'NONE'
        ELSE
          OK = mxGetString(PTR, ID1, 80)
          IF (ADJUSTL(ID1)==' ') ID1 = 'NONE'
        END IF

C       RECORD 2, ID LINE 2
        PTR = mxGetField(prhs(2), 1, 'ID2')
        IF (PTR == 0) THEN
          ID2 = 'NONE'
        ELSE
          OK = mxGetString(PTR, ID2, 80)
          IF (ADJUSTL(ID2)==' ') ID2 = 'NONE'
        END IF

C       RECORD 3, date
        PTR = mxGetField(prhs(2), 1, 'date')
        IF (PTR == 0) GO TO 10
        OK = mxGetString(PTR, ID3, 80)

C       RECORD 4, ID LINE 4
        PTR = mxGetField(prhs(2), 1, 'ID4')
        IF (PTR == 0) THEN
          ID4 = 'NONE'
        ELSE
          OK = mxGetString(PTR, ID4, 80)
          IF (ADJUSTL(ID4)==' ') ID4 = 'NONE'
        END IF

C       RECORD 5, ID LINE 5
        PTR = mxGetField(prhs(2), 1, 'ID5')
        IF (PTR == 0) THEN
          ID5 = 'NONE'
        ELSE
          OK = mxGetString(PTR, ID5, 80)
          IF (ADJUSTL(ID5)==' ') ID5 = 'NONE'
        END IF

C       RECORD 6
C       6.1
        PTR = mxGetField(prhs(2), 1, 'functionType')
        IF (PTR == 0) GO TO 10
        CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
        F61 = T
C       6.2
        PTR = mxGetField(prhs(2), 1, 'functionID')
        IF (PTR == 0) THEN
          F62 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F62 = T
        END IF
C       6.3
        PTR = mxGetField(prhs(2), 1, 'versionNumber')
        IF (PTR == 0) THEN
          F63 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F63 = T
        END IF
C       6.4
        PTR = mxGetField(prhs(2), 1, 'loadCaseID')
        IF (PTR == 0) THEN
          F64 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F64 = T
        END IF
C       6.5
        PTR = mxGetField(prhs(2), 1, 'rspEntName')
        IF (PTR == 0) THEN
          F65 = 'NONE'
        ELSE
          OK = mxGetString(PTR, F65, 10)
          IF (ADJUSTL(F65)==' ') F65 = 'NONE'
        END IF
C       6.6
        PTR = mxGetField(prhs(2), 1, 'rspNode')
        IF (PTR == 0) GO TO 10
        CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
        F66 = T
C       6.7
        PTR = mxGetField(prhs(2), 1, 'rspDir')
        IF (PTR == 0) THEN
          F67 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F67 = T
          IF (F67 < -6 .OR. F67 > 6) GO TO 10
        END IF
C       6.8
        PTR = mxGetField(prhs(2), 1, 'refEntName')
        IF (PTR == 0) THEN
          F68 = 'NONE'
        ELSE
          OK = mxGetString(PTR, F68, 10)
          IF (ADJUSTL(F68)==' ') F68 = 'NONE'
        END IF
C       6.9
        PTR = mxGetField(prhs(2), 1, 'refNode')
        IF (PTR == 0) THEN
          F69 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F69 = T
        END IF
C       6.10
        PTR = mxGetField(prhs(2), 1, 'refDir')
        IF (PTR == 0) THEN
          F610 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F610 = T
          IF (F610 < -6 .OR. F610 > 6) GO TO 10
        END IF

C       RECORD 7
C       7.1
        PTR = mxGetField(prhs(2), 1, 'dataFormatType')
        IF (PTR == 0) GO TO 10
        CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
        F71 = T
C       7.2
        PTR = mxGetField(prhs(2), 1, 'numValues')
        IF (PTR == 0) THEN
          F72 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F72 = T
        END IF
C       7.3
        PTR = mxGetField(prhs(2), 1, 'evenSpacing')
        IF (PTR == 0) GO TO 10
        CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
        F73 = T
C       7.4
        PTR = mxGetField(prhs(2), 1, 'x0')
        IF (PTR == 0) GO TO 10
        CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
        F74 = T
C       7.5
        PTR = mxGetField(prhs(2), 1, 'dx')
        IF (PTR == 0) GO TO 10
        CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
        F75 = T
C       7.6
        PTR = mxGetField(prhs(2), 1, 'zAxisValue')
        IF (PTR == 0) THEN
          F76 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F76 = T
        END IF

C       RECORD 8
C       8.1
        PTR = mxGetField(prhs(2), 1, 'abscDataType')
        IF (PTR == 0) GO TO 10
        CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
        F81 = T
C       8.2
        PTR = mxGetField(prhs(2), 1, 'abscLengthUnitsExponent')
        IF (PTR == 0) THEN
          F82 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F82 = T
        END IF
C       8.3
        PTR = mxGetField(prhs(2), 1, 'abscForceUnitsExponent')
        IF (PTR == 0) THEN
          F83 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F83 = T
        END IF
C       8.4
        PTR = mxGetField(prhs(2), 1, 'abscTempUnitsExponent')
        IF (PTR == 0) THEN
          F84 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F84 = T
        END IF
C       8.5
        PTR = mxGetField(prhs(2), 1, 'abscAxisLabel')
        IF (PTR == 0) GO TO 10
        OK = mxGetString(PTR, F85, 20)
C       8.6
        PTR = mxGetField(prhs(2), 1, 'abscAxisUnitLabel')
        IF (PTR == 0) GO TO 10
        OK = mxGetString(PTR, F86, 20)

C       RECORD 9
C       9.1
        PTR = mxGetField(prhs(2), 1, 'ordNumDataType')
        IF (PTR == 0) GO TO 10
        CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
        F91 = T
C       9.2
        PTR = mxGetField(prhs(2), 1, 'ordNumLengthUnitsExponent')
        IF (PTR == 0) THEN
          F92 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F92 = T
        END IF
C       9.3
        PTR = mxGetField(prhs(2), 1, 'ordNumForceUnitsExponent')
        IF (PTR == 0) THEN
          F93 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F93 = T
        END IF
C       9.4
        PTR = mxGetField(prhs(2), 1, 'ordNumTempUnitsExponent')
        IF (PTR == 0) THEN
          F94 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F94 = T
        END IF
C       9.5
        PTR = mxGetField(prhs(2), 1, 'ordNumAxisLabel')
        IF (PTR == 0) GO TO 10
        OK = mxGetString(PTR, F95, 20)
C       9.6
        PTR = mxGetField(prhs(2), 1, 'ordNumAxisUnitLabel')
        IF (PTR == 0) GO TO 10
        OK = mxGetString(PTR, F96, 20)

C       RECORD 10
C       10.1
        PTR = mxGetField(prhs(2), 1, 'ordDenomDataType')
        IF (PTR == 0) THEN
          F101 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F101 = T
        END IF
C       10.2
        PTR = mxGetField(prhs(2), 1, 'ordDenomLengthUnitsExponent')
        IF (PTR == 0) THEN
          F102 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F102 = T
        END IF
C       10.3
        PTR = mxGetField(prhs(2), 1, 'ordDenomForceUnitsExponent')
        IF (PTR == 0) THEN
          F103 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F103 = T
        END IF
C       10.4
        PTR = mxGetField(prhs(2), 1, 'ordDenomTempUnitsExponent')
        IF (PTR == 0) THEN
          F104 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F104 = T
        END IF
C       10.5
        PTR = mxGetField(prhs(2), 1, 'ordDenomAxisLabel')
        IF (PTR == 0) THEN
          F105 = 'NONE'
        ELSE
          OK = mxGetString(PTR, F105, 20)
          IF (ADJUSTL(F105)==' ') F105 = 'NONE'
        END IF
C       10.6
        PTR = mxGetField(prhs(2), 1, 'ordDenomAxisUnitLabel')
        IF (PTR == 0) THEN
          F106 = 'NONE'
        ELSE
          OK = mxGetString(PTR, F106, 20)
          IF (ADJUSTL(F106)==' ') F106 = 'NONE'
        END IF

C       RECORD 11
C       11.1
        PTR = mxGetField(prhs(2), 1, 'zDataType')
        IF (PTR == 0) THEN
          F111 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F111 = T
        END IF
C       11.2
        PTR = mxGetField(prhs(2), 1, 'zLengthUnitsExponent')
        IF (PTR == 0) THEN
          F112 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F112 = T
        END IF
C       11.3
        PTR = mxGetField(prhs(2), 1, 'zForceUnitsExponent')
        IF (PTR == 0) THEN
          F113 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F113 = T
        END IF
C       11.4
        PTR = mxGetField(prhs(2), 1, 'zTempUnitsExponent')
        IF (PTR == 0) THEN
          F114 = 0
        ELSE
          CALL mxCopyPtrToReal8(mxGetDoubles(PTR), T, N1)
          F114 = T
        END IF
C       11.5
        PTR = mxGetField(prhs(2), 1, 'zAxisLabel')
        IF (PTR == 0) THEN
          F115 = 'NONE'
        ELSE
          OK = mxGetString(PTR, F115, 20)
          IF (ADJUSTL(F115)==' ') F115 = 'NONE'
        END IF
C       11.6
        PTR = mxGetField(prhs(2), 1, 'zAxisUnitLabel')
        IF (PTR == 0) THEN
          F116 = 'NONE'
        ELSE
          OK = mxGetString(PTR, F116, 20)
          IF (ADJUSTL(F116)==' ') F116 = 'NONE'
        END IF

        ISEVEN = F73

C       CHECK IF DATA SHOULD BE WRITTEN
        PTR = mxGetField(prhs(2), 1, 'data')
        IF (PTR == 0) THEN
          WRITEDATA = .FALSE.
        ELSE
          WRITEDATA = .TRUE.
C         GET DATATYPE OF DATA: SINGLE/DOUBLE, REAL/COMPLEX ?
          ISDOUBLE(1) = mxIsDouble(PTR)
          ISCOMPLEX = mxIsComplex(PTR)
          NVAL = mxGetM(PTR)
          IF (NVAL < 1) GO TO 10

C         GET CASENUM FROM DATATYPE AND X-EVENNESS
          CASENUM = 1
          IF (ISEVEN == .FALSE.) THEN
            CASENUM = CASENUM + 1 ! = UNEVEN
            XPTR = mxGetField(prhs(2), 1, 'x')
            IF (XPTR == 0) GO TO 10 ! X FIELD NOT PRESENT
            NVALX = mxGetM(XPTR)
            IF (NVALX /= NVAL) GO TO 10 ! MISMATCHING X-DATA
            ALLOCATE(XDATA(NVAL))
            ISDOUBLE(2) = mxIsDouble(XPTR)
            IF (ISDOUBLE(2) > 0) THEN
              CALL mxCopyPtrToReal8(mxGetDoubles(XPTR), XDATA, NVAL)
            ELSE
              CALL mxCopyPtrToReal4(mxGetSingles(XPTR), XDATA, NVAL)
            END IF
          END IF
          IF (ISCOMPLEX) CASENUM = CASENUM + 2 ! = COMPLEX
          IF (ISDOUBLE(1)) CASENUM = CASENUM + 4 ! = DOUBLE

          IF (CASENUM > 6) THEN ! COMPLEX DOUBLE
            ALLOCATE(Y_CD(NVAL))
            CALL mxCopyPtrToComplex16(mxGetComplexDoubles(PTR), Y_CD,
     &        NVAL)
            F71 = 6

          ELSE IF (CASENUM > 4) THEN ! REAL DOUBLE
            ALLOCATE(Y_RD(NVAL))
            CALL mxCopyPtrToReal8(mxGetDoubles(PTR), Y_RD, NVAL)
            F71 = 4

          ELSE IF (CASENUM > 2) THEN ! COMPLEX SINGLE
            ALLOCATE(Y_CS(NVAL))
            CALL mxCopyPtrToComplex8(mxGetComplexSingles(PTR), Y_CS,
     &        NVAL)
            F71 = 5

          ELSE ! REAL SINGLE
            ALLOCATE(Y_RS(NVAL))
            CALL mxCopyPtrToReal4(mxGetSingles(PTR), Y_RS, NVAL)
            F71 = 2

          END IF

          F72 = NVAL

        END IF



C ----- WRITE ALL FIELDS
C
C       START OF RECORD, -1
        WRITE(LUN(LIDX),'(I6)',ERR=9) -1

C       DATASET NUMBER
        WRITE(LUN(LIDX),'(I6)',ERR=9) DATASETNUM

C       RECORD 1
        WRITE(LUN(LIDX),'(A80)',ERR=9) ID1

C       RECORD 2
        WRITE(LUN(LIDX),'(A80)',ERR=9) ID2

C       RECORD 3
        WRITE(LUN(LIDX),'(A80)',ERR=9) ID3

C       RECORD 4
        WRITE(LUN(LIDX),'(A80)',ERR=9) ID4

C       RECORD 5
        WRITE(LUN(LIDX),'(A80)',ERR=9) ID5

C       RECORD 6
        WRITE(LUN(LIDX),5806,ERR=9) F61,F62,F63,F64,F65,F66,F67,F68,
     &    F69,F610
5806    FORMAT(2(I5,I10),2(1X,A10,I10,I4))

C       RECORD 7
        INQUIRE(LUN(LIDX), POS=REC7POS(LIDX))
        WRITE(LUN(LIDX),5807,ERR=9) F71,F72,F73,F74,F75,F76
5807    FORMAT(3I10,3ES13.5)

C       RECORD 8
        WRITE(LUN(LIDX),5808,ERR=9) F81,F82,F83,F84,F85,F86
5808    FORMAT(I10,3I5,2(1X,A20))

C       RECORD 9
        WRITE(LUN(LIDX),5809,ERR=9) F91,F92,F93,F94,F95,F96
5809    FORMAT(I10,3I5,2(1X,A20))

C       RECORD 10
        WRITE(LUN(LIDX),5810,ERR=9) F101,F102,F103,F104,F105,F106
5810    FORMAT(I10,3I5,2(1X,A20))

C       RECORD 11
        IF (WRITEDATA == .TRUE.) THEN ! IF DATA IS TO BE WRITTEN IN THIS CALL
          WRITE(LUN(LIDX),5811,ERR=9,ADVANCE='YES') F111,F112,F113,F114,
     &      F115,F116
        ELSE ! DATA WILL COME IN NEXT FUNCTION CALL
          WRITE(LUN(LIDX),5811,ERR=9,ADVANCE='NO') F111,F112,F113,F114,
     &      F115,F116
        END IF
5811    FORMAT(I10,3I5,2(1X,A20))

C       RECORD 12, DATA, IF TO BE WRITTEN
        IF (WRITEDATA == .TRUE.) THEN

          N = 0 ! INITIALISE NUMBER OF VALUES WRITTEN

          IF (CASENUM == 1) THEN
1001        FORMAT(6ES13.5)
            WRITE(LUN(LIDX),1001,ERR=9)(Y_RS(I),I=1,NVAL)
            N = NVAL

          ELSE IF (CASENUM == 2) THEN

1002        FORMAT(6ES13.5)
            DO WHILE (N < NVAL)
              STEP = MIN(3, NVAL-N)
              WRITE(LUN(LIDX),1002,ERR=9)(XDATA(I+N),Y_RS(I+N),I=1,STEP)
              N = N + STEP
            END DO

          ELSE IF (CASENUM == 3) THEN
1003        FORMAT(6ES13.5)
            WRITE(LUN(LIDX),1003,ERR=9)(Y_CS(I),I=1,NVAL)
            N = NVAL

          ELSE IF (CASENUM == 4) THEN
1004        FORMAT(6ES13.5)
            DO WHILE (N < NVAL)
              STEP = MIN(2, NVAL-N)
              WRITE(LUN(LIDX),1004,ERR=9)(XDATA(I+N),Y_CS(I+N),I=1,STEP)
              N = N + STEP
            END DO

          ELSE IF (CASENUM == 5) THEN
1005        FORMAT(4ES20.12)
            WRITE(LUN(LIDX),1005,ERR=9)(Y_RD(I),I=1,NVAL)
            N = NVAL

          ELSE IF (CASENUM == 6) THEN
1006        FORMAT(2(ES13.5,ES20.12))
            DO WHILE (N < NVAL)
              STEP = MIN(2, NVAL-N)
              WRITE(LUN(LIDX),1006,ERR=9)(XDATA(I+N),Y_RD(I+N),I=1,STEP)
              N = N + STEP
            END DO

          ELSE IF (CASENUM == 7) THEN
1007        FORMAT(4ES20.12)
            WRITE(LUN(LIDX),1007,ERR=9)(Y_CD(I),I=1,NVAL)
            N = NVAL

          ELSE IF (CASENUM == 8) THEN
1008        FORMAT(ES13.5,2ES20.12)
            DO WHILE (N < NVAL)
              WRITE(LUN(LIDX),1008,ERR=9)(XDATA(I+N),Y_CD(N+I),I=1,1)
              N = N + 1
            END DO

          ELSE
            GO TO 10 ! ERROR: BAD CASENUM, SHOULD ALREADY BE CHECKED ABOVE
          END IF

C         END OF RECORD, -1, IF DATA IS WRITTEN, OTHERWISE IT IS KEPT UNTERMINATED
          WRITE(LUN(LIDX),'(I6)',ERR=9) -1


        ELSE ! KEEP UNTERMINATED


C         INIT. CASENUM
          CASENUM = 1

C         DETERMINE CASENUM
          ISDOUBLE(1) = (F71==4 .OR. F71==6) ! IS DATA OF TYPE DOUBLE, OR NOT -> SINGLE
          ISCOMPLEX = (F71 > 4) ! IS DATA COMPLEX- (TRUE) OR REAL-VALUED (FALSE)
          IF (NOT(ISEVEN)) CASENUM = CASENUM + 1 ! = EVEN
          IF (ISCOMPLEX) CASENUM = CASENUM + 2 ! = COMPLEX
          IF (ISDOUBLE(1)) CASENUM = CASENUM + 4 ! = DOUBLE


        END IF

C
C ----- WRITIING FINISHED

        S = 1.0 ! SUCCESS
        GO TO 10


      ELSE ! UNKNOWN TYPE

        S = -6.0
        S = DATASETNUM
        GO TO 10

      END IF

      END IF



C --- FINISHED WRITING ---------------------------------------------------------
C ------------------------------------------------------------------------------



C --- RETURN ARGUMENTS AND CLOSE FILES AND VARIABLES

      S = -90 ! SHOULD NEVER HAPPEN -> ERROR IN CODE
      GO TO 10

8     IF (OPENSTAT == 1) THEN ! REMAP IOSTAT ERROR CODE 1 TO -12
        S = -12
      ELSE
        S = OPENSTAT
      END IF
      GO TO 10

C     UNKNOWN ERROR EXIT
9     S = -1.0
      KEEPFILEOPEN = .FALSE.

C     NORMAL/ASSIGNED EXIT
10    plhs(1) = mxCreateDoubleScalar(S)
      IF (nlhs > 1) THEN
        plhs(2) = mxCreateDoubleScalar(DBLE(CASENUM))
      END IF


      IF (ALLOCATED(FILEPATH))  DEALLOCATE(FILEPATH)
      IF (ALLOCATED(XDATA))     DEALLOCATE(XDATA)
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



      RETURN
      END


C ------------------------------------------------------------------------------
C --- END OF PROGRAM -----------------------------------------------------------
C ------------------------------------------------------------------------------


