#include "fintrf.h"


C ----------------------------------------------------------------------
C
C     [STATE, DATACELL] = READUNV(FILEPATH, SKIP_DATA)
C
C     NB: ALL ARGUMENTS ARE REQUIRED (TWO OUTPUTS AND ONE INPUT)
C
C
C     FILEPATH:   [CHAR] RELATIVE OR ABSOLUTE PATH TO FILE TO READ
C
C     SKIP_DATA:  [DOUBLE] ONLY READ HEADER, OPTIONAL (DEFAULT IS 0)
C
C     DATACELL:   [CELL]/[CHAR] CELL ARRAY FOR EACH DATASET IN FILE
C                 IF NOT SUCCESSFUL -> CHAR WITH ERROR DESCRIPTION
C
C     STATE FLAGS:
C       =  1,  SUCCESSFUL
C       =  0,  UNKNOWN ERROR
C       = -1,  EMPTY INPUT
C       = -2,  COULD NOT READ FILEPATH
C       = -3,  FILE DOES NOT EXIST
C       = -4,  TOO FEW INPUTS
C       = -5,  FILEPATH IS NOT A CHAR
C       = -6,  FILE IS ILL-FORMED
C       = -7,  TWO OUTPUT ARGUMENTS ARE REQUIRED
C       = -8,  DEPRECATED, AND NOT THROWN ANY MORE
C       = -9,  ERROR WHILE READING FILE
C       = -10, ERROR IN BINARY/ASCII READ TRANSITION
C       = -12, FLOATING POINT ARITHMETIC OVERFLOW (FROM IOSTAT)
C       = >1,  ERROR CODE FROM IOSTAT WHEN OPENING FILE
C
C ----------------------------------------------------------------------


      SUBROUTINE mexFunction(nlhs, plhs, nrhs, prhs)

      USE ISO_FORTRAN_ENV,ONLY : IOSTAT_EOR, IOSTAT_END

      IMPLICIT NONE

C     GATEWAY VARIABLES
      mwPointer :: plhs(*), prhs(*)
      mwSize    :: nlhs, nrhs

C     FUNCTIONS
      mwPointer :: mxCreateString
      mwPointer :: mxCreateDoubleScalar
      mwPointer :: mxGetDoubles
      mwPointer :: mxGetSingles
      mwPointer :: mxGetInt8s
      mwPointer :: mxGetComplexDoubles
      mwPointer :: mxGetComplexSingles
      mwPointer :: mxCreateCellMatrix
      mwPointer :: mxCreateNumericMatrix
      mwPointer :: mxCreateStructMatrix
      mwPointer :: mxGetN
      mwPointer :: mxGetString
      mwPointer :: mxIsChar
      mwPointer :: mxIsDouble
      mwPointer :: mxIsEmpty
      REAL*8    :: mxGetScalar
      INTEGER*4 :: mxClassIDFromClassName
      INTEGER*4 :: FSEEK

C     MEX. DERIVED TYPES
      mwSize    :: STRLEN, NVAL, KMW, NCELL, M, N
      mwPointer :: CELL_PTR, YPTR
      INTEGER*4 :: CXFLAG, CID_S, CID_D, CID
      mwPointer, DIMENSION(:), ALLOCATABLE :: SPTR_1, SPTR_2 ! USE WHEN NUM. OF DATA RECORDS > MAXRECORDS
      mwSize, PARAMETER :: N1 = 1

C     DATABLOCK RELATED VARIABLES
      INTEGER*4, PARAMETER :: MAXRECORDS = 4096
      INTEGER*4, PARAMETER :: RECORD_STEP = 16384
      INTEGER*4, PARAMETER :: NFIELDS = 49 ! MAXIMUM NUMBER OF FIELDS IN USE, ADJUST WHEN NEEDED
      mwPointer    :: SPTR(MAXRECORDS)
      mwPointer    :: ID_PTR(NFIELDS)
      CHARACTER*32 :: fieldnames(NFIELDS) ! LENGTH OF LONGEST FIELDNAME (32), ADJUST WHEN NEEDED

C     UNV DATASET RELATED VARIABLES
C     ------------------------------------------------------------------
      INTEGER*4, PARAMETER :: DATASETS(5) = (/151,164,58,1806,2400/) ! SUPPORTED DATASET TYPES
      INTEGER*4          :: DATASETNUM, PREV_DATASETNUM
      CHARACTER(LEN=80)  :: REC0, REC_TEMP
      LOGICAL*1          :: USE_BINARY
C     DATASET 58/58b:
      CHARACTER(LEN=1)   :: F02
      CHARACTER(LEN=80)  :: ID1, ID2, ID3, ID5
      CHARACTER(LEN=120) :: ID4 ! EXTEND TO 120, DUE TO SIEMENS TESTLAB BEHAVIOUR
      CHARACTER(LEN=10)  :: F65, F68
      CHARACTER(LEN=20)  :: F85, F86
      CHARACTER(LEN=20)  :: F95, F96
      CHARACTER(LEN=20)  :: F105, F106
      CHARACTER(LEN=20)  :: F115, F116
      CHARACTER(LEN=6)   :: DATATYPE
      INTEGER*8          :: F06
      INTEGER*4          :: F05, CASENUM
      INTEGER*4          :: F61, F62, F63, F64, F66, F67, F69, F610
      INTEGER*4          :: F71, F72, F73
      INTEGER*4          :: F81, F82, F83, F84
      INTEGER*4          :: F91, F92, F93, F94
      INTEGER*4          :: F101, F102, F103, F104
      INTEGER*4          :: F111, F112, F113, F114
      INTEGER*1          :: F03, F04
      REAL*4             :: F74, F75, F76
      REAL*8             :: ISEVEN, ABSC_N, ABSC_D, ABSC_0, ORDDATATYPE
C     DATASET 151:
      CHARACTER(LEN=10)  :: DS151F41, DS151F42
      CHARACTER(LEN=10)  :: DS151F51, DS151F52
      CHARACTER(LEN=10)  :: DS151F71, DS151F72
      INTEGER*4          :: DS151F43, DS151F44, DS151F45
C     DATASET 164:
      CHARACTER(LEN=22)  :: UNITSSTR
      CHARACTER(LEN=20)  :: DS164F12
      INTEGER*4          :: DS164F11, DS164F13
      REAL*8             :: DS164F21, DS164F22, DS164F23, DS164F24
C     DATASET 1806:
      CHARACTER(LEN=10)  :: F1806_1_1, F1806_2_1, F1806_2_2, F1806_3_1
      CHARACTER(LEN=10)  :: F1806_3_2, F1806_3_3, F1806_5_7
      CHARACTER(LEN=40)  :: F1806_4_1
      INTEGER*4          :: F1806_5_1, F1806_5_2, F1806_5_3
      INTEGER*2          :: F1806_5_4, F1806_5_5, F1806_5_6
      REAL*8             :: F1806_6_1
C     DATASET 2400:
      CHARACTER(LEN=40)  :: F2400_2_1, F2400_3_1
      INTEGER*4          :: F2400_1_1, F2400_1_4, F2400_5_1, F2400_5_2
      INTEGER*4          :: F2400_5_3, F2400_5_4, F2400_5_5
      INTEGER*2          :: F2400_1_2, F2400_1_3, F2400_4_1(32)
      INTEGER*1          :: F2400_6_1
C     ------------------------------------------------------------------

      REAL*8    :: S ! STATE FLAG
      INTEGER*8 :: I, K, NUMFIELDS, MAXRECORDS_LOCAL, SKIP_DATA, RECPOS
      INTEGER*4 :: VAL_INT, OK, NOK, LUN, LUN_BIN
      INTEGER*1 :: USE_ALLOCATION ! DYNAMIC ALLOCATION FLAG
      LOGICAL   :: VAL_BOOL ! LOGICAL STATUS VALUE, E.G. FOR FUNCTION STATE RETURNS

C     ALLOCATABLE VARIABLES
      CHARACTER(LEN=:), ALLOCATABLE         :: FILEPATH
      REAL*4,     DIMENSION(:), ALLOCATABLE :: Y_RS, XDATA
      REAL*8,     DIMENSION(:), ALLOCATABLE :: Y_RD
      COMPLEX*8,  DIMENSION(:), ALLOCATABLE :: Y_CS
      COMPLEX*16, DIMENSION(:), ALLOCATABLE :: Y_CD


C     ------------------------------------------------------------------
C     END OF VARIABLE DEFINITIONS --------------------------------------
C     ------------------------------------------------------------------


C     INITIALISE (NON-PARAMETRIC) VARIABLES
      LUN = 0                       ! FILE UNIT NUMBER, IF STILL ZERO AT END OF PROGRAM => LUN NOT PROPERLY ASSIGNED
      LUN_BIN = 0                   ! FILE UNIT NUMBER, USED FOR BINARY READ (58b)
      NCELL = 0                     ! CELL COUNTER
      S = 0                         ! OUTPUT STATE
      USE_BINARY = .FALSE.          ! DATA IS WRITTEN AS BINARY
      SKIP_DATA = 0                 ! SKIP READING DATA, DEFAULT IS FALSE (ZERO)
      MAXRECORDS_LOCAL = MAXRECORDS ! MAXIMUM NUMBER OF RECORDS CURRENTLY
      USE_ALLOCATION = 0            ! USE DYNAMIC ALLOCATION OF OUTPUT CELL ARRAY, DEFAULT IS OFF; IS TRIGGERED WHEN NUMBER OF RECORDS EXCEEDS "MAXRECORDS"
      PREV_DATASETNUM = 0           ! PREVIOUS DATASETNUM IN LOOP, USED TO MINIMISE UNNECESSARY RE-DEFINITIONS
      CID_S = mxClassIDFromClassName('single') ! MX FLAG FOR SINGLE PRECISION
      CID_D = mxClassIDFromClassName('double') ! MX FLAG FOR DOUBLE PRECISION


80    FORMAT(A80)


C     AT LEAST ONE INPUT ARGUMENT (FILEPATH) IS REQUIRED
      IF (nrhs < 1) THEN
        S = -4
        GO TO 10 ! EXIT
      ELSE IF (nrhs > 1) THEN ! CHECK WHETHER ONLY HEADERS SHOULD BE READ OR NOT, DEFAULT IS FULL READ.
        IF (NOT(mxIsEmpty(prhs(2))) .AND. mxIsDouble(prhs(2))) THEN ! INPUT MUST BE OF KIND DOUBLE
          SKIP_DATA = DBLE(mxGetScalar(prhs(2))) ! GET SCALAR DOUBLE VALUE (FIRST VALUE IS READ IF MATRIX)
        ELSE
          SKIP_DATA = 0 ! READ ALL DATA
        END IF
      END IF


C     CHECK IF TWO OUTPUT ARGUMENTS ARE USED, IF NOT -> ERROR
      IF (nlhs /= 2) THEN
        S = -7
        GO TO 10 ! EXIT
      END IF


C     CHECK IF INPUT ARGUMENT IS OF CHAR TYPE
      OK = mxIsChar(prhs(1))
      IF (NOT(OK)) THEN ! CHAR TYPE IS REQUIRED
        S = -5
        GO TO 10 ! EXIT
      END IF


C     GET LENGTH OF INPUT STRING
      STRLEN = mxGetN(prhs(1))
      IF (STRLEN < 1) THEN ! EMPTY FILEPATH -> ERROR
        S = -1
        GO TO 10 ! EXIT
      END IF


C     ALLOCATE FILEPATH FOR REQUIRED LENGTH
      ALLOCATE(CHARACTER(LEN=STRLEN) :: FILEPATH)


C     COPY INPUT STRING TO FILEPATH
      NOK = mxGetString(prhs(1), FILEPATH, STRLEN)
      IF (NOK /= 0) THEN ! COPYING FAILED
        S = -2
        GO TO 10
      END IF


C     CHECK IF FILEPATH EXIST
      INQUIRE(FILE=FILEPATH, EXIST=VAL_BOOL)
      IF (NOT(VAL_BOOL)) THEN
        S = -3
        GO TO 10
      END IF


C     OPEN FILE LOCATED AT <FILEPATH>
      OPEN (
     & NEWUNIT = LUN,
     & FILE = FILEPATH,
     & STATUS = 'OLD',
     & IOSTAT = VAL_INT,
     & POSITION = 'REWIND',
     & ACTION = 'READ',
     & ACCESS = 'STREAM',
     & ENCODING = 'DEFAULT',
     & FORM = 'FORMATTED')


C     CHECK IF FILE OPENING FAILED -> RETURN IOSTAT CODE
      IF (VAL_INT /= 0) THEN
        IF (VAL_INT == 1) THEN ! REMAP IOSTAT ERROR CODE 1 TO -12
          S = -12
        ELSE
          S = VAL_INT
        END IF
        GO TO 10
      END IF



C     READ FIRST LINE IN FILE
      READ(LUN, 80, END=10, ERR=9) REC0



C ----------------------------------------------------------------------
C     READ DATA WHILE DATARECORD IS PREPENDED WITH "-1", USING STRICT FORMATTING
      DO WHILE (REC0 == "    -1")


C     READ DATASET NUMBER, 58B COMPATIBILITY PREPARED (BUT NOT SUPPORTED YET)
582   FORMAT(I6, 1A1, I6, I6, I12, I12, I6, I6, I12, I12)
      READ(LUN, 582, END=10, ERR=9) DATASETNUM, F02, F03, F04, F05, F06

      USE_BINARY = (F02 == 'b') ! NB: LOWER CASE MANDATED BY STANDARD


C --- HANDLE DATABLOCK ACCORDING TO DATASET NUMBER ---------------------
C ----------------------------------------------------------------------
      SELECT CASE (DATASETNUM)

C --- DATASET 151 ------------------------------------------------------
C ----------------------------------------------------------------------
      CASE (151)

C       RECORD 1, ID LINE 1, model file name
        READ(LUN, 80, END=10, ERR=9) ID1

C       RECORD 2, ID LINE 2, model file description
        READ(LUN, 80, END=10, ERR=9) ID2

C       RECORD 3, ID LINE 3, program which created DB
        READ(LUN, 80, END=10, ERR=9) ID3

C       RECORD 4
1514    FORMAT(A10, A10, 3I10)
        READ(LUN, 1514, END=10, ERR=9)
     &    DS151F41, DS151F42, DS151F43, DS151F44, DS151F45

C       RECORD 5
1515    FORMAT(A10, A10)
        READ(LUN, 1515, END=10, ERR=9)
     &    DS151F51, DS151F52

C       RECORD 6, ID LINE 6, program which created unv-file
        READ(LUN, 80, END=10, ERR=9) ID4

C       RECORD 7
1517    FORMAT(A10, A10)
        READ(LUN, 1517, END=10, ERR=9)
     &    DS151F71, DS151F72


C       FIX TEXT ALIGNMENT
        ID1 = ADJUSTL(ID1)
        ID2 = ADJUSTL(ID2)
        ID3 = ADJUSTL(ID3)
        ID4 = ADJUSTL(ID4)
        CALL REMOVE_NON_ASCII(ID1, LEN_TRIM(ID1))
        CALL REMOVE_NON_ASCII(ID2, LEN_TRIM(ID2))
        CALL REMOVE_NON_ASCII(ID3, LEN_TRIM(ID3))
        CALL REMOVE_NON_ASCII(ID4, LEN_TRIM(ID4))

        DS151F41 = ADJUSTL(DS151F41)
        DS151F42 = ADJUSTL(DS151F42)
        DS151F51 = ADJUSTL(DS151F51)
        DS151F52 = ADJUSTL(DS151F52)
        DS151F71 = ADJUSTL(DS151F71)
        DS151F72 = ADJUSTL(DS151F72)


        fieldnames(1)  = 'datasetType'
        fieldnames(2)  = 'modelName'
        fieldnames(3)  = 'modelDescription'
        fieldnames(4)  = 'databaseProgram'
        fieldnames(5)  = 'databaseCreatedDate'
        fieldnames(6)  = 'databaseCreatedTime'
        fieldnames(7)  = 'databaseVersion1'
        fieldnames(8)  = 'databaseVersion2'
        fieldnames(9)  = 'fileType'
        fieldnames(10) = 'databaseLastSavedDate'
        fieldnames(11) = 'databaseLastSavedTime'
        fieldnames(12) = 'unvProgram'
        fieldnames(13) = 'fileCreatedDate'
        fieldnames(14) = 'fileCreatedTime'

        NUMFIELDS = 14 ! NUMBER OF FIELDS IN DATASET 151


        ID_PTR(1) = mxCreateDoubleScalar(DBLE(DATASETNUM)) ! dataset number
        ID_PTR(2) = mxCreateString(ID1) ! ID Line 1, model file name
        ID_PTR(3) = mxCreateString(ID2) ! ID Line 2, model file description
        ID_PTR(4) = mxCreateString(ID3) ! ID Line 3, program which created DB

        ID_PTR(5) = mxCreateString(DS151F41) ! date database created (DD-MMM-YY)
        ID_PTR(6) = mxCreateString(DS151F42) ! time database created (HH:MM:SS)
        ID_PTR(7) = mxCreateDoubleScalar(DBLE(DS151F43)) ! Version from database
        ID_PTR(8) = mxCreateDoubleScalar(DBLE(DS151F44)) ! Version from database
        ID_PTR(9) = mxCreateDoubleScalar(DBLE(DS151F45)) ! File type

        ID_PTR(10) = mxCreateString(DS151F51) ! date database last saved (DD-MMM-YY)
        ID_PTR(11) = mxCreateString(DS151F52) ! time database last saved (HH:MM:SS)

        ID_PTR(12) = mxCreateString(ID4) ! ID Line 6, program which created universal file

        ID_PTR(13) = mxCreateString(DS151F71) ! date file written (DD-MMM-YY)
        ID_PTR(14) = mxCreateString(DS151F72) ! time file written (HH:MM:SS)





C --- DATASET 164 ------------------------------------------------------
C ----------------------------------------------------------------------
      CASE (164)

C       RECORD 1
1641    FORMAT(I10, A20, I10)
        READ(LUN, 1641, END=10, ERR=9)
     &    DS164F11, DS164F12, DS164F13

C       RECORD 2
1642    FORMAT(3D25.17)
        READ(LUN, 1642, END=10, ERR=9)
     &    DS164F21, DS164F22, DS164F23, DS164F24


        SELECT CASE (DS164F11) ! units code
          CASE (1)
            UNITSSTR = 'SI: Meter (newton)'
          CASE (2)
            UNITSSTR = 'BG: Foot (pound f)'
          CASE (3)
            UNITSSTR = 'MG: Meter (kilogram f)'
          CASE (4)
            UNITSSTR = 'BA: Foot (poundal)'
          CASE (5)
            UNITSSTR = 'MM: mm (milli newton)'
          CASE (6)
            UNITSSTR = 'CM: cm (centi newton)'
          CASE (7)
            UNITSSTR = 'IN: Inch (pound f)'
          CASE (8)
            UNITSSTR = 'GM: mm (kilogram f)'
          CASE (9)
            UNITSSTR = 'US: USER_DEFINED'
          CASE DEFAULT
            UNITSSTR = 'Unknown units'
        END SELECT


        fieldnames(1) = 'datasetType'
        fieldnames(2) = 'unitsCode'
        fieldnames(3) = 'unitsString'
        fieldnames(4) = 'unitsDescription'
        fieldnames(5) = 'temperatureMode'
        fieldnames(6) = 'length'
        fieldnames(7) = 'force'
        fieldnames(8) = 'temperature'
        fieldnames(9) = 'temperatureOffset'

        NUMFIELDS = 9

        ID_PTR(1) = mxCreateDoubleScalar(DBLE(DATASETNUM)) ! dataset number
        ID_PTR(2) = mxCreateDoubleScalar(DBLE(DS164F11))   ! units code
        ID_PTR(3) = mxCreateString(UNITSSTR)               ! derived string from units code
        ID_PTR(4) = mxCreateString(DS164F12)               ! units description
        ID_PTR(5) = mxCreateDoubleScalar(DBLE(DS164F13))   ! temperature mode: 1 = absolute, 2 = relative
        ID_PTR(6) = mxCreateDoubleScalar(DS164F21)         ! length
        ID_PTR(7) = mxCreateDoubleScalar(DS164F22)         ! force
        ID_PTR(8) = mxCreateDoubleScalar(DS164F23)         ! temperature
        ID_PTR(9) = mxCreateDoubleScalar(DS164F24)         ! temperature offset





C --- DATASET 58 -------------------------------------------------------
C ----------------------------------------------------------------------
      CASE (58)


C       READ HEADER

C       RECORD 1, ID LINE 1
        READ(LUN, 80, END=10, ERR=9) ID1

C       RECORD 2, ID LINE 2
        READ(LUN, 80, END=10, ERR=9) ID2

C       RECORD 3, ID LINE 3
        READ(LUN, 80, END=10, ERR=9) ID3

C       RECORD 4, ID LINE 4
        READ(LUN, '(A120)', END=10, ERR=9) ID4

C       RECORD 5, ID LINE 5
        READ(LUN, 80, END=10, ERR=9) ID5

C       RECORD 6
5806    FORMAT(2(I5, I10), 2(1X, A10, I10, I4))
        READ(LUN, 5806, END=10, ERR=9)
     &    F61, F62, F63, F64, F65, F66, F67, F68, F69, F610

C       RECORD 7
5807    FORMAT(3I10, 3ES13.5)
        READ(LUN, 5807, END=10, ERR=9)
     &    F71, F72, F73, F74, F75, F76

C       RECORD 8
5808    FORMAT(I10, 3I5, 2(1X, A20))
        READ(LUN, 5808, END=10, ERR=9)
     &    F81, F82, F83, F84, F85, F86

C       RECORD 9
5809    FORMAT(I10, 3I5, 2(1X, A20))
        READ(LUN, 5809, END=10, ERR=9)
     &    F91, F92, F93, F94, F95, F96

C       RECORD 10
5810    FORMAT(I10, 3I5, 2(1X, A20))
        READ(LUN, 5810, END=10, ERR=9)
     &    F101, F102, F103, F104, F105, F106

C       RECORD 11
5811    FORMAT(I10, 3I5, 2(1X, A20))
        READ(LUN, 5811, END=10, ERR=9)
     &    F111, F112, F113, F114, F115, F116


        ORDDATATYPE = DBLE(F71) ! ORDINATE DATA TYPE NUMBER
        ISEVEN = DBLE(F73)      ! IS X/ABSCISSA NON-REGULAR (UNEVEN) OR REGULAR (EVEN)
        ABSC_N = DBLE(F72)     ! NUMBER OF X-VALUES
        NVAL = INT8(ABSC_N)    ! NUMBER OF DATA VALUES



        IF (SKIP_DATA < 0.5) THEN
          IF (NOT(ALLOCATED(XDATA)) .OR. SIZE(XDATA) /= NVAL) THEN
            IF (ALLOCATED(Y_RS)) DEALLOCATE(XDATA)
            ALLOCATE(XDATA(NVAL))
          END IF
        END IF

C       INITIALISE THE CASENUM VARIABLE
        CASENUM = 1

        IF (ISEVEN > 0.5) THEN    ! IF X-DATA EVENLY SPACED
          ABSC_0 = DBLE(F74)       ! CAST X0 TO DOUBLE
          ABSC_D = DBLE(F75)    ! CASE DELTA-X TO DOUBLE
          IF (ABSC_D < 1E-3 .AND. F81 == 17) THEN
            IF (ABSC_D < 1E-4) THEN
              ABSC_D = 2.0_8/KIDNNT(2.0_8/ABSC_D) ! FOR SMALL DX:S APPLY ROUNDING, WHEN ABSCISSA IS TIME
            ELSE
              ABSC_D = 10.0_8/KIDNNT(10.0_8/ABSC_D) ! FOR SMALL DX:S APPLY ROUNDING, WHEN ABSCISSA IS TIME
            END IF
          END IF
          IF (SKIP_DATA < 0.5) THEN
            DO K = 1,NVAL
              XDATA(K) = (K-1)*ABSC_D + ABSC_0
            END DO
          END IF
        ELSE
          ABSC_0 = 0.0_8
          ABSC_D = 0.0_8
          CASENUM = CASENUM + 1 ! = UNEVEN X-DATA
        END IF

        IF (ORDDATATYPE > 4.5) THEN ! = COMPLEX
          CASENUM = CASENUM + 2
        END IF

        IF (ORDDATATYPE == 4 .OR. ORDDATATYPE == 6) THEN ! = DOUBLE
          CASENUM = CASENUM + 4
        END IF



C       FIX TEXT ALIGNMENT
        ID1 = ADJUSTL(ID1)
        ID2 = ADJUSTL(ID2)
        ID3 = ADJUSTL(ID3)
        ID4 = ADJUSTL(ID4)
        ID5 = ADJUSTL(ID5)

        CALL REMOVE_NON_ASCII(ID1, LEN_TRIM(ID1))
        CALL REMOVE_NON_ASCII(ID2, LEN_TRIM(ID2))
        CALL REMOVE_NON_ASCII(ID3, LEN_TRIM(ID3))
        CALL REMOVE_NON_ASCII(ID4, LEN_TRIM(ID4))
        CALL REMOVE_NON_ASCII(ID5, LEN_TRIM(ID5))

        F65 =  ADJUSTL(F65)
        F68 =  ADJUSTL(F68)
        F85 =  ADJUSTL(F85)
        F86 =  ADJUSTL(F86)
        F95 =  ADJUSTL(F95)
        F86 =  ADJUSTL(F86)
        F96 =  ADJUSTL(F96)
        F105 = ADJUSTL(F105)
        F106 = ADJUSTL(F106)
        F115 = ADJUSTL(F115)
        F116 = ADJUSTL(F116)



        IF (USE_BINARY) THEN ! BINARY FORMAT

          DATATYPE = 'BINARY'

          IF (F05 /= 11) THEN ! 58b FORMAT STIPULATES EXACTLY 11 LINES
            S = -6 ! ILL-FORMED FILE
            GO TO 10
          END IF


          INQUIRE(LUN, POS=RECPOS)
          CLOSE(LUN, STATUS='KEEP')

          OPEN (
     &      NEWUNIT = LUN_BIN,
     &      FILE = FILEPATH,
     &      STATUS = 'OLD',
     &      IOSTAT = VAL_INT,
     &      POSITION = 'REWIND',
     &      ACTION = 'READ',
     &      ACCESS = 'STREAM')

          IF (VAL_INT /= 0) THEN
            IF (VAL_INT == 1) THEN ! REMAP IOSTAT ERROR CODE 1 TO -12
              S = -12
            ELSE
              S = VAL_INT
            END IF
            GO TO 10
          END IF


C         --------------------------------------------------------------
          SELECT CASE (CASENUM)
C         --------------------------------------------------------------

C           ------------------------------------------------------------
            CASE (1) ! REAL, SINGLE, EVEN SPACING

              IF (NOT(ALLOCATED(Y_RS))) THEN
                ALLOCATE(Y_RS(NVAL))
              ELSE IF(SIZE(Y_RS) /= NVAL) THEN
                DEALLOCATE(Y_RS)
                ALLOCATE(Y_RS(NVAL))
              END IF

              READ(LUN_BIN, POS=RECPOS, ERR=9) (Y_RS(I), I=1,NVAL)

              IF (SKIP_DATA < 1) THEN
                CXFLAG = 0 ! SET COMPLEX FLAG TO REAL (ZERO)
                YPTR = mxCreateNumericMatrix(NVAL, N1, CID_S, CXFLAG)
                CALL mxCopyReal4ToPtr(Y_RS, mxGetSingles(YPTR), NVAL)
                ID_PTR(3) = YPTR
              END IF


C           ------------------------------------------------------------
            CASE (2) ! REAL, SINGLE, UNEVEN SPACING

              IF (NOT(ALLOCATED(Y_RS))) THEN
                ALLOCATE(Y_RS(NVAL))
              ELSE IF(SIZE(Y_RS) /= NVAL) THEN
                DEALLOCATE(Y_RS)
                ALLOCATE(Y_RS(NVAL))
              END IF

              READ(LUN_BIN, POS=RECPOS, ERR=9)
     &          (XDATA(I), Y_RS(I), I=1,NVAL)

              IF (SKIP_DATA < 1) THEN
                CXFLAG = 0 ! SET COMPLEX FLAG TO REAL (ZERO)
                YPTR = mxCreateNumericMatrix(NVAL, N1, CID_S, CXFLAG)
                CALL mxCopyReal4ToPtr(Y_RS, mxGetSingles(YPTR), NVAL)
                ID_PTR(3) = YPTR
              END IF


C           ------------------------------------------------------------
            CASE (3) ! COMPLEX, SINGLE, EVEN SPACING

              IF (NOT(ALLOCATED(Y_CS))) THEN
                ALLOCATE(Y_CS(NVAL))
              ELSE IF(SIZE(Y_CS) /= NVAL) THEN
                DEALLOCATE(Y_CS)
                ALLOCATE(Y_CS(NVAL))
              END IF

              READ(LUN_BIN, POS=RECPOS, ERR=9) (Y_CS(I), I=1,NVAL)

              IF (SKIP_DATA < 1) THEN
                CXFLAG = 1 ! SET COMPLEX FLAG TO COMPLEX (ONE)
                YPTR = mxCreateNumericMatrix(NVAL, N1, CID_S, CXFLAG)
                CALL mxCopyComplex8ToPtr(Y_CS, mxGetComplexSingles(YPTR)
     &            , NVAL)
                ID_PTR(3) = YPTR
              END IF


C           ------------------------------------------------------------
            CASE (4) ! COMPLEX, SINGLE, UNEVEN SPACING

              IF (NOT(ALLOCATED(Y_CS))) THEN
                ALLOCATE(Y_CS(NVAL))
              ELSE IF(SIZE(Y_CS) /= NVAL) THEN
                DEALLOCATE(Y_CS)
                ALLOCATE(Y_CS(NVAL))
              END IF

              READ(LUN_BIN, POS=RECPOS, ERR=9)
     &          (XDATA(I), Y_CS(I), I = 1,NVAL)

              IF (SKIP_DATA < 1) THEN
                CXFLAG = 1 ! SET COMPLEX FLAG TO COMPLEX (ONE)
                YPTR = mxCreateNumericMatrix(NVAL, N1, CID_S, CXFLAG)
                CALL mxCopyComplex8ToPtr(Y_CS, mxGetComplexSingles(YPTR)
     &            , NVAL)
                ID_PTR(3) = YPTR
              END IF


C           ------------------------------------------------------------
            CASE (5) ! REAL, DOUBLE, EVEN SPACING

              IF (NOT(ALLOCATED(Y_RD))) THEN
                ALLOCATE(Y_RD(NVAL))
              ELSE IF(SIZE(Y_RD) /= NVAL) THEN
                DEALLOCATE(Y_RD)
                ALLOCATE(Y_RD(NVAL))
              END IF

              READ(LUN_BIN, POS=RECPOS, ERR=9) (Y_RD(I), I=1,NVAL)

              IF (SKIP_DATA < 1) THEN
                CXFLAG = 0 ! SET COMPLEX FLAG TO REAL (ZERO)
                YPTR = mxCreateNumericMatrix(NVAL, N1, CID_D, CXFLAG)
                CALL mxCopyReal8ToPtr(Y_RD, mxGetDoubles(YPTR), NVAL)
                ID_PTR(3) = YPTR
              END IF


C           ------------------------------------------------------------
            CASE (6) ! REAL, DOUBLE, UNEVEN SPACING

              IF (NOT(ALLOCATED(Y_RD))) THEN
                ALLOCATE(Y_RD(NVAL))
              ELSE IF(SIZE(Y_RD) /= NVAL) THEN
                DEALLOCATE(Y_RD)
                ALLOCATE(Y_RD(NVAL))
              END IF

              READ(LUN_BIN, POS=RECPOS, ERR=9)
     &          (XDATA(I), Y_RD(I), I=1,NVAL)

              IF (SKIP_DATA < 1) THEN
                CXFLAG = 0 ! SET COMPLEX FLAG TO REAL (ZERO)
                YPTR = mxCreateNumericMatrix(NVAL, N1, CID_D, CXFLAG)
                CALL mxCopyReal8ToPtr(Y_RD, mxGetDoubles(YPTR), NVAL)
                ID_PTR(3) = YPTR
              END IF


C           ------------------------------------------------------------
            CASE (7) ! COMPLEX, DOUBLE, EVEN SPACING

              IF (NOT(ALLOCATED(Y_CD))) THEN
                ALLOCATE(Y_CD(NVAL))
              ELSE IF(SIZE(Y_CD) /= NVAL) THEN
                DEALLOCATE(Y_CD)
                ALLOCATE(Y_CD(NVAL))
              END IF

              READ(LUN_BIN, POS=RECPOS, ERR=9) (Y_CD(I), I=1,NVAL)

              IF (SKIP_DATA < 1) THEN
                CXFLAG = 1 ! SET COMPLEX FLAG TO COMPLEX (ONE)
                YPTR = mxCreateNumericMatrix(NVAL, N1, CID_D, CXFLAG)
                CALL mxCopyComplex16ToPtr(Y_CD,mxGetComplexDoubles(YPTR)
     &            , NVAL)
                ID_PTR(3) = YPTR
              END IF

C           ------------------------------------------------------------
            CASE (8) ! COMPLEX, DOUBLE, UNEVEN SPACING

              IF (NOT(ALLOCATED(Y_CD))) THEN
                ALLOCATE(Y_CD(NVAL))
              ELSE IF(SIZE(Y_CD) /= NVAL) THEN
                DEALLOCATE(Y_CD)
                ALLOCATE(Y_CD(NVAL))
              END IF

              READ(LUN_BIN, POS=RECPOS, ERR=9)
     &          (XDATA(I), Y_CD(I), I=1,NVAL)

              IF (SKIP_DATA < 1) THEN
                CXFLAG = 1 ! SET COMPLEX FLAG TO COMPLEX (ONE)
                YPTR = mxCreateNumericMatrix(NVAL, N1, CID_D, CXFLAG)
                CALL mxCopyComplex16ToPtr(Y_CD,mxGetComplexDoubles(YPTR)
     &            , NVAL)
                ID_PTR(3) = YPTR
              END IF

C         --------------------------------------------------------------
          END SELECT ! END OF CASENUM SWITCH
C         --------------------------------------------------------------



C         GET CURRENT POSITION OF BINARY FILE UNIT
          INQUIRE(LUN_BIN, POS=RECPOS)

C         CLOSE FILE UNIT FOR BINARY READ
          CLOSE(LUN_BIN, STATUS='KEEP')

          OPEN (
     &      NEWUNIT = LUN,
     &      FILE = FILEPATH,
     &      STATUS = 'OLD',
     &      IOSTAT = VAL_INT,
     &      POSITION = 'REWIND',
     &      ACTION = 'READ',
     &      ACCESS = 'STREAM',
     &      ENCODING = 'DEFAULT',
     &      FORM = 'FORMATTED')

C         MOVE NEW FILE UNIT TO PREVIOUS POSITION
          VAL_INT = FSEEK(LUN, RECPOS+1, 1)
          IF (VAL_INT /= 0) THEN
            S = -10
            GO TO 10
          END IF



        ELSE IF (SKIP_DATA > 0.5) THEN ! SKIP READING DATA, JUST METADATA

          REC_TEMP = "0"
          ID_PTR(2) = mxCreateDoubleScalar(1.0_8) ! CREATE FAKE OUTPUT FOR X/ABSCISSA
          ID_PTR(3) = mxCreateDoubleScalar(1.0_8) ! CREATE FAKE OUTPUT FOR DATA


C         SKIP DATA
          SELECT CASE (CASENUM)
            CASE (1) ! REAL, SINGLE, EVEN SPACING
              VAL_INT = FSEEK(LUN, INT4(NVAL*4-6*4), 1)

            CASE (2)  ! REAL, SINGLE, UNEVEN SPACING
              VAL_INT = FSEEK(LUN, INT4(NVAL*2*4-6*4), 1)

            CASE (3)  ! COMPLEX, SINGLE, EVEN SPACING
              VAL_INT = FSEEK(LUN, INT4(NVAL*2*4-6*4), 1)

            CASE (4)  ! COMPLEX, SINGLE, UNEVEN SPACING
              VAL_INT = FSEEK(LUN, INT4(NVAL*3*4-6*4), 1)

            CASE (5)  ! REAL, DOUBLE, EVEN SPACING
              VAL_INT = FSEEK(LUN, INT4(NVAL*8-4*8), 1)

            CASE (6)  ! REAL, DOUBLE, UNEVEN SPACING
              VAL_INT = FSEEK(LUN, INT4(NVAL*8*2-4*8), 1)

            CASE (7)  ! COMPLEX, DOUBLE, EVEN SPACING
              VAL_INT = FSEEK(LUN, INT4(NVAL*8*2-4*8), 1)

            CASE (8)  ! COMPLEX, DOUBLE, UNEVEN SPACING
              VAL_INT = FSEEK(LUN, INT4(NVAL*8*3-4*8), 1)

          END SELECT


          DO WHILE (REC_TEMP /= "    -1")
            READ(LUN, 80, END=11, ERR=9) REC_TEMP
          END DO
          READ(LUN, 80, END=11, ERR=9) REC_TEMP
          REC0 = "    -1"


        ELSE


C       ----------------------------------------------------------------
C       RECORD 12 - ASCII
C       ----------------------------------------------------------------
C       READ DATA ACCORDING TO THE EIGHT CASES

        DATATYPE = 'ASCII'

        SELECT CASE (CASENUM)
C        ---------------------------------------------------------------
         CASE(1) ! REAL, SINGLE, EVEN SPACING

          IF (NOT(ALLOCATED(Y_RS))) THEN
            ALLOCATE(Y_RS(NVAL))
          ELSE IF(SIZE(Y_RS) /= NVAL) THEN
            DEALLOCATE(Y_RS)
            ALLOCATE(Y_RS(NVAL))
          END IF

1001      FORMAT(6ES13.5)
          READ(LUN, 1001, END=9, ERR=9) (Y_RS(I), I=1,NVAL)

          CXFLAG = 0 ! SET COMPLEX FLAG TO REAL (ZERO)
          YPTR = mxCreateNumericMatrix(NVAL, N1, CID_S, CXFLAG)
          CALL mxCopyReal4ToPtr(Y_RS, mxGetSingles(YPTR), NVAL)
          ID_PTR(3) = YPTR


C        ---------------------------------------------------------------
         CASE(2) ! REAL, SINGLE, UNEVEN SPACING

          IF (NOT(ALLOCATED(Y_RS))) THEN
            ALLOCATE(Y_RS(NVAL))
          ELSE IF(SIZE(Y_RS) /= NVAL) THEN
            DEALLOCATE(Y_RS)
            ALLOCATE(Y_RS(NVAL))
          END IF

1002      FORMAT(6ES13.5)
          READ(LUN, 1002, END=9, ERR=9) (XDATA(I), Y_RS(I), I=1,NVAL)

          CXFLAG = 0 ! SET COMPLEX FLAG TO REAL (ZERO)
          YPTR = mxCreateNumericMatrix(NVAL, N1, CID_S, CXFLAG)
          CALL mxCopyReal4ToPtr(Y_RS, mxGetSingles(YPTR), NVAL)
          ID_PTR(3) = YPTR


C        ---------------------------------------------------------------
         CASE(3) ! COMPLEX, SINGLE, EVEN SPACING

          IF (NOT(ALLOCATED(Y_CS))) THEN
            ALLOCATE(Y_CS(NVAL))
          ELSE IF(SIZE(Y_CS) /= NVAL) THEN
            DEALLOCATE(Y_CS)
            ALLOCATE(Y_CS(NVAL))
          END IF

1003      FORMAT(6ES13.5)
          READ(LUN, 1003, END=9, ERR=9) (Y_CS(I), I=1,NVAL)

          CXFLAG = 1 ! SET COMPLEX FLAG TO COMPLEX (ONE)
          YPTR = mxCreateNumericMatrix(NVAL, N1, CID_S, CXFLAG)
          CALL mxCopyComplex8ToPtr(Y_CS, mxGetComplexSingles(YPTR),NVAL)
          ID_PTR(3) = YPTR


C        ---------------------------------------------------------------
         CASE(4) ! COMPLEX, SINGLE, UNEVEN SPACING

          IF (NOT(ALLOCATED(Y_CS))) THEN
            ALLOCATE(Y_CS(NVAL))
          ELSE IF(SIZE(Y_CS) /= NVAL) THEN
            DEALLOCATE(Y_CS)
            ALLOCATE(Y_CS(NVAL))
          END IF

1004      FORMAT(6ES13.5)
          READ(LUN, 1004, END=9, ERR=9) (XDATA(I), Y_CS(I), I = 1,NVAL)

          CXFLAG = 1 ! SET COMPLEX FLAG TO COMPLEX (ONE)
          YPTR = mxCreateNumericMatrix(NVAL, N1, CID_S, CXFLAG)
          CALL mxCopyComplex8ToPtr(Y_CS, mxGetComplexSingles(YPTR),NVAL)
          ID_PTR(3) = YPTR


C        ---------------------------------------------------------------
         CASE(5) ! REAL, DOUBLE, EVEN SPACING

          IF (NOT(ALLOCATED(Y_RD))) THEN
            ALLOCATE(Y_RD(NVAL))
          ELSE IF(SIZE(Y_RD) /= NVAL) THEN
            DEALLOCATE(Y_RD)
            ALLOCATE(Y_RD(NVAL))
          END IF

1005      FORMAT(4ES20.12)
          READ(LUN, 1005, END=9, ERR=9) (Y_RD(I), I=1,NVAL)

          CXFLAG = 0 ! SET COMPLEX FLAG TO REAL (ZERO)
          YPTR = mxCreateNumericMatrix(NVAL, N1, CID_D, CXFLAG)
          CALL mxCopyReal8ToPtr(Y_RD, mxGetDoubles(YPTR), NVAL)
          ID_PTR(3) = YPTR


C        ---------------------------------------------------------------
         CASE(6) ! REAL, DOUBLE, UNEVEN SPACING

          IF (NOT(ALLOCATED(Y_RD))) THEN
            ALLOCATE(Y_RD(NVAL))
          ELSE IF(SIZE(Y_RD) /= NVAL) THEN
            DEALLOCATE(Y_RD)
            ALLOCATE(Y_RD(NVAL))
          END IF

1006      FORMAT(2(ES13.5, ES20.12))
          READ(LUN, 1006, END=9, ERR=9) (XDATA(I), Y_RD(I), I=1,NVAL)

          CXFLAG = 0 ! SET COMPLEX FLAG TO REAL (ZERO)
          YPTR = mxCreateNumericMatrix(NVAL, N1, CID_D, CXFLAG)
          CALL mxCopyReal8ToPtr(Y_RD, mxGetDoubles(YPTR), NVAL)
          ID_PTR(3) = YPTR


C        ---------------------------------------------------------------
         CASE(7) ! COMPLEX, DOUBLE, EVEN SPACING

          IF (NOT(ALLOCATED(Y_CD))) THEN
            ALLOCATE(Y_CD(NVAL))
          ELSE IF(SIZE(Y_CD) /= NVAL) THEN
            DEALLOCATE(Y_CD)
            ALLOCATE(Y_CD(NVAL))
          END IF

1007      FORMAT(4ES20.12)
          READ(LUN, 1007, END=9, ERR=9) (Y_CD(I), I=1,NVAL)

          CXFLAG = 1 ! SET COMPLEX FLAG TO COMPLEX (ONE)
          YPTR = mxCreateNumericMatrix(NVAL, N1, CID_D, CXFLAG)
          CALL mxCopyComplex16ToPtr(Y_CD,mxGetComplexDoubles(YPTR),NVAL)
          ID_PTR(3) = YPTR


C        ---------------------------------------------------------------
         CASE(8) ! COMPLEX, DOUBLE, UNEVEN SPACING

          IF (NOT(ALLOCATED(Y_CD))) THEN
            ALLOCATE(Y_CD(NVAL))
          ELSE IF(SIZE(Y_CD) /= NVAL) THEN
            DEALLOCATE(Y_CD)
            ALLOCATE(Y_CD(NVAL))
          END IF

1008      FORMAT(ES13.5, 2ES20.12)
          READ(LUN, 1008, END=9, ERR=9) (XDATA(I), Y_CD(I), I=1,NVAL)

          CXFLAG = 1 ! SET COMPLEX FLAG TO COMPLEX (ONE)
          YPTR = mxCreateNumericMatrix(NVAL, N1, CID_D, CXFLAG)
          CALL mxCopyComplex16ToPtr(Y_CD,mxGetComplexDoubles(YPTR),NVAL)
          ID_PTR(3) = YPTR


C       ----------------------------------------------------------------
        END SELECT ! END OF CASENUM SWITCH
C       ----------------------------------------------------------------


        END IF ! END OF BINARY/ASCII SWITCH


11      CONTINUE ! CONTINUE WITH PREPARING OUTPUT STRUCTURE


        IF (PREV_DATASETNUM /= DATASETNUM) THEN

          NUMFIELDS = 49

C         DATA
          fieldnames(1) =  'datasetType'
          fieldnames(2) =  'x'
          fieldnames(3) =  'data'
          fieldnames(49) = 'dataType' ! BINARY OR ASCII

C         RECORD 1-5
          fieldnames(4) =  'ID1'
          fieldnames(5) =  'ID2'
          fieldnames(6) =  'date'
          fieldnames(7) =  'ID4'
          fieldnames(8) =  'ID5'

C         RECORD 6
          fieldnames(9) =  'functionType'
          fieldnames(10) = 'functionID'
          fieldnames(11) = 'versionNumber'
          fieldnames(12) = 'loadCaseID'
          fieldnames(13) = 'rspEntName'
          fieldnames(14) = 'rspNode'
          fieldnames(15) = 'rspDir'
          fieldnames(16) = 'refEntName'
          fieldnames(17) = 'refNode'
          fieldnames(18) = 'refDir'

C         RECORD 7
          fieldnames(19) = 'dataFormatType'
          fieldnames(20) = 'numValues'
          fieldnames(21) = 'evenSpacing'
          fieldnames(22) = 'x0'
          fieldnames(23) = 'dx'
          fieldnames(24) = 'zAxisValue'

C         RECORD 8
          fieldnames(25) = 'abscDataType'
          fieldnames(26) = 'abscLengthUnitsExponent'
          fieldnames(27) = 'abscForceUnitsExponent'
          fieldnames(28) = 'abscTempUnitsExponent'
          fieldnames(29) = 'abscAxisLabel'
          fieldnames(30) = 'abscAxisUnitLabel'

C         RECORD 9
          fieldnames(31) = 'ordNumDataType'
          fieldnames(32) = 'ordNumLengthUnitsExponent'
          fieldnames(33) = 'ordNumForceUnitsExponent'
          fieldnames(34) = 'ordNumTempUnitsExponent'
          fieldnames(35) = 'ordNumAxisLabel'
          fieldnames(36) = 'ordNumAxisUnitLabel'

C         RECORD 10
          fieldnames(37) = 'ordDenomDataType'
          fieldnames(38) = 'ordDenomLengthUnitsExponent'
          fieldnames(39) = 'ordDenomForceUnitsExponent'
          fieldnames(40) = 'ordDenomTempUnitsExponent'
          fieldnames(41) = 'ordDenomAxisLabel'
          fieldnames(42) = 'ordDenomAxisUnitLabel'

C         RECORD 11
          fieldnames(43) = 'zDataType'
          fieldnames(44) = 'zLengthUnitsExponent'
          fieldnames(45) = 'zForceUnitsExponent'
          fieldnames(46) = 'zTempUnitsExponent'
          fieldnames(47) = 'zAxisLabel'
          fieldnames(48) = 'zAxisUnitLabel'

        END IF


        IF (SKIP_DATA < 0.5 .AND. ALLOCATED(XDATA)) THEN
C         COPY XDATA TO SINGLE OUTPUT MATRIX
          CXFLAG = 0 ! SET COMPLEX FLAG TO REAL (ZERO)
          ID_PTR(2) = mxCreateNumericMatrix(NVAL, N1, CID_S, CXFLAG)
          CALL mxCopyReal4ToPtr(XDATA, mxGetSingles(ID_PTR(2)), NVAL)
        END IF

C       RECORD 1-5
        ID_PTR(1) = mxCreateDoubleScalar(DBLE(DATASETNUM)) ! Dataset number
        ID_PTR(4) = mxCreateString(ID1)                    ! ID Line 1
        ID_PTR(5) = mxCreateString(ID2)                    ! ID Line 2
        ID_PTR(6) = mxCreateString(ID3)                    ! ID Line 3, date
        ID_PTR(7) = mxCreateString(ID4)                    ! ID Line 4
        ID_PTR(8) = mxCreateString(ID5)                    ! ID Line 5

C       RECORD 6
        ID_PTR(9) =  mxCreateDoubleScalar(DBLE(F61))  ! Function Type
        ID_PTR(10) = mxCreateDoubleScalar(DBLE(F62))  ! Function Identification Number
        ID_PTR(11) = mxCreateDoubleScalar(DBLE(F63))  ! Version Number, or sequence number
        ID_PTR(12) = mxCreateDoubleScalar(DBLE(F64))  ! Load Case Identification Number
        ID_PTR(13) = mxCreateString(F65)              ! Response Entity Name
        ID_PTR(14) = mxCreateDoubleScalar(DBLE(F66))  ! Response Node
        ID_PTR(15) = mxCreateDoubleScalar(DBLE(F67))  ! Response Direction
        ID_PTR(16) = mxCreateString(F68)              ! Reference Entity Name
        ID_PTR(17) = mxCreateDoubleScalar(DBLE(F69))  ! Reference Node
        ID_PTR(18) = mxCreateDoubleScalar(DBLE(F610)) ! Reference Direction

C       RECORD 7
        ID_PTR(19) = mxCreateDoubleScalar(ORDDATATYPE) ! Ordinate Data Type
        ID_PTR(20) = mxCreateDoubleScalar(ABSC_N)      ! Number of data pairs for uneven abscissa...
        ID_PTR(21) = mxCreateDoubleScalar(ISEVEN)      ! Abscissa Spacing (0 = uneven, 1 = even)
        ID_PTR(22) = mxCreateDoubleScalar(ABSC_0)      ! Abscissa minimum (0 if spacing uneven)
        ID_PTR(23) = mxCreateDoubleScalar(ABSC_D)      ! Abscissa increment (0 if spacing uneven)
        ID_PTR(24) = mxCreateDoubleScalar(DBLE(F76))   ! Z-axis value (0 if unused)

C       RECORD 8
        ID_PTR(25) = mxCreateDoubleScalar(DBLE(F81)) ! Abscissa Data Type
        ID_PTR(26) = mxCreateDoubleScalar(DBLE(F82)) ! Length units exponent
        ID_PTR(27) = mxCreateDoubleScalar(DBLE(F83)) ! Force units exponent
        ID_PTR(28) = mxCreateDoubleScalar(DBLE(F84)) ! Temperature units exponent
        ID_PTR(29) = mxCreateString(F85)             ! Axis label
        ID_PTR(30) = mxCreateString(F86)             ! Axis units label

C       RECORD 9
        ID_PTR(31) = mxCreateDoubleScalar(DBLE(F91)) ! Ordinate Data Type
        ID_PTR(32) = mxCreateDoubleScalar(DBLE(F92)) ! Ordinate length units exponent
        ID_PTR(33) = mxCreateDoubleScalar(DBLE(F93)) ! Ordinate force units exponent
        ID_PTR(34) = mxCreateDoubleScalar(DBLE(F94)) ! Ordinate temperature units exponent
        ID_PTR(35) = mxCreateString(F95)             ! Ordinate axis label
        ID_PTR(36) = mxCreateString(F96)             ! Ordinate axis units label

C       RECORD 10
        ID_PTR(37) = mxCreateDoubleScalar(DBLE(F101)) ! Ordinate Denominator Data Type
        ID_PTR(38) = mxCreateDoubleScalar(DBLE(F102)) ! Ordinate Denominator length units exponent
        ID_PTR(39) = mxCreateDoubleScalar(DBLE(F103)) ! Ordinate Denominator force units exponent
        ID_PTR(40) = mxCreateDoubleScalar(DBLE(F104)) ! Ordinate Denominator temperature units exponent
        ID_PTR(41) = mxCreateString(F105)             ! Ordinate Denominator axis label
        ID_PTR(42) = mxCreateString(F106)             ! Ordinate Denominator axis units label

C       RECORD 11
        ID_PTR(43) = mxCreateDoubleScalar(DBLE(F111)) ! Z-Axis Data Type
        ID_PTR(44) = mxCreateDoubleScalar(DBLE(F112)) ! Z-Axis length units exponent
        ID_PTR(45) = mxCreateDoubleScalar(DBLE(F113)) ! Z-Axis force units exponent
        ID_PTR(46) = mxCreateDoubleScalar(DBLE(F114)) ! Z-Axis temperature units exponent
        ID_PTR(47) = mxCreateString(F115)             ! Z-Axis label
        ID_PTR(48) = mxCreateString(F116)             ! Z-Axis units label

C       MISC
        ID_PTR(49) = mxCreateString(DATATYPE) ! Function Type



C --- DATASET 2400 ------------------------------------------------------
C ----------------------------------------------------------------------
      CASE (2400)

C       RECORD 1
24001   FORMAT(I12, 2I6, I12)
        READ(LUN, 24001, END=10, ERR=9) F2400_1_1, F2400_1_2, F2400_1_3,
     & F2400_1_4

C       RECORD 2
24002   FORMAT(40A2)
        READ(LUN, 24002, END=10, ERR=9) F2400_2_1

C       RECORD 3
24003   FORMAT(40A2)
        READ(LUN, 24003, END=10, ERR=9) F2400_3_1

C       RECORD 4
24004   FORMAT(32I2)
        READ(LUN, 24004, END=10, ERR=9) (F2400_4_1(I), I = 1,32)

C       RECORD 5
24005   FORMAT(5I12)
        READ(LUN, 24005, END=10, ERR=9) F2400_5_1, F2400_5_2, F2400_5_3,
     & F2400_5_4, F2400_5_5

C       RECORD 6
24006   FORMAT(I12)
        READ(LUN, 24006, END=10, ERR=9) F2400_6_1


C       DEFINE OUTPUT DATA
        IF (PREV_DATASETNUM /= DATASETNUM) THEN

          NUMFIELDS = 14

          fieldnames(1) =  'datasetType'

C         RECORD 1
          fieldnames(2)  = 'modelUID'
          fieldnames(3)  = 'entityType'
          fieldnames(4)  = 'entitySubtype'
          fieldnames(5)  = 'versionNumber'

C         RECORD 2
          fieldnames(6)  = 'entityName'

C         RECORD 3
          fieldnames(7)  = 'partNumber'

C         RECORD 4
          fieldnames(8)  = 'statusMask'

C         RECORD 5
          fieldnames(9)  = 'date'
          fieldnames(10) = 'time'
          fieldnames(11) = 'itemVersionID'
          fieldnames(12) = 'itemID'
          fieldnames(13) = 'primaryParentUID'

C         RECORD 6
          fieldnames(14) = 'optSwitch'

        END IF


C       COPY DATA TO OUTPUT POINTERS
        ID_PTR(1) = mxCreateDoubleScalar(DBLE(DATASETNUM)) ! Dataset number
C       RECORD 1
        ID_PTR(2)  = mxCreateDoubleScalar(DBLE(F2400_1_1)) ! Model UID
        ID_PTR(3)  = mxCreateDoubleScalar(DBLE(F2400_1_2)) ! Entity type
        ID_PTR(4)  = mxCreateDoubleScalar(DBLE(F2400_1_3)) ! Entity subtype
        ID_PTR(5)  = mxCreateDoubleScalar(DBLE(F2400_1_4)) ! Version number

C       RECORD 2
        ID_PTR(6) = mxCreateString(F2400_2_1)              ! Entity name

C       RECORD 3
        ID_PTR(7) = mxCreateString(F2400_3_1)              ! Part number

C       RECORD 4
        CXFLAG = 0 ! SET COMPLEX FLAG TO REAL (ZERO)
        CID = mxClassIDFromClassName('int8') ! MX FLAG FOR INT8
        M = 32
        N = 1
        ID_PTR(8)  = mxCreateNumericMatrix(M, N, CID, CXFLAG) ! Status mask
        CALL mxCopyInteger1ToPtr(F2400_4_1, mxGetInt8s(ID_PTR(8)), M)

C       RECORD 5
        ID_PTR(9)  = mxCreateDoubleScalar(DBLE(F2400_5_1)) ! Date short time format
        ID_PTR(10)  = mxCreateDoubleScalar(DBLE(F2400_5_2)) ! Time short time format
        ID_PTR(11) = mxCreateDoubleScalar(DBLE(F2400_5_3)) ! IDM item version ID
        ID_PTR(12) = mxCreateDoubleScalar(DBLE(F2400_5_4)) ! IDM item ID
        ID_PTR(13) = mxCreateDoubleScalar(DBLE(F2400_5_5)) ! Primary parent UID

C       RECORD 6
        ID_PTR(14) = mxCreateDoubleScalar(DBLE(F2400_6_1)) ! Optimization switches





C --- DATASET 1806 ------------------------------------------------------
C ----------------------------------------------------------------------
      CASE (1806)

C       RECORD 1
18061   FORMAT(10A2)
        READ(LUN, 24001, END=10, ERR=9) F1806_1_1

C       RECORD 2
18062   FORMAT(10A2,2X,10A2)
        READ(LUN, 24002, END=10, ERR=9) F1806_2_1, F1806_2_2

C       RECORD 3
18063   FORMAT(10A2,2X,10A2,2X,10A2)
        READ(LUN, 24003, END=10, ERR=9) F1806_3_1, F1806_3_2, F1806_3_3

C       RECORD 4
18064   FORMAT(40A2)
        READ(LUN, 24004, END=10, ERR=9) F1806_4_1

C       RECORD 5
18065   FORMAT(3I12,3I6,10A2)
        READ(LUN, 24005, END=10, ERR=9) F1806_5_1, F1806_5_2, F1806_5_3,
     &    F1806_5_4, F1806_5_5, F1806_5_6, F1806_5_7

C       RECORD 6
18066   FORMAT(1P1E15.7)
        READ(LUN, 24006, END=10, ERR=9) F1806_6_1


C       DEFINE OUTPUT DATA
        IF (PREV_DATASETNUM /= DATASETNUM) THEN

          NUMFIELDS = 16

          fieldnames(1) =  'datasetType'

C         RECORD 1
          fieldnames(2)  = 'serial'

C         RECORD 2
          fieldnames(3)  = 'manufacturer'
          fieldnames(4)  = 'model'

C         RECORD 3
          fieldnames(5)  = 'calibrationBy'
          fieldnames(6)  = 'calibrationDate'
          fieldnames(7)  = 'calibrationDue'

C         RECORD 4
          fieldnames(8)  = 'description'

C         RECORD 5
          fieldnames(9)  = 'operatingMode'
          fieldnames(10) = 'dataType'
          fieldnames(11) = 'typeQualifier'
          fieldnames(12) = 'length'
          fieldnames(13) = 'force'
          fieldnames(14) = 'temperature'
          fieldnames(15) = 'unitsLabel'

C         RECORD 6
          fieldnames(16) = 'sensitivity'

        END IF

C       COPY DATA TO OUTPUT POINTERS
        ID_PTR(1) = mxCreateDoubleScalar(DBLE(DATASETNUM)) ! Dataset number
C       RECORD 1
        ID_PTR(2)  = mxCreateString(F1806_1_1)             ! Model UID

C       RECORD 2
        ID_PTR(3) = mxCreateString(F1806_2_1)              ! Manufacturer
        ID_PTR(4) = mxCreateString(F1806_2_2)              ! Model

C       RECORD 3
        ID_PTR(5) = mxCreateString(F1806_3_1)              ! Calibration by
        ID_PTR(6) = mxCreateString(F1806_3_2)              ! Calibration date
        ID_PTR(7) = mxCreateString(F1806_3_3)              ! Calibration due date

C       RECORD 4
        ID_PTR(8) = mxCreateString(F1806_4_1)              ! Transducer description

C       RECORD 5
        ID_PTR(9)  = mxCreateDoubleScalar(DBLE(F1806_5_1)) ! Operating mode
        ID_PTR(10) = mxCreateDoubleScalar(DBLE(F1806_5_2)) ! Data type
        ID_PTR(11) = mxCreateDoubleScalar(DBLE(F1806_5_3)) ! Type qualifier
        ID_PTR(12) = mxCreateDoubleScalar(DBLE(F1806_5_4)) ! Length units exponents
        ID_PTR(13) = mxCreateDoubleScalar(DBLE(F1806_5_4)) ! Force units exponents
        ID_PTR(14) = mxCreateDoubleScalar(DBLE(F1806_5_4)) ! Temperature exponents
        ID_PTR(15) = mxCreateString(F1806_5_7)             ! Units label

C       RECORD 6
        ID_PTR(16) = mxCreateDoubleScalar(F1806_6_1)       ! Sensitivity (mv/EU)


      END SELECT ! END OF DATA SET NUMBER SWITCH


      PREV_DATASETNUM = DATASETNUM


C     CREATE AND COPY STRUCT TO CELL ARRAY, IF DATASET NUMBER IS SUPPORTED
      IF (ANY(DATASETNUM == DATASETS)) THEN
        
        S = 1 ! -> SUCCESS = TRUE
      
        NCELL = NCELL + 1 ! INCREMENT CELL DATA RECORD COUNTER WITH ONE
        
C       CHECK IF STRUCT POINTER ARRAYS NEEDS TO BE RESIZED
        IF (NCELL > MAXRECORDS_LOCAL) THEN
          MAXRECORDS_LOCAL = MAXRECORDS_LOCAL + RECORD_STEP

          SELECT CASE (USE_ALLOCATION)
           CASE (0)
            USE_ALLOCATION = 1 ! USE SPTR_1
            ALLOCATE(SPTR_1(MAXRECORDS_LOCAL)) ! RESIZE SPTR_1
            SPTR_1(1:NCELL-1) = SPTR(:) ! COPY STATICALLY SIZED SPTR CONTENT TO SPTR_1

           CASE (1)
            USE_ALLOCATION = 2 ! USE SPTR_2
            ALLOCATE(SPTR_2(MAXRECORDS_LOCAL)) ! RESIZE SPTR_2
            SPTR_2(1:NCELL-1) = SPTR_1(:) ! COPY SPTR_1 CONTENT TO SPTR_2
            DEALLOCATE(SPTR_1)

           CASE (2)
            USE_ALLOCATION = 1 ! USE SPTR_1
            ALLOCATE(SPTR_1(MAXRECORDS_LOCAL)) ! RESIZE SPTR_1
            SPTR_1(1:NCELL-1) = SPTR_2(:) ! COPY SPTR_2 CONTENT TO SPTR_1
            DEALLOCATE(SPTR_2)

          END SELECT

        END IF

        SELECT CASE (USE_ALLOCATION)
          CASE (0)
            SPTR(NCELL) = mxCreateStructMatrix(N1, N1, NUMFIELDS,
     &        fieldnames(1:NUMFIELDS))
            DO K = 1,NUMFIELDS
              CALL mxSetField(SPTR(NCELL),N1,fieldnames(K),ID_PTR(K))
            END DO

          CASE (1)
            SPTR_1(NCELL) = mxCreateStructMatrix(N1, N1, NUMFIELDS,
     &        fieldnames(1:NUMFIELDS))
             DO K = 1,NUMFIELDS
              CALL mxSetField(SPTR_1(NCELL),N1,fieldnames(K),ID_PTR(K))
             END DO

          CASE (2)
            SPTR_2(NCELL) = mxCreateStructMatrix(N1, N1, NUMFIELDS,
     &        fieldnames(1:NUMFIELDS))
             DO K = 1,NUMFIELDS
              CALL mxSetField(SPTR_2(NCELL),N1,fieldnames(K),ID_PTR(K))
             END DO

        END SELECT

        IF (SKIP_DATA < 0.5 .OR. USE_BINARY) THEN
          READ(LUN, 80, END=10, ERR=9) REC0
          READ(LUN, 80, END=10, ERR=9) REC0 ! NB: READ TWICE
        END IF


      ELSE IF (DATASETNUM == -58) THEN
        REC0 = ID1
C       DO NOTHING HERE -> REPEAT LOOP


      ELSE ! INVALID OR UNSUPPORTED DATASET NUMBER
        REC0 = "0"

C       CHECK STRICTLY FOR THE CORRECTLY FORMATTED END TERMINATOR
        DO WHILE (REC0 /= "    -1")
          READ(LUN, 80, END=10, ERR=9) REC0
        END DO


      END IF



C ----------------------------------------------------------------------
      END DO ! END OF WHILE LOOP
C ----------------------------------------------------------------------


      GO TO 10 ! SKIP ERROR LABEL 9 => NORMAL EXIT


C --- FINISHED READING -------------------------------------------------
C ----------------------------------------------------------------------



C --- RETURN ARGUMENTS AND CLOSE FILE AND VARIABLES

C     ERROR EXIT
9     S = -9
C       NCELL = NCELL - 1 ! DECREASE CELL COUNT BY ONE, SINCE THE LAST ONE FAILED

10    CONTINUE ! NORMAL EXIT

      plhs(1) = mxCreateDoubleScalar(S) ! CREATE FIRST RETURN ARGUMENT: STATE


C     DEALLOCATE ALLOCATED VARIABLES
      IF (ALLOCATED(FILEPATH)) DEALLOCATE(FILEPATH)
      IF (ALLOCATED(XDATA)) DEALLOCATE(XDATA)
      IF (ALLOCATED(Y_RS)) DEALLOCATE(Y_RS)
      IF (ALLOCATED(Y_RD)) DEALLOCATE(Y_RD)
      IF (ALLOCATED(Y_CS)) DEALLOCATE(Y_CS)
      IF (ALLOCATED(Y_CD)) DEALLOCATE(Y_CD)
      IF (ALLOCATED(Y_CD)) DEALLOCATE(Y_CD)


C     IF LUN HAVE BEEN INITIALISED
      IF (LUN /= 0) THEN
        INQUIRE(UNIT=LUN, OPENED=OK) ! CHECK IF FILE UNIT IS OPEN
        IF (OK) THEN
          CLOSE(LUN, STATUS='KEEP') ! CLOSE IF OPEN
        END IF
      END IF
C     IF LUN_BIN HAVE BEEN INITIALISED
      IF (LUN_BIN /= 0) THEN
        INQUIRE(UNIT=LUN_BIN, OPENED=OK) ! CHECK IF BINARY FILE UNIT IS OPEN
        IF (OK) THEN
          CLOSE(LUN_BIN, STATUS='KEEP') ! CLOSE IF OPEN
        END IF
      END IF


C     CREATE AND ASSIGN CELL MATRIX
      IF (nlhs > 1) THEN
        IF (NCELL > 0) THEN
          CELL_PTR = mxCreateCellMatrix(NCELL, N1)
          SELECT CASE (USE_ALLOCATION)
            CASE (0)
              DO KMW = 1,NCELL
                CALL mxSetCell(CELL_PTR, KMW, SPTR(KMW))
              END DO

            CASE (1)
              DO KMW = 1,NCELL
                CALL mxSetCell(CELL_PTR, KMW, SPTR_1(KMW))
              END DO

            CASE (2)
              DO KMW = 1,NCELL
                CALL mxSetCell(CELL_PTR, KMW, SPTR_2(KMW))
              END DO

          END SELECT
          plhs(2) = CELL_PTR ! ASSIGN POINTER TO OUTPUT ARGUMENT #2

        ELSE IF (INT(S) == 1) THEN
          CELL_PTR = mxCreateCellMatrix(N1, N1)
          plhs(2) = CELL_PTR ! ASSIGN POINTER TO OUTPUT ARGUMENT #2

        ELSE ! OUTPUT ERROR STRING IF NOT SUCCESSFUL
          SELECT CASE (INT(S))
            CASE (0)
              plhs(2) = mxCreateString('Unknown error.')

            CASE (-1)
              plhs(2) = mxCreateString('Empty filepath input.')

            CASE (-2)
              plhs(2) = mxCreateString('Could not read filepath.')

            CASE (-3)
              plhs(2) = mxCreateString('File does not exist.')

            CASE (-4)
              plhs(2) = mxCreateString('Too few inputs.')

            CASE (-5)
              plhs(2) = mxCreateString('Input is not of type char.')

            CASE (-6)
              plhs(2) = mxCreateString('Filepath is ill-formed.')

            CASE (-7)
              plhs(2) = mxCreateString('2 output arguments are needed.')

            CASE (-9)
              plhs(2) = mxCreateString('Error while reading file.')

            CASE (-12)
              plhs(2) = mxCreateString('Arithmetic overflow.')

            CASE DEFAULT
              plhs(2) = mxCreateString('Read error: check IOSTAT code.')

          END SELECT
        END IF

      END IF


C     IF DYNAMIC STRUCT RE-ALLOCATION HAVE BEEN USED -> DEALLOCATE DYNAMIC STRUCT POINTERS
      IF (USE_ALLOCATION > 0) THEN
        IF (ALLOCATED(SPTR_1)) DEALLOCATE(SPTR_1)
        IF (ALLOCATED(SPTR_2)) DEALLOCATE(SPTR_2)
      END IF



      RETURN
      END


      SUBROUTINE REMOVE_NON_ASCII(STR, L)
        CHARACTER(LEN=L), INTENT(INOUT) :: STR
        INTEGER :: K

        DO K = 1,L
          STR(K:K) = ACHAR(MIN(IACHAR(STR(K:K)), 127))
        END DO

      END SUBROUTINE REMOVE_NON_ASCII


C ----------------------------------------------------------------------
C --- END OF PROGRAM ---------------------------------------------------
C ----------------------------------------------------------------------


