#include "fintrf.h"


C ----------------------------------------------------------------------
C
C     [STATE, DATACELL] = READUNV(FILEPATH)
C
C     NB: ALL ARGUMENTS ARE REQUIRED (TWO OUTPUTS AND ONE INPUT)
C
C
C     FILEPATH: CHAR WITH RELATIVE OR ABSOLUTE PATH TO FILE TO READ
C
C     DATACELL: CELL ARRAY FOR EACH DATASET IN FILE
C               IF NO DATA READ -> CHAR WITH ERROR DESCRIPTION
C
C     STATE FLAGS:
C       =  1,  SUCCESFUL
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
C       = -12, FLOATING POINT ARITHMETIC OVERFLOW (FROM IOSTAT)
C       = >1,  ERROR CODE FROM IOSTAT WHEN OPENING FILE
C
C ----------------------------------------------------------------------


      SUBROUTINE mexFunction(nlhs, plhs, nrhs, prhs)


      IMPLICIT NONE      
      

      mwPointer :: plhs(*), prhs(*)
      mwSize :: nlhs, nrhs
      mwPointer :: mxCreateString
      mwPointer :: mxCreateDoubleScalar
      REAL*8 :: mxGetScalar
      mwPointer :: mxGetDoubles
      mwPointer :: mxGetSingles
      mwPointer :: mxGetComplexDoubles
      mwPointer :: mxGetComplexSingles
      mwPointer :: mxCreateCellMatrix
      INTEGER*4 :: mxClassIDFromClassName, CXFLAG, CID_S, CID_D, FSEEK
      mwPointer :: mxCreateNumericMatrix
      mwPointer :: mxCreateStructMatrix
      mwPointer :: mxGetN
      mwPointer :: mxGetString
      mwPointer :: mxIsChar
      mwPointer :: mxIsDouble
      mwPointer :: mxIsEmpty
      mwSize :: STRLEN
      mwPointer :: CELL_PTR, YPTR
      INTEGER*4, PARAMETER :: MAXRECORDS = 4096
      INTEGER*4, PARAMETER :: RECORD_STEP = 16384
      mwPointer :: SPTR(MAXRECORDS)
      mwPointer, DIMENSION(:), ALLOCATABLE :: SPTR_1, SPTR_2 ! USE WHEN NUM. OF DATA RECORDS > MAXRECORDS
      mwSize, PARAMETER :: N1 = 1
      mwSize :: NVAL, KMW, NCELL
      REAL*8 :: SKIP_DATA
      
C       integer*4, parameter :: ucs4  = selected_char_kind ("ISO_10646")


      INTEGER*4, PARAMETER :: NFIELDS = 48 ! MAXIMUM NUMBER OF FIELDS IN USE, ADJUST WHEN NEEDED
      mwPointer :: ID_PTR(NFIELDS)
      CHARACTER*32 :: fieldnames(NFIELDS) ! LENGTH OF LONGEST FIELDNAME (32), ADJUST WHEN NEEDED

      CHARACTER(LEN=80) :: REC0
      CHARACTER(LEN=80) :: ID0, ID1, ID2, ID3, ID5 ! ID0 IS A TEMP. VAR.
      CHARACTER(LEN=120) :: ID4
      CHARACTER(LEN=10) :: F65, F68
      CHARACTER(LEN=10) :: DS151F41, DS151F42
      CHARACTER(LEN=10) :: DS151F51, DS151F52
      CHARACTER(LEN=10) :: DS151F71, DS151F72
      CHARACTER(LEN=20) :: F85, F86
      CHARACTER(LEN=20) :: F95, F96
      CHARACTER(LEN=20) :: F105, F106
      CHARACTER(LEN=20) :: F115, F116
      CHARACTER(LEN=20) :: DS164F12
      CHARACTER(LEN=22) :: UNITSSTR
      CHARACTER(LEN=1) :: F02
      INTEGER*8 :: F05, F06, F09, F010
      INTEGER*4 :: F03, F04, F07, F08
      INTEGER*4 :: DS151F43, DS151F44, DS151F45
      INTEGER*4 :: F61, F62, F63, F64, F66, F67, F69, F610
      INTEGER*4 :: F71, F72, F73
      INTEGER*4 :: F81, F82, F83, F84
      INTEGER*4 :: F91, F92, F93, F94
      INTEGER*4 :: F101, F102, F103, F104
      INTEGER*4 :: F111, F112, F113, F114
      INTEGER*4 :: DS164F11, DS164F13
      REAL*8 :: DS164F21, DS164F22, DS164F23, DS164F24
      REAL*4 :: F74, F75, F76
      REAL*8 :: ISEVEN, ABSCNUM, ABSCINCR, ABSC0, ORDDATATYPE
      REAL*8 :: S(1)
      INTEGER*1 :: USE_ALLOCATION
      INTEGER*4 :: STATUS_INT, OK, NOK, DATASETNUM, CASENUM, LUN, STEP
      INTEGER*4, PARAMETER :: DATASETS(3) = (/151, 164, 58/)
      INTEGER*8 :: N, I, K, FIELDSTOUSE, MAXRECORDS_LOCAL
      LOGICAL :: STATUS_BOOL
      CHARACTER(LEN=:), ALLOCATABLE :: FILEPATH
      REAL*4, DIMENSION(:), ALLOCATABLE :: Y_RS, XDATA
      REAL*8, DIMENSION(:), ALLOCATABLE :: Y_RD
      COMPLEX*8, DIMENSION(:), ALLOCATABLE :: Y_CS
      COMPLEX*16, DIMENSION(:), ALLOCATABLE :: Y_CD


C     INITIALISE (NON-PARAMETRIC) VARIABLES
      NCELL = 0 ! CELL COUNTER
      S = 0.0 ! OUTPUT STATE
      LUN = 0 ! FILE UNIT NUMBER
      SKIP_DATA = 0.0 ! SKIP READING DATA
      MAXRECORDS_LOCAL = MAXRECORDS ! MAXIMUM NUMBER OF RECORDS CURRENTLY
      USE_ALLOCATION = 0 ! USE DYNAMIC ALLOCATION OF OUTPUT CELL ARRAY
      CID_S = mxClassIDFromClassName('single') ! FLAG FOR SINGLE PRECISION
      CID_D = mxClassIDFromClassName('double') ! FLAG FOR DOUBLE PRECISION


C     AT LEAST ONE INPUT ARGUMENT (FILEPATH) IS REQUIRED
      IF (nrhs < 1) THEN
        S = -4.0
        GO TO 10
      ELSE IF (nrhs > 1) THEN
        IF (NOT(mxIsEmpty(prhs(2))) .AND. mxIsDouble(prhs(2))) THEN
          SKIP_DATA = DBLE(mxGetScalar(prhs(2)))
        ELSE
          SKIP_DATA = 0.0
        END IF
      END IF


C     CHECK IF TWO OUTPUT ARGUMENTS ARE USED, IF NOT -> ERROR
      IF (nlhs /= 2) THEN
        S = -7.0
        GO TO 10
      END IF


C     CHECK IF INPUT ARGUMENT IS OF CHAR TYPE
      OK = mxIsChar(prhs(1))
      IF (NOT(OK)) THEN ! CHAR TYPE IS REQUIRED
        S = -5.0
        GO TO 10
      END IF


C     GET LENGTH OF INPUT STRING
      STRLEN = mxGetN(prhs(1))
      IF (STRLEN < 1) THEN ! EMPTY FILEPATH -> ERROR
        S = -1.0
        GO TO 10
      END IF


C     ALLOCATE FILEPATH FOR REQUIRED LENGTH
      ALLOCATE(CHARACTER(LEN=STRLEN) :: FILEPATH)


C     COPY INPUT STRING TO FILEPATH
      NOK = mxGetString(prhs(1), FILEPATH, STRLEN)
      IF (NOK /= 0) THEN ! COPYING FAILED
        S = -2.0
        GO TO 10
      END IF


C     CHECK IF FILEPATH EXIST
      INQUIRE(FILE=FILEPATH, EXIST=STATUS_BOOL)
      IF (NOT(STATUS_BOOL)) THEN
        S = -3.0
        GO TO 10
      END IF


C     OPEN FILE LOCATED AT <FILEPATH>
      LUN = 7
      OPEN (
     & UNIT = LUN,
     & FILE = FILEPATH,
     & STATUS = 'OLD',
     & IOSTAT = STATUS_INT,
     & POSITION = 'REWIND',
     & ACTION = 'READ',
     & ENCODING = 'DEFAULT',
     & FORM = 'FORMATTED')


C     CHECK IF FILE OPENING FAILED -> RETURN IOSTAT CODE
      IF (STATUS_INT /= 0) THEN
        IF (STATUS_INT == 1) THEN ! REMAP IOSTAT ERROR CODE 1 TO -12
          S = -12.0
        ELSE
          S = STATUS_INT
        END IF
        GO TO 10
      END IF



C     READ FIRST LINE IN FILE
      READ(LUN, '(A80)', END=10, ERR=9) REC0



C ----------------------------------------------------------------------
C     READ DATA WHILE DATARECORD IS PREPENDED WITH "-1"
      DO WHILE (ADJUSTL(REC0) == "-1")



C     READ DATASET NUMBER, 58B COMPATIBILITY PREPARED (BUT NOT SUPPORTED YET)
      READ(LUN, 582, END=10, ERR=9)
     &  DATASETNUM, F02, F03, F04, F05, F06, F07, F08, F09, F010
582   FORMAT(I6, 1A1, I6,I6, I12, I12, I6, I6, I12, I12)



C     CODE RELATED TO ALL VALID DATASETS
      IF (ANY(DATASETNUM == DATASETS)) THEN

        NCELL = NCELL + 1 ! INCREMENT CELL DATA RECORD COUNTER WITH ONE

        S = 1.0 ! -> SUCCESS = TRUE

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

      END IF



C --- DATASET 151 ------------------------------------------------------
C ----------------------------------------------------------------------
      IF (DATASETNUM == 151) THEN

C       RECORD 1, ID LINE 1, model file name
        READ(LUN, '(A80)', END=10, ERR=9) ID1

C       RECORD 2, ID LINE 2, model file description
        READ(LUN, '(A80)', END=10, ERR=9) ID2

C       RECORD 3, ID LINE 3, program which created DB
        READ(LUN, '(A80)', END=10, ERR=9) ID3

C       RECORD 4
        READ(LUN, 1514, END=10, ERR=9)
     &    DS151F41, DS151F42, DS151F43, DS151F44, DS151F45
1514    FORMAT(A10, A10, 3I10)

C       RECORD 5
        READ(LUN, 1515, END=10, ERR=9)
     &    DS151F51, DS151F52
1515    FORMAT(A10, A10)

C       RECORD 6, ID LINE 6, program which created unv-file
        READ(LUN, '(A80)', END=10, ERR=9) ID4

C       RECORD 7
        READ(LUN, 1517, END=10, ERR=9)
     &    DS151F71, DS151F72
1517    FORMAT(A10, A10)


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


        fieldnames(1) = 'datasetType'
        fieldnames(2) = 'modelName'
        fieldnames(3) = 'modelDescription'
        fieldnames(4) = 'databaseProgram'
        fieldnames(5) = 'databaseCreatedDate'
        fieldnames(6) = 'databaseCreatedTime'
        fieldnames(7) = 'databaseVersion1'
        fieldnames(8) = 'databaseVersion2'
        fieldnames(9) = 'fileType'
        fieldnames(10) = 'databaseLastSavedDate'
        fieldnames(11) = 'databaseLastSavedTime'
        fieldnames(12) = 'unvProgram'
        fieldnames(13) = 'fileCreatedDate'
        fieldnames(14) = 'fileCreatedTime'

        FIELDSTOUSE = 14 ! NUMBER OF FIELDS IN DATASET 151        
        

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
      ELSEIF (DATASETNUM == 164) THEN

C       RECORD 1
        READ(LUN, 1641, END=10, ERR=9)
     &    DS164F11, DS164F12, DS164F13
1641    FORMAT(I10, A20, I10)

C       RECORD 2
        READ(LUN, 1642, END=10, ERR=9)
     &    DS164F21, DS164F22, DS164F23, DS164F24
1642    FORMAT(3D25.17)


        SELECT CASE (DS164F11)
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

        FIELDSTOUSE = 9

        ID_PTR(1) = mxCreateDoubleScalar(DBLE(DATASETNUM)) ! dataset number
        ID_PTR(2) = mxCreateDoubleScalar(DBLE(DS164F11)) ! units code
        ID_PTR(3) = mxCreateString(UNITSSTR) ! derived string from units code
        ID_PTR(4) = mxCreateString(DS164F12) ! units description
        ID_PTR(5) = mxCreateDoubleScalar(DBLE(DS164F13)) ! temperature mode: 1 = absolute, 2 = relative
        ID_PTR(6) = mxCreateDoubleScalar(DS164F21) ! length
        ID_PTR(7) = mxCreateDoubleScalar(DS164F22) ! force
        ID_PTR(8) = mxCreateDoubleScalar(DS164F23) ! temperature
        ID_PTR(9) = mxCreateDoubleScalar(DS164F24) ! temperature offset





C --- DATASET 58 -------------------------------------------------------
C ----------------------------------------------------------------------
      ELSEIF (DATASETNUM == 58) THEN


C       READ HEADER

C       RECORD 1, ID LINE 1
        READ(LUN, '(A80)', END=10, ERR=9) ID1

C       RECORD 2, ID LINE 2
        READ(LUN, '(A80)', END=10, ERR=9) ID2

C       RECORD 3, ID LINE 3
        READ(LUN, '(A80)', END=10, ERR=9) ID3

C       RECORD 4, ID LINE 4
        READ(LUN, '(A120)', END=10, ERR=9) ID4

C       RECORD 5, ID LINE 5
        READ(LUN, '(A80)', END=10, ERR=9) ID5

C       RECORD 6
        READ(LUN, 5806, END=10, ERR=9)
     &    F61, F62, F63, F64, F65, F66, F67, F68, F69, F610
5806    FORMAT(2(I5, I10), 2(1X, A10, I10, I4))

C       RECORD 7
        READ(LUN, 5807, END=10, ERR=9)
     &    F71, F72, F73, F74, F75, F76
5807    FORMAT(3I10, 3ES13.5)


        ORDDATATYPE = DBLE(F71)
        ISEVEN = DBLE(F73)
        ABSCNUM = DBLE(F72)
        NVAL = INT8(ABSCNUM)
        ABSC0 = DBLE(F74)
        ABSCINCR = DBLE(F75)


C       RECORD 8
        READ(LUN, 5808, END=10, ERR=9)
     &    F81, F82, F83, F84, F85, F86
5808    FORMAT(I10, 3I5, 2(1X, A20))

C       RECORD 9
        READ(LUN, 5809, END=10, ERR=9)
     &    F91, F92, F93, F94, F95, F96
5809    FORMAT(I10, 3I5, 2(1X, A20))

C       RECORD 10
        READ(LUN, 5810, END=10, ERR=9)
     &    F101, F102, F103, F104, F105, F106
5810    FORMAT(I10, 3I5, 2(1X, A20))

C       RECORD 11
        READ(LUN, 5811, END=10, ERR=9)
     &    F111, F112, F113, F114, F115, F116
5811    FORMAT(I10, 3I5, 2(1X, A20))

        CASENUM = 1

        IF (SKIP_DATA < 1) THEN
          ALLOCATE(XDATA(NVAL))
        END IF

        IF (ISEVEN > 0.5) THEN ! XDATA EVENLY SPACED
          IF (ABSCINCR < 1E-3 .AND. F81 == 17) THEN
            IF (ABSCINCR < 1E-4) THEN
              ABSCINCR = 2.0/NINT(2.0/ABSCINCR) ! FOR SMALL DX:S APPLY ROUNDING, WHEN ABSCISSA IS TIME
            ELSE
              ABSCINCR = 10.0/NINT(10.0/ABSCINCR) ! FOR SMALL DX:S APPLY ROUNDING, WHEN ABSCISSA IS TIME
            END IF
          END IF
          IF (SKIP_DATA < 1) THEN
            DO K = 1,NVAL
              XDATA(K) = (K-1)*ABSCINCR + ABSC0
            END DO
          END IF
        ELSE
          CASENUM = CASENUM + 1 ! = UNEVEN XDATA
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
        
        F65 = ADJUSTL(F65)
        F68 = ADJUSTL(F68)
        F85 = ADJUSTL(F85)
        F86 = ADJUSTL(F86)
        F95 = ADJUSTL(F95)
        F86 = ADJUSTL(F86)
        F96 = ADJUSTL(F96)
        F105 = ADJUSTL(F105)
        F106 = ADJUSTL(F106)
        F115 = ADJUSTL(F115)
        F116 = ADJUSTL(F116)



        IF (F02 == 'b' .OR. F02 == 'B') THEN ! BINARY FORMAT

C         SKIP FOR NOW
          ID1 = "0"
          DO WHILE (ID1 /= "    -1")
            READ(LUN, '(A80)', END=10, ERR=9) ID1
          END DO
          DATASETNUM = -58
          
          
        ELSE IF (SKIP_DATA > 0) THEN
        
C         SKIP READING DATA, JUST METADATA
          ID0 = "0"
          ID_PTR(2) = mxCreateDoubleScalar(DBLE(1.0)) ! CREATE FAKE OUTPUT
          ID_PTR(3) = mxCreateDoubleScalar(DBLE(1.0)) ! CREATE FAKE OUTPUT
          
          IF      (CASENUM == 1) THEN ! REAL, SINGLE, EVEN SPACING
            STATUS_INT = FSEEK(LUN, NVAL*4-6*4, 1)
            
          ELSE IF (CASENUM == 2) THEN ! REAL, SINGLE, UNEVEN SPACING
            STATUS_INT = FSEEK(LUN, NVAL*2*4-6*4, 1)
            
          ELSE IF (CASENUM == 3) THEN ! COMPLEX, SINGLE, EVEN SPACING
            STATUS_INT = FSEEK(LUN, NVAL*2*4-6*4, 1)          
          
          ELSE IF (CASENUM == 4) THEN ! COMPLEX, SINGLE, UNEVEN SPACING
            STATUS_INT = FSEEK(LUN, NVAL*3*4-6*4, 1)   
          
          ELSE IF (CASENUM == 5) THEN ! REAL, DOUBLE, EVEN SPACING
            STATUS_INT = FSEEK(LUN, NVAL*8-4*8, 1)
          
          ELSE IF (CASENUM == 6) THEN ! REAL, DOUBLE, UNEVEN SPACING
            STATUS_INT = FSEEK(LUN, NVAL*8*2-4*8, 1)
          
          ELSE IF (CASENUM == 7) THEN ! COMPLEX, DOUBLE, EVEN SPACING
            STATUS_INT = FSEEK(LUN, NVAL*8*2-4*8, 1)
          
          ELSE IF (CASENUM == 8) THEN ! COMPLEX, DOUBLE, UNEVEN SPACING
            STATUS_INT = FSEEK(LUN, NVAL*8*3-4*8, 1)
          
          END IF
          
          DO WHILE (ADJUSTL(ID0) /= "-1")
            READ(LUN, '(A80)', END=11, ERR=9) ID0
          END DO
          READ(LUN, '(A80)', END=11, ERR=9) ID0
          REC0 = "    -1"          
          

        ELSE

C       RECORD 12 - ASCII
C       READ DATA ACCORDING TO THE EIGHT CASES
        N = 0 ! INITIALISE NUMBER OF POINTS READ

C       CASE 1, 58
        IF (CASENUM == 1) THEN ! REAL, SINGLE, EVEN SPACING

          ALLOCATE(Y_RS(NVAL))

1001      FORMAT(6ES13.5)
          READ(LUN, 1001, END=9, ERR=9) (Y_RS(I), I = 1,NVAL)
          N = NVAL

          IF (SKIP_DATA < 1) THEN
            CXFLAG = 0
            YPTR = mxCreateNumericMatrix(NVAL, N1, CID_S, CXFLAG)
            CALL mxCopyReal4ToPtr(Y_RS, mxGetSingles(YPTR), NVAL)
            ID_PTR(3) = YPTR
          END IF

          DEALLOCATE(Y_RS)


C       CASE 2, 58
        ELSEIF (CASENUM == 2) THEN ! REAL, SINGLE, UNEVEN SPACING

          ALLOCATE(Y_RS(NVAL))

1002      FORMAT(6ES13.5)
          DO WHILE (N < NVAL)
            STEP = MIN(3, NVAL-N)
            READ(LUN, 1002, END=9, ERR=9)
     &        (XDATA(I+N), Y_RS(I+N), I = 1,STEP)
            N = N + STEP
          END DO

          CXFLAG = 0
          YPTR = mxCreateNumericMatrix(NVAL, N1, CID_S, CXFLAG)
          CALL mxCopyReal4ToPtr(Y_RS, mxGetSingles(YPTR), NVAL)
          ID_PTR(3) = YPTR

          DEALLOCATE(Y_RS)


C       CASE 3, 58
        ELSEIF (CASENUM == 3) THEN ! COMPLEX, SINGLE, EVEN SPACING

          ALLOCATE(Y_CS(NVAL))

1003      FORMAT(6ES13.5)
          READ(LUN, 1003, END=9, ERR=9) (Y_CS(I), I = 1,NVAL)
          N = NVAL

          CXFLAG = 1
          YPTR = mxCreateNumericMatrix(NVAL, N1, CID_S, CXFLAG)
          CALL mxCopyComplex8ToPtr(Y_CS, mxGetComplexSingles(YPTR),NVAL)
          ID_PTR(3) = YPTR

          DEALLOCATE(Y_CS)


C       CASE 4, 58
        ELSEIF (CASENUM == 4) THEN ! COMPLEX, SINGLE, UNEVEN SPACING

          ALLOCATE(Y_CS(NVAL))

1004      FORMAT(6ES13.5)
          DO WHILE (N < NVAL)
            STEP = MIN(2, NVAL-N)
            READ(LUN, 1004, END=9, ERR=9)
     &        (XDATA(I+N), Y_CS(I+N), I = 1,STEP)
            N = N + STEP
          END DO

          CXFLAG = 1
          YPTR = mxCreateNumericMatrix(NVAL, N1, CID_S, CXFLAG)
          CALL mxCopyComplex8ToPtr(Y_CS, mxGetComplexSingles(YPTR),NVAL)
          ID_PTR(3) = YPTR

          DEALLOCATE(Y_CS)


C       CASE 5, 58
        ELSEIF (CASENUM == 5) THEN ! REAL, DOUBLE, EVEN SPACING

          ALLOCATE(Y_RD(NVAL))

1005      FORMAT(4ES20.12)
          READ(LUN, 1005, END=9, ERR=9) (Y_RD(I), I = 1,NVAL)

          CXFLAG = 0
          YPTR = mxCreateNumericMatrix(NVAL, N1, CID_D, CXFLAG)
          CALL mxCopyReal8ToPtr(Y_RD, mxGetDoubles(YPTR), NVAL)
          ID_PTR(3) = YPTR

          DEALLOCATE(Y_RD)


C       CASE 6, 58
        ELSEIF (CASENUM == 6) THEN ! REAL, DOUBLE, UNEVEN SPACING

          ALLOCATE(Y_RD(NVAL))

1006      FORMAT(2(ES13.5,ES20.12))
          DO WHILE (N < NVAL)
            STEP = MIN(2, NVAL-N)
            READ(LUN, 1006, END=9, ERR=9)
     &        (XDATA(I+N), Y_RD(I+N), I = 1,STEP)
            N = N + STEP
          END DO

          CXFLAG = 0
          YPTR = mxCreateNumericMatrix(NVAL, N1, CID_D, CXFLAG)
          CALL mxCopyReal8ToPtr(Y_RD, mxGetDoubles(YPTR), NVAL)
          ID_PTR(3) = YPTR

          DEALLOCATE(Y_RD)


C       CASE 7, 58
        ELSEIF (CASENUM == 7) THEN ! COMPLEX, DOUBLE, EVEN SPACING

          ALLOCATE(Y_CD(NVAL))

1007      FORMAT(4ES20.12)
          READ(LUN, 1007, END=9, ERR=9) (Y_CD(I), I = 1,NVAL)
          N = NVAL

          CXFLAG = 1
          YPTR = mxCreateNumericMatrix(NVAL, N1, CID_D, CXFLAG)
          CALL mxCopyComplex16ToPtr(Y_CD,mxGetComplexDoubles(YPTR),NVAL)
          ID_PTR(3) = YPTR

          DEALLOCATE(Y_CD)


C       CASE 8, 58
        ELSEIF (CASENUM == 8) THEN ! COMPLEX, DOUBLE, UNEVEN SPACING

          ALLOCATE(Y_CD(NVAL))

1008      FORMAT(ES13.5,2ES20.12)
          DO WHILE (N < NVAL)
            READ(LUN, 1008, END=208, ERR=9) (XDATA(N+1), Y_CD(N+1))
            N = N + 1
          END DO

208       CXFLAG = 1
          YPTR = mxCreateNumericMatrix(NVAL, N1, CID_D, CXFLAG)
          CALL mxCopyComplex16ToPtr(Y_CD,mxGetComplexDoubles(YPTR),NVAL)
          ID_PTR(3) = YPTR

          DEALLOCATE(Y_CD)

        END IF ! END OF CASE SWITCH

        END IF ! END OF BINARY/ASCII SWITCH


C       DATA
11      fieldnames(1) = 'datasetType'
        fieldnames(2) = 'x'
        fieldnames(3) = 'data'

C       RECORD 1-5
        fieldnames(4) = 'ID1'
        fieldnames(5) = 'ID2'
        fieldnames(6) = 'date'
        fieldnames(7) = 'ID4'
        fieldnames(8) = 'ID5'

C       RECORD 6
        fieldnames(9) = 'functionType'
        fieldnames(10) = 'functionID'
        fieldnames(11) = 'versionNumber'
        fieldnames(12) = 'loadCaseID'
        fieldnames(13) = 'rspEntName'
        fieldnames(14) = 'rspNode'
        fieldnames(15) = 'rspDir'
        fieldnames(16) = 'refEntName'
        fieldnames(17) = 'refNode'
        fieldnames(18) = 'refDir'

C       RECORD 7
        fieldnames(19) = 'dataFormatType'
        fieldnames(20) = 'numValues'
        fieldnames(21) = 'evenSpacing'
        fieldnames(22) = 'x0'
        fieldnames(23) = 'dx'
        fieldnames(24) = 'zAxisValue'

C       RECORD 8
        fieldnames(25) = 'abscDataType'
        fieldnames(26) = 'abscLengthUnitsExponent'
        fieldnames(27) = 'abscForceUnitsExponent'
        fieldnames(28) = 'abscTempUnitsExponent'
        fieldnames(29) = 'abscAxisLabel'
        fieldnames(30) = 'abscAxisUnitLabel'

C       RECORD 9
        fieldnames(31) = 'ordNumDataType'
        fieldnames(32) = 'ordNumLengthUnitsExponent'
        fieldnames(33) = 'ordNumForceUnitsExponent'
        fieldnames(34) = 'ordNumTempUnitsExponent'
        fieldnames(35) = 'ordNumAxisLabel'
        fieldnames(36) = 'ordNumAxisUnitLabel'

C       RECORD 10
        fieldnames(37) = 'ordDenomDataType'
        fieldnames(38) = 'ordDenomLengthUnitsExponent'
        fieldnames(39) = 'ordDenomForceUnitsExponent'
        fieldnames(40) = 'ordDenomTempUnitsExponent'
        fieldnames(41) = 'ordDenomAxisLabel'
        fieldnames(42) = 'ordDenomAxisUnitLabel'

C       RECORD 11
        fieldnames(43) = 'zDataType'
        fieldnames(44) = 'zLengthUnitsExponent'
        fieldnames(45) = 'zForceUnitsExponent'
        fieldnames(46) = 'zTempUnitsExponent'
        fieldnames(47) = 'zAxisLabel'
        fieldnames(48) = 'zAxisUnitLabel'


        FIELDSTOUSE = 48


        IF (SKIP_DATA < 1 .AND. ALLOCATED(XDATA)) THEN
C         COPY XDATA TO SINGLE OUTPUT MATRIX
          CXFLAG = 0
          ID_PTR(2) = mxCreateNumericMatrix(NVAL, N1, CID_S, CXFLAG)
          CALL mxCopyReal4ToPtr(XDATA, mxGetSingles(ID_PTR(2)), NVAL)
          DEALLOCATE(XDATA)
        END IF       
        
C       RECORD 1-5
        ID_PTR(1) = mxCreateDoubleScalar(DBLE(DATASETNUM)) ! Dataset number
        ID_PTR(4) = mxCreateString(ID1) ! ID Line 1
        ID_PTR(5) = mxCreateString(ID2) ! ID Line 2
        ID_PTR(6) = mxCreateString(ID3) ! ID Line 3, date
        ID_PTR(7) = mxCreateString(ID4) ! ID Line 4
        ID_PTR(8) = mxCreateString(ID5) ! ID Line 5

C       RECORD 6
        ID_PTR(9) = mxCreateDoubleScalar(DBLE(F61)) ! Function Type
        ID_PTR(10) = mxCreateDoubleScalar(DBLE(F62)) ! Function Identification Number
        ID_PTR(11) = mxCreateDoubleScalar(DBLE(F63)) ! Version Number, or sequence number
        ID_PTR(12) = mxCreateDoubleScalar(DBLE(F64)) ! Load Case Identification Number
        ID_PTR(13) = mxCreateString(F65) ! Response Entity Name
        ID_PTR(14) = mxCreateDoubleScalar(DBLE(F66)) ! Response Node
        ID_PTR(15) = mxCreateDoubleScalar(DBLE(F67)) ! Response Direction
        ID_PTR(16) = mxCreateString(F68) ! Reference Entity Name
        ID_PTR(17) = mxCreateDoubleScalar(DBLE(F69)) ! Reference Node
        ID_PTR(18) = mxCreateDoubleScalar(DBLE(F610)) ! Reference Direction

C       RECORD 7
        ID_PTR(19) = mxCreateDoubleScalar(ORDDATATYPE) ! Ordinate Data Type
        ID_PTR(20) = mxCreateDoubleScalar(ABSCNUM) ! Number of data pairs for uneven abscissa...
        ID_PTR(21) = mxCreateDoubleScalar(ISEVEN) ! Abscissa Spacing (0 = uneven, 1 = even)
        ID_PTR(22) = mxCreateDoubleScalar(ABSC0) ! Abscissa minimum (0.0 if spacing uneven)
        ID_PTR(23) = mxCreateDoubleScalar(ABSCINCR) ! Abscissa increment (0.0 if spacing uneven)
        ID_PTR(24) = mxCreateDoubleScalar(DBLE(F76)) ! Z-axis value (0.0 if unused)

C       RECORD 8
        ID_PTR(25) = mxCreateDoubleScalar(DBLE(F81)) ! Abscissa Data Type
        ID_PTR(26) = mxCreateDoubleScalar(DBLE(F82)) ! Length units exponent
        ID_PTR(27) = mxCreateDoubleScalar(DBLE(F83)) ! Force units exponent
        ID_PTR(28) = mxCreateDoubleScalar(DBLE(F84)) ! Temperature units exponent
        ID_PTR(29) = mxCreateString(F85) ! Axis label
        ID_PTR(30) = mxCreateString(F86) ! Axis units label

C       RECORD 9
        ID_PTR(31) = mxCreateDoubleScalar(DBLE(F91)) ! Ordinate Data Type
        ID_PTR(32) = mxCreateDoubleScalar(DBLE(F92)) ! Ordinate length units exponent
        ID_PTR(33) = mxCreateDoubleScalar(DBLE(F93)) ! Ordinate force units exponent
        ID_PTR(34) = mxCreateDoubleScalar(DBLE(F94)) ! Ordinate temperature units exponent
        ID_PTR(35) = mxCreateString(F95) ! Ordinate axis label
        ID_PTR(36) = mxCreateString(F96) ! Ordinate axis units label

C       RECORD 10
        ID_PTR(37) = mxCreateDoubleScalar(DBLE(F101)) ! Ordinate Denominator Data Type
        ID_PTR(38) = mxCreateDoubleScalar(DBLE(F102)) ! Ordinate Denominator length units exponent
        ID_PTR(39) = mxCreateDoubleScalar(DBLE(F103)) ! Ordinate Denominator force units exponent
        ID_PTR(40) = mxCreateDoubleScalar(DBLE(F104)) ! Ordinate Denominator temperature units exponent
        ID_PTR(41) = mxCreateString(F105) ! Ordinate axis label
        ID_PTR(42) = mxCreateString(F106) ! Ordinate axis units label

C       RECORD 11
        ID_PTR(43) = mxCreateDoubleScalar(DBLE(F111)) ! Ordinate Denominator Data Type
        ID_PTR(44) = mxCreateDoubleScalar(DBLE(F112)) ! Ordinate Denominator length units exponent
        ID_PTR(45) = mxCreateDoubleScalar(DBLE(F113)) ! Ordinate Denominator force units exponent
        ID_PTR(46) = mxCreateDoubleScalar(DBLE(F114)) ! Ordinate Denominator temperature units exponent
        ID_PTR(47) = mxCreateString(F115) ! Ordinate axis label
        ID_PTR(48) = mxCreateString(F116) ! Ordinate axis units label


      END IF


C     COPY STRUCT TO CELL ARRAY, IF DATASET NUMBER IS VALID
      IF (ANY(DATASETNUM == DATASETS)) THEN

        SELECT CASE (USE_ALLOCATION)
          CASE (0)
            SPTR(NCELL) = mxCreateStructMatrix(N1, N1, FIELDSTOUSE,
     &        fieldnames(1:FIELDSTOUSE))
            DO K = 1,FIELDSTOUSE
              CALL mxSetField(SPTR(NCELL),N1,fieldnames(K),ID_PTR(K))
            END DO

          CASE (1)
            SPTR_1(NCELL) = mxCreateStructMatrix(N1, N1, FIELDSTOUSE,
     &        fieldnames(1:FIELDSTOUSE))
             DO K = 1,FIELDSTOUSE
              CALL mxSetField(SPTR_1(NCELL),N1,fieldnames(K),ID_PTR(K))
             END DO

          CASE (2)
            SPTR_2(NCELL) = mxCreateStructMatrix(N1, N1, FIELDSTOUSE,
     &        fieldnames(1:FIELDSTOUSE))
             DO K = 1,FIELDSTOUSE
              CALL mxSetField(SPTR_2(NCELL),N1,fieldnames(K),ID_PTR(K))
             END DO

        END SELECT

        IF (SKIP_DATA < 1) THEN
          READ(LUN, '(A80)', END=10, ERR=9) REC0        
          READ(LUN, '(A80)', END=10, ERR=9) REC0 ! READ TWICE
        END IF
        
        
      ELSE IF (DATASETNUM == -58) THEN
        REC0 = ID1
C       DO NOTHING HERE -> REPEAT LOOP      


      ELSE ! INVALID OR UNSUPPORTED DATASET NUMBER


        REC0 = "0"

C       CHECK STRICTLY FOR THE CORRECT FORMATTING OF END TERMINATOR
        DO WHILE (REC0 /= "    -1")
          READ(LUN, '(A80)', END=10, ERR=9) REC0
        END DO


      END IF



C ----------------------------------------------------------------------
      END DO ! END OF WHILE LOOP
C ----------------------------------------------------------------------


      GO TO 10 ! SKIP ERROR LABEL 9


C --- FINISHED READING -------------------------------------------------
C ----------------------------------------------------------------------



C --- RETURN ARGUMENTS AND CLOSE FILE AND VARIABLES

9     S = -9.0
      NCELL = NCELL - 1 ! DECREASE CELL COUNT BY ONE, SINCE LAST FAILED

10    PLHS(1) = mxCreateDoubleScalar(S)


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
        INQUIRE(UNIT=LUN, OPENED=OK) ! CHECK IF FILE IS OPEN
        IF (OK) THEN
          CLOSE(LUN, STATUS='KEEP') ! CLOSE IF OPEN
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

        ELSE IF (INT(S(1)) == 1) THEN
          CELL_PTR = mxCreateCellMatrix(N1, N1)
          plhs(2) = CELL_PTR ! ASSIGN POINTER TO OUTPUT ARGUMENT #2

        ELSE ! CREATE EMPTY CELL IF NOTHING READ
          K = S(1)
          SELECT CASE (K)
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
              plhs(2) = mxCreateString('Two output arguments are require
     &d.')

            CASE (-9)
              plhs(2) = mxCreateString('Error while reading file.')

            CASE (-12)
              plhs(2) = mxCreateString('Floating point arithmetic overfl
     &ow.')

            CASE DEFAULT
              plhs(2) = mxCreateString('Read error: check IOSTAT code.')

          END SELECT
        END IF

      END IF


C     IF ALLOCATION HAVE BEEN USED -> DEALLOCATE DYNAMIC STRUCT POINTERS
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


