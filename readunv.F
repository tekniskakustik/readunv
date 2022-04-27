#include "fintrf.h"


C     Gateway routine
      subroutine mexFunction(nlhs, plhs, nrhs, prhs)

      IMPLICIT NONE

C     mexFunction arguments:
      mwPointer plhs(*), prhs(*)
      INTEGER*8 :: nlhs, nrhs

      mwPointer :: mxCreateString
      mwPointer :: mxCreateDoubleScalar
      mwPointer :: mxGetDoubles
      mwPointer :: mxGetSingles
      mwPointer :: mxGetComplexDoubles
      mwPointer :: mxGetComplexSingles
      mwPointer :: mxCreateCellMatrix
      INTEGER*4 :: mxClassIDFromClassName, classid, complexflag
      mwPointer :: mxCreateNumericMatrix
      mwPointer :: mxCreateStructMatrix
      mwPointer :: mxGetN
      mwPointer :: mxGetString
      mwSize :: STRLEN
      mwPointer :: CELL_PTR, YPTR, XPTR
      mwPointer :: SPTR(2048) ! maximum number of data records
      mwSize, PARAMETER :: N1 = 1
      mwSize :: NVAL, K2, CELLCOUNT

      INTEGER*4, PARAMETER :: nfields = 50 ! maximum number of fields in use
      mwPointer :: ID_PTR(nfields)
      CHARACTER*32 :: fieldnames(nfields) ! length of longest fieldname


      CHARACTER(LEN=80) :: REC0
      CHARACTER(LEN=80) :: ID1, ID2, ID3, ID4, ID5
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
      CHARACTER(LEN=6) :: PRECSTR
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
      REAL*4 :: F74, F75,F76
      REAL*8 :: ISEVEN, ABSCNUM, ABSC0, ABSCINCR, ORDDATATYPE
      REAL*8 :: UNITSCODE, S(1)
      INTEGER*4 :: STATUS_INT, OK, DATASETNUM, CASENUM, LUN, STEP
      INTEGER*8 :: N, I, K, FIELDSTOUSE
      LOGICAL :: STATUS_BOOL
      CHARACTER(LEN=:), ALLOCATABLE :: FILEPATH
      REAL*4, DIMENSION(:), ALLOCATABLE :: Y_RS, XDATA
      REAL*8, DIMENSION(:), ALLOCATABLE :: Y_RD
      COMPLEX*8, DIMENSION(:), ALLOCATABLE :: Y_CS
      COMPLEX*16, DIMENSION(:), ALLOCATABLE :: Y_CD



      CELLCOUNT = 0
      S = 0.0
      LUN = 0

      IF (nrhs < 1) GO TO 10

      STRLEN = mxGetN(prhs(1))

      ALLOCATE(CHARACTER(LEN=STRLEN) :: FILEPATH)

      STATUS_INT = mxGetString(prhs(1), FILEPATH, STRLEN)

      INQUIRE(FILE=FILEPATH, EXIST=STATUS_BOOL)

      IF (STATUS_BOOL /= .TRUE.) THEN
        S = -2
        GO TO 10
      END IF

      LUN = 2

      OPEN (UNIT=LUN, FILE=FILEPATH, STATUS='OLD', IOSTAT=STATUS_INT,
     & POSITION='REWIND', ACTION='READ', FORM='FORMATTED')

      IF (STATUS_INT /= 0) THEN
        S = STATUS_INT
        GO TO 10
      END IF




1     READ(LUN,'(A80)',END=10,ERR=9) REC0

      IF (ADJUSTL(REC0) /= "-1") THEN
        GO TO 10
      END IF


      S = 1.0 ! -> SUCCESS = TRUE



      ! read dataset number, 58b compatible
      READ(LUN,582,END=10,ERR=9) DATASETNUM,F02,F03,F04,F05,F06,
     & F07,F08,F09,F010
582   FORMAT(I6,1A1,I6,I6,I12,I12,I6,I6,I12,I12)




C     151
      IF (DATASETNUM == 151) THEN

C       RECORD 1, ID LINE 1, model file name
        READ(LUN,'(A80)',END=10,ERR=9) ID1

C       RECORD 2, ID LINE 2, model file description
        READ(LUN,'(A80)',END=10,ERR=9) ID2

C       RECORD 3, ID LINE 3, program which created DB
        READ(LUN,'(A80)',END=10,ERR=9) ID3

C       RECORD 4
        READ(LUN,1514,END=10,ERR=9) DS151F41,DS151F42,DS151F43,
     & DS151F44,DS151F45
1514    FORMAT(A10,A10,3I10)

C       RECORD 5
        READ(LUN,1515,END=10,ERR=9) DS151F51,DS151F52
1515    FORMAT(A10,A10)

C       RECORD 6, ID LINE 6, program which created unv-file
        READ(LUN,'(A80)',END=10,ERR=9) ID4

C       RECORD 7
        READ(LUN,1517,END=10,ERR=9) DS151F71,DS151F72
1517    FORMAT(A10,A10)


C       fix text alignment
        ID1 = ADJUSTL(ID1)
        ID2 = ADJUSTL(ID2)
        ID3 = ADJUSTL(ID3)
        ID4 = ADJUSTL(ID4)
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


        CELLCOUNT = CELLCOUNT + 1
        FIELDSTOUSE = 14

        SPTR(CELLCOUNT) = mxCreateStructMatrix(N1, N1, FIELDSTOUSE,
     & fieldnames(1:FIELDSTOUSE))


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

        DO K = 1,FIELDSTOUSE
          CALL mxSetField(SPTR(CELLCOUNT),N1,fieldnames(K),ID_PTR(K))
        END DO




C     164
      ELSEIF (DATASETNUM == 164) THEN

C       RECORD 1
        READ(LUN,1641,END=10,ERR=9) DS164F11,DS164F12,DS164F13
1641    FORMAT(I10,A20,I10)

C       RECORD 2
        READ(LUN,1642,END=10,ERR=9) DS164F21,DS164F22,DS164F23,
     & DS164F24
1642    FORMAT(3D25.17)


        UNITSCODE = DBLE(DS164F11)
        IF (UNITSCODE == 1) THEN
          UNITSSTR = 'SI: Meter (newton)'
        ELSEIF (UNITSCODE == 2) THEN
          UNITSSTR = 'BG: Foot (pound f)'
        ELSEIF (UNITSCODE == 3) THEN
          UNITSSTR = 'MG: Meter (kilogram f)'
        ELSEIF (UNITSCODE == 4) THEN
          UNITSSTR = 'BA: Foot (poundal)'
        ELSEIF (UNITSCODE == 5) THEN
          UNITSSTR = 'MM: mm (milli newton)'
        ELSEIF (UNITSCODE == 6) THEN
          UNITSSTR = 'CM: cm (centi newton)'
        ELSEIF (UNITSCODE == 7) THEN
          UNITSSTR = 'IN: Inch (pound f)'
        ELSEIF (UNITSCODE == 8) THEN
          UNITSSTR = 'GM: mm (kilogram f)'
        ELSEIF (UNITSCODE == 9) THEN
          UNITSSTR = 'US: USER_DEFINED'
        ELSE
          UNITSSTR = 'Unknown units'
        END IF



        fieldnames(1) = 'datasetType'
        fieldnames(2) = 'unitsCode'
        fieldnames(3) = 'unitsString'
        fieldnames(4) = 'unitsDescription'
        fieldnames(5) = 'temperatureMode'
        fieldnames(6) = 'length'
        fieldnames(7) = 'force'
        fieldnames(8) = 'temperature'
        fieldnames(9) = 'temperatureOffset'


        CELLCOUNT = CELLCOUNT + 1
        FIELDSTOUSE = 9

        SPTR(CELLCOUNT) = mxCreateStructMatrix(N1, N1, FIELDSTOUSE,
     & fieldnames(1:FIELDSTOUSE))


        ID_PTR(1) = mxCreateDoubleScalar(DBLE(DATASETNUM)) ! dataset number
        ID_PTR(2) = mxCreateDoubleScalar(UNITSCODE) ! units code
        ID_PTR(3) = mxCreateString(UNITSSTR) ! derived string from units code
        ID_PTR(4) = mxCreateString(DS164F12) ! units description
        ID_PTR(5) = mxCreateDoubleScalar(DBLE(DS164F13)) ! temperature mode: 1 = absolute, 2 = relative
        ID_PTR(6) = mxCreateDoubleScalar(DS164F21) ! length
        ID_PTR(7) = mxCreateDoubleScalar(DS164F22) ! force
        ID_PTR(8) = mxCreateDoubleScalar(DS164F23) ! temperature
        ID_PTR(9) = mxCreateDoubleScalar(DS164F24) ! temperature offset

        DO K = 1,FIELDSTOUSE
          CALL mxSetField(SPTR(CELLCOUNT),N1,fieldnames(K),ID_PTR(K))
        END DO



C     58
      ELSEIF (DATASETNUM == 58) THEN

C       READ HEADER

C       RECORD 1, ID LINE 1
        READ(LUN,'(A80)',END=10,ERR=9) ID1

C       RECORD 2, ID LINE 2
        READ(LUN,'(A80)',END=10,ERR=9) ID2

C       RECORD 3, ID LINE 3
        READ(LUN,'(A80)',END=10,ERR=9) ID3

C       RECORD 4, ID LINE 4
        READ(LUN,'(A80)',END=10,ERR=9) ID4

C       RECORD 5, ID LINE 5
        READ(LUN,'(A80)',END=10,ERR=9) ID5

C       RECORD 6
        READ(LUN,5806,END=10,ERR=9) F61,F62,F63,F64,F65,F66,F67,F68,F69
     & ,F610
5806    FORMAT(2(I5,I10),2(1X,A10,I10,I4))

C       RECORD 7
        READ(LUN,5807,END=10,ERR=9) F71,F72,F73,F74,F75,F76
5807    FORMAT(3I10,3E13.5)

        ORDDATATYPE = DBLE(F71)
        ISEVEN = DBLE(F73)
        ABSCNUM = DBLE(F72)
        NVAL = INT8(ABSCNUM)
        ABSC0 = DBLE(F74)
        ABSCINCR = DBLE(F75)

C       RECORD 8
        READ(LUN,5808,END=10,ERR=9) F81,F82,F83,F84,F85,F86
5808    FORMAT(I10,3I5,2(1X,A20))

C       RECORD 9
        READ(LUN,5809,END=10,ERR=9) F91,F92,F93,F94,F95,F96
5809    FORMAT(I10,3I5,2(1X,A20))

C       RECORD 10
        READ(LUN,5810,END=10,ERR=9) F101,F102,F103,F104,F105,F106
5810    FORMAT(I10,3I5,2(1X,A20))

C       RECORD 11
        READ(LUN,5811,END=10,ERR=9) F111,F112,F113,F114,F115,F116
5811    FORMAT(I10,3I5,2(1X,A20))

        CASENUM = 1

        ALLOCATE(XDATA(NVAL))
        IF (ISEVEN > 0.5) THEN
          DO K = 1,NVAL
            XDATA(K) = (K-1)*ABSCINCR + ABSC0
          END DO
        ELSE
          CASENUM = CASENUM + 1 ! = UNEVEN
        END IF

        IF (ORDDATATYPE > 4.5) THEN ! = COMPLEX
          CASENUM = CASENUM + 2
        END IF

        IF (ORDDATATYPE == 4 .OR. ORDDATATYPE == 6) THEN ! = DOUBLE
          PRECSTR = 'DOUBLE'
          CASENUM = CASENUM + 4
        ELSE
          PRECSTR = 'SINGLE'
        END IF



C       FIX TEXT ALIGNMENT
        ID1 = ADJUSTL(ID1)
        ID2 = ADJUSTL(ID2)
        ID3 = ADJUSTL(ID3)
        ID4 = ADJUSTL(ID4)
        ID5 = ADJUSTL(ID5)
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
            READ(LUN,'(A80)',END=10,ERR=9) ID1
          END DO
          GO TO 1

        ELSE

C       RECORD 12 - ASCII
C       READ DATA ACCORDING TO THE EIGHT CASES
        N = 0 ! INITIALISE NUMBER OF READ POINTS

C       CASE 1, 58
        IF (CASENUM == 1) THEN ! REAL, SINGLE, EVEN SPACING

          ALLOCATE(Y_RS(NVAL))

1001      FORMAT(6E13.5)
          READ(LUN,1001,ERR=9)(Y_RS(I),I=1,NVAL)
          N = NVAL
          
          classid = mxClassIDFromClassName('single')
          complexflag = 0
          YPTR = mxCreateNumericMatrix(NVAL, N1, classid, complexflag)
          CALL mxCopyReal4ToPtr(Y_RS, mxGetSingles(YPTR), NVAL)
          ID_PTR(3) = YPTR

          DEALLOCATE(Y_RS)


C       CASE 2, 58
        ELSEIF (CASENUM == 2) THEN ! REAL, SINGLE, UNEVEN SPACING

          ALLOCATE(Y_RS(NVAL))

1002      FORMAT(6E13.5)
          DO WHILE (N < NVAL)
            STEP = MIN(3, NVAL-N)
            READ(LUN,1002,END=202,ERR=9)(XDATA(I+N),Y_RS(I+N),I=1,STEP)
            N = N + STEP
          END DO

202       classid = mxClassIDFromClassName('single')
          complexflag = 0
          YPTR = mxCreateNumericMatrix(NVAL, N1, classid, complexflag)
          CALL mxCopyReal4ToPtr(Y_RS, mxGetSingles(YPTR), NVAL)
          ID_PTR(3) = YPTR

          DEALLOCATE(Y_RS)


C       CASE 3, 58
        ELSEIF (CASENUM == 3) THEN ! complex, single, even spacing

          ALLOCATE(Y_CS(NVAL))

1003      FORMAT(6E13.5)
          READ(LUN,1003,ERR=9)(Y_CS(I),I=1,NVAL)
          N = NVAL

          classid = mxClassIDFromClassName('single')
          complexflag = 1
          YPTR = mxCreateNumericMatrix(NVAL, N1, classid, complexflag)
          CALL mxCopyComplex8ToPtr(Y_CS, mxGetComplexSingles(YPTR),NVAL)
          ID_PTR(3) = YPTR

          DEALLOCATE(Y_CS)


C       CASE 4, 58
        ELSEIF (CASENUM == 4) THEN ! COMPLEX, SINGLE, UNEVEN SPACING

          ALLOCATE(Y_CS(NVAL))

1004      FORMAT(6E13.5)
          DO WHILE (N < NVAL)
            STEP = MIN(2, NVAL-N)
            READ(LUN,1004,END=204,ERR=9)(XDATA(I+N),Y_CS(I+N),I=1,STEP)
            N = N + STEP
          END DO

204       classid = mxClassIDFromClassName('single')
          complexflag = 1
          YPTR = mxCreateNumericMatrix(NVAL, N1, classid, complexflag)
          CALL mxCopyComplex8ToPtr(Y_CS, mxGetComplexSingles(YPTR),NVAL)
          ID_PTR(3) = YPTR

          DEALLOCATE(Y_CS)


C       CASE 5, 58
        ELSEIF (CASENUM == 5) THEN ! REAL, DOUBLE, EVEN SPACING

          ALLOCATE(Y_RD(NVAL))

1005      FORMAT(4E20.12)
          READ(LUN,1005,ERR=9)(Y_RD(I),I=1,NVAL)

          classid = mxClassIDFromClassName('double')
          complexflag = 0
          YPTR = mxCreateNumericMatrix(NVAL, N1, classid, complexflag)
          CALL mxCopyReal8ToPtr(Y_RD, mxGetDoubles(YPTR), NVAL)
          ID_PTR(3) = YPTR

          DEALLOCATE(Y_RD)


C       CASE 6, 58
        ELSEIF (CASENUM == 6) THEN ! REAL, DOUBLE, UNEVEN SPACING

          ALLOCATE(Y_RD(NVAL))

1006      FORMAT(2(E13.5,E20.12))
          DO WHILE (N < NVAL)
            STEP = MIN(2, NVAL-N)
            READ(LUN,1006,END=206,ERR=9)(XDATA(I+N),Y_RD(I+N),I=1,STEP)
            N = N + STEP
          END DO

206       classid = mxClassIDFromClassName('double')
          complexflag = 0
          YPTR = mxCreateNumericMatrix(NVAL, N1, classid, complexflag)
          CALL mxCopyReal8ToPtr(Y_RD, mxGetDoubles(YPTR), NVAL)
          ID_PTR(3) = YPTR

          DEALLOCATE(Y_RD)


C       CASE 7, 58
        ELSEIF (CASENUM == 7) THEN ! COMPLEX, DOUBLE, EVEN SPACING

          ALLOCATE(Y_CD(NVAL))

1007      FORMAT(4E20.12)
          READ(LUN,1007,ERR=9)(Y_CD(I),I=1,NVAL)
          N = NVAL

          classid = mxClassIDFromClassName('double')
          complexflag = 1
          YPTR = mxCreateNumericMatrix(NVAL, N1, classid, complexflag)
          CALL mxCopyComplex16ToPtr(Y_CD,mxGetComplexDoubles(YPTR),NVAL)
          ID_PTR(3) = YPTR

          DEALLOCATE(Y_CD)


C       CASE 8, 58
        ELSEIF (CASENUM == 8) THEN ! COMPLEX, DOUBLE, UNEVEN SPACING

          ALLOCATE(Y_CD(NVAL))

1008      FORMAT(E13.5,2E20.12)
          DO WHILE (N < NVAL)
            READ(LUN,1008,END=208,ERR=9)(XDATA(N+1),Y_CD(N+1))
            N = N + 1
          END DO

208       classid = mxClassIDFromClassName('double')
          complexflag = 1
          YPTR = mxCreateNumericMatrix(NVAL, N1, classid, complexflag)
          CALL mxCopyComplex16ToPtr(Y_CD,mxGetComplexDoubles(YPTR),NVAL)
          ID_PTR(3) = YPTR

          DEALLOCATE(Y_CD)

        END IF ! end of case switch

        END IF ! end of binary/ascii switch



        fieldnames(1) = 'datasetType'
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

        fieldnames(49) = 'dataCase'
        fieldnames(50) = 'precision'


        CELLCOUNT = CELLCOUNT + 1
        FIELDSTOUSE = 50

        
        SPTR(CELLCOUNT) = mxCreateStructMatrix(N1, N1, FIELDSTOUSE,
     & fieldnames(1:FIELDSTOUSE))


        classid = mxClassIDFromClassName('single')
        complexflag = 0
        XPTR = mxCreateNumericMatrix(NVAL, N1, classid, complexflag)
        CALL mxCopyReal4ToPtr(XDATA, mxGetSingles(XPTR), NVAL)
        ID_PTR(2) = XPTR
        DEALLOCATE(XDATA)

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

        ID_PTR(49) = mxCreateDoubleScalar(DBLE(CASENUM))
        ID_PTR(50) = mxCreateString(PRECSTR) ! data precision (string)

        DO K = 1,FIELDSTOUSE
          CALL mxSetField(SPTR(CELLCOUNT),N1,fieldnames(K),ID_PTR(K))
        END DO



      ELSE

        ID1 = "0"
        DO WHILE (ID1 /= "    -1")
          READ(LUN,'(A80)',END=10,ERR=9) ID1
        END DO
        GO TO 1

      END IF



C     read end string, e.g. '-1'
      READ(LUN,'(A80)',END=10,ERR=9) ID1
      GO TO 1 ! if not already end of file return to reading next header




C     return arguments and close files and variables

9     S = -1.0

10    PLHS(1) = mxCreateDoubleScalar(S)


      IF (ALLOCATED(FILEPATH)) DEALLOCATE(FILEPATH)
      IF (ALLOCATED(XDATA)) DEALLOCATE(XDATA)
      IF (ALLOCATED(Y_RS)) DEALLOCATE(Y_RS)
      IF (ALLOCATED(Y_RD)) DEALLOCATE(Y_RD)
      IF (ALLOCATED(Y_CS)) DEALLOCATE(Y_CS)
      IF (ALLOCATED(Y_CD)) DEALLOCATE(Y_CD)


      IF (LUN /= 0) THEN
        INQUIRE(UNIT=LUN, OPENED=OK)
        IF (OK) THEN
          CLOSE(LUN, STATUS='KEEP')
        END IF
        LUN = 0
      END IF


C     CREATE AND ASSIGN CELL MATRIX
      IF ( nlhs > 1 ) THEN
        IF (CELLCOUNT > 0) THEN
          CELL_PTR = mxCreateCellMatrix(CELLCOUNT, N1)
          DO K2 = 1,CELLCOUNT
            CALL mxSetCell(CELL_PTR, K2, SPTR(K2))
          END DO
        ELSE
          CELL_PTR = mxCreateCellMatrix(N1, N1)
        END IF
        plhs(2) = CELL_PTR
      END IF



      RETURN
      END


