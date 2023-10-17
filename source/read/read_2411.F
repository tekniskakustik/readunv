#include "fintrf.h"
C
C
C Universal Dataset Number: 2411
C
C Name:   Nodes - Double Precision
C ----------------------------------------------------------------------------
C
C Record 1:        FORMAT(4I10)
C                  Field 1       -- node label
C                  Field 2       -- export coordinate system number
C                  Field 3       -- displacement coordinate system number
C                  Field 4       -- color
C Record 2:        FORMAT(1P3D25.16)
C                  Fields 1-3    -- node coordinates in the part coordinate
C                                   system
C
C Records 1 and 2 are repeated for each node in the model.
C
C ----------------------------------------------------------------------------
C
C
      SUBROUTINE READ_2411(LUN, ID_PTR, FNAME, FNUM, NFIELDS, S)
C     ------------------------------------------------------------------
C
      IMPLICIT NONE
C
C     IN/OUT ARGUMENTS
      INTEGER,      INTENT(OUT)               :: FNUM
      INTEGER*4,    INTENT(IN)                :: LUN, NFIELDS      
      REAL*8,       INTENT(OUT)               :: S
      CHARACTER*32, INTENT(OUT)               :: FNAME(NFIELDS)
      mwPointer,    INTENT(OUT)               :: ID_PTR(NFIELDS)
C
C     FUNCTIONS
      mwPointer                               :: mxCreateDoubleScalar
      mwPointer                               :: mxCreateNumericMatrix
      mwPointer                               :: mxCreateDoubleMatrix
      mwPointer                               :: mxGetDoubles
      mwPointer                               :: mxGetInt32s
      INTEGER*4                               :: mxClassIDFromClassName
C
C     SUPPORT VARIABLES
      INTEGER*4, PARAMETER                    :: CXFLAG = 0 ! SET COMPLEX FLAG TO REAL (ZERO)
      INTEGER,   PARAMETER                    :: STEP = 512
      INTEGER*4                               :: CID
      mwSize                                  :: M, N, NSIZE
C
C     DATASET 2411 VARIABLES:
      REAL*8,    PARAMETER                    :: DATASETNUM = 2411
      CHARACTER(LEN=40)                       :: REC_0
      INTEGER*4, DIMENSION(:, :), ALLOCATABLE :: REC_1, TEMP_1
      REAL*8,    DIMENSION(:, :), ALLOCATABLE :: REC_2, TEMP_2
C
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
C
C
      FNUM = 6 ! NUMBER OF FIELDS IN DATASET 2411
C     IF FNUM > NFIELDS => SEVERE ERROR (NOT CHECKED)
C
C
C --- INITIALISE VARIABLES
      N = 0 ! NUMBER OF NODES READ
      REC_0 = ''
C
C
C --- ALLOCATE DATA
      NSIZE = STEP
      ALLOCATE(REC_1(4, NSIZE))
      ALLOCATE(REC_2(3, NSIZE))
C
C
C --- READ NUMBER OF NODES UNTIL BLOCK TERMINATOR IS READ
      DO WHILE (REC_0 /= '    -1')
C
C
C ----- RECORD 1
24110   FORMAT(A40) ! YOU NEVER KNOW IF A LINE IS TERMINATED WITH "-1" OR IS ACTUAL DATA, FOR THIS DATASET
        READ(LUN, 24110, ERR=9) REC_0
        IF (REC_0 == '    -1') THEN
          EXIT
        ELSE
          N = N + 1
        END IF
C
C
C ----- CHECK IF REALLOCATION IS NEEDED
        IF (N > NSIZE) THEN
          ALLOCATE(TEMP_2(3, NSIZE + STEP))
          TEMP_2(:, 1:NSIZE) = REC_2
          CALL MOVE_ALLOC(TEMP_2, REC_2)
C
          ALLOCATE(TEMP_1(4, NSIZE + STEP))
          TEMP_1(:, 1:NSIZE) = REC_1
          CALL MOVE_ALLOC(TEMP_1, REC_1)
C
          NSIZE = NSIZE + STEP
        END IF
C
C
C ----- WRITE STRING "REC_0" INTO RECORD 1 VALUES
24111   FORMAT(4I10)
        READ(REC_0, 24111) REC_1(1:4, N)
C
C
C ----- RECORD 2
24112   FORMAT(3(1P3D25.16))
        READ(LUN, 24112, ERR=9) REC_2(1:3, N)
C
      END DO
C
C
C --- NO DATA READ?
      IF (N < 1) THEN
        GO TO 9
      END IF
C
C
C     DEFINE OUTPUT DATA
      FNAME(1) = 'datasetType'
C
C     RECORD 1
      FNAME(2) = 'node'
      FNAME(3) = 'exportCoordinateNumber'
      FNAME(4) = 'displacementCoordinateNumber'
      FNAME(5) = 'colour'
C
C     RECORD 2
      FNAME(6) = 'coordinate'
C
C
C     COPY DATA TO OUTPUT POINTERS
      ID_PTR(1) = mxCreateDoubleScalar(DATASETNUM)      ! Dataset number
C
C
C --- RECORD 1
      CID = mxClassIDFromClassName('int32') ! MX FLAG FOR INT32
C     node label
      ID_PTR(2) = mxCreateNumericMatrix(1, N, CID, CXFLAG)
      CALL mxCopyInteger4ToPtr(REC_1(1, 1:N), mxGetInt32s(ID_PTR(2)), N)
C     export coordinate system number
      ID_PTR(3) = mxCreateNumericMatrix(1, N, CID, CXFLAG)
      CALL mxCopyInteger4ToPtr(REC_1(2, 1:N), mxGetInt32s(ID_PTR(3)), N)
C     displacement coordinate system number
      ID_PTR(4) = mxCreateNumericMatrix(1, N, CID, CXFLAG)
      CALL mxCopyInteger4ToPtr(REC_1(3, 1:N), mxGetInt32s(ID_PTR(4)), N)
C     color
      ID_PTR(5) = mxCreateNumericMatrix(1, N, CID, CXFLAG)
      CALL mxCopyInteger4ToPtr(REC_1(4, 1:N), mxGetInt32s(ID_PTR(5)), N)
C
C
C --- RECORD 2
      M = 3
      ID_PTR(6) = mxCreateDoubleMatrix(M, N, CXFLAG) ! node coordinates in the part coordinate system
      CALL mxCopyReal8ToPtr(REC_2(:, 1:N), mxGetDoubles(ID_PTR(6)), M*N)
C
C
      S = 1
C
      GO TO 10
C
9     S = -9
C
10    CONTINUE
C
C
      IF (ALLOCATED(TEMP_2)) DEALLOCATE(TEMP_2)
      IF (ALLOCATED(REC_2)) DEALLOCATE(REC_2)
      IF (ALLOCATED(TEMP_1)) DEALLOCATE(TEMP_1)
      IF (ALLOCATED(REC_1)) DEALLOCATE(REC_1)
C
C
      END SUBROUTINE
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
C
C
C