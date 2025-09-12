#include "fintrf.h"


      SUBROUTINE READ_2400(LUN, ID_PTR, FNAME, FNUM, NFIELDS, S)
C     ------------------------------------------------------------------

      IMPLICIT NONE

C     IN/OUT ARGUMENTS
      INTEGER*4, INTENT(IN)     :: LUN, NFIELDS
      INTEGER, INTENT(OUT)      :: FNUM
      REAL*8, INTENT(OUT)       :: S
      CHARACTER*32, INTENT(OUT) :: FNAME(NFIELDS)
      mwPointer, INTENT(OUT)    :: ID_PTR(NFIELDS)

C     FUNCTIONS
      mwPointer :: mxCreateString
      mwPointer :: mxCreateDoubleScalar
      mwPointer :: mxGetInt8s
      mwPointer :: mxCreateNumericMatrix
      INTEGER*4 :: mxClassIDFromClassName

C     SUPPORT VARIABLES
      mwSize, PARAMETER    :: M = 32
      mwSize, PARAMETER    :: N = 1
      INTEGER*4, PARAMETER :: CXFLAG = 0 ! SET COMPLEX FLAG TO REAL (ZERO)
      INTEGER*4            :: CID
      INTEGER              :: I

C     DATASET 2400 VARIABLES:
      REAL*8, PARAMETER :: DATASETNUM = 2400.0_8
      CHARACTER(LEN=80) :: REC_2_1, REC_3_1
      INTEGER*4         :: REC_1_1, REC_1_4, REC_5_1, REC_5_2
      INTEGER*4         :: REC_5_3, REC_5_4, REC_5_5
      INTEGER*2         :: REC_1_2, REC_1_3, REC_4_1(32)
      INTEGER*1         :: REC_6_1

C     ------------------------------------------------------------------
C     ------------------------------------------------------------------


      FNUM = 14 ! NUMBER OF FIELDS IN DATASET 2400
C     IF FNUM > NFIELDS => SEVERE ERROR (NOT CHECKED)


C     RECORD 1
24001 FORMAT(I12, 2I6, I12)
      READ(LUN,24001,ERR=9) REC_1_1, REC_1_2, REC_1_3, REC_1_4

C     RECORD 2
24002 FORMAT(A80)
      READ(LUN,24002,ERR=9) REC_2_1
      CALL REMOVE_NON_ASCII(REC_2_1, LEN_TRIM(REC_2_1)) ! REMOVE UN-SUPPORTED CHARACTERS

C     RECORD 3
24003 FORMAT(A80)
      READ(LUN,24003,ERR=9) REC_3_1
      CALL REMOVE_NON_ASCII(REC_3_1, LEN_TRIM(REC_3_1)) ! REMOVE UN-SUPPORTED CHARACTERS

C     RECORD 4
24004 FORMAT(32I2)
      READ(LUN,24004,ERR=9) (REC_4_1(I), I = 1,32)

C     RECORD 5
24005 FORMAT(5I12)
      READ(LUN,24005,ERR=9) REC_5_1, REC_5_2, REC_5_3, REC_5_4, REC_5_5

C     RECORD 6
24006 FORMAT(I12)
      READ(LUN,24006,ERR=9) REC_6_1


C     DEFINE OUTPUT DATA
      FNAME(1)  = 'datasetType'

C     RECORD 1
      FNAME(2)  = 'modelUID'
      FNAME(3)  = 'entityType'
      FNAME(4)  = 'entitySubtype'
      FNAME(5)  = 'versionNumber'

C     RECORD 2
      FNAME(6)  = 'entityName'

C     RECORD 3
      FNAME(7)  = 'partNumber'

C     RECORD 4
      FNAME(8)  = 'statusMask'

C     RECORD 5
      FNAME(9)  = 'date'
      FNAME(10) = 'time'
      FNAME(11) = 'itemVersionID'
      FNAME(12) = 'itemID'
      FNAME(13) = 'primaryParentUID'

C     RECORD 6
      FNAME(14) = 'optSwitch'


C     COPY DATA TO OUTPUT POINTERS
      ID_PTR(1)  = mxCreateDoubleScalar(DATASETNUM)     ! Dataset number

C     RECORD 1
      ID_PTR(2)  = mxCreateDoubleScalar(DBLE(REC_1_1)) ! Model UID
      ID_PTR(3)  = mxCreateDoubleScalar(DBLE(REC_1_2)) ! Entity type
      ID_PTR(4)  = mxCreateDoubleScalar(DBLE(REC_1_3)) ! Entity subtype
      ID_PTR(5)  = mxCreateDoubleScalar(DBLE(REC_1_4)) ! Version number

C     RECORD 2
      ID_PTR(6)  = mxCreateString(REC_2_1)              ! Entity name

C     RECORD 3
      ID_PTR(7)  = mxCreateString(REC_3_1)              ! Part number

C     RECORD 4
      CID = mxClassIDFromClassName('int8') ! MX FLAG FOR INT8
      ID_PTR(8)  = mxCreateNumericMatrix(M, N, CID, CXFLAG) ! Status mask
      CALL mxCopyInteger1ToPtr(REC_4_1, mxGetInt8s(ID_PTR(8)), M)

C     RECORD 5
      ID_PTR(9)  = mxCreateDoubleScalar(DBLE(REC_5_1)) ! Date short time format
      ID_PTR(10) = mxCreateDoubleScalar(DBLE(REC_5_2)) ! Time short time format
      ID_PTR(11) = mxCreateDoubleScalar(DBLE(REC_5_3)) ! IDM item version ID
      ID_PTR(12) = mxCreateDoubleScalar(DBLE(REC_5_4)) ! IDM item ID
      ID_PTR(13) = mxCreateDoubleScalar(DBLE(REC_5_5)) ! Primary parent UID

C     RECORD 6
      ID_PTR(14) = mxCreateDoubleScalar(DBLE(REC_6_1)) ! Optimization switches

      S = 1

      GO TO 10

9     S = -9

10    CONTINUE


      END SUBROUTINE
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------



