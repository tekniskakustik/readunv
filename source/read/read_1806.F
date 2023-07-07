#include "fintrf.h"


      SUBROUTINE READ_1806(LUN, ID_PTR, FNAME, FNUM, NFIELDS, S)
C     ------------------------------------------------------------------

      IMPLICIT NONE

C     IN/OUT ARGUMENTS
      INTEGER*4, INTENT(IN)     :: LUN, NFIELDS
      INTEGER*8, INTENT(OUT)    :: FNUM
      REAL*8, INTENT(OUT)       :: S
      CHARACTER*32, INTENT(OUT) :: FNAME(NFIELDS)
      mwPointer, INTENT(OUT)    :: ID_PTR(NFIELDS)

C     FUNCTIONS
      mwPointer :: mxCreateString
      mwPointer :: mxCreateDoubleScalar

C     DATASET 1806 VARIABLES:
      REAL*8, PARAMETER :: DATASETNUM = 1806
      CHARACTER(LEN=10) :: REC_1_1, REC_2_1, REC_2_2, REC_3_1
      CHARACTER(LEN=10) :: REC_3_2, REC_3_3, REC_5_7
      CHARACTER(LEN=40) :: REC_4_1
      INTEGER*4         :: REC_5_1, REC_5_2, REC_5_3
      INTEGER*2         :: REC_5_4, REC_5_5, REC_5_6
      REAL*8            :: REC_6_1
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------


      FNUM = 16 ! NUMBER OF FIELDS IN DATASET 1806
C     IF FNUM > NFIELDS => SEVERE ERROR (NOT CHECKED)           

C     RECORD 1
18061 FORMAT(10A2)
      READ(LUN, 18061,ERR=9) REC_1_1

C     RECORD 2
18062 FORMAT(10A2,2X,10A2)
      READ(LUN, 18062,ERR=9) REC_2_1, REC_2_2

C     RECORD 3
18063 FORMAT(10A2,2X,10A2,2X,10A2)
      READ(LUN, 18063, END=10,ERR=9) REC_3_1, REC_3_2, REC_3_3

C     RECORD 4
18064 FORMAT(40A2)
      READ(LUN, 18064, END=10,ERR=9) REC_4_1

C     RECORD 5
18065 FORMAT(3I12,3I6,10A2)
      READ(LUN, 18065,ERR=9) REC_5_1, REC_5_2, REC_5_3, REC_5_4
     & , REC_5_5, REC_5_6, REC_5_7

C     RECORD 6
18066 FORMAT(1P1E15.7)
      READ(LUN, 18066, END=10,ERR=9) REC_6_1


C     DEFINE OUTPUT DATA

      FNAME(1)  = 'datasetType'

C     RECORD 1
      FNAME(2)  = 'serial'

C     RECORD 2
      FNAME(3)  = 'manufacturer'
      FNAME(4)  = 'model'

C     RECORD 3
      FNAME(5)  = 'calibrationBy'
      FNAME(6)  = 'calibrationDate'
      FNAME(7)  = 'calibrationDue'

C     RECORD 4
      FNAME(8)  = 'description'

C     RECORD 5
      FNAME(9)  = 'operatingMode'
      FNAME(10) = 'dataType'
      FNAME(11) = 'typeQualifier'
      FNAME(12) = 'length'
      FNAME(13) = 'force'
      FNAME(14) = 'temperature'
      FNAME(15) = 'unitsLabel'

C     RECORD 6
      FNAME(16) = 'sensitivity'


C     COPY DATA TO OUTPUT POINTERS
      ID_PTR(1) = mxCreateDoubleScalar(DATASETNUM)     ! Dataset number

C     RECORD 1
      ID_PTR(2)  = mxCreateString(REC_1_1)             ! Model UID

C     RECORD 2
      ID_PTR(3) = mxCreateString(REC_2_1)              ! Manufacturer
      ID_PTR(4) = mxCreateString(REC_2_2)              ! Model

C     RECORD 3
      ID_PTR(5) = mxCreateString(REC_3_1)              ! Calibration by
      ID_PTR(6) = mxCreateString(REC_3_2)              ! Calibration date
      ID_PTR(7) = mxCreateString(REC_3_3)              ! Calibration due date

C     RECORD 4
      ID_PTR(8) = mxCreateString(REC_4_1)              ! Transducer description

C     RECORD 5
      ID_PTR(9)  = mxCreateDoubleScalar(DBLE(REC_5_1)) ! Operating mode
      ID_PTR(10) = mxCreateDoubleScalar(DBLE(REC_5_2)) ! Data type
      ID_PTR(11) = mxCreateDoubleScalar(DBLE(REC_5_3)) ! Type qualifier
      ID_PTR(12) = mxCreateDoubleScalar(DBLE(REC_5_4)) ! Length units exponents
      ID_PTR(13) = mxCreateDoubleScalar(DBLE(REC_5_4)) ! Force units exponents
      ID_PTR(14) = mxCreateDoubleScalar(DBLE(REC_5_4)) ! Temperature exponents
      ID_PTR(15) = mxCreateString(REC_5_7)             ! Units label

C     RECORD 6
      ID_PTR(16) = mxCreateDoubleScalar(REC_6_1)       ! Sensitivity (mv/EU)

      S = 1

      GO TO 10

9     S = -9

10    CONTINUE


      END SUBROUTINE
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------



