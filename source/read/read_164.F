#include "fintrf.h"
C
C
C
      SUBROUTINE READ_164(LUN, ID_PTR, FNAME, FNUM, NFIELDS, S)
C     ------------------------------------------------------------------
C
      IMPLICIT NONE
C
C     IN/OUT ARGUMENTS
      INTEGER*4, INTENT(IN)     :: LUN, NFIELDS
      INTEGER, INTENT(OUT)      :: FNUM
      REAL*8, INTENT(OUT)       :: S
      CHARACTER*32, INTENT(OUT) :: FNAME(NFIELDS)
      mwPointer, INTENT(OUT)    :: ID_PTR(NFIELDS)
C
C     FUNCTIONS
      mwPointer :: mxCreateString
      mwPointer :: mxCreateDoubleScalar
C
C     DATASET 164 VARIABLES:
      REAL*8, PARAMETER :: DATASETNUM = 164.0_8
      CHARACTER(LEN=22) :: UNITSSTR
      CHARACTER(LEN=20) :: REC_1_2
      INTEGER*4         :: REC_1_1, REC_1_3
      REAL*8            :: REC_2_1, REC_2_2, REC_2_3, REC_2_4
C
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
C
C
      FNUM = 9 ! NUMBER OF FIELDS IN DATASET 164
C     IF FNUM > NFIELDS => SEVERE ERROR (NOT CHECKED)
C
C
C     RECORD 1
1641  FORMAT(I10, A20, I10)
      READ(LUN, 1641, ERR=9) REC_1_1, REC_1_2, REC_1_3
C
C
C     RECORD 2
1642  FORMAT(3D25.17)
      READ(LUN, 1642, ERR=9) REC_2_1, REC_2_2, REC_2_3, REC_2_4

      SELECT CASE (REC_1_1) ! units code
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
C
C
C     FIELD NAMES
      FNAME(1) = 'datasetType'
      FNAME(2) = 'unitsCode'
      FNAME(3) = 'unitsString'
      FNAME(4) = 'unitsDescription'
      FNAME(5) = 'temperatureMode'
      FNAME(6) = 'length'
      FNAME(7) = 'force'
      FNAME(8) = 'temperature'
      FNAME(9) = 'temperatureOffset'
C
C
C --- CREATE OUTPUTS ---------------------------------------------------
C
C     DATASET NUMBER
      ID_PTR(1) = mxCreateDoubleScalar(DATASETNUM)    ! dataset number
C
C     RECORD 1
      ID_PTR(2) = mxCreateDoubleScalar(DBLE(REC_1_1)) ! units code
      ID_PTR(3) = mxCreateString(UNITSSTR)            ! derived string from units code
      ID_PTR(4) = mxCreateString(ADJUSTL(REC_1_2))    ! units description
      ID_PTR(5) = mxCreateDoubleScalar(DBLE(REC_1_3)) ! temperature mode: 1 = absolute, 2 = relative
C
C     RECORD 2
      ID_PTR(6) = mxCreateDoubleScalar(REC_2_1)       ! length
      ID_PTR(7) = mxCreateDoubleScalar(REC_2_2)       ! force
      ID_PTR(8) = mxCreateDoubleScalar(REC_2_3)       ! temperature
      ID_PTR(9) = mxCreateDoubleScalar(REC_2_4)       ! temperature offset
C
C ----------------------------------------------------------------------
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
      END SUBROUTINE
C
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
C
C
C