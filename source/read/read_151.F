#include "fintrf.h"
C
C
C
      SUBROUTINE READ_151(LUN, ID_PTR, FNAME, FNUM, NFIELDS, S)
C     ------------------------------------------------------------------
C
      IMPLICIT NONE
C
C     IN/OUT ARGUMENTS
      INTEGER*4,    INTENT(IN)  :: LUN, NFIELDS
      INTEGER,      INTENT(OUT) :: FNUM
      REAL*8,       INTENT(OUT) :: S
      CHARACTER*32, INTENT(OUT) :: FNAME(NFIELDS)
      mwPointer,    INTENT(OUT) :: ID_PTR(NFIELDS)
C
C     MEX FUNCTIONS
      mwPointer         :: mxCreateString
      mwPointer         :: mxCreateDoubleScalar
C
C     DATASET 151 VARIABLES:
      REAL*8, PARAMETER :: DATASETNUM = 151.0_8
      CHARACTER(LEN=80) :: ID1, ID2, ID3, ID6
      CHARACTER(LEN=10) :: REC_4_1, REC_4_2
      CHARACTER(LEN=10) :: REC_5_1, REC_5_2
      CHARACTER(LEN=10) :: REC_7_1, REC_7_2
      INTEGER*4         :: REC_4_3, REC_4_4, REC_4_5
C
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
C
C
C     NUMBER OF FIELDS IN DATASET 151, IF FNUM > NFIELDS => SEVERE ERROR (NOT CHECKED)
      FNUM = 14
C
C
C --- READ DATA --------------------------------------------------------
C
C     COMMON FORMAT
80    FORMAT(A80)
C
C     RECORD 1, ID LINE 1, model file name
      READ(LUN, 80, ERR=9) ID1
C
C     RECORD 2, ID LINE 2, model file description
      READ(LUN, 80, ERR=9) ID2
C
C     RECORD 3, ID LINE 3, program which created DB
      READ(LUN, 80, ERR=9) ID3
C
C     RECORD 4
1514  FORMAT(A10, A10, 3I10)
      READ(LUN, 1514, ERR=9) REC_4_1,REC_4_2,REC_4_3,REC_4_4,REC_4_5
C
C     RECORD 5
1515  FORMAT(A10, A10)
      READ(LUN, 1515, ERR=9) REC_5_1, REC_5_2
C
C     RECORD 6, ID LINE 6, program which created unv-file
      READ(LUN, 80, ERR=9) ID6
C
C     RECORD 7
1517  FORMAT(A10, A10)
      READ(LUN, 1517, ERR=9) REC_7_1, REC_7_2
C
C ----------------------------------------------------------------------
C
C
C     FIX TEXT ALIGNMENT
      ID1 = ADJUSTL(ID1)
      ID2 = ADJUSTL(ID2)
      ID3 = ADJUSTL(ID3)
      ID6 = ADJUSTL(ID6)
      CALL REMOVE_NON_ASCII(ID1, LEN_TRIM(ID1))
      CALL REMOVE_NON_ASCII(ID2, LEN_TRIM(ID2))
      CALL REMOVE_NON_ASCII(ID3, LEN_TRIM(ID3))
      CALL REMOVE_NON_ASCII(ID6, LEN_TRIM(ID6))
C
C
C     FIELD NAMES
      FNAME(1)  = 'datasetType'
      FNAME(2)  = 'modelName'
      FNAME(3)  = 'modelDescription'
      FNAME(4)  = 'databaseProgram'
      FNAME(5)  = 'databaseCreatedDate'
      FNAME(6)  = 'databaseCreatedTime'
      FNAME(7)  = 'databaseVersion1'
      FNAME(8)  = 'databaseVersion2'
      FNAME(9)  = 'fileType'
      FNAME(10) = 'databaseLastSavedDate'
      FNAME(11) = 'databaseLastSavedTime'
      FNAME(12) = 'unvProgram'
      FNAME(13) = 'fileCreatedDate'
      FNAME(14) = 'fileCreatedTime'
C
C
C --- CREATE OUTPUTS ---------------------------------------------------
C
C     DATASET NUMBER
      ID_PTR(1)  = mxCreateDoubleScalar(DATASETNUM)
C
C     RECORDS 1-3
      ID_PTR(2)  = mxCreateString(ID1)                 ! ID Line 1, model file name
      ID_PTR(3)  = mxCreateString(ID2)                 ! ID Line 2, model file description
      ID_PTR(4)  = mxCreateString(ID3)                 ! ID Line 3, program which created DB
C
C     RECORD 4
      ID_PTR(5)  = mxCreateString(ADJUSTL(REC_4_1))    ! date database created (DD-MMM-YY)
      ID_PTR(6)  = mxCreateString(ADJUSTL(REC_4_2))    ! time database created (HH:MM:SS)
      ID_PTR(7)  = mxCreateDoubleScalar(DBLE(REC_4_3)) ! Version from database
      ID_PTR(8)  = mxCreateDoubleScalar(DBLE(REC_4_4)) ! Version from database
      ID_PTR(9)  = mxCreateDoubleScalar(DBLE(REC_4_5)) ! File type
C
C     RECORD 5
      ID_PTR(10) = mxCreateString(ADJUSTL(REC_5_1))    ! date database last saved (DD-MMM-YY)
      ID_PTR(11) = mxCreateString(ADJUSTL(REC_5_2))    ! time database last saved (HH:MM:SS)
C
C     RECORD 6
      ID_PTR(12) = mxCreateString(ID6)                 ! ID Line 6, program which created universal file
C
C     RECORD 7
      ID_PTR(13) = mxCreateString(ADJUSTL(REC_7_1))    ! date file written (DD-MMM-YY)
      ID_PTR(14) = mxCreateString(ADJUSTL(REC_7_2))    ! time file written (HH:MM:SS)
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