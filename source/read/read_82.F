#include "fintrf.h"
C
C Universal Dataset Number: 82
C
C Name:   Tracelines
C ----------------------------------------------------------------------
C
C              Record 1: FORMAT(3I10)
C                        Field 1 -    trace line number
C                        Field 2 -    number of nodes defining trace line
C                                     (maximum of 250)
C                        Field 3 -    color
C
C              Record 2: FORMAT(80A1)
C                        Field 1 -    Identification line
C
C              Record 3: FORMAT(8I10)
C                        Field 1 -    nodes defining trace line
C                                =    > 0 draw line to node
C                                =    0 move to node (a move to the first
C                                     node is implied)
C              Notes: 1) MODAL-PLUS node numbers must not exceed 8000.
C                     2) Identification line may not be blank.
C                     3) Systan only uses the first 60 characters of the
C                        identification text.
C                     4) MODAL-PLUS does not support trace lines longer than
C                        125 nodes.
C                     5) Supertab only uses the first 40 characters of the
C                        identification line for a name.
C                     6) Repeat Datasets for each Trace_Line
C
C ----------------------------------------------------------------------
C
C
      SUBROUTINE READ_82(LUN, ID_PTR, FNAME, FNUM, NFIELDS, S)
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

C     FUNCTIONS
      mwPointer :: mxCreateString
      mwPointer :: mxCreateDoubleScalar
      mwPointer :: mxGetInt32s
      mwPointer :: mxCreateNumericMatrix
      INTEGER*4 :: mxClassIDFromClassName

C     SUPPORT VARIABLES
      mwSize, PARAMETER    :: M = 1
      mwSize               :: N
      INTEGER*4, PARAMETER :: CXFLAG = 0 ! SET COMPLEX FLAG TO REAL (ZERO)
      INTEGER*4            :: CID
      INTEGER              :: I, NVAL

C     DATASET 82 VARIABLES:
      REAL*8, PARAMETER                    :: DATASETNUM = 82.0_8
      CHARACTER(LEN=80)                    :: REC_2
      INTEGER*4                            :: REC_1(3)
      INTEGER*4, DIMENSION(:), ALLOCATABLE :: REC_3

C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
C
C
      FNUM = 6 ! NUMBER OF FIELDS IN DATASET 82
C     IF FNUM > NFIELDS => SEVERE ERROR (NOT CHECKED)
C
C
C     RECORD 1
821   FORMAT(3I10)
      READ(LUN, 821, ERR=9) REC_1
C
C     RECORD 2
822   FORMAT(A80)
      READ(LUN, 822, ERR=9) REC_2
      CALL REMOVE_NON_ASCII(REC_2, LEN_TRIM(REC_2)) ! REMOVE UN-SUPPORTED CHARACTERS
C
C     RECORD 3
823   FORMAT(8I10)
      NVAL = REC_1(2)
      IF (NVAL > 250) THEN ! NOT ALLOWED ACCORDING TO DATASET DEFINITION
        S = -11 ! == "VALUE OUTSIDE OF ALLOWED BOUNDS"
        GO TO 10
      END IF
      ALLOCATE(REC_3(NVAL))
      READ(LUN, 823, ERR=9) (REC_3(I), I=1,NVAL)
C
C
C     DEFINE OUTPUT DATA
      FNAME(1) = 'datasetType'
C
C     RECORD 1
      FNAME(2) = 'traceline'
      FNAME(3) = 'numNodes'
      FNAME(4) = 'colour'
C
C     RECORD 2
      FNAME(5) = 'ID'
C
C     RECORD 3
      FNAME(6) = 'nodes'
C ----------------------------------------------------------------------
C ----------------------------------------------------------------------
C
C
C --- COPY DATA TO OUTPUT POINTERS -------------------------------------
C ----------------------------------------------------------------------
C
C     Dataset number
      ID_PTR(1) = mxCreateDoubleScalar(DATASETNUM)
C
C
C --- RECORD 1
C     Model UID
      ID_PTR(2) = mxCreateDoubleScalar(DBLE(REC_1(1)))
C     Entity type
      ID_PTR(3) = mxCreateDoubleScalar(DBLE(REC_1(2)))
C     Entity subtype
      ID_PTR(4) = mxCreateDoubleScalar(DBLE(REC_1(3)))
C
C
C --- RECORD 2
C     Identification line
      ID_PTR(5) = mxCreateString(TRIM(REC_2))
C
C
C --- RECORD 3
C     nodes defining trace line
      CID = mxClassIDFromClassName('int32') ! MX FLAG FOR INT32
      N = NVAL
      ID_PTR(6) = mxCreateNumericMatrix(M, N, CID, CXFLAG)
      CALL mxCopyInteger4ToPtr(REC_3, mxGetInt32s(ID_PTR(6)), N)
C
C ----------------------------------------------------------------------
C ----------------------------------------------------------------------
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
C --- DEALLOCATE ARRAYS IF NEEDED
      IF (ALLOCATED(REC_3)) DEALLOCATE(REC_3)
C
C
      END SUBROUTINE
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
C
C
C