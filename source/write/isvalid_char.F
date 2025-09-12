#include "fintrf.h"
C
C
C ----------------------------------------------------------------------
      LOGICAL FUNCTION ISVALID_CHAR(PTR)
C ----------------------------------------------------------------------
C
      IMPLICIT NONE
C
C ----------------------------------------------------------------------
      mwPointer, INTENT(IN) :: PTR
      INTEGER*4 :: mxIsEmpty, mxIsChar
      LOGICAL*1 :: ISVALID
C ----------------------------------------------------------------------
C
C
      IF (PTR == 0) THEN
        ISVALID = .FALSE.
      ELSEIF (mxIsEmpty(PTR) .OR. NOT(mxIsChar(PTR))) THEN
        ISVALID = .FALSE.
      ELSE
        ISVALID = .TRUE.
      END IF
      ISVALID_CHAR = ISVALID
C
C
      END FUNCTION ISVALID_CHAR
C ----------------------------------------------------------------------
C
C
C