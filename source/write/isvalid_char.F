#include "fintrf.h"


      LOGICAL FUNCTION ISVALID_CHAR(PTR)

      IMPLICIT NONE

      mwPointer, INTENT(IN) :: PTR
      INTEGER*4 :: mxIsEmpty, mxIsChar
      LOGICAL*1 :: ISVALID

      IF (PTR == 0) THEN
        ISVALID = .FALSE.
      ELSEIF (mxIsEmpty(PTR) .OR. NOT(mxIsChar(PTR))) THEN
        ISVALID = .FALSE.
      ELSE
        ISVALID = .TRUE.
      END IF

      ISVALID_CHAR = ISVALID

      END FUNCTION ISVALID_CHAR


