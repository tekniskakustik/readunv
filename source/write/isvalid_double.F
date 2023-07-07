#include "fintrf.h"


      LOGICAL FUNCTION ISVALID_DOUBLE(PTR)
C     ------------------------------------------------------------------

      IMPLICIT NONE

      mwPointer, INTENT(IN) :: PTR
      INTEGER*4 :: mxIsEmpty, mxIsDouble
      LOGICAL*1 :: ISVALID

      IF (PTR == 0) THEN
        ISVALID = .FALSE.
      ELSEIF (mxIsEmpty(PTR) .OR. NOT(mxIsDouble(PTR))) THEN
        ISVALID = .FALSE.
      ELSE
        ISVALID = .TRUE.
      END IF

      ISVALID_DOUBLE = ISVALID

      END FUNCTION ISVALID_DOUBLE      
C     ------------------------------------------------------------------


