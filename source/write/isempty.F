#include "fintrf.h"


      LOGICAL FUNCTION ISEMPTY(PTR)
C     ------------------------------------------------------------------

      IMPLICIT NONE

      mwPointer, INTENT(IN) :: PTR
      INTEGER*4 :: mxIsEmpty
      LOGICAL*1 :: NOK

      IF (PTR == 0) THEN
        NOK = .TRUE.
      ELSEIF (mxIsEmpty(PTR)) THEN
        NOK = .TRUE.
       ELSE
        NOK = .FALSE.
      END IF

      ISEMPTY = NOK


      END FUNCTION ISEMPTY
C     ------------------------------------------------------------------


