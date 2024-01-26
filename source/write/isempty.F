#include "fintrf.h"
C
C
C ----------------------------------------------------------------------  
      LOGICAL FUNCTION ISEMPTY(PTR)
C ----------------------------------------------------------------------
C
      IMPLICIT NONE
C
C ----------------------------------------------------------------------  
      mwPointer, INTENT(IN) :: PTR
      INTEGER*4             :: mxIsEmpty
      LOGICAL*1             :: NOK
C ----------------------------------------------------------------------      
C
C
      IF (PTR == 0) THEN
        NOK = .TRUE.
      ELSEIF (mxIsEmpty(PTR)) THEN
        NOK = .TRUE.
      ELSE
        NOK = .FALSE.
      END IF
      ISEMPTY = NOK
C
C
      END FUNCTION ISEMPTY
C ----------------------------------------------------------------------  
C
C
