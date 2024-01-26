#include "fintrf.h"
C
C
C ----------------------------------------------------------------------  
      LOGICAL FUNCTION ISVALID_DOUBLE(PTR)
C ----------------------------------------------------------------------  
C
      IMPLICIT NONE
C
C ----------------------------------------------------------------------  
      mwPointer, INTENT(IN) :: PTR
      INTEGER*4             :: mxIsEmpty, mxIsDouble
      LOGICAL*1             :: ISVALID
C ---------------------------------------------------------------------- 
C
C
      IF (PTR == 0) THEN
        ISVALID = .FALSE.
      ELSEIF (mxIsEmpty(PTR) .OR. NOT(mxIsDouble(PTR))) THEN
        ISVALID = .FALSE.
      ELSE
        ISVALID = .TRUE.
      END IF
      ISVALID_DOUBLE = ISVALID
C
C
      END FUNCTION ISVALID_DOUBLE      
C ----------------------------------------------------------------------  
C
C
C