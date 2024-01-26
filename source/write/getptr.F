#include "fintrf.h"
C
C
C ----------------------------------------------------------------------  
C       GET PROPERTY/FIELD POINTER FROM OBJECT OR STRUCT
        FUNCTION GETPTR(OBJ, ISSTRUCT, FIELD) RESULT(PTR)
C ---------------------------------------------------------------------- 
C
        IMPLICIT NONE
C
        mwPointer                     :: mxGetField
        mwPointer                     :: mxGetProperty
        mwPointer                     :: PTR
        mwPointer, INTENT(IN)         :: OBJ
        LOGICAL*1, INTENT(IN)         :: ISSTRUCT
        CHARACTER (LEN=*), INTENT(IN) :: FIELD
C       ----------------------------------------------------------------
C
        IF (ISSTRUCT) THEN
          PTR = mxGetField(OBJ, 1, FIELD)
        ELSE
          PTR = mxGetProperty(OBJ, 1, FIELD)
        END IF
C
C
        END FUNCTION GETPTR
C ---------------------------------------------------------------------- 
C
C
C