


      SUBROUTINE REMOVE_NON_ASCII(STR, L)
        CHARACTER(LEN=L), INTENT(INOUT) :: STR
        INTEGER :: K

        DO K = 1,L
          STR(K:K) = ACHAR(MIN(IACHAR(STR(K:K)), 127))
        END DO

      END SUBROUTINE REMOVE_NON_ASCII
      
      