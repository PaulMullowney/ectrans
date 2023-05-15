INTERFACE
SUBROUTINE MINV(PAB,KDIMN,KDBA,PZSCRA,PDET1,PTOL,KDIMM,KMODE)
USE PARKIND1, ONLY : JPIM, JPRB
INTEGER(KIND=JPIM), INTENT(IN)    :: KDIMN
INTEGER(KIND=JPIM), INTENT(IN)    :: KDBA
INTEGER(KIND=JPIM), INTENT(IN)    :: KDIMM
INTEGER(KIND=JPIM), INTENT(IN)    :: KMODE
REAL(KIND=JPRB),    INTENT(IN)    :: PTOL
REAL(KIND=JPRB),    INTENT(OUT)   :: PDET1
REAL(KIND=JPRB),    INTENT(INOUT) :: PAB(KDBA,KDIMN+KDIMM)
REAL(KIND=JPRB),    INTENT(INOUT) :: PZSCRA(2*KDIMN)
END SUBROUTINE MINV
END INTERFACE