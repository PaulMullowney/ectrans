SUBROUTINE INV_TRANS(PSPVOR,PSPDIV,PSPSCALAR,FSPGL_PROC,&
& LDSCDERS,LDVORGP,LDDIVGP,LDUVDER,KPROMA,KVSETUV,KVSETSC,KRESOL,&
& PGP)

#include "tsmbkind.h"

IMPLICIT NONE

! Declaration of arguments

REAL_B    ,OPTIONAL, INTENT(IN) :: PSPVOR(:,:)
REAL_B    ,OPTIONAL, INTENT(IN) :: PSPDIV(:,:)
REAL_B    ,OPTIONAL, INTENT(IN) :: PSPSCALAR(:,:)
LOGICAL   ,OPTIONAL, INTENT(IN) :: LDSCDERS
LOGICAL   ,OPTIONAL, INTENT(IN) :: LDVORGP
LOGICAL   ,OPTIONAL, INTENT(IN) :: LDDIVGP
LOGICAL   ,OPTIONAL, INTENT(IN) :: LDUVDER
INTEGER_M ,OPTIONAL, INTENT(IN) :: KPROMA
INTEGER_M ,OPTIONAL, INTENT(IN) :: KVSETUV(:)
INTEGER_M ,OPTIONAL, INTENT(IN) :: KVSETSC(:)
INTEGER_M ,OPTIONAL, INTENT(IN) :: KRESOL
EXTERNAL  FSPGL_PROC
OPTIONAL  FSPGL_PROC

REAL_B    ,INTENT(OUT) :: PGP(:,:,:)
END SUBROUTINE INV_TRANS