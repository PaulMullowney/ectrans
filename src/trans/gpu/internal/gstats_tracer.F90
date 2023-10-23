MODULE GSTATS_TRACER_MOD
  CONTAINS
  SUBROUTINE GSTATS_TRACER(KNUM,KSWITCH)
  USE EC_PARKIND  ,ONLY : JPRD, JPIM ,JPIB

  USE YOMGSTATS
  USE hip_profiling   ,ONLY : roctxRangePushA,&
                              roctxRangePop,&
                              roctxMarkA
  USE iso_c_binding   ,ONLY : c_null_char
  IMPLICIT NONE
  INTEGER :: ret
  INTEGER(KIND=JPIM),INTENT(IN) :: KNUM
  INTEGER(KIND=JPIM),INTENT(IN) :: KSWITCH

  CALL GSTATS(KNUM,KSWITCH)
  !WRITE(*,*) KNUM, KSWITCH, CCDESC(KNUM)
#ifdef HIPGPU
  IF (KSWITCH > 0) THEN
     CALL roctxRangePop()
     CALL roctxMarkA(CCDESC(KNUM)//c_null_char)
  ELSE
     ret = roctxRangePushA(CCDESC(KNUM)//c_null_char)
  ENDIF
#elif defined(CUDAGPU)
#endif

  END SUBROUTINE GSTATS_TRACER
END MODULE GSTATS_TRACER_MOD
