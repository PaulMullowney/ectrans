MODULE roctx_profiling

  INTERFACE
     SUBROUTINE roctxRangePush(message) BIND(c, name="roctxRangePushA")
       USE ISO_C_BINDING,   ONLY: C_CHAR
       IMPLICIT NONE
       CHARACTER(C_CHAR) :: message(*)
     END SUBROUTINE roctxRangePush

     SUBROUTINE roctxRangePop() BIND(c, name="roctxRangePop")
       IMPLICIT NONE
     END SUBROUTINE roctxRangePop

  END INTERFACE

END MODULE roctx_profiling
