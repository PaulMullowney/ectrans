! (C) Copyright 2026- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

MODULE ES_MAP_MOD
  ! Unstructured device mappings of arrays that amdflang would otherwise describe by a dope
  ! vector. Mapping an assumed-shape, POINTER or ALLOCATABLE array maps its descriptor along
  ! with the data, and the descriptor is copied twice and attached again on every entry (three
  ! 48-120 byte transfers). A contiguous array is handed on to an explicit-shape dummy, which
  ! is described by its length alone, so only the data range is mapped. A non-contiguous array
  ! keeps the descriptor-based mapping, as an explicit-shape dummy would see a packed
  ! temporary rather than the storage the device code addresses.
  USE PARKIND_ECTRANS, ONLY: JPRB, JPIB

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: ES_ENTER_TO, ES_ENTER_ALLOC, ES_EXIT_RELEASE, ES_EXIT_FROM, ES_STRIDED_VIEW

  INTERFACE ES_ENTER_TO
    MODULE PROCEDURE ES_ENTER_TO_R2, ES_ENTER_TO_R3, ES_ENTER_TO_I2
  END INTERFACE
  INTERFACE ES_ENTER_ALLOC
    MODULE PROCEDURE ES_ENTER_ALLOC_R2, ES_ENTER_ALLOC_R3
  END INTERFACE
  INTERFACE ES_EXIT_RELEASE
    MODULE PROCEDURE ES_EXIT_RELEASE_R2, ES_EXIT_RELEASE_R3, ES_EXIT_RELEASE_I2
  END INTERFACE
  INTERFACE ES_EXIT_FROM
    MODULE PROCEDURE ES_EXIT_FROM_R2, ES_EXIT_FROM_R3
  END INTERFACE

CONTAINS

  SUBROUTINE ES_ENTER_TO_R2(P)
    REAL(KIND=JPRB), INTENT(IN) :: P(:,:)
    IF (IS_CONTIGUOUS(P)) THEN
      CALL ENTER_TO_R(P, SIZE(P,KIND=JPIB))
    ELSE
      !$OMP TARGET ENTER DATA MAP(TO:P)
    ENDIF
  END SUBROUTINE
  SUBROUTINE ES_ENTER_TO_R3(P)
    REAL(KIND=JPRB), INTENT(IN) :: P(:,:,:)
    IF (IS_CONTIGUOUS(P)) THEN
      CALL ENTER_TO_R(P, SIZE(P,KIND=JPIB))
    ELSE
      !$OMP TARGET ENTER DATA MAP(TO:P)
    ENDIF
  END SUBROUTINE
  SUBROUTINE ES_ENTER_TO_I2(K)
    INTEGER(KIND=JPIB), INTENT(IN) :: K(:,:)
    IF (IS_CONTIGUOUS(K)) THEN
      CALL ENTER_TO_I(K, SIZE(K,KIND=JPIB))
    ELSE
      !$OMP TARGET ENTER DATA MAP(TO:K)
    ENDIF
  END SUBROUTINE

  SUBROUTINE ES_ENTER_ALLOC_R2(P)
    REAL(KIND=JPRB), INTENT(IN) :: P(:,:)
    IF (IS_CONTIGUOUS(P)) THEN
      CALL ENTER_ALLOC_R(P, SIZE(P,KIND=JPIB))
    ELSE
      !$OMP TARGET ENTER DATA MAP(ALLOC:P)
    ENDIF
  END SUBROUTINE
  SUBROUTINE ES_ENTER_ALLOC_R3(P)
    REAL(KIND=JPRB), INTENT(IN) :: P(:,:,:)
    IF (IS_CONTIGUOUS(P)) THEN
      CALL ENTER_ALLOC_R(P, SIZE(P,KIND=JPIB))
    ELSE
      !$OMP TARGET ENTER DATA MAP(ALLOC:P)
    ENDIF
  END SUBROUTINE

  SUBROUTINE ES_EXIT_RELEASE_R2(P)
    REAL(KIND=JPRB), INTENT(IN) :: P(:,:)
    IF (IS_CONTIGUOUS(P)) THEN
      CALL EXIT_RELEASE_R(P, SIZE(P,KIND=JPIB))
    ELSE
      !$OMP TARGET EXIT DATA MAP(RELEASE:P)
    ENDIF
  END SUBROUTINE
  SUBROUTINE ES_EXIT_RELEASE_R3(P)
    REAL(KIND=JPRB), INTENT(IN) :: P(:,:,:)
    IF (IS_CONTIGUOUS(P)) THEN
      CALL EXIT_RELEASE_R(P, SIZE(P,KIND=JPIB))
    ELSE
      !$OMP TARGET EXIT DATA MAP(RELEASE:P)
    ENDIF
  END SUBROUTINE
  SUBROUTINE ES_EXIT_RELEASE_I2(K)
    INTEGER(KIND=JPIB), INTENT(IN) :: K(:,:)
    IF (IS_CONTIGUOUS(K)) THEN
      CALL EXIT_RELEASE_I(K, SIZE(K,KIND=JPIB))
    ELSE
      !$OMP TARGET EXIT DATA MAP(RELEASE:K)
    ENDIF
  END SUBROUTINE

  SUBROUTINE ES_EXIT_FROM_R2(P)
    REAL(KIND=JPRB), INTENT(INOUT) :: P(:,:)
    IF (IS_CONTIGUOUS(P)) THEN
      CALL EXIT_FROM_R(P, SIZE(P,KIND=JPIB))
    ELSE
      !$OMP TARGET EXIT DATA MAP(FROM:P)
    ENDIF
  END SUBROUTINE
  SUBROUTINE ES_EXIT_FROM_R3(P)
    REAL(KIND=JPRB), INTENT(INOUT) :: P(:,:,:)
    IF (IS_CONTIGUOUS(P)) THEN
      CALL EXIT_FROM_R(P, SIZE(P,KIND=JPIB))
    ELSE
      !$OMP TARGET EXIT DATA MAP(FROM:P)
    ENDIF
  END SUBROUTINE

  SUBROUTINE ENTER_TO_R(P, N)
    INTEGER(KIND=JPIB), INTENT(IN) :: N
    REAL(KIND=JPRB), INTENT(IN) :: P(N)
    !$OMP TARGET ENTER DATA MAP(TO:P)
  END SUBROUTINE
  SUBROUTINE ENTER_TO_I(K, N)
    INTEGER(KIND=JPIB), INTENT(IN) :: N
    INTEGER(KIND=JPIB), INTENT(IN) :: K(N)
    !$OMP TARGET ENTER DATA MAP(TO:K)
  END SUBROUTINE
  SUBROUTINE ENTER_ALLOC_R(P, N)
    INTEGER(KIND=JPIB), INTENT(IN) :: N
    REAL(KIND=JPRB), INTENT(IN) :: P(N)
    !$OMP TARGET ENTER DATA MAP(ALLOC:P)
  END SUBROUTINE
  SUBROUTINE EXIT_RELEASE_R(P, N)
    INTEGER(KIND=JPIB), INTENT(IN) :: N
    REAL(KIND=JPRB), INTENT(IN) :: P(N)
    !$OMP TARGET EXIT DATA MAP(RELEASE:P)
  END SUBROUTINE
  SUBROUTINE EXIT_RELEASE_I(K, N)
    INTEGER(KIND=JPIB), INTENT(IN) :: N
    INTEGER(KIND=JPIB), INTENT(IN) :: K(N)
    !$OMP TARGET EXIT DATA MAP(RELEASE:K)
  END SUBROUTINE
  SUBROUTINE EXIT_FROM_R(P, N)
    INTEGER(KIND=JPIB), INTENT(IN) :: N
    REAL(KIND=JPRB), INTENT(INOUT) :: P(N)
    !$OMP TARGET EXIT DATA MAP(FROM:P)
  END SUBROUTINE

  ! For an array that may be a strided section (the PGP argument of the transforms, which
  ! the IFS and GPNORM_TRANS fill from sections of larger arrays): a contiguous 1-D view of
  ! the span from its first to its last element, plus the element stride of each dimension,
  ! so device code can address it through an explicit-shape dummy. The span lies inside the
  ! range EXT_ACC_PASS maps. KLEN is 0, and PVIEW disassociated, for an empty array.
  SUBROUTINE ES_STRIDED_VIEW(P,PVIEW,KLEN,KS1,KS2,KS3)
    USE ISO_C_BINDING, ONLY: C_LOC, C_F_POINTER, C_INTPTR_T, C_SIZEOF
    REAL(KIND=JPRB), INTENT(IN), TARGET :: P(:,:,:)
    REAL(KIND=JPRB), POINTER, CONTIGUOUS, INTENT(OUT) :: PVIEW(:)
    INTEGER(KIND=JPIB), INTENT(OUT) :: KLEN, KS1, KS2, KS3

    INTEGER(KIND=C_INTPTR_T) :: IBASE
    INTEGER(KIND=JPIB) :: IELEM

    KLEN = 0; KS1 = 0; KS2 = 0; KS3 = 0
    NULLIFY(PVIEW)
    IF (SIZE(P) == 0) RETURN
    IELEM = C_SIZEOF(P(1,1,1))
    IBASE = TRANSFER(C_LOC(P(1,1,1)),IBASE)
    IF (SIZE(P,1) > 1) KS1 = (TRANSFER(C_LOC(P(2,1,1)),IBASE)-IBASE)/IELEM
    IF (SIZE(P,2) > 1) KS2 = (TRANSFER(C_LOC(P(1,2,1)),IBASE)-IBASE)/IELEM
    IF (SIZE(P,3) > 1) KS3 = (TRANSFER(C_LOC(P(1,1,2)),IBASE)-IBASE)/IELEM
    IF (KS1 < 0 .OR. KS2 < 0 .OR. KS3 < 0) THEN
      ! Not expected from any caller; refusing is safer than addressing outside the span.
      KLEN = -1
      RETURN
    ENDIF
    KLEN = (SIZE(P,1)-1)*KS1+(SIZE(P,2)-1)*KS2+(SIZE(P,3)-1)*KS3+1
    CALL C_F_POINTER(C_LOC(P(1,1,1)),PVIEW,[KLEN])
  END SUBROUTINE

END MODULE ES_MAP_MOD
