! (C) Copyright 2000- ECMWF.
! (C) Copyright 2000- Meteo-France.
! (C) Copyright 2022- NVIDIA.
! 
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!

MODULE TRMTOL_PACK_UNPACK
  USE BUFFERED_ALLOCATOR_MOD, ONLY: ALLOCATION_RESERVATION_HANDLE
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: TRMTOL_PACK, TRMTOL_PACK_HANDLE, PREPARE_TRMTOL_PACK
  PUBLIC :: TRMTOL_UNPACK, TRMTOL_UNPACK_HANDLE, PREPARE_TRMTOL_UNPACK

  TYPE TRMTOL_PACK_HANDLE
    TYPE(ALLOCATION_RESERVATION_HANDLE) :: HFOUBUF_IN
  END TYPE
  TYPE TRMTOL_UNPACK_HANDLE
    TYPE(ALLOCATION_RESERVATION_HANDLE) :: HREEL
  END TYPE

CONTAINS
  FUNCTION PREPARE_TRMTOL_PACK(ALLOCATOR,KF_LEG) RESULT(HTRMTOL_PACK)
    USE PARKIND_ECTRANS,        ONLY: JPIM, JPRBT, JPIB
    USE TPM_DISTR,              ONLY: D
    USE ISO_C_BINDING,          ONLY: C_SIZEOF
    USE BUFFERED_ALLOCATOR_MOD, ONLY: BUFFERED_ALLOCATOR, RESERVE

    IMPLICIT NONE

    TYPE(BUFFERED_ALLOCATOR), INTENT(INOUT) :: ALLOCATOR
    INTEGER(KIND=JPIM), INTENT(IN) :: KF_LEG

    TYPE(TRMTOL_PACK_HANDLE) :: HTRMTOL_PACK

    INTEGER(KIND=JPIB) :: IALLOC_SZ

    REAL(KIND=JPRBT) :: ZPRBT_DUMMY

    IALLOC_SZ = 2_JPIB*D%NLENGT1B*KF_LEG*C_SIZEOF(ZPRBT_DUMMY)
    HTRMTOL_PACK%HFOUBUF_IN = RESERVE(ALLOCATOR, IALLOC_SZ, "HTRMTOL_PACK%HFOUBUF_IN")
  END FUNCTION
  SUBROUTINE TRMTOL_PACK(ALLOCATOR,HTRMTOL_PACK,ZOUTS,ZOUTA,ZOUTS0,ZOUTA0,FOUBUF_IN,KF_LEG)

    !**** *TRMTOL_PACK* - Packing buffer for TRMTOL

    !     Purpose.
    !     --------
    !        Packs data from LTINV outputs into FOUBUF for conversion to fourier space

    !**   Interface.
    !     ----------
    !        CALL TRMTOL_PACK(...)

    !        Explicit arguments :  ZOUTS - symmetric data
    !        --------------------  ZOUTA - asymmetric data
    !                              ZOUTS0 - symmetric data for KMLOC0
    !                              ZOUTA0 - asymmetric data for KMLOC0
    !                              FOUBUF_IN - output towards TRMTOL
    !                              KF_LEG - number of fields (we have 2XKF_LEG because complex)

    !        Implicit arguments :  None.
    !        --------------------

    !     Externals.
    !     ----------

    !     Reference.
    !     ----------
    !        ECMWF Research Department documentation of the IFS

    !     Author.
    !     -------
    !      Nils Wedi + Mats Hamrud + George Modzynski
    !
    !     Modifications.
    !     --------------
    !        J.Hague : Oct 2012 DR_HOOK round calls to DGEMM:
    !      F. Vana  05-Mar-2015  Support for single precision
    !     ------------------------------------------------------------------

    USE PARKIND_ECTRANS,        ONLY: JPIM, JPRB, JPRBT, JPRD, JPIB
    USE YOMHOOK,                ONLY: LHOOK, DR_HOOK, JPHOOK
    USE TPM_DIM,                ONLY: R
    USE TPM_GEOMETRY,           ONLY: G
    USE TPM_DISTR,              ONLY: D
    USE LEINV_MOD,              ONLY: LEINV_STRIDES
    USE BUFFERED_ALLOCATOR_MOD, ONLY: BUFFERED_ALLOCATOR, ASSIGN_PTR, GET_ALLOCATION
    USE ISO_C_BINDING,          ONLY: C_SIZEOF

    IMPLICIT NONE


    !     DUMMY ARGUMENTS
    TYPE(BUFFERED_ALLOCATOR), INTENT(IN) :: ALLOCATOR
    TYPE(TRMTOL_PACK_HANDLE), INTENT(IN) :: HTRMTOL_PACK
    REAL(KIND=JPRB), INTENT(OUT), POINTER :: FOUBUF_IN(:)
    ! CONTIGUOUS so these can be sequence-associated with the explicit-shape dummies of
    ! TRMTOL_PACK_KERNEL without the compiler allowing for a packing temporary.
    REAL(KIND=JPRBT), INTENT(IN), CONTIGUOUS :: ZOUTS(:), ZOUTA(:)
    REAL(KIND=JPRD), INTENT(IN), CONTIGUOUS :: ZOUTS0(:), ZOUTA0(:)
    INTEGER(KIND=JPIM), INTENT(IN)  :: KF_LEG

    !     LOCAL
    REAL(KIND=JPRBT) :: ZAOA, ZSOA

    ! An element of a non-CONTIGUOUS pointer array may not be sequence-associated with an
    ! explicit-shape dummy (F2018 15.5.2.4); this alias addresses the same allocator slab.
    REAL(KIND=JPRB), POINTER, CONTIGUOUS :: ZFOUBUF_IN_C(:)

    INTEGER(KIND=JPIM) :: KMLOC, KM, ISL, JGL, JK, IGLS
    INTEGER(KIND=JPIB) :: OFFSET1, OFFSET2
    INTEGER(KIND=JPIM)  :: IOUT_STRIDES0
    INTEGER(KIND=JPIB)  :: IOUT_SIZE
    INTEGER(KIND=JPIM)  :: IOUT0_STRIDES0, IOUT0_SIZE

    REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

    ASSOCIATE(D_NUMP=>D%NUMP, R_NDGNH=>R%NDGNH, R_NDGL=>R%NDGL, G_NDGLU=>G%NDGLU, &
            & D_MYMS=>D%MYMS, D_NPNTGTB1=>D%NPNTGTB1, D_OFFSETS_GEMM1=>D%OFFSETS_GEMM1)

    IF (LHOOK) CALL DR_HOOK('TRMTOL_PACK',0,ZHOOK_HANDLE)

    CALL ASSIGN_PTR(FOUBUF_IN, GET_ALLOCATION(ALLOCATOR, HTRMTOL_PACK%HFOUBUF_IN),&
      & 1_JPIB, 2_JPIB*D%NLENGT1B*KF_LEG*C_SIZEOF(FOUBUF_IN(1)))

    CALL LEINV_STRIDES(KF_LEG,IOUT_STRIDES0=IOUT_STRIDES0,IOUT_SIZE=IOUT_SIZE,&
                       IOUT0_STRIDES0=IOUT0_STRIDES0,IOUT0_SIZE=IOUT0_SIZE)

#ifdef OMPGPU
    ! ZOUTS/ZOUTA/ZOUTS0/ZOUTA0/FOUBUF_IN are growing-allocator buffers. Their storage is
    ! registered with omp_target_associate_ptr, so ordinary mapping resolves it in the
    ! compute construct below, but their descriptors are never entered in the present
    ! table and so cannot be MAP(PRESENT)'d.
    ! D, G and R are reached only through their ASSOCIATE aliases. Naming the parent types here
    ! makes the runtime walk every allocatable component of those derived types and re-copy each
    ! component descriptor on entry, so only the aliases are mapped.
    !$OMP TARGET DATA MAP(ECTRANS_MAP_PRESENT_ALLOC:D_MYMS,D_NPNTGTB1,D_NUMP,G_NDGLU,R_NDGNH,R_NDGL) &
    !$OMP&            MAP(ECTRANS_MAP_PRESENT_ALLOC:D_OFFSETS_GEMM1)
#endif
#ifdef ACCGPU
    !$ACC DATA PRESENT(D,D_MYMS,D_NPNTGTB1,D_NUMP,G,G_NDGLU,R,R_NDGNH,R_NDGL) &
    !$ACC&     PRESENT(ZOUTS,ZOUTA,ZOUTS0,ZOUTA0,FOUBUF_IN,D_OFFSETS_GEMM1)
#endif

    ZFOUBUF_IN_C => FOUBUF_IN

    CALL TRMTOL_PACK_KERNEL(ZOUTS,ZOUTA,SIZE(ZOUTS,KIND=JPIB), &
      &                     ZOUTS0,ZOUTA0,SIZE(ZOUTS0,KIND=JPIB), &
      &                     ZFOUBUF_IN_C(1),SIZE(FOUBUF_IN,KIND=JPIB), &
      &                     D_MYMS,D_OFFSETS_GEMM1,D_NPNTGTB1,D_NUMP,R_NDGL, &
      &                     G_NDGLU,SIZE(G_NDGLU)-1, &
      &                     KF_LEG,R_NDGNH,IOUT_STRIDES0,IOUT0_STRIDES0)

#ifdef OMPGPU
    !$OMP END TARGET DATA
#endif
#ifdef ACCGPU
    !$ACC WAIT(1)

    !$ACC END DATA
#endif

    IF (LHOOK) CALL DR_HOOK('TRMTOL_PACK',1,ZHOOK_HANDLE)

    END ASSOCIATE
  END SUBROUTINE TRMTOL_PACK

  ! Pack body with explicit-shape dummy arguments. A POINTER or assumed-shape actual is
  ! described by a dope vector, and the compiler places that dope vector in the device data
  ! environment on every launch: create map entry, copy 48 bytes, tear it down. Explicit
  ! shape carries the extents as FIRSTPRIVATE scalars instead, so nothing is copied.
  SUBROUTINE TRMTOL_PACK_KERNEL(PZOUTS,PZOUTA,KOUT_SIZE,PZOUTS0,PZOUTA0,KOUT0_SIZE, &
    &                           PFOUBUF_IN,KFOUBUF_IN, &
    &                           KMYMS,KOFFSETS_GEMM1,KNPNTGTB1,KNUMP,KNDGL, &
    &                           KNDGLU,KNSMAX, &
    &                           KF_LEG,KNDGNH,KOUT_STRIDES0,KOUT0_STRIDES0)
    USE PARKIND_ECTRANS, ONLY: JPIM, JPRB, JPRBT, JPRD, JPIB

    IMPLICIT NONE

    INTEGER(KIND=JPIB), INTENT(IN)  :: KOUT_SIZE, KOUT0_SIZE, KFOUBUF_IN
    INTEGER(KIND=JPIM), INTENT(IN)  :: KNUMP, KNDGL, KNSMAX
    INTEGER(KIND=JPIM), INTENT(IN)  :: KF_LEG, KNDGNH, KOUT_STRIDES0, KOUT0_STRIDES0
    REAL(KIND=JPRBT),   INTENT(IN)  :: PZOUTS(KOUT_SIZE), PZOUTA(KOUT_SIZE)
    REAL(KIND=JPRD),    INTENT(IN)  :: PZOUTS0(KOUT0_SIZE), PZOUTA0(KOUT0_SIZE)
    REAL(KIND=JPRB),    INTENT(OUT) :: PFOUBUF_IN(KFOUBUF_IN)
    INTEGER(KIND=JPIM), INTENT(IN)  :: KMYMS(KNUMP)
    INTEGER(KIND=JPIB), INTENT(IN)  :: KOFFSETS_GEMM1(KNUMP+1)
    INTEGER(KIND=JPIM), INTENT(IN)  :: KNPNTGTB1(KNUMP,KNDGL)
    INTEGER(KIND=JPIM), INTENT(IN)  :: KNDGLU(0:KNSMAX)

    REAL(KIND=JPRBT) :: ZAOA, ZSOA
    INTEGER(KIND=JPIM) :: KMLOC, KM, ISL, JGL, JK, IGLS
    INTEGER(KIND=JPIB) :: OFFSET1, OFFSET2

#ifdef OMPGPU
    ! Directive incomplete -> putting more variables in SHARED() triggers internal compiler error
    ! ftn-7991: INTERNAL COMPILER ERROR:  "Too few arguments on the stack"
    !$OMP TARGET TEAMS DISTRIBUTE PARALLEL DO COLLAPSE(3) &
    !$OMP& ECTRANS_DEVICE_ADDR_CLAUSE(PZOUTS,PZOUTA,PZOUTS0,PZOUTA0,PFOUBUF_IN) &
    !$OMP& MAP(ECTRANS_MAP_PRESENT_ALLOC:KMYMS,KOFFSETS_GEMM1,KNPNTGTB1,KNDGLU) &
    !$OMP& PRIVATE(KM,ISL,IGLS,OFFSET1,OFFSET2,ZAOA,ZSOA) &
    !$OMP& ECTRANS_LOOP_BOUNDS_CLAUSE(KF_LEG) FIRSTPRIVATE(KOUT_STRIDES0,KOUT0_STRIDES0)
#endif
#ifdef ACCGPU
    !$ACC PARALLEL LOOP COLLAPSE(3) DEFAULT(NONE) PRIVATE(KM,ISL,IGLS,OFFSET1,OFFSET2,ZAOA,ZSOA) &
    !$ACC&              PRESENT(KMYMS,KOFFSETS_GEMM1,KNPNTGTB1,KNDGLU) &
    !$ACC&              FIRSTPRIVATE(KF_LEG,KOUT_STRIDES0,KOUT0_STRIDES0) &
#ifndef _CRAYFTN
    !$ACC& ASYNC(1)
#else
    !$ACC&
#endif
#endif
    DO KMLOC=1,KNUMP
      DO JGL=1,KNDGNH
        DO JK=1,2*KF_LEG
          KM = KMYMS(KMLOC)
          ISL = KNDGNH-KNDGLU(KM)+1
          IF (JGL >= ISL) THEN
            !(DO JGL=ISL,KNDGNH)
            IGLS = KNDGL+1-JGL
            OFFSET1 = 2_JPIB*KNPNTGTB1(KMLOC,JGL )*KF_LEG
            OFFSET2 = 2_JPIB*KNPNTGTB1(KMLOC,IGLS)*KF_LEG

            IF(KM /= 0) THEN
              ZSOA = PZOUTS(JK+(JGL-ISL)*KOUT_STRIDES0+KOFFSETS_GEMM1(KMLOC)*KOUT_STRIDES0)
              ZAOA = PZOUTA(JK+(JGL-ISL)*KOUT_STRIDES0+KOFFSETS_GEMM1(KMLOC)*KOUT_STRIDES0)
            ELSEIF (MOD((JK-1),2) == 0) THEN
              ZSOA = PZOUTS0((JK-1)/2+1+(JGL-1)*KOUT0_STRIDES0)
              ZAOA = PZOUTA0((JK-1)/2+1+(JGL-1)*KOUT0_STRIDES0)
            ELSE
              ! Imaginary values of KM=0 is zero, though I don't think we care
              ZSOA = 0_JPRBT
              ZAOA = 0_JPRBT
            ENDIF

            PFOUBUF_IN(OFFSET1+JK) = ZAOA+ZSOA
            PFOUBUF_IN(OFFSET2+JK) = ZSOA-ZAOA
          ENDIF
        ENDDO
      ENDDO
    ENDDO
  END SUBROUTINE TRMTOL_PACK_KERNEL

  FUNCTION PREPARE_TRMTOL_UNPACK(ALLOCATOR,KF_FS) RESULT(HTRMTOL_UNPACK)
    USE PARKIND_ECTRANS,        ONLY: JPIM, JPRBT, JPIB
    USE TPM_DISTR,              ONLY: D
    USE BUFFERED_ALLOCATOR_MOD, ONLY: BUFFERED_ALLOCATOR, RESERVE
    USE ISO_C_BINDING,          ONLY: C_SIZEOF

    IMPLICIT NONE

    TYPE(BUFFERED_ALLOCATOR), INTENT(INOUT) :: ALLOCATOR
    INTEGER(KIND=JPIM) :: KF_FS

    TYPE(TRMTOL_UNPACK_HANDLE) :: HTRMTOL_UNPACK

    REAL(KIND=JPRBT) :: DUMMY

    HTRMTOL_UNPACK%HREEL = RESERVE(ALLOCATOR, 1_JPIB*D%NLENGTF*KF_FS*C_SIZEOF(DUMMY), "HTRMTOL_UNPACK%HREEL")

  END FUNCTION PREPARE_TRMTOL_UNPACK
SUBROUTINE TRMTOL_UNPACK(ALLOCATOR,HTRMTOL_UNPACK,FOUBUF,PREEL_COMPLEX,KF_CURRENT,KF_TOTAL)

!**** *TRMTOL_UNPACK* - Copy fourier data from buffer to local array

!     Purpose.
!     --------
!        Routine for copying fourier data from buffer to local array

!**   Interface.
!     ----------
!     CALL TRMTOL_UNPACK(...)

!     Explicit arguments :  PREEL_COMPLEX - local fourier/GP array
!     --------------------  KF_CURRENT - number of fields that are read (from Legendre space)
!                           KF_TOTAL - total fields in PREEL ("stride")
!
!     Externals.  None.
!     ----------

!     Author.
!     -------
!        Mats Hamrud *ECMWF*

!     Modifications.
!     --------------
!        Original : 2000-04-01

!     ------------------------------------------------------------------

USE PARKIND_ECTRANS,        ONLY: JPIM, JPRBT, JPIB
USE TPM_DISTR,              ONLY: D, MYSETW
USE TPM_GEOMETRY,           ONLY: G
USE BUFFERED_ALLOCATOR_MOD, ONLY: BUFFERED_ALLOCATOR, ASSIGN_PTR, GET_ALLOCATION
USE ISO_C_BINDING,          ONLY: C_SIZEOF
!

IMPLICIT NONE

! CONTIGUOUS so FOUBUF can be sequence-associated with the explicit-shape dummy of
! TRMTOL_UNPACK_KERNEL without the compiler allowing for a packing temporary.
REAL(KIND=JPRBT), INTENT(IN), CONTIGUOUS :: FOUBUF(:)
REAL(KIND=JPRBT), INTENT(OUT), POINTER :: PREEL_COMPLEX(:)
INTEGER(KIND=JPIM),INTENT(IN) :: KF_CURRENT, KF_TOTAL
TYPE(BUFFERED_ALLOCATOR), INTENT(IN) :: ALLOCATOR
TYPE(TRMTOL_UNPACK_HANDLE), INTENT(IN) :: HTRMTOL_UNPACK

! An element of a non-CONTIGUOUS pointer array may not be sequence-associated with an
! explicit-shape dummy (F2018 15.5.2.4); this alias addresses the same allocator slab.
REAL(KIND=JPRBT), POINTER, CONTIGUOUS :: ZPREEL_COMPLEX_C(:)

INTEGER(KIND=JPIM) :: JM,JF,IGLG,OFFSET_VAR,KGL,ILOEN_MAX
INTEGER(KIND=JPIB) :: IOFF_LAT, ISTA
REAL(KIND=JPRBT) :: RET_REAL, RET_COMPLEX

ASSOCIATE(D_NDGL_FS=>D%NDGL_FS, D_NSTAGTF=>D%NSTAGTF, D_NPNTGTB0=>D%NPNTGTB0, D_NPTRLS=>D%NPTRLS, &
        & G_NLOEN=>G%NLOEN, G_NMEN=>G%NMEN)

CALL ASSIGN_PTR(PREEL_COMPLEX, GET_ALLOCATION(ALLOCATOR, HTRMTOL_UNPACK%HREEL),&
    & 1_JPIB, 1_JPIB*KF_TOTAL*D%NLENGTF*C_SIZEOF(PREEL_COMPLEX(1)))

#ifdef OMPGPU
! FOUBUF/PREEL_COMPLEX are growing-allocator buffers. Their storage is registered with
! omp_target_associate_ptr, so ordinary mapping resolves it in the compute construct
! below, but their descriptors are never entered in the present table and so cannot be
! MAP(PRESENT)'d.
! G and D are reached only through their ASSOCIATE aliases. Naming the parent types here makes
! the runtime walk every allocatable component of TYPE_GEOMETRY and TYPE_DISTR and re-copy each
! component descriptor on entry, so only the aliases are mapped.
!$OMP TARGET DATA MAP(ECTRANS_MAP_PRESENT_ALLOC:G_NLOEN,G_NMEN,D_NPNTGTB0,D_NSTAGTF,D_NDGL_FS)
#endif
#ifdef ACCGPU
!$ACC DATA PRESENT(G,G_NLOEN,G_NMEN,D,D_NPNTGTB0,FOUBUF,PREEL_COMPLEX,D_NSTAGTF,D_NDGL_FS) ASYNC(1)
#endif

OFFSET_VAR=D_NPTRLS(MYSETW)
ILOEN_MAX=MAXVAL(G_NLOEN)
ZPREEL_COMPLEX_C => PREEL_COMPLEX

CALL TRMTOL_UNPACK_KERNEL(FOUBUF,SIZE(FOUBUF,KIND=JPIB), &
  &                       ZPREEL_COMPLEX_C(1),SIZE(PREEL_COMPLEX,KIND=JPIB), &
  &                       G_NLOEN,G_NMEN,SIZE(G_NLOEN), &
  &                       D_NPNTGTB0,SIZE(D_NPNTGTB0,1)-1, &
  &                       D_NSTAGTF,SIZE(D_NSTAGTF), &
  &                       D_NDGL_FS,KF_CURRENT,KF_TOTAL,OFFSET_VAR,ILOEN_MAX)
#ifdef OMPGPU
!$OMP END TARGET DATA
#endif
#ifdef ACCGPU
!$ACC END DATA

!$ACC WAIT(1)
#endif

END ASSOCIATE

END SUBROUTINE TRMTOL_UNPACK

! Unpack body with explicit-shape dummy arguments, for the same reason as TRMTOL_PACK_KERNEL:
! a POINTER or assumed-shape actual costs a dope-vector copy on every launch.
SUBROUTINE TRMTOL_UNPACK_KERNEL(PFOUBUF,KFOUBUF,PREEL_COMPLEX,KREEL, &
  &                             KNLOEN,KNMEN,KNDGL, &
  &                             KNPNTGTB0,KNSMAX, &
  &                             KNSTAGTF,KNSTAGTF_N, &
  &                             KNDGL_FS,KF_CURRENT,KF_TOTAL,KOFFSET_VAR,KLOEN_MAX)
USE PARKIND_ECTRANS, ONLY: JPIM, JPRBT, JPIB

IMPLICIT NONE

INTEGER(KIND=JPIB), INTENT(IN)  :: KFOUBUF, KREEL
INTEGER(KIND=JPIM), INTENT(IN)  :: KNDGL, KNSMAX, KNSTAGTF_N
INTEGER(KIND=JPIM), INTENT(IN)  :: KNDGL_FS, KF_CURRENT, KF_TOTAL, KOFFSET_VAR, KLOEN_MAX
REAL(KIND=JPRBT),   INTENT(IN)  :: PFOUBUF(KFOUBUF)
REAL(KIND=JPRBT),   INTENT(OUT) :: PREEL_COMPLEX(KREEL)
INTEGER(KIND=JPIM), INTENT(IN)  :: KNLOEN(KNDGL), KNMEN(KNDGL)
INTEGER(KIND=JPIM), INTENT(IN)  :: KNPNTGTB0(0:KNSMAX,KNDGL_FS)
INTEGER(KIND=JPIB), INTENT(IN)  :: KNSTAGTF(KNSTAGTF_N)

INTEGER(KIND=JPIM) :: JM, JF, IGLG, KGL
INTEGER(KIND=JPIB) :: IOFF_LAT, ISTA
REAL(KIND=JPRBT) :: RET_REAL, RET_COMPLEX

#ifdef OMPGPU
! Directive incomplete -> putting more variables in SHARED() triggers internal compiler error
! ftn-7991: INTERNAL COMPILER ERROR:  "Too few arguments on the stack"
!$OMP TARGET TEAMS DISTRIBUTE PARALLEL DO COLLAPSE(3) &
!$OMP& ECTRANS_DEVICE_ADDR_CLAUSE(PFOUBUF,PREEL_COMPLEX) &
!$OMP& MAP(ECTRANS_MAP_PRESENT_ALLOC:KNLOEN,KNMEN,KNPNTGTB0,KNSTAGTF) &
!$OMP& PRIVATE(IGLG,IOFF_LAT,ISTA,RET_REAL,RET_COMPLEX) &
!$OMP& ECTRANS_LOOP_BOUNDS_CLAUSE(KF_CURRENT,KLOEN_MAX) FIRSTPRIVATE(KOFFSET_VAR,KF_TOTAL)
#endif
#ifdef ACCGPU
!$ACC PARALLEL LOOP PRIVATE(IGLG,IOFF_LAT,ISTA,RET_REAL,RET_COMPLEX) FIRSTPRIVATE(KF_CURRENT,&
!$ACC&              KF_TOTAL,KOFFSET_VAR,KLOEN_MAX) DEFAULT(NONE) TILE(32,16,1) &
!$ACC&              PRESENT(KNLOEN,KNMEN,KNPNTGTB0,KNSTAGTF) &
#ifndef _CRAYFTN
!$ACC& ASYNC(1)
#else
!$ACC&
#endif
#endif
DO KGL=1,KNDGL_FS
  DO JF=1,KF_CURRENT
    DO JM=0,KLOEN_MAX/2
      IGLG = KOFFSET_VAR+KGL-1

      ! FFT transforms NLON real values to floor(NLON/2)+1 complex numbers. Hence we have
      ! to fill those floor(NLON/2)+1 values.
      ! Truncation happens starting at KNMEN+1. Hence, we zero-fill those values.
      IF (JM <= KNLOEN(IGLG)/2) THEN
        RET_REAL = 0.0_JPRBT
        RET_COMPLEX = 0.0_JPRBT
        IF (JM <= KNMEN(IGLG)) THEN
          ISTA  = 2_JPIB*KNPNTGTB0(JM,KGL)*KF_CURRENT

          RET_REAL    = PFOUBUF(ISTA+2*JF-1)
          RET_COMPLEX = PFOUBUF(ISTA+2*JF  )
        ENDIF
        IOFF_LAT = 1_JPIB*KF_TOTAL*KNSTAGTF(KGL)+(JF-1)*(KNSTAGTF(KGL+1)-KNSTAGTF(KGL))
        PREEL_COMPLEX(IOFF_LAT+2*JM+1) = RET_REAL
        PREEL_COMPLEX(IOFF_LAT+2*JM+2) = RET_COMPLEX
      ENDIF
    ENDDO
  ENDDO
ENDDO
END SUBROUTINE TRMTOL_UNPACK_KERNEL
END MODULE TRMTOL_PACK_UNPACK

