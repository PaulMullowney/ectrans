#define ALIGN(I, A) (((I)+(A)-1)/(A)*(A))
#if defined CUDAGPU
#define ACC_GET_HIP_STREAM ACC_GET_CUDA_STREAM
#define OPENACC_LIB OPENACC
#endif

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

MODULE LEINV_MOD
  USE PARKIND_ECTRANS,        ONLY: JPIM, JPRB, JPRBT, JPRD, JPIB
  USE BUFFERED_ALLOCATOR_MOD, ONLY: BUFFERED_ALLOCATOR
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: LEINV_STRIDES, LEINV

  INTEGER(KIND=JPIM) :: A = 8 !Alignment

CONTAINS
  SUBROUTINE LEINV_STRIDES(KF_LEG,IOUT_STRIDES0,IOUT_SIZE,IIN_STRIDES0,IIN_SIZE,&
                           IOUT0_STRIDES0,IOUT0_SIZE,IIN0_STRIDES0,IIN0_SIZE)
    USE TPM_DIM,   ONLY: R
    USE TPM_DISTR, ONLY: D

    IMPLICIT NONE

    INTEGER(KIND=JPIM), INTENT(IN)  :: KF_LEG

    INTEGER(KIND=JPIM), OPTIONAL :: IOUT_STRIDES0
    INTEGER(KIND=JPIB), OPTIONAL :: IOUT_SIZE
    INTEGER(KIND=JPIM), OPTIONAL :: IIN_STRIDES0
    INTEGER(KIND=JPIB), OPTIONAL :: IIN_SIZE
    INTEGER(KIND=JPIM), OPTIONAL :: IOUT0_STRIDES0, IOUT0_SIZE
    INTEGER(KIND=JPIM), OPTIONAL :: IIN0_STRIDES0, IIN0_SIZE

    ASSOCIATE(D_OFFSETS_GEMM1=>D%OFFSETS_GEMM1, D_OFFSETS_GEMM2=>D%OFFSETS_GEMM2)


    IF (PRESENT(IOUT0_STRIDES0)) &
      IOUT0_STRIDES0 = ALIGN(KF_LEG,A)
    IF (PRESENT(IOUT0_SIZE)) &
      IOUT0_SIZE = IOUT0_STRIDES0 * ALIGN(R%NDGNH,A)
    IF (PRESENT(IIN_STRIDES0)) &
      IIN_STRIDES0 = ALIGN(2*KF_LEG,A)
    IF (PRESENT(IIN_SIZE)) &
      IIN_SIZE = IIN_STRIDES0*D_OFFSETS_GEMM2(D%NUMP+1)
    IF (PRESENT(IOUT_STRIDES0)) &
      IOUT_STRIDES0 = ALIGN(2*KF_LEG,A)
    IF (PRESENT(IOUT_SIZE)) &
      IOUT_SIZE = IOUT_STRIDES0*D_OFFSETS_GEMM1(D%NUMP+1)
    IF (PRESENT(IIN0_STRIDES0)) &
      IIN0_STRIDES0 = ALIGN(KF_LEG,A)
    IF (PRESENT(IIN0_SIZE)) &
      IIN0_SIZE = IIN0_STRIDES0 * ALIGN(MAX((R%NTMAX+2)/2,(R%NTMAX+3)/2),A)

    END ASSOCIATE
  END SUBROUTINE LEINV_STRIDES

  SUBROUTINE LEINV(ALLOCATOR,PIA,ZINP,ZINP0,ZOUTS,ZOUTA,ZOUTS0,ZOUTA0,KF_LEG)
    !**** *LEINV* - Inverse Legendre transform.

    !     Purpose.
    !     --------
    !        Inverse Legendre tranform of all variables(kernel).

    !**   Interface.
    !     ----------
    !        CALL LEINV(...)

    !        Explicit arguments :  KM - zonal wavenumber (input-c)
    !        --------------------  KFC - number of fields to tranform (input-c)
    !                              PIA - spectral fields
    !                              for zonal wavenumber KM (input)

    !        Implicit arguments :  None.
    !        --------------------

    !     Method.
    !     -------

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

    USE TPM_GEN,                     ONLY: LSYNC_TRANS, NOUT, NCUR_RESOL
    USE YOMHOOK,                     ONLY: LHOOK, DR_HOOK, JPHOOK
    USE TPM_DIM,                     ONLY: R
    USE TPM_GEOMETRY,                ONLY: G
    USE TPM_FIELDS_GPU,              ONLY: FG
    USE TPM_DISTR,                   ONLY: D
    USE HICBLAS_MOD,                 ONLY: HIP_DGEMM_BATCHED, &
      &                                    HIP_DGEMM_GROUPED, HIP_SGEMM_GROUPED
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_LONG, C_LOC
    USE MPL_MODULE,                  ONLY: MPL_BARRIER,MPL_ALL_MS_COMM
    USE TPM_STATS,                   ONLY: GSTATS => GSTATS_NVTX
#ifdef ACCGPU
    USE OPENACC_LIB, ONLY: ACC_GET_HIP_STREAM
#endif
#ifdef TRANS_SINGLE
#define HIP_GEMM HIP_SGEMM_GROUPED
#else
#define HIP_GEMM HIP_DGEMM_GROUPED
#endif

    IMPLICIT NONE

    REAL(KIND=JPRB),    INTENT(IN)  :: PIA(:,:,:)
    INTEGER(KIND=JPIM), INTENT(IN)  :: KF_LEG
    REAL(KIND=JPRBT), POINTER, INTENT(OUT) :: ZINP(:), ZOUTS(:), ZOUTA(:)
    REAL(KIND=JPRD), POINTER, INTENT(OUT) :: ZINP0(:), ZOUTS0(:), ZOUTA0(:)
    TYPE(BUFFERED_ALLOCATOR), INTENT(IN) :: ALLOCATOR

    !     LOCAL
    INTEGER(KIND=JPIM)  :: KS(D%NUMP), NS(D%NUMP)
    INTEGER(KIND=JPIB)  :: AOFFSETS(D%NUMP), BOFFSETS(D%NUMP), COFFSETS(D%NUMP)
    INTEGER(KIND=JPIM)  :: KM, KMLOC, IMLOC0(1)
    INTEGER(KIND=JPIM)  :: IOUT_STRIDES0
    INTEGER(KIND=JPIB)  :: IOUT_SIZE
    INTEGER(KIND=JPIM)  :: IIN_STRIDES0
    INTEGER(KIND=JPIB)  :: IIN_SIZE
    INTEGER(KIND=JPIM)  :: IOUT0_STRIDES0, IOUT0_SIZE
    INTEGER(KIND=JPIM)  :: IIN0_STRIDES0, IIN0_SIZE

    ! An element of a non-CONTIGUOUS pointer array may not be sequence-associated with an
    ! explicit-shape dummy (F2018 15.5.2.4), so the load bodies at the end of this module are
    ! handed these aliases instead. They address the same allocator slab, which is contiguous.
    REAL(KIND=JPRBT), POINTER, CONTIGUOUS :: ZINP_C(:)
    REAL(KIND=JPRD),  POINTER, CONTIGUOUS :: ZINP0_C(:)

    REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

    INTEGER(KIND=C_LONG) :: HIP_STREAM

    ASSOCIATE(D_NUMP=>D%NUMP, R_NSMAX=>R%NSMAX, G_NDGLU=>G%NDGLU, D_MYMS=>D%MYMS, D_OFFSETS_GEMM1=>D%OFFSETS_GEMM1,&
        D_OFFSETS_GEMM2=>D%OFFSETS_GEMM2, &
        ZAA=>FG%ZAA, ZAS=>FG%ZAS, ZAA0=>FG%ZAA0, ZAS0=>FG%ZAS0)

    !*       1.1      PREPARATIONS.
    IF (LHOOK) CALL DR_HOOK('LE_DGEMM',0,ZHOOK_HANDLE)

#ifdef ACCGPU
    HIP_STREAM = INT(ACC_GET_HIP_STREAM(1_C_INT), C_LONG)
#endif
#ifdef OMPGPU
    HIP_STREAM = 0_C_LONG
#endif

    !     ------------------------------------------------------------------

    !*       1.       PERFORM LEGENDRE TRANFORM.
    !                 --------------------------

    !*       1.1      PREPARATIONS.

    CALL LEINV_STRIDES(KF_LEG,IOUT_STRIDES0,IOUT_SIZE,IIN_STRIDES0,IIN_SIZE,&
                       IOUT0_STRIDES0,IOUT0_SIZE,IIN0_STRIDES0,IIN0_SIZE)

    ZINP_C => ZINP
    ZINP0_C => ZINP0

#ifdef OMPGPU
    ! ZINP/ZOUT*/PIA are growing-allocator buffers. Under OMPGPU the allocator hands out
    ! device pointers directly (see GROWING_ALLOCATOR_MOD: the host pointer is set from
    ! OMP_TARGET_ALLOC and then self-associated), so their descriptors are never entered
    ! in the present table and cannot be MAP(PRESENT)'d. Naming them in SHARED instead
    ! makes each kernel launch look the data region up in the present table and attach it;
    ! HAS_DEVICE_ADDR states the fact directly and skips both. It does not make the launch
    ! free: the assertion covers the data, not the dummy's descriptor, which is a host stack
    ! object holding the bounds and strides the kernel needs to index with. That 48 bytes
    ! (96 for rank-3 PIA) is still allocated, copied and freed per launch. The compute
    ! constructs that fill ZINP and ZINP0 therefore live in LEINV_LOAD_ANTISYM and
    ! LEINV_LOAD_SYM at the end of this module, whose dummies are explicit-shape and so carry
    ! no descriptor at all. PIA still carries one, for the reason given there.
    ! PIA is assumed-shape rather than a POINTER because both callers pass an array section,
    ! but the section is taken from an allocator buffer in each case (LTINV_MOD's PIA and
    ! LTDIRAD_MOD's POA1), so its base address is a device address like the rest.
    ! The GEMM calls below take their device addresses via USE_DEVICE_ADDR, as before.
    ! D and R are reached only through their ASSOCIATE aliases. Naming the parent types here
    ! makes the runtime walk every allocatable component of TYPE_DISTR and TYPE_DIM and re-copy
    ! each component descriptor on entry, so only the aliases are mapped.
    !$OMP TARGET DATA &
    !$OMP&              MAP(ECTRANS_MAP_PRESENT_ALLOC:D_MYMS,D_NUMP) &
    !$OMP&              MAP(ECTRANS_MAP_PRESENT_ALLOC:ZAA,ZAS) &
    !$OMP&              MAP(ECTRANS_MAP_PRESENT_ALLOC:R_NSMAX,D_OFFSETS_GEMM2)
#endif
#ifdef ACCGPU
    !$ACC DATA PRESENT(D,D_MYMS,D_NUMP) &
    !$ACC&     PRESENT(ZINP,ZOUTS,ZOUTA,ZINP0,ZOUTS0,ZOUTA0) &
    !$ACC&     PRESENT(ZAA,ZAS,PIA) &
    !$ACC&     PRESENT(R,R_NSMAX,D_OFFSETS_GEMM2)
#endif

    ! READ 2:NSMAX+3

    !IF KM=0 and NSMAX is 6:
    !    IA=1
    !    DO=1,6/2+1 ... 1..4
    !       PIA_2=1+1+(J-1)*2 ...2+(0..3)*2 .... 2,4,6,8
    !IF KM=0 and NSMAX is 7:
    !    IA=2
    !    DO=1,7/2+1 ... 1..4
    !       PIA_2=2+1+(1..4-1)*2 ...3+(0..3)*2 .... 3,5,7,9

    CALL LEINV_LOAD_ANTISYM(PIA,ZINP_C(1),SIZE(ZINP,KIND=JPIB),ZINP0_C(1),SIZE(ZINP0), &
      &                     D_MYMS,D_OFFSETS_GEMM2,D_NUMP, &
      &                     KF_LEG,R_NSMAX,IIN_STRIDES0,IIN0_STRIDES0)


    IF (LSYNC_TRANS) THEN
#ifdef ACCGPU
      !$ACC WAIT(1)
#endif
      CALL GSTATS(440,0)
      CALL MPL_BARRIER(MPL_ALL_MS_COMM,CDSTRING='')
      CALL GSTATS(440,1)
    ENDIF
    CALL GSTATS(424,0)

    IMLOC0 = FINDLOC(D_MYMS,0)
    IF (IMLOC0(1) > 0) THEN
      ! compute m=0 in double precision
#ifdef OMPGPU
      !$OMP TARGET DATA USE_DEVICE_ADDR(ZAA0)
#endif
#ifdef ACCGPU
      !$ACC HOST_DATA USE_DEVICE(ZAA0,ZINP0,ZOUTA0)
#endif
      CALL HIP_DGEMM_BATCHED( &
        & 'N', 'T', &
        & KF_LEG, G_NDGLU(0), (R_NSMAX+2)/2, &
        & 1.0_JPRD, &
        & C_LOC(ZINP0), IIN0_STRIDES0, 0, &
        & C_LOC(ZAA0), SIZE(ZAA0,1), 0, &
        & 0.0_JPRD, &
        & C_LOC(ZOUTA0), IOUT0_STRIDES0, 0, &
        & 1, HIP_STREAM, C_LOC(ALLOCATOR%PTR))
#ifdef ACCGPU
      !$ACC END HOST_DATA
#endif
#ifdef OMPGPU
      !$OMP END TARGET DATA
#endif
   ENDIF

    DO KMLOC=1,D_NUMP
      KM = D_MYMS(KMLOC)
      KS(KMLOC) = (R_NSMAX-KM+2)/2
      NS(KMLOC) = G_NDGLU(KM)
      AOFFSETS(KMLOC) = IIN_STRIDES0*D_OFFSETS_GEMM2(KMLOC)
      BOFFSETS(KMLOC) = D%OFFSETS_GEMM_MATRIX(KMLOC)
      COFFSETS(KMLOC) = IOUT_STRIDES0*D_OFFSETS_GEMM1(KMLOC)
    ENDDO
    IF(IMLOC0(1) > 0) THEN
      NS(IMLOC0(1)) = 0
      KS(IMLOC0(1)) = 0
    ENDIF
#ifdef OMPGPU
      !$OMP TARGET DATA USE_DEVICE_ADDR(ZAA)
#endif
#ifdef ACCGPU
      !$ACC HOST_DATA USE_DEVICE(ZAA,ZINP,ZOUTA)
#endif
    CALL HIP_GEMM( &
        & NCUR_RESOL, 11, & ! unique identifier
        & 'N', 'T', &
        & 2*KF_LEG, NS(:), KS(:), &
        & 1.0_JPRBT, &
        & C_LOC(ZINP), IIN_STRIDES0, AOFFSETS, &
        & C_LOC(ZAA), D%LEGENDRE_MATRIX_STRIDES, BOFFSETS, &
        & 0.0_JPRBT, &
        & C_LOC(ZOUTA), IOUT_STRIDES0, COFFSETS, &
        & D_NUMP, HIP_STREAM, C_LOC(ALLOCATOR%PTR))
#ifdef ACCGPU
      !$ACC END HOST_DATA
#endif
#ifdef OMPGPU
      !$OMP END TARGET DATA
#endif

    IF (LSYNC_TRANS) THEN
#ifdef ACCGPU
      !$ACC WAIT(1)
#endif
      CALL GSTATS(444,0)
      CALL MPL_BARRIER(MPL_ALL_MS_COMM,CDSTRING='')
      CALL GSTATS(444,1)
    ENDIF
    CALL GSTATS(424,1)

    ! 2. +++++++++++++ symmetric
    !IF KM=0 and NSMAX is 6:
    !    IS=2
    !    DO=1,4
    !       PIA_2=2+1+(0..3)*2 ... 3+(0..3)*2 ... 3,5,7,9
    !IF KM=0 and NSMAX is 7:
    !    IS=1
    !    DO=1,5
    !       PIA_2=1+1+(1..5-1)*2 ...2+(0..4)*2 .... 2,4,6,8,10

    CALL LEINV_LOAD_SYM(PIA,ZINP_C(1),SIZE(ZINP,KIND=JPIB),ZINP0_C(1),SIZE(ZINP0), &
      &                 D_MYMS,D_OFFSETS_GEMM2,D_NUMP, &
      &                 KF_LEG,R_NSMAX,IIN_STRIDES0,IIN0_STRIDES0)

    IF (LSYNC_TRANS) THEN
#ifdef ACCGPU
      !$ACC WAIT(1)
#endif
      CALL GSTATS(440,0)
      CALL MPL_BARRIER(MPL_ALL_MS_COMM,CDSTRING='')
      CALL GSTATS(440,1)
    ENDIF
    CALL GSTATS(424,0)

    IF (IMLOC0(1) > 0) THEN
#ifdef OMPGPU
      !$OMP TARGET DATA USE_DEVICE_ADDR(ZAS0)
#endif
#ifdef ACCGPU
      !$ACC HOST_DATA USE_DEVICE(ZAS0,ZINP0,ZOUTS0)
#endif
      CALL HIP_DGEMM_BATCHED( &
        & 'N', 'T', &
        & KF_LEG, G_NDGLU(0), (R_NSMAX+3)/2, &
        & 1.0_JPRD, &
        & C_LOC(ZINP0), IIN0_STRIDES0, 0, &
        & C_LOC(ZAS0), SIZE(ZAS0,1), 0, &
        & 0.0_JPRD, &
        & C_LOC(ZOUTS0), IOUT0_STRIDES0, 0, &
        & 1, HIP_STREAM, C_LOC(ALLOCATOR%PTR))
#ifdef ACCGPU
      !$ACC END HOST_DATA
#endif
#ifdef OMPGPU
      !$OMP END TARGET DATA
#endif
    ENDIF

    DO KMLOC=1,D_NUMP
      KM = D_MYMS(KMLOC)
      KS(KMLOC) = (R_NSMAX-KM+3)/2
      NS(KMLOC) = G_NDGLU(KM)
      AOFFSETS(KMLOC) = IIN_STRIDES0*D_OFFSETS_GEMM2(KMLOC)
      BOFFSETS(KMLOC) = D%OFFSETS_GEMM_MATRIX(KMLOC)
      COFFSETS(KMLOC) = IOUT_STRIDES0*D_OFFSETS_GEMM1(KMLOC)
    ENDDO
    IF(IMLOC0(1) > 0) THEN
      NS(IMLOC0(1)) = 0
      KS(IMLOC0(1)) = 0
    ENDIF
#ifdef OMPGPU
    !$OMP TARGET DATA USE_DEVICE_ADDR(ZAS)
#endif
#ifdef ACCGPU
    !$ACC HOST_DATA USE_DEVICE(ZAS,ZINP,ZOUTS)
#endif
    CALL HIP_GEMM( &
      & NCUR_RESOL, 12, & ! unique identifier
      & 'N', 'T', &
      & 2*KF_LEG, NS(:), KS(:), &
      & 1.0_JPRBT, &
      & C_LOC(ZINP), IIN_STRIDES0, AOFFSETS, &
      & C_LOC(ZAS), D%LEGENDRE_MATRIX_STRIDES, BOFFSETS, &
      & 0.0_JPRBT, &
      & C_LOC(ZOUTS), IOUT_STRIDES0, COFFSETS, &
      & D_NUMP, HIP_STREAM, C_LOC(ALLOCATOR%PTR))
#ifdef ACCGPU
    !$ACC END HOST_DATA
#endif
#ifdef OMPGPU
    !$OMP END TARGET DATA
#endif

    IF (LSYNC_TRANS) THEN
#ifdef ACCGPU
      !$ACC WAIT(1)
#endif
      CALL GSTATS(444,0)
      CALL MPL_BARRIER(MPL_ALL_MS_COMM,CDSTRING='')
      CALL GSTATS(444,1)
    ENDIF
    CALL GSTATS(424,1)

#ifdef OMPGPU
    !$OMP END TARGET DATA
#endif
#ifdef ACCGPU
    !$ACC WAIT(1)

    !$ACC END DATA
#endif

    IF (LHOOK) CALL DR_HOOK('LE_DGEMM',1,ZHOOK_HANDLE)
    !     ------------------------------------------------------------------
    END ASSOCIATE
  END SUBROUTINE LEINV

  ! Bodies that load the spectral fields into the GEMM input, with explicit-shape dummy
  ! arguments. A POINTER or assumed-shape actual is described by a dope vector, and the
  ! compiler puts that dope vector in the device data environment on every launch: create map
  ! entry, copy 48-120 bytes, tear the entry down. HAS_DEVICE_ADDR does not suppress it.
  ! Explicit-shape dummies are described by their extents, which travel as FIRSTPRIVATE
  ! scalars, so nothing has to be copied.
  ! PIA is the exception and keeps its descriptor: LEINV is also called from LTINV with a
  ! rank-3 section of the field dimension, whose columns are strided by the parent leading
  ! dimension. An explicit-shape dummy would need that leading dimension, which is not
  ! recoverable from the section, so it would have to come from LEINV's callers.
  SUBROUTINE LEINV_LOAD_ANTISYM(PIA,ZINP,KINP,ZINP0,KINP0, &
    &                           KMYMS,KOFFSETS_GEMM2,KNUMP, &
    &                           KF_LEG,KNSMAX,KIN_STRIDES0,KIN0_STRIDES0)
    IMPLICIT NONE

    INTEGER(KIND=JPIB), INTENT(IN)    :: KINP
    INTEGER(KIND=JPIM), INTENT(IN)    :: KINP0, KNUMP
    INTEGER(KIND=JPIM), INTENT(IN)    :: KF_LEG, KNSMAX, KIN_STRIDES0, KIN0_STRIDES0
    REAL(KIND=JPRB),    INTENT(IN)    :: PIA(:,:,:)
    REAL(KIND=JPRBT),   INTENT(INOUT) :: ZINP(KINP)
    REAL(KIND=JPRD),    INTENT(INOUT) :: ZINP0(KINP0)
    INTEGER(KIND=JPIM), INTENT(IN)    :: KMYMS(KNUMP)
    INTEGER(KIND=JPIB), INTENT(IN)    :: KOFFSETS_GEMM2(KNUMP+1)

    INTEGER(KIND=JPIM) :: KM, KMLOC, IA, JK, J

#ifdef OMPGPU
    ! Directive incomplete -> putting more variables in SHARED() triggers internal compiler error
    ! ftn-7991: INTERNAL COMPILER ERROR:  "Too few arguments on the stack"
    !$OMP TARGET TEAMS DISTRIBUTE PARALLEL DO COLLAPSE(2) &
    !$OMP& PRIVATE(KM,IA,J) &
    !$OMP& ECTRANS_DEVICE_ADDR_CLAUSE(ZINP,ZINP0,PIA) &
    !$OMP& MAP(ECTRANS_MAP_PRESENT_ALLOC:KMYMS,KOFFSETS_GEMM2) &
    !$OMP& ECTRANS_LOOP_BOUNDS_CLAUSE(KF_LEG,KNUMP) &
    !$OMP& FIRSTPRIVATE(KNSMAX,KIN_STRIDES0,KIN0_STRIDES0)
#endif
#ifdef ACCGPU
    !$ACC PARALLEL LOOP COLLAPSE(2) PRIVATE(KM,IA,J) &
    !$ACC& PRESENT(PIA,ZINP,ZINP0,KMYMS,KOFFSETS_GEMM2) &
    !$ACC& FIRSTPRIVATE(KF_LEG,KNUMP,KNSMAX,KIN_STRIDES0,KIN0_STRIDES0) DEFAULT(NONE) &
#ifdef _CRAYFTN
    !$ACC&
#else
    !$ACC& ASYNC(1)
#endif
#endif
    DO KMLOC=1,KNUMP
      DO JK=1,2*KF_LEG
        KM =  KMYMS(KMLOC)
        IA  = 1+MOD(KNSMAX-KM+2,2)
        IF(KM /= 0)THEN
#ifdef ACCGPU
          !$ACC LOOP SEQ
#endif
          DO J=1,(KNSMAX-KM+2)/2
            ZINP(JK+(J-1)*KIN_STRIDES0+KOFFSETS_GEMM2(KMLOC)*KIN_STRIDES0)=PIA(JK,IA+1+(J-1)*2,KMLOC)
          ENDDO
          ! those are only needed with tensor cores (zinp might contain NaNs!)
#if defined(USE_CUTLASS) && defined(USE_CUTLASS_3XTF32)
          !$ACC LOOP SEQ
          DO J=(KNSMAX-KM+2)/2+1,ALIGN((KNSMAX-KM+2)/2,A)
            ZINP(JK+(J-1)*KIN_STRIDES0+KOFFSETS_GEMM2(KMLOC)*KIN_STRIDES0)=0
          ENDDO
#endif
        ELSEIF (MOD((JK-1),2) == 0) THEN
          ! every other field is sufficient because Im(KM=0) == 0
#ifdef ACCGPU
          !$ACC LOOP SEQ
#endif
          DO J=1,(KNSMAX+2)/2
            ZINP0((JK-1)/2+1+(J-1)*KIN0_STRIDES0) = PIA(JK,IA+1+(J-1)*2,KMLOC)
          ENDDO
          ! those are only needed with tensor cores (zinp might contain NaNs!)
#if defined(USE_CUTLASS) && defined(USE_CUTLASS_3XTF32)
          !$ACC LOOP SEQ
          DO J=(KNSMAX+2)/2+1,ALIGN((KNSMAX+2)/2,A)
            ZINP0((JK-1)/2+1+(J-1)*KIN0_STRIDES0) = 0
          ENDDO
#endif
        ENDIF
      ENDDO
    ENDDO
  END SUBROUTINE LEINV_LOAD_ANTISYM

  SUBROUTINE LEINV_LOAD_SYM(PIA,ZINP,KINP,ZINP0,KINP0, &
    &                       KMYMS,KOFFSETS_GEMM2,KNUMP, &
    &                       KF_LEG,KNSMAX,KIN_STRIDES0,KIN0_STRIDES0)
    IMPLICIT NONE

    INTEGER(KIND=JPIB), INTENT(IN)    :: KINP
    INTEGER(KIND=JPIM), INTENT(IN)    :: KINP0, KNUMP
    INTEGER(KIND=JPIM), INTENT(IN)    :: KF_LEG, KNSMAX, KIN_STRIDES0, KIN0_STRIDES0
    REAL(KIND=JPRB),    INTENT(IN)    :: PIA(:,:,:)
    REAL(KIND=JPRBT),   INTENT(INOUT) :: ZINP(KINP)
    REAL(KIND=JPRD),    INTENT(INOUT) :: ZINP0(KINP0)
    INTEGER(KIND=JPIM), INTENT(IN)    :: KMYMS(KNUMP)
    INTEGER(KIND=JPIB), INTENT(IN)    :: KOFFSETS_GEMM2(KNUMP+1)

    INTEGER(KIND=JPIM) :: KM, KMLOC, IS, JK, J

#ifdef OMPGPU
    ! Directive incomplete -> putting more variables in SHARED() triggers internal compiler error
    ! ftn-7991: INTERNAL COMPILER ERROR:  "Too few arguments on the stack"
    !$OMP TARGET TEAMS DISTRIBUTE PARALLEL DO COLLAPSE(2) &
    !$OMP& PRIVATE(KM,IS,J) &
    !$OMP& ECTRANS_DEVICE_ADDR_CLAUSE(ZINP,ZINP0,PIA) &
    !$OMP& MAP(ECTRANS_MAP_PRESENT_ALLOC:KMYMS,KOFFSETS_GEMM2) &
    !$OMP& ECTRANS_LOOP_BOUNDS_CLAUSE(KF_LEG,KNUMP) &
    !$OMP& FIRSTPRIVATE(KNSMAX,KIN_STRIDES0,KIN0_STRIDES0)
#endif
#ifdef ACCGPU
    !$ACC PARALLEL LOOP COLLAPSE(2) PRIVATE(KM,IS,J) &
    !$ACC& PRESENT(PIA,ZINP,ZINP0,KMYMS,KOFFSETS_GEMM2) &
    !$ACC& FIRSTPRIVATE(KF_LEG,KNUMP,KNSMAX,KIN_STRIDES0,KIN0_STRIDES0) DEFAULT(NONE) &
#ifndef _CRAYFTN
    !$ACC& ASYNC(1)
#else
    !$ACC&
#endif
#endif
    DO KMLOC=1,KNUMP
      DO JK=1,2*KF_LEG
        KM =  KMYMS(KMLOC)
        IS  = 1+MOD(KNSMAX-KM+1,2)
        IF(KM /= 0) THEN
#ifdef ACCGPU
          !$ACC LOOP SEQ
#endif
          DO J=1,(KNSMAX-KM+3)/2
            ZINP(JK+(J-1)*KIN_STRIDES0+KOFFSETS_GEMM2(KMLOC)*KIN_STRIDES0)=PIA(JK,IS+1+(J-1)*2,KMLOC)
          ENDDO
#if defined(USE_CUTLASS) && defined(USE_CUTLASS_3XTF32)
          ! those are only needed with tensor cores (zinp might contain NaNs!)
          !$ACC LOOP SEQ
          DO J=(KNSMAX-KM+3)/2+1,ALIGN((KNSMAX-KM+3)/2,A)
            ZINP(JK+(J-1)*KIN_STRIDES0+KOFFSETS_GEMM2(KMLOC)*KIN_STRIDES0)=0
          ENDDO
#endif
        ELSEIF (MOD((JK-1),2) == 0) THEN
#ifdef ACCGPU
          !$ACC LOOP SEQ
#endif
          DO J=1,(KNSMAX+3)/2
            ZINP0((JK-1)/2+1+(J-1)*KIN0_STRIDES0) = PIA(JK,IS+1+(J-1)*2,KMLOC)
          ENDDO
          ! those are only needed with tensor cores (zinp might contain NaNs!)
#if defined(USE_CUTLASS) && defined(USE_CUTLASS_3XTF32)
          !$ACC LOOP SEQ
          DO J=(KNSMAX+3)/2+1,ALIGN((KNSMAX+3)/2,A)
            ZINP0((JK-1)/2+1+(J-1)*KIN0_STRIDES0) = 0
          ENDDO
#endif
        ENDIF
      ENDDO
    ENDDO
  END SUBROUTINE LEINV_LOAD_SYM
END MODULE LEINV_MOD
