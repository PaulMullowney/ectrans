#define ALIGN(I, A) (((I)+(A)-1)/(A)*(A))
! (C) Copyright 1995- ECMWF.
! (C) Copyright 1995- Meteo-France.
! (C) Copyright 2022- NVIDIA.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!

MODULE TRLTOG_MOD
  USE BUFFERED_ALLOCATOR_MOD, ONLY: ALLOCATION_RESERVATION_HANDLE
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: TRLTOG, TRLTOG_HANDLE, PREPARE_TRLTOG

  TYPE TRLTOG_HANDLE
    TYPE(ALLOCATION_RESERVATION_HANDLE) :: HCOMBUFR_AND_COMBUFS
  END TYPE
CONTAINS
  FUNCTION PREPARE_TRLTOG(ALLOCATOR,KF_FS,KF_GP) RESULT(HTRLTOG)
    USE PARKIND_ECTRANS,        ONLY: JPIM, JPRBT, JPIB
    USE TPM_DISTR,              ONLY: D
    USE BUFFERED_ALLOCATOR_MOD, ONLY: BUFFERED_ALLOCATOR, RESERVE
    USE ISO_C_BINDING,          ONLY: C_SIZEOF

    IMPLICIT NONE

    TYPE(BUFFERED_ALLOCATOR), INTENT(INOUT) :: ALLOCATOR
    INTEGER(KIND=JPIM), INTENT(IN) :: KF_GP, KF_FS
    TYPE(TRLTOG_HANDLE) :: HTRLTOG

    REAL(KIND=JPRBT) :: DUMMY

    INTEGER(KIND=JPIB) :: NELEM

    NELEM = 0
    NELEM = NELEM + ALIGN(1_JPIB*KF_GP*D%NGPTOT*C_SIZEOF(DUMMY),128) ! ZCOMBUFR
    NELEM = NELEM + ALIGN(1_JPIB*KF_FS*D%NLENGTF*C_SIZEOF(DUMMY),128) !ZCOMBUFS upper obund

    HTRLTOG%HCOMBUFR_AND_COMBUFS = RESERVE(ALLOCATOR, NELEM, "HTRLTOG%HCOMBUFR_AND_COMBUFS")
  END FUNCTION PREPARE_TRLTOG

  SUBROUTINE TRLTOG(ALLOCATOR,HTRLTOG,PREEL_REAL,KF_FS,KF_GP,KF_UV_G,KF_SCALARS_G,KPTRGP,&
     & KVSETUV,KVSETSC,KVSETSC3A,KVSETSC3B,KVSETSC2,&
     & PGP,PGPUV,PGP3A,PGP3B,PGP2,LPGP_ON_GPU)

    !**** *trltog * - transposition of grid point data from latitudinal
    !   to column structure. This takes place between inverse
    !                 FFT and grid point calculations.
    !                 TRLTOG is the inverse of TRGTOL

    ! Version using CUDA-aware MPI

    !     Purpose.
    !     --------


    !**   Interface.
    !     ----------
    !        *call* *trltog(...)

    !        Explicit arguments :
    !        --------------------
    !           PREEL_REAL    -  Latitudinal data ready for direct FFT (input)
    !           PGP    -  Blocked grid point data    (output)
    !           KVSET    - "v-set" for each field      (input)

    !        Implicit arguments :
    !        --------------------

    !     Method.
    !     -------
    !        See documentation

    !     Externals.
    !     ----------

    !     Reference.
    !     ----------
    !        ECMWF Research Department documentation of the IFS

    !     Author.
    !     -------
    !        MPP Group *ECMWF*

    !     Modifications.
    !     --------------
    !        Original  : 95-10-01
    !        D.Dent    : 97-08-04 Reorganisation to allow NPRTRV
    !                             to differ from NPRGPEW
    !        =99-03-29= Mats Hamrud and Deborah Salmond
    !                   JUMP in FFT's changed to 1
    !                   INDEX introduced and ZCOMBUF not used for same PE
    !         01-11-23  Deborah Salmond and John Hague
    !                   LIMP_NOOLAP Option for non-overlapping message passing
    !                               and buffer packing
    !         01-12-18  Peter Towers
    !                   Improved vector performance of LTOG_PACK,LTOG_UNPACK
    !         03-0-02   G. Radnoti: Call barrier always when nproc>1
    !         08-01-01  G.Mozdzynski: cleanup
    !         09-01-02  G.Mozdzynski: use non-blocking recv and send
    !     ------------------------------------------------------------------

    USE PARKIND_ECTRANS,        ONLY: JPIM, JPRB, JPRBT, JPIB
    USE YOMHOOK,                ONLY: LHOOK, DR_HOOK, JPHOOK
    USE MPL_MODULE,             ONLY: MPL_WAIT, MPL_BARRIER, MPL_ABORT, MPL_RECV, MPL_SEND
    USE TPM_GEN,                ONLY: LSYNC_TRANS, NERR, LMPOFF
    USE EQ_REGIONS_MOD,         ONLY: MY_REGION_EW, MY_REGION_NS
    USE TPM_DISTR,              ONLY: D,MYSETV, MYSETW, MTAGLG,NPRCIDS,MYPROC,NPROC,NPRTRW,NPRTRV
    USE PE2SET_MOD,             ONLY: PE2SET
    USE MPL_DATA_MODULE,        ONLY: MPL_COMM_OML, JP_NON_BLOCKING_STANDARD
    USE OML_MOD,                ONLY: OML_MY_THREAD
    USE ABORT_TRANS_MOD,        ONLY: ABORT_TRANS
#ifdef USE_RAW_MPI
    USE MPI_F08,                ONLY: MPI_COMM, MPI_REQUEST, MPI_REAL4, MPI_REAL8
    ! Missing: MPI_ISEND, MPI_IRECV on purpose due to cray-mpi bug (see https://github.com/ecmwf-ifs/ectrans/pull/157)
#endif
    USE TPM_STATS,              ONLY: GSTATS => GSTATS_NVTX
    USE TPM_TRANS,              ONLY: LDIVGP, LSCDERS, LUVDER, LVORGP, NPROMA
    USE BUFFERED_ALLOCATOR_MOD, ONLY: BUFFERED_ALLOCATOR, ASSIGN_PTR, GET_ALLOCATION
    USE ISO_C_BINDING,          ONLY: C_SIZEOF
    USE OPENACC_EXT,            ONLY: EXT_ACC_ARR_DESC, EXT_ACC_PASS, EXT_ACC_CREATE, &
      &                               EXT_ACC_DELETE
#ifdef ACCGPU
    USE OPENACC,                ONLY: ACC_HANDLE_KIND
#endif

    IMPLICIT NONE

    REAL(KIND=JPRBT),  INTENT(INOUT), POINTER  :: PREEL_REAL(:)
    INTEGER(KIND=JPIM),INTENT(IN)  :: KF_FS,KF_GP
    INTEGER(KIND=JPIM),INTENT(IN)  :: KF_UV_G, KF_SCALARS_G
    INTEGER(KIND=JPIM) ,OPTIONAL, INTENT(IN) :: KPTRGP(:)
    REAL(KIND=JPRB),OPTIONAL,INTENT(OUT)     :: PGP(:,:,:)
    ! CONTIGUOUS so that handing these to the explicit-shape dummies of TRLTOG_UNPACK_RECV
    ! passes a base address. Without it the compiler must allow for a non-contiguous actual
    ! and would pack into a temporary -- a host-side copy of device-resident data.
    REAL(KIND=JPRB),OPTIONAL,INTENT(OUT),CONTIGUOUS :: PGPUV(:,:,:,:)
    REAL(KIND=JPRB),OPTIONAL,INTENT(OUT),CONTIGUOUS :: PGP3A(:,:,:,:)
    REAL(KIND=JPRB),OPTIONAL,INTENT(OUT),CONTIGUOUS :: PGP3B(:,:,:,:)
    REAL(KIND=JPRB),OPTIONAL,INTENT(OUT),CONTIGUOUS :: PGP2(:,:,:)
    LOGICAL, OPTIONAL, INTENT(IN)            :: LPGP_ON_GPU
    INTEGER(KIND=JPIM) ,OPTIONAL, INTENT(IN) :: KVSETUV(:)
    INTEGER(KIND=JPIM) ,OPTIONAL, INTENT(IN) :: KVSETSC(:)
    INTEGER(KIND=JPIM) ,OPTIONAL, INTENT(IN) :: KVSETSC3A(:)
    INTEGER(KIND=JPIM) ,OPTIONAL, INTENT(IN) :: KVSETSC3B(:)
    INTEGER(KIND=JPIM) ,OPTIONAL, INTENT(IN) :: KVSETSC2(:)

    TYPE(BUFFERED_ALLOCATOR), INTENT(IN) :: ALLOCATOR
    TYPE(TRLTOG_HANDLE) :: HTRLTOG

    ! LOCAL VARIABLES

    REAL(KIND=JPRBT), POINTER :: ZCOMBUFS(:),ZCOMBUFR(:)

    ! An element of a non-CONTIGUOUS pointer array may not be sequence-associated with an
    ! explicit-shape dummy (F2018 15.5.2.4), so the pack/unpack bodies are handed these
    ! aliases instead. They address the same allocator slab, which is contiguous by
    ! construction.
    REAL(KIND=JPRBT), POINTER, CONTIGUOUS :: ZPREEL_C(:),ZCOMBUFS_C(:),ZCOMBUFR_C(:)

    ! Extents of the optional gridpoint arrays, so the unpack body can take them as
    ! explicit-shape dummies. Left at 1 when the corresponding array is absent.
    INTEGER(KIND=JPIM) :: IUV1,IUV2,IUV3,IUV4, IG21,IG22,IG23
    INTEGER(KIND=JPIM) :: I3A1,I3A2,I3A3,I3A4, I3B1,I3B2,I3B3,I3B4

    LOGICAL :: LLOCAL_CONTRIBUTION
    INTEGER(KIND=JPIB) :: ISENDTOT (NPROC)
    INTEGER(KIND=JPIB) :: IRECVTOT (NPROC)
    INTEGER(KIND=JPIM) :: ISENDTOT_MPI(NPROC)
    INTEGER(KIND=JPIM) :: IRECVTOT_MPI(NPROC)
    INTEGER(KIND=JPIM) :: IREQ     (NPROC*2)
    INTEGER(KIND=JPIM) :: IRECV_TO_PROC(NPROC)
    INTEGER(KIND=JPIM) :: ISEND_TO_PROC(NPROC)

    INTEGER(KIND=JPIM) :: JFLD, J, JI, JGL, JK, JL, IFLDS, JROC, INR, INS
    INTEGER(KIND=JPIM) :: IFIRSTLAT, ILASTLAT, IFLD, IGL, IGLL,&
                 &ISETA, ISETB, ISETV, ISEND, IRECV, ISETW, IPROC, &
                 &IR, ILOCAL_LAT, ISEND_COUNTS, IRECV_COUNTS, IERROR, II, ILEN, &
                 &JBLK, ILAT_STRIP
    INTEGER(KIND=JPIB) :: IPOS

    ! Contains FIELD, PARS, LEVS
    INTEGER(KIND=JPIM) :: IGP_OFFSETS(KF_GP,3)
    INTEGER(KIND=JPIM), PARAMETER :: IGP_OFFSETS_UV=1, IGP_OFFSETS_GP2=2, IGP_OFFSETS_GP3A=3, IGP_OFFSETS_GP3B=4
    INTEGER(KIND=JPIM) :: IUVPAR,IGP2PAR,IGP3ALEV,IGP3APAR,IGP3BLEV,IGP3BPAR,IOFF

    ! Offset of the current task's first point in the D%NGP_* index tables.
    INTEGER(KIND=JPIM) :: IGP_V
    INTEGER(KIND=JPIM) :: IRECV_FIELD_COUNT(NPRTRV),IRECV_FIELD_COUNT_V
    INTEGER(KIND=JPIM) :: IRECV_WSET_SIZE(NPRTRW),IRECV_WSET_SIZE_V
    INTEGER(KIND=JPIM) :: IRECV_WSET_OFFSET(NPRTRW+1), IRECV_WSET_OFFSET_V
    INTEGER(KIND=JPIB), ALLOCATABLE :: ICOMBUFS_OFFSET(:),ICOMBUFR_OFFSET(:), IFLDA(:,:)
    INTEGER(KIND=JPIB) :: ICOMBUFS_OFFSET_V, ICOMBUFR_OFFSET_V

    INTEGER(KIND=JPIM) :: IVSETUV(KF_UV_G)
    INTEGER(KIND=JPIM) :: IVSETSC(KF_SCALARS_G)
    INTEGER(KIND=JPIM) :: IVSET(KF_GP)
    INTEGER(KIND=JPIM) :: J3

    REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

    TYPE(EXT_ACC_ARR_DESC) :: ACC_POINTERS(5) ! at most 5 copyins...
    INTEGER(KIND=JPIM) :: ACC_POINTERS_CNT
    LOGICAL :: LLPGP_ON_GPU

#ifdef USE_RAW_MPI
    TYPE(MPI_COMM) :: LOCAL_COMM
    TYPE(MPI_REQUEST) :: IREQUEST(NPROC*2)
#else
    INTEGER(KIND=JPIM) :: IREQUEST(NPROC*2)
#endif

#ifdef PARKINDTRANS_SINGLE
#define TRLTOG_DTYPE MPI_REAL4
#else
#define TRLTOG_DTYPE MPI_REAL8
#endif
#ifdef USE_RAW_MPI
    IF(.NOT. LMPOFF) THEN
      LOCAL_COMM%MPI_VAL = MPL_COMM_OML( OML_MY_THREAD() )
    ENDIF
#endif
    !     ------------------------------------------------------------------

    !*       0.    Some initializations
    !              --------------------
    IF (LHOOK) CALL DR_HOOK('TRLTOG',0,ZHOOK_HANDLE)

    ! Bounds of the optional gridpoint arrays, for the explicit-shape dummies of the
    ! local-contribution and unpack kernels. Absent arrays get extent 1 so that the
    ! dummy is still well formed; the callee guards every use with PRESENT.
    IUV1=1; IUV2=1; IUV3=1; IUV4=1
    IG21=1; IG22=1; IG23=1
    I3A1=1; I3A2=1; I3A3=1; I3A4=1
    I3B1=1; I3B2=1; I3B3=1; I3B4=1
    IF (PRESENT(PGPUV)) THEN
      IUV1=SIZE(PGPUV,1); IUV2=SIZE(PGPUV,2); IUV3=SIZE(PGPUV,3); IUV4=SIZE(PGPUV,4)
    ENDIF
    IF (PRESENT(PGP2)) THEN
      IG21=SIZE(PGP2,1); IG22=SIZE(PGP2,2); IG23=SIZE(PGP2,3)
    ENDIF
    IF (PRESENT(PGP3A)) THEN
      I3A1=SIZE(PGP3A,1); I3A2=SIZE(PGP3A,2); I3A3=SIZE(PGP3A,3); I3A4=SIZE(PGP3A,4)
    ENDIF
    IF (PRESENT(PGP3B)) THEN
      I3B1=SIZE(PGP3B,1); I3B2=SIZE(PGP3B,2); I3B3=SIZE(PGP3B,3); I3B4=SIZE(PGP3B,4)
    ENDIF
    ZPREEL_C => PREEL_REAL

    ! Note we have either
    ! - KVSETUV and KVSETSC (with PGP, which has u, v, and scalar fields), or
    ! - KVSETUV, KVSETSC2, KVSETSC3A KVSETSC3B (with PGPUV, GP3A, PGP3B and PGP2)
    ! KVSETs are optionals. Their sizes canalso be inferred from KV_UV_G/KV_SCALARS_G (which
    ! should match PSPXXX and PGPXXX arrays)


    ! We first get the decomposition individually
    IVSETUV(:) = -1
    IF (PRESENT(KVSETUV)) IVSETUV(:) = KVSETUV(:)
    IVSETSC(:)=-1
    IF (PRESENT(KVSETSC)) THEN
      IVSETSC(:) = KVSETSC(:)
    ELSE
      IOFF=0
      IF (PRESENT(KVSETSC2)) THEN
        IVSETSC(IOFF+1:IOFF+SIZE(KVSETSC2))=KVSETSC2(:)
        IOFF = IOFF+SIZE(KVSETSC2)
      ENDIF
      IF (PRESENT(KVSETSC3A)) THEN
        DO J3=1,MERGE(UBOUND(PGP3A,3),UBOUND(PGP3A,3)/3,.NOT. LSCDERS)
          IVSETSC(IOFF+1:IOFF+SIZE(KVSETSC3A))=KVSETSC3A(:)
          IOFF=IOFF+SIZE(KVSETSC3A)
        ENDDO
      ENDIF
      IF (PRESENT(KVSETSC3B)) THEN
        ! If SCDERS is on, the size of PGP is 3X larger because it is
        ! holding various derivatives. The problem is that those are
        ! at different non-contiguous positions, hence we treat them
        ! as separate fields
        DO J3=1,MERGE(UBOUND(PGP3B,3),UBOUND(PGP3B,3)/3,.NOT. LSCDERS)
          IVSETSC(IOFF+1:IOFF+SIZE(KVSETSC3B))=KVSETSC3B(:)
          IOFF=IOFF+SIZE(KVSETSC3B)
        ENDDO
      ENDIF
      IF (IOFF > 0 .AND. IOFF /= KF_SCALARS_G ) THEN
        CALL ABORT_TRANS("TRLTOG: Error in IVSETSC computation")
      ENDIF
    ENDIF

    ! Now from UV and Scalars decomposition we get the full decomposition
    IOFF=0
    IF (KF_UV_G > 0) THEN
      IF (LVORGP) THEN
        IVSET(IOFF+1:IOFF+KF_UV_G) = IVSETUV(:)
        IOFF=IOFF+KF_UV_G
      ENDIF
      IF ( LDIVGP) THEN
        IVSET(IOFF+1:IOFF+KF_UV_G) = IVSETUV(:)
        IOFF=IOFF+KF_UV_G
      ENDIF
      IVSET(IOFF+1:IOFF+KF_UV_G) = IVSETUV(:)
      IOFF=IOFF+KF_UV_G
      IVSET(IOFF+1:IOFF+KF_UV_G) = IVSETUV(:)
      IOFF=IOFF+KF_UV_G
    ENDIF
    IF (KF_SCALARS_G > 0) THEN
      IVSET(IOFF+1:IOFF+KF_SCALARS_G) = IVSETSC(:)
      IOFF=IOFF+KF_SCALARS_G
      IF (LSCDERS) THEN
        IVSET(IOFF+1:IOFF+KF_SCALARS_G) = IVSETSC(:)
        IOFF=IOFF+KF_SCALARS_G
      ENDIF
    ENDIF
    IF (KF_UV_G > 0 .AND. LUVDER) THEN
      IVSET(IOFF+1:IOFF+KF_UV_G) = IVSETUV(:)
      IOFF=IOFF+KF_UV_G
      IVSET(IOFF+1:IOFF+KF_UV_G) = IVSETUV(:)
      IOFF=IOFF+KF_UV_G
    ENDIF
    IF (KF_SCALARS_G > 0) THEN
      IF (LSCDERS) THEN
        IVSET(IOFF+1:IOFF+KF_SCALARS_G) = IVSETSC(:)
        IOFF=IOFF+KF_SCALARS_G
      ENDIF
    ENDIF

    LLPGP_ON_GPU = .FALSE.
    IF (PRESENT(LPGP_ON_GPU)) LLPGP_ON_GPU = LPGP_ON_GPU

    IF (.NOT. PRESENT(PGP)) THEN
      ! This is only relevant if we use the split interface (i.e. not PGP)

      IGP2PAR = 0
      IGP3APAR = 0
      IGP3ALEV = 0
      IGP3BPAR = 0
      IGP3BLEV = 0
      IF (PRESENT(PGP2)) THEN
        IGP2PAR = UBOUND(PGP2,2)
        IF(LSCDERS) IGP2PAR = IGP2PAR/3
      ENDIF
      IF (PRESENT(PGP3A)) THEN
        IGP3ALEV = UBOUND(PGP3A,2)
        IGP3APAR = UBOUND(PGP3A,3)
        IF(LSCDERS) IGP3APAR = IGP3APAR/3
      ENDIF
      IF (PRESENT(PGP3B)) THEN
        IGP3BLEV = UBOUND(PGP3B,2)
        IGP3BPAR = UBOUND(PGP3B,3)
        IF(LSCDERS) IGP3BPAR = IGP3BPAR/3
      ENDIF
      IF (IGP2PAR + IGP3ALEV*IGP3APAR + IGP3BPAR*IGP3BLEV /= KF_SCALARS_G) THEN
        WRITE(NERR,*) IGP2PAR, IGP3APAR, IGP3ALEV, IGP3BPAR, IGP3BLEV
        CALL ABORT_TRANS("INCONSISTENCY IN SCALARS")
      ENDIF

      ! This is only relevant if we use the split interface (i.e. not PGP)
      IUVPAR = 1
      IOFF=1
      IF(LVORGP) THEN
        IGP_OFFSETS(IOFF:IOFF+KF_UV_G-1,1) = IGP_OFFSETS_UV
        IGP_OFFSETS(IOFF:IOFF+KF_UV_G-1,2) = IUVPAR
        IGP_OFFSETS(IOFF:IOFF+KF_UV_G-1,3) = (/(J, J=1,KF_UV_G)/)
        IUVPAR=IUVPAR+1
        IOFF=IOFF+KF_UV_G
      ENDIF

      IF(LDIVGP) THEN
        IGP_OFFSETS(IOFF:IOFF+KF_UV_G-1,1) = IGP_OFFSETS_UV
        IGP_OFFSETS(IOFF:IOFF+KF_UV_G-1,2) = IUVPAR
        IGP_OFFSETS(IOFF:IOFF+KF_UV_G-1,3) = (/(J, J=1,KF_UV_G)/)
        IUVPAR=IUVPAR+1
        IOFF=IOFF+KF_UV_G
      ENDIF

      ! U
      IGP_OFFSETS(IOFF:IOFF+KF_UV_G-1,1) = IGP_OFFSETS_UV
      IGP_OFFSETS(IOFF:IOFF+KF_UV_G-1,2) = IUVPAR
      IGP_OFFSETS(IOFF:IOFF+KF_UV_G-1,3) = (/(J, J=1,KF_UV_G)/)
      IUVPAR=IUVPAR+1
      IOFF=IOFF+KF_UV_G

      ! V
      IGP_OFFSETS(IOFF:IOFF+KF_UV_G-1,1) = IGP_OFFSETS_UV
      IGP_OFFSETS(IOFF:IOFF+KF_UV_G-1,2) = IUVPAR
      IGP_OFFSETS(IOFF:IOFF+KF_UV_G-1,3) = (/(J, J=1,KF_UV_G)/)
      IUVPAR=IUVPAR+1
      IOFF=IOFF+KF_UV_G

      ! Scalars
      ! PGP2
      IGP_OFFSETS(IOFF:IOFF+IGP2PAR-1,1) = IGP_OFFSETS_GP2
      IGP_OFFSETS(IOFF:IOFF+IGP2PAR-1,2) = (/(J, J=1,IGP2PAR)/)
      IOFF=IOFF+IGP2PAR
      ! PGP3A
      IGP_OFFSETS(IOFF:IOFF+IGP3APAR*IGP3ALEV-1,1) = IGP_OFFSETS_GP3A
      IGP_OFFSETS(IOFF:IOFF+IGP3APAR*IGP3ALEV-1,2) = (/(1+J/IGP3ALEV, J=0,IGP3APAR*IGP3ALEV-1)/)
      IGP_OFFSETS(IOFF:IOFF+IGP3APAR*IGP3ALEV-1,3) = (/(1+MOD(J,IGP3ALEV), J=0,IGP3APAR*IGP3ALEV-1)/)
      IOFF=IOFF+IGP3APAR*IGP3ALEV
      ! PGP3B
      IGP_OFFSETS(IOFF:IOFF+IGP3BPAR*IGP3BLEV-1,1) = IGP_OFFSETS_GP3B
      IGP_OFFSETS(IOFF:IOFF+IGP3BPAR*IGP3BLEV-1,2) = (/(1+J/IGP3BLEV, J=0,IGP3BPAR*IGP3BLEV-1)/)
      IGP_OFFSETS(IOFF:IOFF+IGP3BPAR*IGP3BLEV-1,3) = (/(1+MOD(J,IGP3BLEV), J=0,IGP3BPAR*IGP3BLEV-1)/)
      IOFF=IOFF+IGP3BPAR*IGP3BLEV

      IF(LSCDERS) THEN
        !Scalars NS Derivatives
        ! PGP2
        IGP_OFFSETS(IOFF:IOFF+IGP2PAR-1,1) = IGP_OFFSETS_GP2
        IGP_OFFSETS(IOFF:IOFF+IGP2PAR-1,2) = (/(J+IGP2PAR, J=1,IGP2PAR)/)
        IOFF=IOFF+IGP2PAR
        ! PGP3A
        IGP_OFFSETS(IOFF:IOFF+IGP3APAR*IGP3ALEV-1,1) = IGP_OFFSETS_GP3A
        IGP_OFFSETS(IOFF:IOFF+IGP3APAR*IGP3ALEV-1,2) = (/(1+IGP3APAR+J/IGP3ALEV, J=0,IGP3APAR*IGP3ALEV-1)/)
        IGP_OFFSETS(IOFF:IOFF+IGP3APAR*IGP3ALEV-1,3) = (/(1+MOD(J,IGP3ALEV), J=0,IGP3APAR*IGP3ALEV-1)/)
        IOFF=IOFF+IGP3APAR*IGP3ALEV
        ! PGP3B
        IGP_OFFSETS(IOFF:IOFF+IGP3BPAR*IGP3BLEV-1,1) = IGP_OFFSETS_GP3B
        IGP_OFFSETS(IOFF:IOFF+IGP3BPAR*IGP3BLEV-1,2) = (/(1+IGP3BPAR+J/IGP3BLEV, J=0,IGP3BPAR*IGP3BLEV-1)/)
        IGP_OFFSETS(IOFF:IOFF+IGP3BPAR*IGP3BLEV-1,3) = (/(1+MOD(J,IGP3BLEV), J=0,IGP3BPAR*IGP3BLEV-1)/)
        IOFF=IOFF+IGP3BPAR*IGP3BLEV
      ENDIF

      IF(LUVDER) THEN
        ! U Derivative NS
        IGP_OFFSETS(IOFF:IOFF+KF_UV_G-1,1) = IGP_OFFSETS_UV
        IGP_OFFSETS(IOFF:IOFF+KF_UV_G-1,2) = IUVPAR
        IGP_OFFSETS(IOFF:IOFF+KF_UV_G-1,3) = (/(J, J=1,KF_UV_G)/)
        IUVPAR=IUVPAR+1
        IOFF=IOFF+KF_UV_G

        ! V Derivative NS
        IGP_OFFSETS(IOFF:IOFF+KF_UV_G-1,1) = IGP_OFFSETS_UV
        IGP_OFFSETS(IOFF:IOFF+KF_UV_G-1,2) = IUVPAR
        IGP_OFFSETS(IOFF:IOFF+KF_UV_G-1,3) = (/(J, J=1,KF_UV_G)/)
        IUVPAR=IUVPAR+1
        IOFF=IOFF+KF_UV_G
      ENDIF

      IF(LSCDERS) THEN
        !Scalars NS Derivatives
        ! PGP2
        IGP_OFFSETS(IOFF:IOFF+IGP2PAR-1,1) = IGP_OFFSETS_GP2
        IGP_OFFSETS(IOFF:IOFF+IGP2PAR-1,2) = (/(J+2*IGP2PAR, J=1,IGP2PAR)/)
        IOFF=IOFF+IGP2PAR
        ! PGP3A
        IGP_OFFSETS(IOFF:IOFF+IGP3APAR*IGP3ALEV-1,1) = IGP_OFFSETS_GP3A
        IGP_OFFSETS(IOFF:IOFF+IGP3APAR*IGP3ALEV-1,2) = (/(1+2*IGP3APAR+J/IGP3ALEV, J=0,IGP3APAR*IGP3ALEV-1)/)
        IGP_OFFSETS(IOFF:IOFF+IGP3APAR*IGP3ALEV-1,3) = (/(1+MOD(J,IGP3ALEV), J=0,IGP3APAR*IGP3ALEV-1)/)
        IOFF=IOFF+IGP3APAR*IGP3ALEV
        ! PGP3B
        IGP_OFFSETS(IOFF:IOFF+IGP3BPAR*IGP3BLEV-1,1) = IGP_OFFSETS_GP3B
        IGP_OFFSETS(IOFF:IOFF+IGP3BPAR*IGP3BLEV-1,2) = (/(1+2*IGP3BPAR+J/IGP3BLEV, J=0,IGP3BPAR*IGP3BLEV-1)/)
        IGP_OFFSETS(IOFF:IOFF+IGP3BPAR*IGP3BLEV-1,3) = (/(1+MOD(J,IGP3BLEV), J=0,IGP3BPAR*IGP3BLEV-1)/)
        IOFF=IOFF+IGP3BPAR*IGP3BLEV
      ENDIF
    ENDIF

    CALL GSTATS(1806,0)

    ! Prepare receiver arrays
    ! find number of fields on a certain V-set
    IF(NPRTRV == 1) THEN
      ! This is needed because KVSET(JFLD) == -1 if there is only one V-set
      IRECV_FIELD_COUNT(1) = KF_GP
    ELSE
      IRECV_FIELD_COUNT(:) = 0
      DO JFLD=1,KF_GP
        IRECV_FIELD_COUNT(IVSET(JFLD)) = IRECV_FIELD_COUNT(IVSET(JFLD)) + 1
      ENDDO
    ENDIF
    ! find number of grid-points on a certain W-set that overlap with myself
    IRECV_WSET_SIZE(:) = 0
    DO ILOCAL_LAT=D%NFRSTLAT(MY_REGION_NS),D%NLSTLAT(MY_REGION_NS)
      ILAT_STRIP = ILOCAL_LAT-D%NFRSTLAT(MY_REGION_NS)+D%NPTRFLOFF+1
      IRECV_WSET_SIZE(D%NPROCL(ILOCAL_LAT)) = &
          & IRECV_WSET_SIZE(D%NPROCL(ILOCAL_LAT))+D%NONL(ILAT_STRIP,MY_REGION_EW)
    ENDDO
    ! sum up offsets
    IRECV_WSET_OFFSET(1) = 0
    DO JROC=1,NPRTRW
      IRECV_WSET_OFFSET(JROC+1)=IRECV_WSET_OFFSET(JROC)+IRECV_WSET_SIZE(JROC)
    ENDDO
    DO JROC=1,NPROC
      CALL PE2SET(JROC,ISETA,ISETB,ISETW,ISETV)
      ! total recv size is # points per field * # fields
      IRECVTOT(JROC) = 1_JPIB*IRECV_WSET_SIZE(ISETW)*IRECV_FIELD_COUNT(ISETV)
    ENDDO

    ! Prepare sender arrays. The per-point offsets live in D%NGP_* (built once in
    ! SUMP_TRANS); all that is needed here is each task's point count, which is the
    ! span of its slots in D%NGP_OFFSET.
    DO JROC=1,NPROC
      !we always receive the full fourier space
      ISENDTOT(JROC) = 1_JPIB*(D%NGP_OFFSET(JROC+1)-D%NGP_OFFSET(JROC))*KF_FS
    ENDDO
    LLOCAL_CONTRIBUTION = ISENDTOT(MYPROC) > 0

#ifdef OMPGPU
    !$OMP TARGET DATA MAP(TO:IGP_OFFSETS)
#endif
#ifdef ACCGPU
    !$ACC DATA COPYIN(IGP_OFFSETS) ASYNC(1)
#endif

    ACC_POINTERS_CNT = 0
    IF (PRESENT(PGP) .AND. .NOT. LLPGP_ON_GPU) THEN
      ACC_POINTERS_CNT = ACC_POINTERS_CNT + 1
      ACC_POINTERS(ACC_POINTERS_CNT) = EXT_ACC_PASS(PGP)
    ENDIF
    IF (PRESENT(PGPUV) .AND. .NOT. LLPGP_ON_GPU) THEN
      ACC_POINTERS_CNT = ACC_POINTERS_CNT + 1
      ACC_POINTERS(ACC_POINTERS_CNT) = EXT_ACC_PASS(PGPUV)
    ENDIF
    IF (PRESENT(PGP2) .AND. .NOT. LLPGP_ON_GPU) THEN
      ACC_POINTERS_CNT = ACC_POINTERS_CNT + 1
      ACC_POINTERS(ACC_POINTERS_CNT) = EXT_ACC_PASS(PGP2)
    ENDIF
    IF (PRESENT(PGP3A) .AND. .NOT. LLPGP_ON_GPU) THEN
      ACC_POINTERS_CNT = ACC_POINTERS_CNT + 1
      ACC_POINTERS(ACC_POINTERS_CNT) = EXT_ACC_PASS(PGP3A)
    ENDIF
    IF (PRESENT(PGP3B) .AND. .NOT. LLPGP_ON_GPU) THEN
      ACC_POINTERS_CNT = ACC_POINTERS_CNT + 1
      ACC_POINTERS(ACC_POINTERS_CNT) = EXT_ACC_PASS(PGP3B)
    ENDIF

    IF (ACC_POINTERS_CNT > 0) CALL EXT_ACC_CREATE(ACC_POINTERS(1:ACC_POINTERS_CNT), &
#ifdef ACCGPU
         & STREAM=1_ACC_HANDLE_KIND)
#endif
#ifdef OMPGPU
         & STREAM=1)
#endif

#ifdef OMPGPU
    ! PGP/PGPUV/PGP2/PGP3A/PGP3B are user gridpoint arrays, either already device-resident
    ! from the caller's allocator or host storage whose byte range EXT_ACC_CREATE maps here,
    ! depending on LPGP_ON_GPU. PREEL_REAL is a growing-allocator buffer. The pack/unpack
    ! compute constructs name the gridpoint arrays in MAP(ALLOC:...), which is the only form
    ! that covers both residencies and also tolerates the OPTIONAL ones the caller omits;
    ! see the longer note in TRGTOL for why neither HAS_DEVICE_ADDR nor the PRESENT modifier
    ! of ECTRANS_MAP_PRESENT_ALLOC can be used here.
#endif
#ifdef ACCGPU
    !$ACC DATA IF(PRESENT(PGP))   PRESENT(PGP) ASYNC(1)
    !$ACC DATA IF(PRESENT(PGPUV)) PRESENT(PGPUV) ASYNC(1)
    !$ACC DATA IF(PRESENT(PGP2))  PRESENT(PGP2) ASYNC(1)
    !$ACC DATA IF(PRESENT(PGP3A)) PRESENT(PGP3A) ASYNC(1)
    !$ACC DATA IF(PRESENT(PGP3B)) PRESENT(PGP3B) ASYNC(1)

    ! Present until self contribution and packing are done
    !$ACC DATA PRESENT(PREEL_REAL) IF(KF_FS > 0) ASYNC(1)
#endif

    CALL GSTATS(1806,1)
    
    ! Figure out processes that send or recv something
    ISEND_COUNTS   = 0
    IRECV_COUNTS   = 0
    DO JROC=1,NPROC
      IF( JROC /= MYPROC) THEN
        IF(IRECVTOT(JROC) > 0) THEN
          ! I have to recv something, so let me store that
          IRECV_COUNTS = IRECV_COUNTS + 1
          IRECV_TO_PROC(IRECV_COUNTS)=JROC
        ENDIF
        IF(ISENDTOT(JROC) > 0) THEN
          ! I have to send something, so let me store that
          ISEND_COUNTS = ISEND_COUNTS+1
          ISEND_TO_PROC(ISEND_COUNTS)=JROC
        ENDIF
      ENDIF
    ENDDO

    ! ... build this data structure now during the MPI communication
    ! Allocate this buffer now. Add 1 for self contribution
    ALLOCATE(IFLDA(KF_GP,1+IRECV_COUNTS))

    ! Copy local contribution
    IF(LLOCAL_CONTRIBUTION) THEN
      ! I have to send something to myself...

      ! Input is KF_GP fields. We find the resulting KF_FS fields.
      IFLDS = 0
      DO JFLD=1,KF_GP
        IF(IVSET(JFLD) == MYSETV .OR. IVSET(JFLD) == -1) THEN
          IFLDS = IFLDS+1
          IF(PRESENT(KPTRGP)) THEN
            IFLDA(IFLDS,1) = KPTRGP(JFLD)
          ELSE
            IFLDA(IFLDS,1) = JFLD
          ENDIF
        ENDIF
      ENDDO
    ENDIF

    DO INR=1,IRECV_COUNTS
      IRECV=IRECV_TO_PROC(INR)
      CALL PE2SET(IRECV,ISETA,ISETB,ISETW,ISETV)
      IFLDS = 0
      DO JFLD=1,KF_GP
        IF(IVSET(JFLD) == ISETV .OR. IVSET(JFLD) == -1 ) THEN
          IFLDS = IFLDS+1
          IF(PRESENT(KPTRGP)) THEN
            IFLDA(IFLDS,1+INR)=KPTRGP(JFLD)
          ELSE
            IFLDA(IFLDS,1+INR)=JFLD
          ENDIF
        ENDIF
      ENDDO
    ENDDO
   
#ifdef OMPGPU
    !$OMP TARGET DATA MAP(TO:IFLDA)
#endif
#ifdef ACCGPU
    !$ACC DATA COPYIN(IFLDA) ASYNC(1)
#endif

    ! Copy local contribution
    IF(LLOCAL_CONTRIBUTION) THEN

      CALL GSTATS(1604,0)
      CALL GSTATS(450,0)

      IRECV_WSET_OFFSET_V = IRECV_WSET_OFFSET(MYSETW)
      IRECV_WSET_SIZE_V = IRECV_WSET_SIZE(MYSETW)
      IGP_V = D%NGP_OFFSET(MYPROC)
      ASSOCIATE(D_NGP_A=>D%NGP_A, D_NGP_B=>D%NGP_B, D_NGP_STRIDE=>D%NGP_STRIDE)
      IF (PRESENT(PGP)) THEN
#ifdef OMPGPU
        !$OMP TARGET TEAMS DISTRIBUTE PARALLEL DO COLLAPSE(2) ECTRANS_OMP_DEFAULT_CLAUSE &
        !$OMP& PRIVATE(JK,JBLK,IFLD,IPOS) &
        !$OMP& SHARED(IFLDA,PGP,D_NGP_A,D_NGP_B,D_NGP_STRIDE) &
        !$OMP& ECTRANS_DEVICE_ADDR_CLAUSE(PREEL_REAL) &
        !$OMP& MAP(ECTRANS_MAP_PRESENT_ALLOC:D_NGP_A,D_NGP_B,D_NGP_STRIDE) &
        !$OMP& ECTRANS_LOOP_BOUNDS_CLAUSE(KF_FS,IRECV_WSET_SIZE_V) &
        !$OMP& FIRSTPRIVATE(NPROMA,IRECV_WSET_OFFSET_V,IGP_V)
#endif
#ifdef ACCGPU
        !$ACC PARALLEL LOOP COLLAPSE(2) DEFAULT(NONE) PRIVATE(JK,JBLK,IFLD,IPOS) &
        !$ACC&         PRESENT(D_NGP_A,D_NGP_B,D_NGP_STRIDE) &
        !$ACC&         FIRSTPRIVATE(KF_FS,IRECV_WSET_SIZE_V,IRECV_WSET_OFFSET_V, &
        !$ACC&         IGP_V,NPROMA) ASYNC(1)
#endif
        DO JFLD=1,KF_FS
          DO JL=1,IRECV_WSET_SIZE_V
            JK = MOD(IRECV_WSET_OFFSET_V+JL-1,NPROMA)+1
            JBLK = (IRECV_WSET_OFFSET_V+JL-1)/NPROMA+1
            IFLD = IFLDA(JFLD,1)
            IPOS = 1_JPIB*KF_FS*D_NGP_A(IGP_V+JL)+D_NGP_B(IGP_V+JL) &
                & +(JFLD-1)*D_NGP_STRIDE(IGP_V+JL)+1
            PGP(JK,IFLD,JBLK) = PREEL_REAL(IPOS)
          ENDDO
        ENDDO
      ELSE
        CALL TRLTOG_LOCAL_CONTRIB(ZPREEL_C(1),SIZE(PREEL_REAL,KIND=JPIB), &
          &                       IFLDA,SIZE(IFLDA,1),SIZE(IFLDA,2), &
          &                       IGP_OFFSETS,KF_GP, &
          &                       D_NGP_A,D_NGP_B,D_NGP_STRIDE,SIZE(D_NGP_A), &
          &                       PGPUV,IUV1,IUV2,IUV3,IUV4, &
          &                       PGP2,IG21,IG22,IG23, &
          &                       PGP3A,I3A1,I3A2,I3A3,I3A4, &
          &                       PGP3B,I3B1,I3B2,I3B3,I3B4, &
          &                       KF_FS,IRECV_WSET_SIZE_V,IRECV_WSET_OFFSET_V,IGP_V,NPROMA)
      ENDIF
      END ASSOCIATE
      CALL GSTATS(450,1)
      CALL GSTATS(1604,1)
    ENDIF

    ALLOCATE(ICOMBUFS_OFFSET(ISEND_COUNTS+1))
    ICOMBUFS_OFFSET(1) = 0
    DO JROC=1,ISEND_COUNTS
      ICOMBUFS_OFFSET(JROC+1) = ICOMBUFS_OFFSET(JROC) + ISENDTOT(ISEND_TO_PROC(JROC))
    ENDDO
    ALLOCATE(ICOMBUFR_OFFSET(IRECV_COUNTS+1))
    ICOMBUFR_OFFSET(1) = 0
    DO JROC=1,IRECV_COUNTS
      ICOMBUFR_OFFSET(JROC+1) = ICOMBUFR_OFFSET(JROC) + IRECVTOT(IRECV_TO_PROC(JROC))
    ENDDO

    IF (IRECV_COUNTS > 0) THEN
      CALL ASSIGN_PTR(ZCOMBUFR, GET_ALLOCATION(ALLOCATOR, HTRLTOG%HCOMBUFR_AND_COMBUFS),&
          & 1_JPIB, ICOMBUFR_OFFSET(IRECV_COUNTS+1)*C_SIZEOF(ZCOMBUFR(1)))
      ZCOMBUFR_C => ZCOMBUFR
    ENDIF
    IF (ISEND_COUNTS > 0) THEN
      CALL ASSIGN_PTR(ZCOMBUFS, GET_ALLOCATION(ALLOCATOR, HTRLTOG%HCOMBUFR_AND_COMBUFS),&
          & ALIGN(1_JPIB*KF_GP*D%NGPTOT*C_SIZEOF(ZCOMBUFR(1)),128)+1, &
          & ICOMBUFS_OFFSET(ISEND_COUNTS+1)*C_SIZEOF(ZCOMBUFS(1)))
      ZCOMBUFS_C => ZCOMBUFS
    ENDIF

#ifdef OMPGPU
    ! ZCOMBUFS is a growing-allocator buffer; supplied via HAS_DEVICE_ADDR in the pack loop
#endif
#ifdef ACCGPU
    !$ACC DATA PRESENT(ZCOMBUFS) IF(ISEND_COUNTS > 0) ASYNC(1)
#endif
    CALL GSTATS(1605,0)
    CALL GSTATS(451,0)
    ASSOCIATE(D_NGP_A=>D%NGP_A, D_NGP_B=>D%NGP_B, D_NGP_STRIDE=>D%NGP_STRIDE)
    DO INS=1,ISEND_COUNTS
      IPROC = ISEND_TO_PROC(INS)
      ILEN = ISENDTOT(IPROC)/KF_FS
      IGP_V = D%NGP_OFFSET(IPROC)
      ICOMBUFS_OFFSET_V = ICOMBUFS_OFFSET(INS)
      ! Element actuals: sequence association hands over the base address, so no dope
      ! vector is built for the pointer buffers.
      CALL TRLTOG_PACK_SEND(ZPREEL_C(1),SIZE(ZPREEL_C,KIND=JPIB), &
        &                   ZCOMBUFS_C(1),SIZE(ZCOMBUFS_C,KIND=JPIB), &
        &                   D_NGP_A(1),D_NGP_B(1),D_NGP_STRIDE(1),SIZE(D_NGP_A), &
        &                   KF_FS,ILEN,IGP_V,ICOMBUFS_OFFSET_V)
    ENDDO
    END ASSOCIATE
    CALL GSTATS(451,1)
    CALL GSTATS(1605,1)
#ifdef OMPGPU
    ! ZCOMBUFS region closed (now supplied via HAS_DEVICE_ADDR)
#endif
#ifdef ACCGPU
    !$ACC END DATA ! ZCOMBUFS

    !$ACC WAIT(1)
#endif

    CALL GSTATS(805,0)

    IF (LSYNC_TRANS) THEN
      CALL GSTATS(440,0)
      CALL MPL_BARRIER(CDSTRING='')
      CALL GSTATS(440,1)
    ENDIF
    CALL GSTATS(421,0)

    CALL GSTATS(452,0)
    IR=0
    !...Receive loop.........................................................
#ifdef USE_GPU_AWARE_MPI
    ! Under OMPGPU these buffers come from the growing allocator, which hands out device
    ! pointers directly (see GROWING_ALLOCATOR_MOD), so they can go straight to GPU-aware
    ! MPI; a USE_DEVICE_PTR region would only map and re-copy their descriptors.
#ifdef ACCGPU
    !$ACC HOST_DATA USE_DEVICE(ZCOMBUFS,ZCOMBUFR)
#endif
#else
#ifdef OMPGPU
    !$OMP TARGET UPDATE FROM(ZCOMBUFS) IF(ISEND_COUNTS > 0)
#endif
#ifdef ACCGPU
    !! this is safe-but-slow fallback for running without GPU-aware MPI
    !$ACC UPDATE HOST(ZCOMBUFS) IF(ISEND_COUNTS > 0)
#endif
#endif

    ! Skip the own contribution because this is ok to overflow
    ISENDTOT(MYPROC) = 0
    IRECVTOT(MYPROC) = 0

    ISENDTOT_MPI = ISENDTOT
    IRECVTOT_MPI = IRECVTOT
    IF (ANY(ISENDTOT_MPI /= ISENDTOT)) &
      & CALL MPL_ABORT("Overflow in trltog")
    IF (ANY(IRECVTOT_MPI /= IRECVTOT)) &
      & CALL MPL_ABORT("Overflow in trltog")

    DO INR=1,IRECV_COUNTS
      IR=IR+1
      IRECV=IRECV_TO_PROC(INR)
#ifdef USE_RAW_MPI
      CALL MPI_IRECV(ZCOMBUFR(ICOMBUFR_OFFSET(INR)+1:ICOMBUFR_OFFSET(INR+1)), &
        & IRECVTOT_MPI(IRECV), &
        & TRLTOG_DTYPE,NPRCIDS(IRECV)-1, &
        & MTAGLG, LOCAL_COMM, IREQUEST(IR), &
        & IERROR )
      IREQ(IR) = IREQUEST(IR)%MPI_VAL
#else
      CALL MPL_RECV(ZCOMBUFR(ICOMBUFR_OFFSET(INR)+1:ICOMBUFR_OFFSET(INR+1)), &
        &           KSOURCE=NPRCIDS(IRECV), KTAG=MTAGLG, KMP_TYPE=JP_NON_BLOCKING_STANDARD, &
        &           KREQUEST=IREQUEST(IR))
      IREQ(IR) = IREQUEST(IR)
#endif
    ENDDO

    !...Send loop.........................................................
    DO INS=1,ISEND_COUNTS
      IR=IR+1
      ISEND=ISEND_TO_PROC(INS)
#ifdef USE_RAW_MPI
      CALL MPI_ISEND(ZCOMBUFS(ICOMBUFS_OFFSET(INS)+1:ICOMBUFS_OFFSET(INS+1)),ISENDTOT_MPI(ISEND), &
        & TRLTOG_DTYPE, NPRCIDS(ISEND)-1,MTAGLG,LOCAL_COMM,IREQUEST(IR),IERROR)
      IREQ(IR) = IREQUEST(IR)%MPI_VAL
#else
      CALL MPL_SEND(ZCOMBUFS(ICOMBUFS_OFFSET(INS)+1:ICOMBUFS_OFFSET(INS+1)), &
        &           KDEST=NPRCIDS(ISEND), KTAG=MTAGLG, KMP_TYPE=JP_NON_BLOCKING_STANDARD, &
        &           KREQUEST=IREQUEST(IR))
      IREQ(IR) = IREQUEST(IR)
#endif
    ENDDO

    CALL GSTATS(452,1)

    CALL GSTATS(453,0)
    IF(IR > 0) THEN
      CALL MPL_WAIT(KREQUEST=IREQ(1:IR), &
      & CDSTRING='TRLTOG: WAIT FOR SENDS AND RECEIVES')
    ENDIF
    CALL GSTATS(453,1)

#ifdef USE_GPU_AWARE_MPI
#ifdef ACCGPU
    !$ACC END HOST_DATA ! ZCOMBUFS, ZCOMBUFR
#endif
#else
#ifdef OMPGPU
#endif
    !! this is safe-but-slow fallback for running without GPU-aware MPI
#ifdef OMPGPU
    !$OMP TARGET UPDATE TO(ZCOMBUFR) IF(IRECV_COUNTS > 0)
#endif
#ifdef ACCGPU
    !$ACC UPDATE DEVICE(ZCOMBUFR) IF(IRECV_COUNTS > 0)
#endif
#endif

    IF (LSYNC_TRANS) THEN
      CALL GSTATS(441,0)
      CALL MPL_BARRIER(CDSTRING='')
      CALL GSTATS(441,1)
    ENDIF
    CALL GSTATS(421,1)

#ifdef OMPGPU
    ! ZCOMBUFR is a growing-allocator buffer; supplied via HAS_DEVICE_ADDR in the unpack loops
#endif
#ifdef ACCGPU
    !$ACC DATA PRESENT(ZCOMBUFR) IF(IRECV_COUNTS > 0) ASYNC(1)
#endif
    CALL GSTATS(805,1)

    !  Unpack loop.........................................................

    CALL GSTATS(1606,0)
    CALL GSTATS(454,0)
    DO INR=1,IRECV_COUNTS
      IRECV=IRECV_TO_PROC(INR)
      CALL PE2SET(IRECV,ISETA,ISETB,ISETW,ISETV)

      IRECV_FIELD_COUNT_V = IRECV_FIELD_COUNT(ISETV)
      ICOMBUFR_OFFSET_V = ICOMBUFR_OFFSET(INR)

      IRECV_WSET_OFFSET_V = IRECV_WSET_OFFSET(ISETW)
      IRECV_WSET_SIZE_V = IRECV_WSET_SIZE(ISETW)
      IF (PRESENT(PGP)) THEN
#ifdef OMPGPU
        !$OMP TARGET TEAMS DISTRIBUTE PARALLEL DO COLLAPSE(2) ECTRANS_OMP_DEFAULT_CLAUSE &
        !$OMP& PRIVATE(JK,JBLK,IFLD,JI) &
        !$OMP& SHARED(IFLDA,PGP) ECTRANS_DEVICE_ADDR_CLAUSE(ZCOMBUFR) &
        !$OMP& ECTRANS_LOOP_BOUNDS_CLAUSE(IRECV_FIELD_COUNT_V,IRECV_WSET_SIZE_V) &
        !$OMP& FIRSTPRIVATE(NPROMA,IRECV_WSET_OFFSET_V,ICOMBUFR_OFFSET_V,INR)
#endif
#ifdef ACCGPU
        !$ACC PARALLEL LOOP COLLAPSE(2) DEFAULT(NONE) PRIVATE(JK,JBLK,IFLD,JI) &
        !$ACC&              FIRSTPRIVATE(INR,IRECV_FIELD_COUNT_V,IRECV_WSET_SIZE_V,&
        !$ACC&              IRECV_WSET_OFFSET_V,NPROMA,ICOMBUFR_OFFSET_V) ASYNC(1)
#endif
        DO JFLD=1,IRECV_FIELD_COUNT_V
          DO JL=1,IRECV_WSET_SIZE_V
            JK = MOD(IRECV_WSET_OFFSET_V+JL-1,NPROMA)+1
            JBLK = (IRECV_WSET_OFFSET_V+JL-1)/NPROMA+1
            IFLD=IFLDA(JFLD,1+INR)
            JI = ICOMBUFR_OFFSET_V+(JFLD-1)*IRECV_WSET_SIZE_V+JL
            PGP(JK,IFLD,JBLK) = ZCOMBUFR(JI)
          ENDDO
        ENDDO
      ELSE
        CALL TRLTOG_UNPACK_RECV(ZCOMBUFR_C(1),SIZE(ZCOMBUFR_C,KIND=JPIB), &
          &                     IFLDA,SIZE(IFLDA,1),SIZE(IFLDA,2), &
          &                     IGP_OFFSETS,KF_GP, &
          &                     PGPUV,IUV1,IUV2,IUV3,IUV4, &
          &                     PGP2,IG21,IG22,IG23, &
          &                     PGP3A,I3A1,I3A2,I3A3,I3A4, &
          &                     PGP3B,I3B1,I3B2,I3B3,I3B4, &
          &                     IRECV_FIELD_COUNT_V,IRECV_WSET_SIZE_V, &
          &                     IRECV_WSET_OFFSET_V,ICOMBUFR_OFFSET_V,NPROMA,INR)
      ENDIF
    ENDDO
#ifdef OMPGPU
#endif
#ifdef ACCGPU
    !$ACC WAIT(1)
#endif

#ifdef OMPGPU
    ! ZCOMBUFR region closed (now supplied via HAS_DEVICE_ADDR)
#endif
#ifdef ACCGPU
    !$ACC END DATA ! ZCOMBUFR
#endif
    IF (LSYNC_TRANS) THEN
#ifdef ACCGPU
      !$ACC WAIT(1)
#endif
      CALL GSTATS(440,0)
      CALL MPL_BARRIER(CDSTRING='')
      CALL GSTATS(440,1)
    ENDIF
    CALL GSTATS(422,0)
#ifdef OMPGPU
    !$OMP END TARGET DATA ! IFLDA
#endif
#ifdef ACCGPU
    !$ACC END DATA ! IFLDA
    !$ACC END DATA ! PREEL_REAL
    !$ACC END DATA ! PGP3B
    !$ACC END DATA ! PGP3A
    !$ACC END DATA ! PGP2
    !$ACC END DATA ! PGPUV
    !$ACC END DATA ! PGP
#endif
    IF (PRESENT(PGP)) THEN
#ifdef OMPGPU
      !$OMP TARGET UPDATE FROM(PGP) IF (.NOT. LLPGP_ON_GPU)
#endif
#ifdef ACCGPU
      !$ACC UPDATE HOST(PGP) IF (.NOT. LLPGP_ON_GPU) ASYNC(1)
#endif
    ENDIF
    IF (PRESENT(PGPUV)) THEN
#ifdef OMPGPU
      !$OMP TARGET UPDATE FROM(PGPUV) IF (.NOT. LLPGP_ON_GPU)
#endif
#ifdef ACCGPU
      !$ACC UPDATE HOST(PGPUV) IF (.NOT. LLPGP_ON_GPU) ASYNC(1)
#endif
    ENDIF
    IF (PRESENT(PGP2)) THEN
#ifdef OMPGPU
      !$OMP TARGET UPDATE FROM(PGP2) IF (.NOT. LLPGP_ON_GPU)
#endif
#ifdef ACCGPU
      !$ACC UPDATE HOST(PGP2) IF (.NOT. LLPGP_ON_GPU) ASYNC(1)
#endif
    ENDIF
    IF (PRESENT(PGP3A)) THEN
#ifdef OMPGPU
      !$OMP TARGET UPDATE FROM(PGP3A) IF (.NOT. LLPGP_ON_GPU)
#endif
#ifdef ACCGPU
      !$ACC UPDATE HOST(PGP3A) IF (.NOT. LLPGP_ON_GPU) ASYNC(1)
#endif
    ENDIF
    IF (PRESENT(PGP3B)) THEN
#ifdef OMPGPU
      !$OMP TARGET UPDATE FROM(PGP3B) IF (.NOT. LLPGP_ON_GPU)
#endif
#ifdef ACCGPU
      !$ACC UPDATE HOST(PGP3B) IF (.NOT. LLPGP_ON_GPU) ASYNC(1)
#endif
    ENDIF
    IF (ACC_POINTERS_CNT > 0) CALL EXT_ACC_DELETE(ACC_POINTERS(1:ACC_POINTERS_CNT), &
#ifdef ACCGPU
         & STREAM=1_ACC_HANDLE_KIND)
#endif
#ifdef OMPGPU
         & STREAM=1)
#endif

    IF (LSYNC_TRANS) THEN
#ifdef ACCGPU
      !$ACC WAIT(1)
#endif
      CALL GSTATS(442,0)
      CALL MPL_BARRIER(CDSTRING='')
      CALL GSTATS(442,1)
    ENDIF
    CALL GSTATS(422,1)

#ifdef ACCGPU
    !$ACC END DATA ! IGP_OFFSETS

    !$ACC WAIT(1)
#endif
#ifdef OMPGPU
    !$OMP END TARGET DATA !IGP_OFFSETS
#endif

    CALL GSTATS(454,1)
    CALL GSTATS(1606,1)

    ! Free this now
    DEALLOCATE(IFLDA)

    IF (LHOOK) CALL DR_HOOK('TRLTOG',1,ZHOOK_HANDLE)
  END SUBROUTINE TRLTOG

  ! The pack and unpack bodies live in their own procedures so that every array they touch
  ! can be an explicit-shape dummy. A POINTER or assumed-shape actual is described by a dope
  ! vector, and the compiler places that dope vector in the device data environment on every
  ! launch: it creates a map entry, copies 48 bytes and tears the entry down again, once per
  ! construct. Neither HAS_DEVICE_ADDR nor an enclosing USE_DEVICE_ADDR region suppresses
  ! that. An explicit-shape dummy is described entirely by its extents, which travel as
  ! FIRSTPRIVATE scalars, so no descriptor has to reach the device at all.
  SUBROUTINE TRLTOG_PACK_SEND(PREEL_REAL,KPREEL,ZCOMBUFS,KCOMBUFS, &
    &                         KNGP_A,KNGP_B,KNGP_STRIDE,KNGP, &
    &                         KF_FS,KLEN,KGP_V,KCOMBUFS_OFFSET)
    USE PARKIND_ECTRANS, ONLY: JPIM, JPRBT, JPIB

    IMPLICIT NONE

    INTEGER(KIND=JPIB), INTENT(IN)    :: KPREEL, KCOMBUFS, KCOMBUFS_OFFSET
    INTEGER(KIND=JPIM), INTENT(IN)    :: KNGP, KF_FS, KLEN, KGP_V
    REAL(KIND=JPRBT),   INTENT(IN)    :: PREEL_REAL(KPREEL)
    REAL(KIND=JPRBT),   INTENT(INOUT) :: ZCOMBUFS(KCOMBUFS)
    INTEGER(KIND=JPIM), INTENT(IN)    :: KNGP_A(KNGP), KNGP_B(KNGP), KNGP_STRIDE(KNGP)

    INTEGER(KIND=JPIM) :: JFLD, JL
    INTEGER(KIND=JPIB) :: IPOS

#ifdef OMPGPU
    !$OMP TARGET TEAMS DISTRIBUTE PARALLEL DO COLLAPSE(2) ECTRANS_OMP_DEFAULT_CLAUSE &
    !$OMP& PRIVATE(IPOS) &
    !$OMP& SHARED(KNGP_A,KNGP_B,KNGP_STRIDE) &
    !$OMP& ECTRANS_DEVICE_ADDR_CLAUSE(PREEL_REAL,ZCOMBUFS) &
    !$OMP& MAP(ECTRANS_MAP_PRESENT_ALLOC:KNGP_A,KNGP_B,KNGP_STRIDE) &
    !$OMP& ECTRANS_LOOP_BOUNDS_CLAUSE(KF_FS,KLEN) FIRSTPRIVATE(KGP_V,KCOMBUFS_OFFSET)
#endif
#ifdef ACCGPU
    !$ACC PARALLEL LOOP DEFAULT(NONE) PRIVATE(IPOS) &
    !$ACC&              PRESENT(KNGP_A,KNGP_B,KNGP_STRIDE,PREEL_REAL,ZCOMBUFS) &
    !$ACC&              FIRSTPRIVATE(KF_FS,KLEN,KGP_V,KCOMBUFS_OFFSET) COLLAPSE(2) ASYNC(1)
#endif
    DO JFLD=1,KF_FS
      DO JL=1,KLEN
        IPOS = 1_JPIB*KF_FS*KNGP_A(KGP_V+JL)+KNGP_B(KGP_V+JL) &
            & +(JFLD-1)*KNGP_STRIDE(KGP_V+JL)+1
        ZCOMBUFS(KCOMBUFS_OFFSET+(JFLD-1)*KLEN+JL) = PREEL_REAL(IPOS)
      ENDDO
    ENDDO
  END SUBROUTINE TRLTOG_PACK_SEND

  ! Receive-side unpack. ZCOMBUFR, IFLDA and IGP_OFFSETS are explicit-shape here for the
  ! same reason as above. PGPUV/PGP2/PGP3A/PGP3B are OPTIONAL but still explicit-shape,
  ! with their extents passed alongside: OPTIONAL assumed-shape ICEs the AFAR 24.x
  ! frontend once combined with the device-address clauses (see ~/ecmwf/repro_use_device_addr).
  SUBROUTINE TRLTOG_UNPACK_RECV(ZCOMBUFR,KCOMBUFR,KFLDA,KFLDA1,KFLDA2, &
    &                           KGP_OFFSETS,KGP, &
    &                           PGPUV,KUV1,KUV2,KUV3,KUV4, &
    &                           PGP2,KG21,KG22,KG23, &
    &                           PGP3A,K3A1,K3A2,K3A3,K3A4, &
    &                           PGP3B,K3B1,K3B2,K3B3,K3B4, &
    &                           KFIELD_COUNT,KWSET_SIZE,KWSET_OFFSET,KCOMBUFR_OFFSET, &
    &                           KNPROMA,KNR)
    USE PARKIND_ECTRANS, ONLY: JPIM, JPRB, JPRBT, JPIB

    IMPLICIT NONE

    INTEGER(KIND=JPIB), INTENT(IN) :: KCOMBUFR, KCOMBUFR_OFFSET
    INTEGER(KIND=JPIM), INTENT(IN) :: KFLDA1, KFLDA2, KGP
    INTEGER(KIND=JPIM), INTENT(IN) :: KFIELD_COUNT, KWSET_SIZE, KWSET_OFFSET, KNPROMA, KNR
    REAL(KIND=JPRBT),   INTENT(IN) :: ZCOMBUFR(KCOMBUFR)
    INTEGER(KIND=JPIB), INTENT(IN) :: KFLDA(KFLDA1,KFLDA2)
    INTEGER(KIND=JPIM), INTENT(IN) :: KGP_OFFSETS(KGP,3)
    INTEGER(KIND=JPIM), INTENT(IN) :: KUV1,KUV2,KUV3,KUV4
    INTEGER(KIND=JPIM), INTENT(IN) :: KG21,KG22,KG23
    INTEGER(KIND=JPIM), INTENT(IN) :: K3A1,K3A2,K3A3,K3A4
    INTEGER(KIND=JPIM), INTENT(IN) :: K3B1,K3B2,K3B3,K3B4
    REAL(KIND=JPRB), OPTIONAL, INTENT(OUT) :: PGPUV(KUV1,KUV2,KUV3,KUV4)
    REAL(KIND=JPRB), OPTIONAL, INTENT(OUT) :: PGP3A(K3A1,K3A2,K3A3,K3A4)
    REAL(KIND=JPRB), OPTIONAL, INTENT(OUT) :: PGP3B(K3B1,K3B2,K3B3,K3B4)
    REAL(KIND=JPRB), OPTIONAL, INTENT(OUT) :: PGP2(KG21,KG22,KG23)

    INTEGER(KIND=JPIM), PARAMETER :: IGP_OFFSETS_UV=1, IGP_OFFSETS_GP2=2
    INTEGER(KIND=JPIM), PARAMETER :: IGP_OFFSETS_GP3A=3, IGP_OFFSETS_GP3B=4
    INTEGER(KIND=JPIM) :: JFLD, JL, JK, JBLK, IFLD
    INTEGER(KIND=JPIB) :: JI

#ifdef OMPGPU
    !$OMP TARGET TEAMS DISTRIBUTE PARALLEL DO COLLAPSE(2) ECTRANS_OMP_DEFAULT_CLAUSE &
    !$OMP& PRIVATE(JK,JBLK,IFLD,JI) &
    !$OMP& SHARED(KFLDA,KGP_OFFSETS) &
    !$OMP& ECTRANS_LOOP_BOUNDS_CLAUSE(KFIELD_COUNT,KWSET_SIZE) &
    !$OMP& FIRSTPRIVATE(KNPROMA,KWSET_OFFSET,KCOMBUFR_OFFSET,KNR) &
    !$OMP& ECTRANS_DEVICE_ADDR_CLAUSE(ZCOMBUFR) &
    !$OMP& MAP(ALLOC:PGPUV,PGP2,PGP3A,PGP3B)
#endif
#ifdef ACCGPU
    !$ACC PARALLEL LOOP COLLAPSE(2) DEFAULT(PRESENT) PRIVATE(JK,JBLK,IFLD,JI) &
    !$ACC&              NO_CREATE(PGPUV,PGP2,PGP3A,PGP3B) &
    !$ACC&              FIRSTPRIVATE(KNR,KFIELD_COUNT,KWSET_SIZE, &
    !$ACC&              KWSET_OFFSET,KNPROMA,KCOMBUFR_OFFSET) ASYNC(1)
#endif
    DO JFLD=1,KFIELD_COUNT
      DO JL=1,KWSET_SIZE
        JK = MOD(KWSET_OFFSET+JL-1,KNPROMA)+1
        JBLK = (KWSET_OFFSET+JL-1)/KNPROMA+1
        IFLD=KFLDA(JFLD,1+KNR)
        JI = KCOMBUFR_OFFSET+(JFLD-1)*KWSET_SIZE+JL
        IF(KGP_OFFSETS(IFLD,1) == IGP_OFFSETS_UV) THEN
          PGPUV(JK,KGP_OFFSETS(IFLD,3),KGP_OFFSETS(IFLD,2),JBLK) = ZCOMBUFR(JI)
        ELSEIF(KGP_OFFSETS(IFLD,1) == IGP_OFFSETS_GP2) THEN
          PGP2(JK,KGP_OFFSETS(IFLD,2),JBLK) = ZCOMBUFR(JI)
        ELSEIF(KGP_OFFSETS(IFLD,1) == IGP_OFFSETS_GP3A) THEN
          PGP3A(JK,KGP_OFFSETS(IFLD,3),KGP_OFFSETS(IFLD,2),JBLK) = ZCOMBUFR(JI)
        ELSEIF(KGP_OFFSETS(IFLD,1) == IGP_OFFSETS_GP3B) THEN
          PGP3B(JK,KGP_OFFSETS(IFLD,3),KGP_OFFSETS(IFLD,2),JBLK) = ZCOMBUFR(JI)
        ENDIF
      ENDDO
    ENDDO
  END SUBROUTINE TRLTOG_UNPACK_RECV

  ! Local contribution: the share of the transpose that stays on this task, so it moves
  ! straight from PREEL_REAL into the gridpoint arrays without a buffer or a message.
  ! Same explicit-shape treatment as the unpack above.
  SUBROUTINE TRLTOG_LOCAL_CONTRIB(PREEL_REAL,KPREEL,KFLDA,KFLDA1,KFLDA2, &
    &                             KGP_OFFSETS,KGP, &
    &                             KNGP_A,KNGP_B,KNGP_STRIDE,KNGP, &
    &                             PGPUV,KUV1,KUV2,KUV3,KUV4, &
    &                             PGP2,KG21,KG22,KG23, &
    &                             PGP3A,K3A1,K3A2,K3A3,K3A4, &
    &                             PGP3B,K3B1,K3B2,K3B3,K3B4, &
    &                             KF_FS,KWSET_SIZE,KWSET_OFFSET,KGP_V,KNPROMA)
    USE PARKIND_ECTRANS, ONLY: JPIM, JPRB, JPRBT, JPIB

    IMPLICIT NONE

    INTEGER(KIND=JPIB), INTENT(IN) :: KPREEL
    INTEGER(KIND=JPIM), INTENT(IN) :: KFLDA1, KFLDA2, KGP, KNGP
    INTEGER(KIND=JPIM), INTENT(IN) :: KF_FS, KWSET_SIZE, KWSET_OFFSET, KGP_V, KNPROMA
    REAL(KIND=JPRBT),   INTENT(IN) :: PREEL_REAL(KPREEL)
    INTEGER(KIND=JPIB), INTENT(IN) :: KFLDA(KFLDA1,KFLDA2)
    INTEGER(KIND=JPIM), INTENT(IN) :: KGP_OFFSETS(KGP,3)
    INTEGER(KIND=JPIM), INTENT(IN) :: KNGP_A(KNGP), KNGP_B(KNGP), KNGP_STRIDE(KNGP)
    INTEGER(KIND=JPIM), INTENT(IN) :: KUV1,KUV2,KUV3,KUV4
    INTEGER(KIND=JPIM), INTENT(IN) :: KG21,KG22,KG23
    INTEGER(KIND=JPIM), INTENT(IN) :: K3A1,K3A2,K3A3,K3A4
    INTEGER(KIND=JPIM), INTENT(IN) :: K3B1,K3B2,K3B3,K3B4
    REAL(KIND=JPRB), OPTIONAL, INTENT(OUT) :: PGPUV(KUV1,KUV2,KUV3,KUV4)
    REAL(KIND=JPRB), OPTIONAL, INTENT(OUT) :: PGP3A(K3A1,K3A2,K3A3,K3A4)
    REAL(KIND=JPRB), OPTIONAL, INTENT(OUT) :: PGP3B(K3B1,K3B2,K3B3,K3B4)
    REAL(KIND=JPRB), OPTIONAL, INTENT(OUT) :: PGP2(KG21,KG22,KG23)

    INTEGER(KIND=JPIM), PARAMETER :: IGP_OFFSETS_UV=1, IGP_OFFSETS_GP2=2
    INTEGER(KIND=JPIM), PARAMETER :: IGP_OFFSETS_GP3A=3, IGP_OFFSETS_GP3B=4
    INTEGER(KIND=JPIM) :: JFLD, JL, JK, JBLK, IFLD
    INTEGER(KIND=JPIB) :: IPOS

#ifdef OMPGPU
    !$OMP TARGET TEAMS DISTRIBUTE PARALLEL DO COLLAPSE(2) ECTRANS_OMP_DEFAULT_CLAUSE &
    !$OMP& PRIVATE(JK,JBLK,IFLD,IPOS) &
    !$OMP& SHARED(KFLDA,KGP_OFFSETS,KNGP_A,KNGP_B,KNGP_STRIDE) &
    !$OMP& MAP(ECTRANS_MAP_PRESENT_ALLOC:KNGP_A,KNGP_B,KNGP_STRIDE) &
    !$OMP& ECTRANS_LOOP_BOUNDS_CLAUSE(KF_FS,KWSET_SIZE) &
    !$OMP& FIRSTPRIVATE(KNPROMA,KWSET_OFFSET,KGP_V) &
    !$OMP& ECTRANS_DEVICE_ADDR_CLAUSE(PREEL_REAL) &
    !$OMP& MAP(ALLOC:PGPUV,PGP2,PGP3A,PGP3B)
#endif
#ifdef ACCGPU
    !$ACC PARALLEL LOOP COLLAPSE(2) DEFAULT(PRESENT) PRIVATE(JK,JBLK,IFLD,IPOS) &
    !$ACC&              PRESENT(KNGP_A,KNGP_B,KNGP_STRIDE) &
    !$ACC&              NO_CREATE(PGPUV,PGP2,PGP3A,PGP3B) &
    !$ACC&              FIRSTPRIVATE(KF_FS,KWSET_SIZE,KWSET_OFFSET,KGP_V,KNPROMA) ASYNC(1)
#endif
    DO JFLD=1,KF_FS
      DO JL=1,KWSET_SIZE
        JK = MOD(KWSET_OFFSET+JL-1,KNPROMA)+1
        JBLK = (KWSET_OFFSET+JL-1)/KNPROMA+1
        IFLD = KFLDA(JFLD,1)
        IPOS = 1_JPIB*KF_FS*KNGP_A(KGP_V+JL)+KNGP_B(KGP_V+JL) &
            & +(JFLD-1)*KNGP_STRIDE(KGP_V+JL)+1
        IF(KGP_OFFSETS(IFLD,1) == IGP_OFFSETS_UV) THEN
          PGPUV(JK,KGP_OFFSETS(IFLD,3),KGP_OFFSETS(IFLD,2),JBLK) = PREEL_REAL(IPOS)
        ELSEIF(KGP_OFFSETS(IFLD,1) == IGP_OFFSETS_GP2) THEN
          PGP2(JK,KGP_OFFSETS(IFLD,2),JBLK) = PREEL_REAL(IPOS)
        ELSEIF(KGP_OFFSETS(IFLD,1) == IGP_OFFSETS_GP3A) THEN
          PGP3A(JK,KGP_OFFSETS(IFLD,3),KGP_OFFSETS(IFLD,2),JBLK) = PREEL_REAL(IPOS)
        ELSEIF(KGP_OFFSETS(IFLD,1) == IGP_OFFSETS_GP3B) THEN
          PGP3B(JK,KGP_OFFSETS(IFLD,3),KGP_OFFSETS(IFLD,2),JBLK) = PREEL_REAL(IPOS)
        ENDIF
      ENDDO
    ENDDO
  END SUBROUTINE TRLTOG_LOCAL_CONTRIB
END MODULE TRLTOG_MOD

