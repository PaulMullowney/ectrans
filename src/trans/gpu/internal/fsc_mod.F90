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

MODULE FSC_MOD
  USE PARKIND_ECTRANS,        ONLY: JPIM, JPRBT, JPIB
  USE TPM_DISTR,              ONLY: D
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: FSC

  ! Wavenumbers per thread in FSC_DIVIDE_ACHTE and FSC_EWDERIV. Amortises the per-(KGL,JF)
  ! invariant loads over this many wavenumbers; see the comments at those kernels.
  INTEGER(KIND=JPIM), PARAMETER :: ITILE = 4

CONTAINS

SUBROUTINE FSC(PREEL_COMPLEX, KF_FS, KF_UV, KF_SCALARS, KUV_OFFSET, &
        & KSCALARS_OFFSET, KSCALARS_NSDER_OFFSET, KUV_EWDER_OFFSET, KSCALARS_EWDER_OFFSET)

!**** *FSC - Division by a*cos(theta), east-west derivatives

!     Purpose.
!     --------
!        In Fourier space divide u and v and all north-south
!        derivatives by a*cos(theta). Also compute east-west derivatives
!        of u,v,thermodynamic, passiv scalar variables and surface
!        pressure.

!**   Interface.
!     ----------
!        CALL FSC(..)
!        Explicit arguments :  KF_FS - total stride
!        --------------------  KF_UV - # uv layers
!                              KF_SCALARS - # scalar layers
!                              *_OFFSET - offset of the respective layer
!
!     Method.
!     -------

!     Externals.   None.
!     ----------

!     Author.
!     -------
!        Mats Hamrud *ECMWF*

!     Modifications.
!     --------------
!        Original : 00-03-03 (From SC2FSC)

!     ------------------------------------------------------------------

USE TPM_DISTR,       ONLY: MYSETW,  MYPROC, NPROC, D
USE TPM_GEOMETRY,    ONLY: G
USE TPM_FIELDS,      ONLY: F
USE TPM_DIM,         ONLY: R
!

IMPLICIT NONE
! CONTIGUOUS so that handing this to the explicit-shape dummies of the helpers below passes a
! base address. Without it the compiler must allow for a non-contiguous actual and would pack
! into a temporary -- a host-side copy of device-resident data.
REAL(KIND=JPRBT), INTENT(INOUT), CONTIGUOUS :: PREEL_COMPLEX(:)
INTEGER(KIND=JPIM), INTENT(IN) :: KF_FS, KF_UV, KF_SCALARS
INTEGER(KIND=JPIM), INTENT(IN) :: KUV_OFFSET, KSCALARS_OFFSET, KSCALARS_NSDER_OFFSET
INTEGER(KIND=JPIM), INTENT(IN) :: KUV_EWDER_OFFSET, KSCALARS_EWDER_OFFSET

INTEGER(KIND=JPIM) :: OFFSET_VAR,ILOEN_MAX
INTEGER(KIND=JPIM) :: IBEG,IEND,IINC

! Extents of PREEL_COMPLEX and of the geometry tables, for the explicit-shape dummies of the
! helpers below.
INTEGER(KIND=JPIB) :: IPREEL
INTEGER(KIND=JPIM) :: INSTAGTF,INMEN,INLOEN,IRACTHE

ASSOCIATE(D_NUMP=>D%NUMP, D_NPTRLS=>D%NPTRLS, D_NSTAGTF=>D%NSTAGTF, G_NMEN=>G%NMEN, &
        & G_NLOEN=>G%NLOEN, F_RACTHE=>F%RACTHE, R_NSMAX=>R%NSMAX)
!     ------------------------------------------------------------------

IF(MYPROC > NPROC/2)THEN
  IBEG=1
  IEND=D%NDGL_FS
  IINC=1
ELSE
  IBEG=D%NDGL_FS
  IEND=1
  IINC=-1
ENDIF

#ifdef OMPGPU
! Only the ASSOCIATE aliases are mapped: naming the parent derived type makes the runtime
! walk and re-copy every one of its allocatable component descriptors on region entry.
!$OMP TARGET DATA &
!$OMP& MAP(ECTRANS_MAP_PRESENT_ALLOC:D_NPTRLS,D_NSTAGTF,F_RACTHE,G_NMEN,G_NLOEN,R_NSMAX)
#endif
#ifdef ACCGPU
!$ACC DATA &
!$ACC& PRESENT(D,D_NPTRLS,D_NSTAGTF,PREEL_COMPLEX,F,F_RACTHE,G,G_NMEN,G_NLOEN,R,R_NSMAX)
#endif

!     ------------------------------------------------------------------

!*       1.    DIVIDE U V AND N-S DERIVATIVES BY A*COS(THETA)
!              ----------------------------------------------

OFFSET_VAR=D%NPTRLS(MYSETW)

IPREEL = SIZE(PREEL_COMPLEX,KIND=JPIB)
INSTAGTF = SIZE(D_NSTAGTF)
INMEN = SIZE(G_NMEN)
INLOEN = SIZE(G_NLOEN)
IRACTHE = SIZE(F_RACTHE)

!*       1.1      U AND V.
CALL FSC_DIVIDE_ACHTE(PREEL_COMPLEX,IPREEL,D_NSTAGTF,INSTAGTF,G_NMEN,INMEN, &
    & F_RACTHE,IRACTHE,IBEG,IEND,IINC,2*KF_UV,R_NSMAX,OFFSET_VAR,KF_FS,KUV_OFFSET)

!*      1.2      N-S DERIVATIVES

IF (KSCALARS_NSDER_OFFSET >= 0) THEN
  CALL FSC_DIVIDE_ACHTE(PREEL_COMPLEX,IPREEL,D_NSTAGTF,INSTAGTF,G_NMEN,INMEN, &
      & F_RACTHE,IRACTHE,IBEG,IEND,IINC,KF_SCALARS,R_NSMAX,OFFSET_VAR,KF_FS, &
      & KSCALARS_NSDER_OFFSET)
ENDIF

!     ------------------------------------------------------------------

!*       2.    EAST-WEST DERIVATIVES
!              ---------------------

!*       2.1      U AND V.

ILOEN_MAX = MAXVAL(G_NLOEN)
IF (KUV_EWDER_OFFSET >= 0) THEN
  CALL FSC_EWDERIV(PREEL_COMPLEX,IPREEL,D_NSTAGTF,INSTAGTF,G_NMEN,INMEN, &
      & G_NLOEN,INLOEN,F_RACTHE,IRACTHE,IBEG,IEND,IINC,2*KF_UV,ILOEN_MAX, &
      & OFFSET_VAR,KF_FS,KUV_OFFSET,KUV_EWDER_OFFSET)
ENDIF

!*       2.2     SCALAR VARIABLES

IF (KSCALARS_EWDER_OFFSET > 0) THEN
  CALL FSC_EWDERIV(PREEL_COMPLEX,IPREEL,D_NSTAGTF,INSTAGTF,G_NMEN,INMEN, &
      & G_NLOEN,INLOEN,F_RACTHE,IRACTHE,IBEG,IEND,IINC,KF_SCALARS,ILOEN_MAX, &
      & OFFSET_VAR,KF_FS,KSCALARS_OFFSET,KSCALARS_EWDER_OFFSET)
ENDIF

#ifdef ACCGPU
!$ACC WAIT(1)

!$ACC END DATA
#endif
#ifdef OMPGPU
!$OMP END TARGET DATA
#endif
!     ------------------------------------------------------------------
END ASSOCIATE

END SUBROUTINE FSC

! Loop bodies with explicit-shape dummy arguments. A POINTER, assumed-shape or derived-type
! component actual is described by a dope vector, and the compiler puts that dope vector in the
! device data environment on every launch: create map entry, copy 48-120 bytes, tear the entry
! down. HAS_DEVICE_ADDR does not suppress it. Explicit-shape dummies are described by their
! extents, which travel as FIRSTPRIVATE scalars, so nothing has to be copied.

! Division by a*cos(theta), shared by the u/v fields and by the north-south derivatives: the two
! differ only in which layer offset they start from and how many layers they cover.
SUBROUTINE FSC_DIVIDE_ACHTE(PREEL_COMPLEX,KPREEL,KNSTAGTF,KNSTAGTF1,KNMEN,KNMEN1, &
    & PRACTHE,KRACTHE1,KBEG,KEND,KINC,KFIELDS,KNSMAX,KOFFSET_VAR,KF_FS,KOFFSET)
USE PARKIND_ECTRANS, ONLY: JPIM, JPRBT, JPIB, JPRD

IMPLICIT NONE

INTEGER(KIND=JPIB), INTENT(IN)    :: KPREEL
INTEGER(KIND=JPIM), INTENT(IN)    :: KNSTAGTF1, KNMEN1, KRACTHE1
INTEGER(KIND=JPIM), INTENT(IN)    :: KBEG, KEND, KINC, KFIELDS, KNSMAX
INTEGER(KIND=JPIM), INTENT(IN)    :: KOFFSET_VAR, KF_FS, KOFFSET
REAL(KIND=JPRBT),   INTENT(INOUT) :: PREEL_COMPLEX(KPREEL)
INTEGER(KIND=JPIB), INTENT(IN)    :: KNSTAGTF(KNSTAGTF1)
INTEGER(KIND=JPIM), INTENT(IN)    :: KNMEN(KNMEN1)
REAL(KIND=JPRD),    INTENT(IN)    :: PRACTHE(KRACTHE1)

INTEGER(KIND=JPIM) :: KGL, JF, JM, IGLG, JMP, INJM, INMEN
INTEGER(KIND=JPIB) :: IOFF_LAT, IOFF
REAL(KIND=JPRBT) :: ZACHTE2

! Wavenumber loop strip-mined so the per-(KGL,JF) invariants are loaded once per thread.
! KNMEN(IGLG), both KNSTAGTF lookups and the PRACTHE weight do not depend on JM, but
! collapsed over (KGL,JF,JM) they were re-loaded for every wavenumber, which is the 4.6:1
! vL1d-to-HBM byte ratio the roofline reports for this kernel. The parallel index stays the
! unit-stride component and the serial loop strides by the number of parallel positions, so
! consecutive lanes still touch consecutive wavenumbers and the coalescing on
! PREEL_COMPLEX is unchanged. See TRLTOM_UNPACK_KERNEL for the tile-size sweep.
INJM = (KNSMAX+ITILE)/ITILE

#ifdef OMPGPU
!$OMP TARGET TEAMS DISTRIBUTE PARALLEL DO COLLAPSE(3) DEFAULT(ECTRANS_OMP_DEFAULT) &
!$OMP& PRIVATE(IGLG,IOFF_LAT,IOFF,ZACHTE2,JM,INMEN) &
!$OMP& SHARED(PREEL_COMPLEX,KNSTAGTF,KNMEN,PRACTHE) &
!$OMP& MAP(ECTRANS_MAP_PRESENT_ALLOC:KNSTAGTF,KNMEN,PRACTHE) &
!$OMP& ECTRANS_LOOP_BOUNDS_CLAUSE(KBEG,KEND,KINC,KFIELDS,KNSMAX) &
!$OMP& FIRSTPRIVATE(KOFFSET_VAR,KF_FS,KOFFSET,INJM)
#endif
#ifdef ACCGPU
!$ACC PARALLEL LOOP COLLAPSE(3) DEFAULT(NONE) &
!$ACC& PRIVATE(IGLG,IOFF_LAT,IOFF,ZACHTE2,JM,JF,KGL,INMEN) &
!$ACC& PRESENT(PREEL_COMPLEX,KNSTAGTF,KNMEN,PRACTHE) &
!$ACC& FIRSTPRIVATE(KBEG,KEND,KINC,KOFFSET_VAR,KFIELDS,KOFFSET,KF_FS,KNSMAX,INJM) &
#ifdef _CRAYFTN
!! NOTE: These asynchronous kernels are triggering the error: HIPFFT_PARSE_ERROR
!$ACC&
#else
!$ACC& ASYNC(1)
#endif
#endif
DO KGL=KBEG,KEND,KINC
  DO JF=1,KFIELDS
    DO JMP=0,INJM-1
      IGLG    = KOFFSET_VAR+KGL-1
      INMEN   = KNMEN(IGLG)
      IOFF_LAT = 1_JPIB*KF_FS*KNSTAGTF(KGL)
      IOFF = IOFF_LAT+(KOFFSET+JF-1)*(KNSTAGTF(KGL+1)-KNSTAGTF(KGL))
      ZACHTE2 = REAL(PRACTHE(IGLG),JPRBT)
#ifdef ACCGPU
      !$ACC LOOP SEQ
#endif
      DO JM=JMP,KNSMAX,INJM !(note that KNSMAX <= KNMEN(IGLG) for all IGLG)
        IF (JM <= INMEN) THEN
          PREEL_COMPLEX(IOFF+2*JM+1) = &
              & PREEL_COMPLEX(IOFF+2*JM+1)*ZACHTE2
          PREEL_COMPLEX(IOFF+2*JM+2) = &
              & PREEL_COMPLEX(IOFF+2*JM+2)*ZACHTE2
        ENDIF
      ENDDO
    ENDDO
  ENDDO
ENDDO
END SUBROUTINE FSC_DIVIDE_ACHTE

! East-west derivative, shared by the u/v fields and by the scalar variables: the two differ only
! in which layer offsets they read from and write to, and how many layers they cover.
SUBROUTINE FSC_EWDERIV(PREEL_COMPLEX,KPREEL,KNSTAGTF,KNSTAGTF1,KNMEN,KNMEN1, &
    & KNLOEN,KNLOEN1,PRACTHE,KRACTHE1,KBEG,KEND,KINC,KFIELDS,KLOEN_MAX, &
    & KOFFSET_VAR,KF_FS,KOFFSET,KEWDER_OFFSET)
USE PARKIND_ECTRANS, ONLY: JPIM, JPRBT, JPIB, JPRD

IMPLICIT NONE

INTEGER(KIND=JPIB), INTENT(IN)    :: KPREEL
INTEGER(KIND=JPIM), INTENT(IN)    :: KNSTAGTF1, KNMEN1, KNLOEN1, KRACTHE1
INTEGER(KIND=JPIM), INTENT(IN)    :: KBEG, KEND, KINC, KFIELDS, KLOEN_MAX
INTEGER(KIND=JPIM), INTENT(IN)    :: KOFFSET_VAR, KF_FS, KOFFSET, KEWDER_OFFSET
REAL(KIND=JPRBT),   INTENT(INOUT) :: PREEL_COMPLEX(KPREEL)
INTEGER(KIND=JPIB), INTENT(IN)    :: KNSTAGTF(KNSTAGTF1)
INTEGER(KIND=JPIM), INTENT(IN)    :: KNMEN(KNMEN1)
INTEGER(KIND=JPIM), INTENT(IN)    :: KNLOEN(KNLOEN1)
REAL(KIND=JPRD),    INTENT(IN)    :: PRACTHE(KRACTHE1)

INTEGER(KIND=JPIM) :: KGL, JF, JM, IGLG, JMP, INJM, INMEN, ILOEN2
INTEGER(KIND=JPIB) :: IOFF_LAT, IOFF, IOFF_EWDER
REAL(KIND=JPRBT) :: ZACHTE2
REAL(KIND=JPRBT) :: RET_REAL, RET_COMPLEX

! Wavenumber loop strip-mined as in FSC_DIVIDE_ACHTE above. KNLOEN(IGLG), KNMEN(IGLG), both
! KNSTAGTF lookups and the PRACTHE weight are all invariant in JM, and this kernel carries
! five such loads against two payload accesses -- a 5.1:1 vL1d-to-HBM byte ratio, the worst
! of the class.
INJM = (KLOEN_MAX/2+ITILE)/ITILE

#ifdef OMPGPU
!$OMP TARGET TEAMS DISTRIBUTE PARALLEL DO COLLAPSE(3) DEFAULT(ECTRANS_OMP_DEFAULT) &
!$OMP& PRIVATE(IGLG,IOFF_LAT,IOFF,IOFF_EWDER,RET_REAL,RET_COMPLEX,ZACHTE2,JM,INMEN,ILOEN2) &
!$OMP& SHARED(PREEL_COMPLEX,KNSTAGTF,KNMEN,KNLOEN,PRACTHE) &
!$OMP& MAP(ECTRANS_MAP_PRESENT_ALLOC:KNSTAGTF,KNMEN,KNLOEN,PRACTHE) &
!$OMP& ECTRANS_LOOP_BOUNDS_CLAUSE(KBEG,KEND,KINC,KFIELDS,KLOEN_MAX) &
!$OMP& FIRSTPRIVATE(KOFFSET_VAR,KF_FS,KOFFSET,KEWDER_OFFSET,INJM)
#endif
#ifdef ACCGPU
!$ACC PARALLEL LOOP COLLAPSE(3) DEFAULT(NONE) &
!$ACC& PRIVATE(IGLG,IOFF_LAT,IOFF,IOFF_EWDER,RET_REAL,RET_COMPLEX,ZACHTE2,JM,JF,KGL,INMEN,ILOEN2) &
!$ACC& PRESENT(PREEL_COMPLEX,KNSTAGTF,KNMEN,KNLOEN,PRACTHE) &
!$ACC& FIRSTPRIVATE(KBEG,KEND,KINC,KOFFSET_VAR,KFIELDS,KEWDER_OFFSET,KOFFSET,KF_FS,KLOEN_MAX,INJM) &
#ifdef _CRAYFTN
!! NOTE: These asynchronous kernels are triggering the error: HIPFFT_PARSE_ERROR
!$ACC&
#else
!$ACC& ASYNC(1)
#endif
#endif
DO KGL=KBEG,KEND,KINC
  DO JF=1,KFIELDS
    DO JMP=0,INJM-1
      IGLG = KOFFSET_VAR+KGL-1
      ILOEN2 = KNLOEN(IGLG)/2
      INMEN = KNMEN(IGLG)
      IOFF_LAT = 1_JPIB*KF_FS*KNSTAGTF(KGL)
      IOFF = IOFF_LAT+(KOFFSET+JF-1)*(KNSTAGTF(KGL+1)-KNSTAGTF(KGL))
      IOFF_EWDER = IOFF_LAT+(KEWDER_OFFSET+JF-1)*(KNSTAGTF(KGL+1)-KNSTAGTF(KGL))
      ZACHTE2 = REAL(PRACTHE(IGLG),JPRBT)
#ifdef ACCGPU
      !$ACC LOOP SEQ
#endif
      DO JM=JMP,KLOEN_MAX/2,INJM
        ! FFT transforms NLON real values to floor(NLON/2)+1 complex numbers. Hence we have
        ! to fill those floor(NLON/2)+1 values.
        ! Truncation happens starting at KNMEN+1. Hence, we zero-fill those values.
        IF (JM <= ILOEN2) THEN
          RET_REAL = 0.0_JPRBT
          RET_COMPLEX = 0.0_JPRBT

          IF (JM <= INMEN) THEN
            RET_REAL = &
                & -PREEL_COMPLEX(IOFF+2*JM+2)*ZACHTE2*REAL(JM,JPRBT)
            RET_COMPLEX =  &
                &  PREEL_COMPLEX(IOFF+2*JM+1)*ZACHTE2*REAL(JM,JPRBT)
          ENDIF
          ! The rest from KNMEN(IGLG+1)...MAX is zero truncated
          PREEL_COMPLEX(IOFF_EWDER+2*JM+1) = RET_REAL
          PREEL_COMPLEX(IOFF_EWDER+2*JM+2) = RET_COMPLEX
        ENDIF
      ENDDO
    ENDDO
  ENDDO
ENDDO
END SUBROUTINE FSC_EWDERIV
END MODULE FSC_MOD
