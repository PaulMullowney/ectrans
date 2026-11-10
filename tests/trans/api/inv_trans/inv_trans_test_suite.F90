! (C) Copyright 2026- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

MODULE INV_TRANS_TEST_SUITE

USE PARKIND1, ONLY: JPIM, JPRB, JPRD
USE MPL_MODULE, ONLY: MPL_INIT, MPL_NPROC, MPL_MYRANK, MPL_ALLREDUCE, MPL_END

IMPLICIT NONE

#include "setup_trans0.h"
#include "setup_trans.h"
#include "trans_inq.h"
#include "inv_trans.h"
#include "trans_end.h"

! Spectral truncation used for all tests below
INTEGER(KIND=JPIM), PARAMETER :: JPTRUNCATION = 21

! Number of latitudes used for all tests below
INTEGER(KIND=JPIM), PARAMETER :: JPNGDL = 2 * (JPTRUNCATION + 1)

! Tolerance for "close to"
REAL(KIND=JPRB), PARAMETER :: PPTOLERANCE = 100.0_JPRB * EPSILON(1.0_JPRB)

! JPPROMA blocking factor
INTEGER(KIND=JPIM), PARAMETER :: JPPROMA = 16

! Earth radius in metres
REAL(KIND=JPRD), PARAMETER :: PPEARTH_RADIUS = 6371229.0_JPRD

LOGICAL :: LUSE_MPI

! Whether this driver can place grid-point arrays on the device itself
#if defined(OMPGPU) || defined(ACCGPU)
LOGICAL, PARAMETER :: LLGPU = .TRUE.
#else
LOGICAL, PARAMETER :: LLGPU = .FALSE.
#endif

! Value of grid-point data that the transform must not overwrite
REAL(KIND=JPRB), PARAMETER :: PPJUNK = 1.0E10_JPRB

CONTAINS

!---------------------------------------------------------------------------------------------------

! Approximate equality check for reals
ELEMENTAL LOGICAL FUNCTION APPROX_EQ(PA, PB) RESULT(KRET)
  REAL(KIND=JPRB), INTENT(IN) :: PA
  REAL(KIND=JPRB), INTENT(IN) :: PB

  KRET = ABS(PA - PB) < PPTOLERANCE
END FUNCTION APPROX_EQ

! Zonal wind U = COS(latitude) of the rotational wind whose vorticity is the (m=0, n=1) harmonic
FUNCTION ROTATIONAL_WIND_U(KGPBLKS, K_REGIONS_NS, K_REGIONS_EW) RESULT(PU)
  INTEGER(KIND=JPIM), INTENT(IN) :: KGPBLKS
  INTEGER(KIND=JPIM), INTENT(IN) :: K_REGIONS_NS
  INTEGER(KIND=JPIM), INTENT(IN) :: K_REGIONS_EW

  REAL(KIND=JPRB), ALLOCATABLE :: PU(:,:)

  INTEGER(KIND=JPIM) :: IPTRFLOFF, IMY_REGION_NS, IMY_REGION_EW
  INTEGER(KIND=JPIM), DIMENSION(K_REGIONS_NS) :: IFRSTLAT, ILSTLAT
  REAL(KIND=JPRD) :: ZMU(JPNGDL)
  INTEGER(KIND=JPIM), DIMENSION(JPNGDL + K_REGIONS_NS - 1, K_REGIONS_EW) :: NSTA, NONL
  INTEGER(KIND=JPIM) :: ILAT, IBL, JROF, JGLAT, ISTLON, IENDLON, JLON

  CALL TRANS_INQ(KPTRFLOFF=IPTRFLOFF, KMY_REGION_NS=IMY_REGION_NS, KFRSTLAT=IFRSTLAT, &
    &            KLSTLAT=ILSTLAT, PMU=ZMU, KMY_REGION_EW=IMY_REGION_EW, KSTA=NSTA, KONL=NONL)
  ILAT = IPTRFLOFF

  ALLOCATE(PU(JPPROMA, KGPBLKS))
  PU(:,:) = 0.0_JPRB

  IBL = 1
  JROF = 1
  DO JGLAT = IFRSTLAT(IMY_REGION_NS), ILSTLAT(IMY_REGION_NS)
    ILAT = ILAT + 1
    ISTLON = NSTA(ILAT, IMY_REGION_EW)
    IENDLON = ISTLON - 1 + NONL(ILAT, IMY_REGION_EW)
    DO JLON = ISTLON, IENDLON
      PU(JROF, IBL) = COS(ASIN(ZMU(JGLAT)))
      JROF = JROF + 1
      IF (JROF > JPPROMA) THEN
        JROF = 1
        IBL  = IBL + 1
      ENDIF
    ENDDO
  ENDDO
END FUNCTION ROTATIONAL_WIND_U

! Determine if this task handles the m=0 mode
LOGICAL FUNCTION HAVE_M0_MODE()
  INTEGER :: NUM_MY_ZON_WNS
  INTEGER, ALLOCATABLE :: MY_ZON_WNS(:)

  CALL TRANS_INQ(KNUMP=NUM_MY_ZON_WNS)
  ALLOCATE(MY_ZON_WNS(NUM_MY_ZON_WNS))
  CALL TRANS_INQ(KMYMS=MY_ZON_WNS)

  HAVE_M0_MODE = ANY(MY_ZON_WNS == 0)
END FUNCTION HAVE_M0_MODE

! Setup fixture
SUBROUTINE SETUP_TEST(KSPEC2, KGPTOT, KGPBLKS, K_REGIONS_NS, K_REGIONS_EW)
  USE UTIL, ONLY: DETECT_MPIRUN

  INTEGER(KIND=JPIM), INTENT(OUT) :: KSPEC2
  INTEGER(KIND=JPIM), INTENT(OUT) :: KGPTOT
  INTEGER(KIND=JPIM), INTENT(OUT) :: KGPBLKS
  INTEGER(KIND=JPIM), INTENT(OUT) :: K_REGIONS_NS
  INTEGER(KIND=JPIM), INTENT(OUT) :: K_REGIONS_EW

  INTEGER(KIND=JPIM) :: ILOEN(JPNGDL)
  INTEGER(KIND=JPIM) :: I
  INTEGER(KIND=JPIM) :: IPROC

  ! Set up MPI
  LUSE_MPI = DETECT_MPIRUN()
  IF (LUSE_MPI) THEN
    CALL MPL_INIT
    IPROC = MPL_NPROC()
  ELSE
    IPROC = 1
  ENDIF

  CALL SETUP_TRANS0(LDMPOFF=.NOT. LUSE_MPI, KPRGPNS=IPROC, KPRGPEW=1, KPRTRW=IPROC, &
    &               K_REGIONS_NS=K_REGIONS_NS, K_REGIONS_EW=K_REGIONS_EW, PRAD=PPEARTH_RADIUS)

  ! Define octahedral grid
  DO I = 1, JPTRUNCATION + 1
    ILOEN(I) = 20 + 4 * I
    ILOEN(JPNGDL - I + 1) = ILOEN(I)
  END DO

  CALL SETUP_TRANS(KSMAX=JPTRUNCATION, KDGL=JPNGDL, KLOEN=ILOEN)

  CALL TRANS_INQ(KSPEC2=KSPEC2, KGPTOT=KGPTOT)

  ! Number of JPPROMA blocks
  KGPBLKS = (KGPTOT - 1) / JPPROMA + 1
END SUBROUTINE SETUP_TEST

! Tear down fixture
SUBROUTINE CLEANUP_TEST

  CALL TRANS_END

  IF (LUSE_MPI) THEN
    CALL MPL_END(LDMEMINFO=.FALSE.)
  ENDIF
END SUBROUTINE CLEANUP_TEST

!---------------------------------------------------------------------------------------------------

! Test INV_TRANS with call mode 2 and wind, "3A" and surface fields returned in non-contiguous
! sections (fields 2:3 of 3, 2 of 2 and 2 of 2, with JPPROMA blocking). The spectra are the
! vorticity of the rotational wind U = COS(latitude), V = 0, and scalar fields of ones. The other
! fields hold PPJUNK, which must not be overwritten. On the GPU the arrays are device-resident and
! the results are read back from the device.
INTEGER FUNCTION ECTRANS_TEST_TRANS_API_INV_TRANS_CALL_MODE_2_PGP_ON_GPU_NONCONTIGUOUS() RESULT(KRET) BIND(C)
  REAL(KIND=JPRB), ALLOCATABLE :: ZGPUV(:,:,:,:), ZGP3A(:,:,:,:), ZGP2(:,:,:), ZU(:,:)
  REAL(KIND=JPRB), ALLOCATABLE :: ZSPVOR(:,:), ZSPDIV(:,:), ZSPSC3A(:,:,:), ZSPSC2(:,:)
  INTEGER(KIND=JPIM) :: ISPEC2, IGPTOT, IGPBLKS, I_REGIONS_NS, I_REGIONS_EW, IBL, ILEN

  ! Set up everything
  CALL SETUP_TEST(ISPEC2, IGPTOT, IGPBLKS, I_REGIONS_NS, I_REGIONS_EW)
  ALLOCATE(ZSPVOR(1,ISPEC2), ZSPDIV(1,ISPEC2), ZSPSC3A(1,ISPEC2,1), ZSPSC2(1,ISPEC2))
  ZSPVOR(:,:) = 0.0_JPRB
  ZSPDIV(:,:) = 0.0_JPRB
  ZSPSC3A(:,:,:) = 0.0_JPRB
  ZSPSC2(:,:) = 0.0_JPRB
  IF (HAVE_M0_MODE()) THEN
    ! epsilon_{m=0}^{n=1} = sqrt((n^2 - m^2)/(4n^2 - 1)) = sqrt(1/3)
    ZSPVOR(1,3) = 2.0_JPRB * SQRT(1.0_JPRB / 3.0_JPRB) / PPEARTH_RADIUS
    ZSPSC3A(1,1,1) = 1.0_JPRB
    ZSPSC2(1,1) = 1.0_JPRB
  ENDIF
  ALLOCATE(ZGPUV(JPPROMA,1,3,IGPBLKS), ZGP3A(JPPROMA,1,2,IGPBLKS), ZGP2(JPPROMA,2,IGPBLKS))
  ZGPUV(:,:,:,:) = PPJUNK
  ZGP3A(:,:,:,:) = PPJUNK
  ZGP2(:,:,:) = PPJUNK

#if defined(OMPGPU)
  !$OMP TARGET ENTER DATA MAP(TO: ZGPUV, ZGP3A, ZGP2)
#elif defined(ACCGPU)
  !$ACC ENTER DATA COPYIN(ZGPUV, ZGP3A, ZGP2)
#endif

  CALL INV_TRANS(PSPVOR=ZSPVOR, PSPDIV=ZSPDIV, PSPSC3A=ZSPSC3A, PSPSC2=ZSPSC2, &
    &            PGPUV=ZGPUV(:,:,2:3,:), PGP3A=ZGP3A(:,:,2:2,:), PGP2=ZGP2(:,2:2,:), KPROMA=JPPROMA, &
    &            LPGP_ON_GPU=LLGPU)

#if defined(OMPGPU)
  !$OMP TARGET EXIT DATA MAP(FROM: ZGPUV, ZGP3A, ZGP2)
#elif defined(ACCGPU)
  !$ACC EXIT DATA COPYOUT(ZGPUV, ZGP3A, ZGP2)
#endif

  ZU = ROTATIONAL_WIND_U(IGPBLKS, I_REGIONS_NS, I_REGIONS_EW)

  ! Compare the points of each block that hold grid points
  KRET = 0
  DO IBL = 1, IGPBLKS
    ILEN = MIN(JPPROMA, IGPTOT - (IBL - 1) * JPPROMA)
    IF (ANY(.NOT. APPROX_EQ(ZGPUV(1:ILEN,1,2,IBL), ZU(1:ILEN,IBL)))) KRET = 1
    IF (ANY(.NOT. APPROX_EQ(ZGPUV(1:ILEN,1,3,IBL), 0.0_JPRB))) KRET = 1
    IF (ANY(.NOT. APPROX_EQ(ZGP3A(1:ILEN,1,2,IBL), 1.0_JPRB))) KRET = 1
    IF (ANY(.NOT. APPROX_EQ(ZGP2(1:ILEN,2,IBL), 1.0_JPRB))) KRET = 1
  ENDDO

  ! The fields outside the sections are untouched
  IF (ANY(ZGPUV(:,:,1,:) /= PPJUNK) .OR. ANY(ZGP3A(:,:,1,:) /= PPJUNK) .OR. ANY(ZGP2(:,1,:) /= PPJUNK)) KRET = 1

  IF (LUSE_MPI) CALL MPL_ALLREDUCE(KRET, CDOPER="MAX")

  ! Tear down everything
  DEALLOCATE(ZU, ZGP2, ZGP3A, ZGPUV, ZSPSC2, ZSPSC3A, ZSPDIV, ZSPVOR)
  CALL CLEANUP_TEST
END FUNCTION ECTRANS_TEST_TRANS_API_INV_TRANS_CALL_MODE_2_PGP_ON_GPU_NONCONTIGUOUS

!---------------------------------------------------------------------------------------------------

END MODULE INV_TRANS_TEST_SUITE
