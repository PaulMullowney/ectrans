! (C) Copyright 1991- ECMWF.
! (C) Copyright 1991- Meteo-France.
! (C) Copyright 2022- NVIDIA.
! 
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!

MODULE UVTVD_MOD
CONTAINS
SUBROUTINE UVTVD(KF_UV,KLDUV,KLDVD,PU,PV,PVOR,PDIV)

!**** *UVTVD* - Compute vor/div from u and v in spectral space

!     Purpose.
!     --------
!        To compute vorticity and divergence from u and v in spectral
!       space. Input u and v from KM to NTMAX+1, output vorticity and
!       divergence from KM to NTMAX.

!**   Interface.
!     ----------
!        CALL UVTVD(KM,KF_UV,PEPSNM,PU,PV,PVOR,PDIV)

!        Explicit arguments :  KM - zonal wave-number
!        --------------------  KF_UV - number of fields (levels)
!                              KLDUV - leading dimension of the buffer PU and PV
!                                      are slices of
!                              KLDVD - leading dimension of the buffer PVOR and PDIV
!                                      are slices of
!                              PEPSNM - REPSNM for wavenumber KM
!                              PU - u wind component for zonal
!                                   wavenumber KM
!                              PV - v wind component for zonal
!                                   wavenumber KM
!                              PVOR - vorticity for zonal
!                                     wavenumber KM
!                              PDIV - divergence for zonal
!                                     wavenumber KM


!     Method.  See ref.
!     -------

!     Externals.  None.
!     ----------

!     Reference.
!     ----------
!        ECMWF Research Department documentation of the IFS

!     Author.
!     -------
!        Mats Hamrud and Philippe Courtier  *ECMWF*

!     Modifications.
!     --------------
!        Original : 91-07-01
!        D. Giard : NTMAX instead of NSMAX
!     ------------------------------------------------------------------

USE PARKIND_ECTRANS, ONLY: JPIM, JPRBT
USE TPM_DIM,         ONLY: R
USE TPM_DISTR,       ONLY: D
USE TPM_FIELDS_GPU,  ONLY: FG
!

IMPLICIT NONE

!     DUMMY INTEGER SCALARS
INTEGER(KIND=JPIM), INTENT(IN)  :: KF_UV
! The four fields are explicit shape so that no array descriptor has to be mapped to the
! device on every launch of the compute constructs below. They are slices of two different
! spectral buffers, so their first extents are those buffers' leading dimensions and the
! caller passes the base element of each slice; only 1:2*KF_UV of each is addressed here.
INTEGER(KIND=JPIM), INTENT(IN)  :: KLDUV, KLDVD
REAL(KIND=JPRBT), INTENT(OUT)    :: PVOR(KLDVD,R%NTMAX+3,D%NUMP),PDIV(KLDVD,R%NTMAX+3,D%NUMP)
REAL(KIND=JPRBT), INTENT(INOUT)  :: PU  (KLDUV,R%NTMAX+3,D%NUMP),PV  (KLDUV,R%NTMAX+3,D%NUMP)
INTEGER(KIND=JPIM)  :: KM, KMLOC

!     LOCAL INTEGER SCALARS
INTEGER(KIND=JPIM) :: II, IN, IR, J, JN

!     LOCAL REAL SCALARS
REAL(KIND=JPRBT) :: ZKM,ZJN

!     ------------------------------------------------------------------
ASSOCIATE(D_NUMP=>D%NUMP, R_NTMAX=>R%NTMAX, D_MYMS=>D%MYMS, ZEPSNM=>FG%ZEPSNM)

!*       1.    COMPUTE U V FROM VORTICITY AND DIVERGENCE.
!              ------------------------------------------

#ifdef OMPGPU
! PU/PV/PVOR/PDIV are backed by the growing allocator. Their storage is registered with
! omp_target_associate_ptr, so ordinary mapping resolves it inside the nested compute
! constructs, but it is never entered in the present table and so cannot be MAP(PRESENT)'d.
! D, R and FG are reached only through their ASSOCIATE aliases. Naming the parent types here
! makes the runtime walk every allocatable component of those derived types and re-copy each
! component descriptor on entry, so only the aliases are mapped.
!$OMP TARGET DATA MAP(ECTRANS_MAP_PRESENT_ALLOC:D_MYMS,D_NUMP,R_NTMAX,ZEPSNM)
#endif
#ifdef ACCGPU
!$ACC DATA &
!$ACC& PRESENT(D,D_MYMS,D_NUMP,R,R_NTMAX) &
!$ACC& PRESENT(FG,ZEPSNM,PU,PV,PVOR,PDIV) ASYNC(1)
#endif

!*       1.1      SET N=KM-1 COMPONENT TO 0 FOR U AND V

#ifdef OMPGPU
!$OMP TARGET TEAMS DISTRIBUTE PARALLEL DO COLLAPSE(2) PRIVATE(KM) &
!$OMP& SHARED(PU,PV) &
!$OMP& ECTRANS_LOOP_BOUNDS_CLAUSE(KF_UV) DEFAULT(ECTRANS_OMP_DEFAULT)
#endif
#ifdef ACCGPU
!$ACC PARALLEL LOOP COLLAPSE(2) PRIVATE(KM) FIRSTPRIVATE(KF_UV) DEFAULT(NONE) &
#ifndef _CRAYFTN
!$ACC& ASYNC(1)
#else
!$ACC&
#endif
#endif
DO KMLOC=1,D_NUMP
  DO J=1,2*KF_UV
    KM = D_MYMS(KMLOC)
    PU(J,R_NTMAX+4-KM,KMLOC) = 0.0_JPRBT
    PV(J,R_NTMAX+4-KM,KMLOC) = 0.0_JPRBT
  ENDDO
ENDDO

!*       1.2      COMPUTE VORTICITY AND DIVERGENCE.

#ifdef OMPGPU
!$OMP TARGET TEAMS DISTRIBUTE PARALLEL DO COLLAPSE(3) PRIVATE(IR,II,IN,KM,ZKM,ZJN) &
!$OMP& SHARED(PVOR,PV,PU,PDIV) &
!$OMP& ECTRANS_LOOP_BOUNDS_CLAUSE(KF_UV) DEFAULT(ECTRANS_OMP_DEFAULT)
#endif
#ifdef ACCGPU
!$ACC PARALLEL LOOP COLLAPSE(3) PRIVATE(IR,II,IN,KM,ZKM,ZJN) FIRSTPRIVATE(KF_UV) DEFAULT(NONE) &
#ifndef _CRAYFTN
!$ACC& ASYNC(1)
#else
!$ACC&
#endif
#endif
DO KMLOC=1,D_NUMP
  DO JN=0,R_NTMAX
    DO J=1,KF_UV
      IR = 2*J-1
      II = IR+1
      KM = D_MYMS(KMLOC)
      ZKM = REAL(KM,JPRBT)

      IF(KM /= 0 .AND. JN >= KM) THEN
        ! (DO JN=KN,R_NTMAX)
        IN = R_NTMAX+3-JN
        ZJN = JN

        PVOR(IR,IN,KMLOC) = -ZKM*PV(II,IN,KMLOC)-&
         &ZJN*ZEPSNM(KMLOC,JN+1)*PU(IR,IN-1,KMLOC)+&
         &(ZJN+1)*ZEPSNM(KMLOC,JN)*PU(IR,IN+1,KMLOC)
        PVOR(II,IN,KMLOC) = +ZKM*PV(IR,IN,KMLOC)-&
         &ZJN*ZEPSNM(KMLOC,JN+1)*PU(II,IN-1,KMLOC)+&
         &(ZJN+1)*ZEPSNM(KMLOC,JN)*PU(II,IN+1,KMLOC)
        PDIV(IR,IN,KMLOC) = -ZKM*PU(II,IN,KMLOC)+&
         &ZJN*ZEPSNM(KMLOC,JN+1)*PV(IR,IN-1,KMLOC)-&
         &(ZJN+1)*ZEPSNM(KMLOC,JN)*PV(IR,IN+1,KMLOC)
        PDIV(II,IN,KMLOC) = +ZKM*PU(IR,IN,KMLOC)+&
         &ZJN*ZEPSNM(KMLOC,JN+1)*PV(II,IN-1,KMLOC)-&
         &(ZJN+1)*ZEPSNM(KMLOC,JN)*PV(II,IN+1,KMLOC)

      ELSEIF(KM == 0) THEN
        ! (DO JN=0,R_NTMAX)
        IN = R_NTMAX+3-JN
        ZJN = JN

        PVOR(IR,IN,KMLOC) = -&
         &ZJN*ZEPSNM(KMLOC,JN+1)*PU(IR,IN-1,KMLOC)+&
         &(ZJN+1)*ZEPSNM(KMLOC,JN)*PU(IR,IN+1,KMLOC)
        PDIV(IR,IN,KMLOC) = &
         &ZJN*ZEPSNM(KMLOC,JN+1)*PV(IR,IN-1,KMLOC)-&
         &(ZJN+1)*ZEPSNM(KMLOC,JN)*PV(IR,IN+1,KMLOC)
      ENDIF
    ENDDO
  ENDDO
ENDDO

#ifdef ACCGPU
!$ACC END DATA
#endif
#ifdef OMPGPU
!$OMP END TARGET DATA
#endif
!     ------------------------------------------------------------------
END ASSOCIATE

END SUBROUTINE UVTVD
END MODULE UVTVD_MOD
