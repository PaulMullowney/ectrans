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

MODULE PRFI1B_MOD
  CONTAINS
  SUBROUTINE PRFI1B(PIA,PSPEC,KFIELDS,KDIM,KFLDPTR)
  
  USE PARKIND1,        ONLY: JPIM, JPRB
  USE TPM_DIM,         ONLY: R
  USE TPM_DISTR,       ONLY: D
  USE ABORT_TRANS_MOD, ONLY: ABORT_TRANS
  
  !**** *PRFI1* - Prepare spectral fields for inverse Legendre transform
  
  !     Purpose.
  !     --------
  !        To extract the spectral fields for a specific zonal wavenumber
  !        and put them in an order suitable for the inverse Legendre           .
  !        tranforms.The ordering is from NSMAX to KM for better conditioning.
  !        Elements 1,2 and NLCM(KM)+1 are zeroed in preparation for computing
  !        u,v and derivatives in spectral space.
  
  !**   Interface.
  !     ----------
  !        *CALL* *PRFI1B(...)*
  
  !        Explicit arguments :  KM     - zonal wavenumber
  !        ------------------    PIA    - spectral components for transform
  !                              PSPEC  - spectral array
  !                              KFIELDS  - number of fields
  
  
  !        Implicit arguments :  None.
  !        --------------------
  
  !     Method.
  !     -------
  
  !     Externals.   None.
  !     ----------
  
  !     Reference.
  !     ----------
  !        ECMWF Research Department documentation of the IFS
  
  !     Author.
  !     -------
  !        Mats Hamrud and Philippe Courtier  *ECMWF*
  
  !     Modifications.
  !     --------------
  !        Original : 00-02-01 From PRFI1B in IFS CY22R1
  
  !     ------------------------------------------------------------------
  
  IMPLICIT NONE
  
  INTEGER(KIND=JPIM),INTENT(IN)   :: KFIELDS
  ! CONTIGUOUS so that handing PSPEC to the explicit-shape dummy of PRFI1B_EXTRACT passes a base
  ! address. Without it the compiler must allow for a non-contiguous actual and would pack into a
  ! temporary, which would not be the storage the caller mapped.
  REAL(KIND=JPRB)   ,INTENT(IN)   ,CONTIGUOUS :: PSPEC(:,:)
  REAL(KIND=JPRB)   ,INTENT(INOUT)  :: PIA(:,:,:)
  INTEGER(KIND=JPIM),INTENT(IN) :: KDIM
  INTEGER(KIND=JPIM),INTENT(IN),OPTIONAL :: KFLDPTR(:)
  
  !     LOCAL INTEGER SCALARS
  ! Extents for the explicit-shape dummies of PRFI1B_EXTRACT.
  INTEGER(KIND=JPIM) :: ISPEC1, INASM0_UB
  
  !     ------------------------------------------------------------------
  
  !*       1.    EXTRACT FIELDS FROM SPECTRAL ARRAYS.
  !              --------------------------------------------------

  ASSOCIATE(D_NUMP=>D%NUMP, D_MYMS=>D%MYMS, D_NASM0=>D%NASM0, R_NSMAX=>R%NSMAX)

#ifdef ACCGPU
  !$ACC DATA PRESENT(D,D_NUMP,R,R_NSMAX,D_MYMS,D_NASM0,PIA,PSPEC) ASYNC(1)
#endif
#ifdef OMPGPU
  ! PIA and PSPEC are dropped from the present check, not from the data environment.
  ! PIA's storage comes from the growing allocator (omp_target_alloc +
  ! omp_target_associate_ptr) and PSPEC is a caller-supplied spectral array already
  ! resident on the device. Neither descriptor is entered in the present table, so
  ! MAP(PRESENT) cannot succeed, but both remain in SHARED on the compute construct
  ! below and resolve through ordinary mapping.
  ! D and R are reached only through their ASSOCIATE aliases. Naming the parent types
  ! here makes the runtime walk every allocatable component of TYPE_DISTR and TYPE_DIM
  ! and re-copy each component descriptor on entry, so only the aliases are mapped.
  !$OMP TARGET DATA MAP(ECTRANS_MAP_PRESENT_ALLOC:D_NUMP,R_NSMAX,D_MYMS,D_NASM0)
#endif

  IF(PRESENT(KFLDPTR)) THEN
 
    CALL ABORT_TRANS("KFLDPTR not implemented for GPU")

  ELSE

    !loop over wavenumber

    ISPEC1 = SIZE(PSPEC,1)
    INASM0_UB = UBOUND(D_NASM0,1)

    CALL PRFI1B_EXTRACT(PIA,PSPEC,ISPEC1,KDIM,D_MYMS,D_NUMP,D_NASM0,INASM0_UB, &
      &                 KFIELDS,R_NSMAX)

  ENDIF

#ifdef ACCGPU
  !$ACC END DATA
#endif
#ifdef OMPGPU
  !$OMP END TARGET DATA
#endif

  END ASSOCIATE

  !     ------------------------------------------------------------------

  END SUBROUTINE PRFI1B

  ! Loop body with explicit-shape dummy arguments. A POINTER, assumed-shape or derived-type
  ! component actual is described by a dope vector, and the compiler puts that dope vector in the
  ! device data environment on every launch: create map entry, copy 48-120 bytes, tear the entry
  ! down. HAS_DEVICE_ADDR does not suppress it. Explicit-shape dummies are described by their
  ! extents, which travel as FIRSTPRIVATE scalars, so nothing has to be copied.
  !
  ! PIA is the exception. Every actual is a leading-dimension section of the LTINV work array
  ! (PIA(IFIRST+1:IFIRST+2*KF_UV,:,:) and similar), which is not contiguous, and an element of a
  ! non-contiguous array may not be sequence-associated with an explicit-shape dummy
  ! (F2018 15.5.2.4). It stays assumed-shape and keeps its descriptor.
  SUBROUTINE PRFI1B_EXTRACT(PIA,PSPEC,KSPEC1,KDIM,KMYMS,KNUMP,KNASM0,KNASM0_UB, &
    &                       KFIELDS,KNSMAX)
  USE PARKIND1,        ONLY: JPIM, JPRB

  IMPLICIT NONE

  INTEGER(KIND=JPIM),INTENT(IN)    :: KSPEC1, KDIM, KNUMP, KNASM0_UB
  INTEGER(KIND=JPIM),INTENT(IN)    :: KFIELDS, KNSMAX
  REAL(KIND=JPRB)   ,INTENT(INOUT) :: PIA(:,:,:)
  REAL(KIND=JPRB)   ,INTENT(IN)    :: PSPEC(KSPEC1,KDIM)
  INTEGER(KIND=JPIM),INTENT(IN)    :: KMYMS(KNUMP)
  ! D%NASM0 is allocated (0:R%NSMAX), and KM is 0 for the zonal mean, so the lower bound has to
  ! be carried over explicitly.
  INTEGER(KIND=JPIM),INTENT(IN)    :: KNASM0(0:KNASM0_UB)

  INTEGER(KIND=JPIM) :: KM, KMLOC
  INTEGER(KIND=JPIM) :: INM, JN, JFLD, IASM0

#ifdef OMPGPU
  !$OMP TARGET TEAMS DISTRIBUTE PARALLEL DO COLLAPSE(3) DEFAULT(ECTRANS_OMP_DEFAULT) &
  !$OMP& PRIVATE(KM,IASM0,INM) &
  !$OMP& SHARED(PIA,PSPEC,KMYMS,KNASM0) &
  !$OMP& MAP(ECTRANS_MAP_PRESENT_ALLOC:KMYMS,KNASM0) &
  !$OMP& ECTRANS_LOOP_BOUNDS_CLAUSE(KFIELDS,KNUMP,KNSMAX)
#endif
#ifdef ACCGPU
  !$ACC PARALLEL LOOP DEFAULT(NONE) COLLAPSE(3) PRIVATE(KM,IASM0,INM) &
  !$ACC& PRESENT(PIA,PSPEC,KMYMS,KNASM0) &
  !$ACC& FIRSTPRIVATE(KFIELDS,KDIM,KNUMP,KNSMAX) &
#ifndef _CRAYFTN
  !$ACC& ASYNC(1)
#else
  !$ACC&
#endif
#endif
  DO KMLOC=1,KNUMP
    DO JN=0,KNSMAX+3
      DO JFLD=1,KFIELDS
        KM = KMYMS(KMLOC)

        IF (JN <= 1) THEN
            PIA(2*JFLD-1,JN+1,KMLOC) = 0.0_JPRB
            PIA(2*JFLD  ,JN+1,KMLOC) = 0.0_JPRB
        ELSEIF (JN <= KNSMAX+2-KM) THEN
            IASM0 = KNASM0(KM)
            INM = IASM0+((KNSMAX+2-JN)-KM)*2
            PIA(2*JFLD-1,JN+1,KMLOC) = PSPEC(JFLD,INM  )
            PIA(2*JFLD  ,JN+1,KMLOC) = PSPEC(JFLD,INM+1)
        ELSEIF (JN <= KNSMAX+3-KM) THEN
            PIA(2*JFLD-1,JN+1,KMLOC) = 0.0_JPRB
            PIA(2*JFLD  ,JN+1,KMLOC) = 0.0_JPRB
        ENDIF
        ENDDO
      ENDDO
  ENDDO
  END SUBROUTINE PRFI1B_EXTRACT
END MODULE PRFI1B_MOD
