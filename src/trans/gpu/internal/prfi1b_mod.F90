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
  USE PARKIND1, ONLY: JPIM

  IMPLICIT NONE

  ! Fields walked serially by one thread in PRFI1B_EXTRACT. Held to 2, not the 4 used by the
  ! larger strip-mined kernels, because the strip-mined extent must stay at least one
  ! wavefront wide. See the comment in PRFI1B_EXTRACT.
  INTEGER(KIND=JPIM), PARAMETER :: ITILE = 2

  CONTAINS
  SUBROUTINE PRFI1B(PIA,KOFF,PSPEC,KFIELDS,KDIM,KFLDPTR)
  
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
  !                              KOFF   - offset of the first field in PIA's leading dimension
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
  ! The whole LTINV work array, not a field section, so that it reaches PRFI1B_EXTRACT as an
  ! explicit-shape dummy; KOFF selects the fields.
  REAL(KIND=JPRB)   ,INTENT(INOUT)  ,CONTIGUOUS :: PIA(:,:,:)
  INTEGER(KIND=JPIM),INTENT(IN) :: KOFF
  INTEGER(KIND=JPIM),INTENT(IN) :: KDIM
  INTEGER(KIND=JPIM),INTENT(IN),OPTIONAL :: KFLDPTR(:)
  
  !     LOCAL INTEGER SCALARS
  ! Extents for the explicit-shape dummies of PRFI1B_EXTRACT.
  INTEGER(KIND=JPIM) :: ISPEC1, INASM0_UB, IPIA1, IPIA2, IPIA3
  
  !     ------------------------------------------------------------------
  
  !*       1.    EXTRACT FIELDS FROM SPECTRAL ARRAYS.
  !              --------------------------------------------------

  ASSOCIATE(D_NUMP=>D%NUMP, D_MYMS=>D%MYMS, D_NASM0=>D%NASM0, R_NSMAX=>R%NSMAX)

#ifdef ACCGPU
  !$ACC DATA PRESENT(D,D_NUMP,R,R_NSMAX,D_MYMS,D_NASM0,PIA,PSPEC) ASYNC(1)
#endif
#ifdef OMPGPU
  !$OMP TARGET DATA MAP(ECTRANS_MAP_PRESENT_ALLOC:D_NUMP,R_NSMAX,D_MYMS,D_NASM0)
#endif

  IF(PRESENT(KFLDPTR)) THEN
 
    CALL ABORT_TRANS("KFLDPTR not implemented for GPU")

  ELSE

    !loop over wavenumber

    ISPEC1 = SIZE(PSPEC,1)
    INASM0_UB = UBOUND(D_NASM0,1)

    IPIA1 = SIZE(PIA,1)
    IPIA2 = SIZE(PIA,2)
    IPIA3 = SIZE(PIA,3)

    CALL PRFI1B_EXTRACT(PIA,IPIA1,IPIA2,IPIA3,KOFF,PSPEC,ISPEC1,KDIM,D_MYMS,D_NUMP,D_NASM0, &
      &                 INASM0_UB,KFIELDS,R_NSMAX)

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
  ! PIA is the whole LTINV work array rather than the field section being filled, since a
  ! leading-dimension section is not contiguous and could not be passed explicit-shape. KOFF is
  ! the section's offset in the leading dimension.
  SUBROUTINE PRFI1B_EXTRACT(PIA,KPIA1,KPIA2,KPIA3,KOFF,PSPEC,KSPEC1,KDIM,KMYMS,KNUMP, &
    &                       KNASM0,KNASM0_UB,KFIELDS,KNSMAX)
  USE PARKIND1,        ONLY: JPIM, JPRB

  IMPLICIT NONE

  INTEGER(KIND=JPIM),INTENT(IN)    :: KSPEC1, KDIM, KNUMP, KNASM0_UB
  INTEGER(KIND=JPIM),INTENT(IN)    :: KFIELDS, KNSMAX
  INTEGER(KIND=JPIM),INTENT(IN)    :: KPIA1, KPIA2, KPIA3, KOFF
  REAL(KIND=JPRB)   ,INTENT(INOUT) :: PIA(KPIA1,KPIA2,KPIA3)
  REAL(KIND=JPRB)   ,INTENT(IN)    :: PSPEC(KSPEC1,KDIM)
  INTEGER(KIND=JPIM),INTENT(IN)    :: KMYMS(KNUMP)
  ! D%NASM0 is allocated (0:R%NSMAX), and KM is 0 for the zonal mean, so the lower bound has to
  ! be carried over explicitly.
  INTEGER(KIND=JPIM),INTENT(IN)    :: KNASM0(0:KNASM0_UB)

  INTEGER(KIND=JPIM) :: KM, KMLOC
  INTEGER(KIND=JPIM) :: INM, JN, JFLD, JFP, INJF, IASM0

  ! Strip-mine the field loop. KM, IASM0 and INM depend only on KMLOC and JN, so collapsing
  ! the field loop outright makes every field redo the KMYMS load, the dependent KNASM0 load
  ! and the address arithmetic: four loads per element where two are useful. Walking ITILE
  ! fields serially per thread loads them once per tile instead.
  !
  ! JFLD is unit-stride in both PSPEC(JFLD,...) and, through 2*JFLD, in PIA(2*JFLD-1,...), so
  ! the parallel index has to stay unit-stride and the serial loop strides by INJF. Handing
  ! each thread a contiguous block of fields instead would space the lanes ITILE apart and
  ! lose the coalescing this kernel depends on.
  !
  ! INJF must not fall below the 64-lane wavefront. It is the innermost collapsed extent, so
  ! once it is narrower than a wavefront the lanes of one wavefront straddle two values of JN,
  ! which have unrelated INM and therefore land in unrelated parts of PSPEC. Every access then
  ! splits in two and the extra cache traffic costs more than the hoisted loads save. At the
  ! 137 fields this is called with, ITILE=4 leaves 35 and measured 1.06x, while ITILE=2 leaves
  ! 69 and measures 1.17x. The same floor explains the tile sweep in trltom_pack_unpack.F90,
  ! where the strip-mined extent is four times larger and ITILE=4 is therefore the right
  ! choice: the rule is the largest ITILE that keeps the extent at or above 64, not a constant.
  INJF = (KFIELDS+ITILE-1)/ITILE

#ifdef OMPGPU
  !$OMP TARGET TEAMS DISTRIBUTE PARALLEL DO COLLAPSE(3) DEFAULT(ECTRANS_OMP_DEFAULT) &
  !$OMP& PRIVATE(KM,IASM0,INM,JFLD) &
  !$OMP& SHARED(PIA,PSPEC,KMYMS,KNASM0) &
  !$OMP& MAP(ECTRANS_MAP_PRESENT_ALLOC:KMYMS,KNASM0) &
  !$OMP& FIRSTPRIVATE(INJF,KOFF) &
  !$OMP& ECTRANS_LOOP_BOUNDS_CLAUSE(KFIELDS,KNUMP,KNSMAX)
#endif
#ifdef ACCGPU
  !$ACC PARALLEL LOOP DEFAULT(NONE) COLLAPSE(3) PRIVATE(KM,IASM0,INM,JFLD) &
  !$ACC& PRESENT(PIA,PSPEC,KMYMS,KNASM0) &
  !$ACC& FIRSTPRIVATE(KFIELDS,KDIM,KNUMP,KNSMAX,INJF,KOFF) &
#ifndef _CRAYFTN
  !$ACC& ASYNC(1)
#else
  !$ACC&
#endif
#endif
  DO KMLOC=1,KNUMP
    DO JN=0,KNSMAX+3
      DO JFP=1,INJF
        KM = KMYMS(KMLOC)

        IF (JN <= 1) THEN
#ifdef ACCGPU
            !$ACC LOOP SEQ
#endif
            DO JFLD=JFP,KFIELDS,INJF
              PIA(KOFF+2*JFLD-1,JN+1,KMLOC) = 0.0_JPRB
              PIA(KOFF+2*JFLD  ,JN+1,KMLOC) = 0.0_JPRB
            ENDDO
        ELSEIF (JN <= KNSMAX+2-KM) THEN
            IASM0 = KNASM0(KM)
            INM = IASM0+((KNSMAX+2-JN)-KM)*2
#ifdef ACCGPU
            !$ACC LOOP SEQ
#endif
            DO JFLD=JFP,KFIELDS,INJF
              PIA(KOFF+2*JFLD-1,JN+1,KMLOC) = PSPEC(JFLD,INM  )
              PIA(KOFF+2*JFLD  ,JN+1,KMLOC) = PSPEC(JFLD,INM+1)
            ENDDO
        ELSEIF (JN <= KNSMAX+3-KM) THEN
#ifdef ACCGPU
            !$ACC LOOP SEQ
#endif
            DO JFLD=JFP,KFIELDS,INJF
              PIA(KOFF+2*JFLD-1,JN+1,KMLOC) = 0.0_JPRB
              PIA(KOFF+2*JFLD  ,JN+1,KMLOC) = 0.0_JPRB
            ENDDO
        ENDIF
        ENDDO
      ENDDO
  ENDDO
  END SUBROUTINE PRFI1B_EXTRACT
END MODULE PRFI1B_MOD
