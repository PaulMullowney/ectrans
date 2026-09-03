! (C) Copyright 2020- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!

module ectrans_device

! Bind each MPI rank to its own GPU when more than one device is visible.
!
! ecTrans never selects a device itself, so with every GPU visible each rank falls back to
! the runtime default and they all pile onto the same one. Masking with ROCR_VISIBLE_DEVICES
! hides the problem by leaving one device per rank, but it also hides the peer devices, which
! blocks GPU-to-GPU transports. Choosing the device here keeps all of them visible.
!
! This has to run before anything touches the GPU -- in particular before acc_init -- which is
! earlier than MPI initialisation, so the node-local rank comes from the launcher's
! environment rather than from MPI. When only one device is visible the routine does nothing,
! so masked runs are unaffected.

use parkind1, only: jpim

implicit none

private
public :: ectrans_select_device

contains

!===================================================================================================

function local_rank_from_environment(cdvar) result(krank)

  ! Node-local rank as published by the launcher, or -1 if none of them is set.

  character(len=*), intent(out) :: cdvar ! Name of the variable that supplied the rank
  integer(kind=jpim) :: krank

  ! Ordered by how specific they are: a launcher's own variable beats a generic PMI one.
  character(len=32), parameter :: cl_candidates(6) = [ character(len=32) :: &
    & 'OMPI_COMM_WORLD_LOCAL_RANK', &  ! Open MPI
    & 'SLURM_LOCALID',               &  ! Slurm
    & 'MV2_COMM_WORLD_LOCAL_RANK',   &  ! MVAPICH2
    & 'MPI_LOCALRANKID',             &  ! Intel MPI
    & 'FLUX_TASK_LOCAL_ID',          &  ! Flux
    & 'PMI_LOCAL_RANK' ]                ! generic PMI

  character(len=32) :: clvalue
  integer :: jvar, ilen, istat, ivalue

  krank = -1
  cdvar = ''

  do jvar = 1, size(cl_candidates)
    call get_environment_variable(trim(cl_candidates(jvar)), clvalue, ilen, istat)
    if (istat == 0 .and. ilen > 0) then
      read(clvalue(1:ilen), *, iostat=istat) ivalue
      if (istat == 0 .and. ivalue >= 0) then
        krank = int(ivalue, jpim)
        cdvar = cl_candidates(jvar)
        return
      endif
    endif
  enddo

end function local_rank_from_environment

!===================================================================================================

subroutine ectrans_select_device(kdevice, kndevice, cdsource)

  ! Pick this rank's device. Returns kdevice = -1 when no choice was made, either because
  ! fewer than two devices are visible or because the launcher published no local rank.

#ifdef OMPGPU
  use omp_lib, only: omp_get_num_devices, omp_set_default_device
#endif
#ifdef ACCGPU
  use openacc, only: acc_set_device_num, acc_get_device_type, acc_get_num_devices
#endif
  use iso_c_binding, only: c_int

  integer(kind=jpim), intent(out) :: kdevice  ! Device this rank will use, -1 if unchanged
  integer(kind=jpim), intent(out) :: kndevice ! Number of visible devices
  character(len=*), intent(out) :: cdsource   ! Environment variable the rank came from

#if defined(HIP) || defined(CUDA)
  interface
    integer(c_int) function device_set(kdev) bind(c, name='hipSetDevice')
      import :: c_int
      integer(c_int), value :: kdev
    end function device_set
    integer(c_int) function device_count(kdev) bind(c, name='hipGetDeviceCount')
      import :: c_int
      integer(c_int) :: kdev
    end function device_count
  end interface
  integer(c_int) :: idev, istat
#endif

  integer(kind=jpim) :: ilocal

  kdevice  = -1
  kndevice = 0
  cdsource = ''

#ifdef OMPGPU
  kndevice = int(omp_get_num_devices(), jpim)
#elif defined(ACCGPU)
  kndevice = int(acc_get_num_devices(acc_get_device_type()), jpim)
#elif defined(HIP) || defined(CUDA)
  istat = device_count(idev)
  if (istat == 0) kndevice = int(idev, jpim)
#endif

  ! One device means the run is already masked to a single GPU per rank; nothing to choose.
  if (kndevice < 2) return

  ilocal = local_rank_from_environment(cdsource)
  if (ilocal < 0) return

  kdevice = mod(ilocal, kndevice)

  ! Set every runtime that will allocate or launch. OpenMP target regions, OpenACC regions and
  ! the HIP current device are independent settings, and the FFT and BLAS handles ecTrans
  ! creates follow the HIP one, so leaving any of them behind would split a rank across devices.
#ifdef OMPGPU
  call omp_set_default_device(int(kdevice))
#endif
#ifdef ACCGPU
  call acc_set_device_num(int(kdevice), acc_get_device_type())
#endif
#if defined(HIP) || defined(CUDA)
  istat = device_set(int(kdevice, c_int))
  if (istat /= 0) kdevice = -1
#endif

end subroutine ectrans_select_device

!===================================================================================================

end module ectrans_device
