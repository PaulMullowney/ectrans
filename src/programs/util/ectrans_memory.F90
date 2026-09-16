module ectrans_memory
use, intrinsic :: iso_c_binding, only : c_char

private
public :: allocator

type allocator_t
contains
    procedure, nopass :: set_pinning
    procedure, nopass :: set_device_resident
    procedure, nopass :: device_resident_supported
    procedure, nopass :: set_logging
    procedure, nopass :: set_logging_output_unit

    procedure, nopass, private :: allocate_var_real32_r1
    procedure, nopass, private :: allocate_var_real32_r2
    procedure, nopass, private :: allocate_var_real32_r3
    procedure, nopass, private :: allocate_var_real32_r4
    procedure, nopass, private :: allocate_var_real64_r1
    procedure, nopass, private :: allocate_var_real64_r2
    procedure, nopass, private :: allocate_var_real64_r3
    procedure, nopass, private :: allocate_var_real64_r4
    procedure, nopass, private :: allocate_var_label_real32_r1
    procedure, nopass, private :: allocate_var_label_real32_r2
    procedure, nopass, private :: allocate_var_label_real32_r3
    procedure, nopass, private :: allocate_var_label_real32_r4
    procedure, nopass, private :: allocate_var_label_real64_r1
    procedure, nopass, private :: allocate_var_label_real64_r2
    procedure, nopass, private :: allocate_var_label_real64_r3
    procedure, nopass, private :: allocate_var_label_real64_r4
    generic :: allocate => &
        & allocate_var_real32_r1, &
        & allocate_var_real32_r2, &
        & allocate_var_real32_r3, &
        & allocate_var_real32_r4, &
        & allocate_var_real64_r1, &
        & allocate_var_real64_r2, &
        & allocate_var_real64_r3, &
        & allocate_var_real64_r4, &
        & allocate_var_label_real32_r1, &
        & allocate_var_label_real32_r2, &
        & allocate_var_label_real32_r3, &
        & allocate_var_label_real32_r4, &
        & allocate_var_label_real64_r1, &
        & allocate_var_label_real64_r2, &
        & allocate_var_label_real64_r3, &
        & allocate_var_label_real64_r4

    procedure, nopass, private :: deallocate_var_real32_r1
    procedure, nopass, private :: deallocate_var_real32_r2
    procedure, nopass, private :: deallocate_var_real32_r3
    procedure, nopass, private :: deallocate_var_real32_r4
    procedure, nopass, private :: deallocate_var_real64_r1
    procedure, nopass, private :: deallocate_var_real64_r2
    procedure, nopass, private :: deallocate_var_real64_r3
    procedure, nopass, private :: deallocate_var_real64_r4
    procedure, nopass, private :: deallocate_var_label_real32_r1
    procedure, nopass, private :: deallocate_var_label_real32_r2
    procedure, nopass, private :: deallocate_var_label_real32_r3
    procedure, nopass, private :: deallocate_var_label_real32_r4
    procedure, nopass, private :: deallocate_var_label_real64_r1
    procedure, nopass, private :: deallocate_var_label_real64_r2
    procedure, nopass, private :: deallocate_var_label_real64_r3
    procedure, nopass, private :: deallocate_var_label_real64_r4
    generic :: deallocate => &
        & deallocate_var_real32_r1, &
        & deallocate_var_real32_r2, &
        & deallocate_var_real32_r3, &
        & deallocate_var_real32_r4, &
        & deallocate_var_real64_r1, &
        & deallocate_var_real64_r2, &
        & deallocate_var_real64_r3, &
        & deallocate_var_real64_r4, &
        & deallocate_var_label_real32_r1, &
        & deallocate_var_label_real32_r2, &
        & deallocate_var_label_real32_r3, &
        & deallocate_var_label_real32_r4, &
        & deallocate_var_label_real64_r1, &
        & deallocate_var_label_real64_r2, &
        & deallocate_var_label_real64_r3, &
        & deallocate_var_label_real64_r4
end type

type(allocator_t) :: allocator

character(kind=c_char), pointer, private :: c_label(:) => null()

! When set, allocate() returns device memory rather than host memory. This is what makes
! LPGP_ON_GPU valid for the gridpoint arrays: TRGTOL and TRLTOG hand those arrays to their
! pack/unpack kernels as storage that is already device-resident -- through HAS_DEVICE_ADDR
! under OpenMP offload, and through the present table under OpenACC. Either way the array's
! base address has to be a device address, which only holds for storage obtained this way.
logical, private :: device_resident_ = .false.

interface
    function c_allocate_var(bytes) result(ptr) bind(c, name="ectrans_memory_allocate_var")
        use iso_c_binding, only: c_ptr, c_size_t
        integer(kind=c_size_t), value :: bytes
        type(c_ptr) :: ptr
    end function

    subroutine c_deallocate_var(ptr, bytes) bind(c, name="ectrans_memory_deallocate_var")
        use iso_c_binding, only: c_ptr, c_size_t
        type(c_ptr), value, intent(in) :: ptr
        integer(c_size_t), value, intent(in) :: bytes
    end subroutine

    subroutine c_set_pinning(pinning) bind(c, name="ectrans_memory_set_pinning")
        use iso_c_binding, only: c_int
        integer(c_int), value :: pinning
    end subroutine

    subroutine c_set_label(label) bind(c, name="ectrans_memory_set_label")
        use iso_c_binding, only: c_ptr
        type(c_ptr), value :: label ! must be null-terminated !
    end subroutine

    subroutine c_unset_label() bind(c, name="ectrans_memory_unset_label")
    end subroutine

    subroutine c_set_logging(logging) bind(c,name="ectrans_memory_set_logging")
        use iso_c_binding, only: c_int
        integer(c_int), value :: logging
    end subroutine

    subroutine c_set_logging_fortran_output_unit(output_unit) bind(c, &
          & name="ectrans_memory_set_logging_fortran_output_unit")
        use iso_c_binding, only: c_int
        integer(c_int), value :: output_unit
    end subroutine

end interface

contains

    function required_bytes(shape,real_kind)
        use iso_c_binding, only: c_int, c_size_t, c_float, c_double
        integer(c_int), intent(in) :: shape(:)
        integer, intent(in) :: real_kind
        integer(c_size_t) :: required_bytes
        if (real_kind == c_float) then
            required_bytes = product(int(shape,c_size_t)) * 4_c_size_t
        elseif (real_kind == c_double) then
            required_bytes = product(int(shape,c_size_t)) * 8_c_size_t
        else
            required_bytes = 0
        endif
    end function

    subroutine set_pinning(pinning)
        logical, intent(in) :: pinning
        if (pinning) then
            call c_set_pinning(1)
        else
            call c_set_pinning(0)
        endif
    end subroutine

    subroutine set_device_resident(device_resident)
        logical, intent(in) :: device_resident
        if (device_resident .and. .not. device_resident_supported()) then
            write(0,'(a)') 'ectrans_memory: device-resident allocation requires an OpenMP &
                &offload (OMPGPU) or OpenACC (ACCGPU) build'
            error stop 1
        endif
        device_resident_ = device_resident
    end subroutine

    function device_resident_supported() result(supported)
        logical :: supported
#if defined(OMPGPU) || defined(ACCGPU)
        supported = .true.
#else
        supported = .false.
#endif
    end function

    ! Single dispatch point for every allocate_var_* below. Host storage keeps going through
    ! the C helper so the pinning and logging behaviour is unchanged; device storage uses the
    ! same allocate-then-associate pattern as the library's internal growing allocator, which
    ! is what the transform kernels already consume successfully. The two offload models spell
    ! that pattern differently -- omp_target_alloc + omp_target_associate_ptr against
    ! acc_malloc + acc_map_data -- but both end up mapping the device address to itself, so
    ! the array the caller receives has a device base address that the runtime can resolve.
    function allocate_bytes(bytes) result(mem)
#ifdef OMPGPU
        use omp_lib, only : omp_get_default_device, omp_target_alloc, omp_target_associate_ptr
#endif
#if defined(ACCGPU) && !defined(OMPGPU)
        use openacc, only : acc_malloc, acc_map_data, c_devptr
#endif
        use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_associated, c_f_pointer, &
            & c_signed_char
        integer(c_size_t), intent(in) :: bytes
        type(c_ptr) :: mem
#ifdef OMPGPU
        integer :: device_num, ierr
#endif
#if defined(ACCGPU) && !defined(OMPGPU)
        type(c_devptr) :: dmem
        ! acc_map_data takes the host-side object rather than a raw address, so the freshly
        ! allocated range is viewed as bytes purely to have something to name it with.
        integer(c_signed_char), pointer :: cbuf(:)
#endif

        if (.not. device_resident_ .or. bytes == 0_c_size_t) then
            mem = c_allocate_var(bytes)
            return
        endif

#ifdef OMPGPU
        device_num = omp_get_default_device()
        mem = omp_target_alloc(bytes, device_num)
        if (.not. c_associated(mem)) then
            write(0,'(a,i0,a)') 'ectrans_memory: omp_target_alloc failed for ', bytes, ' bytes'
            error stop 1
        endif
        ! Associate the device address with itself so the runtime can still resolve the
        ! range if anything looks it up; the kernels reach it via HAS_DEVICE_ADDR.
        ierr = omp_target_associate_ptr(mem, mem, bytes, 0_c_size_t, device_num)
#elif defined(ACCGPU)
        dmem = acc_malloc(bytes)
        mem = transfer(dmem, mem)
        if (.not. c_associated(mem)) then
            write(0,'(a,i0,a)') 'ectrans_memory: acc_malloc failed for ', bytes, ' bytes'
            error stop 1
        endif
        ! Enter the range in the present table, mapped to itself, so the PRESENT clauses on
        ! the pack/unpack constructs resolve it instead of trying to copy it in.
        call c_f_pointer(mem, cbuf, [bytes])
        call acc_map_data(cbuf, dmem, bytes)
#else
        mem = c_allocate_var(bytes)
#endif
    end function

    subroutine deallocate_bytes(mem, bytes)
#ifdef OMPGPU
        use omp_lib, only : omp_get_default_device, omp_target_free, omp_target_disassociate_ptr
#endif
#if defined(ACCGPU) && !defined(OMPGPU)
        use openacc, only : acc_free, acc_unmap_data, c_devptr
#endif
        use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_associated, c_f_pointer, &
            & c_signed_char
        type(c_ptr), intent(in) :: mem
        integer(c_size_t), intent(in) :: bytes
        logical :: on_device
#ifdef OMPGPU
        integer :: ierr
#endif
#if defined(ACCGPU) && !defined(OMPGPU)
        type(c_devptr) :: dmem
        integer(c_signed_char), pointer :: cbuf(:)
#endif

        on_device = .false.
#if defined(OMPGPU) || defined(ACCGPU)
        on_device = device_resident_ .and. bytes > 0_c_size_t .and. c_associated(mem)
#endif
        if (on_device) then
            ! Drop the present-table entry before releasing the storage. Without this the
            ! runtime keeps a mapping for an address it no longer owns, and the next
            ! allocation handed back the same address fails to map.
#ifdef OMPGPU
            ierr = omp_target_disassociate_ptr(mem, omp_get_default_device())
            call omp_target_free(mem, omp_get_default_device())
#elif defined(ACCGPU)
            call c_f_pointer(mem, cbuf, [bytes])
            call acc_unmap_data(cbuf)
            dmem = transfer(mem, dmem)
            call acc_free(dmem)
#endif
        else
            call c_deallocate_var(mem, bytes)
        endif
    end subroutine

    subroutine set_logging(logging)
        logical, intent(in) :: logging
        if (logging) then
            call c_set_logging(1)
        else
            call c_set_logging(0)
        endif
    end subroutine

    subroutine set_logging_output_unit(output_unit)
        integer, intent(in) :: output_unit
        call c_set_logging_fortran_output_unit(output_unit)
    end subroutine

    subroutine allocate_var_real32_r1(array, shape)
        use, intrinsic :: iso_c_binding, only : c_size_t, c_ptr, c_float, c_int, c_f_pointer
        real(c_float), pointer, intent(inout) :: array(:)
        integer(c_int), intent(in) :: shape(:)
        type(c_ptr) :: mem
        mem = allocate_bytes(required_bytes(shape,c_float))
        call c_f_pointer(mem, array, shape)
    end subroutine

    subroutine allocate_var_real32_r2(array, shape)
        use, intrinsic :: iso_c_binding, only : c_size_t, c_ptr, c_float, c_int, c_f_pointer
        real(c_float), pointer, intent(inout) :: array(:,:)
        integer(c_int), intent(in) :: shape(:)
        type(c_ptr) :: mem
        mem = allocate_bytes(required_bytes(shape,c_float))
        call c_f_pointer(mem, array, shape)
    end subroutine

    subroutine allocate_var_real32_r3(array, shape)
        use, intrinsic :: iso_c_binding, only : c_size_t, c_ptr, c_float, c_int, c_f_pointer
        real(c_float), pointer, intent(inout) :: array(:,:,:)
        integer(c_int), intent(in) :: shape(:)
        type(c_ptr) :: mem
        mem = allocate_bytes(required_bytes(shape,c_float))
        call c_f_pointer(mem, array, shape)
    end subroutine

    subroutine allocate_var_real32_r4(array, shape)
        use, intrinsic :: iso_c_binding, only : c_size_t, c_ptr, c_float, c_int, c_f_pointer
        real(c_float), pointer, intent(inout) :: array(:,:,:,:)
        integer(c_int), intent(in) :: shape(:)
        type(c_ptr) :: mem
        mem = allocate_bytes(required_bytes(shape,c_float))
        call c_f_pointer(mem, array, shape)
    end subroutine

    subroutine allocate_var_real64_r1(array, shape)
        use, intrinsic :: iso_c_binding, only : c_size_t, c_ptr, c_double, c_int, c_f_pointer
        real(c_double), pointer, intent(inout) :: array(:)
        integer(c_int), intent(in) :: shape(:)
        type(c_ptr) :: mem
        mem = allocate_bytes(required_bytes(shape,c_double))
        call c_f_pointer(mem, array, shape)
    end subroutine

    subroutine allocate_var_real64_r2(array, shape)
        use, intrinsic :: iso_c_binding, only : c_size_t, c_ptr, c_double, c_int, c_f_pointer
        real(c_double), pointer, intent(inout) :: array(:,:)
        integer(c_int), intent(in) :: shape(:)
        type(c_ptr) :: mem
        mem = allocate_bytes(required_bytes(shape,c_double))
        call c_f_pointer(mem, array, shape)
    end subroutine

    subroutine allocate_var_real64_r3(array, shape)
        use, intrinsic :: iso_c_binding, only : c_size_t, c_ptr, c_double, c_int, c_f_pointer
        real(c_double), pointer, intent(inout) :: array(:,:,:)
        integer(c_int), intent(in) :: shape(:)
        type(c_ptr) :: mem
        mem = allocate_bytes(required_bytes(shape,c_double))
        call c_f_pointer(mem, array, shape)
    end subroutine

    subroutine allocate_var_real64_r4(array, shape)
        use, intrinsic :: iso_c_binding, only : c_size_t, c_ptr, c_double, c_int, c_f_pointer
        real(c_double), pointer, intent(inout) :: array(:,:,:,:)
        integer(c_int), intent(in) :: shape(:)
        type(c_ptr) :: mem
        mem = allocate_bytes(required_bytes(shape,c_double))
        call c_f_pointer(mem, array, shape)
    end subroutine

    subroutine deallocate_var_real32_r1(array)
        use, intrinsic :: iso_c_binding, only : c_size_t, c_ptr, c_float, c_loc, c_null_ptr
        real(c_float), pointer, intent(inout) :: array(:)
        type(c_ptr) :: mem
        integer(c_size_t) :: bytes
        bytes = required_bytes(shape(array),c_float)
        mem = c_null_ptr
        if (bytes > 0) then
            mem = c_loc(array(1))
        endif
        call deallocate_bytes(mem, bytes)
        array => null()
    end subroutine

    subroutine deallocate_var_real32_r2(array)
        use, intrinsic :: iso_c_binding, only : c_size_t, c_ptr, c_float, c_loc, c_null_ptr
        real(c_float), pointer, intent(inout) :: array(:,:)
        type(c_ptr) :: mem
        integer(c_size_t) :: bytes
        bytes = required_bytes(shape(array),c_float)
        mem = c_null_ptr
        if (bytes > 0) then
            mem = c_loc(array(1,1))
        endif
        call deallocate_bytes(mem, bytes)
        array => null()
    end subroutine

    subroutine deallocate_var_real32_r3(array)
        use, intrinsic :: iso_c_binding, only : c_size_t, c_ptr, c_float, c_loc, c_null_ptr
        real(c_float), pointer, intent(inout) :: array(:,:,:)
        type(c_ptr) :: mem
        integer(c_size_t) :: bytes
        bytes = required_bytes(shape(array),c_float)
        mem = c_null_ptr
        if (bytes > 0) then
            mem = c_loc(array(1,1,1))
        endif
        call deallocate_bytes(mem, bytes)
        array => null()
    end subroutine

    subroutine deallocate_var_real32_r4(array)
        use, intrinsic :: iso_c_binding, only : c_size_t, c_ptr, c_float, c_loc, c_null_ptr
        real(c_float), pointer, intent(inout) :: array(:,:,:,:)
        type(c_ptr) :: mem
        integer(c_size_t) :: bytes
        bytes = required_bytes(shape(array),c_float)
        mem = c_null_ptr
        if (bytes > 0) then
            mem = c_loc(array(1,1,1,1))
        endif
        call deallocate_bytes(mem, bytes)
        array => null()
    end subroutine

    subroutine deallocate_var_real64_r1(array)
        use, intrinsic :: iso_c_binding, only : c_size_t, c_ptr, c_double, c_loc, c_null_ptr
        real(c_double), pointer, intent(inout) :: array(:)
        type(c_ptr) :: mem
        integer(c_size_t) :: bytes
        bytes = required_bytes(shape(array),c_double)
        mem = c_null_ptr
        if (bytes > 0) then
            mem = c_loc(array(1))
        endif
        call deallocate_bytes(mem, bytes)
        array => null()
    end subroutine

    subroutine deallocate_var_real64_r2(array)
        use, intrinsic :: iso_c_binding, only : c_size_t, c_ptr, c_double, c_loc, c_null_ptr
        real(c_double), pointer, intent(inout) :: array(:,:)
        type(c_ptr) :: mem
        integer(c_size_t) :: bytes
        bytes = required_bytes(shape(array),c_double)
        mem = c_null_ptr
        if (bytes > 0) then
            mem = c_loc(array(1,1))
        endif
        call deallocate_bytes(mem, bytes)
        array => null()
    end subroutine

    subroutine deallocate_var_real64_r3(array)
        use, intrinsic :: iso_c_binding, only : c_size_t, c_ptr, c_double, c_loc, c_null_ptr
        real(c_double), pointer, intent(inout) :: array(:,:,:)
        type(c_ptr) :: mem
        integer(c_size_t) :: bytes
        bytes = required_bytes(shape(array),c_double)
        mem = c_null_ptr
        if (bytes > 0) then
            mem = c_loc(array(1,1,1))
        endif
        call deallocate_bytes(mem, bytes)
        array => null()
    end subroutine

    subroutine deallocate_var_real64_r4(array)
        use, intrinsic :: iso_c_binding, only : c_size_t, c_ptr, c_double, c_loc, c_null_ptr
        real(c_double), pointer, intent(inout) :: array(:,:,:,:)
        type(c_ptr) :: mem
        integer(c_size_t) :: bytes
        bytes = required_bytes(shape(array),c_double)
        mem = c_null_ptr
        if (bytes > 0) then
            mem = c_loc(array(1,1,1,1))
        endif
        call deallocate_bytes(mem, bytes)
        array => null()
    end subroutine

    subroutine set_label(label)
        use, intrinsic :: iso_c_binding, only : c_null_char, c_loc
        character(len=*), intent(in) :: label
        integer :: j, N
        if (associated(c_label)) then
            deallocate(c_label)
        endif
        N = len_trim(label)
        allocate(c_label(N+1))
        do j = 1, N
           c_label(j) = label(j:j)
        enddo
        c_label(N+1) = c_null_char
        call c_set_label(c_loc(c_label(1)))
    end subroutine

    subroutine unset_label()
        if (associated(c_label)) then
            deallocate(c_label)
        endif
        call c_unset_label()
    end subroutine

    subroutine allocate_var_label_real32_r1(label, array, shape)
        use, intrinsic :: iso_c_binding, only : c_size_t, c_ptr, c_float, c_int, c_f_pointer
        character(len=*), intent(in) :: label
        real(c_float), pointer, intent(inout) :: array(:)
        integer(c_int), intent(in) :: shape(:)
        call set_label(label)
        call allocate_var_real32_r1(array, shape)
        call unset_label()
    end subroutine

    subroutine allocate_var_label_real32_r2(label, array, shape)
        use, intrinsic :: iso_c_binding, only : c_size_t, c_ptr, c_float, c_int, c_f_pointer
        character(len=*), intent(in) :: label
        real(c_float), pointer, intent(inout) :: array(:,:)
        integer(c_int), intent(in) :: shape(:)
        call set_label(label)
        call allocate_var_real32_r2(array, shape)
        call unset_label()
    end subroutine

    subroutine allocate_var_label_real32_r3(label, array, shape)
        use, intrinsic :: iso_c_binding, only : c_size_t, c_ptr, c_float, c_int, c_f_pointer
        character(len=*), intent(in) :: label
        real(c_float), pointer, intent(inout) :: array(:,:,:)
        integer(c_int), intent(in) :: shape(:)
        call set_label(label)
        call allocate_var_real32_r3(array, shape)
        call unset_label()
    end subroutine

    subroutine allocate_var_label_real32_r4(label, array, shape)
        use, intrinsic :: iso_c_binding, only : c_size_t, c_ptr, c_float, c_int, c_f_pointer
        character(len=*), intent(in) :: label
        real(c_float), pointer, intent(inout) :: array(:,:,:,:)
        integer(c_int), intent(in) :: shape(:)
        call set_label(label)
        call allocate_var_real32_r4(array, shape)
        call unset_label()
    end subroutine

    subroutine allocate_var_label_real64_r1(label, array, shape)
        use, intrinsic :: iso_c_binding, only : c_size_t, c_ptr, c_double, c_int, c_f_pointer
        character(len=*), intent(in) :: label
        real(c_double), pointer, intent(inout) :: array(:)
        integer(c_int), intent(in) :: shape(:)
        call set_label(label)
        call allocate_var_real64_r1(array, shape)
        call unset_label()
    end subroutine

    subroutine allocate_var_label_real64_r2(label, array, shape)
        use, intrinsic :: iso_c_binding, only : c_size_t, c_ptr, c_double, c_int, c_f_pointer
        character(len=*), intent(in) :: label
        real(c_double), pointer, intent(inout) :: array(:,:)
        integer(c_int), intent(in) :: shape(:)
        call set_label(label)
        call allocate_var_real64_r2(array, shape)
        call unset_label()
    end subroutine

    subroutine allocate_var_label_real64_r3(label, array, shape)
        use, intrinsic :: iso_c_binding, only : c_size_t, c_ptr, c_double, c_int, c_f_pointer
        character(len=*), intent(in) :: label
        real(c_double), pointer, intent(inout) :: array(:,:,:)
        integer(c_int), intent(in) :: shape(:)
        call set_label(label)
        call allocate_var_real64_r3(array, shape)
        call unset_label()
    end subroutine

    subroutine allocate_var_label_real64_r4(label, array, shape)
        use, intrinsic :: iso_c_binding, only : c_size_t, c_ptr, c_double, c_int, c_f_pointer
        character(len=*), intent(in) :: label
        real(c_double), pointer, intent(inout) :: array(:,:,:,:)
        integer(c_int), intent(in) :: shape(:)
        call set_label(label)
        call allocate_var_real64_r4(array, shape)
        call unset_label()
    end subroutine

    subroutine deallocate_var_label_real32_r1(label, array)
        use, intrinsic :: iso_c_binding, only : c_size_t, c_ptr, c_float, c_loc
        character(len=*), intent(in) :: label
        real(c_float), pointer, intent(inout) :: array(:)
        call set_label(label)
        call deallocate_var_real32_r1(array)
        call unset_label()
    end subroutine

    subroutine deallocate_var_label_real32_r2(label, array)
        use, intrinsic :: iso_c_binding, only : c_size_t, c_ptr, c_float, c_loc
        character(len=*), intent(in) :: label
        real(c_float), pointer, intent(inout) :: array(:,:)
        call set_label(label)
        call deallocate_var_real32_r2(array)
        call unset_label()
    end subroutine

    subroutine deallocate_var_label_real32_r3(label, array)
        use, intrinsic :: iso_c_binding, only : c_size_t, c_ptr, c_float, c_loc
        character(len=*), intent(in) :: label
        real(c_float), pointer, intent(inout) :: array(:,:,:)
        call set_label(label)
        call deallocate_var_real32_r3(array)
        call unset_label()
    end subroutine

    subroutine deallocate_var_label_real32_r4(label, array)
        use, intrinsic :: iso_c_binding, only : c_size_t, c_ptr, c_float, c_loc
        character(len=*), intent(in) :: label
        real(c_float), pointer, intent(inout) :: array(:,:,:,:)
        call set_label(label)
        call deallocate_var_real32_r4(array)
        call unset_label()
    end subroutine

    subroutine deallocate_var_label_real64_r1(label, array)
        use, intrinsic :: iso_c_binding, only : c_size_t, c_ptr, c_double, c_loc
        character(len=*), intent(in) :: label
        real(c_double), pointer, intent(inout) :: array(:)
        call set_label(label)
        call deallocate_var_real64_r1(array)
        call unset_label()
    end subroutine

    subroutine deallocate_var_label_real64_r2(label, array)
        use, intrinsic :: iso_c_binding, only : c_size_t, c_ptr, c_double, c_loc
        character(len=*), intent(in) :: label
        real(c_double), pointer, intent(inout) :: array(:,:)
        call set_label(label)
        call deallocate_var_real64_r2(array)
        call unset_label()
    end subroutine

    subroutine deallocate_var_label_real64_r3(label, array)
        use, intrinsic :: iso_c_binding, only : c_size_t, c_ptr, c_double, c_loc
        character(len=*), intent(in) :: label
        real(c_double), pointer, intent(inout) :: array(:,:,:)
        call set_label(label)
        call deallocate_var_real64_r3(array)
        call unset_label()
    end subroutine

    subroutine deallocate_var_label_real64_r4(label, array)
        use, intrinsic :: iso_c_binding, only : c_size_t, c_ptr, c_double, c_loc
        character(len=*), intent(in) :: label
        real(c_double), pointer, intent(inout) :: array(:,:,:,:)
        call set_label(label)
        call deallocate_var_real64_r4(array)
        call unset_label()
    end subroutine

    subroutine c_write_to_fortran_unit(unit,msg_cptr) bind(c, name="ectrans_memory_write_to_fortran_unit")
        use, intrinsic :: iso_c_binding, only: c_int32_t, c_ptr, c_char, c_associated
        integer(c_int32_t), value, intent(in) :: unit
        type(c_ptr), value, intent(in) :: msg_cptr
        character(kind=c_char,len=:), allocatable :: msg
        if( c_associated(msg_cptr) ) then
            call copy_c_ptr_to_string( msg_cptr, msg )
            write(unit,'(A)', advance='no') msg
        endif
    contains
        subroutine copy_c_str_to_string(s,string)
            use, intrinsic :: iso_c_binding
            character(kind=c_char,len=1), intent(in) :: s(:)
            character(len=:), allocatable :: string
            integer :: i, nchars
            do i = 1, size(s)
            if (s(i) == c_null_char) exit
            enddo
            nchars = i - 1  ! Exclude null character from Fortran string
            allocate( character(len=(nchars),kind=c_char) :: string )
            do i=1,nchars
            string(i:i) = s(i)
            enddo
        end subroutine
        subroutine copy_c_ptr_to_string(cptr,string)
            use, intrinsic :: iso_c_binding
            type(c_ptr), intent(in) :: cptr
            character(kind=c_char,len=:), allocatable :: string
            character(kind=c_char), dimension(:), pointer  :: s
            integer(c_int), parameter :: MAX_STR_LEN = 2550
            call c_f_pointer ( cptr , s, (/MAX_STR_LEN/) )
            call copy_c_str_to_string( s, string )
        end subroutine
    end subroutine
end module
