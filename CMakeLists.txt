# (C) Copyright 2020- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

cmake_minimum_required( VERSION 3.25 FATAL_ERROR )

find_package( ecbuild 3.14.2 REQUIRED HINTS ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/../ecbuild )

project( ectrans LANGUAGES C CXX Fortran )
include( ectrans_macros )

# CMake 3.29 adds CMAKE_TEST_LAUNCHER defined either as CMake variable or environment.
# This launcher is a semi-colon-separted list of arguments that is used to launch serial tasks,
# and can be defined during the CMake configuration.
# This is e.g. required for GPU tests that need access to slurm resources:
#    export CMAKE_TEST_LAUNCHER="srun;-n;1"
# To run the tests then:
#    salloc -q <queue> --gpus-per-task=1 -n <nproc>    ctest <ctest-args>
# Before cmake 3.29 this could only be achieved with CMAKE_CROSSCOMPILING_EMULATOR.
# This next snippet ensures forward compatibility
if( ${CMAKE_VERSION} VERSION_LESS "3.29" )
  if( DEFINED CMAKE_TEST_LAUNCHER )
    set(CMAKE_CROSSCOMPILING_EMULATOR ${CMAKE_TEST_LAUNCHER})
  elseif(DEFINED ENV{CMAKE_TEST_LAUNCHER})
    set(CMAKE_CROSSCOMPILING_EMULATOR $ENV{CMAKE_TEST_LAUNCHER})
  endif()
endif()
if( CMAKE_CROSSCOMPILING_EMULATOR )
  set( CMAKE_TEST_LAUNCHER ${CMAKE_CROSSCOMPILING_EMULATOR} )
endif()

set(CMAKE_CXX_STANDARD 17)

ecbuild_enable_fortran( REQUIRED NO_MODULE_DIRECTORY )

### Find (optional) dependencies

ecbuild_find_package( NAME fiat VERSION 1.3.0 REQUIRED )

# Inherit MPI feature from FIAT (if you don't want MPI, rebuild FIAT with ENABLE_MPI=OFF)
set( HAVE_MPI ${fiat_HAVE_MPI} )
set( ectrans_HAVE_MPI ${HAVE_MPI} ) # also needed as more specific alias to HAVE_MPI

ecbuild_add_option( FEATURE OMP
                    DEFAULT ON
                    DESCRIPTION "Support for OpenMP shared memory parallelism"
                    REQUIRED_PACKAGES "OpenMP COMPONENTS Fortran" )

ecbuild_add_option( FEATURE ACC
                    DEFAULT OFF
                    DESCRIPTION "Support for using GPUs with OpenACC"
                    REQUIRED_PACKAGES "OpenACC COMPONENTS Fortran" )

ecbuild_add_option( FEATURE DOUBLE_PRECISION
                    DEFAULT ON
                    DESCRIPTION "Support for Double Precision" )

ecbuild_add_option( FEATURE SINGLE_PRECISION
                    DEFAULT ON
                    DESCRIPTION "Support for Single Precision" )

# Check DOUBLE_PRECISION or SINGLE_PRECISION is enabled, and if not, abort
if( (NOT HAVE_DOUBLE_PRECISION) AND (NOT HAVE_SINGLE_PRECISION) )
  ecbuild_critical("Please enable one or both of the DOUBLE_PRECISION and SINGLE_PRECISION features")
endif()

set( HAVE_dp ${HAVE_DOUBLE_PRECISION} )
set( HAVE_sp ${HAVE_SINGLE_PRECISION} )

ecbuild_add_option( FEATURE CPU
                    DEFAULT ON
                    DESCRIPTION "Compile CPU version of ectrans" )

ecbuild_add_option( FEATURE MKL
                    DESCRIPTION "Use MKL for BLAS and/or FFTW"
                    DEFAULT ON
                    REQUIRED_PACKAGES "MKL QUIET"
                    CONDITION HAVE_CPU )

if( NOT HAVE_MKL )
    option( FFTW_ENABLE_MKL OFF )
endif()

if( HAVE_CPU )
  ecbuild_find_package( NAME FFTW REQUIRED COMPONENTS double single )
endif()

ecbuild_add_option( FEATURE TRANSI
                    DEFAULT ON
                    DESCRIPTION "Compile TransI C-interface to trans"
                    CONDITION HAVE_DOUBLE_PRECISION AND HAVE_CPU )

# Search for available GPU runtimes, searching for CUDA first and, if not found,
# attempt to find HIP
if( ECTRANS_ENABLE_GPU OR (NOT DEFINED ECTRANS_ENABLE_GPU AND ENABLE_GPU))
  set(HAVE_CUDA 0)
  set(HAVE_HIP 0)
  ectrans_find_cuda() # sets "HAVE_CUDA"
  if( NOT HAVE_CUDA )
    ectrans_find_hip() # sets "HAVE_HIP"
  endif()
endif()

ecbuild_add_option( FEATURE GPU
                    DEFAULT OFF
                    DESCRIPTION "Compile GPU version of ectrans (Requires OpenACC or sufficient OpenMP offloading support)"
                    CONDITION (HAVE_HIP OR HAVE_CUDA) AND (HAVE_ACC OR HAVE_OMP) )

# Check CPU or GPU is enabled, and if not, abort
if( (NOT HAVE_CPU) AND (NOT HAVE_GPU) )
  ecbuild_critical("Please enable one or both of the CPU and GPU features")
endif()

if( HAVE_GPU )
  if( HAVE_ACC )
    set( GPU_OFFLOAD "ACC" )
  elseif( HAVE_OMP )
    set( GPU_OFFLOAD "OMP" )
  else()
    ecbuild_error("Could not enable GPU as OMP or ACC were not enabled")
  endif()
endif()

# The OpenMP offload path asserts that growing-allocator-backed buffers are
# already on the device with MAP(PRESENT,ALLOC:...). nvfortran does not implement
# the OpenMP 5.1 'present' map-type modifier (verified absent in 26.3 and 26.5)
# and rejects the clause at parse time. The modifier only adds a runtime check
# that the data is present, so dropping it leaves the mapping semantics unchanged
# and costs only the diagnostic.
if( CMAKE_Fortran_COMPILER_ID MATCHES "NVHPC" )
  set( ECTRANS_MAP_PRESENT_ALLOC "ALLOC" )
else()
  set( ECTRANS_MAP_PRESENT_ALLOC "PRESENT,ALLOC" )
endif()

# Most OpenMP compute constructs in the GPU backend use DEFAULT(NONE) and name
# ASSOCIATE variables (R_NTMAX=>R%NTMAX and similar) in their loop bounds and
# bodies. nvfortran cannot resolve those against its DEFAULT(NONE) check and
# rejects them with "The selector r must be specified in either shared, private
# or fprivate clauses", regardless of what the SHARED clause lists - naming the
# base objects, the associate names, or both makes no difference. DEFAULT(SHARED)
# is accepted, and is what the OpenMP implicit rules would give anyway, so the
# only thing lost is the strictness check. Keep DEFAULT(NONE) everywhere else.
if( CMAKE_Fortran_COMPILER_ID MATCHES "NVHPC" )
  set( ECTRANS_OMP_DEFAULT "SHARED" )
else()
  set( ECTRANS_OMP_DEFAULT "NONE" )
endif()

# ECTRANS_OMP_DEFAULT_CLAUSE is the whole DEFAULT clause rather than just its argument, so that it
# can expand to nothing. It belongs on the constructs that also name device-resident arrays through
# ECTRANS_DEVICE_ADDR_CLAUSE, because those two clauses cannot be combined on amdflang 23.3.0 or
# 24.1.0-pre: HAS_DEVICE_ADDR is itself a data-sharing attribute clause (OpenMP 5.2, sec. 5.4.9) and
# so should satisfy DEFAULT(NONE) unaided, and the spec forbids naming those arrays in SHARED as
# well, but amdflang rejects the pair regardless. Dropping the clause there leaves the construct to
# the implicit rules, which is all DEFAULT(NONE) would have asserted anyway.
#
# The conflict is specific to compilers that implement HAS_DEVICE_ADDR, which nvfortran does not,
# so the pair never arises there. It still cannot have DEFAULT(NONE), for the separate ASSOCIATE
# reason above: the only way to satisfy its check is to name the parent derived types (D, R, F, G)
# in SHARED - naming the ASSOCIATE selectors alone does not do it - and that would pull whole
# derived types into the construct's data environment, which the per-component ASSOCIATE mapping
# exists to avoid. So it keeps the argument from ECTRANS_OMP_DEFAULT, and only the clause's
# presence is decided here.
#
# Because the macro can expand to nothing it has to share a line with other clauses, or the empty
# expansion would leave a continuation line holding only its own marker. Constructs without
# ECTRANS_DEVICE_ADDR_CLAUSE keep DEFAULT(ECTRANS_OMP_DEFAULT): the clause has to go on appearing
# wherever amdflang accepts it.
if( CMAKE_Fortran_COMPILER_ID MATCHES "Flang" )
  set( ECTRANS_OMP_DEFAULT_CLAUSE "" )
else()
  set( ECTRANS_OMP_DEFAULT_CLAUSE "DEFAULT(${ECTRANS_OMP_DEFAULT})" )
endif()

# ECTRANS_DEVICE_ADDR_CLAUSE is the clause the compute constructs use for the growing-allocator
# buffers and gridpoint arrays whose storage is already on the device. HAS_DEVICE_ADDR is the
# one that says so, and makes the construct take the address as-is rather than mapping it.
# nvfortran does not implement that OpenMP 5.1 clause (rejected at parse time in 26.3), so
# there those arrays are named in SHARED instead and left to the construct's implicit mapping.
#
# Unlike ECTRANS_LOOP_BOUNDS_CLAUSE below, the two expansions here are not equivalent: SHARED
# is a fallback that gives the same answer by a more expensive route, not a free choice
# between two spellings.
#
# This is correct but not free, and on this path it is expensive: where the storage is not
# resolvable in the present table the runtime copies the whole array in and out on every
# launch. Running with --gp-on-gpu makes the gridpoint arrays genuinely device-resident and
# takes that traffic away: at T639 on 8 GPUs, three iterations move 1334MB in and 2752MB out
# without it against 265MB and 255MB with it, worth ~22ms per step in single precision
# (52.4ms against 30.6ms). Measured on 26.3 from the NVCOMPILER_ACC_NOTIFY=3 transfer log.
#
# IS_DEVICE_PTR, the OpenMP 4.5 predecessor, parses and carries the right meaning but is not
# a usable substitute: nvfortran does not marshal the array descriptor with it, so any
# assumed-shape list item (PIA in LEINV, the PGP* gridpoint arrays) indexes off uninitialised
# bounds and faults with CUDA_ERROR_ILLEGAL_ADDRESS. Verified on 26.3.
if( CMAKE_Fortran_COMPILER_ID MATCHES "NVHPC" )
  set( ECTRANS_DEVICE_ADDR_CLAUSE "SHARED" )
else()
  set( ECTRANS_DEVICE_ADDR_CLAUSE "HAS_DEVICE_ADDR" )
endif()

# ECTRANS_LOOP_BOUNDS_CLAUSE is the data-sharing clause the compute constructs use for the
# scalars that bound their loops, which they have to name for DEFAULT(NONE) to be satisfied.
# It is a separate clause from the rest because which one is used decides the launch geometry on
# nvfortran: with the bound in FIRSTPRIVATE it stops folding the trip count into the launch
# and emits a grid-stride kernel of one block per SM, so a loop nest that should fill the
# device runs on 148 blocks of 128 threads on a B200 no matter how large it is. Naming the
# same scalar in SHARED restores a trip-count-sized grid. Verified on 26.3 with a 1.2M
# iteration nest: 9375 blocks and 20us in SHARED against 148 blocks and 34us in FIRSTPRIVATE.
#
# Only the bounds move. A scalar in SHARED is still implicitly firstprivate on the target
# construct, so it arrives with its host value and needs no MAP; this is what the loop bounds
# in these constructs relied on before they were moved to FIRSTPRIVATE. It holds only because
# the bounds are read-only - anything assigned in the loop body must stay FIRSTPRIVATE.
if( CMAKE_Fortran_COMPILER_ID MATCHES "NVHPC" )
  set( ECTRANS_LOOP_BOUNDS_CLAUSE "SHARED" )
else()
  set( ECTRANS_LOOP_BOUNDS_CLAUSE "FIRSTPRIVATE" )
endif()

ecbuild_add_option( FEATURE CUTLASS
                    DEFAULT OFF
                    CONDITION HAVE_GPU AND HAVE_CUDA AND CMAKE_Fortran_COMPILER_ID MATCHES "NVHPC"
                    DESCRIPTION "Support for Cutlass BLAS operations"
                    REQUIRED_PACKAGES "NvidiaCutlass VERSION 2.11" )

# following also needs cuda arch sm80 to be effective
ecbuild_add_option( FEATURE CUTLASS_3XTF32
                    DEFAULT ON
                    CONDITION HAVE_SINGLE_PRECISION AND HAVE_CUTLASS
                    DESCRIPTION "Support for 3xTF32 with Cutlass (>= 2.8) and CUDA_ARCHITECTURES >= 80" )

ecbuild_add_option( FEATURE GPU_AWARE_MPI
                    DEFAULT ON
                    CONDITION HAVE_GPU AND HAVE_MPI
                    REQUIRED_PACKAGES "MPI COMPONENTS Fortran"
                    DESCRIPTION "Enable GPU-aware MPI" )

ecbuild_add_option( FEATURE GPU_GRAPHS_GEMM
                    DEFAULT ON
                    CONDITION HAVE_GPU
                    DESCRIPTION "Enable graph-based optimisation of Legendre transform GEMM kernel" )

ecbuild_add_option( FEATURE GPU_GRAPHS_FFT
                    DEFAULT ON
                    CONDITION HAVE_GPU
                    DESCRIPTION "Enable graph-based optimisation of FFT kernels" )

ecbuild_add_option( FEATURE ROCFFT
                    DEFAULT OFF
                    CONDITION HAVE_HIP
                    DESCRIPTION "Use rocFFT library instead of hipFFT for FFT operations on AMD GPUs" )

if( BUILD_SHARED_LIBS )
  set( GPU_STATIC_DEFAULT OFF )
else()
  set( GPU_STATIC_DEFAULT ON )
endif()
ecbuild_add_option( FEATURE GPU_STATIC
                    DEFAULT ${GPU_STATIC_DEFAULT}
                    DESCRIPTION "Compile GPU library as static library"
                    CONDITION HAVE_GPU )

ecbuild_add_option( FEATURE ETRANS
                    DEFAULT OFF
                    DESCRIPTION "Include Limited-Area-Model Transforms" )

# Note: ETRANS GPU does not support OpenMP yet or FFT graphs yet
set( HAVE_ETRANS_GPU 0 )
if( HAVE_ETRANS AND HAVE_GPU )
  if( HAVE_ACC AND NOT HAVE_GRAPHS_FFT )
    set( HAVE_ETRANS_GPU 1 )
  else()
    ecbuild_warn( "ETRANS and GPU features requested, but ACC and GRAPHS_FFT also requested."
                  "The GPU version of etrans only supports OpenACC at the moment, with FFT graphs disabled." )
  endif()
endif()

# ectrans4py Python wrapper (can only be built with one precision)
ecbuild_add_option( FEATURE ECTRANS4PY
                    DEFAULT OFF
                    CONDITION HAVE_ETRANS AND NOT (HAVE_DOUBLE_PRECISION AND HAVE_SINGLE_PRECISION)
                    DESCRIPTION "Compile ectrans4py interface routines for python binding w/ ctypesForFortran" )


ectrans_find_lapack()

ecbuild_add_option( FEATURE FIELD_API
                    DEFAULT OFF
                    DESCRIPTION "Compile field API interface to ectrans"
                    REQUIRED_PACKAGES "field_api" )

ecbuild_add_option( FEATURE FIAT_DEVELOP
                    DEFAULT OFF
                    DESCRIPTION "Explicitly add knowledge that fiat is on develop branch" )

ecbuild_add_option( FEATURE REFERENCE_TESTS DEFAULT OFF
                    DESCRIPTION "Compare each ectrans-benchmark checksum file against references" )

ecbuild_add_option( FEATURE BITID_TESTS DEFAULT OFF
                    DESCRIPTION "Compare each ectrans-benchmark checksum file against build-local references" )

ecbuild_add_option( FEATURE MEMORY_TESTS DEFAULT ON
                    CONDITION HAVE_TESTS AND HAVE_TRANSI
                    DESCRIPTION "Enable memory tests" )

set( _ectrans_default_reference_dir "${ECTRANS_TEST_REFERENCE_DIR}" )
if( NOT DEFINED ECTRANS_TEST_REFERENCE_DIR )
  ectrans_default_reference_dir( _ectrans_default_reference_dir )
endif()
set( ECTRANS_TEST_REFERENCE_DIR
  "${_ectrans_default_reference_dir}"
     CACHE PATH "Directory containing ectrans-benchmark checksum reference files" )
if (HAVE_REFERENCE_TESTS)
  ecbuild_info("Reference tests requested")
  ecbuild_info("    ECTRANS_TEST_REFERENCE_DIR : ${ECTRANS_TEST_REFERENCE_DIR}")
  if (NOT EXISTS "${ECTRANS_TEST_REFERENCE_DIR}")
    ecbuild_warn("Reference tests requested, but reference directory does not exist: ${ECTRANS_TEST_REFERENCE_DIR}.
    After running the bit-identical tests, you can copy the generated checksum reference files to the reference directory using the script:
    ${CMAKE_BINARY_DIR}/tests/ectrans-update-references.sh")
  endif()
endif()

### Add sources
ecbuild_override_compiler_flags(
  COMPILE_FLAGS ${CMAKE_CURRENT_SOURCE_DIR}/cmake/ectrans_compile_flags.cmake
  INHERIT_ECBUILD_FLAGS
)
include( ectrans_contiguous_workaround )
add_subdirectory( src )

### Add tests
if( HAVE_TESTS )
  add_subdirectory( tests )
endif()

### Export
if( BUILD_SHARED_LIBS )
  set( PACKAGE_REQUIRES_PRIVATE_DEPENDENCIES 0 )
else()
  set( PACKAGE_REQUIRES_PRIVATE_DEPENDENCIES 1 )
endif()

include( ectrans_reference_provenance )

ecbuild_install_project( NAME ${PROJECT_NAME} )

ecbuild_print_summary()
