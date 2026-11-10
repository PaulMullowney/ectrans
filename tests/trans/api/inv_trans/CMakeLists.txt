# (C) Copyright 2026- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

# API tests for INV_TRANS

# Define list of INV_TRANS tests
set( test_list
  ectrans_test_trans_api_inv_trans_call_mode_2_pgp_on_gpu_noncontiguous
)

# Declare tests that WILL_FAIL
set( will_fail_list
)

# The device-residency test does not depend on the number of OpenMP threads
generate_api_test_suite( SUITE_NAME inv_trans_test_suite
                         TESTS ${test_list}
                         WILL_FAIL_LIST ${will_fail_list}
                         MPIxOMPS 0x1 1x1 2x1
                         BACKENDS cpu_sp cpu_dp gpu_sp gpu_dp )
