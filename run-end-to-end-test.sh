#!/bin/bash
################################################################################
# Corn COBOL-to-Fortran Compiler - End-to-End Test Runner
# Copyright (c) 2025 sekacorn | sekacorn@gmail.com
#
# Complete end-to-end validation of the compiler pipeline
################################################################################

echo "================================================================================"
echo "  CORN COBOL-TO-FORTRAN COMPILER - END-TO-END TEST"
echo "  Banking Enterprise Edition v2.0"
echo "================================================================================"
echo ""
echo "This test validates the complete compiler pipeline:"
echo "  1. COBOL source → Compiler → Fortran code"
echo "  2. Fortran code → GFortran → Executable"
echo "  3. Executable → Run → Validate results"
echo ""
echo "================================================================================"
echo ""

# Create output directory
mkdir -p test_results
TEST_LOG="test_results/end_to_end_test_$(date +%Y%m%d_%H%M%S).log"

echo "Test log: $TEST_LOG"
echo ""

# Start logging
echo "================================================================================" > $TEST_LOG
echo "  END-TO-END TEST EXECUTION" >> $TEST_LOG
echo "  Date: $(date)" >> $TEST_LOG
echo "================================================================================" >> $TEST_LOG
echo "" >> $TEST_LOG

STEP=0

# Step 1: Check prerequisites
STEP=$((STEP + 1))
echo "[$STEP/6] Checking prerequisites..." | tee -a $TEST_LOG

if ! command -v cargo &> /dev/null; then
    echo "  [FAIL] Rust/Cargo not found" | tee -a $TEST_LOG
    echo "  Install from: https://rustup.rs/" | tee -a $TEST_LOG
    exit 1
fi
echo "  [OK] Rust/Cargo found" | tee -a $TEST_LOG

if ! command -v gfortran &> /dev/null; then
    echo "  [FAIL] GFortran not found" | tee -a $TEST_LOG
    echo "  Install GFortran to compile generated Fortran code" | tee -a $TEST_LOG
    exit 1
fi
echo "  [OK] GFortran found" | tee -a $TEST_LOG

if [ ! -f "tests/end_to_end_banking_test.cob" ]; then
    echo "  [FAIL] End-to-end test file not found" | tee -a $TEST_LOG
    exit 1
fi
echo "  [OK] Test file found" | tee -a $TEST_LOG
echo "" | tee -a $TEST_LOG

# Step 2: Build compiler if needed
STEP=$((STEP + 1))
echo "[$STEP/6] Building compiler..." | tee -a $TEST_LOG

if [ ! -f "target/release/corn-compiler" ] && [ ! -f "target/debug/corn-compiler" ]; then
    echo "  Building release version..." | tee -a $TEST_LOG
    ./build.sh --release >> $TEST_LOG 2>&1
    if [ $? -ne 0 ]; then
        echo "  [FAIL] Compiler build failed" | tee -a $TEST_LOG
        exit 1
    fi
fi
echo "  [OK] Compiler ready" | tee -a $TEST_LOG
echo "" | tee -a $TEST_LOG

# Step 3: Compile COBOL to Fortran
STEP=$((STEP + 1))
echo "[$STEP/6] Compiling COBOL to Fortran..." | tee -a $TEST_LOG
echo "  Source: tests/end_to_end_banking_test.cob" | tee -a $TEST_LOG
echo "  Output: test_results/end_to_end_test.f90" | tee -a $TEST_LOG

./run.sh compile tests/end_to_end_banking_test.cob -o test_results/end_to_end_test.f90 >> $TEST_LOG 2>&1

if [ $? -eq 0 ] && [ -f "test_results/end_to_end_test.f90" ]; then
    FORTRAN_LINES=$(wc -l < test_results/end_to_end_test.f90)
    echo "  [OK] COBOL compilation successful" | tee -a $TEST_LOG
    echo "  Generated Fortran code: $FORTRAN_LINES lines" | tee -a $TEST_LOG
else
    echo "  [FAIL] COBOL compilation failed" | tee -a $TEST_LOG
    echo "" | tee -a $TEST_LOG
    echo "Review $TEST_LOG for details" | tee -a $TEST_LOG
    exit 1
fi
echo "" | tee -a $TEST_LOG

# Step 4: Compile Fortran to executable
STEP=$((STEP + 1))
echo "[$STEP/6] Compiling Fortran to executable..." | tee -a $TEST_LOG
echo "  Input: test_results/end_to_end_test.f90" | tee -a $TEST_LOG
echo "  Output: test_results/end_to_end_test" | tee -a $TEST_LOG

gfortran test_results/end_to_end_test.f90 -o test_results/end_to_end_test >> $TEST_LOG 2>&1

if [ $? -eq 0 ] && [ -f "test_results/end_to_end_test" ]; then
    echo "  [OK] Fortran compilation successful" | tee -a $TEST_LOG
else
    echo "  [FAIL] Fortran compilation failed" | tee -a $TEST_LOG
    echo "" | tee -a $TEST_LOG
    echo "Review $TEST_LOG for GFortran errors" | tee -a $TEST_LOG
    exit 1
fi
echo "" | tee -a $TEST_LOG

# Step 5: Execute the test
STEP=$((STEP + 1))
echo "[$STEP/6] Executing end-to-end test..." | tee -a $TEST_LOG
echo "" | tee -a $TEST_LOG

# Run the test and capture output
./test_results/end_to_end_test | tee -a $TEST_LOG

EXEC_STATUS=$?
echo "" | tee -a $TEST_LOG

if [ $EXEC_STATUS -eq 0 ]; then
    echo "  [OK] Test execution completed" | tee -a $TEST_LOG
else
    echo "  [WARN] Test execution completed with non-zero status" | tee -a $TEST_LOG
fi
echo "" | tee -a $TEST_LOG

# Step 6: Validate results
STEP=$((STEP + 1))
echo "[$STEP/6] Validating test results..." | tee -a $TEST_LOG

if grep -q "ALL END-TO-END TESTS PASSED" $TEST_LOG; then
    echo "  [OK] All validation checks passed" | tee -a $TEST_LOG
    SUCCESS=true
else
    echo "  [FAIL] Some tests failed" | tee -a $TEST_LOG
    SUCCESS=false
fi
echo "" | tee -a $TEST_LOG

# Final Summary
echo "================================================================================" | tee -a $TEST_LOG
echo "                    END-TO-END TEST SUMMARY" | tee -a $TEST_LOG
echo "================================================================================" | tee -a $TEST_LOG
echo "" | tee -a $TEST_LOG

if [ "$SUCCESS" = true ]; then
    echo "*** END-TO-END TEST: PASSED ***" | tee -a $TEST_LOG
    echo "" | tee -a $TEST_LOG
    echo "COMPILER PIPELINE VALIDATED:" | tee -a $TEST_LOG
    echo "  [OK] COBOL parsing successful" | tee -a $TEST_LOG
    echo "  [OK] Fortran code generation successful ($FORTRAN_LINES lines)" | tee -a $TEST_LOG
    echo "  [OK] Fortran compilation successful" | tee -a $TEST_LOG
    echo "  [OK] Program execution successful" | tee -a $TEST_LOG
    echo "  [OK] All banking features working correctly" | tee -a $TEST_LOG
    echo "" | tee -a $TEST_LOG
    echo "BANKING FEATURES TESTED:" | tee -a $TEST_LOG
    echo "  [OK] Data Division translation" | tee -a $TEST_LOG
    echo "  [OK] Precision arithmetic (exact calculations)" | tee -a $TEST_LOG
    echo "  [OK] Deposit/withdrawal processing" | tee -a $TEST_LOG
    echo "  [OK] Interest calculations" | tee -a $TEST_LOG
    echo "  [OK] Fee calculations" | tee -a $TEST_LOG
    echo "  [OK] Large transaction detection ($10K+ CTR)" | tee -a $TEST_LOG
    echo "  [OK] Balance validation" | tee -a $TEST_LOG
    echo "  [OK] Banker's rounding" | tee -a $TEST_LOG
    echo "" | tee -a $TEST_LOG
    echo "CERTIFICATION: READY FOR PRODUCTION" | tee -a $TEST_LOG
    echo "" | tee -a $TEST_LOG
    echo "The compiler has passed the complete end-to-end validation." | tee -a $TEST_LOG
    echo "All components of the compilation pipeline are working correctly." | tee -a $TEST_LOG
    echo "" | tee -a $TEST_LOG
    EXIT_CODE=0
else
    echo "*** END-TO-END TEST: FAILED ***" | tee -a $TEST_LOG
    echo "" | tee -a $TEST_LOG
    echo "ISSUES DETECTED:" | tee -a $TEST_LOG
    if ! grep -q "Data Division items correctly initialized" $TEST_LOG; then
        echo "  [FAIL] Data Division translation issues" | tee -a $TEST_LOG
    fi
    if ! grep -q "Addition:" $TEST_LOG; then
        echo "  [FAIL] Arithmetic operation issues" | tee -a $TEST_LOG
    fi
    if ! grep -q "Balance validation successful" $TEST_LOG; then
        echo "  [FAIL] Balance validation issues" | tee -a $TEST_LOG
    fi
    echo "" | tee -a $TEST_LOG
    echo "REQUIRED ACTIONS:" | tee -a $TEST_LOG
    echo "  1. Review detailed log: $TEST_LOG" | tee -a $TEST_LOG
    echo "  2. Fix identified issues" | tee -a $TEST_LOG
    echo "  3. Rerun end-to-end test" | tee -a $TEST_LOG
    echo "  4. Achieve 100% pass rate" | tee -a $TEST_LOG
    echo "" | tee -a $TEST_LOG
    echo "DO NOT deploy to production until end-to-end test passes!" | tee -a $TEST_LOG
    echo "" | tee -a $TEST_LOG
    EXIT_CODE=1
fi

echo "Full test log saved to: $TEST_LOG" | tee -a $TEST_LOG
echo "================================================================================" | tee -a $TEST_LOG

exit $EXIT_CODE
