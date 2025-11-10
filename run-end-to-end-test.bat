@echo off
REM ============================================================================
REM Corn COBOL-to-Fortran Compiler - End-to-End Test Runner (Windows)
REM Copyright (c) 2025 sekacorn | sekacorn@gmail.com
REM
REM Complete end-to-end validation of the compiler pipeline
REM ============================================================================

echo ================================================================================
echo   CORN COBOL-TO-FORTRAN COMPILER - END-TO-END TEST
echo   Banking Enterprise Edition v2.0
echo ================================================================================
echo.
echo This test validates the complete compiler pipeline:
echo   1. COBOL source - Compiler - Fortran code
echo   2. Fortran code - GFortran - Executable
echo   3. Executable - Run - Validate results
echo.
echo ================================================================================
echo.

REM Create output directory
if not exist "test_results" mkdir test_results
set TEST_LOG=test_results\end_to_end_test_%date:~-4,4%%date:~-10,2%%date:~-7,2%_%time:~0,2%%time:~3,2%%time:~6,2%.log
set TEST_LOG=%TEST_LOG: =0%

echo Test log: %TEST_LOG%
echo.

REM Start logging
echo ================================================================================ > %TEST_LOG%
echo   END-TO-END TEST EXECUTION >> %TEST_LOG%
echo   Date: %date% %time% >> %TEST_LOG%
echo ================================================================================ >> %TEST_LOG%
echo. >> %TEST_LOG%

set STEP=0

REM Step 1: Check prerequisites
set /a STEP=%STEP%+1
echo [%STEP%/6] Checking prerequisites...
echo [%STEP%/6] Checking prerequisites... >> %TEST_LOG%

where cargo >nul 2>nul
if %errorlevel% neq 0 (
    echo   [FAIL] Rust/Cargo not found
    echo   [FAIL] Rust/Cargo not found >> %TEST_LOG%
    echo   Install from: https://rustup.rs/
    pause
    exit /b 1
)
echo   [OK] Rust/Cargo found
echo   [OK] Rust/Cargo found >> %TEST_LOG%

where gfortran >nul 2>nul
if %errorlevel% neq 0 (
    echo   [FAIL] GFortran not found
    echo   [FAIL] GFortran not found >> %TEST_LOG%
    echo   Install GFortran to compile generated Fortran code
    pause
    exit /b 1
)
echo   [OK] GFortran found
echo   [OK] GFortran found >> %TEST_LOG%

if not exist "tests\end_to_end_banking_test.cob" (
    echo   [FAIL] End-to-end test file not found
    echo   [FAIL] End-to-end test file not found >> %TEST_LOG%
    pause
    exit /b 1
)
echo   [OK] Test file found
echo   [OK] Test file found >> %TEST_LOG%
echo.
echo. >> %TEST_LOG%

REM Step 2: Build compiler if needed
set /a STEP=%STEP%+1
echo [%STEP%/6] Building compiler...
echo [%STEP%/6] Building compiler... >> %TEST_LOG%

if not exist "target\release\corn-compiler.exe" (
    if not exist "target\debug\corn-compiler.exe" (
        echo   Building release version...
        echo   Building release version... >> %TEST_LOG%
        call build.bat --release >> %TEST_LOG% 2>&1
        if %errorlevel% neq 0 (
            echo   [FAIL] Compiler build failed
            echo   [FAIL] Compiler build failed >> %TEST_LOG%
            pause
            exit /b 1
        )
    )
)
echo   [OK] Compiler ready
echo   [OK] Compiler ready >> %TEST_LOG%
echo.
echo. >> %TEST_LOG%

REM Step 3: Compile COBOL to Fortran
set /a STEP=%STEP%+1
echo [%STEP%/6] Compiling COBOL to Fortran...
echo [%STEP%/6] Compiling COBOL to Fortran... >> %TEST_LOG%
echo   Source: tests\end_to_end_banking_test.cob
echo   Source: tests\end_to_end_banking_test.cob >> %TEST_LOG%
echo   Output: test_results\end_to_end_test.f90
echo   Output: test_results\end_to_end_test.f90 >> %TEST_LOG%

call run.bat compile tests\end_to_end_banking_test.cob -o test_results\end_to_end_test.f90 >> %TEST_LOG% 2>&1

if %errorlevel% equ 0 (
    if exist "test_results\end_to_end_test.f90" (
        echo   [OK] COBOL compilation successful
        echo   [OK] COBOL compilation successful >> %TEST_LOG%
    ) else (
        echo   [FAIL] Fortran file not generated
        echo   [FAIL] Fortran file not generated >> %TEST_LOG%
        pause
        exit /b 1
    )
) else (
    echo   [FAIL] COBOL compilation failed
    echo   [FAIL] COBOL compilation failed >> %TEST_LOG%
    echo.
    echo Review %TEST_LOG% for details
    pause
    exit /b 1
)
echo.
echo. >> %TEST_LOG%

REM Step 4: Compile Fortran to executable
set /a STEP=%STEP%+1
echo [%STEP%/6] Compiling Fortran to executable...
echo [%STEP%/6] Compiling Fortran to executable... >> %TEST_LOG%
echo   Input: test_results\end_to_end_test.f90
echo   Input: test_results\end_to_end_test.f90 >> %TEST_LOG%
echo   Output: test_results\end_to_end_test.exe
echo   Output: test_results\end_to_end_test.exe >> %TEST_LOG%

gfortran test_results\end_to_end_test.f90 -o test_results\end_to_end_test.exe >> %TEST_LOG% 2>&1

if %errorlevel% equ 0 (
    if exist "test_results\end_to_end_test.exe" (
        echo   [OK] Fortran compilation successful
        echo   [OK] Fortran compilation successful >> %TEST_LOG%
    ) else (
        echo   [FAIL] Executable not generated
        echo   [FAIL] Executable not generated >> %TEST_LOG%
        pause
        exit /b 1
    )
) else (
    echo   [FAIL] Fortran compilation failed
    echo   [FAIL] Fortran compilation failed >> %TEST_LOG%
    echo.
    echo Review %TEST_LOG% for GFortran errors
    pause
    exit /b 1
)
echo.
echo. >> %TEST_LOG%

REM Step 5: Execute the test
set /a STEP=%STEP%+1
echo [%STEP%/6] Executing end-to-end test...
echo [%STEP%/6] Executing end-to-end test... >> %TEST_LOG%
echo.
echo. >> %TEST_LOG%

REM Run the test and capture output
test_results\end_to_end_test.exe >> %TEST_LOG%
set EXEC_STATUS=%errorlevel%

echo.
echo. >> %TEST_LOG%

if %EXEC_STATUS% equ 0 (
    echo   [OK] Test execution completed
    echo   [OK] Test execution completed >> %TEST_LOG%
) else (
    echo   [WARN] Test execution completed with non-zero status
    echo   [WARN] Test execution completed with non-zero status >> %TEST_LOG%
)
echo.
echo. >> %TEST_LOG%

REM Step 6: Validate results
set /a STEP=%STEP%+1
echo [%STEP%/6] Validating test results...
echo [%STEP%/6] Validating test results... >> %TEST_LOG%

findstr /C:"ALL END-TO-END TESTS PASSED" %TEST_LOG% >nul
if %errorlevel% equ 0 (
    echo   [OK] All validation checks passed
    echo   [OK] All validation checks passed >> %TEST_LOG%
    set SUCCESS=true
) else (
    echo   [FAIL] Some tests failed
    echo   [FAIL] Some tests failed >> %TEST_LOG%
    set SUCCESS=false
)
echo.
echo. >> %TEST_LOG%

REM Final Summary
echo ================================================================================ >> %TEST_LOG%
echo                     END-TO-END TEST SUMMARY >> %TEST_LOG%
echo ================================================================================ >> %TEST_LOG%
echo. >> %TEST_LOG%

echo ================================================================================
echo                     END-TO-END TEST SUMMARY
echo ================================================================================
echo.

if "%SUCCESS%"=="true" (
    echo *** END-TO-END TEST: PASSED ***
    echo *** END-TO-END TEST: PASSED *** >> %TEST_LOG%
    echo.
    echo. >> %TEST_LOG%
    echo COMPILER PIPELINE VALIDATED:
    echo   - COBOL parsing successful
    echo   - Fortran code generation successful
    echo   - Fortran compilation successful
    echo   - Program execution successful
    echo   - All banking features working correctly
    echo.
    echo BANKING FEATURES TESTED:
    echo   - Data Division translation
    echo   - Precision arithmetic (exact calculations^)
    echo   - Deposit/withdrawal processing
    echo   - Interest calculations
    echo   - Fee calculations
    echo   - Large transaction detection ($10K+ CTR^)
    echo   - Balance validation
    echo   - Banker's rounding
    echo.
    echo CERTIFICATION: READY FOR PRODUCTION
    echo.
    echo The compiler has passed the complete end-to-end validation.
    echo All components of the compilation pipeline are working correctly.
    echo.
    set EXIT_CODE=0
) else (
    echo *** END-TO-END TEST: FAILED ***
    echo *** END-TO-END TEST: FAILED *** >> %TEST_LOG%
    echo.
    echo. >> %TEST_LOG%
    echo ISSUES DETECTED
    echo.
    echo REQUIRED ACTIONS:
    echo   1. Review detailed log: %TEST_LOG%
    echo   2. Fix identified issues
    echo   3. Rerun end-to-end test
    echo   4. Achieve 100%% pass rate
    echo.
    echo DO NOT deploy to production until end-to-end test passes!
    echo.
    set EXIT_CODE=1
)

echo Full test log saved to: %TEST_LOG%
echo ================================================================================

pause
exit /b %EXIT_CODE%
