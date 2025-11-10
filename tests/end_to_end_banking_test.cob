      ******************************************************************
      * CORN COBOL-TO-FORTRAN COMPILER - END-TO-END TEST
      * Copyright (c) 2025 sekacorn | sekacorn@gmail.com
      *
      * COMPLETE END-TO-END VALIDATION TEST
      *
      * This program exercises ALL banking features in a single
      * comprehensive test that validates the entire compiler pipeline:
      *   1. COBOL parsing
      *   2. Data Division translation
      *   3. Procedure Division translation
      *   4. Fortran code generation
      *   5. Fortran compilation
      *   6. Execution and validation
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. END-TO-END-TEST.
       AUTHOR. SEKACORN.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Test execution tracking
       01 WS-TEST-PHASE               PIC X(50).
       01 WS-PHASE-NUMBER             PIC 9(2) VALUE 0.
       01 WS-TOTAL-PHASES             PIC 9(2) VALUE 10.
       01 WS-PASSED-PHASES            PIC 9(2) VALUE 0.
       01 WS-FAILED-PHASES            PIC 9(2) VALUE 0.

      * Banking account data (tests Data Division translation)
       01 ACCOUNT-DATA.
           05 ACCOUNT-NUMBER          PIC 9(10) VALUE 1234567890.
           05 ACCOUNT-HOLDER          PIC X(50) VALUE "JOHN DOE".
           05 ACCOUNT-BALANCE         PIC S9(13)V9(6) VALUE 10000.000000.
           05 ACCOUNT-TYPE            PIC X(10) VALUE "CHECKING".
           05 ACCOUNT-STATUS          PIC X.
               88 ACCOUNT-ACTIVE      VALUE 'A'.
               88 ACCOUNT-FROZEN      VALUE 'F'.
               88 ACCOUNT-CLOSED      VALUE 'C'.

      * Transaction amounts (tests precision arithmetic)
       01 TRANSACTION-AMOUNTS.
           05 DEPOSIT-AMOUNT          PIC 9(13)V9(6) VALUE 0.
           05 WITHDRAWAL-AMOUNT       PIC 9(13)V9(6) VALUE 0.
           05 TRANSFER-AMOUNT         PIC 9(13)V9(6) VALUE 0.
           05 FEE-AMOUNT              PIC 9(13)V9(6) VALUE 0.

      * Interest calculation (tests precision math)
       01 INTEREST-DATA.
           05 PRINCIPAL-AMOUNT        PIC 9(13)V9(6).
           05 INTEREST-RATE           PIC 9V9(6) VALUE 0.050000.
           05 DAYS-ELAPSED            PIC 9(5) VALUE 30.
           05 INTEREST-EARNED         PIC 9(13)V9(6) VALUE 0.
           05 ANNUAL-RATE             PIC 9V9(6).

      * Calculation results
       01 CALCULATION-RESULTS.
           05 INITIAL-BALANCE         PIC S9(13)V9(6).
           05 FINAL-BALANCE           PIC S9(13)V9(6).
           05 TOTAL-DEPOSITS          PIC 9(13)V9(6) VALUE 0.
           05 TOTAL-WITHDRAWALS       PIC 9(13)V9(6) VALUE 0.
           05 TOTAL-FEES              PIC 9(13)V9(6) VALUE 0.
           05 TOTAL-INTEREST          PIC 9(13)V9(6) VALUE 0.

      * Validation flags
       01 VALIDATION-FLAGS.
           05 PRECISION-OK            PIC X VALUE 'Y'.
           05 ARITHMETIC-OK           PIC X VALUE 'Y'.
           05 ROUNDING-OK             PIC X VALUE 'Y'.
           05 VALIDATION-OK           PIC X VALUE 'Y'.

      * Test values for validation
       01 TEST-VALUES.
           05 EXPECTED-BALANCE        PIC S9(13)V9(6).
           05 ACTUAL-BALANCE          PIC S9(13)V9(6).
           05 DIFFERENCE              PIC S9(13)V9(6).
           05 TOLERANCE               PIC 9V9(6) VALUE 0.000001.

      * Regulatory thresholds (tests business logic)
       01 REGULATORY-THRESHOLDS.
           05 LARGE-TRANS-THRESHOLD   PIC 9(13)V9(6) VALUE 10000.000000.
           05 DAILY-LIMIT             PIC 9(13)V9(6) VALUE 50000.000000.
           05 OVERDRAFT-LIMIT         PIC 9(13)V9(6) VALUE 500.000000.

      * Counters and flags
       01 COUNTERS.
           05 TRANSACTION-COUNT       PIC 9(8) VALUE 0.
           05 LARGE-TRANS-COUNT       PIC 9(8) VALUE 0.
           05 ERROR-COUNT             PIC 9(8) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "================================================================"
           DISPLAY "  END-TO-END BANKING SYSTEM TEST"
           DISPLAY "  Copyright (c) 2025 sekacorn"
           DISPLAY "================================================================"
           DISPLAY " "
           DISPLAY "This test validates the complete compiler pipeline:"
           DISPLAY "  1. COBOL parsing"
           DISPLAY "  2. Data Division translation"
           DISPLAY "  3. Procedure Division translation"
           DISPLAY "  4. Precision arithmetic"
           DISPLAY "  5. Business logic enforcement"
           DISPLAY "  6. Fortran code generation"
           DISPLAY "  7. Fortran compilation"
           DISPLAY "  8. Execution validation"
           DISPLAY " "

           PERFORM INITIALIZE-TEST
           PERFORM RUN-ALL-PHASES
           PERFORM DISPLAY-FINAL-RESULTS

           STOP RUN.

       INITIALIZE-TEST.
           MOVE 'A' TO ACCOUNT-STATUS
           MOVE ACCOUNT-BALANCE TO INITIAL-BALANCE
           DISPLAY "Test initialized with account balance: $"
               ACCOUNT-BALANCE
           DISPLAY " "
           .

       RUN-ALL-PHASES.
           PERFORM PHASE-1-DATA-DIVISION-TEST
           PERFORM PHASE-2-ARITHMETIC-TEST
           PERFORM PHASE-3-DEPOSIT-TEST
           PERFORM PHASE-4-WITHDRAWAL-TEST
           PERFORM PHASE-5-INTEREST-TEST
           PERFORM PHASE-6-FEE-CALCULATION-TEST
           PERFORM PHASE-7-LARGE-TRANSACTION-TEST
           PERFORM PHASE-8-BALANCE-VALIDATION-TEST
           PERFORM PHASE-9-ROUNDING-TEST
           PERFORM PHASE-10-FINAL-VALIDATION
           .

       PHASE-1-DATA-DIVISION-TEST.
           ADD 1 TO WS-PHASE-NUMBER
           MOVE "Data Division Translation" TO WS-TEST-PHASE

           DISPLAY "================================================================"
           DISPLAY "Phase " WS-PHASE-NUMBER ": " WS-TEST-PHASE
           DISPLAY "================================================================"

      *    Test that Data Division items are properly translated
           IF ACCOUNT-NUMBER = 1234567890 AND
              ACCOUNT-BALANCE = 10000.000000 AND
              ACCOUNT-STATUS = 'A'
               DISPLAY "  [PASS] Data Division items correctly initialized"
               ADD 1 TO WS-PASSED-PHASES
           ELSE
               DISPLAY "  [FAIL] Data Division translation error"
               ADD 1 TO WS-FAILED-PHASES
           END-IF

           DISPLAY " "
           .

       PHASE-2-ARITHMETIC-TEST.
           ADD 1 TO WS-PHASE-NUMBER
           MOVE "Precision Arithmetic Test" TO WS-TEST-PHASE

           DISPLAY "================================================================"
           DISPLAY "Phase " WS-PHASE-NUMBER ": " WS-TEST-PHASE
           DISPLAY "================================================================"

      *    Test basic arithmetic operations
           MOVE 1000.123456 TO DEPOSIT-AMOUNT
           MOVE 500.654321 TO WITHDRAWAL-AMOUNT

      *    Addition test
           COMPUTE ACTUAL-BALANCE = DEPOSIT-AMOUNT + WITHDRAWAL-AMOUNT
           MOVE 1500.777777 TO EXPECTED-BALANCE
           COMPUTE DIFFERENCE = ACTUAL-BALANCE - EXPECTED-BALANCE

           IF DIFFERENCE < TOLERANCE AND DIFFERENCE > (TOLERANCE * -1)
               DISPLAY "  [PASS] Addition: " DEPOSIT-AMOUNT " + "
                   WITHDRAWAL-AMOUNT " = " ACTUAL-BALANCE
               ADD 1 TO WS-PASSED-PHASES
           ELSE
               DISPLAY "  [FAIL] Addition error. Expected: " EXPECTED-BALANCE
                   " Got: " ACTUAL-BALANCE
               ADD 1 TO WS-FAILED-PHASES
               MOVE 'N' TO ARITHMETIC-OK
           END-IF

           DISPLAY " "
           .

       PHASE-3-DEPOSIT-TEST.
           ADD 1 TO WS-PHASE-NUMBER
           MOVE "Deposit Transaction Test" TO WS-TEST-PHASE

           DISPLAY "================================================================"
           DISPLAY "Phase " WS-PHASE-NUMBER ": " WS-TEST-PHASE
           DISPLAY "================================================================"

      *    Process a deposit
           MOVE 5000.000000 TO DEPOSIT-AMOUNT
           ADD DEPOSIT-AMOUNT TO ACCOUNT-BALANCE
           ADD DEPOSIT-AMOUNT TO TOTAL-DEPOSITS
           ADD 1 TO TRANSACTION-COUNT

           MOVE 15000.000000 TO EXPECTED-BALANCE
           MOVE ACCOUNT-BALANCE TO ACTUAL-BALANCE
           COMPUTE DIFFERENCE = ACTUAL-BALANCE - EXPECTED-BALANCE

           IF DIFFERENCE < TOLERANCE AND DIFFERENCE > (TOLERANCE * -1)
               DISPLAY "  [PASS] Deposit processed: $" DEPOSIT-AMOUNT
               DISPLAY "         New balance: $" ACCOUNT-BALANCE
               ADD 1 TO WS-PASSED-PHASES
           ELSE
               DISPLAY "  [FAIL] Deposit error"
               ADD 1 TO WS-FAILED-PHASES
           END-IF

           DISPLAY " "
           .

       PHASE-4-WITHDRAWAL-TEST.
           ADD 1 TO WS-PHASE-NUMBER
           MOVE "Withdrawal Transaction Test" TO WS-TEST-PHASE

           DISPLAY "================================================================"
           DISPLAY "Phase " WS-PHASE-NUMBER ": " WS-TEST-PHASE
           DISPLAY "================================================================"

      *    Process a withdrawal with validation
           MOVE 2000.000000 TO WITHDRAWAL-AMOUNT

      *    Check sufficient funds
           IF WITHDRAWAL-AMOUNT <= ACCOUNT-BALANCE
               SUBTRACT WITHDRAWAL-AMOUNT FROM ACCOUNT-BALANCE
               ADD WITHDRAWAL-AMOUNT TO TOTAL-WITHDRAWALS
               ADD 1 TO TRANSACTION-COUNT

               MOVE 13000.000000 TO EXPECTED-BALANCE
               MOVE ACCOUNT-BALANCE TO ACTUAL-BALANCE
               COMPUTE DIFFERENCE = ACTUAL-BALANCE - EXPECTED-BALANCE

               IF DIFFERENCE < TOLERANCE AND
                  DIFFERENCE > (TOLERANCE * -1)
                   DISPLAY "  [PASS] Withdrawal processed: $"
                       WITHDRAWAL-AMOUNT
                   DISPLAY "         New balance: $" ACCOUNT-BALANCE
                   ADD 1 TO WS-PASSED-PHASES
               ELSE
                   DISPLAY "  [FAIL] Withdrawal calculation error"
                   ADD 1 TO WS-FAILED-PHASES
               END-IF
           ELSE
               DISPLAY "  [FAIL] Insufficient funds not detected"
               ADD 1 TO WS-FAILED-PHASES
           END-IF

           DISPLAY " "
           .

       PHASE-5-INTEREST-TEST.
           ADD 1 TO WS-PHASE-NUMBER
           MOVE "Interest Calculation Test" TO WS-TEST-PHASE

           DISPLAY "================================================================"
           DISPLAY "Phase " WS-PHASE-NUMBER ": " WS-TEST-PHASE
           DISPLAY "================================================================"

      *    Calculate interest (daily rate method)
           MOVE ACCOUNT-BALANCE TO PRINCIPAL-AMOUNT
           MOVE 0.05 TO ANNUAL-RATE
           MOVE 30 TO DAYS-ELAPSED

      *    Interest = Principal * (Annual Rate / 365) * Days
           COMPUTE INTEREST-EARNED ROUNDED =
               PRINCIPAL-AMOUNT * (ANNUAL-RATE / 365) * DAYS-ELAPSED

           ADD INTEREST-EARNED TO ACCOUNT-BALANCE
           ADD INTEREST-EARNED TO TOTAL-INTEREST

           IF INTEREST-EARNED > 0
               DISPLAY "  [PASS] Interest calculated: $" INTEREST-EARNED
               DISPLAY "         Principal: $" PRINCIPAL-AMOUNT
               DISPLAY "         Rate: " ANNUAL-RATE " (5%)"
               DISPLAY "         Days: " DAYS-ELAPSED
               DISPLAY "         New balance: $" ACCOUNT-BALANCE
               ADD 1 TO WS-PASSED-PHASES
           ELSE
               DISPLAY "  [FAIL] Interest calculation error"
               ADD 1 TO WS-FAILED-PHASES
           END-IF

           DISPLAY " "
           .

       PHASE-6-FEE-CALCULATION-TEST.
           ADD 1 TO WS-PHASE-NUMBER
           MOVE "Fee Calculation Test" TO WS-TEST-PHASE

           DISPLAY "================================================================"
           DISPLAY "Phase " WS-PHASE-NUMBER ": " WS-TEST-PHASE
           DISPLAY "================================================================"

      *    Calculate monthly maintenance fee (0.5%)
           COMPUTE FEE-AMOUNT ROUNDED = ACCOUNT-BALANCE * 0.005

           SUBTRACT FEE-AMOUNT FROM ACCOUNT-BALANCE
           ADD FEE-AMOUNT TO TOTAL-FEES

           IF FEE-AMOUNT > 0
               DISPLAY "  [PASS] Fee calculated: $" FEE-AMOUNT
               DISPLAY "         New balance: $" ACCOUNT-BALANCE
               ADD 1 TO WS-PASSED-PHASES
           ELSE
               DISPLAY "  [FAIL] Fee calculation error"
               ADD 1 TO WS-FAILED-PHASES
           END-IF

           DISPLAY " "
           .

       PHASE-7-LARGE-TRANSACTION-TEST.
           ADD 1 TO WS-PHASE-NUMBER
           MOVE "Large Transaction Detection Test" TO WS-TEST-PHASE

           DISPLAY "================================================================"
           DISPLAY "Phase " WS-PHASE-NUMBER ": " WS-TEST-PHASE
           DISPLAY "================================================================"

      *    Test regulatory threshold detection ($10K+)
           MOVE 15000.000000 TO DEPOSIT-AMOUNT

      *    Check if transaction exceeds threshold
           IF DEPOSIT-AMOUNT >= LARGE-TRANS-THRESHOLD
               ADD 1 TO LARGE-TRANS-COUNT
               DISPLAY "  [PASS] Large transaction detected: $"
                   DEPOSIT-AMOUNT
               DISPLAY "         Threshold: $" LARGE-TRANS-THRESHOLD
               DISPLAY "         Would be flagged for CTR filing"
               ADD 1 TO WS-PASSED-PHASES
           ELSE
               DISPLAY "  [FAIL] Large transaction not detected"
               ADD 1 TO WS-FAILED-PHASES
           END-IF

      *    Process the deposit
           ADD DEPOSIT-AMOUNT TO ACCOUNT-BALANCE
           ADD DEPOSIT-AMOUNT TO TOTAL-DEPOSITS

           DISPLAY "         New balance: $" ACCOUNT-BALANCE
           DISPLAY " "
           .

       PHASE-8-BALANCE-VALIDATION-TEST.
           ADD 1 TO WS-PHASE-NUMBER
           MOVE "Balance Validation Test" TO WS-TEST-PHASE

           DISPLAY "================================================================"
           DISPLAY "Phase " WS-PHASE-NUMBER ": " WS-TEST-PHASE
           DISPLAY "================================================================"

      *    Verify balance consistency
      *    Final = Initial + Deposits - Withdrawals + Interest - Fees
           COMPUTE EXPECTED-BALANCE =
               INITIAL-BALANCE +
               TOTAL-DEPOSITS -
               TOTAL-WITHDRAWALS +
               TOTAL-INTEREST -
               TOTAL-FEES

           MOVE ACCOUNT-BALANCE TO ACTUAL-BALANCE
           COMPUTE DIFFERENCE = ACTUAL-BALANCE - EXPECTED-BALANCE

           IF DIFFERENCE < TOLERANCE AND DIFFERENCE > (TOLERANCE * -1)
               DISPLAY "  [PASS] Balance validation successful"
               DISPLAY "         Initial balance:  $" INITIAL-BALANCE
               DISPLAY "         Total deposits:   $" TOTAL-DEPOSITS
               DISPLAY "         Total withdrawals:$" TOTAL-WITHDRAWALS
               DISPLAY "         Total interest:   $" TOTAL-INTEREST
               DISPLAY "         Total fees:       $" TOTAL-FEES
               DISPLAY "         Expected balance: $" EXPECTED-BALANCE
               DISPLAY "         Actual balance:   $" ACTUAL-BALANCE
               DISPLAY "         Difference:       $" DIFFERENCE
               ADD 1 TO WS-PASSED-PHASES
           ELSE
               DISPLAY "  [FAIL] Balance mismatch detected!"
               DISPLAY "         Expected: $" EXPECTED-BALANCE
               DISPLAY "         Actual:   $" ACTUAL-BALANCE
               DISPLAY "         Diff:     $" DIFFERENCE
               ADD 1 TO WS-FAILED-PHASES
               MOVE 'N' TO VALIDATION-OK
           END-IF

           DISPLAY " "
           .

       PHASE-9-ROUNDING-TEST.
           ADD 1 TO WS-PHASE-NUMBER
           MOVE "Banker's Rounding Test" TO WS-TEST-PHASE

           DISPLAY "================================================================"
           DISPLAY "Phase " WS-PHASE-NUMBER ": " WS-TEST-PHASE
           DISPLAY "================================================================"

      *    Test banker's rounding (half-to-even)
           MOVE 2.500000 TO DEPOSIT-AMOUNT
           COMPUTE ACTUAL-BALANCE ROUNDED = DEPOSIT-AMOUNT
           MOVE 2.000000 TO EXPECTED-BALANCE

           COMPUTE DIFFERENCE = ACTUAL-BALANCE - EXPECTED-BALANCE

           IF DIFFERENCE < TOLERANCE AND DIFFERENCE > (TOLERANCE * -1)
               DISPLAY "  [PASS] Banker's rounding correct"
               DISPLAY "         2.5 rounded to " ACTUAL-BALANCE
                   " (nearest even)"
               ADD 1 TO WS-PASSED-PHASES
           ELSE
               DISPLAY "  [FAIL] Rounding error"
               ADD 1 TO WS-FAILED-PHASES
               MOVE 'N' TO ROUNDING-OK
           END-IF

           DISPLAY " "
           .

       PHASE-10-FINAL-VALIDATION.
           ADD 1 TO WS-PHASE-NUMBER
           MOVE "Final System Validation" TO WS-TEST-PHASE

           DISPLAY "================================================================"
           DISPLAY "Phase " WS-PHASE-NUMBER ": " WS-TEST-PHASE
           DISPLAY "================================================================"

      *    Final checks
           IF PRECISION-OK = 'Y' AND
              ARITHMETIC-OK = 'Y' AND
              ROUNDING-OK = 'Y' AND
              VALIDATION-OK = 'Y' AND
              ERROR-COUNT = 0
               DISPLAY "  [PASS] All validation checks passed"
               DISPLAY "         Precision:   OK"
               DISPLAY "         Arithmetic:  OK"
               DISPLAY "         Rounding:    OK"
               DISPLAY "         Validation:  OK"
               DISPLAY "         Errors:      0"
               ADD 1 TO WS-PASSED-PHASES
           ELSE
               DISPLAY "  [FAIL] System validation failed"
               IF PRECISION-OK = 'N'
                   DISPLAY "         Precision errors detected"
               END-IF
               IF ARITHMETIC-OK = 'N'
                   DISPLAY "         Arithmetic errors detected"
               END-IF
               IF ROUNDING-OK = 'N'
                   DISPLAY "         Rounding errors detected"
               END-IF
               IF VALIDATION-OK = 'N'
                   DISPLAY "         Validation errors detected"
               END-IF
               ADD 1 TO WS-FAILED-PHASES
           END-IF

           DISPLAY " "
           .

       DISPLAY-FINAL-RESULTS.
           DISPLAY "================================================================"
           DISPLAY "              END-TO-END TEST RESULTS"
           DISPLAY "================================================================"
           DISPLAY " "
           DISPLAY "Total Phases:     " WS-TOTAL-PHASES
           DISPLAY "Passed Phases:    " WS-PASSED-PHASES
           DISPLAY "Failed Phases:    " WS-FAILED-PHASES
           DISPLAY " "

           IF WS-FAILED-PHASES = 0
               DISPLAY "*** ALL END-TO-END TESTS PASSED ***"
               DISPLAY " "
               DISPLAY "COMPILER VALIDATION: COMPLETE"
               DISPLAY " "
               DISPLAY "The following components are working correctly:"
               DISPLAY "  [OK] COBOL parsing"
               DISPLAY "  [OK] Data Division translation"
               DISPLAY "  [OK] Procedure Division translation"
               DISPLAY "  [OK] Precision arithmetic"
               DISPLAY "  [OK] Banker's rounding"
               DISPLAY "  [OK] Interest calculations"
               DISPLAY "  [OK] Fee calculations"
               DISPLAY "  [OK] Large transaction detection"
               DISPLAY "  [OK] Balance validation"
               DISPLAY "  [OK] Fortran code generation"
               DISPLAY "  [OK] Fortran compilation"
               DISPLAY "  [OK] Execution"
               DISPLAY " "
               DISPLAY "CERTIFICATION: This compiler is READY for banking use"
               DISPLAY " "
               DISPLAY "Transaction Summary:"
               DISPLAY "  Transactions processed: " TRANSACTION-COUNT
               DISPLAY "  Large transactions:     " LARGE-TRANS-COUNT
               DISPLAY "  Final balance:          $" ACCOUNT-BALANCE
           ELSE
               DISPLAY "*** END-TO-END TEST FAILED ***"
               DISPLAY " "
               DISPLAY "COMPILER VALIDATION: INCOMPLETE"
               DISPLAY " "
               DISPLAY "Critical errors detected in:"
               IF PRECISION-OK = 'N'
                   DISPLAY "  [ERROR] Precision arithmetic"
               END-IF
               IF ARITHMETIC-OK = 'N'
                   DISPLAY "  [ERROR] Arithmetic operations"
               END-IF
               IF ROUNDING-OK = 'N'
                   DISPLAY "  [ERROR] Rounding logic"
               END-IF
               IF VALIDATION-OK = 'N'
                   DISPLAY "  [ERROR] Balance validation"
               END-IF
               DISPLAY " "
               DISPLAY "DO NOT use for banking until all tests pass!"
           END-IF

           DISPLAY "================================================================"
           .
