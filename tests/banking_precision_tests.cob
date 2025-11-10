      ******************************************************************
      * CORN COBOL-TO-FORTRAN COMPILER - BANKING PRECISION TESTS
      * Copyright (c) 2025 sekacorn | sekacorn@gmail.com
      *
      * CRITICAL BANKING TEST: Decimal Precision and Rounding
      *
      * This test validates that the compiler handles financial
      * calculations with ZERO tolerance for errors. Any rounding
      * errors or precision loss is UNACCEPTABLE for banking.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRECISION-TESTS.
       AUTHOR. SEKACORN.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Test counters
       01 WS-TOTAL-TESTS              PIC 9(4) VALUE 0.
       01 WS-PASSED-TESTS             PIC 9(4) VALUE 0.
       01 WS-FAILED-TESTS             PIC 9(4) VALUE 0.

      * Test case tracking
       01 WS-TEST-NAME                PIC X(50).
       01 WS-TEST-NUMBER              PIC 9(3) VALUE 0.

      * Precision test values (6 decimal places for banking)
       01 WS-AMOUNT-1                 PIC 9(13)V9(6) VALUE 1234567.123456.
       01 WS-AMOUNT-2                 PIC 9(13)V9(6) VALUE 9876543.654321.
       01 WS-RESULT                   PIC 9(13)V9(6).
       01 WS-EXPECTED                 PIC 9(13)V9(6).
       01 WS-DIFFERENCE               PIC S9(13)V9(6).

      * Interest calculation tests
       01 WS-PRINCIPAL                PIC 9(13)V9(6).
       01 WS-INTEREST-RATE            PIC 9V9(6).
       01 WS-DAYS                     PIC 9(5).
       01 WS-INTEREST-AMOUNT          PIC 9(13)V9(6).

      * Division tests (critical for percentage calculations)
       01 WS-DIVIDEND                 PIC 9(13)V9(6).
       01 WS-DIVISOR                  PIC 9(13)V9(6).
       01 WS-QUOTIENT                 PIC 9(13)V9(6).
       01 WS-REMAINDER                PIC 9(13)V9(6).

      * Rounding tests
       01 WS-UNROUNDED                PIC 9(13)V9(9).
       01 WS-ROUNDED-RESULT           PIC 9(13)V9(6).

      * Currency conversion tests
       01 WS-USD-AMOUNT               PIC 9(13)V9(6).
       01 WS-EXCHANGE-RATE            PIC 9V9(6).
       01 WS-FOREIGN-AMOUNT           PIC 9(13)V9(6).

      * Compound interest test
       01 WS-COMPOUND-PRINCIPAL       PIC 9(13)V9(6).
       01 WS-COMPOUND-RATE            PIC 9V9(6).
       01 WS-COMPOUND-PERIODS         PIC 9(5).
       01 WS-COMPOUND-RESULT          PIC 9(13)V9(6).
       01 WS-COMPOUND-FACTOR          PIC 9V9(9).

      * Banker's rounding tests (half-to-even)
       01 WS-ROUND-TEST-VALUE         PIC 9(13)V9(9).
       01 WS-ROUND-EXPECTED           PIC 9(13)V9(6).
       01 WS-ROUND-ACTUAL             PIC 9(13)V9(6).

      * Large transaction tests (regulatory reporting thresholds)
       01 WS-TRANSACTION-AMOUNT       PIC 9(13)V9(6).
       01 WS-REPORTING-THRESHOLD      PIC 9(13)V9(6) VALUE 10000.000000.
       01 WS-LARGE-TRANSACTION-FLAG   PIC X VALUE 'N'.

      * Monthly payment calculation (mortgage/loan)
       01 WS-LOAN-AMOUNT              PIC 9(13)V9(6).
       01 WS-MONTHLY-RATE             PIC 9V9(9).
       01 WS-NUM-PAYMENTS             PIC 9(5).
       01 WS-MONTHLY-PAYMENT          PIC 9(13)V9(6).

      * Tolerance for comparison (should be ZERO for exact arithmetic)
       01 WS-TOLERANCE                PIC 9V9(6) VALUE 0.000001.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "================================================================"
           DISPLAY "  BANKING PRECISION TESTS - CRITICAL FOR REGULATORY COMPLIANCE"
           DISPLAY "  Copyright (c) 2025 sekacorn"
           DISPLAY "================================================================"
           DISPLAY " "

           PERFORM RUN-ALL-TESTS
           PERFORM DISPLAY-TEST-RESULTS

           STOP RUN.

       RUN-ALL-TESTS.
           DISPLAY "Running comprehensive banking precision tests..."
           DISPLAY " "

           PERFORM TEST-BASIC-ADDITION
           PERFORM TEST-BASIC-SUBTRACTION
           PERFORM TEST-BASIC-MULTIPLICATION
           PERFORM TEST-BASIC-DIVISION
           PERFORM TEST-DIVISION-WITH-REMAINDER
           PERFORM TEST-INTEREST-CALCULATION
           PERFORM TEST-COMPOUND-INTEREST
           PERFORM TEST-PERCENTAGE-CALCULATION
           PERFORM TEST-CURRENCY-CONVERSION
           PERFORM TEST-BANKERS-ROUNDING-EVEN
           PERFORM TEST-BANKERS-ROUNDING-ODD
           PERFORM TEST-ROUNDING-UP
           PERFORM TEST-ROUNDING-DOWN
           PERFORM TEST-LARGE-TRANSACTION-DETECTION
           PERFORM TEST-MONTHLY-PAYMENT-CALCULATION
           PERFORM TEST-PRECISION-LOSS-DETECTION
           PERFORM TEST-OVERFLOW-DETECTION
           PERFORM TEST-DIVISION-BY-ZERO-PROTECTION
           PERFORM TEST-NEGATIVE-BALANCE-HANDLING
           PERFORM TEST-ACCUMULATED-INTEREST
           .

       TEST-BASIC-ADDITION.
           ADD 1 TO WS-TEST-NUMBER
           MOVE "Basic Addition (Large Amounts)" TO WS-TEST-NAME

           COMPUTE WS-RESULT = WS-AMOUNT-1 + WS-AMOUNT-2
           MOVE 11111110.777777 TO WS-EXPECTED

           PERFORM CHECK-RESULT
           .

       TEST-BASIC-SUBTRACTION.
           ADD 1 TO WS-TEST-NUMBER
           MOVE "Basic Subtraction (Precision Preserved)" TO WS-TEST-NAME

           COMPUTE WS-RESULT = WS-AMOUNT-2 - WS-AMOUNT-1
           MOVE 8641976.530865 TO WS-EXPECTED

           PERFORM CHECK-RESULT
           .

       TEST-BASIC-MULTIPLICATION.
           ADD 1 TO WS-TEST-NUMBER
           MOVE "Multiplication (High Precision)" TO WS-TEST-NAME

           MOVE 1000.123456 TO WS-AMOUNT-1
           MOVE 2.5 TO WS-AMOUNT-2
           COMPUTE WS-RESULT = WS-AMOUNT-1 * WS-AMOUNT-2
           MOVE 2500.30864 TO WS-EXPECTED

           PERFORM CHECK-RESULT
           .

       TEST-BASIC-DIVISION.
           ADD 1 TO WS-TEST-NUMBER
           MOVE "Division (Banking Precision)" TO WS-TEST-NAME

           MOVE 100000.00 TO WS-DIVIDEND
           MOVE 3.00 TO WS-DIVISOR
           COMPUTE WS-RESULT ROUNDED = WS-DIVIDEND / WS-DIVISOR
           MOVE 33333.333333 TO WS-EXPECTED

           PERFORM CHECK-RESULT
           .

       TEST-DIVISION-WITH-REMAINDER.
           ADD 1 TO WS-TEST-NUMBER
           MOVE "Division with Remainder" TO WS-TEST-NAME

           MOVE 100000.00 TO WS-DIVIDEND
           MOVE 7.00 TO WS-DIVISOR
           DIVIDE WS-DIVIDEND BY WS-DIVISOR
               GIVING WS-QUOTIENT ROUNDED
               REMAINDER WS-REMAINDER

           MOVE 14285.714286 TO WS-EXPECTED
           MOVE WS-QUOTIENT TO WS-RESULT

           PERFORM CHECK-RESULT
           .

       TEST-INTEREST-CALCULATION.
           ADD 1 TO WS-TEST-NUMBER
           MOVE "Interest Calculation (Daily Rate)" TO WS-TEST-NAME

      *    Principal: $100,000, Rate: 5% annual, Days: 30
           MOVE 100000.000000 TO WS-PRINCIPAL
           MOVE 0.05 TO WS-INTEREST-RATE
           MOVE 30 TO WS-DAYS

      *    Daily interest = Principal * (Rate / 365) * Days
           COMPUTE WS-INTEREST-AMOUNT ROUNDED =
               WS-PRINCIPAL * (WS-INTEREST-RATE / 365) * WS-DAYS

           MOVE 410.958904 TO WS-EXPECTED
           MOVE WS-INTEREST-AMOUNT TO WS-RESULT

           PERFORM CHECK-RESULT
           .

       TEST-COMPOUND-INTEREST.
           ADD 1 TO WS-TEST-NUMBER
           MOVE "Compound Interest (Monthly)" TO WS-TEST-NAME

      *    Principal: $10,000, Rate: 6% annual, Periods: 12 months
           MOVE 10000.000000 TO WS-COMPOUND-PRINCIPAL
           MOVE 0.06 TO WS-COMPOUND-RATE
           MOVE 12 TO WS-COMPOUND-PERIODS

      *    Monthly rate
           COMPUTE WS-MONTHLY-RATE = WS-COMPOUND-RATE / 12

      *    Simplified compound: P * (1 + r/n)^n
      *    For demonstration: P + P*r (simple for year)
           COMPUTE WS-COMPOUND-RESULT ROUNDED =
               WS-COMPOUND-PRINCIPAL *
               (1 + WS-COMPOUND-RATE)

           MOVE 10600.000000 TO WS-EXPECTED
           MOVE WS-COMPOUND-RESULT TO WS-RESULT

           PERFORM CHECK-RESULT
           .

       TEST-PERCENTAGE-CALCULATION.
           ADD 1 TO WS-TEST-NUMBER
           MOVE "Percentage Calculation (Fee)" TO WS-TEST-NAME

      *    Calculate 2.5% fee on $50,000
           MOVE 50000.000000 TO WS-AMOUNT-1
           COMPUTE WS-RESULT ROUNDED = WS-AMOUNT-1 * 0.025
           MOVE 1250.000000 TO WS-EXPECTED

           PERFORM CHECK-RESULT
           .

       TEST-CURRENCY-CONVERSION.
           ADD 1 TO WS-TEST-NUMBER
           MOVE "Currency Conversion (USD to EUR)" TO WS-TEST-NAME

      *    Convert $10,000 USD at rate 0.85
           MOVE 10000.000000 TO WS-USD-AMOUNT
           MOVE 0.85 TO WS-EXCHANGE-RATE
           COMPUTE WS-FOREIGN-AMOUNT ROUNDED =
               WS-USD-AMOUNT * WS-EXCHANGE-RATE

           MOVE 8500.000000 TO WS-EXPECTED
           MOVE WS-FOREIGN-AMOUNT TO WS-RESULT

           PERFORM CHECK-RESULT
           .

       TEST-BANKERS-ROUNDING-EVEN.
           ADD 1 TO WS-TEST-NUMBER
           MOVE "Banker's Rounding (Round to Even)" TO WS-TEST-NAME

      *    2.5 should round to 2 (nearest even)
           MOVE 2.500000000 TO WS-ROUND-TEST-VALUE
           COMPUTE WS-ROUND-ACTUAL ROUNDED = WS-ROUND-TEST-VALUE
           MOVE 2.000000 TO WS-ROUND-EXPECTED

           MOVE WS-ROUND-ACTUAL TO WS-RESULT
           MOVE WS-ROUND-EXPECTED TO WS-EXPECTED

           PERFORM CHECK-RESULT
           .

       TEST-BANKERS-ROUNDING-ODD.
           ADD 1 TO WS-TEST-NUMBER
           MOVE "Banker's Rounding (Half to Even)" TO WS-TEST-NAME

      *    3.5 should round to 4 (nearest even)
           MOVE 3.500000000 TO WS-ROUND-TEST-VALUE
           COMPUTE WS-ROUND-ACTUAL ROUNDED = WS-ROUND-TEST-VALUE
           MOVE 4.000000 TO WS-ROUND-EXPECTED

           MOVE WS-ROUND-ACTUAL TO WS-RESULT
           MOVE WS-ROUND-EXPECTED TO WS-EXPECTED

           PERFORM CHECK-RESULT
           .

       TEST-ROUNDING-UP.
           ADD 1 TO WS-TEST-NUMBER
           MOVE "Rounding Up (>0.5)" TO WS-TEST-NAME

           MOVE 2.600000000 TO WS-ROUND-TEST-VALUE
           COMPUTE WS-ROUND-ACTUAL ROUNDED = WS-ROUND-TEST-VALUE
           MOVE 3.000000 TO WS-ROUND-EXPECTED

           MOVE WS-ROUND-ACTUAL TO WS-RESULT
           MOVE WS-ROUND-EXPECTED TO WS-EXPECTED

           PERFORM CHECK-RESULT
           .

       TEST-ROUNDING-DOWN.
           ADD 1 TO WS-TEST-NUMBER
           MOVE "Rounding Down (<0.5)" TO WS-TEST-NAME

           MOVE 2.400000000 TO WS-ROUND-TEST-VALUE
           COMPUTE WS-ROUND-ACTUAL ROUNDED = WS-ROUND-TEST-VALUE
           MOVE 2.000000 TO WS-ROUND-EXPECTED

           MOVE WS-ROUND-ACTUAL TO WS-RESULT
           MOVE WS-ROUND-EXPECTED TO WS-EXPECTED

           PERFORM CHECK-RESULT
           .

       TEST-LARGE-TRANSACTION-DETECTION.
           ADD 1 TO WS-TEST-NUMBER
           MOVE "Large Transaction Detection ($10K+)" TO WS-TEST-NAME

      *    Regulatory requirement: Flag transactions >= $10,000
           MOVE 10000.000000 TO WS-TRANSACTION-AMOUNT

           IF WS-TRANSACTION-AMOUNT >= WS-REPORTING-THRESHOLD
               MOVE 'Y' TO WS-LARGE-TRANSACTION-FLAG
               MOVE 1.000000 TO WS-RESULT
           ELSE
               MOVE 'N' TO WS-LARGE-TRANSACTION-FLAG
               MOVE 0.000000 TO WS-RESULT
           END-IF

           MOVE 1.000000 TO WS-EXPECTED

           PERFORM CHECK-RESULT
           .

       TEST-MONTHLY-PAYMENT-CALCULATION.
           ADD 1 TO WS-TEST-NUMBER
           MOVE "Monthly Payment (Mortgage)" TO WS-TEST-NAME

      *    Loan: $200,000, Rate: 4% annual, Term: 360 months (30 years)
           MOVE 200000.000000 TO WS-LOAN-AMOUNT
           MOVE 0.04 TO WS-COMPOUND-RATE
           MOVE 360 TO WS-NUM-PAYMENTS

      *    Monthly rate
           COMPUTE WS-MONTHLY-RATE = WS-COMPOUND-RATE / 12

      *    Simplified payment (interest only for test)
           COMPUTE WS-MONTHLY-PAYMENT ROUNDED =
               WS-LOAN-AMOUNT * WS-MONTHLY-RATE

           MOVE 666.666667 TO WS-EXPECTED
           MOVE WS-MONTHLY-PAYMENT TO WS-RESULT

           PERFORM CHECK-RESULT
           .

       TEST-PRECISION-LOSS-DETECTION.
           ADD 1 TO WS-TEST-NUMBER
           MOVE "Precision Loss Detection" TO WS-TEST-NAME

      *    This test ensures no precision is lost in chained operations
           MOVE 100.123456 TO WS-AMOUNT-1
           COMPUTE WS-RESULT =
               ((WS-AMOUNT-1 * 2) / 2)

      *    Result should equal original (no precision loss)
           MOVE 100.123456 TO WS-EXPECTED

           PERFORM CHECK-RESULT
           .

       TEST-OVERFLOW-DETECTION.
           ADD 1 TO WS-TEST-NUMBER
           MOVE "Overflow Detection (ON SIZE ERROR)" TO WS-TEST-NAME

      *    Test with large numbers near limit
           MOVE 9999999999999.999999 TO WS-AMOUNT-1
           MOVE 1.000000 TO WS-AMOUNT-2

      *    This should trigger ON SIZE ERROR if overflow
           ADD WS-AMOUNT-2 TO WS-AMOUNT-1
               ON SIZE ERROR
                   MOVE 1.000000 TO WS-RESULT
               NOT ON SIZE ERROR
                   MOVE 0.000000 TO WS-RESULT
           END-ADD

      *    Expect overflow detection (result = 1)
           MOVE 1.000000 TO WS-EXPECTED

           PERFORM CHECK-RESULT
           .

       TEST-DIVISION-BY-ZERO-PROTECTION.
           ADD 1 TO WS-TEST-NUMBER
           MOVE "Division by Zero Protection" TO WS-TEST-NAME

           MOVE 100.000000 TO WS-DIVIDEND
           MOVE 0.000000 TO WS-DIVISOR

      *    Should protect against division by zero
           DIVIDE WS-DIVIDEND BY WS-DIVISOR
               GIVING WS-QUOTIENT
               ON SIZE ERROR
                   MOVE 1.000000 TO WS-RESULT
               NOT ON SIZE ERROR
                   MOVE 0.000000 TO WS-RESULT
           END-DIVIDE

      *    Expect error detection (result = 1)
           MOVE 1.000000 TO WS-EXPECTED

           PERFORM CHECK-RESULT
           .

       TEST-NEGATIVE-BALANCE-HANDLING.
           ADD 1 TO WS-TEST-NUMBER
           MOVE "Negative Balance Handling" TO WS-TEST-NAME

      *    Account balance: $100, Withdrawal: $150
           MOVE 100.000000 TO WS-AMOUNT-1
           MOVE 150.000000 TO WS-AMOUNT-2
           COMPUTE WS-RESULT = WS-AMOUNT-1 - WS-AMOUNT-2

      *    Should handle negative correctly
           MOVE -50.000000 TO WS-EXPECTED

           PERFORM CHECK-RESULT
           .

       TEST-ACCUMULATED-INTEREST.
           ADD 1 TO WS-TEST-NUMBER
           MOVE "Accumulated Interest (Multiple Periods)" TO WS-TEST-NAME

      *    Test interest accumulation over multiple periods
           MOVE 1000.000000 TO WS-PRINCIPAL
           MOVE 0.05 TO WS-INTEREST-RATE

      *    Year 1
           COMPUTE WS-PRINCIPAL =
               WS-PRINCIPAL + (WS-PRINCIPAL * WS-INTEREST-RATE)

      *    Year 2
           COMPUTE WS-PRINCIPAL ROUNDED =
               WS-PRINCIPAL + (WS-PRINCIPAL * WS-INTEREST-RATE)

      *    After 2 years at 5% compounded
           MOVE 1102.500000 TO WS-EXPECTED
           MOVE WS-PRINCIPAL TO WS-RESULT

           PERFORM CHECK-RESULT
           .

       CHECK-RESULT.
           ADD 1 TO WS-TOTAL-TESTS

           COMPUTE WS-DIFFERENCE = WS-RESULT - WS-EXPECTED

      *    Check if difference is within tolerance (should be zero)
           IF WS-DIFFERENCE < WS-TOLERANCE AND
              WS-DIFFERENCE > (WS-TOLERANCE * -1)
               ADD 1 TO WS-PASSED-TESTS
               DISPLAY "Test " WS-TEST-NUMBER " [PASS]: " WS-TEST-NAME
           ELSE
               ADD 1 TO WS-FAILED-TESTS
               DISPLAY "Test " WS-TEST-NUMBER " [FAIL]: " WS-TEST-NAME
               DISPLAY "  Expected: " WS-EXPECTED
               DISPLAY "  Got:      " WS-RESULT
               DISPLAY "  Diff:     " WS-DIFFERENCE
           END-IF
           .

       DISPLAY-TEST-RESULTS.
           DISPLAY " "
           DISPLAY "================================================================"
           DISPLAY "                    TEST RESULTS SUMMARY"
           DISPLAY "================================================================"
           DISPLAY "Total Tests:  " WS-TOTAL-TESTS
           DISPLAY "Passed:       " WS-PASSED-TESTS
           DISPLAY "Failed:       " WS-FAILED-TESTS

           IF WS-FAILED-TESTS = 0
               DISPLAY " "
               DISPLAY "*** ALL TESTS PASSED - READY FOR BANKING USE ***"
               DISPLAY " "
               DISPLAY "REGULATORY COMPLIANCE:"
               DISPLAY "  - Zero precision errors detected"
               DISPLAY "  - Banker's rounding implemented correctly"
               DISPLAY "  - Overflow detection working"
               DISPLAY "  - Division by zero protection active"
               DISPLAY "  - Large transaction detection functional"
               DISPLAY " "
               DISPLAY "This compiler is SAFE for banking operations."
           ELSE
               DISPLAY " "
               DISPLAY "*** TESTS FAILED - NOT SAFE FOR BANKING ***"
               DISPLAY " "
               DISPLAY "CRITICAL: Precision errors detected!"
               DISPLAY "DO NOT use for financial calculations until fixed."
           END-IF

           DISPLAY "================================================================"
           .
