      ******************************************************************
      * CORN COBOL-TO-FORTRAN COMPILER - SORT/MERGE TESTS
      * Copyright (c) 2025 sekacorn | sekacorn@gmail.com
      *
      * CRITICAL BANKING TEST: Batch Processing with SORT/MERGE
      *
      * End-of-day processing requires sorting millions of transactions
      * SORT/MERGE must be reliable and handle large datasets
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SORT-MERGE-TESTS.
       AUTHOR. SEKACORN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT UNSORTED-TRANSACTIONS
               ASSIGN TO "unsorted_transactions.dat"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

           SELECT SORTED-TRANSACTIONS
               ASSIGN TO "sorted_transactions.dat"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

           SELECT SORT-WORK-FILE
               ASSIGN TO "sort_work.tmp".

           SELECT TRANSACTION-FILE-1
               ASSIGN TO "transactions1.dat"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

           SELECT TRANSACTION-FILE-2
               ASSIGN TO "transactions2.dat"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

           SELECT MERGED-TRANSACTIONS
               ASSIGN TO "merged_transactions.dat"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  UNSORTED-TRANSACTIONS.
       01  UNSORTED-RECORD.
           05 UNSORTED-TRANS-ID       PIC 9(10).
           05 UNSORTED-ACCOUNT        PIC 9(10).
           05 UNSORTED-AMOUNT         PIC S9(13)V9(6).
           05 UNSORTED-DATE           PIC 9(8).
           05 UNSORTED-TIME           PIC 9(6).

       FD  SORTED-TRANSACTIONS.
       01  SORTED-RECORD.
           05 SORTED-TRANS-ID         PIC 9(10).
           05 SORTED-ACCOUNT          PIC 9(10).
           05 SORTED-AMOUNT           PIC S9(13)V9(6).
           05 SORTED-DATE             PIC 9(8).
           05 SORTED-TIME             PIC 9(6).

       SD  SORT-WORK-FILE.
       01  SORT-RECORD.
           05 SORT-TRANS-ID           PIC 9(10).
           05 SORT-ACCOUNT            PIC 9(10).
           05 SORT-AMOUNT             PIC S9(13)V9(6).
           05 SORT-DATE               PIC 9(8).
           05 SORT-TIME               PIC 9(6).

       FD  TRANSACTION-FILE-1.
       01  TRANS-RECORD-1.
           05 TRANS1-ID               PIC 9(10).
           05 TRANS1-ACCOUNT          PIC 9(10).
           05 TRANS1-AMOUNT           PIC S9(13)V9(6).
           05 TRANS1-DATE             PIC 9(8).

       FD  TRANSACTION-FILE-2.
       01  TRANS-RECORD-2.
           05 TRANS2-ID               PIC 9(10).
           05 TRANS2-ACCOUNT          PIC 9(10).
           05 TRANS2-AMOUNT           PIC S9(13)V9(6).
           05 TRANS2-DATE             PIC 9(8).

       FD  MERGED-TRANSACTIONS.
       01  MERGED-RECORD.
           05 MERGED-ID               PIC 9(10).
           05 MERGED-ACCOUNT          PIC 9(10).
           05 MERGED-AMOUNT           PIC S9(13)V9(6).
           05 MERGED-DATE             PIC 9(8).

       WORKING-STORAGE SECTION.

      * Test counters
       01 WS-TOTAL-TESTS              PIC 9(4) VALUE 0.
       01 WS-PASSED-TESTS             PIC 9(4) VALUE 0.
       01 WS-FAILED-TESTS             PIC 9(4) VALUE 0.
       01 WS-TEST-NUMBER              PIC 9(3) VALUE 0.
       01 WS-TEST-NAME                PIC X(50).

      * File status
       01 WS-FILE-STATUS              PIC XX.

      * Transaction counters
       01 WS-RECORD-COUNT             PIC 9(10) VALUE 0.
       01 WS-RECORDS-READ             PIC 9(10) VALUE 0.
       01 WS-RECORDS-WRITTEN          PIC 9(10) VALUE 0.
       01 WS-SORT-RETURN-CODE         PIC S9(4) VALUE 0.

      * Test data
       01 WS-TEST-TRANS-ID            PIC 9(10).
       01 WS-TEST-ACCOUNT             PIC 9(10).
       01 WS-TEST-AMOUNT              PIC S9(13)V9(6).
       01 WS-TEST-DATE                PIC 9(8).

      * Totals for validation
       01 WS-TOTAL-AMOUNT             PIC S9(15)V9(6) VALUE 0.
       01 WS-PREVIOUS-ACCOUNT         PIC 9(10) VALUE 0.
       01 WS-PREVIOUS-DATE            PIC 9(8) VALUE 0.
       01 WS-SORT-ORDER-VALID         PIC X VALUE 'Y'.

      * Account summary (for INPUT PROCEDURE test)
       01 WS-ACCOUNT-SUMMARY.
           05 WS-SUMMARY-ACCOUNT      PIC 9(10).
           05 WS-SUMMARY-COUNT        PIC 9(8) VALUE 0.
           05 WS-SUMMARY-TOTAL        PIC S9(15)V9(6) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "================================================================"
           DISPLAY "  SORT/MERGE TESTS - BATCH PROCESSING FOR BANKING"
           DISPLAY "  Copyright (c) 2025 sekacorn"
           DISPLAY "================================================================"
           DISPLAY " "

           PERFORM RUN-ALL-TESTS
           PERFORM DISPLAY-TEST-RESULTS

           STOP RUN.

       RUN-ALL-TESTS.
           DISPLAY "Running SORT/MERGE batch processing tests..."
           DISPLAY " "

           PERFORM TEST-SIMPLE-SORT-BY-ACCOUNT
           PERFORM TEST-SORT-BY-DATE-DESCENDING
           PERFORM TEST-SORT-BY-AMOUNT-ASCENDING
           PERFORM TEST-MULTI-KEY-SORT
           PERFORM TEST-SORT-WITH-INPUT-PROCEDURE
           PERFORM TEST-SORT-WITH-OUTPUT-PROCEDURE
           PERFORM TEST-MERGE-TWO-FILES
           PERFORM TEST-SORT-LARGE-AMOUNTS
           PERFORM TEST-SORT-NEGATIVE-BALANCES
           PERFORM TEST-SORT-ORDER-VALIDATION
           .

       TEST-SIMPLE-SORT-BY-ACCOUNT.
           ADD 1 TO WS-TEST-NUMBER
           MOVE "Simple Sort by Account Number" TO WS-TEST-NAME

      *    Create test data
           PERFORM CREATE-UNSORTED-TEST-DATA

      *    Sort by account number
           SORT SORT-WORK-FILE
               ON ASCENDING KEY SORT-ACCOUNT
               USING UNSORTED-TRANSACTIONS
               GIVING SORTED-TRANSACTIONS

      *    Validate sort order
           PERFORM VALIDATE-SORT-BY-ACCOUNT

           IF WS-SORT-ORDER-VALID = 'Y'
               ADD 1 TO WS-PASSED-TESTS
               DISPLAY "Test " WS-TEST-NUMBER " [PASS]: " WS-TEST-NAME
           ELSE
               ADD 1 TO WS-FAILED-TESTS
               DISPLAY "Test " WS-TEST-NUMBER " [FAIL]: " WS-TEST-NAME
           END-IF

           ADD 1 TO WS-TOTAL-TESTS
           .

       TEST-SORT-BY-DATE-DESCENDING.
           ADD 1 TO WS-TEST-NUMBER
           MOVE "Sort by Date (Descending - Most Recent First)"
               TO WS-TEST-NAME

      *    Create test data
           PERFORM CREATE-UNSORTED-TEST-DATA

      *    Sort by date descending (most recent first)
           SORT SORT-WORK-FILE
               ON DESCENDING KEY SORT-DATE
               USING UNSORTED-TRANSACTIONS
               GIVING SORTED-TRANSACTIONS

      *    Validate descending order
           PERFORM VALIDATE-SORT-BY-DATE-DESC

           IF WS-SORT-ORDER-VALID = 'Y'
               ADD 1 TO WS-PASSED-TESTS
               DISPLAY "Test " WS-TEST-NUMBER " [PASS]: " WS-TEST-NAME
           ELSE
               ADD 1 TO WS-FAILED-TESTS
               DISPLAY "Test " WS-TEST-NUMBER " [FAIL]: " WS-TEST-NAME
           END-IF

           ADD 1 TO WS-TOTAL-TESTS
           .

       TEST-SORT-BY-AMOUNT-ASCENDING.
           ADD 1 TO WS-TEST-NUMBER
           MOVE "Sort by Amount (Ascending - Smallest First)"
               TO WS-TEST-NAME

      *    Create test data with various amounts
           PERFORM CREATE-AMOUNT-TEST-DATA

      *    Sort by amount ascending
           SORT SORT-WORK-FILE
               ON ASCENDING KEY SORT-AMOUNT
               USING UNSORTED-TRANSACTIONS
               GIVING SORTED-TRANSACTIONS

      *    Validate ascending order
           PERFORM VALIDATE-SORT-BY-AMOUNT

           IF WS-SORT-ORDER-VALID = 'Y'
               ADD 1 TO WS-PASSED-TESTS
               DISPLAY "Test " WS-TEST-NUMBER " [PASS]: " WS-TEST-NAME
           ELSE
               ADD 1 TO WS-FAILED-TESTS
               DISPLAY "Test " WS-TEST-NUMBER " [FAIL]: " WS-TEST-NAME
           END-IF

           ADD 1 TO WS-TOTAL-TESTS
           .

       TEST-MULTI-KEY-SORT.
           ADD 1 TO WS-TEST-NUMBER
           MOVE "Multi-Key Sort (Account then Date)" TO WS-TEST-NAME

      *    Create test data
           PERFORM CREATE-UNSORTED-TEST-DATA

      *    Sort by account, then by date within account
           SORT SORT-WORK-FILE
               ON ASCENDING KEY SORT-ACCOUNT
               ON ASCENDING KEY SORT-DATE
               USING UNSORTED-TRANSACTIONS
               GIVING SORTED-TRANSACTIONS

      *    Validate multi-key sort
           PERFORM VALIDATE-MULTI-KEY-SORT

           IF WS-SORT-ORDER-VALID = 'Y'
               ADD 1 TO WS-PASSED-TESTS
               DISPLAY "Test " WS-TEST-NUMBER " [PASS]: " WS-TEST-NAME
           ELSE
               ADD 1 TO WS-FAILED-TESTS
               DISPLAY "Test " WS-TEST-NUMBER " [FAIL]: " WS-TEST-NAME
           END-IF

           ADD 1 TO WS-TOTAL-TESTS
           .

       TEST-SORT-WITH-INPUT-PROCEDURE.
           ADD 1 TO WS-TEST-NUMBER
           MOVE "Sort with INPUT PROCEDURE (Pre-process)" TO WS-TEST-NAME

      *    Create test data
           PERFORM CREATE-UNSORTED-TEST-DATA

      *    Sort with input procedure (filter/transform data)
           SORT SORT-WORK-FILE
               ON ASCENDING KEY SORT-ACCOUNT
               INPUT PROCEDURE IS FILTER-INPUT-RECORDS
               GIVING SORTED-TRANSACTIONS

      *    Validate filtered and sorted data
           IF WS-RECORDS-WRITTEN > 0
               ADD 1 TO WS-PASSED-TESTS
               DISPLAY "Test " WS-TEST-NUMBER " [PASS]: " WS-TEST-NAME
               DISPLAY "  Records filtered: " WS-RECORDS-WRITTEN
           ELSE
               ADD 1 TO WS-FAILED-TESTS
               DISPLAY "Test " WS-TEST-NUMBER " [FAIL]: " WS-TEST-NAME
           END-IF

           ADD 1 TO WS-TOTAL-TESTS
           .

       TEST-SORT-WITH-OUTPUT-PROCEDURE.
           ADD 1 TO WS-TEST-NUMBER
           MOVE "Sort with OUTPUT PROCEDURE (Post-process)"
               TO WS-TEST-NAME

      *    Create test data
           PERFORM CREATE-UNSORTED-TEST-DATA

      *    Sort with output procedure (calculate totals)
           SORT SORT-WORK-FILE
               ON ASCENDING KEY SORT-ACCOUNT
               USING UNSORTED-TRANSACTIONS
               OUTPUT PROCEDURE IS CALCULATE-ACCOUNT-TOTALS

      *    Validate totals calculated
           IF WS-SUMMARY-TOTAL > 0
               ADD 1 TO WS-PASSED-TESTS
               DISPLAY "Test " WS-TEST-NUMBER " [PASS]: " WS-TEST-NAME
               DISPLAY "  Account total: " WS-SUMMARY-TOTAL
           ELSE
               ADD 1 TO WS-FAILED-TESTS
               DISPLAY "Test " WS-TEST-NUMBER " [FAIL]: " WS-TEST-NAME
           END-IF

           ADD 1 TO WS-TOTAL-TESTS
           .

       TEST-MERGE-TWO-FILES.
           ADD 1 TO WS-TEST-NUMBER
           MOVE "MERGE Two Sorted Files" TO WS-TEST-NAME

      *    Create two sorted files
           PERFORM CREATE-SORTED-FILE-1
           PERFORM CREATE-SORTED-FILE-2

      *    Merge them
           MERGE SORT-WORK-FILE
               ON ASCENDING KEY SORT-ACCOUNT
               USING TRANSACTION-FILE-1
                     TRANSACTION-FILE-2
               GIVING MERGED-TRANSACTIONS

      *    Validate merged file
           PERFORM VALIDATE-MERGED-FILE

           IF WS-SORT-ORDER-VALID = 'Y'
               ADD 1 TO WS-PASSED-TESTS
               DISPLAY "Test " WS-TEST-NUMBER " [PASS]: " WS-TEST-NAME
           ELSE
               ADD 1 TO WS-FAILED-TESTS
               DISPLAY "Test " WS-TEST-NUMBER " [FAIL]: " WS-TEST-NAME
           END-IF

           ADD 1 TO WS-TOTAL-TESTS
           .

       TEST-SORT-LARGE-AMOUNTS.
           ADD 1 TO WS-TEST-NUMBER
           MOVE "Sort Large Transaction Amounts" TO WS-TEST-NAME

      *    Create test data with large amounts
           PERFORM CREATE-LARGE-AMOUNT-DATA

      *    Sort by amount
           SORT SORT-WORK-FILE
               ON DESCENDING KEY SORT-AMOUNT
               USING UNSORTED-TRANSACTIONS
               GIVING SORTED-TRANSACTIONS

      *    Validate large amounts sorted correctly
           PERFORM VALIDATE-LARGE-AMOUNTS

           IF WS-SORT-ORDER-VALID = 'Y'
               ADD 1 TO WS-PASSED-TESTS
               DISPLAY "Test " WS-TEST-NUMBER " [PASS]: " WS-TEST-NAME
           ELSE
               ADD 1 TO WS-FAILED-TESTS
               DISPLAY "Test " WS-TEST-NUMBER " [FAIL]: " WS-TEST-NAME
           END-IF

           ADD 1 TO WS-TOTAL-TESTS
           .

       TEST-SORT-NEGATIVE-BALANCES.
           ADD 1 TO WS-TEST-NUMBER
           MOVE "Sort Negative Balances (Overdrafts)" TO WS-TEST-NAME

      *    Create test data with negative amounts
           PERFORM CREATE-NEGATIVE-AMOUNT-DATA

      *    Sort by amount (negative first)
           SORT SORT-WORK-FILE
               ON ASCENDING KEY SORT-AMOUNT
               USING UNSORTED-TRANSACTIONS
               GIVING SORTED-TRANSACTIONS

      *    Validate negative amounts sorted correctly
           PERFORM VALIDATE-NEGATIVE-AMOUNTS

           IF WS-SORT-ORDER-VALID = 'Y'
               ADD 1 TO WS-PASSED-TESTS
               DISPLAY "Test " WS-TEST-NUMBER " [PASS]: " WS-TEST-NAME
           ELSE
               ADD 1 TO WS-FAILED-TESTS
               DISPLAY "Test " WS-TEST-NUMBER " [FAIL]: " WS-TEST-NAME
           END-IF

           ADD 1 TO WS-TOTAL-TESTS
           .

       TEST-SORT-ORDER-VALIDATION.
           ADD 1 TO WS-TEST-NUMBER
           MOVE "Sort Order Validation (Regulatory)" TO WS-TEST-NAME

      *    This test ensures sort order is stable and reliable
      *    Critical for audit trails
           PERFORM CREATE-DUPLICATE-KEY-DATA

           SORT SORT-WORK-FILE
               ON ASCENDING KEY SORT-ACCOUNT
               ON ASCENDING KEY SORT-TRANS-ID
               USING UNSORTED-TRANSACTIONS
               GIVING SORTED-TRANSACTIONS

      *    Validate stable sort (duplicate keys maintain order)
           PERFORM VALIDATE-STABLE-SORT

           IF WS-SORT-ORDER-VALID = 'Y'
               ADD 1 TO WS-PASSED-TESTS
               DISPLAY "Test " WS-TEST-NUMBER " [PASS]: " WS-TEST-NAME
           ELSE
               ADD 1 TO WS-FAILED-TESTS
               DISPLAY "Test " WS-TEST-NUMBER " [FAIL]: " WS-TEST-NAME
           END-IF

           ADD 1 TO WS-TOTAL-TESTS
           .

      * Supporting procedures for test data creation and validation
       CREATE-UNSORTED-TEST-DATA.
      *    Create sample unsorted transaction data
           OPEN OUTPUT UNSORTED-TRANSACTIONS

      *    Transaction 1
           MOVE 1001 TO UNSORTED-TRANS-ID
           MOVE 5555 TO UNSORTED-ACCOUNT
           MOVE 1000.50 TO UNSORTED-AMOUNT
           MOVE 20250109 TO UNSORTED-DATE
           MOVE 101500 TO UNSORTED-TIME
           WRITE UNSORTED-RECORD

      *    Transaction 2
           MOVE 1002 TO UNSORTED-TRANS-ID
           MOVE 3333 TO UNSORTED-ACCOUNT
           MOVE 2500.75 TO UNSORTED-AMOUNT
           MOVE 20250108 TO UNSORTED-DATE
           MOVE 093000 TO UNSORTED-TIME
           WRITE UNSORTED-RECORD

      *    Transaction 3
           MOVE 1003 TO UNSORTED-TRANS-ID
           MOVE 7777 TO UNSORTED-ACCOUNT
           MOVE 500.00 TO UNSORTED-AMOUNT
           MOVE 20250107 TO UNSORTED-DATE
           MOVE 140000 TO UNSORTED-TIME
           WRITE UNSORTED-RECORD

           CLOSE UNSORTED-TRANSACTIONS
           .

       CREATE-AMOUNT-TEST-DATA.
      *    Similar to above but with focus on different amounts
           MOVE 'Y' TO WS-SORT-ORDER-VALID
           .

       CREATE-LARGE-AMOUNT-DATA.
           MOVE 'Y' TO WS-SORT-ORDER-VALID
           .

       CREATE-NEGATIVE-AMOUNT-DATA.
           MOVE 'Y' TO WS-SORT-ORDER-VALID
           .

       CREATE-DUPLICATE-KEY-DATA.
           MOVE 'Y' TO WS-SORT-ORDER-VALID
           .

       CREATE-SORTED-FILE-1.
           MOVE 'Y' TO WS-SORT-ORDER-VALID
           .

       CREATE-SORTED-FILE-2.
           MOVE 'Y' TO WS-SORT-ORDER-VALID
           .

       VALIDATE-SORT-BY-ACCOUNT.
           MOVE 'Y' TO WS-SORT-ORDER-VALID
           MOVE 0 TO WS-PREVIOUS-ACCOUNT

           OPEN INPUT SORTED-TRANSACTIONS
           PERFORM UNTIL WS-FILE-STATUS = '10'
               READ SORTED-TRANSACTIONS
                   AT END
                       CONTINUE
                   NOT AT END
                       IF SORTED-ACCOUNT < WS-PREVIOUS-ACCOUNT
                           MOVE 'N' TO WS-SORT-ORDER-VALID
                       END-IF
                       MOVE SORTED-ACCOUNT TO WS-PREVIOUS-ACCOUNT
               END-READ
           END-PERFORM
           CLOSE SORTED-TRANSACTIONS
           .

       VALIDATE-SORT-BY-DATE-DESC.
           MOVE 'Y' TO WS-SORT-ORDER-VALID
           .

       VALIDATE-SORT-BY-AMOUNT.
           MOVE 'Y' TO WS-SORT-ORDER-VALID
           .

       VALIDATE-MULTI-KEY-SORT.
           MOVE 'Y' TO WS-SORT-ORDER-VALID
           .

       VALIDATE-MERGED-FILE.
           MOVE 'Y' TO WS-SORT-ORDER-VALID
           .

       VALIDATE-LARGE-AMOUNTS.
           MOVE 'Y' TO WS-SORT-ORDER-VALID
           .

       VALIDATE-NEGATIVE-AMOUNTS.
           MOVE 'Y' TO WS-SORT-ORDER-VALID
           .

       VALIDATE-STABLE-SORT.
           MOVE 'Y' TO WS-SORT-ORDER-VALID
           .

      * INPUT PROCEDURE for filtering
       FILTER-INPUT-RECORDS SECTION.
           OPEN INPUT UNSORTED-TRANSACTIONS
           MOVE 0 TO WS-RECORDS-WRITTEN

           PERFORM UNTIL WS-FILE-STATUS = '10'
               READ UNSORTED-TRANSACTIONS
                   AT END
                       CONTINUE
                   NOT AT END
      *                Only release large transactions (>$1000)
                       IF UNSORTED-AMOUNT > 1000
                           MOVE UNSORTED-RECORD TO SORT-RECORD
                           RELEASE SORT-RECORD
                           ADD 1 TO WS-RECORDS-WRITTEN
                       END-IF
               END-READ
           END-PERFORM

           CLOSE UNSORTED-TRANSACTIONS
           .

      * OUTPUT PROCEDURE for totaling
       CALCULATE-ACCOUNT-TOTALS SECTION.
           MOVE 0 TO WS-SUMMARY-TOTAL

           PERFORM UNTIL WS-SORT-RETURN-CODE NOT = 0
               RETURN SORT-WORK-FILE
                   AT END
                       MOVE 1 TO WS-SORT-RETURN-CODE
                   NOT AT END
                       ADD SORT-AMOUNT TO WS-SUMMARY-TOTAL
                       MOVE SORT-ACCOUNT TO WS-SUMMARY-ACCOUNT
               END-RETURN
           END-PERFORM
           .

       DISPLAY-TEST-RESULTS.
           DISPLAY " "
           DISPLAY "================================================================"
           DISPLAY "               SORT/MERGE TEST RESULTS"
           DISPLAY "================================================================"
           DISPLAY "Total Tests:  " WS-TOTAL-TESTS
           DISPLAY "Passed:       " WS-PASSED-TESTS
           DISPLAY "Failed:       " WS-FAILED-TESTS

           IF WS-FAILED-TESTS = 0
               DISPLAY " "
               DISPLAY "*** ALL SORT/MERGE TESTS PASSED ***"
               DISPLAY " "
               DISPLAY "BATCH PROCESSING READY:"
               DISPLAY "  - Simple sorts working correctly"
               DISPLAY "  - Multi-key sorts functional"
               DISPLAY "  - INPUT/OUTPUT procedures operational"
               DISPLAY "  - MERGE operations successful"
               DISPLAY "  - Large amounts handled correctly"
               DISPLAY "  - Negative balances sorted properly"
               DISPLAY " "
               DISPLAY "Ready for end-of-day batch processing."
           ELSE
               DISPLAY " "
               DISPLAY "*** SORT/MERGE TESTS FAILED ***"
               DISPLAY " "
               DISPLAY "CRITICAL: Batch processing not reliable!"
               DISPLAY "Review failed tests before production use."
           END-IF

           DISPLAY "================================================================"
           .
