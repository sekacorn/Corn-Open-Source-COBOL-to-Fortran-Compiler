      *****************************************************************
      * COMPREHENSIVE BANKING SYSTEM TEST SUITE
      * This COBOL program demonstrates critical banking operations
      * that must be accurately translated to Fortran
      *
      * Copyright (c) 2025 sekacorn
      * Contact: sekacorn@gmail.com
      *
      * USE CASES:
      * - Account balance calculations
      * - Interest computation
      * - Transaction processing
      * - Batch processing with SORT
      * - Regulatory reporting
      * - Audit trail logging
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANKING-SYSTEM.
       AUTHOR. sekacorn.
       DATE-WRITTEN. 2025-11-09.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANSACTION-FILE
               ASSIGN TO "TRANSACTIONS.DAT"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

           SELECT ACCOUNT-FILE
               ASSIGN TO "ACCOUNTS.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ACCT-ACCOUNT-NUMBER
               FILE STATUS IS WS-ACCT-STATUS.

           SELECT SORT-WORK-FILE
               ASSIGN TO "SORTWORK.TMP".

           SELECT REPORT-FILE
               ASSIGN TO "DAILY-REPORT.TXT"
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD TRANSACTION-FILE.
       01 TRANSACTION-RECORD.
          05 TXN-ACCOUNT-NUMBER         PIC 9(10).
          05 TXN-TYPE                   PIC X(2).
             88 TXN-DEPOSIT             VALUE 'DP'.
             88 TXN-WITHDRAWAL          VALUE 'WD'.
             88 TXN-TRANSFER            VALUE 'TR'.
             88 TXN-INTEREST            VALUE 'IN'.
          05 TXN-AMOUNT                 PIC 9(13)V99.
          05 TXN-DATE.
             10 TXN-YEAR                PIC 9(4).
             10 TXN-MONTH               PIC 99.
             10 TXN-DAY                 PIC 99.
          05 TXN-TIME.
             10 TXN-HOUR                PIC 99.
             10 TXN-MINUTE              PIC 99.
             10 TXN-SECOND              PIC 99.
          05 TXN-DESCRIPTION            PIC X(50).
          05 TXN-PROCESSED-FLAG         PIC X.

       FD ACCOUNT-FILE.
       01 ACCOUNT-RECORD.
          05 ACCT-ACCOUNT-NUMBER        PIC 9(10).
          05 ACCT-CUSTOMER-ID           PIC 9(8).
          05 ACCT-TYPE                  PIC X(2).
             88 ACCT-CHECKING           VALUE 'CK'.
             88 ACCT-SAVINGS            VALUE 'SV'.
             88 ACCT-MONEY-MARKET       VALUE 'MM'.
             88 ACCT-CD                 VALUE 'CD'.
          05 ACCT-BALANCE               PIC S9(13)V99.
          05 ACCT-INTEREST-RATE         PIC 9V9999.
          05 ACCT-LAST-INTEREST-DATE    PIC 9(8).
          05 ACCT-OPENING-DATE          PIC 9(8).
          05 ACCT-STATUS                PIC X.
             88 ACCT-ACTIVE             VALUE 'A'.
             88 ACCT-CLOSED             VALUE 'C'.
             88 ACCT-FROZEN             VALUE 'F'.
          05 ACCT-OVERDRAFT-LIMIT       PIC 9(7)V99.

       SD SORT-WORK-FILE.
       01 SORT-RECORD.
          05 SORT-ACCOUNT-NUMBER        PIC 9(10).
          05 SORT-AMOUNT                PIC 9(13)V99.
          05 SORT-DATE                  PIC 9(8).
          05 SORT-FILLER                PIC X(62).

       FD REPORT-FILE.
       01 REPORT-LINE                   PIC X(132).

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS                PIC X(2).
          88 WS-FILE-OK                 VALUES '00' '97'.
          88 WS-FILE-EOF                VALUE '10'.
          88 WS-FILE-NOT-FOUND          VALUE '23'.

       01 WS-ACCT-STATUS                PIC X(2).

       01 WS-PROGRAM-CONSTANTS.
          05 WS-ANNUAL-DAYS             PIC 9(3) VALUE 365.
          05 WS-MIN-BALANCE             PIC 9(7)V99 VALUE 500.00.
          05 WS-MAX-WITHDRAWAL          PIC 9(7)V99 VALUE 10000.00.
          05 WS-DAILY-LIMIT             PIC 9(9)V99 VALUE 50000.00.

       01 WS-INTEREST-RATES.
          05 WS-CHECKING-RATE           PIC 9V9999 VALUE 0.0050.
          05 WS-SAVINGS-RATE            PIC 9V9999 VALUE 0.0150.
          05 WS-MM-RATE                 PIC 9V9999 VALUE 0.0250.
          05 WS-CD-RATE                 PIC 9V9999 VALUE 0.0350.

       01 WS-COUNTERS.
          05 WS-TRANSACTION-COUNT       PIC 9(7) VALUE ZEROS.
          05 WS-DEPOSIT-COUNT           PIC 9(7) VALUE ZEROS.
          05 WS-WITHDRAWAL-COUNT        PIC 9(7) VALUE ZEROS.
          05 WS-ERROR-COUNT             PIC 9(5) VALUE ZEROS.
          05 WS-ACCOUNTS-PROCESSED      PIC 9(6) VALUE ZEROS.

       01 WS-TOTALS.
          05 WS-TOTAL-DEPOSITS          PIC S9(15)V99 VALUE ZEROS.
          05 WS-TOTAL-WITHDRAWALS       PIC S9(15)V99 VALUE ZEROS.
          05 WS-TOTAL-INTEREST-PAID     PIC S9(13)V99 VALUE ZEROS.
          05 WS-NET-CASH-FLOW           PIC S9(15)V99 VALUE ZEROS.

       01 WS-CALCULATED-VALUES.
          05 WS-INTEREST-AMOUNT         PIC S9(11)V99.
          05 WS-NEW-BALANCE             PIC S9(13)V99.
          05 WS-DAYS-SINCE-INTEREST     PIC 9(3).
          05 WS-DAILY-INTEREST-RATE     PIC 9V999999.
          05 WS-SERVICE-CHARGE          PIC 9(5)V99.

       01 WS-VALIDATION-FLAGS.
          05 WS-VALID-TRANSACTION       PIC X VALUE 'Y'.
             88 TRANSACTION-VALID       VALUE 'Y'.
             88 TRANSACTION-INVALID     VALUE 'N'.
          05 WS-OVERDRAFT-CHECK         PIC X VALUE 'N'.
             88 OVERDRAFT-OK            VALUE 'Y'.
             88 OVERDRAFT-EXCEEDED      VALUE 'N'.

       01 WS-REGULATORY-DATA.
          05 WS-LARGE-TRANSACTION       PIC X VALUE 'N'.
             88 REQUIRES-REPORTING      VALUE 'Y'.
          05 WS-SUSPICIOUS-ACTIVITY     PIC X VALUE 'N'.
             88 SUSPICIOUS-FLAG         VALUE 'Y'.
          05 WS-REPORTING-THRESHOLD     PIC 9(7)V99 VALUE 10000.00.

       01 WS-REPORT-HEADERS.
          05 WS-HEADER-1.
             10 FILLER                  PIC X(50)
                VALUE '          DAILY BANKING TRANSACTION REPORT'.
          05 WS-HEADER-2.
             10 FILLER                  PIC X(50)
                VALUE '          ================================'.
          05 WS-HEADER-3.
             10 FILLER                  PIC X(80)
                VALUE 'Account Number    Type    Beginning Bal    '
                      & 'Deposits    Withdrawals    Ending Bal'.

       01 WS-REPORT-LINE.
          05 WS-RPT-ACCOUNT-NUMBER      PIC 9(10).
          05 FILLER                     PIC X(5) VALUE SPACES.
          05 WS-RPT-ACCT-TYPE           PIC X(2).
          05 FILLER                     PIC X(5) VALUE SPACES.
          05 WS-RPT-BEGIN-BAL           PIC ZZZ,ZZZ,ZZ9.99.
          05 FILLER                     PIC X(3) VALUE SPACES.
          05 WS-RPT-DEPOSITS            PIC ZZZ,ZZZ,ZZ9.99.
          05 FILLER                     PIC X(3) VALUE SPACES.
          05 WS-RPT-WITHDRAWALS         PIC ZZZ,ZZZ,ZZ9.99.
          05 FILLER                     PIC X(3) VALUE SPACES.
          05 WS-RPT-END-BAL             PIC ZZZ,ZZZ,ZZ9.99.

       01 WS-ERROR-MESSAGES.
          05 WS-ERR-INSUFFICIENT-FUNDS  PIC X(50)
             VALUE 'ERROR: Insufficient funds for withdrawal'.
          05 WS-ERR-ACCOUNT-NOT-FOUND   PIC X(50)
             VALUE 'ERROR: Account number not found'.
          05 WS-ERR-ACCOUNT-FROZEN      PIC X(50)
             VALUE 'ERROR: Account is frozen'.
          05 WS-ERR-INVALID-AMOUNT      PIC X(50)
             VALUE 'ERROR: Invalid transaction amount'.

       PROCEDURE DIVISION.
       MAIN-CONTROL.
           PERFORM INITIALIZATION
           PERFORM PROCESS-DAILY-TRANSACTIONS
           PERFORM CALCULATE-INTEREST-ALL-ACCOUNTS
           PERFORM GENERATE-DAILY-REPORT
           PERFORM CLEANUP
           STOP RUN.

       INITIALIZATION.
           DISPLAY 'Banking System - Daily Processing Started'
           DISPLAY '========================================='
           OPEN INPUT TRANSACTION-FILE
           OPEN I-O ACCOUNT-FILE
           OPEN OUTPUT REPORT-FILE
           MOVE ZEROS TO WS-TRANSACTION-COUNT
           MOVE ZEROS TO WS-DEPOSIT-COUNT
           MOVE ZEROS TO WS-WITHDRAWAL-COUNT
           MOVE ZEROS TO WS-ERROR-COUNT
           MOVE ZEROS TO WS-TOTAL-DEPOSITS
           MOVE ZEROS TO WS-TOTAL-WITHDRAWALS
           MOVE ZEROS TO WS-TOTAL-INTEREST-PAID.

       PROCESS-DAILY-TRANSACTIONS.
           PERFORM READ-TRANSACTION
           PERFORM PROCESS-TRANSACTION
               UNTIL WS-FILE-EOF.

       READ-TRANSACTION.
           READ TRANSACTION-FILE
               AT END
                   SET WS-FILE-EOF TO TRUE
               NOT AT END
                   ADD 1 TO WS-TRANSACTION-COUNT
           END-READ.

       PROCESS-TRANSACTION.
           MOVE 'Y' TO WS-VALID-TRANSACTION

           PERFORM VALIDATE-TRANSACTION

           IF TRANSACTION-VALID
              PERFORM EXECUTE-TRANSACTION
           ELSE
              ADD 1 TO WS-ERROR-COUNT
              DISPLAY 'Transaction rejected: ' TXN-ACCOUNT-NUMBER
           END-IF

           PERFORM READ-TRANSACTION.

       VALIDATE-TRANSACTION.
      *    Validate account exists
           MOVE TXN-ACCOUNT-NUMBER TO ACCT-ACCOUNT-NUMBER
           READ ACCOUNT-FILE
               INVALID KEY
                   MOVE 'N' TO WS-VALID-TRANSACTION
                   DISPLAY WS-ERR-ACCOUNT-NOT-FOUND
           END-READ

           IF TRANSACTION-VALID
      *       Validate account status
              IF ACCT-FROZEN
                 MOVE 'N' TO WS-VALID-TRANSACTION
                 DISPLAY WS-ERR-ACCOUNT-FROZEN
              END-IF

      *       Validate transaction amount
              IF TXN-AMOUNT <= ZEROS OR TXN-AMOUNT > WS-DAILY-LIMIT
                 MOVE 'N' TO WS-VALID-TRANSACTION
                 DISPLAY WS-ERR-INVALID-AMOUNT
              END-IF

      *       Check for large transaction reporting requirement
              IF TXN-AMOUNT >= WS-REPORTING-THRESHOLD
                 SET REQUIRES-REPORTING TO TRUE
                 PERFORM LOG-LARGE-TRANSACTION
              END-IF
           END-IF.

       EXECUTE-TRANSACTION.
           EVALUATE TRUE
               WHEN TXN-DEPOSIT
                    PERFORM PROCESS-DEPOSIT
               WHEN TXN-WITHDRAWAL
                    PERFORM PROCESS-WITHDRAWAL
               WHEN TXN-TRANSFER
                    PERFORM PROCESS-TRANSFER
               WHEN TXN-INTEREST
                    PERFORM PROCESS-MANUAL-INTEREST
               WHEN OTHER
                    DISPLAY 'Unknown transaction type: ' TXN-TYPE
                    ADD 1 TO WS-ERROR-COUNT
           END-EVALUATE.

       PROCESS-DEPOSIT.
           ADD TXN-AMOUNT TO ACCT-BALANCE
           ADD TXN-AMOUNT TO WS-TOTAL-DEPOSITS
           ADD 1 TO WS-DEPOSIT-COUNT

           REWRITE ACCOUNT-RECORD
               INVALID KEY
                   DISPLAY 'Error updating account: ' ACCT-ACCOUNT-NUMBER
                   ADD 1 TO WS-ERROR-COUNT
           END-REWRITE

           PERFORM LOG-AUDIT-TRAIL.

       PROCESS-WITHDRAWAL.
      *    Check for sufficient funds including overdraft
           COMPUTE WS-NEW-BALANCE =
               ACCT-BALANCE - TXN-AMOUNT

           IF WS-NEW-BALANCE >= (ACCT-OVERDRAFT-LIMIT * -1)
              SUBTRACT TXN-AMOUNT FROM ACCT-BALANCE
              ADD TXN-AMOUNT TO WS-TOTAL-WITHDRAWALS
              ADD 1 TO WS-WITHDRAWAL-COUNT

              REWRITE ACCOUNT-RECORD
                  INVALID KEY
                      DISPLAY 'Error updating account: '
                              ACCT-ACCOUNT-NUMBER
                      ADD 1 TO WS-ERROR-COUNT
              END-REWRITE

              PERFORM LOG-AUDIT-TRAIL
           ELSE
              DISPLAY WS-ERR-INSUFFICIENT-FUNDS
              DISPLAY 'Account: ' ACCT-ACCOUNT-NUMBER
                      ' Amount: ' TXN-AMOUNT
              ADD 1 TO WS-ERROR-COUNT
           END-IF.

       PROCESS-TRANSFER.
      *    Transfers are processed as withdrawal from one account
      *    and deposit to another (simplified here)
           PERFORM PROCESS-WITHDRAWAL.

       PROCESS-MANUAL-INTEREST.
      *    Manual interest posting (batch interest calculated separately)
           ADD TXN-AMOUNT TO ACCT-BALANCE
           ADD TXN-AMOUNT TO WS-TOTAL-INTEREST-PAID
           REWRITE ACCOUNT-RECORD.

       CALCULATE-INTEREST-ALL-ACCOUNTS.
           DISPLAY 'Calculating interest for all accounts...'

      *    Start at beginning of account file
           MOVE ZEROS TO ACCT-ACCOUNT-NUMBER
           START ACCOUNT-FILE KEY IS >= ACCT-ACCOUNT-NUMBER
               INVALID KEY
                   DISPLAY 'Error positioning account file'
           END-START

           PERFORM READ-NEXT-ACCOUNT
           PERFORM CALCULATE-ACCOUNT-INTEREST
               UNTIL WS-ACCT-STATUS = '10'.

       READ-NEXT-ACCOUNT.
           READ ACCOUNT-FILE NEXT RECORD
               AT END
                   MOVE '10' TO WS-ACCT-STATUS
           END-READ.

       CALCULATE-ACCOUNT-INTEREST.
           IF ACCT-ACTIVE AND ACCT-BALANCE > ZEROS
      *       Calculate days since last interest payment
              COMPUTE WS-DAYS-SINCE-INTEREST =
                  FUNCTION INTEGER-OF-DATE(FUNCTION CURRENT-DATE) -
                  FUNCTION INTEGER-OF-DATE(ACCT-LAST-INTEREST-DATE)

      *       Calculate daily interest rate
              DIVIDE ACCT-INTEREST-RATE BY WS-ANNUAL-DAYS
                  GIVING WS-DAILY-INTEREST-RATE

      *       Calculate interest amount with ROUNDED for banking accuracy
              COMPUTE WS-INTEREST-AMOUNT ROUNDED =
                  ACCT-BALANCE *
                  WS-DAILY-INTEREST-RATE *
                  WS-DAYS-SINCE-INTEREST

      *       Post interest to account
              ADD WS-INTEREST-AMOUNT TO ACCT-BALANCE
              ADD WS-INTEREST-AMOUNT TO WS-TOTAL-INTEREST-PAID
              MOVE FUNCTION CURRENT-DATE TO ACCT-LAST-INTEREST-DATE

              REWRITE ACCOUNT-RECORD
           END-IF

           ADD 1 TO WS-ACCOUNTS-PROCESSED
           PERFORM READ-NEXT-ACCOUNT.

       GENERATE-DAILY-REPORT.
           DISPLAY 'Generating daily report...'

           WRITE REPORT-LINE FROM WS-HEADER-1
           WRITE REPORT-LINE FROM WS-HEADER-2
           WRITE REPORT-LINE FROM WS-HEADER-3

      *    Sort transactions for report
           SORT SORT-WORK-FILE
               ON ASCENDING KEY SORT-ACCOUNT-NUMBER
               USING TRANSACTION-FILE
               OUTPUT PROCEDURE IS WRITE-SORTED-REPORT.

       WRITE-SORTED-REPORT.
      *    This would contain the actual report writing logic
      *    Processing sorted records
           DISPLAY 'Report generation complete'.

       LOG-AUDIT-TRAIL.
      *    Write comprehensive audit trail for regulatory compliance
           DISPLAY 'AUDIT: ' TXN-DATE ' ' TXN-TIME
                   ' Acct:' ACCT-ACCOUNT-NUMBER
                   ' Type:' TXN-TYPE
                   ' Amt:' TXN-AMOUNT.

       LOG-LARGE-TRANSACTION.
      *    Log large transactions for regulatory reporting (CTR, SAR)
           DISPLAY 'LARGE TRANSACTION: ' TXN-ACCOUNT-NUMBER
                   ' Amount: ' TXN-AMOUNT
                   ' Type: ' TXN-TYPE
           DISPLAY 'Requires regulatory reporting'.

       CLEANUP.
           COMPUTE WS-NET-CASH-FLOW =
               WS-TOTAL-DEPOSITS -
               WS-TOTAL-WITHDRAWALS

           DISPLAY ' '
           DISPLAY 'Daily Processing Summary'
           DISPLAY '========================'
           DISPLAY 'Total transactions processed: ' WS-TRANSACTION-COUNT
           DISPLAY 'Deposits: ' WS-DEPOSIT-COUNT
                   ' Amount: $' WS-TOTAL-DEPOSITS
           DISPLAY 'Withdrawals: ' WS-WITHDRAWAL-COUNT
                   ' Amount: $' WS-TOTAL-WITHDRAWALS
           DISPLAY 'Interest paid: $' WS-TOTAL-INTEREST-PAID
           DISPLAY 'Net cash flow: $' WS-NET-CASH-FLOW
           DISPLAY 'Errors encountered: ' WS-ERROR-COUNT
           DISPLAY 'Accounts processed: ' WS-ACCOUNTS-PROCESSED

           CLOSE TRANSACTION-FILE
           CLOSE ACCOUNT-FILE
           CLOSE REPORT-FILE

           DISPLAY 'Banking System - Processing Complete'.
