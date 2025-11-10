      *****************************************************************
      * COMPREHENSIVE COBOL TEST PROGRAM
      * This program demonstrates various COBOL features that should
      * be translated to Fortran
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROLL-SYSTEM.
       AUTHOR. MIGRATION-TEAM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE
               ASSIGN TO "EMPLOYEES.DAT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

           SELECT PAYROLL-FILE
               ASSIGN TO "PAYROLL.DAT"
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD EMPLOYEE-FILE.
       01 EMPLOYEE-RECORD.
          05 EMP-ID                PIC 9(6).
          05 EMP-NAME              PIC X(30).
          05 EMP-SALARY            PIC 9(7)V99.
          05 EMP-DEPT              PIC X(10).
          05 EMP-HIRE-DATE.
             10 HIRE-YEAR          PIC 9999.
             10 HIRE-MONTH         PIC 99.
             10 HIRE-DAY           PIC 99.

       FD PAYROLL-FILE.
       01 PAYROLL-RECORD           PIC X(100).

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS           PIC X(2).
       01 WS-EOF-FLAG              PIC X VALUE 'N'.
          88 END-OF-FILE           VALUE 'Y'.

       01 WS-COUNTERS.
          05 WS-EMPLOYEE-COUNT     PIC 9(5) VALUE ZEROS.
          05 WS-HIGH-EARNER-COUNT  PIC 9(5) VALUE ZEROS.
          05 WS-TOTAL-SALARY       PIC 9(9)V99 VALUE ZEROS.

       01 WS-CALCULATIONS.
          05 WS-BONUS              PIC 9(7)V99.
          05 WS-TAX                PIC 9(7)V99.
          05 WS-NET-PAY            PIC 9(7)V99.

       01 WS-CONSTANTS.
          05 WS-TAX-RATE           PIC V999 VALUE 0.25.
          05 WS-BONUS-RATE         PIC V99  VALUE 0.10.
          05 WS-HIGH-SALARY        PIC 9(7)V99 VALUE 75000.00.

       01 WS-MESSAGES.
          05 WS-START-MSG          PIC X(40)
             VALUE 'Starting Payroll Processing...'.
          05 WS-END-MSG            PIC X(40)
             VALUE 'Payroll Processing Complete.'.

       01 WS-EMPLOYEE-TABLE.
          05 WS-EMP-ENTRY OCCURS 100 TIMES.
             10 WS-TAB-ID          PIC 9(6).
             10 WS-TAB-NAME        PIC X(30).
             10 WS-TAB-SALARY      PIC 9(7)V99.

       PROCEDURE DIVISION.
       MAIN-PROCESS.
           PERFORM INITIALIZATION
           PERFORM PROCESS-EMPLOYEES
           PERFORM GENERATE-REPORTS
           PERFORM CLEANUP
           STOP RUN.

       INITIALIZATION.
           DISPLAY WS-START-MSG
           OPEN INPUT EMPLOYEE-FILE
           OPEN OUTPUT PAYROLL-FILE
           MOVE ZEROS TO WS-EMPLOYEE-COUNT
           MOVE ZEROS TO WS-HIGH-EARNER-COUNT
           MOVE ZEROS TO WS-TOTAL-SALARY.

       PROCESS-EMPLOYEES.
           PERFORM READ-EMPLOYEE
           PERFORM PROCESS-EMPLOYEE-RECORD
               UNTIL END-OF-FILE.

       READ-EMPLOYEE.
           READ EMPLOYEE-FILE
               AT END
                   MOVE 'Y' TO WS-EOF-FLAG
           END-READ.

       PROCESS-EMPLOYEE-RECORD.
           ADD 1 TO WS-EMPLOYEE-COUNT
           ADD EMP-SALARY TO WS-TOTAL-SALARY

           IF EMP-SALARY > WS-HIGH-SALARY
              THEN
                  ADD 1 TO WS-HIGH-EARNER-COUNT
                  PERFORM CALCULATE-HIGH-BONUS
              ELSE
                  PERFORM CALCULATE-STANDARD-PAY
           END-IF

           PERFORM WRITE-PAYROLL-RECORD
           PERFORM READ-EMPLOYEE.

       CALCULATE-HIGH-BONUS.
           COMPUTE WS-BONUS = EMP-SALARY * WS-BONUS-RATE
           COMPUTE WS-TAX = EMP-SALARY * WS-TAX-RATE
           COMPUTE WS-NET-PAY = EMP-SALARY + WS-BONUS - WS-TAX.

       CALCULATE-STANDARD-PAY.
           MOVE ZEROS TO WS-BONUS
           COMPUTE WS-TAX = EMP-SALARY * WS-TAX-RATE
           COMPUTE WS-NET-PAY = EMP-SALARY - WS-TAX.

       WRITE-PAYROLL-RECORD.
           STRING EMP-ID DELIMITED BY SIZE
                  EMP-NAME DELIMITED BY SIZE
                  WS-NET-PAY DELIMITED BY SIZE
                  INTO PAYROLL-RECORD
           END-STRING
           WRITE PAYROLL-RECORD.

       GENERATE-REPORTS.
           DISPLAY 'Total Employees Processed: ' WS-EMPLOYEE-COUNT
           DISPLAY 'High Earners Count: ' WS-HIGH-EARNER-COUNT
           DISPLAY 'Total Salary Amount: ' WS-TOTAL-SALARY.

       CLEANUP.
           CLOSE EMPLOYEE-FILE
           CLOSE PAYROLL-FILE
           DISPLAY WS-END-MSG.

      *****************************************************************
      * Additional Test Cases for Different COBOL Features
      *****************************************************************

       TEST-ARITHMETIC.
           ADD 10 TO WS-EMPLOYEE-COUNT
           ADD 100 200 GIVING WS-TOTAL-SALARY
           SUBTRACT 50 FROM WS-EMPLOYEE-COUNT
           SUBTRACT 25 FROM 100 GIVING WS-BONUS
           MULTIPLY EMP-SALARY BY 2 GIVING WS-NET-PAY
           DIVIDE EMP-SALARY BY 12 GIVING WS-BONUS
           DIVIDE 1000 BY 3 GIVING WS-NET-PAY REMAINDER WS-TAX
           COMPUTE WS-NET-PAY = (EMP-SALARY * 1.05) - WS-TAX.

       TEST-CONDITIONALS.
           EVALUATE EMP-DEPT
               WHEN 'SALES'
                   PERFORM SALES-PROCESSING
               WHEN 'IT'
                   PERFORM IT-PROCESSING
               WHEN 'HR'
                   PERFORM HR-PROCESSING
               WHEN OTHER
                   PERFORM DEFAULT-PROCESSING
           END-EVALUATE.

       TEST-LOOPS.
           PERFORM VARYING WS-EMPLOYEE-COUNT FROM 1 BY 1
               UNTIL WS-EMPLOYEE-COUNT > 100
                   DISPLAY 'Processing employee: ' WS-EMPLOYEE-COUNT
           END-PERFORM.

       SALES-PROCESSING.
           DISPLAY 'Processing Sales Department'.

       IT-PROCESSING.
           DISPLAY 'Processing IT Department'.

       HR-PROCESSING.
           DISPLAY 'Processing HR Department'.

       DEFAULT-PROCESSING.
           DISPLAY 'Processing Other Department'.
