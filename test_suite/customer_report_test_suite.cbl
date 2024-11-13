
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTOMER-REPORT-TEST-SUITE.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO 'test-customer.dat'
           ORGANIZATION IS LINE SEQUENTIAL.
           
       DATA DIVISION.
       FILE SECTION.
       FD CUSTOMER-FILE.
       01 CUSTOMER-RECORD.
           05 CUSTOMER-ID       PIC 9(5).
           05 CUSTOMER-NAME     PIC X(30).
           05 CUSTOMER-BALANCE  PIC 9(7)V99.
           
       WORKING-STORAGE SECTION.
       01 TEST-SUITE-STATUS.
           05 TESTS-RUN        PIC 99 VALUE ZERO.
           05 TESTS-PASSED     PIC 99 VALUE ZERO.
           05 TESTS-FAILED     PIC 99 VALUE ZERO.
       
       01 TEST-CASES.
           05 TEST-CASE-1.
               10 TC1-ID           PIC 9(5) VALUE 12345.
               10 TC1-NAME         PIC X(30) VALUE 'John Doe'.
               10 TC1-BALANCE      PIC 9(7)V99 VALUE 1234.56.
           05 TEST-CASE-2.
               10 TC2-ID           PIC 9(5) VALUE 99999.
               10 TC2-NAME         PIC X(30) VALUE 'Maximum ID Test'.
               10 TC2-BALANCE      PIC 9(7)V99 VALUE 9999999.99.
           05 TEST-CASE-3.
               10 TC3-ID           PIC 9(5) VALUE 00001.
               10 TC3-NAME         PIC X(30) VALUE 'Minimum ID Test'.
               10 TC3-BALANCE      PIC 9(7)V99 VALUE 0000000.01.
       
       PROCEDURE DIVISION.
           DISPLAY 'Starting Comprehensive COBOL Test Suite...'
           DISPLAY '========================================'
           
           PERFORM TEST-CUSTOMER-RECORD-NORMAL
           PERFORM TEST-CUSTOMER-RECORD-BOUNDARY
           PERFORM TEST-CUSTOMER-RECORD-EDGE
           
           DISPLAY '========================================'
           DISPLAY 'Test Suite Summary:'
           DISPLAY 'Tests Run:    ' TESTS-RUN
           DISPLAY 'Tests Passed: ' TESTS-PASSED
           DISPLAY 'Tests Failed: ' TESTS-FAILED
           
           STOP RUN.
           
       TEST-CUSTOMER-RECORD-NORMAL.
           DISPLAY 'Test Case 1: Normal Customer Record'
           ADD 1 TO TESTS-RUN
           MOVE TC1-ID TO CUSTOMER-ID
           MOVE TC1-NAME TO CUSTOMER-NAME
           MOVE TC1-BALANCE TO CUSTOMER-BALANCE
           
           IF CUSTOMER-ID = TC1-ID AND
              CUSTOMER-NAME = TC1-NAME AND
              CUSTOMER-BALANCE = TC1-BALANCE
               ADD 1 TO TESTS-PASSED
               DISPLAY '  Normal Case: PASSED'
           ELSE
               ADD 1 TO TESTS-FAILED
               DISPLAY '  Normal Case: FAILED'
           END-IF.
           
       TEST-CUSTOMER-RECORD-BOUNDARY.
           DISPLAY 'Test Case 2: Maximum Values'
           ADD 1 TO TESTS-RUN
           MOVE TC2-ID TO CUSTOMER-ID
           MOVE TC2-NAME TO CUSTOMER-NAME
           MOVE TC2-BALANCE TO CUSTOMER-BALANCE
           
           IF CUSTOMER-ID = TC2-ID AND
              CUSTOMER-BALANCE = TC2-BALANCE
               ADD 1 TO TESTS-PASSED
               DISPLAY '  Maximum Values: PASSED'
           ELSE
               ADD 1 TO TESTS-FAILED
               DISPLAY '  Maximum Values: FAILED'
           END-IF.
           
       TEST-CUSTOMER-RECORD-EDGE.
           DISPLAY 'Test Case 3: Minimum Values'
           ADD 1 TO TESTS-RUN
           MOVE TC3-ID TO CUSTOMER-ID
           MOVE TC3-NAME TO CUSTOMER-NAME
           MOVE TC3-BALANCE TO CUSTOMER-BALANCE
           
           IF CUSTOMER-ID = TC3-ID AND
              CUSTOMER-BALANCE = TC3-BALANCE
               ADD 1 TO TESTS-PASSED
               DISPLAY '  Minimum Values: PASSED'
           ELSE
               ADD 1 TO TESTS-FAILED
               DISPLAY '  Minimum Values: FAILED'
           END-IF.
