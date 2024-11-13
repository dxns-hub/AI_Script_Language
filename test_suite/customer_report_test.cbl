
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTOMER-REPORT-TEST.
       
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
       01 TEST-CASES.
           05 TEST-CUSTOMER-ID       PIC 9(5) VALUE 12345.
           05 TEST-CUSTOMER-NAME     PIC X(30) VALUE 'John Doe'.
           05 TEST-CUSTOMER-BALANCE  PIC 9(7)V99 VALUE 1234.56.
       
       PROCEDURE DIVISION.
           DISPLAY 'Running COBOL Tests...'
           
           PERFORM TEST-CUSTOMER-RECORD
           
           STOP RUN.
           
       TEST-CUSTOMER-RECORD.
           DISPLAY 'Testing Customer Record Creation...'
           MOVE TEST-CUSTOMER-ID TO CUSTOMER-ID
           MOVE TEST-CUSTOMER-NAME TO CUSTOMER-NAME
           MOVE TEST-CUSTOMER-BALANCE TO CUSTOMER-BALANCE
           
           IF CUSTOMER-ID = TEST-CUSTOMER-ID
               DISPLAY 'Customer ID Test: PASSED'
           ELSE
               DISPLAY 'Customer ID Test: FAILED'
           END-IF
           
           IF CUSTOMER-NAME = TEST-CUSTOMER-NAME
               DISPLAY 'Customer Name Test: PASSED'
           ELSE
               DISPLAY 'Customer Name Test: FAILED'
           END-IF
           
           IF CUSTOMER-BALANCE = TEST-CUSTOMER-BALANCE
               DISPLAY 'Customer Balance Test: PASSED'
           ELSE
               DISPLAY 'Customer Balance Test: FAILED'
           END-IF.
