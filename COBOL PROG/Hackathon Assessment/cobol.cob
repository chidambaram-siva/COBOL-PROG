       IDENTIFICATION DIVISION.
       PROGRAM-ID. VSAMFILE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT1 ASSIGN TO ACCOUNTF
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS ACCT-NO
           FILE STATUS IS WS-A-FS.

           SELECT TRANSACCT ASSIGN TO TRANSACC
           ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD ACCOUNT1.
       01 ACCT-REC.
          05 ACCT-NO PIC X(10).
          05 CUST-NAME PIC X(30).
          05 ACCT-TYPE PIC X(1).
          05 BALANCE PIC 9(7)V99.
          05 ACCT-STATUS PIC X(1).
       FD TRANSACCT.
       01 TRAN-REC.
          05 TRANSNO PIC X(10).
          05 TR-AMOUNT PIC 9(7)V99.
       WORKING-STORAGE SECTION.
       01 WS-A-FS PIC XX.
       01 WS-T-FS PIC XX.
       01 TRANS PIC 9(5) VALUE 0.
       01 SUCCESS PIC 9(5) VALUE 0.
       01 FAILED PIC 9(5) VALUE 0.
       01 TOTAL PIC 9(9)V99 VALUE 0.
       01 MAXWITHDRAW PIC 9(7)V99 VALUE 20000.
       01 MINBAL PIC 9(7)V99 VALUE 1000.
       01 NEWBAL PIC 9(7)99.

       PROCEDURE DIVISION.
           OPEN I-O ACCOUNT1
                INPUT TRANSACCT.
           IF WS-A-FS NOT = "00"
               DISPLAY "ERROR IN VSAM FILE OPEN"
               STOP RUN
           END-IF.
           PERFORM UNTIL WS-T-FS = "10"
               READ TRANSACCT
                   AT END
                       MOVE "10" TO WS-T-FS
                   NOT AT END
                       PERFORM ACCOUNT-PROCESS
               END-READ
           END-PERFORM.
           PERFORM DISPLAY-SUMMARY.
           CLOSE ACCOUNT1 TRANSACCT.
           STOP RUN.
       ACCOUNT-PROCESS.
           ADD 1 TO TRANS
           MOVE TRANSNO TO ACCT-NO
           READ ACCOUNT1 KEY IS ACCT-NO
               INVALID KEY
                   DISPLAY "ACCOUNT NOT FOUND:" TRANSNO
                   ADD 1 TO FAILED
                   EXIT PARAGRAPH
           END-READ
           IF ACCT-STATUS = "I"
               DISPLAY "INACTIVE ACCOUNT      :" TRANSNO
               ADD 1 TO FAILED
               EXIT PARAGRAPH
           END-IF
           IF TR-AMOUNT > MAXWITHDRAW
               DISPLAY "LIMIT EXCEED          :" TRANSNO
               ADD 1 TO FAILED
               EXIT PARAGRAPH
           END-IF
           IF BALANCE - TR-AMOUNT > MINBAL
               DISPLAY "CHECK THE BALANCE     :" TRANSNO
               ADD 1 TO FAILED
               EXIT PARAGRAPH
           END-IF
           MOVE NEWBAL TO BALANCE
           REWRITE ACCT-REC
               INVALID KEY
                   DISPLAY "REWRITE FAILED:" TRANSNO
                   ADD 1 TO FAILED
                   EXIT PARAGRAPH
           END-REWRITE
           ADD 1 TO SUCCESS
           ADD TR-AMOUNT TO TOTAL
           DISPLAY "WITHDRAWAL SUCESSFUL  :" TRANSNO.
       DISPLAY-SUMMARY.
           DISPLAY "TOTAL TRANSACTIONS    :" TRANS
           DISPLAY "SUCCESSFUL WITHDRAWALS:" SUCCESS
           DISPLAY "FAILED WITHDRAWALS    :" FAILED
           DISPLAY "TOTAL AMOUNT DISPENSED:" TOTAL
           DISPLAY "BATCH COMPLETED SUCCESSFULLY".
