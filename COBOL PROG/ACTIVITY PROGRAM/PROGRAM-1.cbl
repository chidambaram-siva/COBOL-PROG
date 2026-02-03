       IDENTIFICATION DIVISION.
       PROGRAM-ID. NUMCHECK.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-CHOICE     PIC 9.
       01  WS-NUMBER     PIC 9(5).
       01  WS-TEMP       PIC 9(5).
       01  WS-REVERSE    PIC 9(5) VALUE 0.
       01  WS-DIGIT      PIC 9.
       01  WS-SUM        PIC 9(6) VALUE 0.
       01  WS-COUNT      PIC 9(5).
       01  WS-ARM-SUM    PIC 9(6) VALUE 0.
       01  WS-POWER      PIC 9(6).

       PROCEDURE DIVISION.
       MAIN-PARA.
  
               DISPLAY "1. Check Palindrome Number"
               DISPLAY "2. Check Armstrong Number"
               DISPLAY "3. Check Perfect Number"
               DISPLAY "4. Exit"
               DISPLAY "Enter your choice: "
               ACCEPT WS-CHOICE

               EVALUATE WS-CHOICE
                   WHEN 1
                       PERFORM PALINDROME-CHECK
                   WHEN 2
                       PERFORM ARMSTRONG-CHECK
                   WHEN 3
                       PERFORM PERFECT-CHECK
                   WHEN 4
                       DISPLAY "Exiting Program..."
                   WHEN OTHER
                       DISPLAY "Invalid Choice"
               END-EVALUATE
    

           STOP RUN.

       PALINDROME-CHECK.
           DISPLAY "Enter number: "
           ACCEPT WS-NUMBER

           MOVE WS-NUMBER TO WS-TEMP
           MOVE 0 TO WS-REVERSE

           PERFORM UNTIL WS-TEMP = 0
               DIVIDE WS-TEMP BY 10
                   GIVING WS-TEMP
                   REMAINDER WS-DIGIT
               COMPUTE WS-REVERSE =
                   (WS-REVERSE * 10) + WS-DIGIT
           END-PERFORM

           IF WS-REVERSE = WS-NUMBER
               DISPLAY "Palindrome Number"
           ELSE
               DISPLAY "Not a Palindrome Number"
           END-IF.

       ARMSTRONG-CHECK.
           DISPLAY "Enter number: "
           ACCEPT WS-NUMBER

           MOVE WS-NUMBER TO WS-TEMP
           MOVE 0 TO WS-ARM-SUM

           PERFORM UNTIL WS-TEMP = 0
               DIVIDE WS-TEMP BY 10
                   GIVING WS-TEMP
                   REMAINDER WS-DIGIT
               COMPUTE WS-POWER =
                   WS-DIGIT * WS-DIGIT * WS-DIGIT
               ADD WS-POWER TO WS-ARM-SUM
           END-PERFORM

           IF WS-ARM-SUM = WS-NUMBER
               DISPLAY "Armstrong Number"
           ELSE
               DISPLAY "Not an Armstrong Number"
           END-IF.

       PERFECT-CHECK.
           DISPLAY "Enter number: "
           ACCEPT WS-NUMBER

           MOVE 0 TO WS-SUM

           PERFORM VARYING WS-COUNT FROM 1 BY 1
               UNTIL WS-COUNT = WS-NUMBER
               DIVIDE WS-NUMBER BY WS-COUNT
                   GIVING WS-TEMP
                   REMAINDER WS-DIGIT
               IF WS-DIGIT = 0
                   ADD WS-COUNT TO WS-SUM
               END-IF
           END-PERFORM

           IF WS-SUM = WS-NUMBER
               DISPLAY "Perfect Number"
           ELSE
               DISPLAY "Not a Perfect Number"
           END-IF.