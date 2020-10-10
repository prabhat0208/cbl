      *-------------------------
       IDENTIFICATION DIVISION.
      *-------------------------
       PROGRAM-ID.    ADDONE. //put jcl jobid of 8 bits
       AUTHOR.        DIV.

      *------------------------
       ENVIRONMENT DIVISION.
      *------------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUST-RECS ASSIGN TO CUSTRECS // always set your program id same as cbl prog name
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TOP-ACCTS ASSIGN TO TOPACCTS
           ORGANIZATION IS LINE SEQUENTIAL.

      *-------------------
       DATA DIVISION.
      *-------------------
       FILE SECTION.
       FD  CUST-RECS RECORD CONTAINS 80 CHARACTERS RECORDING MODE F.
       01  CUST-RECORD.
           05 CUST-FIRSTNAME PIC X(11).
           05 FILLER PIC X(3) VALUE SPACES.
           05 CUST-LASTNAME PIC X(22).
           05 FILLER PIC X(28) VALUE SPACES.
           05 CUST-ACCTBAL PIC ZZZ,ZZZ,ZZZ,ZZZ.

       FD  TOP-ACCTS RECORD CONTAINS 80 CHARACTERS RECORDING MODE F.
       01  OUTPUT-CUSTOMERS.
           05 OUTPUT-FIRSTN PIC X(11).
           05 FILLER PIC X(3) VALUE SPACES.
           05 OUTPUT-LASTN PIC X(22).
           05 FILLER PIC X(28) VALUE SPACES.
           05 OUTPUT-BAL PIC ZZZ,ZZZ,ZZZ,ZZZ.
           05 ws-space pic x(4) value spaces.

       WORKING-STORAGE SECTION.

       01  WS-CUSTOMERS.
           05 WS-FISRTN PIC X(11).
           05 FILLER PIC X(3) VALUE SPACES.
           05 WS-LASTN PIC X(22).
           05 FILLER PIC X(28) VALUE SPACES.
           05 WS-BAL PIC ZZZ,ZZZ,ZZZ,ZZZ.
           05 ws-space pic x(4) value spaces.

       01  WS-LAST-REC PIC X(1).
       01  WS-BAL-NUM USAGE IS COMP-1.

      ****************************************************************
      *                  PROCEDURE DIVISION                          *
      ****************************************************************
       PROCEDURE DIVISION.
      *
       OPEN-FILES.
           OPEN INPUT CUST-RECS.
           OPEN OUTPUT TOP-ACCTS.

           PERFORM UNTIL WS-LAST-REC = 'Y'
           READ CUST-RECS INTO WS-CUSTOMERS
           AT END MOVE 'Y' TO WS-LAST-REC
           NOT AT END PERFORM WRITE-REC
           END-READ
           END-PERFORM.

           CLOSE CUST-RECS.
           CLOSE TOP-ACCTS.
           STOP RUN.

       WRITE-REC.
           COMPUTE WS-BAL-NUM = FUNCTION NUMVAL-C(OUTPUT-BAL)
           IF WS-BAL-NUM < 50000 THEN
              MOVE WS-CUSTOMERS TO OUTPUT-CUSTOMERS
              WRITE OUTPUT-CUSTOMERS
              END-WRITE
           END-IF.
