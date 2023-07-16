      *****************************************************************
      * Program name:    PBURWDE
      * Original author: YUNUS EMRE AKDIK
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  PBURWDE.
       AUTHOR. YUNUS EMRE AKDIK
      *****************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INP-FILE ASSIGN TO INPFILE
                           STATUS ST-INPFILE.
           SELECT OUT-FILE ASSIGN TO OUTFILE
                           STATUS ST-OUTFILE.
      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
       FD INP-FILE RECORDING MODE F.
       01 INP-REC.
         03 INP-OPT                PIC X(01).
         03 INP-ID                 PIC 9(05).
       FD OUT-FILE RECORDING MODE F.
       01 OUT-REC.
           03 OUT-DESC             PIC X(118).
       WORKING-STORAGE SECTION.
       01 WS-WORK-AREA.
           05 ST-OUTFILE           PIC 9(02).
              88 OUT-SUCCESS             VALUE 00.
           05 ST-INPFILE           PIC 9(02).
              88 INP-SUCCESS             VALUE 00.
              88 INP-EOF                 VALUE 10.
           05 WS-OPERATION-TYPE    PIC 9(01).
              88 VALID-OPT               VALUE 1 THRU 4.
           05 WS-SUB-AREA.
              07 WS-SUB-FUNC          PIC 9(01).
                 88 WS-SUB-FUNC-READ     VALUE 1.
                 88 WS-SUB-FUNC-WRITE    VALUE 2.
                 88 WS-SUB-FUNC-DELETE   VALUE 3.
                 88 WS-SUB-FUNC-UPDATE   VALUE 4.
              07 WS-SUB-ID            PIC 9(05).
              07 WS-SUB-RC            PIC 9(02).
              07 WS-SUB-NAME          PIC X(15).
              07 WS-SUB-SURNAME       PIC X(15).
              07 WS-SUB-EXC           PIC S9(03).
              07 WS-SUB-DATE          PIC S9(07).
              07 WS-SUB-BALANCE       PIC S9(15).
              07 WS-SUB-DESC          PIC X(119).
      *****************************************************************
       PROCEDURE DIVISION.
       H000-MAIN.
           PERFORM H100-INITIALIZE.
           PERFORM H200-PROCESS UNTIL INP-EOF.
           PERFORM H300-TERMINATE.
       H000-END. EXIT.

       H100-INITIALIZE.
           OPEN INPUT INP-FILE.
           IF NOT INP-SUCCESS
              DISPLAY "INP-FILE OPEN ERROR"
              PERFORM H300-TERMINATE
           END-IF.
           OPEN OUTPUT OUT-FILE.
           IF NOT OUT-SUCCESS
              DISPLAY "OUT-FILE OPEN ERROR"
              PERFORM H300-TERMINATE
           END-IF.
           READ INP-FILE.
           IF NOT INP-SUCCESS
              DISPLAY "INP-FILE READ ERROR"
              PERFORM H300-TERMINATE
           END-IF.
       H100-END. EXIT.

       H200-PROCESS.
           PERFORM H210-CHECK-OPERATION-TYPE.
           IF VALID-OPT
              IF WS-SUB-FUNC-WRITE
                   MOVE "Yunus Emre     " TO WS-SUB-NAME
                   MOVE "Akdik          " TO WS-SUB-SURNAME
                   MOVE 948 TO WS-SUB-EXC
                   MOVE 20191231 TO WS-SUB-DATE
               ELSE IF WS-SUB-FUNC-READ
                   MOVE SPACES TO WS-SUB-NAME
                   MOVE SPACES TO WS-SUB-SURNAME
                   MOVE ZEROES TO WS-SUB-EXC
                   MOVE ZEROES TO WS-SUB-DATE
              END-IF
              MOVE ZEROES TO WS-SUB-BALANCE
              MOVE INP-ID TO WS-SUB-ID
              MOVE SPACES TO WS-SUB-DESC
              MOVE ZEROS  TO WS-SUB-RC
              CALL 'PBSUB' USING WS-SUB-AREA
              MOVE WS-SUB-DESC TO OUT-DESC
              WRITE OUT-REC
            END-IF.
            MOVE SPACES TO WS-SUB-DESC
            READ INP-FILE.
       H200-END. EXIT.

       H210-CHECK-OPERATION-TYPE.
            EVALUATE INP-OPT
               WHEN 'R'
                  SET WS-SUB-FUNC-READ TO TRUE
               WHEN 'W'
                  SET WS-SUB-FUNC-WRITE TO TRUE
               WHEN 'D'
                  SET WS-SUB-FUNC-DELETE TO TRUE
               WHEN 'U'
                  SET WS-SUB-FUNC-UPDATE TO TRUE
               WHEN OTHER
                  MOVE 0 TO WS-SUB-FUNC
           END-EVALUATE.
           MOVE WS-SUB-FUNC TO WS-OPERATION-TYPE.
           MOVE SPACES TO OUT-DESC.
           IF NOT VALID-OPT
              STRING INP-OPT DELIMITED BY SIZE
                     INP-ID  DELIMITED BY SIZE
                     "-INVALID OPERATION TYPE." DELIMITED BY SIZE
                     INTO OUT-DESC
               WRITE OUT-REC
           END-IF.
       H210-END. EXIT.

       H300-TERMINATE.
           CLOSE INP-FILE
                 OUT-FILE.
           STOP RUN.
       H300-END. EXIT.
