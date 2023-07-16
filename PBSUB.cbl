      *****************************************************************
      * Program name:    PBSUB
      * Original author: YUNUS EMRE AKDIK
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  PBSUB.
       AUTHOR. PBSUB.
      *****************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IDX-FILE ASSIGN TO IOINDEX
                           STATUS ST-IDXFILE
                           ORGANIZATION INDEXED
                           ACCESS RANDOM
                           RECORD KEY IDX-KEY.
       DATA DIVISION.
       FILE SECTION.
       FD IDX-FILE.
       01 IDX-REC.
           03 IDX-KEY.
                05 IDX-ID          PIC S9(5) COMP-3.
           03 IDX-EXC              PIC S9(3) COMP.
           03 IDX-NAME             PIC X(15).
           03 IDX-SURNAME          PIC X(15).
           03 IDX-DATE             PIC S9(7) COMP-3.
           03 IDX-BALANCE          PIC S9(15) COMP-3.
       WORKING-STORAGE SECTION.
       01 WS-WORK-AREA.
           03 ST-IDXFILE           PIC 9(02).
              88 ST-IDXFILE-OK     VALUE 00 97.
              88 ST-IDXFILE-EOF    VALUE 10.
           03 WS-ID                   PIC S9(05) COMP-3.
           03 WS-COUNTER              PIC S9(02).
           03 WS-COUNTER-J            PIC S9(02).
           03 WS-DESC-1               PIC X(09).
           03 WS-DESC-2               PIC X(96).
           03 WS-FNAME-FROM           PIC X(15).
           03 WS-FNAME-TO             PIC X(15).
           03 WS-LNAME-FROM           PIC X(15).
           03 WS-LNAME-TO             PIC X(15).
       LINKAGE SECTION.
       01 WS-SUB-AREA.
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
       PROCEDURE DIVISION USING WS-SUB-AREA.
       H100-MAIN.
           PERFORM H200-INITIALIZE.
           PERFORM H300-PROCESS.
           PERFORM H900-TERMINATE.
       H100-END. EXIT.

       H200-INITIALIZE.
           OPEN I-O IDX-FILE.
           IF NOT ST-IDXFILE-OK
              DISPLAY "IDX-FILE OPEN ERROR"
              DISPLAY "STATUS CODE: " ST-IDXFILE
              PERFORM H900-TERMINATE
           END-IF.
           MOVE SPACES TO WS-DESC-1.
           MOVE SPACES TO WS-DESC-2.
           MOVE SPACES TO WS-FNAME-FROM.
           MOVE SPACES TO WS-FNAME-TO.
           MOVE SPACES TO WS-LNAME-FROM.
           MOVE SPACES TO WS-LNAME-TO.
       H200-END. EXIT.

       H300-PROCESS.
           MOVE WS-SUB-ID TO IDX-ID
           READ IDX-FILE KEY IDX-KEY
           INVALID KEY
              IF WS-SUB-FUNC-WRITE THEN PERFORM H320-WRITE
              ELSE
              PERFORM H800-INVALID-KEY
              END-IF
           NOT INVALID KEY
           IF WS-SUB-FUNC-READ THEN PERFORM H310-READ
           END-IF
           IF WS-SUB-FUNC-DELETE THEN PERFORM H330-DELETE
           END-IF
           IF WS-SUB-FUNC-UPDATE THEN PERFORM H340-UPDATE
           END-IF
           IF WS-SUB-FUNC-WRITE THEN PERFORM H800-INVALID-KEY
           END-IF
           END-READ.
       H300-END. EXIT.

       H310-READ.
              MOVE IDX-EXC TO WS-SUB-EXC.
              MOVE IDX-NAME TO WS-SUB-NAME.
              MOVE IDX-SURNAME TO WS-SUB-SURNAME.
              MOVE IDX-DATE TO WS-SUB-DATE.
              MOVE IDX-BALANCE TO WS-SUB-BALANCE.
              MOVE IDX-ID TO WS-SUB-ID.
              PERFORM H700-NOT-INVALID.
       H310-END. EXIT.

       H320-WRITE.
            MOVE WS-SUB-EXC TO  IDX-EXC.
            MOVE WS-SUB-NAME TO IDX-NAME.
            MOVE WS-SUB-SURNAME TO IDX-SURNAME.
            MOVE WS-SUB-DATE TO IDX-DATE.
            MOVE WS-SUB-BALANCE TO IDX-BALANCE.
            MOVE WS-SUB-ID TO IDX-ID.
            WRITE IDX-REC.
            PERFORM H700-NOT-INVALID.
       H320-END. EXIT.

       H330-DELETE.
           DELETE IDX-FILE.
           PERFORM H700-NOT-INVALID.
       H330-END. EXIT.

       H340-UPDATE.
             MOVE SPACES TO WS-FNAME-TO.
             MOVE 1 TO WS-COUNTER-J.
             MOVE 0 TO WS-COUNTER.
             MOVE IDX-NAME TO WS-FNAME-FROM
             MOVE IDX-SURNAME TO WS-LNAME-FROM
             PERFORM VARYING WS-COUNTER FROM 1 BY 1
                UNTIL WS-COUNTER > LENGTH OF WS-FNAME-FROM
                IF WS-FNAME-FROM (WS-COUNTER:1) = ' '
                   CONTINUE
                ELSE
                    MOVE WS-FNAME-FROM (WS-COUNTER:1) TO
                                    WS-FNAME-TO (WS-COUNTER-J:1)
                    ADD 1 TO WS-COUNTER-J
                END-IF
           END-PERFORM.
           MOVE WS-LNAME-FROM TO WS-LNAME-TO.
           INSPECT WS-LNAME-TO REPLACING ALL 'e' BY 'i'.
           INSPECT WS-LNAME-TO REPLACING ALL 'E' BY 'I'.
           INSPECT WS-LNAME-TO REPLACING ALL 'a' BY 'e'.
           INSPECT WS-LNAME-TO REPLACING ALL 'A' BY 'E'.
           MOVE WS-LNAME-TO TO IDX-SURNAME.
           MOVE WS-FNAME-TO TO IDX-NAME.
           REWRITE IDX-REC.
           PERFORM H700-NOT-INVALID.
       H340-END. EXIT.

       H700-NOT-INVALID.
           IF WS-SUB-FUNC-READ
              MOVE "-read-rc:" TO WS-DESC-1
              MOVE "KAYIT BULUNDU." TO WS-DESC-2
           END-IF.
           IF WS-SUB-FUNC-WRITE
              MOVE "-wrte-rc:" TO WS-DESC-1
              MOVE "KAYIT EKLENDI." TO WS-DESC-2
           END-IF.
           IF WS-SUB-FUNC-DELETE
              MOVE "-dlte-rc:" TO WS-DESC-1
              MOVE "KAYIT SILINDI." TO WS-DESC-2
           END-IF.
           IF WS-SUB-FUNC-UPDATE
              MOVE "-updt-rc:" TO WS-DESC-1
              MOVE FUNCTION TRIM(WS-FNAME-TO) TO WS-FNAME-TO
              STRING "KAYIT GUNCELLENDI." DELIMITED BY SIZE
                     FUNCTION TRIM(WS-FNAME-FROM TRAILING)
                     DELIMITED BY SIZE
                     " " DELIMITED BY SIZE
                     FUNCTION TRIM(WS-LNAME-FROM TRAILING)
                     DELIMITED BY SIZE
                     " > " DELIMITED BY SIZE
                     FUNCTION TRIM(WS-FNAME-TO TRAILING)
                     DELIMITED BY SIZE
                     " " DELIMITED BY SIZE
                     FUNCTION TRIM(WS-LNAME-TO TRAILING)
                     DELIMITED BY SIZE
                     INTO WS-DESC-2
           END-IF.
           PERFORM H810-WRITE-LOG.
       H700-END. EXIT.

       H800-INVALID-KEY.
           IF WS-SUB-FUNC-READ
              MOVE "-read-rc:" TO WS-DESC-1
              MOVE "KAYIT BULUNAMADI." TO WS-DESC-2
           END-IF.
           IF WS-SUB-FUNC-WRITE
              MOVE "-wrte-rc:" TO WS-DESC-1
              MOVE "KAYIT EKLENEMEDI." TO WS-DESC-2
           END-IF.
           IF WS-SUB-FUNC-DELETE
              MOVE "-dlte-rc:" TO WS-DESC-1
              MOVE "KAYIT SILINEMEDI." TO WS-DESC-2
           END-IF.
           IF WS-SUB-FUNC-UPDATE
              MOVE "-updt-rc:" TO WS-DESC-1
              MOVE "KAYIT GUNCELLENEMEDI." TO WS-DESC-2
           END-IF.
           PERFORM H810-WRITE-LOG.
       H800-END. EXIT.

       H810-WRITE-LOG.
           MOVE ST-IDXFILE TO WS-SUB-RC.
           STRING WS-SUB-ID DELIMITED BY SIZE
                  WS-DESC-1 DELIMITED BY SIZE
                  WS-SUB-RC DELIMITED BY SIZE
                  '-' DELIMITED BY SIZE
                  WS-DESC-2 DELIMITED BY SIZE
                  INTO WS-SUB-DESC.
       H810-END. EXIT.

       H900-TERMINATE.
           CLOSE IDX-FILE.
           GOBACK.
       H900-END. EXIT.
