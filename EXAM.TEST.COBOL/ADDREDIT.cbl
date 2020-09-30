      *--------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *--------------------------------------------------------------
       PROGRAM-ID.    ADDREDIT.
       AUTHOR.        WARRIORS.
      *--------------------------------------------------------------
      ***************************************************************
      * Workshop:                   FINAL EXAM
      * Developer:                  kun zhang
      *                             dave peel
      *
      * Created:                    2020-09-19
      * Modified:
      *    2020-09-25 Initial release                               dgp
      *    2020-09-25 Added 'ADDR' to address type duplication msg  dgp
      *    2020-09-26 Corrected 110-INIT-STATE-ZIP-TABLE to read
                      entire table.  Wyoming is back in the union   dgp
      * Developer Contact:
      * V R M:                      V0R0M1
      *  Version Level
      *  Release Level
      *  Modification Level
      * SUB-PROGRAM Called from PARTMAIN
      * Evaluates the data that is passed defined below for errors and
      * then returns information back to PARTMAIN.
      * PARTMAIN passes.
      *  01  WS-SUPP-ADDRESSES-MAIN.
      *     05  WS-SUPP-ADDRESS.
      *         10   WS-ADDRESS-TYPE      PIC X(01) VALUE SPACES.
      *             88  ORDER-ADDRESS           VALUE '1'.
      *             88  SCHED-ADDRESS           VALUE '2'.
      *             88  REMIT-ADDRESS           VALUE '3'.
      *             88  ADDRESS-TYPE-VALID VALUES '1' '2' '3'.
      *         10  WS-ADDRESS-1         PIC X(15) VALUE SPACES.
      *         10  WS-ADDRESS-2         PIC X(15) VALUE SPACES.
      *         10  WS-ADDRESS-3         PIC X(15) VALUE SPACES.
      *         10  WS-CITY              PIC X(15) VALUE SPACES.
      *         10  WS-ADDR-STATE        PIC X(02) VALUE SPACES.
      *         10  WS-ZIP-CODE          PIC 9(10) VALUE ZERO.
      *
      *    300-ERROR-MESSAGE-RETURN
      *         Will send message to the PARTMAIN.
      ***************************************************************

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT STATEZIP-FILE ASSIGN TO STATEZIP.


       DATA DIVISION.
       FILE SECTION.

       FD  STATEZIP-FILE

           DATA RECORD IS STATEZIP-REC.

       01  STATEZIP-REC.
           05  SZ-STATE-NAME    PIC X(16).
           05  SZ-STATE-ABBREV  PIC X(02).
           05  FILLER           PIC X(02).
           05  SZ-ZIP-LOW       PIC 9(05).
           05  FILLER           PIC X(03).
           05  SZ-ZIP-HIGH      PIC 9(05).
           05  FILLER           PIC X(87).


       WORKING-STORAGE SECTION.

      *Matches the defined filelds in the COPYBOOK.
       01  WS-SUPP-ADDRESSES-MAIN.
           05  WS-SUPP-ADDRESS OCCURS 3 TIMES.
               10   WS-ADDRESS-TYPE      PIC X(01) VALUE SPACES.
                       88  ADDRESS-TYPE-VALID VALUES '1' '2' '3'.
               10  WS-ADDRESS-1         PIC X(15) VALUE SPACES.
               10  WS-ADDRESS-2         PIC X(15) VALUE SPACES.
               10  WS-ADDRESS-3         PIC X(15) VALUE SPACES.
               10  WS-CITY              PIC X(15) VALUE SPACES.
               10  WS-ADDR-STATE        PIC X(02) VALUE SPACES.
               10  WS-ZIP-CODE          PIC X(10) VALUE SPACES.
               10  WS-SPLIT-ZIP-CODE REDEFINES WS-ZIP-CODE.
                   15  WS-ZIP-BASIC     PIC 9(5).
                   15  WS-ZIP-PLUS4     PIC 9(5).

       01  WS-STATE-ZIP-TABLE.
           05 WS-STATE-ZIP-MAXENTRY     PIC 9(02) COMP VALUE 72.
           05 WS-STATE-ZIP-ENTRY OCCURS 72 TIMES
                ASCENDING KEY IS WS-SZ-ABBREV
                INDEXED BY STATEZIP-IDX.
                10 WS-SZ-ABBREV           PIC X(02).
                10 WS-SZ-ZIPLOW           PIC 9(05) COMP.
                10 WS-SZ-ZIPHIGH          PIC 9(05) COMP.
      * Define the error message structure
       01  WS-ERROR-MESSAGES.
           05 ERR-MSG-BUFFER     PIC X(28).
           05 ERR-DUP-TYPE-BUFF.
               10 FILLER          PIC X(10) VALUE 'ADDR TYPE '.
               10 ERR-DUP-TYP-VAL PIC X(01).
               10 FILLER          PIC X(14) VALUE ' IS DUPLICATED'.
               10 FILLER          PIC X(05) VALUE SPACES.
           05 ERR-INVSTATE-BUFF.
               10 FILLER          PIC X(15) VALUE 'INVALID STATE: '.
               10 ERR-INVSTATE-VAL PIC X(02).
               10 FILLER          PIC X(13) VALUE SPACES.
           05 ERR-STATE-ZIP-BUFF.
               10 FILLER         PIC X(04) VALUE 'ZIP '.
               10 ERR-SZ-ZIP-VAL PIC X(05).
               10 FILLER         PIC X(11) VALUE ' IS NOT IN '.
               10 ERR-SZ-ST-VAL  PIC X(02).
               10 FILLER         PIC X(08) VALUE SPACES.

       01 WS-ERROR-NUMBER     PIC 9(3)   VALUE ZEROS.
       01 COUNTERS-AND-FLAGS.
           05 ADDR-IDX                PIC 9.
           05 ADDRTYP-IDX             PIC 9 COMP.
           05 ADDR-RECS-FOUND         PIC 9 COMP.
      * RECNO-FOR-TYPE stores the record number in which each value
      * of ADDRESS-TYPE has been encountered.  This is used as an
      * easy means to determine whether an ADDRESS-TYPE value occurs
      * more than once, which is a defect.
           05 RECNO-FOR-TYPE OCCURS 3 TIMES INDEXED BY RECNO-IDX
                                       PIC 9 VALUE 0.
      * There are three addr types, and three addr records.  In this
      * configuration, it's impossible for more than one type to be
      * repeated.  (A given type can be present twice or thrice, but
      * there's no room to repeat more than one type.)  So all we need
      * to track is whether it's already been reported or not.
           05 DUP-TYPE-REPORTED-FLAG  PIC X.
                 88 DUP-TYPE-HAS-BEEN-REPORTED VALUE 'Y'.
           05 STATE-ZIP-MATCH-FLAG   PIC X.
                 88 STATE-ZIP-MATCH-FOUND VALUE 'Y'.
           05 COMP-ZIP-BASIC        PIC 9(05) COMP.
           05 STATE-BLANK-FLAG      PIC X VALUE 'N'.
                 88 STATE-IS-NOT-BLANK VALUE 'N'.
           05 ZIP-BLANK-FLAG     PIC X VALUE 'N'.
                 88 ZIP-IS-NOT-BLANK    VALUE 'N'.
           05 STATE-VALID-FLAG   PIC X VALUE 'Y'.
                 88 STATE-IS-VALID   VALUE 'Y'.
           05 STATE-ZIP-LOADED-FLAG   PIC X VALUE SPACES.
                88 STATE-ZIP-LOADED    VALUE 'Y'.
           05 STATE-ZIP-EOF           PIC X VALUE SPACES.
                88 NO-MORE-STATEZIPS  VALUE 'Y'.


      *--------------------------------------------------------------
       LINKAGE SECTION.
      *--------------------------------------------------------------
       01 LS-SUPP-ADDRESS-ONE         PIC X(73).
       01 LS-SUPP-ADDRESS-TWO         PIC X(73).
       01 LS-SUPP-ADDRESS-THREE       PIC X(73).
       01 LS-RETURN-CODE     PIC 9(1).
       01 LS-RETURN-MESSAGE  PIC X(90).
       01 LS-RETURN-MESSAGE-BUFFERS REDEFINES LS-RETURN-MESSAGE.
           05 LS-RETURN-MESSAGE-MEMBER OCCURS 3 TIMES PIC X(30).
       01 LS-ERROR-TOT    PIC 9(3).
      *****************************************************************
      * This subroutine validates:
      * -the mandatory fields are not empty
      * and
      *   -ADDRESS TYPE
      *   -     must be one of the listed 88 level fields
      *   -The address zip code must between
      *         two columns of zip codes
      *****************************************************************
       PROCEDURE DIVISION
           USING
                LS-SUPP-ADDRESS-ONE,
                LS-SUPP-ADDRESS-TWO,
                LS-SUPP-ADDRESS-THREE,
                LS-RETURN-CODE,
                LS-RETURN-MESSAGE,
                LS-ERROR-TOT.
      * Business logics start here
       000-MAIN.
           PERFORM 100-INITIALIZATION
           PERFORM 120-GET-DATA
           MOVE 1 TO ADDR-IDX
           PERFORM 200-PROCESS-DATA
           MOVE 2 TO ADDR-IDX
           PERFORM 200-PROCESS-DATA
           MOVE 3 TO ADDR-IDX
           PERFORM 200-PROCESS-DATA

           GOBACK.

       100-INITIALIZATION.
           PERFORM 110-INIT-STATE-ZIP-TABLE.
           MOVE 0 TO LS-RETURN-CODE
           MOVE SPACES TO LS-RETURN-MESSAGE
           MOVE 0 TO LS-ERROR-TOT
           MOVE 0 TO ADDR-RECS-FOUND.
           INITIALIZE RECNO-FOR-TYPE(1)
           INITIALIZE RECNO-FOR-TYPE(2)
           INITIALIZE RECNO-FOR-TYPE(3)
           INITIALIZE STATE-BLANK-FLAG
           INITIALIZE ZIP-BLANK-FLAG
           INITIALIZE STATE-VALID-FLAG
           MOVE 'N' TO DUP-TYPE-REPORTED-FLAG.


       110-INIT-STATE-ZIP-TABLE.
           IF NOT STATE-ZIP-LOADED
               OPEN INPUT STATEZIP-FILE
               READ STATEZIP-FILE
                 AT END MOVE 'Y' TO STATE-ZIP-EOF
               END-READ
               PERFORM VARYING STATEZIP-IDX FROM 1 BY 1
                  UNTIL STATEZIP-IDX > WS-STATE-ZIP-MAXENTRY OR
                          NO-MORE-STATEZIPS
                    MOVE SZ-STATE-ABBREV TO WS-SZ-ABBREV(STATEZIP-IDX)
                    MOVE SZ-ZIP-LOW TO WS-SZ-ZIPLOW(STATEZIP-IDX)
                    MOVE SZ-ZIP-HIGH TO WS-SZ-ZIPHIGH(STATEZIP-IDX)
                    READ STATEZIP-FILE
                       AT END MOVE 'Y' TO STATE-ZIP-EOF
                    END-READ
               END-PERFORM
               MOVE 'Y' TO STATE-ZIP-LOADED-FLAG
               CLOSE STATEZIP-FILE
           END-IF.

       120-GET-DATA.
           MOVE LS-SUPP-ADDRESS-ONE TO WS-SUPP-ADDRESS(1)
           MOVE LS-SUPP-ADDRESS-TWO TO WS-SUPP-ADDRESS(2)
           MOVE LS-SUPP-ADDRESS-THREE TO WS-SUPP-ADDRESS(3).


       200-PROCESS-DATA.

      * Initialize variables local to each address
           MOVE 'N' TO STATE-BLANK-FLAG
           MOVE 'N' TO ZIP-BLANK-FLAG
           MOVE 'Y' TO STATE-VALID-FLAG.

           IF WS-SUPP-ADDRESS(ADDR-IDX) = SPACES THEN
              EVALUATE ADDR-IDX  *> some all blanks are ok
                 WHEN 1 *> First rec can't be all blank
                    PERFORM 820-REPORT-NON-TRAILING-BLANK
                 WHEN 2 *> Second rec can't be blank unless third is
                    IF WS-SUPP-ADDRESS(3) NOT = SPACES THEN
                       PERFORM 820-REPORT-NON-TRAILING-BLANK
                    END-IF
              END-EVALUATE  *> (Third can always be blank)
           ELSE  *> don't test anything else unless record is non-blank
             IF WS-ADDRESS-TYPE(ADDR-IDX) = SPACE THEN
                 MOVE 'ADDRESS TYPE BLANK' TO  ERR-MSG-BUFFER
                 PERFORM 800-PREPARE-ERR-MSG
             ELSE
                IF NOT ADDRESS-TYPE-VALID(ADDR-IDX) THEN
                   MOVE 'ADDRESS TYPE INVALID' TO ERR-MSG-BUFFER
                   PERFORM 800-PREPARE-ERR-MSG
                ELSE
      * ADDRESS-TYPE is valid, therefore 1-3,
      * It's thus a valid index into RECNO-FOR-TYPE
                  MOVE WS-ADDRESS-TYPE(ADDR-IDX) TO ADDRTYP-IDX
                  SET RECNO-IDX TO ADDRTYP-IDX
                  IF RECNO-FOR-TYPE(RECNO-IDX) NOT = 0 AND
                     NOT DUP-TYPE-HAS-BEEN-REPORTED THEN
                      MOVE WS-ADDRESS-TYPE(ADDR-IDX) TO
                           ERR-DUP-TYP-VAL
                      MOVE ERR-DUP-TYPE-BUFF TO ERR-MSG-BUFFER
                      PERFORM 800-PREPARE-ERR-MSG
                      MOVE 'Y' TO DUP-TYPE-REPORTED-FLAG
                  END-IF
                  MOVE ADDR-IDX TO RECNO-FOR-TYPE(RECNO-IDX)
                END-IF
              END-IF
              IF WS-ADDRESS-1(ADDR-IDX) = SPACE THEN
                 MOVE 'ADDRESS1 IS BLANK' TO ERR-MSG-BUFFER
                 PERFORM 800-PREPARE-ERR-MSG
              END-IF
              IF WS-CITY(ADDR-IDX) = SPACE THEN
                 MOVE 'CITY IS BLANK' TO ERR-MSG-BUFFER
                 PERFORM 800-PREPARE-ERR-MSG
              END-IF
              IF WS-ADDR-STATE(ADDR-IDX) = SPACE THEN
                 MOVE 'Y' TO STATE-BLANK-FLAG
                 MOVE 'STATE IS BLANK' TO ERR-MSG-BUFFER
                 PERFORM 800-PREPARE-ERR-MSG
              END-IF
              IF WS-ZIP-CODE(ADDR-IDX) = SPACE THEN
                 MOVE 'Y' TO ZIP-BLANK-FLAG
                 MOVE 'ZIP-CODE IS BLANK' TO ERR-MSG-BUFFER
                 PERFORM 800-PREPARE-ERR-MSG
              END-IF
              IF ZIP-IS-NOT-BLANK AND STATE-IS-NOT-BLANK
                 PERFORM 210-VERIFY-STATE-ZIP
              END-IF

           END-IF.



       210-VERIFY-STATE-ZIP.
           SET STATEZIP-IDX TO 1
           MOVE 'N' TO STATE-ZIP-MATCH-FLAG
           MOVE 'N' TO STATE-VALID-FLAG
           MOVE WS-ZIP-BASIC(ADDR-IDX) TO COMP-ZIP-BASIC
      * Skip through State-Zip table until we reach:
      *    A record with the same abbreviation; (great success)
      *    A record with a lexically greater abbreviation; (go fish) or
      *    The end of the table (go fish).
           PERFORM VARYING STATEZIP-IDX FROM 1 BY 1
             UNTIL WS-SZ-ABBREV(STATEZIP-IDX) >= WS-ADDR-STATE(ADDR-IDX)
               OR STATEZIP-IDX > WS-STATE-ZIP-MAXENTRY
           END-PERFORM

           IF WS-SZ-ABBREV(STATEZIP-IDX) = WS-ADDR-STATE(ADDR-IDX)
             MOVE 'Y' TO STATE-VALID-FLAG  *> found the state
      * Test the zip against the first state-matching entry
             IF COMP-ZIP-BASIC >= WS-SZ-ZIPLOW(STATEZIP-IDX) AND
                                 <= WS-SZ-ZIPHIGH(STATEZIP-IDX)
                MOVE 'Y' TO STATE-ZIP-MATCH-FLAG
              END-IF

      * If the first hit wasn't a state+zip match, keep looking until:
      *    We find a state+zip match;
      *    We run out of entries for that state; or
      *    We reach the end of the table.
             PERFORM VARYING STATEZIP-IDX FROM STATEZIP-IDX BY 1
               UNTIL STATE-ZIP-MATCH-FOUND OR
                  STATEZIP-IDX > WS-STATE-ZIP-MAXENTRY OR
                  WS-SZ-ABBREV(STATEZIP-IDX) > WS-ADDR-STATE(ADDR-IDX)
               IF COMP-ZIP-BASIC >= WS-SZ-ZIPLOW(STATEZIP-IDX) AND
                                <= WS-SZ-ZIPHIGH(STATEZIP-IDX)
                    MOVE 'Y' TO STATE-ZIP-MATCH-FLAG
               END-IF
             END-PERFORM
           END-IF


           IF NOT STATE-IS-VALID THEN
              PERFORM 810-REPORT-INVALID-STATE
           ELSE
              IF NOT STATE-ZIP-MATCH-FOUND THEN
                 PERFORM 820-REPORT-STATE-ZIP-MISMATCH
              END-IF
           END-IF.


       800-PREPARE-ERR-MSG.
      * Handles incrementing the error count, as well as moving the
      * contents of ERR-MSG-BUFFER to the appropriate position in the
      * return buffer, until the error count reaches 4.

           MOVE 8 TO LS-RETURN-CODE
           IF LS-ERROR-TOT < 4 *> Stop counting after 4
              ADD 1 TO LS-ERROR-TOT

              IF LS-ERROR-TOT < 4 *> only room for 3 msgs
                 STRING ADDR-IDX
                    ':'
                    ERR-MSG-BUFFER
                    DELIMITED BY SIZE
                 INTO
                     LS-RETURN-MESSAGE-MEMBER(LS-ERROR-TOT)
              END-IF
           END-IF.

       810-REPORT-INVALID-STATE.
           MOVE 'N' TO STATE-VALID-FLAG

           MOVE WS-ADDR-STATE(ADDR-IDX) TO ERR-INVSTATE-VAL
           MOVE ERR-INVSTATE-BUFF TO ERR-MSG-BUFFER
           PERFORM 800-PREPARE-ERR-MSG.

       820-REPORT-STATE-ZIP-MISMATCH.
           MOVE WS-ZIP-BASIC(ADDR-IDX) TO ERR-SZ-ZIP-VAL
           MOVE WS-ADDR-STATE(ADDR-IDX) TO ERR-SZ-ST-VAL
           MOVE ERR-STATE-ZIP-BUFF TO ERR-MSG-BUFFER
           PERFORM 800-PREPARE-ERR-MSG.

       820-REPORT-NON-TRAILING-BLANK.
           MOVE 'BLANK ADDRS MUST BE AT END' TO ERR-MSG-BUFFER
           PERFORM 800-PREPARE-ERR-MSG.



