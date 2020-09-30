      *--------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *--------------------------------------------------------------
       PROGRAM-ID.    SUPPEDIT.
       AUTHOR.        WARRIORS.
      *--------------------------------------------------------------
      ***************************************************************
      * Workshop:                   FINAL EXAM
      * Developer:                  kun zhang
      *                             dave peel
      * Created:                    2020-09-16
      * Modified:
      *    2020-09-22 Initial release                               dgp
      * Developer Contact:
      * V R M:                      V0R0M1
      *  Version Level
      *  Release Level
      *  Modification Level
      * SUB-PROGRAM Called from PARTMAIN
      * Evaluates the data that is passed defined below for errors and
      * then returns information back to PARTMAIN.
      * PARTMAIN passes.
      *    OU-SUPPLIER-CODE     PIC X(10).
      *    OU-SUPPLIER-TYPE     PIC X(01).
      *    OU-SUPPLIER-NAME     PIC X(15).
      *    OU-SUPPLIER-PERF     PIC 9(03).
      *    OU-SUPPLIER-RATING   PIC X(01).
      *    OU-SUPPLIER-STATUS   PIC X(01) VALUE SPACES.
      *    OU-SUPPLIER-ACT-DATE PIC 9(08) VALUE ZERO.
      * SUPPEDIT returns:
      *    LS-SUPPEDIT-RETURN-CODE    PIC 9  *> 8 FOR ERROR, 0 OTHERWISE
      *    LS-SUPPEDIT-RETURN-MESSAGE PIC X(90)
      *                                      *> msgs for up to 3  errors
      *                                      *> otherwise unspecified
      *    LS-SUPPEDIT-ERROR-TOT      PIC 9(3) *> count of errs 0-4
      *
      ***************************************************************

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

       DATA DIVISION.

       FILE SECTION.

       WORKING-STORAGE SECTION.

       01  INTERNAL-BUFFERS.
           05 ERR-MSG-BUFFER     PIC X(30).

       01  W-INPUT-DATE-INT    PIC 9(9) COMP.
       01  W-PICSTR-IN.
           10 W-PICSTR-LTH-IN   PIC S9(4) COMP VALUE 8.
           10 W-PICSTR-STR-IN   PIC X(8) VALUE 'YYYYMMDD'.
       01 W-DATE-IN-CEE.
           10 W-DATE-IN-LTH-CEE  PIC S9(4) COMP VALUE 8.
           10 W-DATE-IN-STR-CEE  PIC X(8).
       01  FC.
           10 FC-SEV             PIC S9(4) COMP.
           10 FC-MSG             PIC S9(4) COMP.
           10 FC-CTW             PIC X.
           10 FC-FAC             PIC X(3).
           10 FC-ISI             PIC S9(8) COMP.

      *-----------------------------------------------------------      ---
       LINKAGE SECTION.
      *--------------------------------------------------------------
      *Matches the defined filelds in the COPYBOOK.
       01  LS-SUPPLIER-CODE     PIC X(10).
       01  LS-SUPPLIER-TYPE     PIC X(01).
      * we don't care about TYPE semantics unless they involve a rule
                88 SUPPLIER-TYPE-VALID VALUES 'S' 'D' 'M' 'I'.
                88 SUBCONTRACTOR VALUE 'S'.
       01  LS-SUPPLIER-NAME     PIC X(15).
       01  LS-SUPPLIER-PERF     PIC 9(03).
       01  LS-SUPPLIER-RATING   PIC X(01).
      * we don't care about RATING semantics unless they involve a rule
                88 SUPPLIER-RATING-VALID VALUES '3' '2' '1'.
                88 HIGHEST-QUALITY VALUE '3'.
       01  LS-SUPPLIER-STATUS   PIC X(01).
                88 SUPPLIER-STATUS-VALID VALUES '1' '2' '3'.
      *  we don't care about STATUS semantics.
       01 LS-SUPPLIER-ACT-DATE PIC X(08).
       01 LS-SUPPEDIT-RETURN-CODE     PIC 9(1).
       01 LS-SUPPEDIT-RETURN-MESSAGE  PIC X(90).
       01 LS-RETURN-MESSAGE-TABLE
               REDEFINES LS-SUPPEDIT-RETURN-MESSAGE.
           05 LS-RETURN-MESSAGE-MEMBER OCCURS 3 TIMES PIC X(30).
       01 LS-SUPPEDIT-ERROR-TOT    PIC 9(3).


      *****************************************************************
      * This subroutine validates that:
      * -the mandatory fields are not empty
      * and
      *   -SUPPLIER TYPE, SUPPLIER RATING, SUPPLIER STATUS
      *   -     must be one of the listed 88 level fields
      *   -SUPPLIER ACTIVATE DATE must be blank or valid date
      *   -If SUPPLIER is a SUBCONTRACTOR, the SUPPLIER RATING
      *         must be Highest Quality
      *****************************************************************
       PROCEDURE DIVISION
           USING
                LS-SUPPLIER-CODE,
                LS-SUPPLIER-TYPE,
                LS-SUPPLIER-NAME,
                LS-SUPPLIER-PERF,
                LS-SUPPLIER-RATING,
                LS-SUPPLIER-STATUS,
                LS-SUPPLIER-ACT-DATE,
                LS-SUPPEDIT-RETURN-CODE,
                LS-SUPPEDIT-RETURN-MESSAGE,
                LS-SUPPEDIT-ERROR-TOT.

       000-MAIN.
           PERFORM 100-INITIALIZATION.
           PERFORM 200-PROCESS-DATA.
           GOBACK.

       100-INITIALIZATION.
           MOVE 0 TO LS-SUPPEDIT-RETURN-CODE.
           MOVE SPACES TO LS-SUPPEDIT-RETURN-MESSAGE.
           MOVE 0 TO LS-SUPPEDIT-ERROR-TOT.

       200-PROCESS-DATA.
      *****************************************************************
      * Tests are prioritized in cascaded if-then statements, so that
      * multiple similar errors on the same field aren't reported more
      * than once.  E.g., if it's blank, it's obviously invalid, too.
      *****************************************************************
           IF LS-SUPPLIER-CODE = SPACE THEN
              MOVE 'SUPPLIER CODE BLANK' TO  ERR-MSG-BUFFER
              PERFORM PREPARE-ERR-MSG
           END-IF
           IF LS-SUPPLIER-TYPE = SPACE THEN
              MOVE 'SUPPLIER TYPE BLANK' TO ERR-MSG-BUFFER
              PERFORM PREPARE-ERR-MSG
           ELSE
              IF NOT SUPPLIER-TYPE-VALID
                 MOVE 'SUPPLIER TYPE INVALID' TO ERR-MSG-BUFFER
                 PERFORM PREPARE-ERR-MSG
              END-IF
           END-IF
           IF LS-SUPPLIER-NAME = SPACE THEN
              MOVE 'SUPPLIER NAME BLANK' TO ERR-MSG-BUFFER
              PERFORM PREPARE-ERR-MSG
           END-IF
           IF LS-SUPPLIER-PERF = SPACE THEN
              MOVE 'SUPPLIER PERF BLANK' TO ERR-MSG-BUFFER
              PERFORM PREPARE-ERR-MSG
           ELSE
              IF LS-SUPPLIER-PERF IS NOT NUMERIC THEN
                 MOVE 'SUPPLIER PERF IS NON-NUMER' TO ERR-MSG-BUFFER
                 PERFORM PREPARE-ERR-MSG
              END-IF
           END-IF
           IF LS-SUPPLIER-RATING = SPACES THEN
              MOVE 'SUPPLIER RATING BLANK' TO ERR-MSG-BUFFER
              PERFORM PREPARE-ERR-MSG
           ELSE
              IF NOT SUPPLIER-RATING-VALID THEN
                  MOVE 'SUPPLIER RATING INVALID' TO ERR-MSG-BUFFER
                  PERFORM PREPARE-ERR-MSG
              ELSE
                  IF SUBCONTRACTOR AND NOT HIGHEST-QUALITY THEN
                      MOVE 'INCOMPAT.SUPPLIER TYPE/RATING'
                           TO ERR-MSG-BUFFER
                      PERFORM PREPARE-ERR-MSG
                  END-IF
              END-IF
           END-IF
           IF LS-SUPPLIER-STATUS = SPACES THEN
              MOVE 'SUPPLIER STATUS BLANK' TO ERR-MSG-BUFFER
              PERFORM PREPARE-ERR-MSG
           ELSE
              IF NOT SUPPLIER-STATUS-VALID THEN
                 MOVE 'SUPPLIER STATUS INVALID' TO ERR-MSG-BUFFER
                 PERFORM PREPARE-ERR-MSG
              END-IF
           END-IF

           IF LS-SUPPLIER-ACT-DATE NOT = SPACES *> Blank date is ok.
              MOVE LS-SUPPLIER-ACT-DATE TO W-DATE-IN-STR-CEE
              CALL 'CEEDAYS' USING W-DATE-IN-CEE
                   W-PICSTR-IN, W-INPUT-DATE-INT, FC
              IF FC-SEV NOT = ZERO *> if not blank, must be valid date
                 MOVE 'SUPPLIER ACT DATE INVALID' TO ERR-MSG-BUFFER
                 PERFORM PREPARE-ERR-MSG
              END-IF
           END-IF.

       PREPARE-ERR-MSG.
      * Handles incrementing the error count, as well as moving the
      * contents of ERR-MSG-BUFFER to the appropriate position in the
      * return buffer, until the error count reaches 4.

           MOVE 8 TO LS-SUPPEDIT-RETURN-CODE
           IF LS-SUPPEDIT-ERROR-TOT < 4 *> Stop counting after 4
              ADD 1 TO LS-SUPPEDIT-ERROR-TOT

              IF LS-SUPPEDIT-ERROR-TOT < 4 *> only room for 3 msgs
                 MOVE ERR-MSG-BUFFER TO
                     LS-RETURN-MESSAGE-MEMBER(LS-SUPPEDIT-ERROR-TOT)
              END-IF
           END-IF.




