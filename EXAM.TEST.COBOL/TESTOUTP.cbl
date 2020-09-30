       IDENTIFICATION DIVISION.
       PROGRAM-ID.      TESTOUTP.
       AUTHOR.          WARRIORS.

      *****************************************************************
      * ***** This program is a driver for automated integration testing
      * ***** of the entire PARTMAIN application.  Black box testing is
      * ***** carried out, strictly comparing outputs to their expected
      * ***** values, based on the test cases.
      * *****
      * ***** It reads test cases consisting of a PARTMAIN input
      * ***** record glued to its expected return values from each
      * ***** subroutine in order (PARTEDIT,SUPPEDIT,ADDREDIT,POEDIT)
      * *****
      * ***** Each expected return value is expressed as:
      * *****   Expected return code       PIC 9
      * *****   Expected error count       PIC 9(3)
      * *****   Expected error messages    PIC X(90)
      * *****
      * ***** The "downstream" output files from PARTMAIN
      * *****  (i.e., PARTS, SUPLIERS, ADRESSES, PURCHRDS),
      * ***** Along with the error output and the intermediate output
      * ***** produced for purposes of report generation, are opened,
      * ***** and stepped through.  Based on the sum of the expected
      * ***** return codes, the PARTSUPP values present in the testcase
      * ***** file are verified in either the happy or sad path output.
      * ***** Helpful messages (if total error count for the record < 4)
      * ***** are compared against those contained in the test cases'
      * *****   Expected error messages).
      * ***** Discrepancies are printed out.
      * ***** At EOF, counts of test cases failed are
      * ***** printed, along with the total cases.
      * Developer:                  Dave
      * Created:                    2020-09-12
      * Modified:
      *  2020-09-14  Plugged in functioning PARTEDIT subr       dgp
      *  2020-09-26  Fixed assumptions on eubr       dgp
      *  2020-09-22  Plugged in functioning SUPPEDIT subr       dgp
      *  2020-09-25  Plugged in functioning ADDREDIT subr       dgp
      *****************************************************************

      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TESTCASE-FILE ASSIGN TO UTSTCASE.

           SELECT TEST-REPORT ASSIGN TO ITSTRPT.

           SELECT FPARTS ASSIGN TO DPARTS.

           SELECT FSUPPS ASSIGN TO DSUPPS.

           SELECT FADDRS ASSIGN TO DADDRS.

           SELECT FPO ASSIGN TO DPO.

           SELECT FOUTPUT ASSIGN TO DOUTPUT.

           SELECT FERROR ASSIGN TO DERROR.

      *     SELECT FREPORT ASSIGN TO RPTINPUT.


       DATA DIVISION.
       FILE SECTION.

       FD  TESTCASE-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 1006 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS TESTCASE-PARTSUPP-BUF.

       01  TESTCASE-PARTSUPP-BUF.
           05  TC-PARTSUPP-BUF  PIC X(473).
           05 EXPECTED-RESULTS OCCURS 4 TIMES.
               10 EXPECTED-RETURN-CODE    PIC 9.
               10 EXPECTED-ERROR-COUNT    PIC 9(3).
               10 EXPECTED-RETURN-MESSAGE PIC X(90).

       FD  TEST-REPORT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 132 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS TEST-REPORT-BUF.

       01  TEST-REPORT-BUF  PIC X(132).

       FD  FPARTS.
           COPY 'PARTS'.

       FD  FSUPPS.
           COPY 'SUPLIERS'.

       FD  FADDRS.
           COPY 'ADRESSES'.

       FD  FPO.
           COPY 'PURCHRDS'.

       FD  FOUTPUT
           RECORDING MODE IS F

           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 473 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS PART-SUPP-ADDR-PO.

           COPY 'PARTSUPP'.

       FD  FERROR
           RECORDING MODE IS F

           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 563 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS OU-ERROR.
       01  OU-ERROR.
           05  OU-ERROUTPUT            PIC X(473).
           05  OU-ERRMESSAGE           PIC X(090).

      * FD  FREPORT
      *     DATA RECORD IS REPORT-LINE-BUF.

      *  01  REPORT-LINE-BUF     PIC X(132) VALUE SPACES.



       WORKING-STORAGE SECTION.

       77 WS-STORAGE-IND                PIC X(60)
                                                       VALUE
             'WORKING STORAGE BEGINS HERE'.

       01  SPECIMEN-ERROR-MESSAGES.
           05 MSG-INVALID-RECORD.
              10 INVALID-RECORD-TXT  PIC X(14) VALUE "INVALID RECORD".
              10 FILLER              PIC X(76) VALUE SPACES.

       01  TST-RPT-HDR1.
           05 FILLER      PIC X(59) VALUE SPACES.
           05 FILLER      PIC X(14) VALUE "COBOL Warriors".
           05 FILLER      PIC X(59) VALUE SPACES.

       01  TST-RPT-HDR2.
           05 FILLER      PIC X(53) VALUE SPACES.
           05 FILLER      PIC X(27) VALUE "PARTSUPP Program Unit Tests".
           05 FILLER      PIC X(52) VALUE SPACES.

       01  TST-RPT-FAIL-HDR.
           05  FILLER             PIC X(18) VALUE "Failed Test case# ".
           05  TST-RPT-TCASE-NO   PIC 9999 VALUE 0.
           05  FILLER             PIC X(02) VALUE SPACES.
           05  FILLER             PIC X(12) VALUE "Subroutine: ".
           05  TST-RPT-FAIL-SUBR  PIC X(10) VALUE SPACES.
           05  FILLER             PIC X(86) VALUE SPACES.

       01  TST-RPT-FAIL-INPUT.
           05   FAILED-INPUT      PIC X(100) VALUE SPACES.
           05   FILLER            PIC X(32) VALUE SPACES.

       01  TST-RPT-FAIL-MSG-DTL.
           05 FILLER         PIC X(23) VALUE "Expected Message Text: ".
           05 TST-RPT-EXP-MSG PIC X(90).
           05 FILLER         PIC X(19) VALUE SPACES.

       01  TST-RPT-FAIL-MSG-DTL2.
           05 FILLER         PIC X(23) VALUE "Found Message Text   : ".
           05 TST-RPT-ACT-MSG PIC X(90).
           05 FILLER         PIC X(19) VALUE SPACES.

       01  TST-RPT-FAIL-HDR-LINES.
           05  FILLER         PIC X(90) VALUE ALL '='.
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(11) VALUE ALL '='.
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(11) VALUE ALL '='.
           05  FILLER         PIC X(16) VALUE SPACES.

       01  TST-RPT-FAIL-DTL.
           05  FILLER         PIC X(11) VALUE "Test case# ".
           05  TST-RPT-CASE-NUM PIC 9(04).
           05  FILLER         PIC X(02) VALUE SPACES.
           05  TST-RPT-FAIL-DESC PIC X(30).

       01  TST-RPT-FAIL-HDR3.
           05  FILLER         PIC X(22) VALUE "Actual: Message Text  ".
           05  FILLER         PIC X(70) VALUE SPACES.
           05  FILLER         PIC X(11) VALUE "Return Code".
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(11) VALUE "Error Count".
           05  FILLER         PIC X(16) VALUE SPACES.

       01  TST-RPT-TLR.
           05  FILLER          PIC X(22) VALUE "Total test cases run: ".
           05  TST-RPT-TTL-TC  PIC Z(05)9 VALUE ZERO.
           05  FILLER          PIC X(02) VALUE SPACES.
           05  FILLER          PIC X(14) VALUE "Cases failed: ".
           05  TST-RPT-FAILED  PIC Z(05)9 VALUE ZERO.

      * FIELDS FOR CALLING PARTEDIT SUBROUTINE
      * The same storages are used for all four subroutines.
      * This greatly simplifies evaluation of the returns, and
      * error processing.
       01 CALL-RETURN-CODE.
           05 WS-SUBR-RETURN-CODE       PIC 9 VALUE 0.
           05 WS-SUBR-RETURN-MESSAGE    PIC X(90) VALUE SPACES.
           05 WS-SUBR-ERROR-COUNT       PIC 9(3) VALUE 0.

       01  FLAGS-AND-ACCUMULATORS.
           05  TESTCASES-TOTAL          PIC 9(6) COMP VALUE 0.
           05  TESTCASES-FAILCOUNT      PIC 9(6) COMP VALUE 0.
           05  TESTCASE-EOF             PIC X VALUE SPACES.
                 88 NO-MORE-TESTCASES  VALUE 'Y'.
           05  GOOD-FILES-EOF           PIC X VALUE SPACES.
                 88 NO-MORE-GOOD-FILES VALUE 'Y'.
           05  ERROR-FILES-EOF          PIC X VALUE SPACES.
                 88 NO-MORE-ERROR-FILES VALUE 'Y'.
           05  RECORD-TOT-EXP-FAILS   PIC 99 COMP.
                 88 RECORD-EXP-PASS VALUE 0.
                 88 REC-EXP-TOT-INVALID VALUE 4.
           05  CLEAN-RUN-SOFAR          PIC X VALUE 'Y'.
                 88 CLEAN-RUN VALUE 'Y'.
           05  SUBR-IDX                 PIC 9 VALUE 0.
           05  TESTCASES-PASSED OCCURS 4 TIMES PIC 9(6) VALUE 0.
           05  TESTCASES-FAILED OCCURS 4 TIMES PIC 9(6) VALUE 0.
           05  WS-ERROR-LCTR           PIC 99 COMP.
           05  WS-ERROR-LENGTH         PIC 99 COMP.
           05  TST-ERRMESSAGE          PIC X(90).
           05  SUBR-NAMES.
                10 FILLER PIC X(8) VALUE "PARTEDIT".
                10 FILLER PIC X(8) VALUE "SUPPEDIT".
                10 FILLER PIC X(8) VALUE "ADDREDIT".
                10 FILLER PIC X(8) VALUE "POEDIT  ".
           05  SUBR-NAME-TABLE REDEFINES SUBR-NAMES.
                10 SUBR-NAME OCCURS 4 TIMES PIC X(8).

       PROCEDURE DIVISION.
           PERFORM 100-HOUSEKEEPING.
           PERFORM 130-READ-TESTCASE-FILE.
          *> "Priming" read; if EOF, testcases file is empty.
          *> Not the end of the world, the header will identify
          *> That zero test cases were run.

           PERFORM 200-MAIN-PARTSUP
                UNTIL NO-MORE-TESTCASES.
           PERFORM 900-CLEANUP.
           GOBACK.

       100-HOUSEKEEPING.
      *     MOVE FUNCTION CURRENT-DATE TO HDG-DATE
           PERFORM 110-OPEN-FILES
           PERFORM 120-PRINT-HEADERS.

       110-OPEN-FILES.
           OPEN INPUT TESTCASE-FILE
           OPEN OUTPUT TEST-REPORT.

           OPEN INPUT FOUTPUT.

           OPEN INPUT FPARTS

           OPEN INPUT FSUPPS

           OPEN INPUT FADDRS

           OPEN INPUT FPO

           OPEN INPUT FERROR.

      *     OPEN INPUT FREPORT.


       120-PRINT-HEADERS.
           WRITE TEST-REPORT-BUF FROM TST-RPT-HDR1
           WRITE TEST-REPORT-BUF FROM TST-RPT-HDR2.

       130-READ-TESTCASE-FILE.
           READ TESTCASE-FILE
            AT END
               MOVE "Y" TO TESTCASE-EOF
           END-READ

           IF NOT NO-MORE-TESTCASES THEN
              ADD +1 TO TESTCASES-TOTAL
              MOVE 0 TO RECORD-TOT-EXP-FAILS
              PERFORM VARYING SUBR-IDX FROM 1 BY 1
                UNTIL SUBR-IDX > 4
                  IF EXPECTED-ERROR-COUNT(SUBR-IDX) > 4 THEN
                     MOVE 4 TO EXPECTED-ERROR-COUNT(SUBR-IDX)
                  END-IF
                  ADD EXPECTED-ERROR-COUNT(SUBR-IDX) TO
                       RECORD-TOT-EXP-FAILS
              END-PERFORM
              IF RECORD-TOT-EXP-FAILS > 4 THEN
                 MOVE 4 TO RECORD-TOT-EXP-FAILS
              END-IF
           END-IF.

       140-READ-ERROR-FILES.
           READ FERROR  *> PARTMAIN annotated PARTSUPP output
             AT END
                MOVE "Y" TO ERROR-FILES-EOF
           END-READ.

       150-READ-GOOD-FILES.
           READ FOUTPUT
           AT END
               MOVE "Y" TO GOOD-FILES-EOF
           END-READ  *> neglecting the condition of eof on individual
                     *> "downstream" output files
           READ FPARTS
           READ FSUPPS
           READ FADDRS
           READ FPO.

       200-MAIN-PARTSUP.
           IF RECORD-EXP-PASS THEN
      * If the testcase was expected to PASS for ALL subroutines, then
      * its data should be found in:
      *                the 4 collated output files,
      *                the PARTMAIN "good" PARTSUPPADDRPO file, and
      *                the report.
              PERFORM 250-TEST-GOOD-OUT
           ELSE
      * Otherwise (if the testcase is supposed to FAIL at least one),
      * then its data should be found in:
      *                the PARTMAIN "error" PARSUPPADDRPO file.
              PERFORM 260-TEST-ERROR-OUT
           END-IF
           PERFORM 130-READ-TESTCASE-FILE.

       250-TEST-GOOD-OUT.
           PERFORM 150-READ-GOOD-FILES
           IF NO-MORE-GOOD-FILES THEN
              MOVE "Unexpected EOF on GOOD PARTSUPP"
                        TO TST-RPT-FAIL-DESC
              PERFORM 400-WRITE-ERROR-DETAIL
           ELSE
              IF TC-PARTSUPP-BUF IS NOT EQUAL
                  PART-SUPP-ADDR-PO THEN
                 MOVE "Good record not found in FOUTPUT"
                        TO TST-RPT-FAIL-DESC
                 PERFORM 400-WRITE-ERROR-DETAIL
              END-IF
           END-IF.

       260-TEST-ERROR-OUT.
           PERFORM 140-READ-ERROR-FILES
           IF NO-MORE-ERROR-FILES THEN
              MOVE "Unexpected EOF on FERROR"
                        TO TST-RPT-FAIL-DESC
              PERFORM 400-WRITE-ERROR-DETAIL
           ELSE
              IF TC-PARTSUPP-BUF IS NOT EQUAL
                  OU-ERROUTPUT THEN
                 MOVE "Error record data not found in FERROR"
                        TO TST-RPT-FAIL-DESC
                 PERFORM 400-WRITE-ERROR-DETAIL
              END-IF
              IF REC-EXP-TOT-INVALID THEN *> Err count 4
                 IF OU-ERRMESSAGE IS NOT EQUAL
                   INVALID-RECORD-TXT
                   MOVE "Incorrect message for invalid record."
                         TO TST-RPT-FAIL-DESC
                   PERFORM 400-WRITE-ERROR-DETAIL
                 END-IF
              ELSE  *> 1-3 Errors should have meaningful messages
                 PERFORM 300-BUILD-ERROR-MSG
                 IF OU-ERRMESSAGE IS NOT EQUAL TST-ERRMESSAGE THEN
                    MOVE "Incorrect descriptive error messages:"
                         TO TST-RPT-FAIL-DESC
                    PERFORM 400-WRITE-ERROR-DETAIL
                    PERFORM 410-WRITE-ERROR-DETAIL-AMP
                 END-IF
              END-IF
           END-IF.

       300-BUILD-ERROR-MSG.
           MOVE 1 TO WS-ERROR-LCTR
           PERFORM VARYING SUBR-IDX FROM 1 BY 1 UNTIL SUBR-IDX > 5
           IF EXPECTED-ERROR-COUNT(SUBR-IDX) NOT = 0 THEN
               COMPUTE WS-ERROR-LENGTH =
                   EXPECTED-ERROR-COUNT(SUBR-IDX) * 30
               MOVE EXPECTED-RETURN-MESSAGE(SUBR-IDX)(1:WS-ERROR-LENGTH)
                   TO TST-ERRMESSAGE(WS-ERROR-LCTR:WS-ERROR-LENGTH)
               ADD WS-ERROR-LENGTH TO WS-ERROR-LCTR
           END-IF
           END-PERFORM
           IF WS-ERROR-LCTR < 91 THEN
              COMPUTE WS-ERROR-LENGTH =
                   (3 - RECORD-TOT-EXP-FAILS) * 30
               MOVE SPACES TO
                   TST-ERRMESSAGE(WS-ERROR-LCTR:WS-ERROR-LENGTH)
           END-IF.

       400-WRITE-ERROR-DETAIL.
           ADD +1 TO TESTCASES-FAILCOUNT
           MOVE TESTCASES-TOTAL TO TST-RPT-CASE-NUM
           WRITE TEST-REPORT-BUF FROM TST-RPT-FAIL-DTL
           MOVE SPACES TO TEST-REPORT-BUF
           WRITE TEST-REPORT-BUF.

       410-WRITE-ERROR-DETAIL-AMP.
           MOVE TST-ERRMESSAGE TO TST-RPT-EXP-MSG
           MOVE OU-ERRMESSAGE TO TST-RPT-ACT-MSG
           WRITE TEST-REPORT-BUF FROM TST-RPT-FAIL-MSG-DTL
           WRITE TEST-REPORT-BUF FROM TST-RPT-FAIL-MSG-DTL2.


       900-CLEANUP.
           PERFORM 910-PRINT-TRAILERS
           CLOSE TESTCASE-FILE, TEST-REPORT
           CLOSE FOUTPUT, FPARTS, FSUPPS, FADDRS, FPO, FERROR.

       910-PRINT-TRAILERS.
           MOVE SPACES TO TEST-REPORT-BUF
           WRITE TEST-REPORT-BUF
           MOVE TESTCASES-TOTAL TO TST-RPT-TTL-TC
           MOVE TESTCASES-FAILCOUNT TO TST-RPT-FAILED
           WRITE TEST-REPORT-BUF FROM TST-RPT-TLR.

