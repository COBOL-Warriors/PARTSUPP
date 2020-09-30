       IDENTIFICATION DIVISION.
       PROGRAM-ID.      UNITTEST.
       AUTHOR.          WARRIORS.

      *****************************************************************
      * ***** This program is a driver for automated unit testing of
      * ***** the subroutines of the PARTSUPP program called by
      * ***** PARTMAIN.
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
      * ***** The returned values are compared with those expected, and
      * ***** discrepancies are printed out.
      * ***** At EOF, counts of test cases failed are
      * ***** printed for each subroutine, along with the total cases.
      * Developer:                  Dave
      * Created:                    2020-09-12
      * Modified:
      *  2020-09-14  Plugged in functioning PARTEDIT subr       dgp
      *  2020-09-16  Plugged in functioning POEDIT   subr       dgp
      *  2020-09-22  Plugged in functioning SUPPEDIT subr       dgp
      *  2020-09-25  Plugged in functioning ADDREDIT subr       dgp
      *****************************************************************

      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TESTCASE-FILE ASSIGN TO UTSTCASE.
           SELECT TEST-REPORT ASSIGN TO UTSTRPT.

       DATA DIVISION.
       FILE SECTION.

       FD  TESTCASE-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 1006 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS TESTCASE-PARTSUPP-BUF.

       01  TESTCASE-PARTSUPP-BUF  PIC X(1006).

       FD  TEST-REPORT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 132 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS TEST-REPORT-BUF.

       01  TEST-REPORT-BUF  PIC X(132).

       WORKING-STORAGE SECTION.

       77 WS-STORAGE-IND                PIC X(60)
              VALUE 'WORKING STORAGE BEGINS HERE'.

       01  TESTCASE-PARTSUPP-REC.
           05 PARTS.
               10  PART-NUMBER       PIC X(23) VALUE SPACES.
               10  PART-NAME         PIC X(14) VALUE SPACES.
               10  SPEC-NUMBER       PIC X(07) VALUE SPACES.
               10  GOVT-COMML-CODE   PIC X(01) VALUE SPACES.
               10  BLUEPRINT-NUMBER  PIC X(10) VALUE SPACES.
               10  UNIT-OF-MEASURE   PIC X(03) VALUE SPACES.
               10  WEEKS-LEAD-TIME   PIC 9(03) VALUE ZERO.
               10  VEHICLE-MAKE      PIC X(03) VALUE SPACES.
                    88 CHRYSLER       VALUE 'CHR'.
                    88 FORD           VALUE 'FOR'.
                    88 GM             VALUE 'GM '.
                    88 VOLKSWAGON     VALUE 'VW '.
                    88 TOYOTA         VALUE 'TOY'.
                    88 JAGUAR         VALUE 'JAG'.
                    88 PEUGEOT        VALUE 'PEU'.
                    88 BMW            VALUE 'BMW'.
               10  VEHICLE-MODEL     PIC X(10) VALUE SPACES.
               10  VEHICLE-YEAR      PIC X(04) VALUE '0000'.
               10  FILLER            PIC X(14) VALUE SPACES.
           05 SUPPLIERS.
               10  SUPPLIER-CODE     PIC X(10) VALUE SPACES.
               10  SUPPLIER-TYPE     PIC X(01) VALUE SPACES.
                    88 SUBCONTRACTOR  VALUE 'S'.
                    88 DISTRIBUTOR    VALUE 'D'.
                    88 MANUFACTURER   VALUE 'M'.
                    88 IMPORTER       VALUE 'I'.
               10  SUPPLIER-NAME     PIC X(15) VALUE SPACES.
               10  SUPPLIER-PERF     PIC 9(03) VALUE ZERO.
               10  SUPPLIER-RATING   PIC X(01) VALUE SPACES.
                    88 HIGHEST-QUALITY VALUE '3'.
                    88 AVERAGE-QUALITY VALUE '2'.
                    88 LOWEST-QUALITY  VALUE '1'.
               10  SUPPLIER-STATUS   PIC X(01) VALUE SPACES.
                    88 GOVT-COMM       VALUE '1'.
                    88 GOVT-ONLY       VALUE '2'.
                    88 COMMERCIAL-ONLY VALUE '3'.
               10  SUPPLIER-ACT-DATE PIC 9(08) VALUE ZERO.
           05 SUPP-ADDRESS OCCURS 3 TIMES INDEXED BY ADDR-IDX.
               10 ADDRESS-TYPE      PIC X(01) VALUE SPACES.
                  88 ORDER-ADDRESS           VALUE '1'.
                  88 SCHED-ADDRESS           VALUE '2'.
                  88 REMIT-ADDRESS           VALUE '3'.
               10 ADDRESS-1         PIC X(15) VALUE SPACES.
               10 ADDRESS-2         PIC X(15) VALUE SPACES.
               10 ADDRESS-3         PIC X(15) VALUE SPACES.
               10 CITY              PIC X(15) VALUE SPACES.
               10 ADDR-STATE        PIC X(02) VALUE SPACES.
               10 ZIP-CODE          PIC 9(10) VALUE ZERO.
           05 PURCHASE-ORDER OCCURS 3 TIMES INDEXED BY PO-IDX.
               10  PO-NUMBER         PIC X(06) VALUE SPACES.
               10  BUYER-CODE        PIC X(03) VALUE SPACES.
               10  QUANTITY          PIC S9(7) VALUE ZERO.
               10  UNIT-PRICE        PIC S9(7)V99 VALUE ZERO.
               10  ORDER-DATE        PIC 9(08) VALUE ZERO.
               10  DELIVERY-DATE     PIC 9(08) VALUE ZERO.
           05 EXPECTED-RESULTS OCCURS 4 TIMES.
               10 EXPECTED-RETURN-CODE    PIC 9.
               10 EXPECTED-ERROR-COUNT    PIC 9(3).
               10 EXPECTED-RETURN-MESSAGE PIC X(90).

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

       01  TST-RPT-FAIL-HDR2.
           05  FILLER         PIC X(22) VALUE "Expected: Message Text".
           05  FILLER         PIC X(70) VALUE SPACES.
           05  FILLER         PIC X(11) VALUE "Return Code".
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(11) VALUE "Error Count".
           05  FILLER         PIC X(16) VALUE SPACES.

       01  TST-RPT-FAIL-HDR-LINES.
           05  FILLER         PIC X(90) VALUE ALL '='.
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(11) VALUE ALL '='.
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(11) VALUE ALL '='.
           05  FILLER         PIC X(16) VALUE SPACES.

       01  TST-RPT-FAIL-DTL1.
           05  TST-RPT-EXP-MSG   PIC X(90) VALUE SPACES.
           05  FILLER            PIC X(05) VALUE SPACES.
           05  TST-RPT-EXP-RC    PIC 9(01) VALUE ZERO.
           05  FILLER            PIC X(10) VALUE SPACES.
           05  TST-RPT-EXP-NERRS PIC 9(03) VALUE ZERO.

       01  TST-RPT-FAIL-HDR3.
           05  FILLER         PIC X(22) VALUE "Actual: Message Text  ".
           05  FILLER         PIC X(70) VALUE SPACES.
           05  FILLER         PIC X(11) VALUE "Return Code".
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(11) VALUE "Error Count".
           05  FILLER         PIC X(16) VALUE SPACES.

       01  TST-RPT-FAIL-DTL2.
           05  TST-RPT-ACT-MSG   PIC X(90) VALUE SPACES.
           05  FILLER            PIC X(05) VALUE SPACES.
           05  TST-RPT-ACT-RC    PIC 9(01) VALUE ZERO.
           05  FILLER            PIC X(10) VALUE SPACES.
           05  TST-RPT-ACT-NERRS PIC 9(03) VALUE ZERO.

       01  TST-RPT-TLR.
           05  FILLER          PIC X(22) VALUE "Total test cases run: ".
           05  TST-RPT-TTL-TC  PIC Z(05)9 VALUE ZERO.
           05  FILLER          PIC X(02) VALUE SPACES.
           05  FILLER          PIC X(14) VALUE "Cases failed: ".
           05  FILLER          PIC X(12) VALUE "  PARTEDIT: ".
           05  TST-RPT-FAIL-PARTEDIT PIC Z(05)9 VALUE ZERO.
           05  FILLER          PIC X(12) VALUE "  SUPPEDIT: ".
           05  TST-RPT-FAIL-SUPPEDIT PIC Z(05)9 VALUE ZERO.
           05  FILLER          PIC X(12) VALUE "  ADDREDIT: ".
           05  TST-RPT-FAIL-ADDREDIT PIC Z(05)9 VALUE ZERO.
           05  FILLER          PIC X(12) VALUE "    POEDIT: ".
           05  TST-RPT-FAIL-POEDIT PIC Z(05)9 VALUE ZERO.

      * FIELDS FOR CALLING SUBROUTINES
      * The same storages are used for all four subroutines.
      * This greatly simplifies evaluation of the returns, and
      * error processing.
       01 CALL-RETURN-CODE.
           05 WS-SUBR-RETURN-CODE       PIC 9 VALUE 0.
           05 WS-SUBR-RETURN-MESSAGE    PIC X(90) VALUE SPACES.
           05 WS-SUBR-ERROR-COUNT       PIC 9(3) VALUE 0.

       01  FLAGS-AND-ACCUMULATORS.
           05  TESTCASES-TOTAL          PIC 9(6) COMP VALUE 0.
           05  TESTCASE-EOF             PIC X VALUE SPACES.
                 88 NO-MORE-TESTCASES  VALUE 'Y'.
           05  CLEAN-RUN-SOFAR          PIC X VALUE 'Y'.
                 88 CLEAN-RUN VALUE 'Y'.
           05  SUBR-IDX                 PIC 9 VALUE 0.
           05  TESTCASES-PASSED OCCURS 4 TIMES PIC 9(6) VALUE 0.
           05  TESTCASES-FAILED OCCURS 4 TIMES PIC 9(6) VALUE 0.
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
          *> Not the end of the world, the trailer will identify
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

       120-PRINT-HEADERS.
           WRITE TEST-REPORT-BUF FROM TST-RPT-HDR1
           WRITE TEST-REPORT-BUF FROM TST-RPT-HDR2.

       130-READ-TESTCASE-FILE.
           READ TESTCASE-FILE INTO TESTCASE-PARTSUPP-REC
            AT END
               MOVE "Y" TO TESTCASE-EOF
           END-READ.

       200-MAIN-PARTSUP.
           ADD +1 TO TESTCASES-TOTAL.
           INITIALIZE WS-SUBR-ERROR-COUNT.
           PERFORM 220-PART-PROCESS
           PERFORM 230-SUPP-PROCESS
           PERFORM 240-ADDR-PROCESS
           PERFORM 250-PO-PROCESS
           PERFORM 130-READ-TESTCASE-FILE.

       220-PART-PROCESS.
           MOVE 1 TO SUBR-IDX
           INITIALIZE WS-SUBR-ERROR-COUNT.
           CALL "PARTEDIT" USING PART-NUMBER,
                               PART-NAME,
                               WEEKS-LEAD-TIME,
                               VEHICLE-MODEL,
                               VEHICLE-MAKE,
                               VEHICLE-YEAR,
                               WS-SUBR-RETURN-CODE,
                               WS-SUBR-RETURN-MESSAGE,
                               WS-SUBR-ERROR-COUNT
           PERFORM 300-ASSESS-RESULTS.


       230-SUPP-PROCESS.
           MOVE 2 TO SUBR-IDX
           INITIALIZE WS-SUBR-ERROR-COUNT.
           CALL "SUPPEDIT" USING SUPPLIER-CODE,
                               SUPPLIER-TYPE,
                               SUPPLIER-NAME,
                               SUPPLIER-PERF,
                               SUPPLIER-RATING,
                               SUPPLIER-STATUS,
                               SUPPLIER-ACT-DATE,
                               WS-SUBR-RETURN-CODE,
                               WS-SUBR-RETURN-MESSAGE,
                               WS-SUBR-ERROR-COUNT

           PERFORM 300-ASSESS-RESULTS.


       240-ADDR-PROCESS.
           MOVE 3 TO SUBR-IDX
           INITIALIZE WS-SUBR-ERROR-COUNT.
           CALL "ADDREDIT" USING SUPP-ADDRESS(1),
                               SUPP-ADDRESS(2),
                               SUPP-ADDRESS(3),
                               WS-SUBR-RETURN-CODE,
                               WS-SUBR-RETURN-MESSAGE,
                               WS-SUBR-ERROR-COUNT
           PERFORM 300-ASSESS-RESULTS.

       250-PO-PROCESS.
           MOVE 4 TO SUBR-IDX
           INITIALIZE WS-SUBR-ERROR-COUNT.
           CALL "POEDIT" USING PURCHASE-ORDER(1),
                              PURCHASE-ORDER(2),
                              PURCHASE-ORDER(3),
                              WS-SUBR-RETURN-CODE,
                             WS-SUBR-RETURN-MESSAGE,
                             WS-SUBR-ERROR-COUNT

           PERFORM 300-ASSESS-RESULTS.

       300-ASSESS-RESULTS.
           MOVE 'Y' TO CLEAN-RUN-SOFAR
           *> Using this flag avoids a really long IF
           IF WS-SUBR-RETURN-CODE IS NOT =
                   EXPECTED-RETURN-CODE(SUBR-IDX) THEN
              MOVE 'N' TO CLEAN-RUN-SOFAR
           END-IF
           IF WS-SUBR-RETURN-MESSAGE IS NOT =
                    EXPECTED-RETURN-MESSAGE(SUBR-IDX) THEN
              MOVE 'N' TO CLEAN-RUN-SOFAR
           END-IF
           IF WS-SUBR-ERROR-COUNT IS NOT =
                    EXPECTED-ERROR-COUNT(SUBR-IDX) THEN
              MOVE 'N' TO CLEAN-RUN-SOFAR
           END-IF
           IF CLEAN-RUN THEN
                ADD +1 TO TESTCASES-PASSED(SUBR-IDX)
             ELSE
                ADD +1 TO TESTCASES-FAILED(SUBR-IDX)
                MOVE SUBR-NAME(SUBR-IDX) TO TST-RPT-FAIL-SUBR
                PERFORM 400-WRITE-ERROR-DETAIL
           END-IF.

       400-WRITE-ERROR-DETAIL.
           MOVE TESTCASES-TOTAL TO TST-RPT-TCASE-NO
           WRITE TEST-REPORT-BUF FROM TST-RPT-FAIL-HDR
      * Show the input that caused the error
           MOVE SPACES TO FAILED-INPUT
           EVALUATE SUBR-IDX
               WHEN 1
                  MOVE PARTS TO FAILED-INPUT
              WHEN 2
                  MOVE SUPPLIERS TO FAILED-INPUT
              WHEN 3
                  MOVE SUPP-ADDRESS(1) TO FAILED-INPUT
                  WRITE TEST-REPORT-BUF FROM TST-RPT-FAIL-INPUT
                  MOVE SPACES TO FAILED-INPUT
                  MOVE SUPP-ADDRESS(2) TO FAILED-INPUT
                  WRITE TEST-REPORT-BUF FROM TST-RPT-FAIL-INPUT
                  MOVE SPACES TO FAILED-INPUT
                  MOVE SUPP-ADDRESS(3) TO FAILED-INPUT
              WHEN 4
                  MOVE PURCHASE-ORDER(1) TO FAILED-INPUT
                  WRITE TEST-REPORT-BUF FROM TST-RPT-FAIL-INPUT
                  MOVE SPACES TO FAILED-INPUT
                  MOVE PURCHASE-ORDER(2) TO FAILED-INPUT
                  WRITE TEST-REPORT-BUF FROM TST-RPT-FAIL-INPUT
                  MOVE SPACES TO FAILED-INPUT
                  MOVE PURCHASE-ORDER(3) TO FAILED-INPUT
           END-EVALUATE
           WRITE TEST-REPORT-BUF FROM TST-RPT-FAIL-INPUT
           WRITE TEST-REPORT-BUF FROM TST-RPT-FAIL-HDR2
           WRITE TEST-REPORT-BUF FROM TST-RPT-FAIL-HDR-LINES
      * Show the subroutine output we actually expected
           MOVE EXPECTED-RETURN-MESSAGE(SUBR-IDX) TO TST-RPT-EXP-MSG
           MOVE EXPECTED-RETURN-CODE(SUBR-IDX) TO TST-RPT-EXP-RC
           MOVE EXPECTED-ERROR-COUNT(SUBR-IDX) TO TST-RPT-EXP-NERRS
           WRITE TEST-REPORT-BUF FROM TST-RPT-FAIL-DTL1
           WRITE TEST-REPORT-BUF FROM TST-RPT-FAIL-HDR3
           WRITE TEST-REPORT-BUF FROM TST-RPT-FAIL-HDR-LINES
           MOVE WS-SUBR-RETURN-MESSAGE TO TST-RPT-ACT-MSG
           MOVE WS-SUBR-RETURN-CODE TO TST-RPT-ACT-RC
           MOVE WS-SUBR-ERROR-COUNT TO TST-RPT-ACT-NERRS
           WRITE TEST-REPORT-BUF FROM TST-RPT-FAIL-DTL2
           MOVE SPACES TO TEST-REPORT-BUF
           WRITE TEST-REPORT-BUF.

       900-CLEANUP.
           PERFORM 910-PRINT-TRAILERS.
           CLOSE TESTCASE-FILE, TEST-REPORT.

       910-PRINT-TRAILERS.
           MOVE SPACES TO TEST-REPORT-BUF.
           WRITE TEST-REPORT-BUF.
           MOVE TESTCASES-TOTAL TO TST-RPT-TTL-TC.
           MOVE TESTCASES-FAILED(1) TO
                TST-RPT-FAIL-PARTEDIT.
           MOVE TESTCASES-FAILED(2) TO
                TST-RPT-FAIL-SUPPEDIT.
           MOVE TESTCASES-FAILED(3) TO
                TST-RPT-FAIL-ADDREDIT.
           MOVE TESTCASES-FAILED(4) TO
                TST-RPT-FAIL-POEDIT.
           WRITE TEST-REPORT-BUF FROM TST-RPT-TLR.

