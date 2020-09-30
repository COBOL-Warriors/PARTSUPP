      *--------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *--------------------------------------------------------------
       PROGRAM-ID.    POEDIT.
       AUTHOR.        WARRIORS.
      *--------------------------------------------------------------
      ***************************************************************
      * Workshop:                    FINAL EXAM
      * Developer:                  stonehugh
      * Created:                    2020-09-08
      * Modified:
      * Modified:
      * Developer Contact:
      * V R M:                      V0R0M11
      *  Version Level
      *  Release Level
      *  Modification Level
      * SUB-PROGRAM Called from PARTMAIN
      * Evaluates the data that is passed defined below for errors and
      * then returns information back to PARTMAIN.
      * PARTMAIN passes.
      *    WS-POEDIT-RETURN-CODE      PIC 9.
      *    WS-POEDIT-RETURN-MESSAGE   PIC X(90)
      *    WS-POEDIT-ERROR-NUMBER     PIC 9(3)
      *    OU-PURCHASE-ORDER(1)
      *    OU-PURCHASE-ORDER(2)
      *    OU-PURCHASE-ORDER(3),
      *
      * POEDIT received
      *    WS-POEDIT-RETURN-CODE      PIC 9.
      *    WS-POEDIT-RETURN-MESSAGE   PIC X(90)
      *    WS-POEDIT-ERROR-NUMBER     PIC 9(3)
      *    OU-PURCHASE-ORDER(1) into  WS1-PURCHASE-ORDER
      *    OU-PURCHASE-ORDER(2) into  WS2-PURCHASE-ORDER
      *    OU-PURCHASE-ORDER(3) into  WS3-PURCHASE-ORDER
      *    Then moved the received WS* fields to WSX-PURCHASE-ORDER
      *    for processing.
      *
      **   998-PROGRAM-RETURN
      *         Will send message to the PARTMAIN.
      *    Notes: Wrote my own date logic could have used IBM CEEdate
      *           but was afraid we would loose access to the zserveros
      *           and new z/OS enviroment may not have it.
      ***************************************************************
      ***************************************************************
      * Modifications
      * Date       Developer Modification#
      *    Changes Below
      * -------------------------------------------------------------
      * 2020-09-11 stonehugh  V0R0M1
      *    Test Shell only.
      *    Modified the 400-VALID-MESSAGE-RETURN to always return
      *    WS-POEDIT-RETURN-CODE = 0
      *    Modified the WS-POEDIT-RETURN-MESSAGE to return error
      *    in the record format below.
      *       Index Number:Error Message 3 x 30 character strings moved
      *       into a 90 character field spaced evenly at 30 characters.
      *    Example
      *     1:Error PO-NUMBER Field     +
      *     2:Error DELIVERY-DATE Field +
      *     3:Valid Record              End of Record
      *
      * 2020-09-16 stonehugh  V0R0M3
      *   Added FD DEBUG-REC, and modified 9999-OUTPUT-DEBUG.
      *    to send the output to the DEBUG-REC so as not to clutter up
      *    the SYSOUT.
      *    JCL Output
      *    //RPTDEBUG DD SYSOUT=*
      *   Removed these two parts from 2020-09-11 Modification that
      *   created the Shell so testing could begin.
      *    Removed: Modified the 400-VALID-MESSAGE-RETURN
      *    to always return WS-POEDIT-RETURN-CODE = 0
      *
      * 2020-09-17 stonehugh  V0R0M4
      *  Refined the logic for the whole program.
      *  Added some additional tests on the dates and for the
      *   WSX-QUANTITY and WSX-UNIT-PRICE
      *  Added additional DISPLAY Statements to aid in testing.
      *
      * 2020-09-17 stonehugh  V0R0M5
      *  Fixed anomalous behavior in the return codes.
      * 2020-09-17 stonehugh  V0R0M6
      *  Added checks for .01 to $1,000.000.00 as valid $ range.
      *  Added check for 1 to 1,000.000 Quantity as valid range.
      *
      * 2020-09-17 stonehugh  V0R0M7 Updated in V0R08
      *  Fix for blank in the return MESSAGE-RETURN, initialized it
      *  with the value in 000-INITIALIZE instead of writing it later
      *  in the 400-VALID-MESSAGE-RETURN.
      *
      * 2020-09-18 stonehugh  V0R08
      *  Moved the INITIALIZE statements to the top of 000-INITIALIZE
      *    before any of the MOVE statement.
      *  Added 450-PROCESS-ERRORS.
      *  Code clean up making sure of terminations.
      *  Changed stonhugh to stonehugh in all of the modifications &
      *  Added additional comments and inline comments.
      *  Moved GOBACK from 1000-CLOSE-END to process last in the
      *  PROCEDURE DIVISION to get rid of the 0004 complile warning
      *  message.
      *
      * 2020-09-18 stonehugh  V0R09
      *  Added ERROR-INDEX-ACCUM to keep track of errors in each index
      *  Will now return up to 3 errors one for each of the indexes.
      *  Before this would return 1 error for each record in error.
      *  Total errors are not counted because a single bad date would
      *  give multiple errors.
      *
      * 2020-09-18 stonehugh  V0R0M10
      *  Rewrite to group by fields so that the error output matches
      *  the PARTEDIT.
      *
      * 2020-09-27 stonehugh  V0R0M10
      *  Changed UNIT-PRICE check to be 1 to 1,000,000.00 from
      *   .01 to 1,000,000.00
      *   Added 500-SUB-PRICE-CHECK-FLAG
      *   Added ZERO-QUANITY-TRUE
      *   Added 500-SUB-QUANTITY-PRICE-COMBO
      *   Added ERROR-QUANTITY-PRICE
      *
      * 2020-09-29 stonehugh  V0R0M11
      *   Added Logic to break out if both the price and the quantity
      *   are 0.
      *   Fixed bug in PERFORM 408-SUB-CLEAN-UP not getting invoked
      *    if the ZERO-QUANITY-TRUE = 'T' Causing ORDER-DATE Error.
      ***************************************************************

      *--------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *--------------------------------------------------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DEBUG-REC   ASSIGN TO RPTDEBUG.


      *--------------------------------------------------------------
       DATA DIVISION.
      *--------------------------------------------------------------
      *--------------------------------------------------------------
       FILE SECTION.
       FD DEBUG-REC
           RECORDING MODE F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 132 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS DEBUG-RECORD.
       01 DEBUG-RECORD                  PIC X(132)   VALUE SPACES.
      *--------------------------------------------------------------

      *--------------------------------------------------------------

      *--------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *--------------------------------------------------------------
      *Break Down the Input from PARTMAIN into individual fields
      *Matches the defined filelds in the COPYBOOK.
       01 WSX-PURCHASE-ORDER.
          05 WSX-PO-NUMBER              PIC X(06)    VALUE SPACES.
          05 WSX-BUYER-CODE             PIC X(03)    VALUE SPACES.
          05 WSX-QUANTITY               PIC S9(7)    VALUE +0.
          05 WSX-UNIT-PRICE             PIC S9(7)V99 VALUE +0.
          05 WSX-ORDER-DATE             PIC 9(08)    VALUE 0.
          05 WSX-DELIVERY-DATE          PIC 9(08)    VALUE 0.

      *Set the error messages these are set at 28 byes so that the
      *PROCESS-INDEX-NUM PIC 9(1) + : can be concatinated with them
      *for a total of 30 byes..
       01 ERROR-FIELD-MESSAGES.
          05 ERROR-PO-NUMBER            PIC X(28)    VALUE SPACES.
          05 ERROR-BUYER-CODE           PIC X(28)    VALUE SPACES.
          05 ERROR-QUANTITY             PIC X(28)    VALUE SPACES.
          05 ERROR-UNIT-PRICE           PIC X(28)    VALUE SPACES.
          05 ERROR-ORDER-DATE           PIC X(28)    VALUE SPACES.
          05 ERROR-DELIVERY-DATE        PIC X(28)    VALUE SPACES.
          05 ERROR-PO-MISSING           PIC X(28)    VALUE SPACES.
          05 PROCESS-INDEX-UNKNOWN      PIC X(28)    VALUE SPACES.
          05 ERROR-QUANTITY-PRICE       PIC X(28)    VALUE SPACES.

       *> Could be as many 7 FIELDS X 3 this will only hold the fields
       *> that are in error where multiple tests have to be preformed
       *> and the ERROR-PO-Missing for if missing in index 1 it is an
       *> error, but may be missing in index 2 and 3.
       *> When MOVED to MESSAGE-RETURN you will only get the first 3
       *> records.
       01 ERROR-GROUP.
          05 ERROR-MESSAGE-TABLE OCCURS 21 TIMES INDEXED BY MSG-IDX.
             10 ERROR-FIELD-MESSAGES-TABLE
                                        PIC X(30)    VALUE SPACES.

      *Set the valid messages. Does not need to get returned, for
      *debug and other output only.
       01 VALID-RECORD-MESSAGES.
          05 VALID-RECORD-GEN           PIC X(28)    VALUE SPACES.
       01 INVALID-RECORD-MESSAGES.
          05 INVALID-RECORD-GEN         PIC X(28)    VALUE SPACES.
       01 ERRORS-GT3                    PIC X(90)    VALUE SPACES.

      *Accumulators Counters
       01 PROCESS-INDEX-NUM             PIC 9(1)     VALUE ZERO.
       01 ERROR-FIELD-ACCUMULATOR       PIC 9(10)    VALUE ZERO.
       01 ERROR-ACCUMULATOR             PIC 9(10)    VALUE ZERO.

      *FLAGS
       01 ZERO-QUANITY-TRUE             PIC X(1)     VALUE SPACES.


      *Three Part Message Return if error. 90 total bytes, broken out
      *as three x 30 byte fields.
       01 MESSAGE-RETURN.
          05 MESSAGE-PART-ONE           PIC X(30)    VALUE SPACES.
          05 MESSAGE-PART-TWO           PIC X(30)    VALUE SPACES.
          05 MESSAGE-PART-THREE         PIC X(30)    VALUE SPACES.

      *Breaks down thed date for testing.
       01 TODAYS-DATE-BREAK-DOWN.
          05 LAST-YEAR                  PIC 9(4)     VALUE ZERO.
          05 TODAYS-DATE.
             10 TCCYY                   PIC 9(4)     VALUE ZERO.
             10 TMM                     PIC 9(2)     VALUE ZERO.
             10 TDD                     PIC 9(2)     VALUE ZERO.
       01 DATE-BREAK-DOWN.
          05 TEMP-DATE.
             10 CCYY                    PIC 9(4)     VALUE ZERO.
             10 MM                      PIC 9(2)     VALUE ZERO.
             10 DD                      PIC 9(2)     VALUE ZERO.

       01 LEAP-YEAR.
          05 LEAP-YEAR-VALUE            PIC X(1)     VALUE SPACES.
       01 MONTHS.
          05 MONTH-MAX                  PIC 9(2)     VALUE 12.
          05 MONTH-MIN                  PIC 9(2)     VALUE 1.

       01 DAYS.
          05 DAYS-31MAX                 PIC 9(2)     VALUE 31.
          05 DAYS-30MAX                 PIC 9(2)     VALUE 30.
          05 DAY-FEBMAX                 PIC 9(2)     VALUE 28.
          05 DAYS-MAX                   PIC 9(2)     VALUE ZERO.
          05 DAYS-MIN                   PIC 9(2)     VALUE 1.

       01 PO-IS-MISSING                 PIC 9(1)     VALUE 0.

      *END WORKING-STORAGE SECTION-----------------------------------

      *--------------------------------------------------------------
       LINKAGE SECTION.
      *--------------------------------------------------------------
       01 WS1-PURCHASE-ORDER            PIC X(41)    VALUE SPACES.
       01 WS2-PURCHASE-ORDER            PIC X(41)    VALUE SPACES.
       01 WS3-PURCHASE-ORDER            PIC X(41)    VALUE SPACES.

       01 WS-POEDIT-RETURN-CODE         PIC 9(1)     VALUE ZERO.
       01 WS-POEDIT-RETURN-MESSAGE      PIC X(90)    VALUE SPACES.
       01 WS-POEDIT-ERROR-NUMBER        PIC 9(3)     VALUE 0.


      *END LINKAGE SECTION-------------------------------------------

      *PROCEDURE DIVISION--------------------------------------------
          PROCEDURE DIVISION
           USING
               WS1-PURCHASE-ORDER
              , WS2-PURCHASE-ORDER
              , WS3-PURCHASE-ORDER
              , WS-POEDIT-RETURN-CODE
              , WS-POEDIT-RETURN-MESSAGE
              , WS-POEDIT-ERROR-NUMBER
              .

           PERFORM 000-INITIALIZE.
           PERFORM 100-OPEN-FILES.
           PERFORM 200-OUTPUT-INPUT.
           PERFORM 500-MAIN-PROCESS
           PERFORM 998-PROGRAM-RETURN.
           PERFORM 1000-CLOSE-END.
           GOBACK.

       000-INITIALIZE.
           *> Initialize first then move and populate fields.
           INITIALIZE ERROR-FIELD-ACCUMULATOR.
           INITIALIZE ERROR-ACCUMULATOR.
           INITIALIZE MESSAGE-RETURN.
           INITIALIZE PROCESS-INDEX-NUM.
           INITIALIZE PO-IS-MISSING.
           INITIALIZE ERROR-GROUP.


           MOVE 'Error PO-NUMBER Field '
              TO ERROR-PO-NUMBER.
           MOVE 'Error BUYER-CODE Field '
              TO ERROR-BUYER-CODE.
           MOVE 'Error QUANTITY Field '
              TO ERROR-QUANTITY.
           MOVE 'Error UNIT-PRICE Field '
              TO ERROR-UNIT-PRICE.
           MOVE 'Error ORDER-DATE Field '
              TO ERROR-ORDER-DATE.
           MOVE 'Error DELIVERY-DATE Field '
              TO ERROR-DELIVERY-DATE.
           MOVE 'Error PO Missing'
              TO ERROR-PO-MISSING.
           MOVE 'Error Process Index Unknown'
              TO PROCESS-INDEX-UNKNOWN.
           MOVE 'Valid Record '
              TO VALID-RECORD-GEN.
           MOVE 'Invalid Record '
              TO INVALID-RECORD-GEN.
           MOVE 'Error Count > 3'
              TO ERRORS-GT3
           MOVE 'F'
              TO ZERO-QUANITY-TRUE.
           MOVE 'Error QUANTITY & PRICE Combo'
              TO ERROR-QUANTITY-PRICE.

           MOVE FUNCTION CURRENT-DATE TO TODAYS-DATE.


       100-OPEN-FILES.
           OPEN OUTPUT DEBUG-REC.

       200-OUTPUT-INPUT.
           MOVE 'Passed to POEDIT*************************************'
              TO DEBUG-RECORD.
           WRITE DEBUG-RECORD.
           MOVE WS1-PURCHASE-ORDER TO DEBUG-RECORD.
           WRITE DEBUG-RECORD.
           MOVE SPACES TO DEBUG-RECORD.
           MOVE WS2-PURCHASE-ORDER TO DEBUG-RECORD.
           WRITE DEBUG-RECORD.
           MOVE SPACES TO DEBUG-RECORD.
           MOVE WS3-PURCHASE-ORDER TO DEBUG-RECORD.
           WRITE DEBUG-RECORD.
           MOVE SPACES TO DEBUG-RECORD.
           MOVE 'End of Passed to POEDIT*****************************'
              TO DEBUG-RECORD.
           WRITE DEBUG-RECORD.

       408-ERROR-MESSAGE-RETURN.
           ADD 1 TO ERROR-ACCUMULATOR.
           *> Dynamic Message INVALID-RECORD-GEN
           EVALUATE PROCESS-INDEX-NUM
           WHEN 1
                STRING PROCESS-INDEX-NUM
                   ':'
                   INVALID-RECORD-GEN
                   DELIMITED BY SIZE
                   INTO ERROR-MESSAGE-TABLE(ERROR-ACCUMULATOR)
           WHEN 2
                STRING PROCESS-INDEX-NUM
                   , ':'
                   , INVALID-RECORD-GEN
                   DELIMITED BY SIZE
                   INTO ERROR-MESSAGE-TABLE(ERROR-ACCUMULATOR)
           WHEN 3
                STRING PROCESS-INDEX-NUM
                   , ':'
                   , INVALID-RECORD-GEN
                   DELIMITED BY SIZE
                   INTO ERROR-MESSAGE-TABLE(ERROR-ACCUMULATOR)
           WHEN OTHER
                DISPLAY 'PROCESS-INDEX-NUM: ' PROCESS-INDEX-NUM
                DISPLAY 'INVALID-RECORD-GEN :' INVALID-RECORD-GEN
           END-EVALUATE
           .
           IF ZERO-QUANITY-TRUE = 'T'
              MOVE 'F' TO ZERO-QUANITY-TRUE
              PERFORM 408-SUB-CLEAN-UP
           ELSE
              PERFORM 408-SUB-CLEAN-UP
           END-IF
           .

       408-SUB-CLEAN-UP.
           DISPLAY 'ERROR-FIELD-ACCUMULATOR ' ERROR-FIELD-ACCUMULATOR
           MOVE 0 TO ERROR-FIELD-ACCUMULATOR.


       500-MAIN-PROCESS.
           *>Index 1
           *>Move in the information.
           MOVE WS1-PURCHASE-ORDER TO WSX-PURCHASE-ORDER.
           PERFORM 500-SUB-CHECK-MISSING-PO.

           *>Index 2
           *>Move in the information.
           MOVE WS2-PURCHASE-ORDER TO WSX-PURCHASE-ORDER.
           PERFORM 500-SUB-CHECK-MISSING-PO.

           *>Index 3
           *>Move in the information.
           MOVE WS3-PURCHASE-ORDER TO WSX-PURCHASE-ORDER.
           PERFORM 500-SUB-CHECK-MISSING-PO.

       500-SUB-CHECK-MISSING-PO.
           MOVE 0 TO PO-IS-MISSING
           ADD 1 TO PROCESS-INDEX-NUM
           PERFORM 500-SUB-WSX-PURCHASE-ORDER
           IF PO-IS-MISSING = 0
              PERFORM 500-SUB-PROC
           END-IF
           .
       500-SUB-PROC.
           PERFORM 500-SUB-WSX-PO-NUMBER.
           PERFORM 500-SUB-WSX-BUYER-CODE.
           PERFORM 500-SUB-WSX-QUANTITY.
           PERFORM 500-SUB-WSX-UNIT-PRICE.
           PERFORM 500-SUB-WSX-ORDER-DATE.
           PERFORM 500-SUB-WSX-DELIVERY-DATE.

       500-SUB-WSX-PURCHASE-ORDER.
           IF PROCESS-INDEX-NUM = 1 *> Not an error if 2 or 3 missing.
              IF WSX-PURCHASE-ORDER = SPACES
                 MOVE ERROR-PO-MISSING TO INVALID-RECORD-GEN
                 PERFORM 408-ERROR-MESSAGE-RETURN
                 MOVE 1 TO PO-IS-MISSING
                 DISPLAY PROCESS-INDEX-NUM 'BLANK WSX-PURCHASE-ORDER'
              END-IF
           END-IF
           .
           IF PROCESS-INDEX-NUM > 1 *> 2 or 3
              IF WSX-PURCHASE-ORDER = SPACES
                 MOVE 1 TO PO-IS-MISSING
                 DISPLAY PROCESS-INDEX-NUM 'BLANK WSX-PURCHASE-ORDER'
              END-IF
           END-IF
           . *> End 500-SUB-WSX-PURCHASE-ORDER

       500-SUB-WSX-PO-NUMBER.
           IF WSX-PO-NUMBER = SPACES
              ADD 1 TO ERROR-FIELD-ACCUMULATOR
              DISPLAY PROCESS-INDEX-NUM 'BLANK WSX-PO-NUMBER'
           END-IF
           .
           *>Be sure PERFORM 408 is the last one in each 500-SUB.
           IF ERROR-FIELD-ACCUMULATOR >= 1
              MOVE ERROR-PO-NUMBER TO INVALID-RECORD-GEN
              PERFORM 408-ERROR-MESSAGE-RETURN
           END-IF
           . *> End 500-SUB-WSX-PO-NUMBER

       500-SUB-WSX-BUYER-CODE.
           IF WSX-BUYER-CODE = SPACES
              ADD 1 TO ERROR-FIELD-ACCUMULATOR
              DISPLAY PROCESS-INDEX-NUM 'BLANK WSX-BUYER-CODE'
           END-IF
           .
           *>Be sure PERFORM 408 is the last one in each 500-SUB.
           IF ERROR-FIELD-ACCUMULATOR >= 1
              MOVE ERROR-BUYER-CODE TO INVALID-RECORD-GEN
              PERFORM 408-ERROR-MESSAGE-RETURN

           END-IF
           . *> End 500-SUB-WSX-BUYER-CODE

       500-SUB-PRICE-CHECK-FLAG.
           MOVE 'T' TO ZERO-QUANITY-TRUE.
           DISPLAY PROCESS-INDEX-NUM
                   'SET ZERO-QUANITY-TRUE TO '
                   ZERO-QUANITY-TRUE.

       500-SUB-QUANTITY-PRICE-COMBO.
           MOVE ERROR-QUANTITY-PRICE TO INVALID-RECORD-GEN
           PERFORM 408-ERROR-MESSAGE-RETURN.
           DISPLAY PROCESS-INDEX-NUM
                   'WROTE ERROR FOR INVALID QUANTIY PRICE COMBO'.
           DISPLAY PROCESS-INDEX-NUM
                   ' WSX-UNIT-PRICE '
                   WSX-UNIT-PRICE
                   ' WSX-QUANTITY '
                   WSX-QUANTITY.

       500-SUB-WSX-QUANTITY.
           IF WSX-QUANTITY = SPACES
              ADD 1 TO ERROR-FIELD-ACCUMULATOR
              DISPLAY PROCESS-INDEX-NUM 'BLANK WSX-QUANTITY'
           END-IF
           .
           IF WSX-QUANTITY IS NOT NUMERIC
              ADD 1 TO ERROR-FIELD-ACCUMULATOR
              DISPLAY PROCESS-INDEX-NUM 'NOT NUMBER ''WSX-QUANTITY'
           END-IF
           .
           IF WSX-QUANTITY IS NUMERIC AND WSX-UNIT-PRICE IS NUMERIC
              IF WSX-QUANTITY = 0 AND WSX-UNIT-PRICE = 0
                 DISPLAY PROCESS-INDEX-NUM
                         'WSX-QUANTITY = 0 and WSX-UNIT-PRICE = 0'
                 DISPLAY 'NO Further Processing Needed for QUANTITY'
              ELSE
                 IF WSX-QUANTITY IS NUMERIC
                    IF WSX-QUANTITY <= 0
                       PERFORM 500-SUB-PRICE-CHECK-FLAG
                       DISPLAY PROCESS-INDEX-NUM 'WSX-QUANTITY <= 0'
                    END-IF
                 END-IF

                 IF WSX-QUANTITY IS NUMERIC
                    IF WSX-QUANTITY > 1000000
                       ADD 1 TO ERROR-FIELD-ACCUMULATOR
                       DISPLAY PROCESS-INDEX-NUM
                               'WSX-QUANTITY > 1,000,000'
                    END-IF
                 END-IF
              END-IF
           END-IF
           .
           *>Be sure PERFORM 408 is the last one in each 500-SUB.
           IF ERROR-FIELD-ACCUMULATOR >= 1
              MOVE ERROR-QUANTITY TO INVALID-RECORD-GEN
              PERFORM 408-ERROR-MESSAGE-RETURN
           END-IF
           . *>End 500-SUB-WSX-QUANTITY

       500-SUB-WSX-UNIT-PRICE.
           IF WSX-UNIT-PRICE IS NOT NUMERIC
              ADD 1 TO ERROR-FIELD-ACCUMULATOR
              DISPLAY PROCESS-INDEX-NUM 'NOT NUMBER ''WSX-UNIT-PRICE'
           END-IF
           .
           IF WSX-QUANTITY IS NUMERIC AND WSX-UNIT-PRICE IS NUMERIC
              IF WSX-QUANTITY = 0 AND WSX-UNIT-PRICE = 0
                 DISPLAY PROCESS-INDEX-NUM
                         'WSX-QUANTITY = 0 and WSX-UNIT-PRICE = 0'
                 DISPLAY 'NO Further Processing Needed for UNIT-PRICE'
              ELSE
                 IF WSX-UNIT-PRICE IS NUMERIC
                    IF WSX-UNIT-PRICE < 1
                       ADD 1 TO ERROR-FIELD-ACCUMULATOR
                       DISPLAY PROCESS-INDEX-NUM 'WSX-UNIT-PRICE < 1.00'
                    END-IF
                 END-IF

                 IF WSX-UNIT-PRICE IS NUMERIC
                    IF WSX-UNIT-PRICE > 0 AND ZERO-QUANITY-TRUE = 'T'
                       PERFORM 500-SUB-QUANTITY-PRICE-COMBO
                       DISPLAY PROCESS-INDEX-NUM


                          'WSX-UNIT-PRICE > 0 WHEN WSX-QUANTITY <= 0'
                    END-IF
                 END-IF

                 IF WSX-UNIT-PRICE IS NUMERIC
                    IF WSX-UNIT-PRICE > 1000000.00
                       ADD 1 TO ERROR-FIELD-ACCUMULATOR
                       DISPLAY PROCESS-INDEX-NUM
                               'WSX-UNIT-PRICE > 1,000,000.00'
                    END-IF
                 END-IF
              END-IF
           END-IF
           .
            *>Be sure PERFORM 408 is the last one in each 500-SUB.
           IF ERROR-FIELD-ACCUMULATOR >= 1
              MOVE ERROR-UNIT-PRICE TO INVALID-RECORD-GEN
              PERFORM 408-ERROR-MESSAGE-RETURN
           END-IF

           . *>END 500-SUB-WSX-UNIT-PRICE

       500-SUB-WSX-ORDER-DATE.
           MOVE WSX-ORDER-DATE TO TEMP-DATE.
           PERFORM 500-SUB-EVALUATE-LEAP-YEAR.
           PERFORM 500-SUB-EVALUATE-DAYS-IN-MONTH.
           IF WSX-ORDER-DATE = SPACES
              ADD 1 TO ERROR-FIELD-ACCUMULATOR
              DISPLAY PROCESS-INDEX-NUM 'WSX-ORDER-DATE'
           END-IF
           .
           IF WSX-ORDER-DATE IS NOT NUMERIC
              ADD 1 TO ERROR-FIELD-ACCUMULATOR
              DISPLAY PROCESS-INDEX-NUM 'NOT NUMBER ''WSX-ORDER-DATE'
           END-IF
           .
          *> If the year is older than this year or last, should check
          *> this out. Example 2019 and 2020 would be valid in 2020.
           IF LAST-YEAR IS NUMERIC
              IF TCCYY IS NUMERIC
                 COMPUTE LAST-YEAR =(TCCYY - 1)
              END-IF
           END-IF
           .
           IF TEMP-DATE > TODAYS-DATE
              ADD 1 TO ERROR-FIELD-ACCUMULATOR
              DISPLAY PROCESS-INDEX-NUM
                      'ORDER TEMP-DATE out of range'
                      TEMP-DATE
           END-IF
           .
           IF CCYY > TCCYY
              OR CCYY < LAST-YEAR
              ADD 1 TO ERROR-FIELD-ACCUMULATOR
              DISPLAY PROCESS-INDEX-NUM
                      'ORDER CCYY out of range'
              DISPLAY CCYY '|' TCCYY '|' LAST-YEAR
           END-IF
           .
           IF DD GREATER THAN DAYS-MAX OR DD LESS THAN DAYS-MIN
              ADD 1 TO ERROR-FIELD-ACCUMULATOR
              DISPLAY PROCESS-INDEX-NUM 'ORDER DD Out of range ' DD
           END-IF
           .

           IF MM GREATER THAN MONTH-MAX OR MM LESS THAN DAYS-MIN
              ADD 1 TO ERROR-FIELD-ACCUMULATOR
              DISPLAY PROCESS-INDEX-NUM 'ORDER MM Out of range ' MM
           END-IF
           .
            *>Be sure PERFORM 408 is the last one in each 500-SUB.
           IF ERROR-FIELD-ACCUMULATOR >= 1
              MOVE ERROR-ORDER-DATE TO INVALID-RECORD-GEN
              PERFORM 408-ERROR-MESSAGE-RETURN
           END-IF
           . *>End 500-SUB-WSX-ORDER-DATE

       500-SUB-WSX-DELIVERY-DATE.
           MOVE WSX-DELIVERY-DATE TO TEMP-DATE.
           IF WSX-DELIVERY-DATE NOT EQUAL TO SPACES
              PERFORM 500-SUB-EVALUATE-LEAP-YEAR
              PERFORM 500-SUB-EVALUATE-DAYS-IN-MONTH
              IF WSX-DELIVERY-DATE IS NOT NUMERIC
                 ADD 1 TO ERROR-FIELD-ACCUMULATOR
                 DISPLAY PROCESS-INDEX-NUM
                         'NOT A NUMBER '
                         'WSX-DELIVERY-DATE'
              END-IF

              IF LAST-YEAR IS NUMERIC
                 IF TCCYY IS NUMERIC
                    COMPUTE LAST-YEAR =(TCCYY - 1)
                 END-IF
              END-IF

              IF TEMP-DATE <= WSX-ORDER-DATE
                 ADD 1 TO ERROR-FIELD-ACCUMULATOR
                 DISPLAY PROCESS-INDEX-NUM
                         'DELIVERY-DATE Must be later than ORDER-DATE.'
                         TEMP-DATE
              END-IF

              IF CCYY > TCCYY
                 OR CCYY < LAST-YEAR
                 ADD 1 TO ERROR-FIELD-ACCUMULATOR
                 DISPLAY PROCESS-INDEX-NUM 'DELIVERY CCYY out of range.'
                 DISPLAY CCYY '|' TCCYY '|' LAST-YEAR
              END-IF

              IF DD GREATER THAN DAYS-MAX OR DD LESS THAN DAYS-MIN
                 ADD 1 TO ERROR-FIELD-ACCUMULATOR
                 DISPLAY PROCESS-INDEX-NUM
                         'DELIVERY DD Out of range '
                         DD
              END-IF

              IF MM GREATER THAN MONTH-MAX OR MM LESS THAN MONTH-MIN
                 ADD 1 TO ERROR-FIELD-ACCUMULATOR
                 DISPLAY PROCESS-INDEX-NUM
                         'DELIVERY MM Out of range '
                         MM
              END-IF

           *>Be sure PERFORM 408 is the last one in each 500-SUB.
              IF ERROR-FIELD-ACCUMULATOR >= 1
                 MOVE ERROR-DELIVERY-DATE TO INVALID-RECORD-GEN
                 PERFORM 408-ERROR-MESSAGE-RETURN
              END-IF
           ELSE
              DISPLAY WSX-DELIVERY-DATE
                      'WSX-DELIVERY-DATE BLANK NOT CHECKING FURTHER'
           END-IF
           . *> End 500-SUB-WSX-DELIVERY-DATE

       500-SUB-EVALUATE-LEAP-YEAR.
           EVALUATE TRUE
           WHEN FUNCTION MOD(CCYY 4) NOT ZERO
           WHEN FUNCTION MOD(CCYY 100) ZERO
              AND FUNCTION MOD(CCYY 400) NOT ZERO
                MOVE 'Y' TO LEAP-YEAR-VALUE
                MOVE 28 TO DAY-FEBMAX
           WHEN OTHER
                MOVE 'N' TO LEAP-YEAR-VALUE
                MOVE 29 TO DAY-FEBMAX
           END-EVALUATE
           .

       500-SUB-EVALUATE-DAYS-IN-MONTH.
           EVALUATE MM
           WHEN 1
                MOVE DAYS-31MAX TO DAYS-MAX
           WHEN 2
                MOVE DAY-FEBMAX TO DAYS-MAX
           WHEN 3
                MOVE DAYS-31MAX TO DAYS-MAX
           WHEN 4
                MOVE DAYS-30MAX TO DAYS-MAX
           WHEN 5
                MOVE DAYS-31MAX TO DAYS-MAX
           WHEN 6
                MOVE DAYS-30MAX TO DAYS-MAX
           WHEN 7
                MOVE DAYS-31MAX TO DAYS-MAX
           WHEN 8
                MOVE DAYS-31MAX TO DAYS-MAX
           WHEN 9
                MOVE DAYS-30MAX TO DAYS-MAX
           WHEN 10
                MOVE DAYS-31MAX TO DAYS-MAX
           WHEN 11
                MOVE DAYS-30MAX TO DAYS-MAX
           WHEN 12
                MOVE DAYS-31MAX TO DAYS-MAX
           END-EVALUATE
           .

       998-PROGRAM-RETURN.
           IF ERROR-ACCUMULATOR >= 1
              MOVE 8 TO WS-POEDIT-RETURN-CODE
              MOVE ERROR-GROUP TO MESSAGE-RETURN
              MOVE ERROR-ACCUMULATOR TO WS-POEDIT-ERROR-NUMBER
              *> Maximum of 4 errors returned, if over 4 errors
              *> POMAIN will use a generic message in log file.
              *> Next line will overwrite WS-POEDIT-ERROR-NUMBER
              *> if needed.
              IF ERROR-ACCUMULATOR > 3
                 MOVE 4 TO WS-POEDIT-ERROR-NUMBER
                 MOVE ERRORS-GT3 TO MESSAGE-RETURN
              END-IF
           ELSE
              MOVE 0 TO WS-POEDIT-RETURN-CODE
              MOVE 0 TO WS-POEDIT-ERROR-NUMBER
              MOVE VALID-RECORD-GEN TO MESSAGE-RETURN
           END-IF
           .
           MOVE MESSAGE-RETURN
              TO WS-POEDIT-RETURN-MESSAGE.
           DISPLAY '**********************************************'.
           DISPLAY 'ERROR-ACCUMULATOR ' ERROR-ACCUMULATOR.
           DISPLAY '**********************************************'.
           DISPLAY 'WS-POEDIT-RETURN-CODE ' WS-POEDIT-RETURN-CODE.
           DISPLAY 'WS-POEDIT-RETURN-MESSAGE '
                   WS-POEDIT-RETURN-MESSAGE.
           DISPLAY 'WS-POEDIT-ERROR-NUMBER ' WS-POEDIT-ERROR-NUMBER.
           DISPLAY '**********************************************'.
           INITIALIZE ERROR-GROUP.

       1000-CLOSE-END.
           CLOSE DEBUG-REC.

      *END PROCEDURE DIVISION----------------------------------------
