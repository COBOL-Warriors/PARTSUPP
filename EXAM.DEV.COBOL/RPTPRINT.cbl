      *--------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *--------------------------------------------------------------
       PROGRAM-ID.    RPTPRINT.
       AUTHOR.        WARRIORS.
      *--------------------------------------------------------------
      ***************************************************************
      ***************************************************************
      * Workshop:                   FINAL EXAM
      * Developer:                  stonehugh
      * Created:                    2020-09-14
      * Modified:
      * Modified:
      * Developer Contact:
      * V R M:                      V0R0M7
      *  Version Level
      *  Release Level
      *  Modification Level
      ***************************************************************
      ***************************************************************
      * Modifications
      * 2020-09-11 stonehugh  V0R0M2
      *   JCL Input
      * //RPTINPUT DD DSN=USER66.EXAM.DEV.RPTPRINT.SORTED,DISP=SHR
      *   JCL Output
      *        RPTPRINT DD SYSOUT=*
      *   JCL Output
      *
      * 2020-09-16 stonehugh  V0R0M3
      *   Added FD DEBUG-REC, and modified 9999-OUTPUT-DEBUG.
      *    to send the output to the DEBUG-REC so as not to clutter up
      *    the SYSOUT.
      *    JCL Output
      *    //RPTDEBUG DD SYSOUT=*
      *
      * 2020-09-21 stonehugh  V0R0M4
      *   Increased the size of the 4 Variables below to accommodate
      *    up to 1 billion from 1 million, and adjusted the filler
      *     to maintain alignment.
      *     TOT-QUANTITY-PO-CALC
      *     TOT-QUANTITY-PO-FMT-OUT
      *     TOT-NUM-PO-CALC
      *     TOT-NUM-PO-FMAT-OUT
      *
      * 2020-09-22 stonehugh V0R0M5
      *   Added City State and Zip to the Address Line.
      *
      * 2020-09-29 stonehugh V0R0M6
      *   Changed code so that if WSX-SUPP-ADDRESS(1:1) is SPACE
      *   To give the not supplied error return instead of Bad Address
      *   type.
      *
      * 2020-09-30 stonehugh V0R0M7
      *   Added logic to trim the - if the +4 part is blank.
      *
      ***************************************************************

      *--------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *--------------------------------------------------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-RECORD ASSIGN TO RPTINPUT.
           SELECT PRINT-REC   ASSIGN TO RPTPRINT.
           SELECT DEBUG-REC   ASSIGN TO RPTDEBUG.
      *--------------------------------------------------------------
       DATA DIVISION.
      *--------------------------------------------------------------
       FILE SECTION.

       FD  INPUT-RECORD.
            COPY 'PARTSUPP'.

       FD PRINT-REC
           RECORDING MODE F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 132 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS PRINT-RECORD.
       01 PRINT-RECORD                  PIC X(132)     VALUE SPACES.

       FD DEBUG-REC
           RECORDING MODE F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 132 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS DEBUG-RECORD.
       01 DEBUG-RECORD                  PIC X(132)     VALUE SPACES.



      *--------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *--------------------------------------------------------------

      *Headers/Trailers and Formated Output**************************
      * Three rows of headings.
      *    PAGE-BREAK page numbers at the page breaks.
      *    RP-HEADER-ONE report page field layout.
      *    RP-HEADER-TWO report page field layout.
      *    000-BUILD-HEADER-VAL report values for the RP-HEADER-ONE.
      *    Values are static for RP-HEADER-TWO
      ***************************************************************
       01 RP-HEADER-ONE.
          05 PARTS-HEADER-ONE.
             10 FILLER                  PIC X(3)       VALUE SPACES.
             *> Max Length 14 buffer 1 before and 1 after to center.
             10 PART-NAME-ONE           PIC X(16)      VALUE SPACES.
             10 FILLER                  PIC X(3)       VALUE SPACES.
             *> Max Length 16 buffer 1 before and 1 after to center.
             10 WEEKS-LEAD-TIME-ONE     PIC X(18)      VALUE SPACES.
             10 FILLER                  PIC X(3)       VALUE SPACES.
             *> Max Length 10 buffer 1 before and 1 after to center.
             10 VEHICLE-MAKE-ONE        PIC X(12)      VALUE SPACES.
             10 FILLER                  PIC X(3)       VALUE SPACES.
          05 SUPPLIERS-HEADER-ONE.
             10 FILLER                  PIC X(3)       VALUE SPACES.
             *> Max Length 15 buffer 1 before and 1 after to center.
             10 SUPPLIER-NAME-ONE       PIC X(17)      VALUE SPACES.
             10 FILLER                  PIC X(3)       VALUE SPACES.
             *> Max Length 15 buffer 1 before and 1 after to center.
             10 SUPPLIER-RATING-ONE     PIC X(17)      VALUE SPACES.
             10 FILLER                  PIC X(3)       VALUE SPACES.

       01 RP-HEADER-TWO.
          05 PARTS-HEADER-TWO.
             10 FILLER                  PIC X(3)       VALUE SPACES.
             *> Max Length 14 buffer 1 before and 1 after to center.
             10 PART-NAME-TWO           PIC X(16)      VALUE ALL '='.
             10 FILLER                  PIC X(3)       VALUE SPACES.
             *> Max Length 16 buffer 1 before and 1 after to center.
             10 WEEKS-LEAD-TIME-TWO     PIC X(18)      VALUE ALL '='.
             10 FILLER                  PIC X(3)       VALUE SPACES.
             *> Max Length 10 buffer 1 before and 1 after to center.
             10 VEHICLE-MAKE-TWO        PIC X(12)      VALUE ALL '='.
             10 FILLER                  PIC X(3)       VALUE SPACES.
          05 SUPPLIERS-HEADER-TWO.
             10 FILLER                  PIC X(3)       VALUE SPACES.
             *> Max Length 15 buffer 1 before and 1 after to center.
             10 SUPPLIER-NAME-TWO       PIC X(17)      VALUE ALL '='.
             10 FILLER                  PIC X(3)       VALUE SPACES.
             *> Max Length 15 buffer 1 before and 1 after to center.
             10 SUPPLIER-RATING-TWO     PIC X(17)      VALUE ALL '='.
             10 FILLER                  PIC X(3)       VALUE SPACES.

       01 REPORT-FORMATED-OUT.
          05 PARTS-FORMATED.
             10 FILLER                  PIC X(3)       VALUE SPACES.
             *> Print Length 14 buffer before and after in the Filler.
             10 PART-NAME-FORMATED      PIC X(14)      VALUE SPACES.
             10 FILLER                  PIC X(11)      VALUE SPACES.
             *> Print Length 16 buffer before and after in the Filler.
             10 WEEKS-LEAD-TIME-FORMATED
                                        PIC ZZ9.
             10 FILLER                  PIC X(13)      VALUE SPACES.
             *> Print Length 10 buffer before and after in the Filler.
             10 VEHICLE-MAKE-FORMATED   PIC X(10)      VALUE SPACES.
             10 FILLER                  PIC X(3)       VALUE SPACES.
          05 SUPPLIERS-FORMATED.
             10 FILLER                  PIC X(5)       VALUE SPACES.
             *> Print Length 15 buffer before and after in the Filler.
             10 SUPPLIER-NAME-FORMATED  PIC X(17)      VALUE SPACES.
             10 FILLER                  PIC X(3)       VALUE SPACES.
             *> Print Length 15 buffer before and after in the Filler.
             10 SUPPLIER-RATING-FORMATED
                                        PIC X(15)      VALUE SPACES.
             10 FILLER                  PIC X(3)       VALUE SPACES.

       01 PAGE-BREAK.
          05 PAGE-NUMBER                PIC 9(3)       VALUE 0.

       01 PAGE-BREAK-FORMATED.
          05 FILLER                     PIC X(1)       VALUE SPACE.
          05 REPORT-DATE                PIC XXXX/XX/XX.
          05 FILLER                     PIC X(45)      VALUE SPACES.
          05 REPORT-LABEL               PIC X(20)      VALUE SPACES.
          05 FILLER                     PIC X(45)      VALUE SPACES.
          05 PAGE-NUMBER-LABEL          PIC X(5)       VALUE SPACES.
          05 PAGE-NUMBER-FORMATED       PIC ZZZ.


      *Addresses for Report******************************************
      * Order Address:
      * Sched Address:
      * Remit Address:
      *
      ***************************************************************
       01 WSX-SUPP-ADDRESS.
          05 WSX-ADDRESS-TYPE           PIC X(01)      VALUE SPACES.
          05 WSX-ADDRESS-1              PIC X(15)      VALUE SPACES.
          05 WSX-ADDRESS-2              PIC X(15)      VALUE SPACES.
          05 WSX-ADDRESS-3              PIC X(15)      VALUE SPACES.
          05 WSX-CITY                   PIC X(15)      VALUE SPACES.
          05 WSX-ADDR-STATE             PIC X(02)      VALUE SPACES.
          05 WSX-ZIP-CODE               PIC 9(10)      VALUE 0.

       01 REPORT-ADDRESS-FORMATED-OUT.
          05 IN-ERROR-ADDRESS           PIC X(80)      VALUE SPACES.
          05 IN-ERROR-ADDRESS-TYPE      PIC X(15)      VALUE SPACES.
          05 TALLY-COUNTER              PIC 9(15)      VALUE 0.
          05 IN-ADDRESS1                PIC X(80)      VALUE SPACES.
          05 IN-ADDRESS2                PIC X(80)      VALUE SPACES.
          05 IN-ADDRESS3                PIC X(80)      VALUE SPACES.
          05 PRT-ORDER-ADDRESS-OUT.
             10 FILLER                  PIC X(1)       VALUE SPACES.
             10 ORDER-ADDRESS-LABEL-OUT PIC X(15)      VALUE SPACES.
             10 FILLER                  PIC X(1)       VALUE SPACES.
             10 ORDER-ADDRESS-OUT       PIC X(80)      VALUE SPACES.
          05 PRT-SCHED-ADDRESS-OUT.
             10 FILLER                  PIC X(1)       VALUE SPACES.
             10 SCHED-ADDRESS-LABEL-OUT PIC X(15)      VALUE SPACES.
             10 FILLER                  PIC X(1)       VALUE SPACES.
             10 SCHED-ADDRESS-OUT       PIC X(80)      VALUE SPACES.
          05 PRT-REMIT-ADDRESS-OUT.
             10 FILLER                  PIC X(1)       VALUE SPACES.
             10 REMIT-ADDRESS-LABEL-OUT PIC X(15)      VALUE SPACES.
             10 FILLER                  PIC X(1)       VALUE SPACES.
             10 REMIT-ADDRESS-OUT       PIC X(80)      VALUE SPACES.

      *Misc FIELDS***************************************************
      *
      *
      ***************************************************************
       01 PART-COMPARE-FIELDS.
          05 PART-NUMBER-FORMATED       PIC X(23)      VALUE SPACES.
          05 PART-NUMBER-LAST-FORMATED  PIC X(23)      VALUE SPACES.

      *Report Format
       01 BLANK-LINE                    PIC X(132)     VALUE SPACES.

      *Flags
       01 RPT-PARTSUP-EOF               PIC X(1)       VALUE SPACE.
       01 INDEX-VALUE-ADDR              PIC 9(1)       VALUE 0.
       01 INDEX-VALUE-PO                PIC 9(1)       VALUE 0.

      *Delimiters
       01 DELIMITER-VALUE.
          05 DASH-DELIM                 PIC X(1)       VALUE SPACE.
          05 FSLASH-DELIM               PIC X(1)       VALUE SPACE.
          05 PIPE-DELIM                 PIC X(1)       VALUE SPACE.


      *Totals for Report*********************************************
      *   The fields used for the calculting first, then are moved
      *    for formated output before writing to report.
      *      TOT-NUM-PO-CALC MOVED TO TOT-NUM-PO-FMAT-OUT
      *      TOT-QUANTITY-PO-CALC MOVED TO TOT-QUANTITY-PO-FMT-OUT
      *      TOT-PRICE-PO-CALC MOVED TO TOT-PRICE-PO-FMT-OUT
      *   000-BUILD-HEADER-VAL report sets the values for the.
      *      TOT-NUM-PO-LBL-OUT
      *      TOT-QUANTITY-PO-LBL-OUT
      *      TOT-PRICE-PO-LBL-OUT
      *  OUTPUT will be the 05 record. Label and the formated numbers.
      *       TOTAL-NUMBER-PO-OUT
      *       TOTAL-QUANTITY-PO-OUT
      *       TOTAL-PRICE-PO-OUT
      ***************************************************************
       01 WSX-PURCHASE-ORDER.
          05 WSX-PO-NUMBER              PIC X(06)      VALUE SPACES.
          05 WSX-BUYER-CODE             PIC X(03)      VALUE SPACES.
          05 WSX-QUANTITY               PIC S9(7)      VALUE +0.
          05 WSX-UNIT-PRICE             PIC S9(7)V99   VALUE +0.
          05 WSX-ORDER-DATE             PIC 9(08)      VALUE 0.
          05 WSX-DELIVERY-DATE          PIC 9(08)      VALUE 0.

       01 RPT-COUNTERS.
          05 TOTAL-NUMBER-PO-CALC.
             10 TOT-NUM-PO-CALC         PIC 9(5)       VALUE 0.
          05 TOTAL-NUMBER-PO-OUT.
             10 FILLER                  PIC X(1)       VALUE SPACE.
             10 TOT-NUM-PO-LBL-OUT      PIC X(35)      VALUE SPACES.
             *>  Filler evens out the output with RPT-PRICE.
             10 FILLER                  PIC X(10)      VALUE SPACES.
             10 TOT-NUM-PO-FMAT-OUT     PIC ZZZZ9.

       01 RPT-QUANTITY.
          05 TOTAL-QUANTITY-PO-CALC.
             10 TOT-QUANTITY-PO-CALC    PIC 9(10)      VALUE 0.
          05 TOTAL-QUANTITY-PO-OUT.
             10 FILLER                  PIC X(1)       VALUE SPACE.
             10 TOT-QUANTITY-PO-LBL-OUT PIC X(35)      VALUE SPACES.
             *>  Filler evends out the output with RPT-PRICE.
             10 FILLER                  PIC X(5)       VALUE SPACES.
             10 TOT-QUANTITY-PO-FMT-OUT PIC ZZZZZZZZZ9.

       01 IN-PRICE.
          05 IN-COST.  *>  Quantity x Price Accumulated Here.
             10 IN-COST-PO1             PIC S9(7)V99   VALUE 0.
             10 IN-COST-PO2             PIC S9(7)V99   VALUE 0.
             10 IN-COST-PO3             PIC S9(7)V99   VALUE 0.

       01 RPT-PRICE.
          05 TOTAL-PRICE-PO-CALC.
             10 PRICE-PO-CALC           PIC 9(09)V99   VALUE 0.
             10 TOT-PRICE-PO-CALC       PIC 9(10)V99   VALUE 0.
          05 TOTAL-PRICE-PO-OUT.
             10 FILLER                  PIC X(1)       VALUE SPACE.
             10 TOT-PRICE-PO-LBL-OUT    PIC X(35)      VALUE SPACES.
             10 TOT-PRICE-PO-FMT-OUT    PIC $$$$,$$$,$$9.99.
      *--------------------------------------------------------------
       PROCEDURE DIVISION.
      *--------------------------------------------------------------
           PERFORM 000-INIT.
           PERFORM 100-OPEN-FILES.
           PERFORM 500-BUILD-REPORT UNTIL RPT-PARTSUP-EOF = 'Y'.
           PERFORM 1000-CLOSE-END.
           GOBACK.

       000-INIT.
           INITIALIZE RP-HEADER-ONE.
           INITIALIZE RP-HEADER-TWO.
           INITIALIZE REPORT-FORMATED-OUT.
           INITIALIZE PAGE-BREAK.
           INITIALIZE PAGE-BREAK-FORMATED.
           INITIALIZE REPORT-ADDRESS-FORMATED-OUT.
           INITIALIZE PART-COMPARE-FIELDS.
           INITIALIZE BLANK-LINE.
           INITIALIZE RPT-PARTSUP-EOF.
           INITIALIZE INDEX-VALUE-ADDR.
           INITIALIZE INDEX-VALUE-PO.
           INITIALIZE DELIMITER-VALUE.
           INITIALIZE RPT-COUNTERS.
           INITIALIZE RPT-QUANTITY.
           INITIALIZE IN-PRICE.

           MOVE '-' TO DASH-DELIM.
           MOVE '/' TO FSLASH-DELIM
           MOVE '|' TO PIPE-DELIM

           MOVE 'N' TO RPT-PARTSUP-EOF.
           MOVE FUNCTION CURRENT-DATE TO REPORT-DATE.
           INSPECT REPORT-DATE REPLACING ALL
              FSLASH-DELIM BY DASH-DELIM
           PERFORM 000-BUILD-HEADER-VAL.

       000-BUILD-HEADER-VAL.
           MOVE '   Part Name    '
              TO PART-NAME-ONE.
           MOVE ' Weeks Lead Time  '
              TO WEEKS-LEAD-TIME-ONE.                   *> X(18)
           MOVE 'Vehicle Make'
              TO VEHICLE-MAKE-ONE.
           MOVE '  Supplier Name  '
              TO SUPPLIER-NAME-ONE.
           MOVE ' Supplier Rating '
              TO SUPPLIER-RATING-ONE.
           MOVE 'Total # Purchase Orders:'              *> X(35)
              TO TOT-NUM-PO-LBL-OUT.
           MOVE 'Total Quantity in Purchase Orders:'    *> X(35)
              TO TOT-QUANTITY-PO-LBL-OUT.
           MOVE 'Total Price Purchase Orders:'          *> X(35)
              TO TOT-PRICE-PO-LBL-OUT.
           MOVE 'Page:'                                 *> X(5)
              TO PAGE-NUMBER-LABEL.
           MOVE 'Order Address:'                        *> X(15)
              TO ORDER-ADDRESS-LABEL-OUT.
           MOVE 'Sched Address:'                        *> X(15)
              TO SCHED-ADDRESS-LABEL-OUT.
           MOVE 'Remit Address:'                        *> X(15)
              TO REMIT-ADDRESS-LABEL-OUT.
           MOVE 'Not Supplied'                          *> X(15
              TO IN-ERROR-ADDRESS.
           MOVE 'Bad Add Type = '                       *> X(15)
              TO IN-ERROR-ADDRESS-TYPE.
           MOVE '   Invoice Report    '                 *> X(20)
              TO REPORT-LABEL.

       100-OPEN-FILES.
           OPEN INPUT INPUT-RECORD.
           OPEN OUTPUT PRINT-REC.
           OPEN OUTPUT DEBUG-REC.

       200-SET-WRITE-HEADER.
           MOVE RP-HEADER-ONE TO PRINT-RECORD.
           PERFORM 300-WRITE-PRINT-RECORD.
           MOVE RP-HEADER-TWO TO PRINT-RECORD.
           PERFORM 300-WRITE-PRINT-RECORD.

       300-WRITE-PRINT-RECORD.
           WRITE PRINT-RECORD.

       300-PAGE-BREAK.
           ADD 1 TO PAGE-NUMBER.
           MOVE TOT-NUM-PO-CALC TO TOT-NUM-PO-FMAT-OUT.
           MOVE TOT-QUANTITY-PO-CALC TO TOT-QUANTITY-PO-FMT-OUT.

           MOVE PAGE-NUMBER TO PAGE-NUMBER-FORMATED.
           MOVE PAGE-BREAK-FORMATED TO PRINT-RECORD.
           PERFORM 300-WRITE-PRINT-RECORD.
           PERFORM 300-WRITE-BLANK-LINE.

       300-WRITE-BLANK-LINE.
           MOVE BLANK-LINE TO PRINT-RECORD.
           PERFORM 300-WRITE-PRINT-RECORD.

       500-BUILD-REPORT.
           READ INPUT-RECORD
           AT END
              MOVE 'Y' TO RPT-PARTSUP-EOF
           END-READ
           .

           IF RPT-PARTSUP-EOF = 'N'
           *> Check First Two Fields to see if they are blank.
              IF PART-NUMBER IN INPUT-RECORD NOT = SPACES
                 MOVE PART-NUMBER IN INPUT-RECORD
                    TO PART-NUMBER-FORMATED
              ELSE
                 PERFORM 9999-OUTPUT-DEBUG
              END-IF

              IF PART-NAME IN INPUT-RECORD NOT = SPACES
                 MOVE PART-NAME IN INPUT-RECORD
                    TO PART-NAME-FORMATED
              ELSE
                 PERFORM 9999-OUTPUT-DEBUG
              END-IF

              MOVE WEEKS-LEAD-TIME IN INPUT-RECORD
                 TO WEEKS-LEAD-TIME-FORMATED

              PERFORM 500-SUB-EVALUATE-VEHICLE-MAKE

              MOVE SUPPLIER-NAME IN INPUT-RECORD
                 TO SUPPLIER-NAME-FORMATED

              PERFORM 500-SUB-EVAL-SUPPLIER-RATING

              IF
                 PART-NUMBER-FORMATED NOT EQUAL TO
                 PART-NUMBER-LAST-FORMATED
                 PERFORM 300-PAGE-BREAK
                 PERFORM 200-SET-WRITE-HEADER
                 MOVE REPORT-FORMATED-OUT TO PRINT-RECORD
                 PERFORM 300-WRITE-PRINT-RECORD
                 PERFORM 500-SUB-EVALUATE-ADDRESS
                 PERFORM 500-SUB-COMPUTE-PO-COSTS
                 MOVE PART-NUMBER-FORMATED
                    TO PART-NUMBER-LAST-FORMATED
                 PERFORM 300-WRITE-BLANK-LINE
                 ADD 1 TO TOT-NUM-PO-CALC
              ELSE
                 PERFORM 9999-OUTPUT-DEBUG
              END-IF
           END-IF
           .
           IF RPT-PARTSUP-EOF = 'Y'
              MOVE '   Report Total     '
                 TO REPORT-LABEL
              PERFORM 300-PAGE-BREAK
              PERFORM 300-WRITE-BLANK-LINE
              MOVE TOTAL-NUMBER-PO-OUT TO PRINT-RECORD
              PERFORM 300-WRITE-PRINT-RECORD
              MOVE TOTAL-QUANTITY-PO-OUT TO PRINT-RECORD
              PERFORM 300-WRITE-PRINT-RECORD
              MOVE TOTAL-PRICE-PO-OUT TO PRINT-RECORD
              PERFORM 300-WRITE-PRINT-RECORD

           END-IF
           .

       500-SUB-EVALUATE-VEHICLE-MAKE.
           EVALUATE
              VEHICLE-MAKE IN INPUT-RECORD
           WHEN 'CHR'
                MOVE 'CHRYSLER'
                   TO VEHICLE-MAKE-FORMATED
           WHEN 'FOR'
                MOVE 'FORD '
                   TO VEHICLE-MAKE-FORMATED
           WHEN 'GM '
                MOVE 'GM'
                   TO VEHICLE-MAKE-FORMATED
           WHEN 'VW '
                MOVE 'VOLKSWAGON'
                   TO VEHICLE-MAKE-FORMATED
           WHEN 'TOY'
                MOVE 'TOYOTA'
                   TO VEHICLE-MAKE-FORMATED
           WHEN 'JAG'
                MOVE 'JAGUAR '
                   TO VEHICLE-MAKE-FORMATED
           WHEN 'PEU'
                MOVE 'PEUGEOT'
                   TO VEHICLE-MAKE-FORMATED
           WHEN 'BMW'
                MOVE 'BMW'
                   TO VEHICLE-MAKE-FORMATED
           WHEN OTHER
                MOVE 'Unknown'
                   TO VEHICLE-MAKE-FORMATED
           END-EVALUATE
           .

       500-SUB-EVAL-SUPPLIER-RATING.
           EVALUATE
              SUPPLIER-RATING IN INPUT-RECORD
           WHEN '3'
                MOVE 'Highest Quality'
                   TO SUPPLIER-RATING-FORMATED
           WHEN '2'
                MOVE 'Average Quality'
                   TO SUPPLIER-RATING-FORMATED
           WHEN '1'
                MOVE 'Lowest Quality'
                   TO SUPPLIER-RATING-FORMATED
           WHEN OTHER
                MOVE 'Unknown Quality'
                   TO SUPPLIER-RATING-FORMATED
           END-EVALUATE
           .

       500-SUB-EVALUATE-ADDRESS.
           PERFORM VARYING ADDR-IDX FROM 1 BY 1 UNTIL ADDR-IDX > 3
                   MOVE SUPP-ADDRESS(ADDR-IDX)
                      TO WSX-SUPP-ADDRESS
                   ADD 1 TO INDEX-VALUE-ADDR
                   PERFORM 500-SUB-SUB-EVALUATE-ADDRESS
           END-PERFORM
           .

           PERFORM 300-WRITE-BLANK-LINE.
           PERFORM 300-WRITE-BLANK-LINE.
           MOVE PRT-ORDER-ADDRESS-OUT TO PRINT-RECORD.
           PERFORM 300-WRITE-PRINT-RECORD.
           MOVE PRT-SCHED-ADDRESS-OUT TO PRINT-RECORD.
           PERFORM 300-WRITE-PRINT-RECORD.
           MOVE PRT-REMIT-ADDRESS-OUT TO PRINT-RECORD.
           PERFORM 300-WRITE-PRINT-RECORD.
           PERFORM 500-CLEAN-UP.

       500-SUB-SUB-EVALUATE-ADDRESS.
           *> In each pass the fields will be updated.
           *> If they are blank, in order to make them blank if
           *> they do not have legit data need to clear each of them
           *> Did add some error checking. Should not be needed as
           *> the sub routines should catch it, but it helped to have it
           *> durring the initial testing.
           IF ORDER-ADDRESS-OUT = IN-ERROR-ADDRESS
              MOVE SPACES TO ORDER-ADDRESS-OUT
           END-IF
           .
           IF SCHED-ADDRESS-OUT = IN-ERROR-ADDRESS
              MOVE SPACES TO SCHED-ADDRESS-OUT
           END-IF
           .
           IF REMIT-ADDRESS-OUT = IN-ERROR-ADDRESS
              MOVE SPACES TO REMIT-ADDRESS-OUT
           END-IF
           .
           *> In order to maintain the spaces in the addresses, all
           *> spaces were replaced by pipe in each record. Then because
           *> when you have a double pipe it means that you have hit the
           *> next field. So in the String it delimits the fields based
           *> on it having a double pipe. Then use INSPECT again to
           *> clean up the pipe and replace it with a space for the final
           *> output. (Not the most elegant). Future project write a
           *> called sub to do string cleaning.
           INSPECT WSX-ADDRESS-1 REPLACING ALL SPACE BY PIPE-DELIM
           INSPECT WSX-ADDRESS-2 REPLACING ALL SPACE BY PIPE-DELIM
           INSPECT WSX-ADDRESS-3 REPLACING ALL SPACE BY PIPE-DELIM
           INSPECT WSX-CITY REPLACING ALL SPACE BY PIPE-DELIM
           INSPECT WSX-ADDR-STATE REPLACING ALL SPACE BY PIPE-DELIM
           INSPECT WSX-ZIP-CODE REPLACING ALL SPACE BY PIPE-DELIM

           EVALUATE
              WSX-SUPP-ADDRESS(1:1)
           WHEN '1' *>  Order Address
                STRING
                   WSX-ADDRESS-1 DELIMITED BY '||'
                   ,
                   SPACE DELIMITED BY '||'
                   ,
                   WSX-ADDRESS-2 DELIMITED BY '||'
                   ,
                   SPACE DELIMITED BY '||'
                   ,
                   WSX-ADDRESS-3 DELIMITED BY '||'
                   ,
                   SPACE DELIMITED BY '||'
                   ,
                   WSX-CITY DELIMITED BY '||'
                   ,
                   ',' DELIMITED BY '||'
                   ,
                   WSX-ADDR-STATE DELIMITED BY '||'
                   ,
                   SPACE DELIMITED BY '||'
                   ,
                   WSX-ZIP-CODE(1:5) DELIMITED BY '||'
                   ,
                   '-' DELIMITED BY '||'
                   ,
                   WSX-ZIP-CODE(6:4) DELIMITED BY '||'
                   INTO IN-ADDRESS1

           WHEN '2' *>  Sched Address.
                STRING
                   WSX-ADDRESS-1 DELIMITED BY '||'

                   SPACE DELIMITED BY '||'

                   WSX-ADDRESS-2 DELIMITED BY '||'
                   ,
                   SPACE DELIMITED BY '||'
                   ,
                   WSX-ADDRESS-3 DELIMITED BY '||'
                   ,
                   SPACE DELIMITED BY '||'
                   ,
                   WSX-CITY DELIMITED BY '||'
                   ,
                   ',' DELIMITED BY '||'
                   ,
                   WSX-ADDR-STATE DELIMITED BY '||'
                   ,
                   SPACE DELIMITED BY '||'
                   ,
                   WSX-ZIP-CODE(1:5) DELIMITED BY '||'
                   ,
                   '-' DELIMITED BY '||'
                   ,
                   WSX-ZIP-CODE(6:4) DELIMITED BY '||'
                   INTO IN-ADDRESS2

           WHEN '3' *>  Remit Address
                STRING
                   WSX-ADDRESS-1 DELIMITED BY '||'
                   ,
                   SPACE DELIMITED BY '||'
                   ,
                   WSX-ADDRESS-2 DELIMITED BY '||'
                   ,
                   SPACE DELIMITED BY '||'
                   ,
                   WSX-ADDRESS-3 DELIMITED BY '||'
                   ,
                   SPACE DELIMITED BY '||'
                   ,
                   WSX-CITY DELIMITED BY '||'
                   ,
                   ',' DELIMITED BY '||'
                   ,
                   WSX-ADDR-STATE DELIMITED BY '||'
                   ,
                   SPACE DELIMITED BY '||'
                   ,
                   WSX-ZIP-CODE(1:5) DELIMITED BY '||'
                   ,
                   '-' DELIMITED BY '||'
                   ,
                   WSX-ZIP-CODE(6:4) DELIMITED BY '||'
                   INTO IN-ADDRESS3


           WHEN OTHER *>  Invalid Address Type
                IF WSX-SUPP-ADDRESS(1:1) = SPACE
                   DISPLAY 'BLANK ADDRESS TYPE IN WSX-SUPP-ADDRESS(1:1)'
                ELSE
                   IF INDEX-VALUE-ADDR = 1
                      STRING IN-ERROR-ADDRESS-TYPE
                         , WSX-SUPP-ADDRESS(1:1)
                         DELIMITED BY SIZE INTO IN-ADDRESS1
                   END-IF
                   IF INDEX-VALUE-ADDR = 2
                      STRING IN-ERROR-ADDRESS-TYPE
                         , WSX-SUPP-ADDRESS(1:1)
                         DELIMITED BY SIZE INTO IN-ADDRESS2
                   END-IF
                   IF INDEX-VALUE-ADDR = 3
                      STRING IN-ERROR-ADDRESS-TYPE
                         , WSX-SUPP-ADDRESS(1:1)
                         DELIMITED BY SIZE INTO IN-ADDRESS3
                   END-IF
                END-IF
           END-EVALUATE
           .

           INSPECT IN-ADDRESS1
              REPLACING ALL PIPE-DELIM BY SPACE
           MOVE IN-ADDRESS1 TO ORDER-ADDRESS-OUT
           .

           IF WSX-ZIP-CODE(6:4) = '||||'
              AND WSX-SUPP-ADDRESS(1:1) = 1
              INSPECT ORDER-ADDRESS-OUT
                 REPLACING ALL DASH-DELIM BY SPACE
           END-IF
           .

           IF IN-ADDRESS1 = SPACES
              MOVE IN-ERROR-ADDRESS TO ORDER-ADDRESS-OUT
           END-IF
           .

           INSPECT IN-ADDRESS2
              REPLACING ALL PIPE-DELIM BY SPACE
           MOVE IN-ADDRESS2 TO SCHED-ADDRESS-OUT
           .

           IF WSX-ZIP-CODE(6:4) = '||||'
              AND WSX-SUPP-ADDRESS(1:1) = 2
              INSPECT SCHED-ADDRESS-OUT
                 REPLACING ALL DASH-DELIM BY SPACE
           END-IF
           .

           IF IN-ADDRESS2 = SPACES
              MOVE IN-ERROR-ADDRESS TO SCHED-ADDRESS-OUT
           END-IF
           .

           INSPECT IN-ADDRESS3
              REPLACING ALL PIPE-DELIM BY SPACE
           MOVE IN-ADDRESS3 TO REMIT-ADDRESS-OUT
           .

           IF WSX-ZIP-CODE(6:4) = '||||'
              AND WSX-SUPP-ADDRESS(1:1) = 3
              INSPECT REMIT-ADDRESS-OUT
                 REPLACING ALL DASH-DELIM BY SPACE
           END-IF
           .

           IF IN-ADDRESS3 = SPACES
              MOVE IN-ERROR-ADDRESS TO REMIT-ADDRESS-OUT
           END-IF
           .
       500-SUB-COMPUTE-PROCESS.
           IF PRICE-PO-CALC IS NUMERIC
              IF WSX-QUANTITY IS NUMERIC
                 IF WSX-UNIT-PRICE IS NUMERIC
                    ADD WSX-QUANTITY TO TOT-QUANTITY-PO-CALC
                    COMPUTE PRICE-PO-CALC
                       =(WSX-QUANTITY * WSX-UNIT-PRICE)
                    ADD PRICE-PO-CALC TO TOT-PRICE-PO-CALC
                    MOVE TOT-PRICE-PO-CALC TO TOT-PRICE-PO-FMT-OUT
                 ELSE
                    PERFORM 9999-OUTPUT-DEBUG
                 END-IF
              ELSE
                 PERFORM 9999-OUTPUT-DEBUG
              END-IF
           ELSE
              PERFORM 9999-OUTPUT-DEBUG
           END-IF
           .
       500-SUB-COMPUTE-PO-COSTS.
           PERFORM VARYING PO-IDX FROM 1 BY 1 UNTIL PO-IDX > 3
                   MOVE PURCHASE-ORDER(PO-IDX)
                      TO WSX-PURCHASE-ORDER
                   ADD 1 TO INDEX-VALUE-PO
                   PERFORM 500-SUB-COMPUTE-PROCESS
           END-PERFORM
           .

       500-CLEAN-UP.
           INITIALIZE IN-ADDRESS1.
           INITIALIZE IN-ADDRESS2.
           INITIALIZE IN-ADDRESS3.
           MOVE 0 TO INDEX-VALUE-ADDR.

       *> This will get invoked if there is a bad record blank line or
       *> other issue. This would be another good part candidate for
       *> call to the string clean program.
       9999-OUTPUT-DEBUG.
           MOVE SPACES TO DEBUG-RECORD.
           MOVE 'Start of RPTPRINT************************************'
              TO DEBUG-RECORD.
           WRITE DEBUG-RECORD.
           MOVE SPACES TO DEBUG-RECORD.
           STRING 'TOT-NUM-PO-FMAT-OUT               :'
              , TOT-NUM-PO-FMAT-OUT
              , SPACES
              DELIMITED BY SIZE INTO DEBUG-RECORD.
           WRITE DEBUG-RECORD.
           STRING 'PART-NUMBER-FORMATED             :'
              , PART-NUMBER-FORMATED
              , SPACES
              DELIMITED BY SIZE INTO DEBUG-RECORD.
           WRITE DEBUG-RECORD.
           MOVE SPACES TO DEBUG-RECORD.
           STRING 'PART-NUMBER-LAST-FORMATED        :'
              PART-NUMBER-LAST-FORMATED
              , SPACES
              DELIMITED BY SIZE INTO DEBUG-RECORD.
           WRITE DEBUG-RECORD.
           MOVE SPACES TO DEBUG-RECORD.
           STRING 'ORDER-ADDRESS-OUT                :'
              , ORDER-ADDRESS-OUT
              , SPACES
              DELIMITED BY SIZE INTO DEBUG-RECORD.
           WRITE DEBUG-RECORD.
           MOVE SPACES TO DEBUG-RECORD.
           STRING 'SCHED-ADDRESS-OUT                :'
              , SCHED-ADDRESS-OUT
              , SPACES
              DELIMITED BY SIZE INTO DEBUG-RECORD.
           WRITE DEBUG-RECORD.
           MOVE SPACES TO DEBUG-RECORD.
           STRING 'REMIT-ADDRESS-OUT                :'
              , REMIT-ADDRESS-OUT
              , SPACES
              DELIMITED BY SIZE INTO DEBUG-RECORD.
           WRITE DEBUG-RECORD.
           MOVE SPACES TO DEBUG-RECORD.
           STRING 'TOT-PRICE-PO-FMT-OUT              :'
              , TOT-PRICE-PO-FMT-OUT
              , SPACES
              DELIMITED BY SIZE INTO DEBUG-RECORD.
           WRITE DEBUG-RECORD.
           MOVE SPACES TO DEBUG-RECORD.
           STRING 'WSX-QUANTITY                      :'
              , WSX-QUANTITY
              , SPACES
              DELIMITED BY SIZE INTO DEBUG-RECORD.
           WRITE DEBUG-RECORD.
           MOVE SPACES TO DEBUG-RECORD.
           MOVE TOTAL-PRICE-PO-OUT TO DEBUG-RECORD.
           WRITE DEBUG-RECORD.
           MOVE SPACES TO DEBUG-RECORD.
           MOVE 'END of RPTPRINT***********************************'
              TO DEBUG-RECORD.
           WRITE DEBUG-RECORD.

       1000-CLOSE-END.
           CLOSE INPUT-RECORD.
           CLOSE PRINT-REC.
           CLOSE DEBUG-REC.
