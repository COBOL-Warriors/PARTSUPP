      *--------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *--------------------------------------------------------------
       PROGRAM-ID.    PARTEDIT.
       AUTHOR.        WARRIORS.
      *--------------------------------------------------------------
      ***************************************************************
      * Workshop:                    FINAL EXAM
      * Developer:                  Maruca
      * Created:                    2020-09-08
      * Modified:
      * Modified:
      * Developer Contact:
      * V R M:                      V0R0M1
      *  Version Level
      *  Release Level
      *  Modification Level
      * SUB-PROGRAM Called from PARTMAIN
      * Evaluates the data that is passed defined below for errors and
      * then returns information back to PARTMAIN.
      * PARTMAIN passes.
      *    G OU-PART-NUMBER           PIC 9.
      *    OU-PART-NAME               PIC 9.
      *    OU-WEEKS-LEAD-TIME         PIC 9.
      *    OU-VEHICLE-MODEL           PIC 9.
      *    OU-VEHICLE-MAKE            PIC 9.
      *    OU-VEHICLE-YEAR            PIC 9.
      *    WS-PARTEDIT-RETURN-CODE    PIC X(90)
      *    WS-PARTEDIT-RETURN-MESSAGE PIC 9(3)
      *    WS-PARTEDIT-ERROR-NUMBER
      *
      **   9998-PROGRAM-RETURN
      *         Will send message to the PARTMAIN.
      ***************************************************************
      ***************************************************************
      * Modifications
      * Date       Developer Modification
      *    Changes Below
      * -------------------------------------------------------------
      * 2020-09-13 maruca    V0R0M1
      *    Add Comments
      *    in the record format below.
      *       Index Number:Error Message 3 x 30 character strings moved
      *       into a 90 character field spaced evenly at 30 characters.
      *    Example
      *     1:Error PO-NUMBER Field     +
      *     2:Error DELIVERY-DATE Field +
      *     3:Valid Record              End of Record
      *
      * 2020-09-16 maruca : change the validation criteria
      *    empty and not valid is just one error , no 2
      *    change validation of VEHICLE-YEAR
      * 2020-09-17 maruca : change to clean the other 2 messages
      *    when we have more than 3 errors.
      *
      *
      ***************************************************************
      *--------------------------------------------------------------
       DATA DIVISION.
      *--------------------------------------------------------------

      *--------------------------------------------------------------
       FILE SECTION.
      *--------------------------------------------------------------

      *--------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *--------------------------------------------------------------
       77 WS-STORAGE-IND      PIC X(60)
                                        VALUE
             'WORKING STORAGE PARTEDIT BEGINS HERE'.

      * use to send 3 errors to the PARTMAIN
      * if the number of errors > 3 not need to have the messages
       01 WS-ERROR-MESSAGES.
          05 WS-MESSAGE-1     PIC X(29) VALUE SPACES.
          05 FILLER           PIC X     VALUE SPACES.
          05 WS-MESSAGE-2     PIC X(29) VALUE SPACES.
          05 FILLER           PIC X     VALUE SPACES.
          05 WS-MESSAGE-3     PIC X(29) VALUE SPACES.
       01 WS-WORKING-MESSAGE  PIC X(29) VALUE SPACES.
       01 WS-ERROR-NUMBER     PIC 9(3)  VALUE ZEROS.
      *variable needed to compare the YEAR as numeric
       01 WS-VEHICLE-YEAR     PIC 9(4)  VALUE ZEROS.
          88 PASS                       VALUES ARE 1990 THRU 2019.

      *--------------------------------------------------------------
       LINKAGE SECTION.
      *--------------------------------------------------------------


       01 LS-PART-NUMBER      PIC X(23).
       01 LS-PART-NAME        PIC X(14).
       01 LS-WEEKS-LEAD-TIME  PIC 9(03).
       01 LS-VEHICLE-MODEL    PIC X(10).
       01 LS-VEHICLE-MAKE     PIC X(03).
          88 CHRYSLER                   VALUE 'CHR'.
          88 FORD                       VALUE 'FOR'.
          88 GM                         VALUE 'GM '.
          88 VOLKSWAGEN                 VALUE 'VW '.
          88 TOYOTA                     VALUE 'TOY'.
          88 JAGUAR                     VALUE 'JAG'.
          88 PEUGEOT                    VALUE 'PEU'.
          88 BMW                        VALUE 'BMW'.
       01 LS-VEHICLE-YEAR     PIC X(04).
       01 LS-RETURN-CODE      PIC 9.
       01 LS-RETURN-MESSAGE   PIC X(90).
       01 LS-ERROR-NUMBER     PIC 9(3).

      *****************************************************************
      * This subroutine validates:
      * -the mandatory fields are not empty
      * and
      * - 3 fields from PARTSUPP file:
      *   -VEHICLE MAKE must be one of the listed 88 level fields
      *   -VEHICLE YEAR must be between 1990 and 2019
      *   -WEEKS LEAD TIME must be numeric and between 1 and 4

      *****************************************************************


       PROCEDURE DIVISION USING
                               LS-PART-NUMBER,
                               LS-PART-NAME,
                               LS-WEEKS-LEAD-TIME,
                               LS-VEHICLE-MODEL,
                               LS-VEHICLE-MAKE,
                               LS-VEHICLE-YEAR,
                               LS-RETURN-CODE,
                               LS-RETURN-MESSAGE,
                               LS-ERROR-NUMBER.


      * Initialize the working fields

           MOVE 0 TO LS-RETURN-CODE.
           MOVE SPACES TO LS-RETURN-MESSAGE.
           MOVE SPACES TO WS-ERROR-MESSAGES.
           MOVE 0 TO WS-ERROR-NUMBER.

      * Start validation:empty fields

           IF LS-PART-NUMBER = SPACE
              MOVE 'PART NUMBER IS EMPTY' TO WS-WORKING-MESSAGE
              PERFORM 100-ERROR-PROCESS

           END-IF.

           IF LS-PART-NAME = SPACE

              MOVE 'PART NAME IS EMPTY' TO WS-WORKING-MESSAGE
              PERFORM 100-ERROR-PROCESS
           END-IF.


           IF LS-VEHICLE-MODEL = SPACE

              MOVE 'VEHICLE MODEL IS EMPTY' TO WS-WORKING-MESSAGE
              PERFORM 100-ERROR-PROCESS
           END-IF.




           IF LS-WEEKS-LEAD-TIME IS < 1 OR > 4

              MOVE 'LEAD TIME IS NOT VALID ' TO WS-WORKING-MESSAGE
              PERFORM 100-ERROR-PROCESS
           END-IF.

           IF LS-VEHICLE-YEAR IS NUMERIC
              MOVE LS-VEHICLE-YEAR TO WS-VEHICLE-YEAR

      *       WS-VEHICLE-YEAR must be < 1990 OR > 2019
              IF NOT PASS
                 MOVE 'VEHICLE YEAR IS NOT VALID'
                    TO WS-WORKING-MESSAGE
                 PERFORM 100-ERROR-PROCESS
              END-IF
           ELSE
              MOVE 'VEHICLE YEAR IS NOT NUMERIC'
                 TO WS-WORKING-MESSAGE
              PERFORM 100-ERROR-PROCESS
           END-IF.

           IF LS-VEHICLE-MAKE NOT = 'CHR' AND 'FOR' AND 'GM ' AND 'VW '
              AND 'TOY' AND 'JAG' AND 'PEU' AND 'BMW'

              MOVE 'VEHICLE MAKE NOT VALID ' TO WS-WORKING-MESSAGE
              PERFORM 100-ERROR-PROCESS
           END-IF.

           MOVE WS-ERROR-MESSAGES TO LS-RETURN-MESSAGE.
           MOVE WS-ERROR-NUMBER TO LS-ERROR-NUMBER.

           GOBACK.
      ****************************************************************
      * Error messages process.
      * If number of error is > 3 the error msg it not relevant
      * Else we need all themsgs on the return message
      ****************************************************************

       100-ERROR-PROCESS.

           ADD 1 TO WS-ERROR-NUMBER.

           EVALUATE WS-ERROR-NUMBER
           WHEN 1
                MOVE WS-WORKING-MESSAGE TO WS-MESSAGE-1
           WHEN 2
                MOVE WS-WORKING-MESSAGE TO WS-MESSAGE-2
           WHEN 3
                MOVE WS-WORKING-MESSAGE TO WS-MESSAGE-3
           WHEN OTHER
                MOVE 'PARTEDIT more than 3 errors' TO WS-MESSAGE-1
                MOVE SPACES TO WS-MESSAGE-2
                MOVE SPACES TO WS-MESSAGE-3


           END-EVALUATE.


           MOVE 8 TO LS-RETURN-CODE.



