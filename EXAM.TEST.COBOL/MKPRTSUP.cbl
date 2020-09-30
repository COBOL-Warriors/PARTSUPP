       IDENTIFICATION DIVISION.
       PROGRAM-ID. MKPRTSUP.
      * Comment: This program creates sample PARTSUPP data
      *   (with the added benefit of not going blind).
       ENVIRONMENT DIVISION.
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       COPY PARTSUPP.


       PROCEDURE DIVISION.

       000-MAIN.
           PERFORM 100-CREATE-DATA.
           GOBACK.

       100-CREATE-DATA.
           MOVE "Part number" TO PART-NUMBER
           MOVE "Part name" TO PART-NAME
           MOVE "Spec N" TO SPEC-NUMBER
           MOVE "1" TO GOVT-COMML-CODE
           MOVE "BLUEPRINT" TO BLUEPRINT-NUMBER
           MOVE "Ea." TO UNIT-OF-MEASURE
           MOVE 3 TO WEEKS-LEAD-TIME
           MOVE "CHR" TO VEHICLE-MAKE
           MOVE "Daytona" TO VEHICLE-MODEL
           MOVE "1984" TO VEHICLE-YEAR

           MOVE "SUPPL CODE" TO SUPPLIER-CODE
           MOVE "S" TO SUPPLIER-TYPE
           MOVE "Supplier Name" TO SUPPLIER-NAME
           MOVE 20 TO SUPPLIER-PERF
           MOVE "3" TO SUPPLIER-RATING
           MOVE "1" TO SUPPLIER-STATUS
           MOVE 20021015 TO SUPPLIER-ACT-DATE


