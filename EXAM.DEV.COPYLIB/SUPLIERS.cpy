       01 OU-SUPPLIERS.
           05  OU-SUPPLIER-CODE     PIC X(10) VALUE SPACES.
           05  OU-SUPPLIER-TYPE     PIC X(01) VALUE SPACES.
                88 OU-VALID-SUPPLIER-TYPE VALUES 'S' 'D' 'M' 'I'.
           05  OU-SUPPLIER-NAME     PIC X(15) VALUE SPACES.
           05  OU-SUPPLIER-PERF     PIC 9(03)  COMP VALUE ZEROS.
           05  OU-SUPPLIER-RATING   PIC X(01) VALUE SPACES.
                88 OU-VALID-SUPPLIER-RATING VALUES '3' '2' '1'.
           05  OU-SUPPLIER-STATUS   PIC X(01) VALUE SPACES.
                88 OU-VALID-SUPPLIER-STATUS VALUES '1' '2' '3'.
           05  OU-SUPPLIER-ACT-DATE PIC 9(08) VALUE ZERO.