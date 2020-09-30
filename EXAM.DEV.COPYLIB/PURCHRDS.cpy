       01  OU-PO-MAIN.
           05  OU-PURCHASE-ORDER OCCURS 3 TIMES.
               10  PO-NUMBER           PIC X(06) VALUE SPACES.
               10  BUYER-CODE          PIC X(03) VALUE SPACES.
               10  QUANTITY            PIC S9(7) VALUE ZEROS.
               10  UNIT-PRICE          PIC S9(7)V99 VALUE ZEROS.
               10  ORDER-DATE          PIC 9(08) VALUE ZERO.
               10  DELIVERY-DATE       PIC 9(08) VALUE ZERO.
