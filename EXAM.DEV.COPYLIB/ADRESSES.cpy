       01  OU-SUPP-ADDRESSES-MAIN.
           05  OU-SUPP-ADDRESS OCCURS 3 TIMES.
               10   ADDRESS-TYPE      PIC X(01) VALUE SPACES.
                   88  ORDER-ADDRESS           VALUE '1'.
                   88  SCHED-ADDRESS           VALUE '2'.
                   88  REMIT-ADDRESS           VALUE '3'.
                   88  ADDRESS-TYPE-VALID VALUES '1' '2' '3'.
               10  ADDRESS-1         PIC X(15) VALUE SPACES.
               10  ADDRESS-2         PIC X(15) VALUE SPACES.
               10  ADDRESS-3         PIC X(15) VALUE SPACES.
               10  CITY              PIC X(15) VALUE SPACES.
               10  ADDR-STATE        PIC X(02) VALUE SPACES.
               10  ZIP-CODE          PIC 9(10) VALUE ZERO.

