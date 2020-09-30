       01  OU-PARTS.
           05  OU-PART-NUMBER       PIC X(23) VALUE SPACES.
           05  OU-PART-NAME         PIC X(14) VALUE SPACES.
           05  OU-SPEC-NUMBER       PIC X(07) VALUE SPACES.
           05  OU-GOVT-COMML-CODE   PIC X(01) VALUE SPACES.
           05  OU-BLUEPRINT-NUMBER  PIC X(10) VALUE SPACES.
           05  OU-UNIT-OF-MEASURE   PIC X(03) VALUE SPACES.
           05  OU-WEEKS-LEAD-TIME   PIC 9(03) COMP VALUE ZEROS.
           05  OU-VEHICLE-MAKE      PIC X(03) VALUE SPACES.
               88 OU-VALID-MAKE VALUES ARE 'CHR' 'FOR' 'GM ' 'VW ' 'TOY'
                                    'JAG' 'PEU' 'BMW'.
           05  OU-VEHICLE-MODEL     PIC X(10) VALUE SPACES.
           05  OU-VEHICLE-YEAR      PIC X(04) VALUE '0000'.
           05  OU-FILLER            PIC X(14) VALUE SPACES.