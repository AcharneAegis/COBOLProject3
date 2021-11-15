       IDENTIFICATION DIVISION.
       PROGRAM-ID. CS370PROGRAM3.
       AUTHOR. P W ASKEW.
      ****************************************************************
      * This program serves to give practice with working with multiple
      * output files as well as expecting bad data.
      * The problem is that a CEO has purchased some warhouses full of 
      * candy and the previous owners did a poor job of trying to 
      * digitize the records. The report may contain incomplete data
      * and the file contains employee info and warehouse info together
      *
      * This program will separate the data into four inventory files
      * and four employee files. It will also create a file for entries
      * that are errors
      * *****
      * INPUT:
      *    The PR3FA21-MESS.txt file contains the following:
      *        1. Warehouse ID
      *        2. Employee ID
      *        3. Employee Position
      *        4. Employee Last Name
      *        5. Employee First Name
      *        6. Employee Middle Initial
      *        7. Hire Date
      *        8. Filler
      *        9. Current Yearly Salary
      *        10. Number of Dependents
      *        11. Health Plan
      *        12. Health Insurance Cost
      *        13. Filler
      *        14. Vender ID
      *        15. Candy ID
      *        16. Candy Data Array
      *            1. Candy Name
      *            2. Candy Box Size
      *            3. Candy Type
      *            4. Cases in Stock
      *            5. Purchase Price
      * *****
      * OUTPUT:
      *    Each Inventory File will contain the following:
      *        1. Warehouse ID
      *        2. Vender ID
      *        3. Candy ID
      *        4. Candy Data Array
      *            1. Candy Name
      *            2. Candy Box Size
      *            3. Candy Type
      *            4. Cases in Stock
      *            5. Purchase Price
      *
      *    Each Employee File will contain the following:
      *        1. Warehouse ID
      *        2. Employee ID
      *        3. Employee Position
      *        4. Employee Last Name
      *        5. Employee First Name
      *        6. Employee Middle Initial
      *        7. Hire Date
      *        8. Filler
      *        9. Current Yearly Salary
      *        10. Number of Dependents
      *        11. Health Plan
      *        12. Health Insurance Cost
      *        13. Filler
      *
      *    The ERROR File will contain a copy of each record without 
      *    a valid Warehouse ID
      *
      ***************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MESSEY-FILE
               ASSIGN TO 'PR3FA21-MESS-1.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT BHAM-EMP-REPORT
               ASSIGN TO PRINTER 'B100-Employee-Report.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT HUNT-EMP-REPORT
               ASSIGN TO PRINTER 'B200-Employee-Report.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ANNI-EMP-REPORT
               ASSIGN TO PRINTER 'B300-Employee-Report.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT MONT-EMP-REPORT
               ASSIGN TO PRINTER 'B400-Employee-Report.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT BHAM-INVENTORY-REPORT
               ASSIGN TO PRINTER 'B100-Inventory-Report.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT HUNT-INVENTORY-REPORT
               ASSIGN TO PRINTER 'B200-Inventory-Report.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ANNI-INVENTORY-REPORT
               ASSIGN TO PRINTER 'B300-Inventory-Report.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT MONT-INVENTORY-REPORT
               ASSIGN TO PRINTER 'B400-Inventory-Report.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ERROR-FILE
               ASSIGN TO PRINTER 'ERROR-File.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD MESSEY-FILE
           RECORD CONTAINS 224 CHARACTERS.

       01  MESSEY-RECORD.
           05  WAREHOUSE-ID            PIC X(4).
           05  EMP-ID                  PIC X(5).
           05  EMP-POS                 PIC AA.
           05  EMP-LAST-NAME           PIC X(10).
           05  EMP-FIRST-NAME          PIC X(10).
           05  EMP-MIDDLE-INI          PIC X.
           05  FILLER                  PIC XX.
           05  EMP-HIRE-DATE           PIC 9(8).
           05  FILLER                  PIC X(25).
           05  EMP-SALARY              PIC 9(8).
           05  EMP-DEPENDENTS          PIC 99.
           05  EMP-HEALTH-PLAN         PIC A.
           05  EMP-HEALTH-COST         PIC 999.
           05  FILLER                  PIC XXXX.
           05  VENDOR-ID               PIC A.
           05  CANDY-ID                PIC XXX.
           05  CANDY-DATA OCCURS 5 TIMES.
               10  CANDY-NAME          PIC X(15).
               10  CANDY-BOX-SIZE      PIC A.
               10  CANDY-TYPE          PIC AA.
               10  CANDY-STOCK         PIC S9(4).
               10  PURCHASE-PRICE      PIC S999V99.

       FD BHAM-EMP-REPORT
           RECORD CONTAINS 85 CHARACTERS.

       01  BHAM-EMP-RECORD             PIC X(85).

       FD HUNT-EMP-REPORT
           RECORD CONTAINS 85 CHARACTERS.

       01  HUNT-EMP-RECORD             PIC X(85).

       FD ANNI-EMP-REPORT
           RECORD CONTAINS 85 CHARACTERS.

       01  ANNI-EMP-RECORD             PIC X(85).

       FD MONT-EMP-REPORT
           RECORD CONTAINS 85 CHARACTERS.

       01  MONT-EMP-RECORD             PIC X(85).

       FD BHAM-INVENTORY-REPORT
           RECORD CONTAINS 143 CHARACTERS.

       01  BHAM-INVENTORY-RECORD       PIC X(143).

       FD HUNT-INVENTORY-REPORT
           RECORD CONTAINS 143 CHARACTERS.

       01  HUNT-INVENTORY-RECORD       PIC X(143).

       FD ANNI-INVENTORY-REPORT
           RECORD CONTAINS 143 CHARACTERS.

       01  ANNI-INVENTORY-RECORD       PIC X(143).

       FD MONT-INVENTORY-REPORT
           RECORD CONTAINS 143 CHARACTERS.

       01  MONT-INVENTORY-RECORD      PIC X(143).

       FD ERROR-FILE
           RECORD CONTAINS 224 CHARACTERS.

       01  ERROR-RECORD                PIC X(224).

       WORKING-STORAGE SECTION.

       01  FLAGS-N-SWITCHES.
           05  EOF-FLAG                PIC X           VALUE ' '.
               88 NO-MORE-DATA                         VALUE 'N'.
           05  FIRST-RECORD                            VALUE 'Y'.
       
       01  TEMP-FIELDS.
           05  TEMP-NEW-WAREHOUSE-ID   PIC X(4).
           05  SUB                     PIC 99          VALUE 1.

       01  WAREHOUSE-TABLE.
           05  FILLER                  PIC X(8)        VALUE 'BHAMB100'.
           05  FILLER                  PIC X(8)        VALUE 'HUNTB200'.
           05  FILLER                  PIC X(8)        VALUE 'ANNIB300'.
           05  FILLER                  PIC X(8)        VALUE 'MONTB400'.

       01  WAREHOUSES REDEFINES WAREHOUSE-TABLE.
           05 WAREHOUSE OCCURS 4 TIMES
               INDEXED BY WAREHOUSE-ID-INDEX.
               10 OLD-WAREHOUSE-ID     PIC X(4).
               10 NEW-WAREHOUSE-ID     PIC X(4).

      *********************    OUTPUT AREA     *************************

       01 EMP-RECORD.
           05  OUT-EMP-WAREHOUSE-ID        PIC X(4).
           05  OUT-EMP-ID                  PIC X(5).
           05  OUT-EMP-POS                 PIC AA.
           05  OUT-LAST-NAME               PIC X(10).
           05  OUT-FIRST-NAME              PIC X(10).
           05  OUT-MIDDLE-INI              PIC X.
           05  FILLER                      PIC XX.
           05  OUT-HIRE-DATE               PIC 9(8).
           05  FILLER                  PIC X(25).
           05  OUT-SALARY                  PIC 9(8).
           05  OUT-DEPENDENTS              PIC 99.
           05  OUT-HEALTH-PLAN             PIC A.
           05  OUT-HEALTH-COST             PIC 999.
           05  FILLER                  PIC XXXX.

       01  INVENTORY-RECORD.
           05  OUT-INV-WAREHOUSE-ID        PIC X(4).
           05  OUT-VENDOR-ID               PIC A.
           05  OUT-CANDY-ID                PIC XXX.
           05  OUT-CANDY-DATA OCCURS 5 TIMES.
               10  OUT-CANDY-NAME          PIC X(15).
               10  OUT-CANDY-BOX-SIZE      PIC A.
               10  OUT-CANDY-TYPE          PIC AA.
               10  OUT-CANDY-STOCK         PIC S9(4).
               10  OUT-PURCHASE-PRICE      PIC S999V99.

       PROCEDURE DIVISION.
       
       10-CONTROL-MODULE.
           
           PERFORM 15-HSKPING-ROUTINE
           PERFORM 20-PROCESS-INPUT-FILE
           PERFORM 40-EOF-ROUTINE
           .

       15-HSKPING-ROUTINE.
           
           OPEN INPUT MESSEY-FILE
               OUTPUT BHAM-EMP-REPORT
               OUTPUT HUNT-EMP-REPORT
               OUTPUT ANNI-EMP-REPORT
               OUTPUT MONT-EMP-REPORT
               OUTPUT BHAM-INVENTORY-REPORT
               OUTPUT HUNT-INVENTORY-REPORT
               OUTPUT ANNI-INVENTORY-REPORT
               OUTPUT MONT-INVENTORY-REPORT
               OUTPUT ERROR-FILE

           .

       20-PROCESS-INPUT-FILE.
           PERFORM UNTIL NO-MORE-DATA
               READ MESSEY-FILE
                   AT END
                       MOVE 'N' TO EOF-FLAG
                   NOT AT END 
                       PERFORM 25-WAREHOUSE-CHECK
               END-READ
           END-PERFORM

           .

       25-WAREHOUSE-CHECK.
       
           SET WAREHOUSE-ID-INDEX TO 1
           SEARCH WAREHOUSE
               AT END PERFORM 35-ID-ERROR
               WHEN WAREHOUSE-ID IS EQUAL TO OLD-WAREHOUSE-ID
                                                   (WAREHOUSE-ID-INDEX)
                   MOVE NEW-WAREHOUSE-ID(WAREHOUSE-ID-INDEX) TO 
                                               TEMP-NEW-WAREHOUSE-ID
                   PERFORM 30-PROCESS-WAREHOUSE-DATA
           END-SEARCH

           .

       30-PROCESS-WAREHOUSE-DATA.
           
           MOVE TEMP-NEW-WAREHOUSE-ID TO OUT-EMP-WAREHOUSE-ID
           MOVE EMP-ID TO OUT-EMP-ID
           MOVE EMP-POS TO OUT-EMP-POS
           MOVE EMP-LAST-NAME TO OUT-LAST-NAME
           MOVE EMP-FIRST-NAME TO OUT-FIRST-NAME
           MOVE EMP-MIDDLE-INI TO OUT-MIDDLE-INI
           MOVE EMP-HIRE-DATE TO OUT-HIRE-DATE
           MOVE EMP-SALARY TO OUT-SALARY
           MOVE EMP-DEPENDENTS TO OUT-DEPENDENTS
           MOVE EMP-HEALTH-PLAN TO OUT-HEALTH-PLAN
           MOVE EMP-HEALTH-COST TO OUT-HEALTH-COST

           MOVE TEMP-NEW-WAREHOUSE-ID TO OUT-INV-WAREHOUSE-ID
           MOVE VENDOR-ID TO OUT-VENDOR-ID
           MOVE CANDY-ID TO OUT-CANDY-ID

           PERFORM VARYING SUB
               FROM 1 BY 1 UNTIL SUB > 5
           MOVE CANDY-DATA (SUB) TO OUT-CANDY-DATA (SUB)
           END-PERFORM

           EVALUATE TRUE
               WHEN TEMP-NEW-WAREHOUSE-ID IS EQUAL TO 'B100'
                   MOVE EMP-RECORD TO BHAM-EMP-RECORD
                   WRITE BHAM-EMP-RECORD 
                   
                   MOVE INVENTORY-RECORD TO BHAM-INVENTORY-RECORD
                   WRITE BHAM-INVENTORY-RECORD


               WHEN TEMP-NEW-WAREHOUSE-ID IS EQUAL TO 'B200'
                   MOVE EMP-RECORD TO HUNT-EMP-RECORD
                   WRITE HUNT-EMP-RECORD
                   
                   MOVE INVENTORY-RECORD TO HUNT-INVENTORY-RECORD
                   WRITE HUNT-INVENTORY-RECORD


               WHEN TEMP-NEW-WAREHOUSE-ID IS EQUAL TO 'B300'
                   MOVE EMP-RECORD TO ANNI-EMP-RECORD
                   WRITE ANNI-EMP-RECORD
                   
                   MOVE INVENTORY-RECORD TO ANNI-INVENTORY-RECORD
                   WRITE ANNI-INVENTORY-RECORD


               WHEN TEMP-NEW-WAREHOUSE-ID IS EQUAL TO 'B400'
                   MOVE EMP-RECORD TO MONT-EMP-RECORD
                   WRITE MONT-EMP-RECORD
                   
                   MOVE INVENTORY-RECORD TO MONT-INVENTORY-RECORD
                   WRITE MONT-INVENTORY-RECORD

           END-EVALUATE
           .

       35-ID-ERROR.
           MOVE MESSEY-RECORD TO ERROR-RECORD
           WRITE ERROR-RECORD

           .

       40-EOF-ROUTINE.
           CLOSE MESSEY-FILE
                BHAM-EMP-REPORT
                HUNT-EMP-REPORT
                ANNI-EMP-REPORT
                MONT-EMP-REPORT
                BHAM-INVENTORY-REPORT
                HUNT-INVENTORY-REPORT
                ANNI-INVENTORY-REPORT
                MONT-INVENTORY-REPORT
                ERROR-FILE

           STOP RUN
           .

           
           