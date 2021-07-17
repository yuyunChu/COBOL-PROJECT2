       IDENTIFICATION DIVISION. 
       PROGRAM-ID. CST8283-PROJECT2 AS "CST8283-PROJECT2".
       AUTHOR. CHU-YU-YUN.
       DATE-WRITTEN. JULY 22TH 2020.
       DATE-COMPILED. JULY 22TH 2020.
       SECURITY.  PROF JASON MOMBOURQUETTE.
************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       CURRENCY SIGN IS "$" WITH PICTURE SYMBOL "$".
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * INPUT FILE 1
           SELECT INVENT-FILE-IN
              ASSIGN TO "./INVENT.TXT"
              ACCESS MODE IS SEQUENTIAL
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS IFI-STATUS.

      * INPUT FILE 2
           SELECT SUPPLIER-FILE-IN
              ASSIGN TO "./SUPPLIERS.TXT"
              ACCESS MODE IS SEQUENTIAL
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS SFI-STATUS.
              
      * OUTPUT FILE 1
           SELECT INVENT-REPORT-OUT
              ASSIGN TO "./INVREPRT.TXT"
              ORGANIZATION IS LINE SEQUENTIAL.                          

      * OUTPUT FILE 2
           SELECT REORDER-REPORT-OUT
              ASSIGN TO "./INVREORD.TXT"
              ORGANIZATION IS LINE SEQUENTIAL. 
              
       DATA DIVISION.
       FILE SECTION.
       FD INVENT-FILE-IN.
       01 INVENT-IN-RECORD.
		   02 PART-NUMBER-IN  	  PIC 9(5).
		   02 PART-NAME-IN        PIC X(20).
		   02 QUANTITY-IN         PIC 9(3).
		   02 UNIT-PRICE-IN    	  PIC 9(2)V99.
	       02 SUPPLIER-CODE-IN    PIC X(5).
           02 RE-ORDER-POINT-IN   PIC 9(3).
       
       FD SUPPLIER-FILE-IN.
       01 SUPPLIER-RECORD-IN.
           02 SUPPLIER-CODE       PIC X(5).
           02 SUPPLIER-NAME       PIC X(15).

            
       FD INVENT-REPORT-OUT.
       01 INVENT-OUT-RECORD.
           02 PART-NUMBER-OUT      PIC X(7).
           02 PART-NAME-OUT        PIC X(27).
           02 QUANTITY-OUT         PIC X(6).
           02 TOTAL-VALUE-OUT      PIC $$$,$$$,$$9.99.
       
       FD REORDER-REPORT-OUT.
       01 REORDER-OUT-RECORD.
           02 PART-NUMBER-OUT2          PIC 9(5).
           02 PART-NAME-OUT2            PIC X(20).
           02 RE-ORDER-POINT-OUT        PIC 9(3).
           02 SUPPLIER-NAME-OUT         PIC X(15).

       WORKING-STORAGE SECTION.
       
       01 MONEY-FORMAT PIC $$$,$$$,$$9.99.
       
       01 SUPPLIER-TABLE.
           05 SUPPLIER-TABLE-RECORD OCCURS 1000 TIMES.
               10 TBL-SUPPLIER-CODE     PIC X(5).
               10 SUPPLIER-NAME-TBL     PIC X(15).
    
       
       01 FLAGS-AND-COUNTERS.
          05 INV-EOF-FLAG                   PIC X(3) VALUE "NO".
          05 SUP-EOF-FLAG                   PIC X(3) VALUE "NO".
          05 FOUND-FLAG                     PIC X(3) VALUE "NO".
          05 SUB                            PIC 9(4) VALUE 1.
          05 INVENTORY-VALUE                PIC 9(15) VALUE ZERO.
          05 AUDIT-READ-COUNTER             PIC 9(4) VALUE ZERO.
          05 AUDIT-WRIT-COUNTER             PIC 9(4) VALUE ZERO.
       
       01 HEADING-LINE.
           05 FILLER	        PIC X(7)  VALUE 'NUMBER'.
           05 FILLER	        PIC X(1)  VALUE SPACES.
           05 FILLER	        PIC X(20) VALUE 'PART NAME'.
           05 FILLER	        PIC X(5)  VALUE SPACES.
           05 FILLER	        PIC X(3)  VALUE 'QTY'.
           05 FILLER	        PIC X(3)  VALUE SPACES.
           05 FILLER	        PIC X(15) VALUE 'VALUE'.
       
   	   01 INVENTORY-DETAIL-LINE.
   	       05 WS-PART-NUMBER-OUT       PIC 9(7).
   	       05 FILLER                   PIC X(1) VALUE SPACES.
   	       05 WS-PART-NAME-OUT         PIC X(20).
           05 FILLER                   PIC X(5) VALUE SPACES.
   	       05 WS-QUANTITY-OUT          PIC 9(3).
           05 FILLER                   PIC X(3) VALUE SPACES.
   	       05 WS-TOTAL-VALUE-OUT       PIC $$$,$$$,$$9.99.
           
       01 WS-REORDER-REPORT.
           02 WS-PART-NUMBER-OUT2          PIC 9(5).
           02 WS-PART-NAME-OUT2            PIC X(20).
           02 WS-RE-ORDER-POINT-OUT        PIC 9(3).
           02 WS-SUPPLIER-NAME-OUT         PIC X(15).
       
       01  TOTAL-LINE1.
		   05 FILLER            PIC X(41) VALUE SPACES.
		   05 FILLER            PIC X(15) VALUE 
			      "===============".
		   05 FILLER            PIC X(75).
           
       01 TOTAL-VALUE           PIC 9(15).
       
       77 IFI-STATUS PIC X(2).
       77 SFI-STATUS PIC X(2).
           
       PROCEDURE DIVISION.
       100-MANAGE-INVENTORY.
           PERFORM 201-INITIALIZE-PRODUCE-INVENTORY-REPORT.
           PERFORM 202-PROCESS-INVENTORY-REPORT 
             UNTIL INV-EOF-FLAG = "YES".
           PERFORM 203-TERMINATE-PROCESS.
           STOP RUN.
           
       201-INITIALIZE-PRODUCE-INVENTORY-REPORT.
           PERFORM 301-OPEN-FILES.
           PERFORM 302-LOAD-SUPPLIER-TABLE
                   VARYING SUB FROM 1 BY 1 UNTIL SUB > 1000
                       OR SUP-EOF-FLAG = "YES".
           PERFORM 303-WRITE-HEADING.
           
       202-PROCESS-INVENTORY-REPORT.
           PERFORM 304-READ-INVENTORY-RECORD.
           IF INV-EOF-FLAG = "NO"
               PERFORM 305-SEARCH-SUPPLIER-RECORD
                       VARYING SUB FROM 1 BY 1 UNTIL SUB > 1000
                       OR FOUND-FLAG = "YES"
               PERFORM 306-CALCULATE-INVENTORY-VALUE
               PERFORM 307-CALCULATE-TOTAL-VALUE
               PERFORM 308-CHECK-FOR-REORDER
               PERFORM 309-WRITE-INVENTORY-RECORD
           END-IF.                                                        
       
       203-TERMINATE-PROCESS.
           PERFORM 310-PRINT-AUDIT-COUNTER.
           PERFORM 311-CLOSE-FILE.
           
       301-OPEN-FILES.    
		   OPEN INPUT INVENT-FILE-IN.
           OPEN INPUT SUPPLIER-FILE-IN.
           OPEN OUTPUT INVENT-REPORT-OUT.
           OPEN OUTPUT REORDER-REPORT-OUT.
       
       302-LOAD-SUPPLIER-TABLE.
           READ SUPPLIER-FILE-IN
               AT END MOVE "YES" TO SUP-EOF-FLAG
                  NOT AT END 
                     MOVE SUPPLIER-RECORD-IN
                               TO SUPPLIER-TABLE-RECORD(SUB).
       303-WRITE-HEADING.
           WRITE INVENT-OUT-RECORD FROM HEADING-LINE.
       
       304-READ-INVENTORY-RECORD.
           MOVE "NO" TO FOUND-FLAG.
           READ INVENT-FILE-IN AT END MOVE "YES" TO INV-EOF-FLAG
                               NOT AT END ADD 1 TO AUDIT-READ-COUNTER.

       305-SEARCH-SUPPLIER-RECORD.
           IF TBL-SUPPLIER-CODE(SUB) = SUPPLIER-CODE-IN           
               MOVE "YES" TO FOUND-FLAG
               MOVE SUPPLIER-NAME-TBL(SUB) TO WS-SUPPLIER-NAME-OUT
           END-IF.
       
       306-CALCULATE-INVENTORY-VALUE.
           COMPUTE INVENTORY-VALUE = QUANTITY-IN * UNIT-PRICE-IN.     
       
       307-CALCULATE-TOTAL-VALUE.
           ADD INVENTORY-VALUE TO TOTAL-VALUE.
           
       308-CHECK-FOR-REORDER.
           IF QUANTITY-IN LESS THAN OR EQUAL TO RE-ORDER-POINT-IN
               PERFORM 401-WRITE-REORDER-RECORD
           END-IF.
           
       309-WRITE-INVENTORY-RECORD.
           MOVE PART-NUMBER-IN TO WS-PART-NUMBER-OUT.
           MOVE PART-NAME-IN TO WS-PART-NAME-OUT.
           MOVE QUANTITY-IN TO WS-QUANTITY-OUT.
           MOVE INVENTORY-VALUE TO WS-TOTAL-VALUE-OUT.
           WRITE INVENT-OUT-RECORD FROM INVENTORY-DETAIL-LINE.
           ADD 1 TO AUDIT-WRIT-COUNTER.
           
       310-PRINT-AUDIT-COUNTER.
           DISPLAY "Inventory records read: " AUDIT-READ-COUNTER.
           DISPLAY "Inventory records written: " AUDIT-WRIT-COUNTER.
           MOVE TOTAL-VALUE TO MONEY-FORMAT.
           DISPLAY "Total value" MONEY-FORMAT.    
           
       401-WRITE-REORDER-RECORD.
           MOVE PART-NUMBER-IN TO WS-PART-NUMBER-OUT2.
           MOVE WS-PART-NAME-OUT TO WS-PART-NAME-OUT2.
           MOVE RE-ORDER-POINT-IN TO WS-RE-ORDER-POINT-OUT.
           WRITE REORDER-OUT-RECORD FROM WS-REORDER-REPORT.             
           
	   311-CLOSE-FILE.
           CLOSE INVENT-FILE-IN.	   
           CLOSE SUPPLIER-FILE-IN.	
           CLOSE INVENT-REPORT-OUT.
           CLOSE REORDER-REPORT-OUT.
           STOP RUN.

           
      *EXIT THIS COBOL PROGRAM
       END PROGRAM CST8283-PROJECT2.
