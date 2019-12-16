       IDENTIFICATION DIVISION.
       PROGRAM-ID.     COBLSC01.
       AUTHOR.         LUCAS CHAPMAN.
       DATE-WRITTEN.   12/3/19.
       DATE-COMPILED.
      ******************************************************************
      * THIS PROGRAM READS A FILE AND CREATES *
      * A PAINT ESTIMATE REPORT.              *
      ************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT PAINT-MASTER
               ASSIGN TO 'D:\IHCC\COBOL\COBLSC01\PAINTEST.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT PRTOUT
               ASSIGN TO 'D:\IHCC\COBOL\COBLSC01\PJOBEST.PRT'
               ORGANIZATION IS RECORD SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  PAINT-MASTER
           LABEL RECORD IS STANDARD
           DATA RECORD IS I-PAINT-REC
           RECORD CONTAINS 23 CHARACTERS.
       01  I-PAINT-REC.
           05  I-PAINT-EST-NO            	PIC X(4).
		   05  I-PAINT-DATE.
			   10   I-PAINT-YY              PIC 9(4).
			   10   I-PAINT-MM              PIC 99.
			   10   I-PAINT-DD              PIC 99.
		   05  I-PAINT-WALL-SQ-FT           PIC 9(4).
		   05  I-PAINT-DOOR-SQ-FT           PIC 9(3).
		   05  I-PAINT-PRICE-GAL            PIC 99V99.
                                         
       FD  PRTOUT
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS PRTLINE
           LINAGE IS 60 WITH FOOTING AT 56.

       01  PRTLINE                     		PIC X(132).

       WORKING-STORAGE SECTION.
       01  WORK-AREA.
           05  C-PCTR                  		PIC 99      VALUE 0.
           05  MORE-RECS               		PIC XXX     VALUE 'YES'.    
		   05  C-WALL-SPACE            	    PIC 9(4)    VALUE 0.
		   05  C-GALS-NEEDED                PIC 9(3)V99 VALUE 0.
           05  C-PAINT-COST-EST             PIC 9(5)V99 VALUE 0.        
           05  C-LABOR-COST-EST             PIC 9(5)V99 VALUE 0.        
           05  C-TOTAL-COST-EST             PIC 9(6)V99 VALUE 0.        
           05  C-GT-PAINT-JOB-CTR           PIC 999     VALUE 0.        
           05  C-GT-GALS-NEEDED             PIC 9(5)V99 VALUE 0.        
           05  C-GT-PAINT-COST-EST          PIC 9(8)V99 VALUE 0.        
           05  C-GT-LABOR-COST-EST          PIC 9(8)V99 VALUE 0.        
           05  C-GT-TOTAL-COST-EST          PIC 9(9)V99 VALUE 0.        
                                                       
       01  CURRENT-DATE-AND-TIME.
           05  I-DATE.
               10  I-YY                		PIC 9(4).
               10  I-MM                		PIC 99.
               10  I-DD                		PIC 99.
           05  I-TIME                  		PIC X(11).
       
	   01  COMPANY-TITLE.
           05  FILLER                  		PIC X(6)    VALUE 'DATE: '. 
           05  O-MM                    		PIC 99.
           05  FILLER                  		PIC X       VALUE '/'.
           05  O-DD                    		PIC 99.
           05  FILLER                  		PIC X       VALUE '/'.
           05  O-YY                    		PIC 9(4).
           05  FILLER                  		PIC X(37)   VALUE ' '.
           05  FILLER                  		PIC X(25)   VALUE 
                                   	       'CHAPMAN''S PAINT ESTIMATOR'.
           05  FILLER                  		PIC X(46)   VALUE ' '.
           05  FILLER                  		PIC X(6)    VALUE 'PAGE: '. 
           05  O-PCTR                  		PIC Z9.         

       01  COLUMN-HEADINGS-1.
          05  FILLER                  	    PIC X(8)    VALUE 
                                                       'ESTIMATE'.
          05  FILLER                  		PIC X(23)   VALUE ' '.
	      05  FILLER	                    PIC X(4)    VALUE 'WALL'.
	      05  FILLER	                    PIC X(7)    VALUE ' '.   
	      05  FILLER	                    PIC X(4)    VALUE 'DOOR'.
	      05  FILLER	                    PIC X(6)    VALUE ' '. 
	      05  FILLER	                    PIC X(5)    VALUE 'TOTAL'.
	      05  FILLER               	        PIC X(6)    VALUE ' '.
	      05  FILLER                        PIC X(7)    VALUE 'GALLONS'.
	      05  FILLER                        PIC X(6)    VALUE ' '.
          05  FILLER                        PIC X(6)    VALUE 'PRICE/'.
          05  FILLER                        PIC X(11)   VALUE ' '.  
          05  FILLER                        PIC X(5)    VALUE 'PAINT'.
          05  FILLER                        PIC X(12)   VALUE ' '.  
          05  FILLER                        PIC X(5)    VALUE 'LABOR'. 
          05  FILLER                        PIC X(12)   VALUE ' '. 
          05  FILLER                        PIC X(5)    VALUE 'TOTAL'.
                                                             
       01  COLUMN-HEADINGS-2.
           05  FILLER                       PIC X(7)    VALUE ' NUMBER'.
           05  FILLER                       PIC X(5)    VALUE ' '.       
           05  FILLER                       PIC X(13)   VALUE 
                                                       'ESTIMATE DATE'.
           05  FILLER                       PIC X(5)    VALUE ' '.
           05  FILLER                       PIC X(5)    VALUE 'SQ/FT'.  
           05  FILLER                  		PIC X(6)    VALUE ' '.
           05  FILLER                  		PIC X(5)    VALUE 'SQ/FT'.
           05  FILLER                  		PIC X(6)    VALUE ' '.
	       05  FILLER                  		PIC X(5)    VALUE 'SQ/FT'.
		   05  FILLER                       PIC X(7)    VALUE ' '.
		   05  FILLER                       PIC X(6)    VALUE 'NEEDED'.
		   05  FILLER                       PIC X(6)    VALUE ' '.
		   05  FILLER                       PIC X(6)    VALUE 'GALLON'.
		   05  FILLER                       PIC X(8)    VALUE ' '.
		   05  FILLER                       PIC X(8)    VALUE 
                                                       'ESTIMATE'.
		   05  FILLER                       PIC X(9)    VALUE ' '.
		   05  FILLER                       PIC X(8)    VALUE 
                                                       'ESTIMATE'.
		   05  FILLER                       PIC X(9)    VALUE ' '.
		   05  FILLER                       PIC X(8)    VALUE 
                                                       'ESTIMATE'.

	   01  BLANK-LINE.
           05  FILLER                       PIC X(132)  VALUE ' '.

       01  DETAIL-LINE.
		   05  FILLER                       PIC XX      VALUE ' '.
           05  O-PAINT-EST-NO               PIC X(4).
		   05  FILLER                       PIC X(7)    VALUE ' '.  
           05  O-PAINT-MM                   PIC 99.
           05  FILLER                       PIC X       VALUE '/'.
           05  O-PAINT-DD                   PIC 99.
		   05  FILLER                       PIC X       VALUE '/'.
           05  O-PAINT-YY                   PIC 9(4).
           05  FILLER                       PIC X(7)    VALUE ' '.
           05  O-PAINT-WALL-SQ-FT           PIC Z,ZZ9.
           05  FILLER                       PIC X(7)    VALUE ' '.
		   05  O-PAINT-DOOR-SQ-FT           PIC ZZ9.
		   05  FILLER                       PIC X(7)    VALUE ' '.
           05  O-WALL-SPACE                 PIC Z,ZZ9.
		   05  FILLER                       PIC X(7)    VALUE ' '.
		   05  O-GALS-NEEDED                PIC ZZZ.99.
		   05  FILLER                       PIC X(7)    VALUE ' '.
		   05  O-PAINT-PRICE-GAL            PIC ZZ.99.
		   05  FILLER                       PIC X(6)    VALUE ' '.
		   05  O-PAINT-COST-EST             PIC $ZZ,ZZZ.99.
		   05  FILLER                       PIC X(7)    VALUE ' '.
		   05  O-LABOR-COST-EST             PIC $ZZ,ZZZ.99.
		   05  FILLER                       PIC X(6)    VALUE ' '.
		   05  O-TOTAL-COST-EST             PIC $ZZZ,ZZZ.99.

       01  TOTAL-LINE.
	       05  FILLER	                    PIC X(13)   VALUE 
                                                       'GRAND TOTALS:'.
           05  FILLER                  	    PIC X(21)   VALUE ' '.
           05  FILLER                  	    PIC X(17)   VALUE 
                                                    'TOTAL ESTIMATES: '.
	       05  O-GT-PAINT-JOBS-CTR          PIC ZZ9.
		   05  FILLER                       PIC X(7)    VALUE ' '.
		   05  O-GT-GALS-NEEDED             PIC ZZ,ZZZ.99.
		   05  FILLER                       PIC X(15)   VALUE ' '.
           05  O-GT-PAINT-COST-EST          PIC $$,$$$,$$$.99.
		   05  FILLER                       PIC X(4)    VALUE ' '.
           05  O-GT-LABOR-COST-EST          PIC $$,$$$,$$$.99.
		   05  FILLER                       PIC XXX     VALUE ' '.
           05  O-GT-TOTAL-COST-EST          PIC $$$,$$$,$$$.99.

	   PROCEDURE DIVISION.
	   0000-MAIN.
		   PERFORM 1000-INIT.
		   PERFORM 2000-MAINLINE
			   UNTIL MORE-RECS = 'NO'.
		   PERFORM 3000-CLOSING.
		   STOP RUN.

	   1000-INIT.
           OPEN INPUT PAINT-MASTER.
           OPEN OUTPUT PRTOUT.

           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.
           MOVE I-YY TO O-YY.
           MOVE I-DD TO O-DD.
           MOVE I-MM TO O-MM.

           PERFORM 9000-READ.
           PERFORM 9100-HEADINGS.

	   9000-READ.
		   READ PAINT-MASTER
			   AT END
				   MOVE 'NO' TO MORE-RECS.

	   9100-HEADINGS.
		   ADD 1 TO C-PCTR.
		   MOVE C-PCTR TO O-PCTR.
		   WRITE PRTLINE FROM COMPANY-TITLE
			   AFTER ADVANCING PAGE.
		   WRITE PRTLINE FROM COLUMN-HEADINGS-1
			   AFTER ADVANCING 2 LINES.
		   WRITE PRTLINE FROM COLUMN-HEADINGS-2
			   AFTER ADVANCING 1 LINE.
		   WRITE PRTLINE FROM BLANK-LINE
			   AFTER ADVANCING 1 LINE.

	   2000-MAINLINE.
		   PERFORM 2100-CALCS.
           PERFORM 2200-OUTPUT.
           PERFORM 9000-READ.
      
	   2100-CALCS.
		   SUBTRACT I-PAINT-DOOR-SQ-FT FROM I-PAINT-WALL-SQ-FT
               GIVING C-WALL-SPACE.
		   DIVIDE C-WALL-SPACE BY 115 GIVING C-GALS-NEEDED ROUNDED.
		   MULTIPLY C-GALS-NEEDED BY I-PAINT-PRICE-GAL GIVING 
               C-PAINT-COST-EST ROUNDED.
		   COMPUTE C-LABOR-COST-EST ROUNDED = 23.55 * 3 * C-GALS-NEEDED.
		   ADD C-PAINT-COST-EST TO C-LABOR-COST-EST
               GIVING C-TOTAL-COST-EST.
		   ADD 1 TO C-GT-PAINT-JOB-CTR.
		   COMPUTE C-GT-GALS-NEEDED = C-GT-GALS-NEEDED + C-GALS-NEEDED.
		   COMPUTE C-GT-PAINT-COST-EST = C-GT-PAINT-COST-EST + 
               C-PAINT-COST-EST.
		   COMPUTE C-GT-LABOR-COST-EST = C-GT-LABOR-COST-EST + 
               C-LABOR-COST-EST.
		   COMPUTE C-GT-TOTAL-COST-EST = C-GT-TOTAL-COST-EST + 
               C-TOTAL-COST-EST.

      * CONVERTS ONLY THE DETAIL/MAINLINE VARIABLES TO ALPANUMERIC *
	   2200-OUTPUT.
		   MOVE I-PAINT-EST-NO TO O-PAINT-EST-NO.
		   MOVE I-PAINT-YY TO O-PAINT-YY.
		   MOVE I-PAINT-DD TO O-PAINT-DD.
		   MOVE I-PAINT-MM TO O-PAINT-MM.
		   MOVE I-PAINT-DOOR-SQ-FT TO O-PAINT-DOOR-SQ-FT.
           MOVE I-PAINT-WALL-SQ-FT TO O-PAINT-WALL-SQ-FT.
		   MOVE C-WALL-SPACE TO O-WALL-SPACE.
		   MOVE C-GALS-NEEDED TO O-GALS-NEEDED.
		   MOVE I-PAINT-PRICE-GAL TO O-PAINT-PRICE-GAL.
		   MOVE C-PAINT-COST-EST TO O-PAINT-COST-EST.
		   MOVE C-LABOR-COST-EST TO O-LABOR-COST-EST.
		   MOVE C-TOTAL-COST-EST TO O-TOTAL-COST-EST.

		   WRITE PRTLINE FROM DETAIL-LINE
       	  	   AFTER ADVANCING 1 LINE
				   AT EOP
					   PERFORM 9100-HEADINGS.

      * CONVERTS ONLY THE GRANDTOTAL VARIABLES TO ALPANUMERIC * 
	   3000-CLOSING.
		   MOVE C-GT-PAINT-JOB-CTR TO O-GT-PAINT-JOBS-CTR.
		   MOVE C-GT-GALS-NEEDED TO O-GT-GALS-NEEDED.
		   MOVE C-GT-PAINT-COST-EST TO O-GT-PAINT-COST-EST.
		   MOVE C-GT-LABOR-COST-EST TO O-GT-LABOR-COST-EST.
		   MOVE C-GT-TOTAL-COST-EST TO O-GT-TOTAL-COST-EST.

		   WRITE PRTLINE FROM TOTAL-LINE
			   AFTER ADVANCING 3 LINES.
		   CLOSE PAINT-MASTER.
		   CLOSE PRTOUT.

	  





