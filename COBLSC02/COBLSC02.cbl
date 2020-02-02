       IDENTIFICATION DIVISION.
	   PROGRAM-ID.    COBLSC02.
	   AUTHOR.        LUCAS CHAPMAN.
	   DATE-WRITTEN.  12/11/2019.
	   DATE-COMPILED.
      ******************************************************************
      * THIS PROGRAM READS A FILE AND CREATES *
      * A BOATS INC. REPORT.                  *
      ************************************************************
	   ENVIRONMENT DIVISION.
	   INPUT-OUTPUT SECTION.
	   FILE-CONTROL.

           SELECT BOAT-MASTER
               ASSIGN TO 'C:\IHCC\COBOL\COBLSC02\CBLBOAT1.DAT'
			   ORGANIZATION IS LINE SEQUENTIAL.

		   SELECT PRTOUT
			   ASSIGN TO 'C:\IHCC\COBOL\COBLSC02\BOATPRT.PRT'
			   ORGANIZATION IS RECORD SEQUENTIAL.

	   DATA DIVISION.
	   FILE SECTION.

       FD  BOAT-MASTER
           LABEL RECORD IS STANDARD
		   DATA RECORD IS I-REC
		   RECORD CONTAINS 42 CHARACTERS.

       01  I-REC.
           05  I-LAST-NAME              PIC X(15).
		   05  I-STATE                  PIC XX.
		   05  I-BOAT-COST              PIC 9(6)V99.
		   05  I-PURCHASE-DATE.         
			   10  I-PURCHASE-DATE-YY   PIC 9(4).
			   10  I-PURCHASE-DATE-MM   PIC 9(2).
			   10  I-PURCHASE-DATE-DD   PIC 9(2).
		   05  I-BOAT-TYPE              PIC X.
		   05  I-ACCESSORY-PACKAGE      PIC 9.
		   05  I-PREP-DELIVER-COST      PIC 9(5)V99.

       FD  PRTOUT
		   LABEL RECORD IS OMITTED
		   RECORD CONTAINS 132 CHARACTERS
		   DATA RECORD IS PRTLINE
		   LINAGE IS 60 WITH FOOTING AT 56.

	   01  PRTLINE                     PIC X(132).

	   WORKING-STORAGE SECTION.
	   01  WORK-AREA.
           05  C-PCTR                  PIC 99      VALUE 0.
           05  MORE-RECS               PIC XXX     VALUE 'YES'.
		   05  H-BOAT-TYPE             PIC X.
           05  C-TOTAL-COST            PIC 9(7)V99 VALUE 0.
		   05  C-MJ-SALES-CTR          PIC 9(4)    VALUE 0.
		   05  C-MJ-TOTAL-COST         PIC 9(9)V99 VALUE 0.
		   05  C-GT-SALES-CTR          PIC 9(5)    VALUE 0.
		   05  C-GT-TOTAL-COST         PIC 9(9)V99 VALUE 0.
       
	   01  CURRENT-DATE-AND-TIME.
		   05  I-DATE.
			   10  I-YY                PIC 9(4).
			   10  I-MM                PIC 99.
			   10  I-DD                PIC 99.
		   05  I-TIME                  PIC X(11).

       01  COMPANY-TITLE.
		   05  FILLER                  PIC X(6)    VALUE 'DATE: '.
		   05  O-MM                    PIC 99.
		   05  FILLER                  PIC X       VALUE '/'.
		   05  O-DD                    PIC 99.
		   05  FILLER                  PIC X       VALUE '/'.
		   05  O-YY                    PIC 9(4).
		   05  FILLER                  PIC X(40)   VALUE ' '.
		   05  FILLER                  PIC X(19)   VALUE 
                                                 'CHAPMAN''S BOAT INC.'.
		   05  FILLER                  PIC X(49)   VALUE ' '.
		   05  FILLER                  PIC X(6)    VALUE 'PAGE: '.
		   05  O-PCTR                  PIC Z9.

       01  COLUMN-HEADINGS-1.
		   05  FILLER                  PIC X(8)    VALUE 'CUSTOMER'.
		   05  FILLER                  PIC X(36)   VALUE ' '.
		   05  FILLER                  PIC X(4)    VALUE 'BOAT'.
		   05  FILLER                  PIC X(9)    VALUE ' '.
		   05  FILLER                  PIC X(8)    VALUE 'PURCHASE'.
		   05  FILLER                  PIC X(11)   VALUE ' '.
		   05  FILLER                  PIC X(9)    VALUE 'ACCESSORY'.
		   05  FILLER                  PIC X(21)   VALUE ' '.
		   05  FILLER                  PIC X(4)    VALUE 'PREP'.
		   05  FILLER                  PIC X(17)   VALUE ' '.
           05  FILLER                  PIC X(5)    VALUE 'TOTAL'.

       01  COLUMN-HEADINGS-2.
		   05  FILLER                  PIC X(9)    VALUE 'LAST NAME'.
		   05  FILLER                  PIC X(35)   VALUE ' '.
		   05  FILLER                  PIC X(4)    VALUE 'COST'.
		   05  FILLER                  PIC X(9)    VALUE ' '.
		   05  FILLER                  PIC X(4)    VALUE 'DATE'.
		   05  FILLER                  PIC X(15)   VALUE ' '.
		   05  FILLER                  PIC X(7)    VALUE 'PACKAGE'.
		   05  FILLER                  PIC X(23)   VALUE ' '.
		   05  FILLER                  PIC X(4)    VALUE 'COST'.
		   05  FILLER                  PIC X(18)   VALUE ' '.
           05  FILLER                  PIC X(4)    VALUE 'COST'.

      * BOAT-TYPE LINE THAT FOLLOWS EVERY CONTROL BREAK *
	   01  COLUMN-HEADINGS-3.
		   05  FILLER                  PIC X(11)   VALUE 'BOAT TYPE: '.
		   05  O-BOAT-TYPE             PIC X(13).
		   05  FILLER                  PIC X(108)  VALUE ' '.

      * BOAT-TYPE LINE THAT FOLLOWS EVERY NEW PAGE *
	   01  PAGE-COLUMN-HEADINGS-3.
		   05  FILLER                  PIC X(11)   VALUE 'BOAT TYPE: '.
		   05  O-PAGE-BOAT-TYPE        PIC X(13).
		   05  FILLER                  PIC X(108)  VALUE ' '.

	   01  BLANK-LINE.
           05  FILLER                  PIC X(132)  VALUE ' '.

	   01  DETAIL-LINE.
		   05  O-LAST-NAME             PIC X(16).
		   05  FILLER                  PIC X(8)    VALUE ' '.
		   05  O-STATE                 PIC XX.
		   05  FILLER                  PIC X(12)   VALUE ' '.
		   05  O-BOAT-COST             PIC ZZZ,ZZZ.99.
		   05  FILLER                  PIC X(9)    VALUE ' '.
		   05  O-PURCHASE-DATE-MM      PIC 99.
		   05  FILLER                  PIC X       VALUE '/'.
		   05  O-PURCHASE-DATE-DD      PIC 99.
		   05  FILLER                  PIC X       VALUE '/'.
		   05  O-PURCHASE-DATE-YY      PIC 99.
		   05  FILLER                  PIC X(11)   VALUE ' '.
		   05  O-ACCESSORY-PACKAGE     PIC X(15).
		   05  FILLER                  PIC X(9)    VALUE ' '.
		   05  O-PREP-DELIVER-COST     PIC ZZZ,ZZZ.99.
		   05  FILLER                  PIC X(10)   VALUE ' '.
		   05  O-TOTAL-COST            PIC Z,ZZZ,ZZZ.99.

      * O-MJ-BOAT-TYPE IS USED FOR EVERY 'SUBTOTALS FOR' *
	   01  MAJOR-SUBTOTALS.
		   05  FILLER                  PIC X(23)   VALUE ' '.
		   05  FILLER                  PIC X(14)   VALUE 
                                                  'SUBTOTALS FOR '.
		   05  O-MJ-BOAT-TYPE          PIC X(13).
		   05  FILLER                  PIC X(10)   VALUE ' '.
		   05  FILLER                  PIC X(14)   VALUE 
                                                  'NUMBER SOLD:  '.
		   05  O-MJ-SALES-CTR          PIC Z,ZZ9.
		   05  FILLER                  PIC X(38)   VALUE ' '.
		   05  O-MJ-TOTAL-COST         PIC $$$$,$$$,$$$.99.

	   01  GRAND-TOTALS.
		   05  FILLER                  PIC X(23)   VALUE ' '.
		   05  FILLER                  PIC X(12)   VALUE 'GRAND TOTALS'.
		   05  FILLER                  PIC X(25)   VALUE ' '.
		   05  FILLER                  PIC X(13)   VALUE 
                                                  'NUMBER SOLD: '.
		   05  O-GT-SALES-CTR          PIC ZZ,ZZ9.
		   05  FILLER                  PIC X(35)   VALUE ' '.
		   05  O-GT-TOTAL-COST         PIC $$$,$$$,$$$,$$$.99.

	   PROCEDURE DIVISION.
	   0000-MAIN.
		   PERFORM 1000-INIT.
		   PERFORM 2000-MAINLINE
			   UNTIL MORE-RECS = 'NO'.
		   PERFORM 3000-CLOSING.
		   STOP RUN.

       1000-INIT.
           OPEN INPUT BOAT-MASTER.
           OPEN OUTPUT PRTOUT.

		   MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.
		   MOVE I-YY TO O-YY.
		   MOVE I-DD TO O-DD.
		   MOVE I-MM TO O-MM.

      * READ IS CALLED BEFORE HEADINGS TO PRINT PAGE-COLUMN-HEADINGS-3 *
		   PERFORM 9100-READ.
           PERFORM 9200-HEADINGS.

           MOVE O-PAGE-BOAT-TYPE TO O-MJ-BOAT-TYPE.
	       MOVE I-BOAT-TYPE TO H-BOAT-TYPE.

	   2000-MAINLINE.
		   IF I-BOAT-TYPE NOT = H-BOAT-TYPE
			   PERFORM 9000-MAJORSUBTOTALS
		   PERFORM 2100-COLUMN-HEADINGS-3.
		   PERFORM 2200-CALCS.
           PERFORM 2300-OUTPUT.
           PERFORM 9100-READ.

      * PRINTS BOAT TYPE LINE FOR EVERY CONTROL BREAK *
	   2100-COLUMN-HEADINGS-3.
      * FOR COLUMN-HEADINGS-3 ONLY *
			EVALUATE I-BOAT-TYPE
			   WHEN 'B'
				   MOVE 'BASS BOAT' TO O-BOAT-TYPE 
			   WHEN 'P'
				   MOVE 'PONTOON' TO O-BOAT-TYPE
			   WHEN 'S'
				   MOVE 'SKI BOAT' TO O-BOAT-TYPE
			   WHEN 'J'
				   MOVE 'JOHN BOAT' TO O-BOAT-TYPE
			   WHEN 'C'
				   MOVE 'CANOE' TO O-BOAT-TYPE
			   WHEN OTHER
                   MOVE 'CABIN CRUSIER' TO O-BOAT-TYPE.

			MOVE O-BOAT-TYPE TO O-MJ-BOAT-TYPE.

		    WRITE PRTLINE FROM COLUMN-HEADINGS-3
			   AFTER ADVANCING 2 LINES
			WRITE PRTLINE FROM BLANK-LINE
			   AFTER ADVANCING 1 LINE.

      * CALCULATES ONLY C-TOTAL-COST AND THE MAJORSUBTOTAL VARIABLES *
       2200-CALCS.
		   ADD I-BOAT-COST TO I-PREP-DELIVER-COST GIVING C-TOTAL-COST.

		   ADD 1 TO C-MJ-SALES-CTR.
		   ADD C-TOTAL-COST TO C-MJ-TOTAL-COST.

      * CONVERTS ONLY THE DETAIL/MAINLINE VARIABLES TO ALPANUMERIC *  
	   2300-OUTPUT.
		   IF I-ACCESSORY-PACKAGE = '1'
			   MOVE 'ELECTRONICS' TO O-ACCESSORY-PACKAGE
		   ELSE
			   IF I-ACCESSORY-PACKAGE = '2'
				   MOVE 'SKI PACKAGE' TO O-ACCESSORY-PACKAGE
			   ELSE
				   MOVE 'FISHING PACKAGE' TO O-ACCESSORY-PACKAGE.

		   MOVE I-LAST-NAME TO O-LAST-NAME.
		   MOVE I-STATE TO O-STATE.
		   MOVE I-BOAT-COST TO O-BOAT-COST.
		   MOVE I-PURCHASE-DATE-YY TO O-PURCHASE-DATE-YY.
		   MOVE I-PURCHASE-DATE-MM TO O-PURCHASE-DATE-MM.
		   MOVE I-PURCHASE-DATE-DD TO O-PURCHASE-DATE-DD.
		   MOVE I-PREP-DELIVER-COST TO O-PREP-DELIVER-COST.
		   MOVE C-TOTAL-COST TO O-TOTAL-COST.

		   WRITE PRTLINE FROM DETAIL-LINE
			   AFTER ADVANCING 1 LINE
				   AT EOP
					   PERFORM 9200-HEADINGS.

      * CONVERTS ONLY GRAND TOTAL VARIABLES TO ALPHANUMERIC AND CALLS *
      * MAJORSUBTOTALS TO FINISH CALCULATIONS *
	   3000-CLOSING.
		   PERFORM 9000-MAJORSUBTOTALS

           MOVE C-GT-SALES-CTR TO O-GT-SALES-CTR.
		   MOVE C-GT-TOTAL-COST TO O-GT-TOTAL-COST.
           
		   WRITE PRTLINE FROM GRAND-TOTALS
			   AFTER ADVANCING 3 LINES.

		   CLOSE BOAT-MASTER.
		   CLOSE PRTOUT.

      * ONLY MAJOR SUBTOTALS ARE CONVERTED TO ALPHANUMERIC AND ADDS *
      * GRAND TOTAL VARIABLES *
	   9000-MAJORSUBTOTALS.
           MOVE C-MJ-SALES-CTR TO O-MJ-SALES-CTR.
		   MOVE C-MJ-TOTAL-COST TO O-MJ-TOTAL-COST.
           
		   WRITE PRTLINE FROM MAJOR-SUBTOTALS
			   AFTER ADVANCING 2 LINES
				   AT EOP
					   PERFORM 9200-HEADINGS.

		   ADD C-MJ-SALES-CTR TO C-GT-SALES-CTR.
		   ADD C-MJ-TOTAL-COST TO C-GT-TOTAL-COST.

		   MOVE 0 TO C-MJ-SALES-CTR.
		   MOVE 0 TO C-MJ-TOTAL-COST.

		   MOVE I-BOAT-TYPE TO H-BOAT-TYPE.

	   9100-READ.
		   READ BOAT-MASTER
			   AT END
				   MOVE 'NO' TO MORE-RECS.

	   9200-HEADINGS.
		   ADD 1 TO C-PCTR.
		   MOVE C-PCTR TO O-PCTR.
      * FOR PAGE-COLUMN-HEADINGS-3 ONLY *
			EVALUATE I-BOAT-TYPE
			   WHEN 'B'
				   MOVE 'BASS BOAT' TO O-PAGE-BOAT-TYPE 
			   WHEN 'P'
				   MOVE 'PONTOON' TO O-PAGE-BOAT-TYPE
			   WHEN 'S'
				   MOVE 'SKI BOAT' TO O-PAGE-BOAT-TYPE
			   WHEN 'J'
				   MOVE 'JOHN BOAT' TO O-PAGE-BOAT-TYPE
			   WHEN 'C'
				   MOVE 'CANOE' TO O-PAGE-BOAT-TYPE
			   WHEN OTHER
				   MOVE 'CABIN CRUSIER' TO O-PAGE-BOAT-TYPE. 

		   WRITE PRTLINE FROM COMPANY-TITLE
			   AFTER ADVANCING PAGE
		   WRITE PRTLINE FROM COLUMN-HEADINGS-1
			   AFTER ADVANCING 2 LINES
		   WRITE PRTLINE FROM COLUMN-HEADINGS-2
			   AFTER ADVANCING 1 LINE
      * PAGE-COLUMN-HEADINGS-3 PRINTS BOAT TYPE LINE FOR EVERY NEW *
      * PAGE *
           WRITE PRTLINE FROM PAGE-COLUMN-HEADINGS-3
			   AFTER ADVANCING 2 LINE
		   WRITE PRTLINE FROM BLANK-LINE
			   AFTER ADVANCING 1 LINE.
