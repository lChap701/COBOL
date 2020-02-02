       IDENTIFICATION DIVISION.
       PROGRAM-ID     COBLSC04.
       AUTHOR.        LUCAS CHAPMAN.
       DATE-WRITTEN.  1/12/2020.
      ******************************************************************
      * THIS PROGRAM READS A FILE AND CREATES  A ORARK CONDO BILLING *
      * REPORT WITH DEALS THAT INCLUDE ANY ADDITIONAL FEES FOR PETS  *
      * AND HOTTUBS, FREE NIGHTS PAIDED FOR BY NIGHT FEES (INCLUDING *
      * REDUCING THE PRICE OF STAYING A NIGHT BY HALF), AND FREE     *
      * CLEANING FOR CERTAIN CONDOS WITH A GRAND TOTAL FOR ALL EIGHT *
      * DIFFERENT CONDOS INCLUDED IN THIS BILLING REPORT             *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT OZARK-MASTER
               ASSIGN TO 'C:\IHCC\COBOL\COBLSC04\OZARK.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT PRTOUT
               ASSIGN TO 'C:\IHCC\COBOL\COBLSC04\VACATION.PRT'
               ORGANIZATION IS RECORD SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  OZARK-MASTER
           LABEL RECORD IS STANDARD
           DATA RECORD IS I-REC
           RECORD CONTAINS 31 CHARACTERS.

       01  I-REC.
           05  I-GUEST                 PIC X(20).
           05  I-CONDO                 PIC XX.
           05  I-BEDROOMS              PIC 9.
           05  I-NIGHTS                PIC 99.
           05  I-PETS                  PIC X.
           05  I-HOTTUB                PIC X.
           05  I-DOCKSLIP              PIC 99V99.

       FD  PRTOUT
		   LABEL RECORD IS OMITTED
		   RECORD CONTAINS 132 CHARACTERS
		   DATA RECORD IS PRTLINE
		   LINAGE IS 60 WITH FOOTING AT 56.

       01  PRTLINE                     PIC X(132).

       WORKING-STORAGE SECTION.
       01  WORK-AREA.
           05  C-PCTR                  PIC 99          VALUE 0.
           05  MORE-RECS               PIC XXX         VALUE 'YES'.
           05  C-BEDROOM-CHARGE        PIC 9(3)V99     VALUE 0.
           05  C-CLEAN-FEE             PIC 9(3)V99     VALUE 0.
           05  C-NIGHT-FEE             PIC 9(4)V99     VALUE 0.
           05  C-CONDO-FEE             PIC 9(6)V99     VALUE 0.
           05  C-DOCKSLIP-FEE          PIC 9(4)V99     VALUE 0.
           05  C-SUBTOTAL              PIC 9(6)V99     VALUE 0.
           05  C-PET-FEE               PIC 9(5)V99     VALUE 0.
           05  C-HOTTUB-FEE            PIC 9(4)V99     VALUE 0.
           05  C-FREE-NIGHT-CTR        PIC 99V9        VALUE 0.
           05  C-FREE-CLEAN-CTR        PIC 99          VALUE 0.
           05  C-ACC-DEALS             PIC S9(5)V99    VALUE 0.
           05  C-TOTAL                 PIC 9(6)V99     VALUE 0.
           05  C-GT-SUBTOTAL           PIC 9(8)V99     VALUE 0.
           05  C-GT-ACC-DEALS          PIC S9(7)V99    VALUE 0.
           05  C-GT-TOTAL              PIC 9(8)V99     VALUE 0.
           05  C-GT-RENTAL-CTR         PIC 999         VALUE 0.
           05  C-GT-FREE-NIGHT-CTR     PIC 99V9        VALUE 0.
           05  C-GT-FREE-CLEAN-CTR     PIC 99          VALUE 0.
           05  C-GT-PET-FEE            PIC 9(5)V99     VALUE 0.
           05  C-GT-HOTTUB-FEE         PIC 9(5)V99     VALUE 0.

       01  CURRENT-DATE-AND-TIME.
		   05  I-DATE.
			   10  I-YY                PIC 9(4).
			   10  I-MM                PIC 99.
			   10  I-DD                PIC 99.
		   05  I-TIME                  PIC X(11).
      *  PRINTS THE CURRENT DATE IN MONTH/DAY/YEAR FORMAT  *
       01  COMPANY-TITLE-LINE.
		   05  FILLER                  PIC X(6)      VALUE 'DATE: '.
		   05  O-MM                    PIC 99.
		   05  FILLER                  PIC X         VALUE '/'.
		   05  O-DD                    PIC 99. 
		   05  FILLER                  PIC X         VALUE '/'.
		   05  O-YY                    PIC 9(4).
		   05  FILLER                  PIC X(42)     VALUE ' '.
		   05  FILLER                  PIC X(14)     VALUE 
                                                    'PMG MANAGEMENT'.
		   05  FILLER                  PIC X(52)     VALUE ' '.
		   05  FILLER                  PIC X(6)      VALUE 'PAGE: '.
           05  O-PCTR                  PIC Z9.
      *  PRINTS THE CURRENT MONTH ONLY WITH NAME OF MONTH INCLUDED  *
      *  MEANING JANUARY IS PRINTED INSTEAD OF A 1 OR JAN           *
       01  OZARKS-HEADING-LINE.
           05  FILLER                  PIC X(8)      VALUE 'COBLSC04'.
           05  FILLER                  PIC X(42)     VALUE ' '.
           05  FILLER                  PIC X(21)     VALUE 
                                                'LAKE OF THE OZARKS - '.
           05  O-HEADING-MM            PIC X(9).
           05  FILLER                  PIC X(52)     VALUE ' '.

       01  COLUMN-HEADINGS-LINE-1.
           05  FILLER                  PIC X(38)     VALUE ' '.
           05  FILLER                  PIC X(4)      VALUE 'STAY'.
           05  FILLER                  PIC X(6)      VALUE ' '.
           05  FILLER                  PIC X(5)      VALUE 'NIGHT'.
           05  FILLER                  PIC X(7)      VALUE ' '.
           05  FILLER                  PIC X(6)      VALUE 'CONDO '.
           05  FILLER                  PIC X(10)     VALUE 'CLEANING  '.
           05  FILLER                  PIC X(9)      VALUE 'DOCK SLIP'.
           05  FILLER                  PIC X(23)     VALUE ' '.
           05  FILLER                  PIC X(4)      VALUE 'DEAL'.
           05  FILLER                  PIC X(10)     VALUE ' '.
           05  FILLER                  PIC X(6)      VALUE 'AMOUNT'.
           05  FILLER                  PIC X(4)      VALUE ' '.

       01  COLUMN-HEADINGS-LINE-2.
           05  FILLER                  PIC X(11)     VALUE 
                                                    'CONDOMINIUM'.
           05  FILLER                  PIC X(6)      VALUE ' '.
           05  FILLER                  PIC X(10)     VALUE 'GUEST NAME'.
           05  FILLER                  PIC X(10)     VALUE ' '. 
           05  FILLER                  PIC X(6)      VALUE 'NIGHTS'.
           05  FILLER                  PIC X(7)      VALUE ' '.
           05  FILLER                  PIC X(3)      VALUE 'FEE'.
           05  FILLER                  PIC X(9)      VALUE ' '.
           05  FILLER                  PIC X(3)      VALUE 'FEE'.
           05  FILLER                  PIC X(6)      VALUE ' '.
           05  FILLER                  PIC X(3)      VALUE 'FEE'.
           05  FILLER                  PIC X(8)      VALUE ' '.
           05  FILLER                  PIC X(3)      VALUE 'FEE'.
           05  FILLER                  PIC X(5)      VALUE ' '.
           05  FILLER                  PIC X(8)      VALUE 'SUBTOTAL'.
           05  FILLER                  PIC X(9)      VALUE ' '.
           05  FILLER                  PIC X(6)      VALUE 'AMOUNT'.
           05  FILLER                  PIC X(12)     VALUE ' '.
           05  FILLER                  PIC X(7)      VALUE 'DUE    '.

       01  BLANK-LINE.
           05  FILLER                  PIC X(132)    VALUE ' '.

       01  DETAIL-LINE.
           05  O-CONDO                 PIC X(15).
           05  FILLER                  PIC XX        VALUE ' '.
           05  O-GUEST                 PIC X(20).
           05  FILLER                  PIC XX        VALUE ' '.
           05  O-NIGHTS                PIC Z9.
           05  FILLER                  PIC XXX       VALUE ' '.
           05  O-NIGHT-FEE             PIC $$,$$$.99.
           05  FILLER                  PIC X         VALUE ' '.
           05  O-CONDO-FEE             PIC $$$$,$$$.99.
           05  FILLER                  PIC XX        VALUE ' '.
           05  O-CLEAN-FEE             PIC $$$$.99.
           05  FILLER                  PIC XX        VALUE ' '.
           05  O-DOCKSLIP-FEE          PIC $$,$$$.99.
           05  FILLER                  PIC XX        VALUE ' '.
           05  O-SUBTOTAL              PIC $$$$,$$$.99.
           05  FILLER                  PIC X(4)      VALUE ' '.
           05  O-ACC-DEALS             PIC $$$,$$$.99+.
           05  FILLER                  PIC X(4)      VALUE ' '.
           05  O-TOTAL                 PIC $$$$,$$$.99.
           05  O-ASTERISKS             PIC X(4).
      *  GRAND TOTAL LINE THAT PRINTS THE SUBTOTAL, DEALS, AND THE  *
      *  TOTAL AMOUNT FOR ALL CONDOS IN THE DAT FILE  *
       01  GRAND-TOTALS-LINE-1.
           05  FILLER                  PIC X(13)     VALUE 
                                                    'GRAND TOTALS:'.
           05  FILLER                  PIC X(71)     VALUE ' '.
           05  O-GT-SUBTOTAL           PIC $$$,$$$,$$$.99.
           05  FILLER                  PIC X         VALUE ' '.
           05  O-GT-ACC-DEALS          PIC $$,$$$,$$$.99+.
           05  FILLER                  PIC X         VALUE ' '.
           05  O-GT-TOTAL              PIC $$$,$$$,$$$.99.
           05  FILLER                  PIC X(4)      VALUE ' '.
      *  PRINTS A CTR FOR NUMBER OF CONDOS READ IN (CALLED RENTALS),  *
      *  A CTR FOR THE NUMBER OF GUESTS WHO RECIEVED A FREE NIGHTS OR *
      *  RECIVED A FIFTY PERCENT DISCOUNT FOR A NIGHT, AND A CTR FOR  *
      *  FREE CLEANING FOR A NIGHT  *
       01  GRAND-TOTALS-LINE-2.
           05  FILLER                  PIC X(12)     VALUE ' '.
           05  FILLER                  PIC X(19)     VALUE 
                                                  'NUMBER OF RENTALS: '.
           05  O-GT-RENTAL-CTR         PIC ZZ9.
           05  FILLER                  PIC X(6)      VALUE ' '.
           05  FILLER                  PIC X(13)     VALUE 
                                                    'FREE NIGHTS: '.
           05  O-GT-FREE-NIGHT-CTR     PIC ZZ.9.
           05  FILLER                  PIC X(18)     VALUE 
                                                   '   FREE CLEANING: '.
           05  O-GT-FREE-CLEAN-CTR     PIC Z9.
           05  FILLER                  PIC X(55)     VALUE ' '.
      *  GRAND TOTAL LINE THAT PRINTS THE ADDITIONAL FEES *
       01  GRAND-TOTALS-LINE-3.
           05  FILLER                  PIC X(14)     VALUE ' '.
           05  FILLER                  PIC X(10)     VALUE 
                                                    'PET FEES: '.
           05  O-GT-PET-FEE            PIC $$$,$$$.99.
           05  FILLER                  PIC X(19)     VALUE ' '.
           05  FILLER                  PIC X(14)     VALUE 
                                                    'HOT TUB FEES: '.
           05  O-GT-HOTTUB-FEE         PIC $$$,$$$.99.
           05  FILLER                  PIC X(55)     VALUE ' '.

       PROCEDURE DIVISION.
	   0000-MAIN.
		   PERFORM 1000-INIT.
		   PERFORM 2000-MAINLINE
			   UNTIL MORE-RECS = 'NO'.
		   PERFORM 3000-CLOSING.
		   STOP RUN.

       1000-INIT.
           OPEN INPUT OZARK-MASTER.
           OPEN OUTPUT PRTOUT.

           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.
		   MOVE I-YY TO O-YY.
		   MOVE I-DD TO O-DD.
	       MOVE I-MM TO O-MM.
      *  EVALUATE STATEMENT THAT CONVERTS THE CURRENT MONTH FROM  *
      *  NUMERIC TO ALPHANUMERIC  *
           EVALUATE I-MM
               WHEN 1
                   MOVE 'JANUARY' TO O-HEADING-MM
               WHEN 2 
                   MOVE 'FEBRUARY' TO O-HEADING-MM
               WHEN 3
                   MOVE 'MARCH' TO O-HEADING-MM
               WHEN 4
                   MOVE 'APRIL' TO O-HEADING-MM
               WHEN 5
                   MOVE 'MAY' TO O-HEADING-MM
               WHEN 6
                   MOVE 'JUNE' TO O-HEADING-MM
               WHEN 7
                   MOVE 'JULY' TO O-HEADING-MM
               WHEN 8
                   MOVE 'AUGUST' TO O-HEADING-MM
               WHEN 9
                   MOVE 'SEPTEMBER' TO O-HEADING-MM
               WHEN 10
                   MOVE 'OCTOBER' TO O-HEADING-MM
               WHEN 11
                   MOVE 'NOVEMBER' TO O-HEADING-MM
               WHEN 12
                   MOVE 'DECEMBER' TO O-HEADING-MM
               WHEN OTHER
                   MOVE 'ERROR' TO O-HEADING-MM.

           PERFORM 9000-HEADINGS.
           PERFORM 9100-READ.

       2000-MAINLINE.
           PERFORM 2100-CALCS.
           PERFORM 2200-OUTPUT.
           PERFORM 9100-READ.

       2100-CALCS.
      *  FINDS THE NAME OF CONDO AND CALCULATES FEES/CHARGES, FREE  *
      *  NIGHTS/CLEANING   *
           EVALUATE I-CONDO
               WHEN 'HB'
                   MOVE 99.50 TO C-BEDROOM-CHARGE
                   MOVE 100 TO C-CLEAN-FEE
                   MOVE 'HORSESHOE BEND' TO O-CONDO
                   MOVE 0 TO C-ACC-DEALS

                   MULTIPLY C-BEDROOM-CHARGE BY I-BEDROOMS GIVING 
                       C-NIGHT-FEE ROUNDED
                   MULTIPLY C-NIGHT-FEE BY I-NIGHTS GIVING 
                       C-CONDO-FEE ROUNDED
     
                   IF I-PETS = 'Y' AND I-HOTTUB = 'Y'
                       MULTIPLY C-CONDO-FEE BY 0.10 GIVING C-PET-FEE 
                           ROUNDED
                       MULTIPLY C-CONDO-FEE BY 0.05 GIVING C-HOTTUB-FEE 
                           ROUNDED
                       ADD C-PET-FEE C-HOTTUB-FEE TO C-ACC-DEALS
                   ELSE
                       MOVE 0 TO C-PET-FEE
                       MOVE 0 TO C-HOTTUB-FEE
                   
                   IF I-PETS = 'Y'
                       MULTIPLY C-CONDO-FEE BY 0.10 GIVING C-PET-FEE 
                           ROUNDED
                       ADD C-PET-FEE TO C-ACC-DEALS
                   ELSE
                       MOVE 0 TO C-PET-FEE
                   
                   IF I-HOTTUB ='Y'
                       MULTIPLY C-CONDO-FEE BY 0.05 GIVING C-HOTTUB-FEE 
                           ROUNDED
                       ADD C-HOTTUB-FEE TO C-ACC-DEALS
                   ELSE
                       MOVE 0 TO C-HOTTUB-FEE
      *  NO FEES FOR USING THE HOTTUB, BUT FEES FOR BRING PETS AND THE *
      *  SEVENTH NIGHT IS FREE  *
               WHEN 'OB'
                   MOVE 188 TO C-BEDROOM-CHARGE
                   MOVE 150 TO C-CLEAN-FEE
                   MOVE 'OSAGE BEACH' TO O-CONDO
      *  CLEARS ANY DEALS FROM THIS CONDO AND THE PREVIOUS CONDO  *
                   MOVE 0 TO C-ACC-DEALS
                   MOVE 0 TO C-HOTTUB-FEE
                   MOVE 0 TO C-PET-FEE
      *  CLEARS THE CTR TO AVOID ADDING EXTRA NUMBERS TO GRAND TOTALS  *
                   MOVE 0 TO C-FREE-NIGHT-CTR
      
                   MULTIPLY C-BEDROOM-CHARGE BY I-BEDROOMS GIVING 
                       C-NIGHT-FEE ROUNDED
                   MULTIPLY C-NIGHT-FEE BY I-NIGHTS GIVING 
                       C-CONDO-FEE ROUNDED
      *  DEAL FOR ANY GUESTS THAT STAYS FOR SEVEN OR MORE NIGHTS  *     
                   IF I-NIGHTS >= 7
                       COMPUTE C-ACC-DEALS = (0  - C-NIGHT-FEE) + 
                           C-ACC-DEALS
                       ADD 1 TO C-FREE-NIGHT-CTR
                                                                 
                   IF I-PETS = 'Y' AND I-HOTTUB = 'Y'
                       MULTIPLY C-CONDO-FEE BY 0.10 GIVING C-PET-FEE
                           ROUNDED
                       ADD C-PET-FEE TO C-ACC-DEALS
                   ELSE
                       MOVE 0 TO C-PET-FEE
      
                   IF I-PETS = 'Y'
                       MULTIPLY C-CONDO-FEE BY 0.10 GIVING 
                           C-PET-FEE ROUNDED
                       ADD C-PET-FEE TO C-ACC-DEALS
                   ELSE
                       MOVE 0 TO C-PET-FEE
      *  NO DEALS OR ADDITIONAL FEES FOR THIS CONDO  *
               WHEN 'PP'
                   MOVE 50 TO C-BEDROOM-CHARGE
                   MOVE 75 TO C-CLEAN-FEE
                   MOVE 'PISTOL POINT' TO O-CONDO
      *  CLEARS ANY DEALS FROM THIS CONDO AND PREVIOUS CONDO  *
                   MOVE 0 TO C-ACC-DEALS
                   MOVE 0 TO C-PET-FEE

                   MULTIPLY C-BEDROOM-CHARGE BY I-BEDROOMS GIVING 
                       C-NIGHT-FEE ROUNDED
                   MULTIPLY C-NIGHT-FEE BY I-NIGHTS GIVING 
                       C-CONDO-FEE ROUNDED
      *  ONLY DEAL IS FOR FREE CLEANING  *
               WHEN 'RB'
                   MOVE 62.10 TO C-BEDROOM-CHARGE
                   MOVE 75 TO C-CLEAN-FEE
                   MOVE 'REGATTA BAY' TO O-CONDO
      *  CLEARS DATA PUT IN CTR FROM PREVIOUS CONDO  * 
                   MOVE 0 TO C-FREE-CLEAN-CTR
      *  CLEARS ANY DEALS FROM THIS CONDO AND PREVIOUS CONDO  *
                   MOVE 0 TO C-ACC-DEALS

                   MULTIPLY C-BEDROOM-CHARGE BY I-BEDROOMS GIVING 
                       C-NIGHT-FEE ROUNDED
                   MULTIPLY C-NIGHT-FEE BY I-NIGHTS GIVING 
                       C-CONDO-FEE ROUNDED
      *  DEAL FOR ANY GUEST THAT STAYS OVER FIVE NIGHTS  *
                   IF I-NIGHTS > 5
                       ADD 1 TO C-FREE-CLEAN-CTR
                       COMPUTE C-ACC-DEALS = (0  - C-CLEAN-FEE) + 
                           C-ACC-DEALS
      * ONLY DEAL IS ADDITIONAL FEES FOR BRING PETS  *
               WHEN 'SB'
                   MOVE 100 TO C-BEDROOM-CHARGE
                   MOVE 150 TO C-CLEAN-FEE
                   MOVE 'SHAWNEE BEND' TO O-CONDO
      *  CLEARS ANY DEALS FROM THIS CONDO AND PREVIOUS CONDO  *
                   MOVE 0 TO C-ACC-DEALS
      *  CLEARS THE CTR TO AVOID ADDING EXTRA NUMBERS TO GRAND TOTALS  *
                   MOVE 0  TO C-FREE-CLEAN-CTR

                   MULTIPLY C-BEDROOM-CHARGE BY I-BEDROOMS GIVING 
                       C-NIGHT-FEE ROUNDED
                   MULTIPLY C-NIGHT-FEE BY I-NIGHTS GIVING 
                       C-CONDO-FEE ROUNDED

                   IF I-PETS = 'Y'
                       MULTIPLY C-CONDO-FEE BY 0.10 GIVING C-PET-FEE 
                           ROUNDED
                       ADD C-PET-FEE TO C-ACC-DEALS
                   ELSE
                       MOVE 0 TO C-PET-FEE
      *  ONLY DEAL IS ADDITIONAL FEES FOR USING HOTTUB  * 
               WHEN 'L'
                   MOVE 76.35 TO C-BEDROOM-CHARGE
                   MOVE 0 TO C-CLEAN-FEE
                   MOVE 'LEDGES' TO O-CONDO
      *  CLEARS ANY DEALS FROM THIS CONDO AND PREVIOUS CONDO  *
                   MOVE 0 TO C-ACC-DEALS
                   MOVE 0 TO C-PET-FEE
      
                   MULTIPLY C-BEDROOM-CHARGE BY I-BEDROOMS GIVING 
                       C-NIGHT-FEE ROUNDED
                   MULTIPLY C-NIGHT-FEE BY I-NIGHTS GIVING 
                       C-CONDO-FEE ROUNDED

                   IF I-HOTTUB ='Y'
                       MULTIPLY C-CONDO-FEE BY 0.075 GIVING C-HOTTUB-FEE
                           ROUNDED
                       ADD C-HOTTUB-FEE TO C-ACC-DEALS
                   ELSE
                       MOVE 0 TO C-HOTTUB-FEE
      *  ONLY DEAL IS THE THIRD NIGHT IS HALF OFF  *
               WHEN 'HT'
                   MOVE 50 TO C-BEDROOM-CHARGE
                   MOVE 100 TO C-CLEAN-FEE
                   MOVE 'HARBOUR TOWNE' TO O-CONDO
      *  CLEARS ANY DEALS FROM THIS CONDO AND PREVIOUS CONDO  * 
                   MOVE 0 TO C-ACC-DEALS
                   MOVE 0 TO C-HOTTUB-FEE
      *  CLEARS THE CTR TO AVOID ADDING EXTRA NUMBERS TO GRAND TOTALS  *
                   MOVE 0 TO C-FREE-NIGHT-CTR
      *  DEAL FOR IF GUEST STAYS FOR THREE OR MORE NIGHTS  *
                   IF I-NIGHTS >= 3
                       ADD 0.5 TO C-FREE-NIGHT-CTR
                       COMPUTE C-NIGHT-FEE ROUNDED = C-BEDROOM-CHARGE *
                           I-BEDROOMS / 2 
                       COMPUTE C-ACC-DEALS = C-ACC-DEALS + (0  - 
                           C-NIGHT-FEE)
                       MULTIPLY C-NIGHT-FEE BY I-NIGHTS GIVING 
                           C-CONDO-FEE ROUNDED
                   ELSE
                       MULTIPLY C-BEDROOM-CHARGE BY I-BEDROOMS GIVING
                           C-NIGHT-FEE ROUNDED
                       MULTIPLY C-NIGHT-FEE BY I-NIGHTS GIVING 
                           C-CONDO-FEE ROUNDED
      * ONLY DEAL IS A FREE NIGHT FOR STAYING SEVEN NIGHTS  * 
               WHEN 'CP'
                   MOVE 125 TO C-BEDROOM-CHARGE
                   MOVE 0 TO C-CLEAN-FEE
                   MOVE 'COMPASSE POINTE' TO O-CONDO
      *  CLEARS ANY DEALS FROM THIS CONDO AND PREVIOUS CONDO  * 
                   MOVE 0 TO C-ACC-DEALS
      *  CLEARS THE CTR TO AVOID ADDING EXTRA NUMBERS TO GRAND TOTALS  *
      *  AND CLEARS ANY AMOUNT PUT IN BY THE PREVIOUS CONDO  *
                   MOVE 0 TO C-FREE-NIGHT-CTR

                   COMPUTE C-NIGHT-FEE ROUNDED = C-BEDROOM-CHARGE *
                       I-BEDROOMS
                   MULTIPLY C-NIGHT-FEE BY I-NIGHTS GIVING 
                       C-CONDO-FEE ROUNDED
      *  DEAL FOR STAYING SEVEN OR MORE NIGHTS  *
                   IF I-NIGHTS >= 7
                       ADD 1 TO C-FREE-NIGHT-CTR
                       COMPUTE C-ACC-DEALS = C-ACC-DEALS + (0  - 
                           C-NIGHT-FEE)
      *  FOR WHEN INVALID DATA IS FOUND IN DAT FILE  *             
               WHEN OTHER
                   MOVE 0 TO C-BEDROOM-CHARGE
                   MOVE 0 TO C-CLEAN-FEE
                   MOVE 'ERROR' TO O-CONDO
                   MOVE 0 TO C-ACC-DEALS
                   MOVE 0 TO C-FREE-NIGHT-CTR

                   MULTIPLY C-BEDROOM-CHARGE BY I-BEDROOMS GIVING 
                       C-NIGHT-FEE ROUNDED
                   MULTIPLY C-NIGHT-FEE BY I-NIGHTS GIVING 
                       C-CONDO-FEE ROUNDED.

           MULTIPLY I-DOCKSLIP BY I-NIGHTS GIVING
               C-DOCKSLIP-FEE ROUNDED.
           COMPUTE C-SUBTOTAL ROUNDED = C-CONDO-FEE + C-CLEAN-FEE + 
               C-DOCKSLIP-FEE.
           ADD C-SUBTOTAL TO C-ACC-DEALS GIVING C-TOTAL ROUNDED.

           COMPUTE C-GT-ACC-DEALS = C-GT-ACC-DEALS + C-ACC-DEALS.
           COMPUTE C-GT-SUBTOTAL = C-GT-SUBTOTAL + C-SUBTOTAL.
           COMPUTE C-GT-TOTAL = C-GT-TOTAL + C-TOTAL.
           COMPUTE C-GT-FREE-NIGHT-CTR = C-GT-FREE-NIGHT-CTR + 
               C-FREE-NIGHT-CTR.
           COMPUTE C-GT-FREE-CLEAN-CTR = C-GT-FREE-CLEAN-CTR + 
               C-FREE-CLEAN-CTR.
           COMPUTE C-GT-PET-FEE = C-GT-PET-FEE + C-PET-FEE.
           COMPUTE C-GT-HOTTUB-FEE = C-GT-HOTTUB-FEE + C-HOTTUB-FEE.
           ADD 1 TO C-GT-RENTAL-CTR.

       2200-OUTPUT.
      *  A FLAG FOR TOTALS EXCEEDING $750.00  *
           IF C-TOTAL > 750
               MOVE '****' TO O-ASTERISKS
           ELSE
               MOVE '    ' TO O-ASTERISKS.

           MOVE I-GUEST TO O-GUEST.
           MOVE I-NIGHTS TO O-NIGHTS.
           MOVE C-NIGHT-FEE TO O-NIGHT-FEE.
           MOVE C-CONDO-FEE TO O-CONDO-FEE.
           MOVE C-CLEAN-FEE TO O-CLEAN-FEE.
           MOVE C-DOCKSLIP-FEE TO O-DOCKSLIP-FEE.
           MOVE C-SUBTOTAL TO O-SUBTOTAL.
           MOVE C-ACC-DEALS TO O-ACC-DEALS.
           MOVE C-TOTAL TO O-TOTAL.

           WRITE PRTLINE FROM DETAIL-LINE
               AFTER ADVANCING 1 LINE
                   AT EOP
                       PERFORM 9000-HEADINGS.

       3000-CLOSING.
           MOVE C-GT-SUBTOTAL TO O-GT-SUBTOTAL.
           MOVE C-GT-ACC-DEALS TO O-GT-ACC-DEALS.
           MOVE C-GT-TOTAL TO O-GT-TOTAL.
           MOVE C-GT-RENTAL-CTR TO O-GT-RENTAL-CTR.
           MOVE C-GT-FREE-CLEAN-CTR TO O-GT-FREE-CLEAN-CTR.
           MOVE C-GT-FREE-NIGHT-CTR TO O-GT-FREE-NIGHT-CTR.
           MOVE C-GT-PET-FEE TO O-GT-PET-FEE.
           MOVE C-GT-HOTTUB-FEE TO O-GT-HOTTUB-FEE.

           WRITE PRTLINE FROM GRAND-TOTALS-LINE-1
               AFTER ADVANCING 3 LINES.
           WRITE PRTLINE FROM GRAND-TOTALS-LINE-2
               AFTER ADVANCING 2 LINES.
           WRITE PRTLINE FROM GRAND-TOTALS-LINE-3
               AFTER ADVANCING 1 LINE.

           CLOSE OZARK-MASTER.
           CLOSE PRTOUT.

       9000-HEADINGS.
           ADD 1 TO C-PCTR.
           MOVE C-PCTR TO O-PCTR.

           WRITE PRTLINE FROM COMPANY-TITLE-LINE
               AFTER ADVANCING PAGE.
           WRITE PRTLINE FROM OZARKS-HEADING-LINE
               AFTER ADVANCING 1 LINE.
           WRITE PRTLINE FROM COLUMN-HEADINGS-LINE-1
               AFTER ADVANCING 2 LINES.
           WRITE PRTLINE FROM COLUMN-HEADINGS-LINE-2
               AFTER ADVANCING 1 LINE.
           WRITE PRTLINE FROM BLANK-LINE
               AFTER ADVANCING 1 LINE.
               
       9100-READ.
           READ OZARK-MASTER
               AT END
                   MOVE 'NO' TO MORE-RECS.