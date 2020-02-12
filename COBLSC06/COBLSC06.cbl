       IDENTIFICATION DIVISION.
       PROGRAM-ID.     COBLSC06.
       AUTHOR.         LUCAS CHAPMAN.
       DATE-WRITTEN.   1/31/20.
       DATE-COMPILED.
      **********************************************************
      *  CREATES A POP SALES REPORT FOR THE ALBIA SOCCER CLUB  *
      *  WITH GRAND TOTALS FOR EACH TYPE OF POP AND EACH TEAM  *
      *  IT ALSO CREATES AN ERROR REPORT FOR INVALID DATA WITH *
      *  INVALID DATA HAVING MESSAGES EXPLAINING WHY EACH DATA *
      *  IS INVALID BEING DISPLAYED USING A TABLE AND TEAMS    *
      *  NAMES AND POP TYPES ARE DISPLAYED USING TABLES        *
      **********************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT POP-MASTER
               ASSIGN TO 'C:\IHCC\COBOL\COBLSC06\CBLPOPSL.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.
      *  PRINTS OUT THE VALID RECORDS  *
           SELECT PRTOUT
               ASSIGN TO 'C:\IHCC\COBOL\COBLSC06\CBLPOPSLB.PRT'
               ORGANIZATION IS RECORD SEQUENTIAL.
      *  PRINTS OUT THE INVALID RECORDS  *
           SELECT ERROUT
               ASSIGN TO 'C:\IHCC\COBOL\COBLSC06\CBLPOPERB.PRT'
               ORGANIZATION IS RECORD SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  POP-MASTER
           LABEL RECORD IS STANDARD
           DATA RECORD IS I-REC
           RECORD CONTAINS 71 CHARACTERS.

       01  I-REC.
           05  I-LNAME         PIC X(15).
           05  I-FNAME         PIC X(15).
           05  I-ADDRESS       PIC X(15).
           05  I-CITY          PIC X(10).
           05  I-STATE         PIC XX.
               88  VAL-STATE               VALUE 'IA' 'IL' 'MI' 'MO' 
                                           'NE' 'WI'.
           05  I-ZIP.
               10  I-5-DIGITS  PIC 9(5).
               10  I-4-DIGITS  PIC 9(4).
           05  I-POP-TYPE      PIC 99.
               88  VAL-POP-TYPE            VALUE 01 THRU 06. 
           05  I-NUM-CASES     PIC 99.
           05  I-TEAM          PIC X.
               88  VAL-TEAM                VALUE 'A' THRU 'E'.

       FD  PRTOUT
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS PRTLINE
           LINAGE IS 60 WITH FOOTING AT 56.

       01  PRTLINE             PIC X(132).

       FD  ERROUT
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS ERRLINE
           LINAGE IS 60 WITH FOOTING AT 56.

       01  ERRLINE             PIC X(132).
       
       WORKING-STORAGE SECTION.
       01  WORK-AREA.
           05  C-PCTR          PIC 99      VALUE 0.
           05  MORE-RECS       PIC X(3)    VALUE 'YES'.
           05  C-ERR-PCTR      PIC 99      VALUE 0.
           05  ERR-SW          PIC X(3)    VALUE ' '.
           05  C-TOTAL         PIC 9(4)V99 VALUE 0.
           05  C-GT-ERR-CTR    PIC 9(4)    VALUE 0.
           05  SUB-STATE       PIC 9.
           
       01  CURRENT-DATE-AND-TIME.
           05  I-DATE.
               10  I-YY        PIC 9(4).
               10  I-MM        PIC 99.
               10  I-DD        PIC 99.
           05  I-TIME          PIC X(11).

       01  COMPANY-TITLE-LINE.
           05  FILLER          PIC X(6)    VALUE 'DATE: '.
		   05  O-MM            PIC 99.
		   05  FILLER          PIC X       VALUE '/'.
		   05  O-DD            PIC 99.
		   05  FILLER          PIC X       VALUE '/'.
		   05  O-YY            PIC 9(4).
		   05  FILLER          PIC X(36)   VALUE ' '.
           05  FILLER          PIC X(28)   VALUE 
                                         'ALBIA SOCCER CLUB FUNDRAISER'.
           05  FILLER          PIC X(44)   VALUE ' '.
           05  FILLER          PIC X(6)    VALUE 'PAGE: '.
           05  O-PCTR          PIC Z9.

       01  ERR-COMPANY-TITLE-LINE.
           05  FILLER          PIC X(6)    VALUE 'DATE: '.
		   05  O-ERR-MM        PIC 99.
		   05  FILLER          PIC X       VALUE '/'.
		   05  O-ERR-DD        PIC 99.
		   05  FILLER          PIC X       VALUE '/'.
		   05  O-ERR-YY        PIC 9(4).
		   05  FILLER          PIC X(36)   VALUE ' '.
           05  FILLER          PIC X(28)   VALUE 
                                         'ALBIA SOCCER CLUB FUNDRAISER'.
           05  FILLER          PIC X(44)   VALUE ' '.
           05  FILLER          PIC X(6)    VALUE 'PAGE: '.
           05  O-ERR-PCTR      PIC Z9.

      *  USED BY BOTH ERRLINE AND PRTLINE BECAUSE IT REMAINS THE SAME  *
       01  DIVISION-LINE.
           05  FILLER          PIC X(8)    VALUE 'COBLSC06'.
           05  FILLER          PIC X(48)   VALUE ' '.
           05  FILLER          PIC X(9)    VALUE '  CHAPMAN'.
           05  FILLER          PIC X(9)    VALUE ' DIVISION'.
           05  FILLER          PIC X(58)   VALUE ' '.

       01  REPORT-TITLE-LINE.
           05  FILLER          PIC X(60)   VALUE ' '.
           05  FILLER          PIC X(12)   VALUE 'SALES REPORT'.
           05  FILLER          PIC X(60)   VALUE ' '.

       01  ERR-REPORT-TITLE-LINE.
           05  FILLER          PIC X(60)   VALUE ' '.
           05  FILLER          PIC X(12)   VALUE 'ERROR REPORT'.
           05  FILLER          PIC X(60)   VALUE ' '.

       01  COLUMN-HEADINGS-LINE.
           05  FILLER          PIC X(12)   VALUE '   LAST NAME'.
           05  FILLER          PIC X(8)    VALUE ' '.
           05  FILLER          PIC X(10)   VALUE 'FIRST NAME'.
           05  FILLER          PIC X(7)    VALUE ' '.
           05  FILLER          PIC X(4)    VALUE 'CITY'.
           05  FILLER          PIC X(8)    VALUE ' '.
           05  FILLER          PIC X(5)    VALUE 'STATE'.
           05  FILLER          PIC X(9)    VALUE ' ZIP CODE'.
           05  FILLER          PIC X(4)    VALUE ' '.
           05  FILLER          PIC X(8)    VALUE 'POP TYPE'.
           05  FILLER          PIC X(13)   VALUE ' '.
           05  FILLER          PIC X(8)    VALUE 'QUANTITY'.
           05  FILLER          PIC X(6)    VALUE ' '.
           05  FILLER          PIC X(11)   VALUE 'DEPOSIT AMT'.
           05  FILLER          PIC X(6)    VALUE ' '.
           05  FILLER          PIC X(13)   VALUE 'TOTAL SALES  '.

       01  ERR-COLUMN-HEADINGS-LINE.
           05  FILLER          PIC X(12)   VALUE 'ERROR RECORD'.
           05  FILLER          PIC X(60)   VALUE ' '.
           05  FILLER          PIC X(17)   VALUE 'ERROR DESCRIPTION'.
           05  FILLER          PIC X(43)   VALUE ' '.

       01  DETAIL-LINE.
           05  FILLER          PIC XXX     VALUE ' '.
           05  O-LNAME         PIC X(15).
           05  FILLER          PIC XX      VALUE ' '.
           05  O-FNAME         PIC X(15).
           05  FILLER          PIC XX      VALUE ' '.
           05  O-CITY          PIC X(10).
           05  FILLER          PIC XXX     VALUE ' '.
           05  O-STATE         PIC XX.
           05  FILLER          PIC XXX     VALUE ' '.
           05  O-5-DIGITS      PIC 9(5).
           05  FILLER          PIC X       VALUE '-'.
           05  O-4-DIGITS      PIC 9(4).
           05  FILLER          PIC XX      VALUE ' '.
           05  O-POP-TYPE      PIC X(16).
           05  FILLER          PIC X(8)    VALUE ' '.
           05  O-NUM-CASES     PIC Z9.
           05  FILLER          PIC X(11)   VALUE ' '.
           05  O-DEPOSIT       PIC $$$$.99.
           05  FILLER          PIC X(9)    VALUE ' '.
           05  O-TOTAL         PIC $$,$$$.99.
           05  FILLER          PIC XXX     VALUE ' '.

      *  STORES THE VALID TEAM NAMES  *
       01  TEAM-INFO.
           05  FILLER          PIC X       VALUE 'A'.
           05  FILLER          PIC X       VALUE 'B'.
           05  FILLER          PIC X       VALUE 'C'.
           05  FILLER          PIC X       VALUE 'D'.
           05  FILLER          PIC X       VALUE 'E'.

      *  TABLE FOR TEAM NAMES  *
       01  TEAM-TABLE REDEFINES TEAM-INFO.
           05  T-TEAM          PIC X       OCCURS 5.

      *  STORES THE VALID POP TYPES  *
       01  POP-TYPE-INFO.
           05  FILLER          PIC X(16)   VALUE 'COKE'.
           05  FILLER          PIC X(16)   VALUE 'DIET COKE'.
           05  FILLER          PIC X(16)   VALUE 'MELLO YELLO'.
           05  FILLER          PIC X(16)   VALUE 'CHERRY COKE'.
           05  FILLER          PIC X(16)   VALUE 'DIET CHERRY COKE'.
           05  FILLER          PIC X(16)   VALUE 'SPRITE'.

      *  TABLE FOR POP TYPES  *
       01  POP-TYPE-TABLE REDEFINES POP-TYPE-INFO.
           05  T-POP-TYPE      PIC X(16)   OCCURS 6.

      *  STORES THE VALID STATE DEPOSIT RATES AND DEPOSIT AMOUNTS  *
       01  STATE-DEPOSIT-INFO.
           05  FILLER          PIC X(10)   VALUE 'IA00500000'.
           05  FILLER          PIC X(10)   VALUE 'NE00500000'.
           05  FILLER          PIC X(10)   VALUE 'WI00500000'.
           05  FILLER          PIC X(10)   VALUE 'MI01000000'.
           05  FILLER          PIC X(10)   VALUE 'IL00000000'.
           05  FILLER          PIC X(10)   VALUE 'MO00000000'.

      *  TABLE FOR THE STATE DEPOSIT RATES AND DEPOSIT AMOUNTS   * 
       01  STATE-DEPOSIT-TABLE REDEFINES STATE-DEPOSIT-INFO.
           05  STATE-DEPOSIT               OCCURS 6.
               10  T-STATE     PIC XX.
               10  T-DEP-RATE  PIC 9V99.
               10  C-DEPOSIT   PIC 9(3)V99.

      *  PRINTS THE INVALID RECORDS WITH A MESSAGE EXPLAINING WHY  *
      *  EACH RECORD IS INVALID                                    *       
       01  ERR-LINE.
           05  O-ERR-REC       PIC X(71).
           05  FILLER          PIC X       VALUE ' '.
           05  O-ERR-DESCRIP   PIC X(60).

      *  STORES ERROR MESSAGES *
       01  ERR-MSG.
           05  FILLER          PIC X(60)   VALUE 
                                           'A LAST NAME IS REQUIRED'.
           05  FILLER          PIC X(60)   VALUE 
                                           'A FIRST NAME IS REQUIRED'.
           05  FILLER		   PIC X(60)   VALUE 
                                           'AN ADDRESS IS REQUIRED'.
           05  FILLER		   PIC X(60)   VALUE 'A CITY IS REQUIRED'.
           05  FILLER          PIC X(60)   VALUE 
                     'A STATE MUST BE EITHER IA, IL, MI, MO, NE, OR WI'.
           05  FILLER          PIC X(60)   VALUE 
                                       'A ZIP CODE NEEDS TO BE NUMERIC'.
           05  FILLER          PIC X(60)   VALUE 
                                         'POP TYPES NEED TO BE NUMERIC'.
           05  FILLER          PIC X(60)   VALUE 
                        'A POP TYPE NEEDS TO BE A NUMBER FROM 01 TO 06'.
           05  FILLER          PIC X(60)   VALUE 
                               'THE NUMBER OF CASES NEED TO BE NUMERIC'.
           05  FILLER          PIC X(60)   VALUE 
                       'THE NUMBER OF CASES NEEDS TO BE GREATER THAN 0'.
           05  FILLER          PIC X(60)   VALUE 
                              'A TEAM NEEDS TO BE A LETTER FROM A TO E'.

      *  TABLE FOR ERROR MESSAGES *
       01  ERR-TABLE REDEFINES ERR-MSG.
	      05  T-ERR-DESCRIP    PIC X(60)   OCCURS 11.

       01  GRAND-TOTAL-HEADING-LINE-1.
           05  FILLER          PIC X(13)   VALUE 'GRAND TOTALS:'.
           05  FILLER          PIC X(119)  VALUE ' '.

       01  GRAND-TOTAL-LINE-1.
           05  FILLER          PIC XXX     VALUE ' '.
           05  O-GT-POP-TYPE               OCCURS 3.
               10  O-GT-POP    PIC X(16).
               10  FILLER      PIC X       VALUE ' '.
               10  O-GT-CTR    PIC ZZZ,ZZ9.
               10  FILLER      PIC X(6)    VALUE ' '.
           05  FILLER          PIC X(39)   VALUE ' '.

      *  TABLE FOR TOTAL QUANTITY OF POP TYPES SOLD  *
       01  GT-SOLD-CTR-TABLE.
           05  C-GT-SOLD-CTR   PIC 9(6)    OCCURS 6.
       01  SUB-SOLD-CTR        PIC 99.

       01  GRAND-TOTAL-HEADING-LINE-2.
           05  FILLER          PIC X(12)   VALUE 'TEAM TOTALS:'.
           05  FILLER          PIC X(120)  VALUE ' '.

       01  GRAND-TOTAL-LINE-2.
           05  FILLER          PIC XXX     VALUE ' '.
           05  O-GT-TEAM       PIC X.
           05  FILLER          PIC X       VALUE ' '.
           05  O-GT-TOTAL      PIC $$$$,$$$,$$$.99.
           05  FILLER          PIC X(112)  VALUE ' '.

      *  TABLE FOR TOTAL QUANTITY OF POP TYPES SOLD  *
       01  GT-TEAM-SALES-TABLE.
           05  C-GT-TEAM-SALES PIC 9(7)V99 OCCURS 5.
       01  SUB-TEAM            PIC 9.

       01  ERR-GRAND-TOTAL-LINE.
           05  FILLER          PIC X(13)   VALUE 'TOTAL ERRORS '.
           05  O-GT-ERR-CTR    PIC Z,ZZ9.
           05  FILLER          PIC X(114)  VALUE ' '.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INIT.
           PERFORM 2000-MAINLINE
               UNTIL MORE-RECS = 'NO'.
           PERFORM 3000-CLOSING.
       STOP RUN.

       1000-INIT.
           OPEN INPUT POP-MASTER.
           OPEN OUTPUT PRTOUT.
           OPEN OUTPUT ERROUT.
           
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.
           MOVE I-YY TO O-YY.
           MOVE I-YY TO O-ERR-YY.
           MOVE I-DD TO O-DD.
           MOVE I-DD TO O-ERR-DD.
           MOVE I-MM TO O-MM.
           MOVE I-MM TO O-ERR-MM.

           PERFORM 9000-READ.
           PERFORM 9100-HEADINGS.
           PERFORM 9200-ERR-HEADINGS.
           PERFORM 
               VARYING SUB-SOLD-CTR FROM 1 BY 1
                   UNTIL SUB-SOLD-CTR > 6
                       MOVE 0 TO C-GT-SOLD-CTR(SUB-SOLD-CTR).
           PERFORM 
               VARYING SUB-TEAM FROM 1 BY 1
                   UNTIL SUB-TEAM > 5
                       MOVE 0 TO C-GT-TEAM-SALES(SUB-TEAM).

       2000-MAINLINE.
           PERFORM 2100-VALIDATION THRU 2100-X.
           IF ERR-SW = 'NO'
               PERFORM 2200-CALCS
               PERFORM 2300-OUTPUT
           ELSE
               PERFORM 2400-ERR-ROUT.

           PERFORM 9000-READ.

      *  CHECKS TO SEE IF EACH RECORD IS VALID OR INVALID  *
       2100-VALIDATION.
           MOVE 'YES' TO ERR-SW.
           IF I-LNAME = ' '
               MOVE T-ERR-DESCRIP(1) TO O-ERR-DESCRIP
               GO TO 2100-X.
           IF I-FNAME = ' '
               MOVE T-ERR-DESCRIP(2) TO O-ERR-DESCRIP
               GO TO 2100-X.
           IF I-ADDRESS = ' '
               MOVE T-ERR-DESCRIP(3) TO O-ERR-DESCRIP
               GO TO 2100-X.
           IF I-CITY = ' '
               MOVE T-ERR-DESCRIP(4) TO O-ERR-DESCRIP
               GO TO 2100-X.
           IF NOT VAL-STATE
               MOVE T-ERR-DESCRIP(5) TO O-ERR-DESCRIP
               GO TO 2100-X.
           IF I-5-DIGITS NOT NUMERIC
               MOVE T-ERR-DESCRIP(6) TO O-ERR-DESCRIP
               GO TO 2100-X.
           IF I-4-DIGITS NOT NUMERIC
               MOVE T-ERR-DESCRIP(6) TO O-ERR-DESCRIP
               GO TO 2100-X.
           IF I-POP-TYPE NOT NUMERIC
               MOVE T-ERR-DESCRIP(7) TO O-ERR-DESCRIP
               GO TO 2100-X.
           IF NOT VAL-POP-TYPE
               MOVE T-ERR-DESCRIP(8) TO O-ERR-DESCRIP
               GO TO 2100-X.
           IF I-NUM-CASES NOT NUMERIC
               MOVE T-ERR-DESCRIP(9) TO O-ERR-DESCRIP
               GO TO 2100-X.
           IF I-NUM-CASES <= 0
               MOVE T-ERR-DESCRIP(10) TO O-ERR-DESCRIP
               GO TO 2100-X.
           IF NOT VAL-TEAM
               MOVE T-ERR-DESCRIP(11) TO O-ERR-DESCRIP
               GO TO 2100-X.
           MOVE 'NO' TO ERR-SW.

       2100-X.
           EXIT.

       2200-CALCS.
           PERFORM 
               VARYING SUB-STATE FROM 1 BY 1
                   UNTIL I-STATE = T-STATE(SUB-STATE).

           COMPUTE C-DEPOSIT(SUB-STATE) = 24 * I-NUM-CASES *
               T-DEP-RATE(SUB-STATE).
           COMPUTE C-TOTAL = 18.71 * I-NUM-CASES + C-DEPOSIT(SUB-STATE).

           ADD I-NUM-CASES TO C-GT-SOLD-CTR(I-POP-TYPE).

           PERFORM
               VARYING SUB-TEAM FROM 1 BY 1
                   UNTIL I-TEAM = T-TEAM(SUB-TEAM).

           ADD C-TOTAL TO C-GT-TEAM-SALES(SUB-TEAM).

       2300-OUTPUT.
           MOVE C-DEPOSIT(SUB-STATE) TO O-DEPOSIT.
           MOVE T-POP-TYPE(I-POP-TYPE) TO O-POP-TYPE.

           MOVE I-LNAME TO O-LNAME.
           MOVE I-FNAME TO O-FNAME.
           MOVE I-CITY TO O-CITY.
           MOVE I-STATE TO O-STATE.
           MOVE I-5-DIGITS TO O-5-DIGITS.
           MOVE I-4-DIGITS TO O-4-DIGITS.
           MOVE I-NUM-CASES TO O-NUM-CASES.
           MOVE C-TOTAL TO O-TOTAL.

           WRITE PRTLINE FROM DETAIL-LINE
               AFTER ADVANCING 2 LINES
                   AT EOP
                       PERFORM 9100-HEADINGS.

      *  MOVES ALL INVALID RECORDS TO O-ERR-REC TO PRINT ON CBLPOPER  *
       2400-ERR-ROUT.
           MOVE I-REC TO O-ERR-REC.

           WRITE ERRLINE FROM ERR-LINE
               AFTER ADVANCING 2 LINES
                   AT EOP
                       PERFORM 9200-ERR-HEADINGS.

           ADD 1 TO C-GT-ERR-CTR.

       3000-CLOSING.
           PERFORM 9100-HEADINGS.

           WRITE PRTLINE FROM GRAND-TOTAL-HEADING-LINE-1
               AFTER ADVANCING 3 LINES.

           PERFORM 3100-GT-POP.
           PERFORM 3200-GT-TEAM.

           MOVE C-GT-ERR-CTR TO O-GT-ERR-CTR.

           WRITE ERRLINE FROM ERR-GRAND-TOTAL-LINE
               AFTER ADVANCING 3 LINES.
   
      *  PRINTS ALL THE LINES OF POP GRAND TOTALS  *
       3100-GT-POP.
           PERFORM 3110-GT-POP
               VARYING I-POP-TYPE FROM 1 BY 1
                   UNTIL I-POP-TYPE > 3.

           WRITE PRTLINE FROM GRAND-TOTAL-LINE-1
               AFTER ADVANCING 2 LINES.

           PERFORM 3120-GT-POP
               VARYING I-POP-TYPE FROM 4 BY 1
                   UNTIL I-POP-TYPE > 6.

           WRITE PRTLINE FROM GRAND-TOTAL-LINE-1
               AFTER ADVANCING 2 LINES.

       3110-GT-POP.
           MOVE T-POP-TYPE(I-POP-TYPE) TO O-GT-POP(I-POP-TYPE).
           MOVE C-GT-SOLD-CTR(I-POP-TYPE) TO O-GT-CTR(I-POP-TYPE).

       3120-GT-POP.
           MOVE T-POP-TYPE(I-POP-TYPE) TO O-GT-POP(I-POP-TYPE - 3).
           MOVE C-GT-SOLD-CTR(I-POP-TYPE) TO O-GT-CTR(I-POP-TYPE - 3).

      *  PRINTS ALL LINES OF TEAM GRAND TOTALS  *
       3200-GT-TEAM.
           WRITE PRTLINE FROM GRAND-TOTAL-HEADING-LINE-2
               AFTER ADVANCING 3 LINES.

           PERFORM 3210-GT-TEAM
               VARYING SUB-TEAM FROM 1 BY 1
                   UNTIL SUB-TEAM > 5.

       3210-GT-TEAM.
           MOVE T-TEAM(SUB-TEAM) TO O-GT-TEAM.
           MOVE C-GT-TEAM-SALES(SUB-TEAM) TO O-GT-TOTAL.

           WRITE PRTLINE FROM GRAND-TOTAL-LINE-2
               AFTER ADVANCING 2 LINES.
          
       9000-READ.
           READ POP-MASTER
               AT END
                   MOVE 'NO' TO MORE-RECS.

       9100-HEADINGS.
           ADD 1 TO C-PCTR.
           MOVE C-PCTR TO O-PCTR.

           WRITE PRTLINE FROM COMPANY-TITLE-LINE
               AFTER ADVANCING PAGE.
           WRITE PRTLINE FROM DIVISION-LINE
               AFTER ADVANCING 1 LINE.
           WRITE PRTLINE FROM REPORT-TITLE-LINE
               AFTER ADVANCING 1 LINE.
           WRITE PRTLINE FROM COLUMN-HEADINGS-LINE
               AFTER ADVANCING 2 LINES.

      *  HEADINGS FOR THE ERROR REPORT  *
       9200-ERR-HEADINGS.
           ADD 1 TO C-ERR-PCTR.
           MOVE C-ERR-PCTR TO O-ERR-PCTR.

           WRITE ERRLINE FROM ERR-COMPANY-TITLE-LINE
               AFTER ADVANCING PAGE.
           WRITE ERRLINE FROM DIVISION-LINE
               AFTER ADVANCING 1 LINE.
           WRITE ERRLINE FROM ERR-REPORT-TITLE-LINE
               AFTER ADVANCING 1 LINE.
           WRITE ERRLINE FROM ERR-COLUMN-HEADINGS-LINE
               AFTER ADVANCING 2 LINES.