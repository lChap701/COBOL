       IDENTIFICATION DIVISION.
       PROGRAM-ID.     COBLSC05.
       AUTHOR.         LUCAS CHAPMAN.
       DATE-WRITTEN.   1/22/20.
       DATE-COMPILED.
      **********************************************************
      *  CREATES A POP SALES REPORT FOR THE ALBIA SOCCER CLUB  *
      *  WITH GRAND TOTALS FOR EACH TYPE OF POP AND EACH TEAM  *
      *  IT ALSO CREATES AN ERROR REPORT FOR INVALID DATA      *
      **********************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT POP-MASTER
               ASSIGN TO 'C:\IHCC\COBOL\COBLSC05\CBLPOPSL.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.
      *  PRINTS OUT THE VALID RECORDS  *
           SELECT PRTOUT
               ASSIGN TO 'C:\IHCC\COBOL\COBLSC05\CBLPOPSL.PRT'
               ORGANIZATION IS RECORD SEQUENTIAL.
      *  PRINTS OUT THE INVALID RECORDS  *
           SELECT ERROUT
               ASSIGN TO 'C:\IHCC\COBOL\COBLSC05\CBLPOPER.PRT'
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
               88  VAL-STATE           VALUE 'IA' 'IL' 'MI' 'MO' 'NE' 
                                           'WI'.
           05  I-ZIP.
               10  I-5-DIGITS  PIC 9(5).
               10  I-4-DIGITS  PIC 9(4).
           05  I-POP-TYPE      PIC 99.
               88  VAL-POP-TYPE        VALUE 01 THRU 06. 
           05  I-NUM-CASES     PIC 99.
           05  I-TEAM          PIC X.
               88  VAL-TEAM            VALUE 'A' THRU 'E'.

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
           05  C-DEPOSIT       PIC 9(3)V99 VALUE 0.
           05  C-TOTAL         PIC 9(4)V99 VALUE 0.
           05  C-GT-TOTAL-A    PIC 9(7)V99 VALUE 0.
           05  C-GT-TOTAL-B    PIC 9(7)V99 VALUE 0.
           05  C-GT-TOTAL-C    PIC 9(7)V99 VALUE 0.
           05  C-GT-TOTAL-D    PIC 9(7)V99 VALUE 0.
           05  C-GT-TOTAL-E    PIC 9(7)V99 VALUE 0.
           05  C-GT-SOLD-1-CTR PIC 9(6)    VALUE 0.
           05  C-GT-SOLD-2-CTR PIC 9(6)    VALUE 0.
           05  C-GT-SOLD-3-CTR PIC 9(6)    VALUE 0.
           05  C-GT-SOLD-4-CTR PIC 9(6)    VALUE 0.
           05  C-GT-SOLD-5-CTR PIC 9(6)    VALUE 0.
           05  C-GT-SOLD-6-CTR PIC 9(6)    VALUE 0.
           05  C-GT-ERR-CTR    PIC 9(4)    VALUE 0.

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
      *  COMPANY TITLE LINE FOR COBLPOPER.PRT  *
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
           05  FILLER          PIC X(8)    VALUE 'COBLSC05'.
           05  FILLER          PIC X(48)   VALUE ' '.
           05  FILLER          PIC X(9)    VALUE '  CHAPMAN'.
           05  FILLER          PIC X(9)    VALUE ' DIVISION'.
           05  FILLER          PIC X(58)   VALUE ' '.

       01  REPORT-TITLE-LINE.
           05  FILLER          PIC X(60)   VALUE ' '.
           05  FILLER          PIC X(12)   VALUE 'SALES REPORT'.
           05  FILLER          PIC X(60)   VALUE ' '.
      *  REPORT TILILE LINE FOR CBLPOPER.PRT  *
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
      *  PRINTS THE INVALID RECORDS WITH A MESSAGE EXPLAINING WHY  *
      *  EACH RECORD IS INVALID                                    *
       01  ERR-LINE.
           05  O-ERR-REC       PIC X(71).
           05  FILLER          PIC X       VALUE ' '.
           05  O-ERR-DESCRIP   PIC X(60).

       01  GRAND-TOTAL-HEADING-LINE-1.
           05  FILLER          PIC X(13)   VALUE 'GRAND TOTALS:'.
           05  FILLER          PIC X(119)  VALUE ' '.

       01  GRAND-TOTAL-LINE-1.
           05  FILLER          PIC XXX     VALUE ' '.
           05  O-GT-POP-TYPE-1 PIC X(16).
           05  FILLER          PIC X       VALUE ' '.
           05  O-GT-SOLD-1-CTR PIC ZZZ,ZZ9.
           05  FILLER          PIC X(6)    VALUE ' '.
           05  O-GT-POP-TYPE-2 PIC X(16).
           05  FILLER          PIC X       VALUE ' '.
           05  O-GT-SOLD-2-CTR PIC ZZZ,ZZ9.
           05  FILLER          PIC X(6)    VALUE ' '.
           05  O-GT-POP-TYPE-3 PIC X(16).
           05  FILLER          PIC X       VALUE ' '.
           05  O-GT-SOLD-3-CTR PIC ZZZ,ZZ9.
           05  FILLER          PIC X(45)   VALUE ' '.

       01  GRAND-TOTAL-HEADING-LINE-2.
           05  FILLER          PIC X(12)   VALUE 'TEAM TOTALS:'.
           05  FILLER          PIC X(120)  VALUE ' '.

       01  GRAND-TOTAL-LINE-2.
           05  FILLER          PIC XXX     VALUE ' '.
           05  O-GT-TEAM       PIC X.
           05  FILLER          PIC X       VALUE ' '.
           05  O-GT-TOTAL      PIC $$$$,$$$,$$$.99.
           05  FILLER          PIC X(112)  VALUE ' '.
      *  PRINTS OUT THE TOTAL INVALID RECORDS ON CBLPOPER.PRT  *
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
               MOVE 'A LAST NAME IS REQUIRED' TO O-ERR-DESCRIP
               GO TO 2100-X.
           IF I-FNAME = ' '
               MOVE 'A FIRST NAME IS REQUIRED' TO O-ERR-DESCRIP
               GO TO 2100-X.
           IF I-ADDRESS = ' '
               MOVE 'AN ADDRESS IS REQUIRED' TO O-ERR-DESCRIP
               GO TO 2100-X.
           IF I-CITY = ' '
               MOVE 'A CITY IS REQUIRED' TO O-ERR-DESCRIP
               GO TO 2100-X.
           IF NOT VAL-STATE
               MOVE 'A STATE MUST BE EITHER IA, IL, MI, MO, NE, OR WI'
                   TO O-ERR-DESCRIP
               GO TO 2100-X.
           IF I-5-DIGITS NOT NUMERIC
               MOVE 'A ZIP CODE NEEDS TO BE NUMERIC' TO O-ERR-DESCRIP
               GO TO 2100-X.
           IF I-4-DIGITS NOT NUMERIC
               MOVE 'A ZIP CODE NEEDS TO BE NUMERIC' TO O-ERR-DESCRIP
               GO TO 2100-X.
           IF I-POP-TYPE NOT NUMERIC
               MOVE 'POP TYPES NEED TO BE NUMERIC' TO O-ERR-DESCRIP
               GO TO 2100-X.
           IF NOT VAL-POP-TYPE
               MOVE 'A POP TYPE NEEDS TO BE A NUMBER FROM 01 TO 06' TO
                   O-ERR-DESCRIP
               GO TO 2100-X.
           IF I-NUM-CASES NOT NUMERIC
               MOVE 'THE NUMBER OF CASES NEED TO BE NUMERIC' TO 
                   O-ERR-DESCRIP
               GO TO 2100-X.
           IF I-NUM-CASES <= 0
               MOVE 'THE NUMBER OF CASES NEEDS TO BE GREATER THAN 0' TO
                   O-ERR-DESCRIP
               GO TO 2100-X.
           IF NOT VAL-TEAM
               MOVE 'A TEAM NEEDS TO BE A LETTER FROM A TO E' TO 
                   O-ERR-DESCRIP
               GO TO 2100-X.
           MOVE 'NO' TO ERR-SW.

       2100-X.
           EXIT.

       2200-CALCS.
           IF I-STATE = 'IA' OR 'NE' OR 'WI'
               COMPUTE C-DEPOSIT = 24 * I-NUM-CASES * 0.05
           ELSE
               IF I-STATE = 'MI'
                   COMPUTE C-DEPOSIT = 24 * I-NUM-CASES * 0.10
               ELSE
                   MOVE 0 TO C-DEPOSIT.

           COMPUTE C-TOTAL = 18.71 * I-NUM-CASES + C-DEPOSIT.

           EVALUATE I-TEAM
               WHEN 'A'
                   ADD C-TOTAL TO C-GT-TOTAL-A
               WHEN 'B'
                   ADD C-TOTAL TO C-GT-TOTAL-B
               WHEN 'C'
                   ADD C-TOTAL TO C-GT-TOTAL-C
               WHEN 'D'
                   ADD C-TOTAL TO C-GT-TOTAL-D
               WHEN 'E'
                   ADD C-TOTAL TO C-GT-TOTAL-E.

           EVALUATE I-POP-TYPE
               WHEN 01
                   ADD I-NUM-CASES TO C-GT-SOLD-1-CTR
               WHEN 02
                   ADD I-NUM-CASES TO C-GT-SOLD-2-CTR
               WHEN 03
                   ADD I-NUM-CASES TO C-GT-SOLD-3-CTR
               WHEN 04
                   ADD I-NUM-CASES TO C-GT-SOLD-4-CTR
               WHEN 05
                   ADD I-NUM-CASES TO C-GT-SOLD-5-CTR
               WHEN 06
                   ADD I-NUM-CASES TO C-GT-SOLD-6-CTR.

       2300-OUTPUT.
           EVALUATE I-POP-TYPE
               WHEN 01
                   MOVE 'COKE' TO O-POP-TYPE
               WHEN 02
                   MOVE 'DIET COKE' TO O-POP-TYPE
               WHEN 03
                   MOVE 'MELLO YELLO' TO O-POP-TYPE
               WHEN 04
                   MOVE 'CHERRY COKE' TO O-POP-TYPE
               WHEN 05
                   MOVE 'DIET CHERRY COKE' TO O-POP-TYPE
               WHEN 06
                   MOVE 'SPRITE' TO O-POP-TYPE.

           MOVE I-LNAME TO O-LNAME.
           MOVE I-FNAME TO O-FNAME.
           MOVE I-CITY TO O-CITY.
           MOVE I-STATE TO O-STATE.
           MOVE I-5-DIGITS TO O-5-DIGITS.
           MOVE I-4-DIGITS TO O-4-DIGITS.
           MOVE I-NUM-CASES TO O-NUM-CASES.
           MOVE C-DEPOSIT TO O-DEPOSIT.
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
           PERFORM 3100-GT-POP.

      *  PRINTS THE SECOND LINE OF POP GRAND TOTALS  *
           MOVE 'CHERRY COKE' TO O-GT-POP-TYPE-1.
           MOVE 'DIET CHERRY COKE' TO O-GT-POP-TYPE-2.
           MOVE 'SPRITE' TO O-GT-POP-TYPE-3.
           MOVE C-GT-SOLD-4-CTR TO O-GT-SOLD-1-CTR.
           MOVE C-GT-SOLD-5-CTR TO O-GT-SOLD-2-CTR.
           MOVE C-GT-SOLD-6-CTR TO O-GT-SOLD-3-CTR.

           WRITE PRTLINE FROM GRAND-TOTAL-LINE-1
               AFTER ADVANCING 2 LINES.

           PERFORM 3200-GT-TEAM.

      *  PRINTS THE LAST LINE OF TEAM GRAND TOTALS  *
           MOVE 'E' TO O-GT-TEAM.
           MOVE C-GT-TOTAL-E TO O-GT-TOTAL.

           WRITE PRTLINE FROM GRAND-TOTAL-LINE-2
               AFTER ADVANCING 2 LINES.

           MOVE C-GT-ERR-CTR TO O-GT-ERR-CTR.

           WRITE ERRLINE FROM ERR-GRAND-TOTAL-LINE
               AFTER ADVANCING 3 LINES.
      *  PRINTS THE FIRST LINE OF POP GRAND TOTALS  *
       3100-GT-POP.
           MOVE 'COKE' TO O-GT-POP-TYPE-1.
           MOVE 'DIET COKE' TO O-GT-POP-TYPE-2.
           MOVE 'MELLO YELLO' TO O-GT-POP-TYPE-3.
           
           MOVE C-GT-SOLD-1-CTR TO O-GT-SOLD-1-CTR.
           MOVE C-GT-SOLD-2-CTR TO O-GT-SOLD-2-CTR.
           MOVE C-GT-SOLD-3-CTR TO O-GT-SOLD-3-CTR.
           
           WRITE PRTLINE FROM GRAND-TOTAL-HEADING-LINE-1
               AFTER ADVANCING 3 LINES.
           WRITE PRTLINE FROM GRAND-TOTAL-LINE-1
               AFTER ADVANCING 2 LINES.
      *  PRINTS THE FIRST LINE OF TEAM GRAND TOTALS  *
       3200-GT-TEAM.
           MOVE 'A' TO O-GT-TEAM.
           MOVE C-GT-TOTAL-A TO O-GT-TOTAL.

           WRITE PRTLINE FROM GRAND-TOTAL-HEADING-LINE-2
               AFTER ADVANCING 3 LINES.
           WRITE PRTLINE FROM GRAND-TOTAL-LINE-2
               AFTER ADVANCING 2 LINES.

           PERFORM 3210-GT-TEAM.
           PERFORM 3220-GT-TEAM.
           PERFORM 3230-GT-TEAM.
      *  PRINTS THE SECOND LINE OF TEAM GRAND TOTALS  *
       3210-GT-TEAM.
           MOVE 'B' TO O-GT-TEAM.
           MOVE C-GT-TOTAL-B TO O-GT-TOTAL.

           WRITE PRTLINE FROM GRAND-TOTAL-LINE-2
               AFTER ADVANCING 2 LINES.

      *  PRINTS THE THIRD LINE OF POP GRAND TOTALS  *
       3220-GT-TEAM.
           MOVE 'C' TO O-GT-TEAM.
           MOVE C-GT-TOTAL-C TO O-GT-TOTAL.

           WRITE PRTLINE FROM GRAND-TOTAL-LINE-2
               AFTER ADVANCING 2 LINES.

      *  PRINTS THE FOURTH LINE OF POP GRAND TOTALS  *
       3230-GT-TEAM.
           MOVE 'D' TO O-GT-TEAM.
           MOVE C-GT-TOTAL-D TO O-GT-TOTAL.

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