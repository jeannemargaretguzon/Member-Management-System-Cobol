      ******************************************************************
      * Authors: Group 5 -
      *     GUZON, JEANNE MARGARET L.
      *     MAGLAQUE, CHARRA JEANNE D.C.
      *     SUGANOB, ELAINE O.
      *     TADIQUE, EROLL JOHN D.
      *     TINIO, BRIDGETTE VALERIE S.
      *     VILLALUZ, HONEYVHEN A.
      * Purpose: CLUB MANAGEMENT SYSTEM
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FINAL-SYSTEM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USER-FILE
               ASSIGN TO 'USERFILE.TXT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ID-NUMBER.

      ****FILES SALES REPORT
           SELECT SALES-FILE ASSIGN TO 'SLSFILE.TXT'
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS IS SEQUENTIAL.

           SELECT WORK-FILE ASSIGN TO 'WRKFILE.TXT'.

           SELECT SORTED-SLS-FILE ASSIGN TO 'SLSSRTD.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.

      *****TERMS AND CONDITIONS
           SELECT TOC-AGREEMENT ASSIGN TO "TERMSANDCONDITION.txt"
              ORGANIZATION IS LINE SEQUENTIAL.

      *****FILES FOR PROGRAM STATE
           SELECT ZUM ASSIGN TO 'ZUMBA.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT BC ASSIGN TO 'BODYCON.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT BB ASSIGN TO 'BODYBUI.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT BX ASSIGN TO 'BOXINGG.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ZUMBAMEM ASSIGN TO 'ZUMLIST.TXT'
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS IS SEQUENTIAL.

           SELECT BODYCONMEM ASSIGN TO 'BODYCONLIST.TXT'
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS IS SEQUENTIAL.

           SELECT BODYBUILDMEM ASSIGN TO 'BODYBUILDLIST.TXT'
              ORGANIZATION IS LINE SEQUENTIAL
              ACCESS IS SEQUENTIAL.

           SELECT BOXINGMEM ASSIGN TO 'BOXINGLIST.TXT'
              ORGANIZATION IS LINE SEQUENTIAL
              ACCESS IS SEQUENTIAL.

      ****FILES PROGRAM MEMBER REPORT
           SELECT PT-FILE ASSIGN TO 'PTFILE.TXT'
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS IS SEQUENTIAL.

           SELECT WORK-FILE-1 ASSIGN TO 'WRKFILE1.TXT'.

           SELECT SORTED-PT-FILE ASSIGN TO 'PTSRTD.TXT'
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  USER-FILE.
       01  MEMBER-RECORD.
           02  ID-NUMBER           PIC 9(03).
           02  MEMBER-NAME         PIC X(25).
           02  MEMBERSHIP-TYPE     PIC X(06).
           02  MEMBER-AGE          PIC 9(02).
           02  MEMBER-SEX          PIC X(01).
           02  MEMBER-CONT-NUM     PIC 9(11).
           02  MEMBER-ADDRESS      PIC X(70).
           02  DATE-JOINED.
               03  MONTH-JOINED    PIC 9(02).
               03  FILLER          PIC X(01) VALUE '/'.
               03  DAY-JOINED      PIC 9(02).
               03  FILLER          PIC X(01) VALUE '/'.
               03  YEAR-JOINED     PIC 9(04).
           02  DATE-EXPIRE.
               03  MONTH-EXPIRE    PIC 9(02).
               03  FILLER          PIC X(01) VALUE '/'.
               03  DAY-EXPIRE      PIC 9(02).
               03  FILLER          PIC X(01) VALUE '/'.
               03  YEAR-EXPIRE     PIC 9(04).
           02  EMERGENCY-CONTACT-INFO.
               03  EMERGENCY-NAME            PIC X(25).
               03  EMERGENCY-REL             PIC X(25).
               03  EMERGENCY-CONT-NUM        PIC 9(11).

      ******************************************************************
      *                   FILES FOR SALES REPORT                       *
      ******************************************************************

       FD  SALES-FILE.
       01  SALES-REC.
           02  MEMTYPE-CODE         PIC 9(02).
           02  MEMTYPE-FEE          PIC 9(04).
           02  MT-YR                PIC 9(04).

       FD  SORTED-SLS-FILE.
       01  SORTED-REC.
           02  MEMTYPE-CODE-SF      PIC 9(02).
           02  MEMTYPE-FEE-SF       PIC 9(04).
           02  MT-YR-SF             PIC 9(04).

       SD  WORK-FILE.
       01  WORK-REC.
           02  MEMTYPE-CODE-W       PIC 9(02).
           02  MEMTYPE-FEE-W        PIC 9(04).
           02  MT-YR-W              PIC 9(04).

      ******************************************************************
      *                  FILES FOR TERMS OF CONDITION                  *
      ******************************************************************

       FD  TOC-AGREEMENT.
       01  PRINT-LINE-TERMS       PIC X(200).

      ******************************************************************
      *                    FILES FOR PROGRAM TYPE                      *
      ******************************************************************

       FD  ZUM.
       01  ZUM-RECORD.
           05  ZUM-DETAILS     PIC X(100).

       FD  BC.
       01  BC-RECORD.
           05  BC-DETAILS      PIC X(100).

       FD  BB.
       01  BB-RECORD.
           05  BB-DETAILS      PIC X(100).

       FD  BX.
       01  BOX-RECORD.
           05  DETAILS         PIC X(100).

       FD  ZUMBAMEM.
       01  ZUMBA-LIST.
           05  ZUMBA-NAMES     PIC X(100).

       FD  BODYCONMEM.
       01  BODYCON-LIST.
           05  BODYCON-NAMES   PIC X(100).

       FD  BODYBUILDMEM.
       01  BODYBUILD-LIST.
           05  BODYBUILD-NAMES PIC X(100).

       FD  BOXINGMEM.
       01  BOXING-LIST.
           05  BOXING-NAMES    PIC X(100).

      ******************************************************************
      *                   FILES FOR PROGRAM RECORDS                    *
      ******************************************************************

       FD  PT-FILE.
       01  PT-REC.
           05  PROGTYPE-CODE          PIC 9(04).
           05  PT-YR                  PIC 9(04).

       FD  SORTED-PT-FILE.
       01  SORTED-PT-REC.
           05  PROGTYPE-CODE-SF       PIC 9(04).
           05  PT-YR-SF               PIC 9(04).

       SD  WORK-FILE-1.
       01  WORK-REC-1.
           05  PROGTYPE-CODE-W        PIC 9(04).
           05  PT-YR-W                PIC 9(04).

       WORKING-STORAGE SECTION.
      ********************** TEMPORARY VARIABLES ***********************
       01  TEMP-VARIABLES.
           02  PROPER-SPACING      PIC X(01) VALUE SPACE.
           02  WS-MEMBERSHIP-NUM   PIC 9(02).
           02  WS-USER-ID          PIC 9(03) VALUE ZEROES.
           02  WS-FILE-STAT        PIC 9(02).
           02  WS-SUB              PIC 9(03).
           02  WS-TERMS            PIC X(200).
           02  CURR-MEMTYPE-CODE   PIC 9(02).
           02  TEMP-USER-ID        PIC 9(03).

       01  WS-ANSWERS.
           02  WS-CHOICE           PIC 9(01).
           02  WS-YES-NO           PIC X(01).
           02  WS-TOC-ANS          PIC X(01).
           02  WS-RUI-ANS          PIC X(01).
           02  IS-REC-FOUND        PIC X(01).
           02  HF-ANS              PIC X(01) VALUE 'N'.
           02  PROGRAM-TYPE        PIC 9.

       01  FLAGS-N-SWITCHES.
           05  EOF-SWITCH       PIC X(01) VALUE 'N'.
           05  OK-TO-CHANGE     PIC X(01).

       01  CHANGE-MEMBER-RECORD.
           02  CHANGE-MEMBER-NAME         PIC X(25).
           02  CHANGE-MEMBERSHIP-TYPE     PIC X(06).
           02  CHANGE-MEMBER-AGE          PIC 9(02).
           02  CHANGE-MEMBER-SEX          PIC X(01).
           02  CHANGE-MEMBER-CONT-NUM     PIC 9(11).
           02  CHANGE-MEMBER-ADDRESS      PIC X(70).
           02  CHANGE-EMERGENCY-CONTACT-INFO.
               03  CHANGE-EMERGENCY-NAME            PIC X(25).
               03  CHANGE-EMERGENCY-REL             PIC X(25).
               03  CHANGE-EMERGENCY-CONT-NUM        PIC 9(11).

       01  MT-TOTALS-BR.
           05  MEMBERS-BR        PIC 9(02) VALUES ZEROES.
           05  PAYMENTS-BR       PIC 9(06)V99 VALUE ZEROES.

       01  MT-TOTALS-SL.
           05  MEMBERS-SL        PIC 9(02) VALUES ZEROES.
           05  PAYMENTS-SL       PIC 9(06)V99 VALUE ZEROES.

       01  MT-TOTALS-GD.
           05  MEMBERS-GD        PIC 9(02) VALUES ZEROES.
           05  PAYMENTS-GD       PIC 9(06)V99 VALUE ZEROES.

       01  MT-FINAL-TOTALS.
           05  TOTAL-MEMBERS       PIC 9(03) VALUE ZEROES.
           05  TOTAL-PAYMENTS      PIC 9(06) VALUE ZEROES.

       01  PT-MEMBER-TOTALS.
           05  MEMBERS-ZU        PIC 9(03) VALUE ZEROES.
           05  MEMBERS-BC        PIC 9(03) VALUE ZEROES.
           05  MEMBERS-BB        PIC 9(03) VALUE ZEROES.
           05  MEMBERS-BX        PIC 9(03) VALUE ZEROES.

       01  PT-FINAL-TOTALS.
           05  PT-TOTAL-MEMBERS    PIC 9(03) VALUE ZEROES.

       01  WS-CURRENT-DATE-FIELDS.
           05  WS-CURRENT-DATE.
               10  WS-CURRENT-YEAR         PIC 9(04).
               10  WS-CURRENT-MONTH        PIC 9(02).
               10  WS-CURRENT-DAY          PIC 9(02).

      ************************** MAIN HEADING ***************************
       01  TITLE-UNDERLINE.
           05  FILLER PIC X(01) VALUE '+'.
           05  FILLER PIC X(64) VALUE ALL '-'.
           05  FILLER PIC X(01) VALUE '+'.

       01  PROGRAM-HEADING-LINE.
           05  FILLER PIC X(01) VALUE '|'.
           05  FILLER PIC X(21) VALUE SPACES.
           05  FILLER PIC X(22) VALUE 'CLUB MANAGEMENT SYSTEM'.
           05  FILLER PIC X(21) VALUE SPACES.
           05  FILLER PIC X(01) VALUE '|'.

      ******************** MEMBERSHIP SELECTION MENU ********************

       01  MEM-MENU-LINE-1.
           05  FILLER   PIC X(06) VALUE SPACES.
           05  FILLER   PIC X(09) VALUE '[3]      '.
           05  FILLER   PIC X(27) VALUE 'BRONZE - 1 MONTH MEMBERSHIP'.
           05  FILLER   PIC X(13) VALUE ' + INCLUSIONS'.
           05  FILLER   PIC X(13) VALUE ' (P 1,000.00)'.

       01  MEM-MENU-LINE-2.
           05  FILLER   PIC X(06) VALUE SPACES.
           05  FILLER   PIC X(09) VALUE '[2]      '.
           05  FILLER   PIC X(28) VALUE 'SILVER - 3 MONTHS MEMBERSHIP'.
           05  FILLER   PIC X(13) VALUE ' + INCLUSIONS'.
           05  FILLER   PIC X(13) VALUE ' (P 2,500.00)'.

       01  MEM-MENU-LINE-3.
           05  FILLER   PIC X(06) VALUE SPACES.
           05  FILLER   PIC X(09) VALUE '[1]      '.
           05  FILLER   PIC X(26) VALUE 'GOLD - 6 MONTHS MEMBERSHIP'.
           05  FILLER   PIC X(13) VALUE ' + INCLUSIONS'.
           05  FILLER   PIC X(13) VALUE ' (P 4,500.00)'.

      ************************* SALES REPORT ****************************
       01  MT-HEADING-LINE-1.
           05  FILLER       PIC X(20) VALUE SPACES.
           05  FILLER       PIC X(22) VALUE 'SALES REPORT FOR YEAR '.
           05  HDR-YR       PIC 9(04).

       01  MT-HEADING-LINE-2.
           05  FILLER       PIC X(25) VALUE '**** MEMBERSHIP TYPE ****'.

       01  MT-HEADING-LINE-3.
           05  FILLER         PIC X(15) VALUE 'MEMBERSHIP TYPE'.
           05  FILLER         PIC X(07) VALUE SPACES.
           05  FILLER         PIC X(07) VALUE 'MEMBERS'.
           05  FILLER         PIC X(13) VALUE SPACES.
           05  FILLER         PIC X(03) VALUE 'FEE'.
           05  FILLER         PIC X(14) VALUE SPACES.
           05  FILLER         PIC X(05) VALUE 'TOTAL'.

       01  UNDERLINES.
           05  TOPIC-UNDERLINE-1         PIC X(66) VALUE ALL '-'.
           05  TOPIC-UNDERLINE-2         PIC X(66) VALUE ALL '='.

       01  MT-SALES-LINE-BR.
           05  MEMTYPE-NAME-BR       PIC X(06) VALUE 'BRONZE'.
           05  FILLER                PIC X(18) VALUE SPACES.
           05  PRN-MEMBERS-BR        PIC Z9.
           05  FILLER                PIC X(13) VALUE SPACES.
           05  PRN-FEE-BR            PIC 9,999.99.
           05  FILLER                PIC X(09) VALUE SPACES.
           05  PRN-PAYMENTS-BR       PIC ZZZ,ZZ9.99.

       01  MT-SALES-LINE-SL.
           05  MEMTYPE-NAME-SL       PIC X(06) VALUE 'SILVER'.
           05  FILLER                PIC X(18) VALUE SPACES.
           05  PRN-MEMBERS-SL        PIC Z9.
           05  FILLER                PIC X(13) VALUE SPACES.
           05  PRN-FEE-SL            PIC 9,999.99.
           05  FILLER                PIC X(09) VALUE SPACES.
           05  PRN-PAYMENTS-SL       PIC ZZZ,ZZ9.99.

       01  MT-SALES-LINE-GD.
           05  MEMTYPE-NAME-GD       PIC X(06) VALUE 'GOLD'.
           05  FILLER                PIC X(18) VALUE SPACES.
           05  PRN-MEMBERS-GD        PIC Z9.
           05  FILLER                PIC X(13) VALUE SPACES.
           05  PRN-FEE-GD            PIC 9,999.99.
           05  FILLER                PIC X(09) VALUE SPACES.
           05  PRN-PAYMENTS-GD       PIC ZZZ,ZZ9.99.

       01  MT-TOTAL-SALES-LINE.
           05  FILLER                    PIC X(06) VALUE 'TOTALS'.
           05  FILLER                    PIC X(18) VALUE SPACES.
           05  PRN-TOTAL-MEMBERS         PIC ZZ9.
           05  FILLER                    PIC X(29) VALUE SPACES.
           05  PRN-TOTAL-PAYMENTS        PIC ZZZ,ZZ9.99.

      ************************ PROGRAM REPORT ***************************
       01  PT-HEADING-LINE-1.
           05  FILLER       PIC X(20) VALUE SPACES.
           05  FILLER       PIC X(23) VALUE 'PROGRAM MEMBERS REPORT '.
           05  HDR-YR-1     PIC 9(04).

       01  PT-HEADING-LINE-2.
           05  FILLER       PIC X(22) VALUE '**** PROGRAM TYPE ****'.

       01  PT-HEADING-LINE-3.
           05  FILLER         PIC X(12) VALUE 'PROGRAM CODE'.
           05  FILLER         PIC X(07) VALUE SPACES.
           05  FILLER         PIC X(12) VALUE 'PROGRAM NAME'.
           05  FILLER         PIC X(25) VALUE SPACES.
           05  FILLER         PIC X(07) VALUE 'MEMBERS'.

       01  PT-TYPE-LINE-1.
           05  FILLER              PIC X(02) VALUE SPACES.
           05  PROGTYPE-CODE-ZU    PIC 9(04) VALUE 1111.
           05  FILLER              PIC X(13) VALUE SPACES.
           05  PROGTYPE-NAME-ZU    PIC X(05) VALUE 'ZUMBA'.
           05  FILLER              PIC X(34) VALUE SPACES.
           05  PRN-MEMBERS-ZU      PIC ZZ9.

       01  PT-TYPE-LINE-2.
           05  FILLER              PIC X(02) VALUE SPACES.
           05  PROGTYPE-CODE-BC    PIC 9(04) VALUE 2222.
           05  FILLER              PIC X(13) VALUE SPACES.
           05  PROGTYPE-NAME-BC    PIC X(17) VALUE 'BODY CONDITIONING'.
           05  FILLER              PIC X(22) VALUE SPACES.
           05  PRN-MEMBERS-BC      PIC ZZ9.

       01  PT-TYPE-LINE-3.
           05  FILLER              PIC X(02) VALUE SPACES.
           05  PROGTYPE-CODE-BB    PIC 9(04) VALUE 3333.
           05  FILLER              PIC X(13) VALUE SPACES.
           05  PROGTYPE-NAME-BB    PIC X(13) VALUE 'BODY BUILDING'.
           05  FILLER              PIC X(26) VALUE SPACES.
           05  PRN-MEMBERS-BB      PIC ZZ9.

       01  PT-TYPE-LINE-4.
           05  FILLER              PIC X(02) VALUE SPACES.
           05  PROGTYPE-CODE-BX    PIC 9(04) VALUE 4444.
           05  FILLER              PIC X(13) VALUE SPACES.
           05  PROGTYPE-NAME-BX    PIC X(06) VALUE 'BOXING'.
           05  FILLER              PIC X(33) VALUE SPACES.
           05  PRN-MEMBERS-BX      PIC ZZ9.

       01  PT-GRAND-TOTALS.
           05 FILLER               PIC X(06) VALUE 'TOTALS'.
           05 FILLER               PIC X(52) VALUE SPACES.
           05 PRN-PT-TOTAL-MEMBERS PIC ZZ9.

      ******************************************************************
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM MAIN-MENU.

       MAIN-MENU.
           DISPLAY PROPER-SPACING.
           DISPLAY TITLE-UNDERLINE.
           DISPLAY PROGRAM-HEADING-LINE.
           DISPLAY TITLE-UNDERLINE.
           DISPLAY PROPER-SPACING.
           DISPLAY "SELECT YOUR TRANSACTION (1-2-3-4-5-6-7-0)"
           DISPLAY PROPER-SPACING.
           DISPLAY "      [1]      REGISTER NEW MEMBER"
           DISPLAY "      [2]      UPDATE USER RECORD"
           DISPLAY "      [3]      DELETE USER RECORD"
           DISPLAY "      [4]      VIEW EXERCISE PROGRAM RECORD"
           DISPLAY "      [5]      VIEW USER RECORD"
           DISPLAY "      [6]      VIEW YEAR-END REPORTS".
           DISPLAY "      [7]      VIEW PROGRAM REPORTS".
           DISPLAY "      [0]      EXIT".
           DISPLAY PROPER-SPACING.
           DISPLAY PROPER-SPACING.
           PERFORM GET-MENU-ANS.

       GET-MENU-ANS.
           DISPLAY "ANSWER:".
           DISPLAY "=======".
           ACCEPT WS-CHOICE.

           IF WS-CHOICE = 1
              OPEN I-O USER-FILE
              PERFORM SIGN-UP-USER

           ELSE IF WS-CHOICE = 2
              OPEN I-O USER-FILE
              PERFORM CHANGE-USER-RECORD

           ELSE IF WS-CHOICE = 3
              OPEN I-O USER-FILE
              PERFORM DELETE-USER-RECORD

           ELSE IF WS-CHOICE = 4
               PERFORM VIEW-PROGRAM-RECORD

           ELSE IF WS-CHOICE = 5
              OPEN INPUT USER-FILE
              PERFORM VIEW-USER-RECORD

           ELSE IF WS-CHOICE = 6
              PERFORM VIEW-YEAR-END-REPORTS

           ELSE IF WS-CHOICE = 7
              PERFORM VIEW-PROGRAM-REPORTS

           ELSE IF WS-CHOICE = 0
              PERFORM PROGRAM-DONE

           ELSE
               DISPLAY PROPER-SPACING
               DISPLAY "PLEASE ENTER A VALID CHOICE."
               DISPLAY PROPER-SPACING
               PERFORM GET-MENU-ANS.

      ******************************************************************
      *                    REGISTER NEW MEMBER                         *
      ******************************************************************

       SIGN-UP-USER.
           DISPLAY PROPER-SPACING.
           DISPLAY "                     REGISTERING NEW MEMBER"
           DISPLAY TITLE-UNDERLINE.
           DISPLAY PROPER-SPACING.
           DISPLAY "Fill up the following before proceeding:"

           PERFORM ANSWER-HEALTH-DECLARATION.
           PERFORM OTHER-USER-INFO.
           PERFORM CHOOSE-MEMBERSHIP-TYPE.
           PERFORM GET-DATE-JOINED.
           PERFORM GET-DATE-EXPIRATION.
           PERFORM TERMS-AND-CONDITIONS-AGREEMENT.
           PERFORM ADD-SALES-REC.
           PERFORM VIEW-USER-INFO.
           PERFORM ADD-THIS-USER.
           PERFORM CHOOSE-PROGRAM.
           PERFORM ADD-THIS-USER-PROGRAM.

           CLOSE USER-FILE
           PERFORM ASK-ANOTHER-TRANSACTION.

       ANSWER-HEALTH-DECLARATION.
           DISPLAY PROPER-SPACING.
           DISPLAY "                     HEALTH DECLARATION FORM"
           DISPLAY TITLE-UNDERLINE.
           DISPLAY PROPER-SPACING.
           DISPLAY "Any underlying health conditions?"
           DISPLAY PROPER-SPACING.
           DISPLAY "Heart Condition: "
               PERFORM ACCEPT-HF-ANSWER.
           DISPLAY PROPER-SPACING.
           DISPLAY "Hypertension: "
               PERFORM ACCEPT-HF-ANSWER.
           DISPLAY PROPER-SPACING.
           DISPLAY "Asthma: "
               PERFORM ACCEPT-HF-ANSWER.
           DISPLAY PROPER-SPACING.
           DISPLAY "Lung Disease: "
               PERFORM ACCEPT-HF-ANSWER.
           DISPLAY PROPER-SPACING.
           DISPLAY "Advance Malignancy / Cancer Bone / Joint Inquiry: "
               PERFORM ACCEPT-HF-ANSWER.
           DISPLAY PROPER-SPACING.
           DISPLAY "Severe Anemia: "
               PERFORM ACCEPT-HF-ANSWER.

       ACCEPT-HF-ANSWER.
           DISPLAY "Press [Y] = Yes | [N] = No"
           ACCEPT HF-ANS.
               IF HF-ANS = 'Y' OR HF-ANS = 'y'
                   PERFORM USER-INELIGIBLE
               ELSE IF HF-ANS = 'N' or HF-ANS = 'n'
                   CONTINUE
               ELSE
                   DISPLAY PROPER-SPACING
                   PERFORM ACCEPT-HF-ANSWER.

       USER-INELIGIBLE.
               DISPLAY PROPER-SPACING.
               DISPLAY "Registrant is not fit to join membership.".
               CLOSE USER-FILE.
               PERFORM ASK-ANOTHER-TRANSACTION.

       OTHER-USER-INFO.
           DISPLAY PROPER-SPACING.
           DISPLAY "                     USER INFORMATION SHEET"
           DISPLAY TITLE-UNDERLINE.
           DISPLAY PROPER-SPACING.
           PERFORM GET-USER-ID.
           DISPLAY "MEMBERSHIP ID. NO: " ID-NUMBER.

           DISPLAY PROPER-SPACING.
               DISPLAY "FULL NAME:  "
               ACCEPT MEMBER-NAME
           DISPLAY PROPER-SPACING.
               DISPLAY "AGE: "
               ACCEPT MEMBER-AGE.
           DISPLAY PROPER-SPACING.
               DISPLAY "SEX: [F] = Female [M] = Male [O] = Other"
               ACCEPT MEMBER-SEX.
           DISPLAY PROPER-SPACING.
               DISPLAY "CONTACT NUMBER: (09XXXXXXXXX)"
               ACCEPT MEMBER-CONT-NUM.
           DISPLAY PROPER-SPACING.
               DISPLAY "ADDRESS (HOUSE NO. / STREET / BARANGAY / CITY)"
               ACCEPT MEMBER-ADDRESS.

      ***** FOR EMERGENCY PURPOSES
           DISPLAY PROPER-SPACING.
           DISPLAY PROPER-SPACING.
               DISPLAY "IN CASE OF EMERGENCY"
               DISPLAY "--------------------"
               DISPLAY "EMERGENCY CONTACT PERSON: "
               ACCEPT EMERGENCY-NAME.
           DISPLAY PROPER-SPACING.
               DISPLAY "RELATIONSHIP: "
               ACCEPT EMERGENCY-REL.
           DISPLAY PROPER-SPACING.
               DISPLAY "CONTACT NUMBER: (09XXXXXXXXX)"
               ACCEPT EMERGENCY-CONT-NUM.

       GET-USER-ID.
           MOVE ZEROES TO ID-NUMBER.
           MOVE 'N' TO EOF-SWITCH
           PERFORM UNTIL EOF-SWITCH = 'Y'
               READ USER-FILE
                   AT END
                       MOVE ID-NUMBER TO TEMP-USER-ID
                       MOVE 'Y' TO EOF-SWITCH
               END-READ
           END-PERFORM.

           ADD 1 TO TEMP-USER-ID
           MOVE TEMP-USER-ID TO ID-NUMBER.

       CHOOSE-MEMBERSHIP-TYPE.
           DISPLAY PROPER-SPACING.
           DISPLAY PROPER-SPACING.
           DISPLAY "                       MEMBERSHIP SELECTION"
           DISPLAY TITLE-UNDERLINE.
           DISPLAY PROPER-SPACING.
           DISPLAY MEM-MENU-LINE-1.
           DISPLAY MEM-MENU-LINE-2.
           DISPLAY MEM-MENU-LINE-3.
           DISPLAY PROPER-SPACING.
           DISPLAY "MEMBERSHIP TYPE: ".
           ACCEPT WS-MEMBERSHIP-NUM.

           IF WS-MEMBERSHIP-NUM = 3
              MOVE 'BRONZE' TO MEMBERSHIP-TYPE
           ELSE IF WS-MEMBERSHIP-NUM = 2
              MOVE 'SILVER' TO MEMBERSHIP-TYPE
           ELSE IF WS-MEMBERSHIP-NUM = 1
              MOVE 'GOLD' TO MEMBERSHIP-TYPE
           ELSE
              DISPLAY "PRESS [3], [2], OR [1] ONLY."
              PERFORM CHOOSE-MEMBERSHIP-TYPE.

       GET-DATE-JOINED.
           DISPLAY PROPER-SPACING.
           DISPLAY "ENTER STARTING DATE OF SUBSCRIPTION."
           DISPLAY "ENTER DATE: (MM/DD/YYYY) "
           ACCEPT DATE-JOINED.

       GET-DATE-EXPIRATION.
           MOVE DATE-JOINED TO DATE-EXPIRE.

           IF WS-MEMBERSHIP-NUM = 3
               ADD 1 TO MONTH-EXPIRE.

           IF WS-MEMBERSHIP-NUM = 2
               ADD 3 TO MONTH-EXPIRE.

           IF WS-MEMBERSHIP-NUM = 1
               ADD 6 TO MONTH-EXPIRE.

      ***** COMPUTE EXPIRATION OF SUBSCRIPTION
           COMPUTE YEAR-EXPIRE = YEAR-EXPIRE + (MONTH-EXPIRE / 12)
           COMPUTE MONTH-EXPIRE = MONTH-JOINED + (FUNCTION MOD
               (MONTH-EXPIRE, 12) - MONTH-JOINED).

               IF MONTH-EXPIRE = 0
                   MOVE 12 TO MONTH-EXPIRE
                   SUBTRACT 1 FROM YEAR-EXPIRE.

       DISPLAY-TOC.
           MOVE 'N' TO EOF-SWITCH.
           OPEN INPUT TOC-AGREEMENT
           DISPLAY PROPER-SPACING.
           PERFORM UNTIL EOF-SWITCH = 'Y'
                  READ TOC-AGREEMENT
                      AT END MOVE 'Y' TO EOF-SWITCH
                      NOT AT END DISPLAY PRINT-LINE-TERMS
                  END-READ
           END-PERFORM
           CLOSE TOC-AGREEMENT.

       TERMS-AND-CONDITIONS-AGREEMENT.
           PERFORM DISPLAY-TOC.
           DISPLAY PROPER-SPACING.
           DISPLAY "AGREE WITH TERMS AND CONDITIONS? "
           DISPLAY "PRESS [Y] = AGREE. PRESS [N] = DISAGREE".
           ACCEPT WS-TOC-ANS.

               IF WS-TOC-ANS = 'Y' OR 'y'
                  CONTINUE
               ELSE
                  DISPLAY "TRANSACTION CANCELLED."
                  CLOSE USER-FILE
                  PERFORM ASK-ANOTHER-TRANSACTION.

       ADD-SALES-REC.
           OPEN EXTEND SALES-FILE
           IF WS-MEMBERSHIP-NUM = 3
              MOVE 12 TO MEMTYPE-CODE
              MOVE 1000 TO MEMTYPE-FEE.

           IF WS-MEMBERSHIP-NUM = 2
              MOVE 14 TO MEMTYPE-CODE
              MOVE 2500 TO MEMTYPE-FEE.

           IF WS-MEMBERSHIP-NUM = 1
              MOVE 16 TO MEMTYPE-CODE
              MOVE 4500 TO MEMTYPE-FEE.

           MOVE YEAR-JOINED TO MT-YR.
           WRITE SALES-REC BEFORE ADVANCING 1.
           CLOSE SALES-FILE.

       VIEW-USER-INFO.
           DISPLAY PROPER-SPACING.
           DISPLAY "   USER INFORMATION    "
           DISPLAY "-----------------------"

           DISPLAY "ID NUMBER             >>>     " ID-NUMBER.
           DISPLAY "NAME                  >>>     " MEMBER-NAME.
           DISPLAY "AGE                   >>>     " MEMBER-AGE.
           DISPLAY "SEX                   >>>     " MEMBER-SEX.
           DISPLAY "CONTACT NO.           >>>     " MEMBER-CONT-NUM.
           DISPLAY "HOME ADDRESS          >>>     " MEMBER-ADDRESS.

           DISPLAY PROPER-SPACING.
           DISPLAY "EMERGENCY CONTACT PERSON".
           DISPLAY "------------------------"
           DISPLAY "NAME                  >>>     " EMERGENCY-NAME.
           DISPLAY "CONTACT NO.           >>>     " EMERGENCY-CONT-NUM.
           DISPLAY "RELATIONSHIP          >>>     " EMERGENCY-REL.

           DISPLAY PROPER-SPACING.
           DISPLAY "  SUBSCRIPTION DETAILS  "
           DISPLAY "------------------------"
           DISPLAY "MEMBERSHIP TYPE       >>>     " MEMBERSHIP-TYPE.
           DISPLAY "START OF SUBSCRIPTION >>>     " DATE-JOINED.
           DISPLAY "END OF SUBSCRIPTION   >>>     " DATE-EXPIRE.
           DISPLAY PROPER-SPACING.

       ADD-THIS-USER.
           WRITE MEMBER-RECORD BEFORE ADVANCING 1.
           DISPLAY PROPER-SPACING.
           DISPLAY "MEMBER ADDED.".

      ******************************************************************
      *                  WRITE MEMBERS LIST IN PROGRAM                 *
      ******************************************************************
       PROGRAM-MENU.
           DISPLAY PROPER-SPACING.
           DISPLAY "                  DISPLAYING AVAILABLE PROGRAM"
           DISPLAY TITLE-UNDERLINE.
           DISPLAY PROPER-SPACING.
           DISPLAY "      [1]      ENTERING ZUMBA CLASS"
           DISPLAY "      [2]      ENTERING BODY CONDITIONING CLASS"
           DISPLAY "      [3]      ENTERING BODY BUILDING CLASS"
           DISPLAY "      [4]      ENTERING BOXING CLASS"
           DISPLAY PROPER-SPACING.
           DISPLAY "SELECT PROGRAM TYPE: ".
           DISPLAY "====================".
           ACCEPT PROGRAM-TYPE.

       ADD-THIS-USER-PROGRAM.
           MOVE YEAR-JOINED TO PT-YR.
           WRITE PT-REC BEFORE ADVANCING 1 LINE.
           CLOSE PT-FILE.

       CHOOSE-PROGRAM.
           PERFORM PROGRAM-MENU
           OPEN EXTEND PT-FILE
           MOVE 'N' TO EOF-SWITCH

           IF PROGRAM-TYPE = 1
               MOVE 1111 TO PROGTYPE-CODE
               OPEN INPUT ZUM
               PERFORM UNTIL EOF-SWITCH = 'Y'
                     READ ZUM
                         AT END MOVE 'Y' TO EOF-SWITCH
                         NOT AT END DISPLAY ZUM-RECORD
                     END-READ
                END-PERFORM
                CLOSE ZUM
                OPEN EXTEND ZUMBAMEM
                    DISPLAY PROPER-SPACING
                    DISPLAY "MEMBER ADDED."
                    MOVE MEMBER-NAME TO ZUMBA-NAMES
                    WRITE ZUMBA-LIST BEFORE ADVANCING 1
                END-WRITE
                CLOSE ZUMBAMEM
           ELSE IF PROGRAM-TYPE = 2
               MOVE 2222 TO PROGTYPE-CODE
               OPEN INPUT BC
               PERFORM UNTIL EOF-SWITCH = 'Y'
                     READ BC
                         AT END MOVE 'Y' TO EOF-SWITCH
                         NOT AT END DISPLAY BC-RECORD
                     END-READ
                END-PERFORM
                CLOSE BC
                OPEN EXTEND BODYCONMEM
                    DISPLAY PROPER-SPACING
                    DISPLAY "MEMBER ADDED."
                    MOVE MEMBER-NAME TO BODYCON-NAMES
                    WRITE BODYCON-LIST BEFORE ADVANCING 1
                END-WRITE
                CLOSE BODYCONMEM
           ELSE IF PROGRAM-TYPE = 3
               MOVE 3333 TO PROGTYPE-CODE
               OPEN INPUT BB
               PERFORM UNTIL EOF-SWITCH = 'Y'
                     READ BB
                         AT END MOVE 'Y' TO EOF-SWITCH
                         NOT AT END DISPLAY BB-RECORD
                     END-READ
                END-PERFORM
                CLOSE BB
                OPEN EXTEND BODYBUILDMEM
                    DISPLAY PROPER-SPACING
                    DISPLAY "MEMBER ADDED."
                    MOVE MEMBER-NAME TO BODYBUILD-NAMES
                    WRITE BODYBUILD-LIST BEFORE ADVANCING 1
                END-WRITE
                CLOSE BODYBUILDMEM
           ELSE IF PROGRAM-TYPE = 4
               MOVE 4444 TO PROGTYPE-CODE
               OPEN INPUT BX
               PERFORM UNTIL EOF-SWITCH = 'Y'
                     READ BX
                         AT END MOVE 'Y' TO EOF-SWITCH
                         NOT AT END DISPLAY BOX-RECORD
                     END-READ
               END-PERFORM
               CLOSE BX
               OPEN EXTEND BOXINGMEM
                    DISPLAY PROPER-SPACING
                    DISPLAY "MEMBER ADDED."
                    MOVE MEMBER-NAME TO BOXING-NAMES
                    WRITE BOXING-LIST BEFORE ADVANCING 1
               END-WRITE
               CLOSE BOXINGMEM
           ELSE
              DISPLAY "PRESS [4], [3], [2], OR [1] ONLY."
              CLOSE PT-FILE
              PERFORM CHOOSE-PROGRAM.

       VIEW-PROGRAM-RECORD.
           DISPLAY PROPER-SPACING.
           MOVE 'N' TO EOF-SWITCH.
           PERFORM PROGRAM-MENU.
           IF PROGRAM-TYPE = 1
               OPEN INPUT ZUMBAMEM
               PERFORM UNTIL EOF-SWITCH = 'Y'
                     READ ZUMBAMEM
                         AT END MOVE 'Y' TO EOF-SWITCH
                         NOT AT END DISPLAY ZUMBA-LIST
                     END-READ
               END-PERFORM
               CLOSE ZUMBAMEM

           ELSE IF PROGRAM-TYPE = 2
               OPEN INPUT BODYCONMEM
               PERFORM UNTIL EOF-SWITCH = 'Y'
                     READ BODYCONMEM
                         AT END MOVE 'Y' TO EOF-SWITCH
                         NOT AT END DISPLAY BODYCON-LIST
                     END-READ
               END-PERFORM
               CLOSE BODYCONMEM

           ELSE IF PROGRAM-TYPE = 3
               OPEN INPUT BODYBUILDMEM
               PERFORM UNTIL EOF-SWITCH = 'Y'
                     READ BODYBUILDMEM
                         AT END MOVE 'Y' TO EOF-SWITCH
                         NOT AT END DISPLAY BODYBUILD-LIST
                     END-READ
                END-PERFORM
                CLOSE BODYBUILDMEM

           ELSE IF PROGRAM-TYPE = 4
               OPEN INPUT BOXINGMEM
               PERFORM UNTIL EOF-SWITCH = 'Y'
                     READ BOXINGMEM
                         AT END MOVE 'Y' TO EOF-SWITCH
                         NOT AT END DISPLAY BOXING-LIST
                     END-READ
                END-PERFORM
                CLOSE BOXINGMEM
           ELSE
               DISPLAY "INVALID CHOICE".
           PERFORM ASK-ANOTHER-TRANSACTION.

      ******************************************************************
      *                  WRITE MEMBERS LIST IN PROGRAM                 *
      ******************************************************************
       FINDING-USER.
           MOVE 'N' TO IS-REC-FOUND.
           MOVE 'N' TO EOF-SWITCH.
           PERFORM GET-ID-NUMBER.
           MOVE SPACES TO MEMBER-RECORD.
           PERFORM READ-RECORD
               UNTIL IS-REC-FOUND = 'Y' OR EOF-SWITCH = 'Y'.
           DISPLAY PROPER-SPACING.
           PERFORM VIEW-USER-INFO.

       GET-ID-NUMBER.
           MOVE ZEROES TO WS-USER-ID.
           DISPLAY PROPER-SPACING.
           DISPLAY "ENTER MEMBERSHIP ID. NO.: "
           DISPLAY "=========================".
               ACCEPT WS-USER-ID.

       READ-RECORD.
           READ USER-FILE
               AT END
                   MOVE 'Y' TO EOF-SWITCH
               NOT AT END
                   IF WS-USER-ID IS EQUAL TO ID-NUMBER THEN
                      MOVE 'Y' TO IS-REC-FOUND.

      ******************************************************************
      *                       VIEW USER RECORD                         *
      ******************************************************************

       VIEW-USER-RECORD.
           PERFORM FINDING-USER.
           CLOSE USER-FILE.
           PERFORM ASK-ANOTHER-TRANSACTION.

      ******************************************************************
      *                     UPDATE USER RECORD                         *
      ******************************************************************

       CHANGE-USER-RECORD.
           PERFORM FINDING-USER
           MOVE 'N' TO EOF-SWITCH
           PERFORM MODIFY-CONTENT

           PERFORM UPDATE-RECORD UNTIL EOF-SWITCH = 'Y'.
           DISPLAY PROPER-SPACING.
           PERFORM VIEW-USER-INFO.
           DISPLAY "USER INFORMATION CHANGED."
           DISPLAY PROPER-SPACING.

           CLOSE USER-FILE.
           PERFORM ASK-ANOTHER-TRANSACTION.

       MODIFY-CONTENT.
           MOVE ZEROES TO OK-TO-CHANGE.
           DISPLAY PROPER-SPACING.
           DISPLAY "  PRESS:  "
           DISPLAY PROPER-SPACING.
           DISPLAY "      [1]     TO EDIT NAME"
           DISPLAY "      [2]     TO EDIT AGE"
           DISPLAY "      [3]     TO EDIT SEX"
           DISPLAY "      [4]     TO EDIT CONTACT NO"
           DISPLAY "      [5]     TO EDIT ADDRESS"
           DISPLAY "      [6]     TO EDIT EMERGENCY CONTACT INFORMATION"
           DISPLAY "      [0]     TO BACK TO MAIN MENU"
           DISPLAY PROPER-SPACING.
           DISPLAY "ANSWER:".
           DISPLAY "=======".
           ACCEPT OK-TO-CHANGE.

           DISPLAY PROPER-SPACING.
           IF OK-TO-CHANGE = 1
               DISPLAY "NAME: "
               ACCEPT CHANGE-MEMBER-NAME
               MOVE CHANGE-MEMBER-NAME TO MEMBER-NAME

           ELSE IF OK-TO-CHANGE = 2
               DISPLAY "AGE:"
               ACCEPT CHANGE-MEMBER-AGE
               MOVE CHANGE-MEMBER-AGE TO MEMBER-AGE

           ELSE IF OK-TO-CHANGE = 3
               DISPLAY "SEX: "
               ACCEPT CHANGE-MEMBER-SEX
               MOVE CHANGE-MEMBER-SEX TO MEMBER-SEX

           ELSE IF OK-TO-CHANGE = 4
               DISPLAY "CONTACT NO.: "
               ACCEPT CHANGE-MEMBER-CONT-NUM
               MOVE CHANGE-MEMBER-CONT-NUM TO MEMBER-CONT-NUM

           ELSE IF OK-TO-CHANGE = 5
               DISPLAY "ADDRESS: "
               ACCEPT CHANGE-MEMBER-ADDRESS
               MOVE CHANGE-MEMBER-ADDRESS TO MEMBER-ADDRESS

           ELSE IF OK-TO-CHANGE = 6
               DISPLAY "CHANGE EMERGENCY CONTACT INFO: "
               DISPLAY "EMERGENCY CONTACT PERSON: "
               ACCEPT CHANGE-EMERGENCY-NAME
                   MOVE CHANGE-EMERGENCY-NAME TO EMERGENCY-NAME
               DISPLAY PROPER-SPACING
               DISPLAY "RELATIONSHIP: "
               ACCEPT CHANGE-EMERGENCY-REL
                   MOVE CHANGE-EMERGENCY-REL TO EMERGENCY-REL
               DISPLAY PROPER-SPACING
               DISPLAY "CONTACT NO.: "
               ACCEPT CHANGE-EMERGENCY-CONT-NUM
                   MOVE CHANGE-EMERGENCY-CONT-NUM TO EMERGENCY-CONT-NUM
           ELSE
              CLOSE USER-FILE
              DISPLAY "GOING BACK TO MENU"
              PERFORM MAIN-MENU.

       UPDATE-RECORD.
           REWRITE MEMBER-RECORD.
           MOVE 'Y' TO EOF-SWITCH.

      ******************************************************************
      *                     DELETE USER RECORD                         *
      ******************************************************************

       DELETE-USER-RECORD.
           PERFORM FINDING-USER
           MOVE 'N' TO EOF-SWITCH
           PERFORM DELETE-USER.

           PERFORM UPDATE-RECORD UNTIL EOF-SWITCH = 'Y'.
           DISPLAY PROPER-SPACING.
           DISPLAY "USER INFORMATION DELETED."
           CLOSE USER-FILE

           PERFORM ASK-ANOTHER-TRANSACTION.

       DELETE-USER.
           DISPLAY "DELETE THIS USER INFORMATION? "
           DISPLAY "PRESS [Y] = AGREE. PRESS [N] = DISAGREE".
           ACCEPT OK-TO-CHANGE.

               IF OK-TO-CHANGE = 'Y' OR 'y'
                  MOVE SPACES TO MEMBER-NAME
                  MOVE SPACES TO MEMBERSHIP-TYPE
                  MOVE 0 TO MEMBER-AGE
                  MOVE SPACES TO MEMBER-SEX
                  MOVE 0 TO MEMBER-CONT-NUM
                  MOVE SPACES TO MEMBER-ADDRESS
                  MOVE SPACES TO DATE-JOINED
                  MOVE SPACES TO DATE-EXPIRE
                  MOVE SPACES TO EMERGENCY-CONTACT-INFO
               ELSE
                  CLOSE USER-FILE
                  DISPLAY "TRANSACTION CANCELLED."
                  PERFORM ASK-ANOTHER-TRANSACTION.

      ******************************************************************
      *                    VIEW YEAR-END REPORTS                       *
      ******************************************************************

       VIEW-YEAR-END-REPORTS.
           SORT WORK-FILE
               ON ASCENDING KEY MEMTYPE-CODE
               USING SALES-FILE
               GIVING SORTED-SLS-FILE.
           PERFORM PRINT-SALES-REPORT.
           PERFORM ASK-ANOTHER-TRANSACTION.

       PRINT-SALES-REPORT.
           PERFORM PRINT-SALES-HEADINGS.
           OPEN INPUT SORTED-SLS-FILE.

           MOVE 'N' TO EOF-SWITCH
           READ SORTED-SLS-FILE
               AT END
                   MOVE 'Y' TO EOF-SWITCH
           END-READ

           MOVE ZEROES TO MT-TOTALS-BR, MT-TOTALS-SL,
               MT-TOTALS-GD, MT-FINAL-TOTALS.
           PERFORM COMPUTE-SALES
               UNTIL EOF-SWITCH = 'Y'.

           PERFORM PRINT-SALES-LINES.
           CLOSE SORTED-SLS-FILE.

       PRINT-SALES-HEADINGS.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS.
           MOVE WS-CURRENT-YEAR TO HDR-YR.
           DISPLAY PROPER-SPACING.
           DISPLAY TITLE-UNDERLINE.
           DISPLAY PROGRAM-HEADING-LINE.
           DISPLAY TITLE-UNDERLINE.
           DISPLAY PROPER-SPACING.
           DISPLAY MT-HEADING-LINE-1
           DISPLAY PROPER-SPACING.
           DISPLAY MT-HEADING-LINE-2
           DISPLAY PROPER-SPACING.
           DISPLAY MT-HEADING-LINE-3
           DISPLAY TOPIC-UNDERLINE-1.

       PRINT-SALES-LINES.
           MOVE MEMBERS-BR TO PRN-MEMBERS-BR
           MOVE PAYMENTS-BR TO PRN-PAYMENTS-BR
           MOVE 1000 TO PRN-FEE-BR
           DISPLAY MT-SALES-LINE-BR.

           MOVE MEMBERS-SL TO PRN-MEMBERS-SL
           MOVE PAYMENTS-SL TO PRN-PAYMENTS-SL
           MOVE 2500 TO PRN-FEE-SL
           DISPLAY MT-SALES-LINE-SL.

           MOVE MEMBERS-GD TO PRN-MEMBERS-GD
           MOVE PAYMENTS-GD TO PRN-PAYMENTS-GD
           MOVE 4500 TO PRN-FEE-GD
           DISPLAY MT-SALES-LINE-GD.

           DISPLAY TOPIC-UNDERLINE-2.
           MOVE TOTAL-MEMBERS TO PRN-TOTAL-MEMBERS.
           MOVE TOTAL-PAYMENTS TO PRN-TOTAL-PAYMENTS.
           DISPLAY MT-TOTAL-SALES-LINE.
           DISPLAY PROPER-SPACING.

       COMPUTE-SALES.
           IF MEMTYPE-CODE-SF = 12 AND MT-YR-SF = WS-CURRENT-YEAR
               ADD 1 TO MEMBERS-BR, TOTAL-MEMBERS
               MOVE MEMTYPE-FEE-SF TO PRN-FEE-BR
               ADD MEMTYPE-FEE-SF TO PAYMENTS-BR, TOTAL-PAYMENTS.

           IF MEMTYPE-CODE-SF = 14 AND MT-YR-SF = WS-CURRENT-YEAR
               ADD 1 TO MEMBERS-SL, TOTAL-MEMBERS
               MOVE MEMTYPE-FEE-SF TO PRN-FEE-SL
               ADD MEMTYPE-FEE-SF TO PAYMENTS-SL, TOTAL-PAYMENTS.

           IF MEMTYPE-CODE-SF = 16 AND MT-YR-SF = WS-CURRENT-YEAR
               ADD 1 TO MEMBERS-GD, TOTAL-MEMBERS
               MOVE MEMTYPE-FEE-SF TO PRN-FEE-GD
               ADD MEMTYPE-FEE-SF TO PAYMENTS-GD, TOTAL-PAYMENTS.

           READ SORTED-SLS-FILE
                AT END MOVE 'Y' TO EOF-SWITCH
           END-READ.

      ******************************************************************
      *                    VIEW PROGRAM REPORTS                        *
      ******************************************************************

       VIEW-PROGRAM-REPORTS.
           SORT WORK-FILE-1
               ON ASCENDING KEY PROGTYPE-CODE
               USING PT-FILE
               GIVING SORTED-PT-FILE.
           PERFORM PRINT-PROGRAM-REPORT.
           PERFORM ASK-ANOTHER-TRANSACTION.

       PRINT-PROGRAM-REPORT.
           PERFORM PRINT-PT-HEADINGS.
           OPEN INPUT SORTED-PT-FILE.

           MOVE 'N' TO EOF-SWITCH
           READ SORTED-PT-FILE
               AT END
                   MOVE 'Y' TO EOF-SWITCH
           END-READ.

           MOVE ZEROES TO PT-MEMBER-TOTALS, PT-FINAL-TOTALS.
           PERFORM COMPUTE-PT-MEMBERS
               UNTIL EOF-SWITCH = 'Y'.

           PERFORM PRINT-PT-LINES.
           CLOSE SORTED-PT-FILE.

       PRINT-PT-HEADINGS.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS.
           MOVE WS-CURRENT-YEAR TO HDR-YR-1.
           DISPLAY PROPER-SPACING.
           DISPLAY TITLE-UNDERLINE.
           DISPLAY PROGRAM-HEADING-LINE.
           DISPLAY TITLE-UNDERLINE.
           DISPLAY PROPER-SPACING.
           DISPLAY PT-HEADING-LINE-1.
           DISPLAY PROPER-SPACING.
           DISPLAY PT-HEADING-LINE-2.
           DISPLAY PROPER-SPACING.
           DISPLAY PT-HEADING-LINE-3.
           DISPLAY TOPIC-UNDERLINE-1.

       PRINT-PT-LINES.
           MOVE MEMBERS-ZU TO PRN-MEMBERS-ZU
           DISPLAY PT-TYPE-LINE-1.

           MOVE MEMBERS-BC TO PRN-MEMBERS-BC
           DISPLAY PT-TYPE-LINE-2.

           MOVE MEMBERS-BB TO PRN-MEMBERS-BB
           DISPLAY PT-TYPE-LINE-3.

           MOVE MEMBERS-BX TO PRN-MEMBERS-BX
           DISPLAY PT-TYPE-LINE-4.

           DISPLAY TOPIC-UNDERLINE-2.
           MOVE PT-TOTAL-MEMBERS TO PRN-PT-TOTAL-MEMBERS.
           DISPLAY PT-GRAND-TOTALS.
           DISPLAY PROPER-SPACING.

       COMPUTE-PT-MEMBERS.
           IF PROGTYPE-CODE-SF = 1111 AND PT-YR-SF = WS-CURRENT-YEAR
               ADD 1 TO MEMBERS-ZU, PT-TOTAL-MEMBERS.

           IF PROGTYPE-CODE-SF = 2222 AND PT-YR-SF = WS-CURRENT-YEAR
               ADD 1 TO MEMBERS-BC, PT-TOTAL-MEMBERS.

           IF PROGTYPE-CODE-SF = 3333 AND PT-YR-SF = WS-CURRENT-YEAR
               ADD 1 TO MEMBERS-BB, PT-TOTAL-MEMBERS.

           IF PROGTYPE-CODE-SF = 4444 AND PT-YR-SF = WS-CURRENT-YEAR
               ADD 1 TO MEMBERS-BX, PT-TOTAL-MEMBERS.

           READ SORTED-PT-FILE
                AT END MOVE 'Y' TO EOF-SWITCH
           END-READ.

      ******************************************************************
      *                    ASK ANOTHER TRANSACTION                     *
      ******************************************************************

       ASK-ANOTHER-TRANSACTION.
           DISPLAY PROPER-SPACING.
           DISPLAY "+=============================+"
           DISPLAY "|     ANOTHER TRANSACTION?    |".
           DISPLAY "+=============================+"
           DISPLAY PROPER-SPACING.
           DISPLAY "PRESS [Y] = YES | [N] = NO".
           ACCEPT WS-YES-NO.

               IF WS-YES-NO = 'Y' OR 'y'
                  PERFORM MAIN-MENU
               ELSE IF WS-YES-NO = 'N' OR 'n'
                  PERFORM PROGRAM-DONE
               ELSE
                  DISPLAY PROPER-SPACING
                  DISPLAY "PRESS [Y] OR [N] ONLY."
                  PERFORM ASK-ANOTHER-TRANSACTION.

      ******************************************************************
      *                         PROGRAM DONE                           *
      ******************************************************************

       PROGRAM-DONE.
           DISPLAY PROPER-SPACING.
           DISPLAY 'THANK YOU FOR USING OUR SYSTEM.'
           STOP RUN.

       END PROGRAM FINAL-SYSTEM.
