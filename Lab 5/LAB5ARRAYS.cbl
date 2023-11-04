       IDENTIFICATION DIVISION.
       PROGRAM-ID.     LAB5ARRAYS.
       AUTHOR.         AUSTIN OGLETREE.
      *************************************************
      *      LAB 5 - ARRAYS
      *
      *    THIS PROGRAM READS A STUDENT FILE.  
      *    YOU WILL BE WORKING WITH ARRAY DATA.
      *    MAKE SURE YOU FOLLOW ALL COMMENT INSTRUCTIONS
      *******
      *  INPUT: THIS PROGRAM WILL USE THE FOLLOWING FIELDS 
      *         FROM THE STUDENT INPUT FILE:
      *         STUDENT NAME, ARRAY OF 
      *         4 TEST SCORES
      *******
      *  OUTPUT: PRINTED DETAIL SUMMARY REPORT
      *          STUDENT NAME, ORIGINAL GRADES, GRADE AVERAGE AND
      *          LETTER GRADE, A TOTAL LINE WITH A STUDENT COUNT

      *******
      *  CALCULATIONS:
      *      SUM 4 TEST SCORES
      *      AVERAGE OF INDIVIDUAL TEST SCORES
      *      TOTAL NUMBER OF STUDENTSS
      *   
      *******
      *   INSTRUCTIONS
      *   1. Follow Instruction Comments found in the Program. 
      ******
      *  NOTE:  Open the correct out put and your output 
      *         with Word or other good word processor.
      *         Don't use Notepad. 
      *********************************************      
       ENVIRONMENT DIVISION.
      *
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
           SELECT STUDENT-FILE
               ASSIGN TO "GRADES.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.
      *
           SELECT STUDENT-REPORT-FILE
               ASSIGN TO PRINTER "L5OUTPUT.TXT".
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD STUDENT-FILE
           RECORD CONTAINS 80 CHARACTERS.
      *
       01  STUDENT-RECORD.
           05  SR-CLASS-CODE                   PIC X(5).
           05  SR-NAME                         PIC X(20).
      * CODE GRADE ARRAY HERE
           05 CODE-GRADE-IN    OCCURS 4 TIMES PIC 9(3).


           05  FILLER                          PIC X(29).
      *
       FD  STUDENT-REPORT-FILE
           RECORD CONTAINS 80 CHARACTERS.
      *
       01  REPORT-LINE                     PIC X(80).
      *
       WORKING-STORAGE SECTION.
      *
       01  FLAGS-N-SWITCHES.
           05  EOF-FLAG                     PIC X      VALUE ' '.
               88 NO-MORE-DATA                         VALUE 'N'.
               88 MORE-RECORDS                         VALUE 'Y'.
           05  SUB                          PIC 99      VALUE 1.
      *
       01  REPORT-FIELDS.
           05  PROPER-SPACING               PIC 9      VALUE 1.
      *
       01  WS-CURRENT-DATE.
           05  WS-YEAR                      PIC 99.
           05  WS-MONTH                     PIC 99.
           05  WS-DAY                       PIC 99.
      *
       01  DETAIL-FIELDS.
           05  DF-TEST-TOTAL                PIC S9(5)   VALUE +0.
           05  DF-AVG                       PIC S9(3)   VALUE +0.
           
          
          
      *
       01  STUDENT-FIELD.
           05  CF-STUDENT-COUNT             PIC S99     VALUE +0.    
      *
      ********************OUTPUT AREA*********************************

       01  HEADING-1.
           05                              PIC X(6) VALUE 'DATE:'.
           05  H1-DATE.
               10  H1-MONTH                PIC Z9.
               10                          PIC X    VALUE '/'.
               10  H1-DAY                  PIC 99.
               10                          PIC X    VALUE '/'.
               10  H1-YEAR                 PIC 99.
           05                              PIC X(16) VALUE SPACE.
           05                              PIC X(14) VALUE
                                           'STUDENT REPORT'.
           05                              PIC X(24) VALUE SPACE.
           05 H1-PAGE-NO                   PIC 99 VALUE ZERO.
      *
       01  HEADING-2.
           05                              PIC X(8) VALUE SPACE.
           05                              PIC X(11) VALUE 'NAME'.
           05                              PIC X(5)  VALUE SPACES.
           05                              PIC X(8)  VALUE 'SCORE   '.
           05                              PIC X(8)  VALUE 'SCORE   '.
           05                              PIC X(8)  VALUE 'SCORE   '.
           05                              PIC X(8)  VALUE 'SCORE   '.
           05                              PIC X(7)  VALUE 'AVERAGE'.
           05                              PIC X(4)  VALUE SPACE.
           05                              PIC X(7)  VALUE 'GRADE'.
      *
       01  DETAIL-LINE.
           05                              PIC X(2)   VALUE SPACE.
           05  DL-NAME                     PIC X(20).
           05                              PIC X(3).
      * CODE DETAIL LINE ARRAY HERE
           05 CODE-GRADE-OUT                OCCURS 4 TIMES.
              10 D-GRADE PIC 9(3).
              10 FILLER PIC X(3) VALUE SPACES.
           
           


           05  DL-STUDENT-AVERAGE          PIC Z99.
           05                              PIC X(9)    VALUE SPACE.
           05  DL-GRADE                    PIC X.
      *
       01  STUDENT-TOTAL-LINE.
           05                              PIC X(25)   VALUE
                            'TOTAL MUMBER OF STUDENTS:'.
           05                              PIC X(3)    VALUE SPACE.
           05  ST-TOTAL                    PIC ZZ9.


       PROCEDURE DIVISION.

       100-PRINT-STUDENT-REPORT.
           PERFORM 200-HSKPING-ROUTINE
           PERFORM 400-READ-STUDENT-FILE
           PERFORM 800-FINAL-ROUTINE
        .
      *
       200-HSKPING-ROUTINE.
           OPEN INPUT  STUDENT-FILE
                OUTPUT STUDENT-REPORT-FILE

           ACCEPT WS-CURRENT-DATE FROM DATE

           MOVE WS-MONTH TO H1-MONTH
           MOVE WS-DAY TO H1-DAY
           MOVE WS-YEAR TO H1-YEAR

           PERFORM 300-REPORT-HEADER
       .
      *
       300-REPORT-HEADER.

           ADD 1 TO H1-PAGE-NO

           WRITE REPORT-LINE FROM HEADING-1
               AFTER ADVANCING PAGE
           MOVE 2 TO PROPER-SPACING

           PERFORM 500-PRINT-COLUMN-HEADER
       .
      *
       400-READ-STUDENT-FILE.

           PERFORM UNTIL NO-MORE-DATA
               READ STUDENT-FILE
                   AT END
                       MOVE 'N' TO EOF-FLAG
                   NOT AT END
                       PERFORM 600-PROCESS-STUDENT-RECORD
               END-READ
           END-PERFORM

       .
      *
       500-PRINT-COLUMN-HEADER.

           WRITE REPORT-LINE FROM HEADING-2
               AFTER ADVANCING 2 LINES
       .
      *
       600-PROCESS-STUDENT-RECORD.

           MOVE SR-NAME TO DL-NAME

      *  CODE PERFORM VARYING TO ACCESS THE ARRAY HERE
          MOVE 1 TO SUB


           
           PERFORM VARYING SUB
           FROM 1 BY 1 UNTIL SUB > 4
           ADD CODE-GRADE-IN(SUB) TO DF-TEST-TOTAL
           MOVE CODE-GRADE-IN(SUB) TO CODE-GRADE-OUT(SUB)
           
           
           END-PERFORM
           
           



      * GET THE AVERAGE HERE

           COMPUTE DF-AVG = DF-TEST-TOTAL / 4


           MOVE DF-AVG TO DL-STUDENT-AVERAGE 

           ADD 1 TO CF-STUDENT-COUNT 
                              
      * GET THE LETTER GRADE HERE - USE AN EVALUATE TRUE
 
          EVALUATE TRUE
                 WHEN DF-AVG >=90
                     MOVE 'A' TO DL-GRADE

                 WHEN DF-AVG >=80 AND DF-AVG <=89
                     MOVE 'B' TO DL-GRADE

                 WHEN DF-AVG >=70 AND DF-AVG <=79
                     MOVE 'C' TO DL-GRADE

                 WHEN DF-AVG >=60 AND DF-AVG <=69
                     MOVE 'D' TO DL-GRADE

                 WHEN DF-AVG <60
                     MOVE 'F' TO DL-GRADE
                 
         END-EVALUATE





           

           MOVE DETAIL-LINE TO REPORT-LINE
           PERFORM 700-WRITE-A-LINE
           MOVE 1 TO PROPER-SPACING

           MOVE ZEROS TO DF-AVG
           MOVE ZEROS TO DF-TEST-TOTAL

           .
      *
       700-WRITE-A-LINE.
           WRITE REPORT-LINE
               AFTER ADVANCING PROPER-SPACING
           .
      *
       800-FINAL-ROUTINE.

         MOVE CF-STUDENT-COUNT TO ST-TOTAL
         MOVE STUDENT-TOTAL-LINE TO REPORT-LINE
         MOVE 2 TO PROPER-SPACING
         PERFORM 700-WRITE-A-LINE

           CLOSE STUDENT-FILE
                 STUDENT-REPORT-FILE

            STOP RUN
            .

