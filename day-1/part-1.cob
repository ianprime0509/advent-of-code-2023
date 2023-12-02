       *> Day 1, part 1
        IDENTIFICATION DIVISION.
        PROGRAM-ID. AOC-Day-1-Part-1.

        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        SELECT input-file
            ASSIGN TO "input.txt"
            ORGANIZATION LINE SEQUENTIAL.

        DATA DIVISION.
        FILE SECTION.
        FD input-file.
        01 input-line PIC X(100).

        WORKING-STORAGE SECTION.
        01 end-of-file     PIC X     VALUE "N".
        01 calibration-sum PIC 9(10).
        01 line-pos        PIC 9(10).
        01 line-value.
            05 first-digit PIC 9.
            05 last-digit  PIC 9.

        PROCEDURE DIVISION.
        main-paragraph.
            OPEN INPUT input-file
            PERFORM UNTIL end-of-file = "Y"
                READ input-file INTO input-line
                    AT END
                        MOVE "Y" TO end-of-file
                    NOT AT END
                        PERFORM handle-line
                END-READ
            END-PERFORM
            CLOSE input-file
            DISPLAY calibration-sum
            GOBACK
            .

        handle-line.
            MOVE 0 to first-digit
            MOVE 0 to last-digit
            PERFORM VARYING line-pos
                            FROM 1 BY 1
                            UNTIL line-pos > LENGTH OF input-line
                IF input-line(line-pos:1) IS NUMERIC
                    IF first-digit = 0
                        MOVE input-line(line-pos:1) TO first-digit
                    ELSE
                        MOVE input-line(line-pos:1) TO last-digit
                    END-IF
                END-IF
            END-PERFORM
            IF last-digit = 0
                MOVE first-digit TO last-digit
            END-IF
            ADD FUNCTION NUMVAL(line-value) TO calibration-sum
            .
