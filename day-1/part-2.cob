       *> Day 1, part 2
        IDENTIFICATION DIVISION.
        PROGRAM-ID. AOC-Day-1-Part-2.

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
                EVALUATE input-line(line-pos:1)
                WHEN NUMERIC
                    IF first-digit = 0
                        MOVE input-line(line-pos:1) TO first-digit
                    ELSE
                        MOVE input-line(line-pos:1) TO last-digit
                    END-IF
                WHEN = "o"
                    EVALUATE TRUE
                    WHEN input-line(line-pos:3) = "one"
                        IF first-digit = 0
                            MOVE 1 TO first-digit
                        ELSE
                            MOVE 1 TO last-digit
                        END-IF
                    END-EVALUATE
                WHEN "t"
                    EVALUATE TRUE
                    WHEN input-line(line-pos:3) = "two"
                        IF first-digit = 0
                            MOVE 2 TO first-digit
                        ELSE
                            MOVE 2 TO last-digit
                        END-IF
                    WHEN input-line(line-pos:5) = "three"
                        IF first-digit = 0
                            MOVE 3 TO first-digit
                        ELSE
                            MOVE 3 TO last-digit
                        END-IF
                    END-EVALUATE
                WHEN = "f"
                    EVALUATE TRUE
                    WHEN input-line(line-pos:4) = "four"
                        IF first-digit = 0
                            MOVE 4 TO first-digit
                        ELSE
                            MOVE 4 TO last-digit
                        END-IF
                    WHEN input-line(line-pos:4) = "five"
                        IF first-digit = 0
                            MOVE 5 TO first-digit
                        ELSE
                            MOVE 5 TO last-digit
                        END-IF
                    END-EVALUATE
                WHEN = "s"
                    EVALUATE TRUE
                    WHEN input-line(line-pos:3) = "six"
                        IF first-digit = 0
                            MOVE 6 TO first-digit
                        ELSE
                            MOVE 6 TO last-digit
                        END-IF
                    WHEN input-line(line-pos:5) = "seven"
                        IF first-digit = 0
                            MOVE 7 TO first-digit
                        ELSE
                            MOVE 7 TO last-digit
                        END-IF
                    END-EVALUATE
                WHEN = "e"
                    EVALUATE TRUE
                    WHEN input-line(line-pos:5) = "eight"
                        IF first-digit = 0
                            MOVE 8 TO first-digit
                        ELSE
                            MOVE 8 TO last-digit
                        END-IF
                    END-EVALUATE
                WHEN = "n"
                    EVALUATE TRUE
                    WHEN input-line(line-pos:4) = "nine"
                        IF first-digit = 0
                            MOVE 9 TO first-digit
                        ELSE
                            MOVE 9 TO last-digit
                        END-IF
                    END-EVALUATE
                END-EVALUATE
            END-PERFORM
            IF last-digit = 0
                MOVE first-digit TO last-digit
            END-IF
            ADD FUNCTION NUMVAL(line-value) TO calibration-sum
            .
