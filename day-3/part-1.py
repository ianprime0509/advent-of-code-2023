#!/usr/bin/env python3

import re

number = re.compile(r"\d+")

with open("input.txt") as input_file:
    schematic = [line.strip() for line in input_file]


def is_symbol(c):
    return c != "." and not ("0" <= c <= "9")


def is_part_number(row, start_col, end_col):
    for i in range(row - 1, row + 2):
        for j in range(start_col - 1, end_col + 1):
            if (
                0 <= i < len(schematic)
                and 0 <= j < len(schematic[i])
                and is_symbol(schematic[i][j])
            ):
                return True
    return False


part_sum = 0

for i, line in enumerate(schematic):
    for m in number.finditer(line):
        if is_part_number(i, m.start(), m.end()):
            part_sum += int(m.group(0))

print(part_sum)
