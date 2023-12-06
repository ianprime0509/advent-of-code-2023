#!/usr/bin/env python3

with open("input.txt") as input_file:
    schematic = [line.strip() for line in input_file]


def is_digit(c):
    return "0" <= c <= "9"


def number_at(row, col):
    start = col
    while start > 0 and is_digit(schematic[row][start - 1]):
        start -= 1
    end = col
    while end < len(schematic[row]) and is_digit(schematic[row][end]):
        end += 1
    return int(schematic[row][start:end]), end


def find_part_numbers(row, col):
    result = []
    for i in range(max(0, row - 1), min(row + 2, len(schematic))):
        j = max(0, col - 1)
        while j < min(col + 2, len(schematic[i])):
            if is_digit(schematic[i][j]):
                part_number, j = number_at(i, j)
                result.append(part_number)
            else:
                j += 1
    return result


def find_gears(line):
    j = line.find("*")
    while j != -1:
        yield j
        j = line.find("*", j + 1)


gear_ratios = 0

for i, line in enumerate(schematic):
    for j in find_gears(line):
        part_numbers = find_part_numbers(i, j)
        if len(part_numbers) == 2:
            gear_ratios += part_numbers[0] * part_numbers[1]

print(gear_ratios)
