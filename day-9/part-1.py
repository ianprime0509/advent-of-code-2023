#!/usr/bin/env python3

with open("input.txt") as input_file:

    def parse_line(line):
        return [int(n) for n in line.strip().split(" ")]

    input_data = [parse_line(line) for line in input_file]


def predict(seq):
    def diffs(ns):
        return [ns[i + 1] - ns[i] for i in range(0, len(ns) - 1)]

    def done(diff):
        for n in diff:
            if n != 0:
                return False
        return True

    inc_diffs = [[*seq]]
    while True:
        next_diff = diffs(inc_diffs[-1])
        if done(next_diff):
            break
        inc_diffs.append(next_diff)

    inc_diffs.reverse()
    for i, diff in enumerate(inc_diffs[1:]):
        diff.append(diff[-1] + inc_diffs[i][-1])

    return inc_diffs[-1][-1]


sum_predictions = 0
for seq in input_data:
    sum_predictions += predict(seq)
print(sum_predictions)
