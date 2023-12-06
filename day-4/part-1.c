#include <ctype.h>
#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int card_value(char *line);

int main(void) {
  int value_sum = 0;
  char *line = NULL;
  size_t line_len = 0;
  ssize_t line_read;
  FILE *input;

  if (!(input = fopen("input.txt", "r"))) {
    perror("open");
    return 1;
  }

  while ((line_read = getline(&line, &line_len, input)) != -1) {
    value_sum += card_value(line);
  }
  if (errno != 0) {
    perror("read");
    free(line);
    return 1;
  }

  free(line);
  printf("%d\n", value_sum);
  return 0;
}

static int card_value(char *line) {
  bool winning[100] = {};
  int value = 0, num;

  line = strchr(line, ':') + 1; /* Card 1:^ */

  /* Read winning numbers */
  for (; *line != '|'; line++) {
    if (isdigit(*line)) {
      num = 0;
      while (isdigit(*line)) {
        num = 10 * num + (*line++ - '0');
      }
      winning[num] = true;
    }
  }

  /* Read card numbers */
  for (; *line; line++) {
    if (isdigit(*line)) {
      num = 0;
      while (isdigit(*line)) {
        num = 10 * num + (*line++ - '0');
      }
      if (winning[num]) {
        value = value == 0 ? 1 : value * 2;
      }
    }
  }

  return value;
}
