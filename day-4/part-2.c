#include <ctype.h>
#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int card_matches(char *line);

int main(void) {
  int n_cards = 0, n_copies[999] = {}, curr_card = 0, curr_matches, i;
  char *line = NULL;
  size_t line_len = 0;
  ssize_t line_read;
  FILE *input;

  if (!(input = fopen("input.txt", "r"))) {
    perror("open");
    return 1;
  }

  while ((line_read = getline(&line, &line_len, input)) != -1) {
    /* Original copy */
    n_cards++;
    n_copies[curr_card] += 1;

    /* Calculate new copies of subsequent cards */
    curr_matches = card_matches(line);
    for (i = 0; i < curr_matches; i++) {
      n_copies[curr_card + i + 1] += n_copies[curr_card];
      n_cards += n_copies[curr_card];
    }
    curr_card++;
  }
  if (errno != 0) {
    perror("read");
    free(line);
    return 1;
  }

  free(line);
  printf("%d\n", n_cards);
  return 0;
}

static int card_matches(char *line) {
  bool winning[100] = {};
  int matches = 0, num;

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
        matches++;
      }
    }
  }

  return matches;
}
