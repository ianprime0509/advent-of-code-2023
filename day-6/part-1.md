# Part 1

Given the total time of the race is $t_r$ and the time used to hold down the
button is $t$, the total distance travelled is $t(t_r - t)$. This is a downward
facing parabola (second derivative is negative), so we can just set this
distance equal to the desired minimum distance, find the two solutions $t_0$ and
$t_1$ for $t$, and the number of ways to win the race is just the number of
integers lying between $t_0$ and $t_1$, inclusive, or
$floor(t_1) - ceil(t_0) + 1$.

Given the distance $d$, this expands to $t^2 - t_r t + d = 0$, so using the
quadratic formula gives an equation for the two solutions:

$$
t = \frac{t_r \pm \sqrt{t_r^2 - 4d}}{2}
$$

## Solution

Input:

```
Time:        59     68     82     74
Distance:   543   1020   1664   1022
```

First race: $t_0 \approx 11.4$, $t_1 \approx 47.6$, so the total number of
possibilities is $47 - 12 + 1 = 36$.

Second race: $t_0 \approx 22.3$, $t_1 \approx 45.7$, $45 - 23 + 1 = 23$.

Third race: $t_0 \approx 36.8$, $t_1 \approx 45.1$, $45 - 37 + 1 = 9$.

Fourth race: $t_0 \approx 18.4$, $t_1 \approx 55.6$, $55 - 19 + 1 = 37$.

Product: $36 * 23 * 9 * 37 = 275724$
