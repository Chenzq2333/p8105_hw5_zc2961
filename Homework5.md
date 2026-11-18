Homework 5
================
Zhiquan Chen

## Problem1

I simulated birthdays as independent draws from 365 equally likely days.
The function below returns `TRUE` if at least two people share a
birthday and `FALSE` otherwise.

``` r
simulate_birthday = function(group_size) {
  birthdays = sample(1:365, size = group_size, replace = TRUE)
  any(duplicated(birthdays))
}
```

``` r
set.seed(1)

birthday_results =
  tibble(group_size = 2:50) |>
  mutate(
    shared_birthday = map(
      group_size,
      \(n) map_lgl(
        seq_len(10000),
        \(iteration) simulate_birthday(group_size = n)
      )
    ),
    probability = map_dbl(shared_birthday, mean)
  )

stopifnot(
  nrow(birthday_results) == 49,
  all(lengths(birthday_results$shared_birthday) == 10000),
  all(birthday_results$probability >= 0),
  all(birthday_results$probability <= 1)
)
```

``` r
birthday_results |>
  ggplot(aes(x = group_size, y = probability)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey60") +
  geom_line(linewidth = 0.8, color = "#0072B2") +
  geom_point(size = 1.8, color = "#0072B2") +
  scale_x_continuous(breaks = c(2, 10, 20, 23, 30, 40, 50)) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    labels = scales::label_percent()
  ) +
  labs(
    title = "Shared birthdays become more likely as the group grows",
    subtitle = "10,000 simulated groups at each group size",
    x = "Number of people in the group",
    y = "Probability of at least one shared birthday"
  )
```

<img src="Homework5_files/figure-gfm/p1_plot-1.png" alt="" width="90%" />

For each group size, I averaged the results of 10,000 simulations. The
estimated probability of a shared birthday is 50.2% for 23 people and
97.2% for 50 people. The probability rises quickly because the number of
pairs of people increases with group size. A group of 23 people already
contains 253 possible pairs, so a shared birthday is fairly likely even
though there are 365 possible birthdays.
