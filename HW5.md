HW5
================
Adeena Moghni
2024-11-10

## Problem 1

``` r
birthday_sim = function(n){
  birthday_date <- sample(1:365, n, replace = TRUE) 
  length(birthday_date) != length(unique(birthday_date))
}
```

``` r
sim_results_df = 
  expand_grid(
    sample_size = 2:50,
    iter = 1:10000
  ) %>% 
  mutate(
    birthday_sim_df = map(sample_size, birthday_sim)
  ) %>% 
  unnest(birthday_sim_df) %>% 
  group_by(sample_size) %>% 
  
  summarize(
    total = sum(birthday_sim_df),
    probability = total/10000)

ggplot(sim_results_df, aes(x = sample_size, y = probability)) +
  geom_point() + geom_line() +
  labs(x = "Sample Size", 
       y = "P(at least 2 people sharing the same birthday)",
       title = "Probability of 2 People Sharing a Birthday vs Sample Size",
       caption = "Based off simulation of 10,000 iterations per sample size"
       )
```

![](HW5_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

As expected, the probability of 2 people sharing a birthday increases as
sample size increases. Surprisingly, though, it only takes about 50
people for the probability of 2 people sharing a birthday in that group
becoming 100%.

## Problem 2

``` r
power_sim = function(true_mu){
    x = rnorm(n = 30, mean = true_mu, sd = 5)
    t_test_result= t.test(x, mu = 0, conf.level = .95)
    tibble(xbar = mean(x), 
           p_value = t_test_result$p.value)
}
```

``` r
simulation_results_df = 
  expand_grid(
    true_mu = c(0, 1, 2, 3, 4, 5, 6),
    iter = 1:5000
  ) %>% 
  group_by(true_mu) %>%
  mutate(
    power_sim_df = map(true_mu, power_sim)
  ) %>% 
  unnest(power_sim_df)
```

``` r
power_graphing_df = 
  simulation_results_df %>% 
  group_by(true_mu) %>% 
  summarize(
    total = sum(p_value < .05),
    proportion = total/5000)

ggplot(power_graphing_df, aes(x = true_mu, y = proportion)) +
  geom_point() + geom_line() +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6)) +
  labs(x = "True Average", 
       y = "Power",
       title = "Effect Size vs Power"
       )
```

![](HW5_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

As the true value of $\mu$ increases, the likelihood of the data sets
having a sample mean value of 0 decreases and moves farther and farther
away from 0. Thus, at an alpha value of .05, the probability of
rejecting the null of $\mu$ = 0 increases until it reaches 1 at $\mu$ =
5. In other words, the effect size increases as $\mu$ increases since we
set the null hypothesis as $\mu$ = 0, thereby increasing the power.

``` r
xbar_df = 
  simulation_results_df %>% 
  group_by(true_mu) %>%
  summarize(
    avg_all_xbar = mean(xbar)
  )

xbar_reject_df = 
  simulation_results_df %>%
  filter(p_value < .05) %>% 
  group_by(true_mu) %>%
  summarize(
    avg_rejected_xbar = mean(xbar)
  )

xbar_graphing_df = 
  left_join(
    xbar_df,
    xbar_reject_df,
    by = "true_mu"
  ) %>% 
  pivot_longer(
    cols = avg_all_xbar:avg_rejected_xbar, 
    names_to = "classification", 
    values_to = "average_xbar")

ggplot(xbar_graphing_df, aes(x = true_mu, y = average_xbar, color = classification)) +
  geom_point() + geom_line() +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6)) +
  labs(x = "True Average", 
       y = "Average Sample Mean",
       title = "Sample Mean vs True Mean"
       )
```

![](HW5_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

As seen from the graph, the sample mean for all samples starts off lower
than sample means of rejected samples when $\mu$ is lower. This is
because $\mu$ is closer to 0. Thus, only the samples that have sample
means that fall within the rejection region (samples with means that are
extremely high or extremely low) will be rejected. As $\mu$ increases,
the likelihood of sample means falling within this rejection region
increases, and as seen in the previous graph, the proportion of samples
that are rejected reaches 1. Because of this, the average of all sample
means and the average of rejected sample means overlap as $\mu$
increases to 4, 5, and 6.

## Problem 3

``` r
homicide_df = 
  read_csv(file = "./homicide-data.csv", na = c("NA", ".", "")) %>%
  janitor::clean_names() %>% 
  distinct() %>% 
  mutate(
    city_state = paste(city, ",", state),
    status = ifelse(disposition %in% c("Closed without arrest", "Open/No arrest"), 
                    "Unsolved", 
                    "Solved"))
```

    ## Rows: 52179 Columns: 12
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (9): uid, victim_last, victim_first, victim_race, victim_age, victim_sex...
    ## dbl (3): reported_date, lat, lon
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

The data on homicides by city has 52179 rows by 14 columns. The
variables to describe the data are: uid, reported_date, victim_last,
victim_first, victim_race, victim_age, victim_sex, city, state, lat,
lon, disposition, city_state, status

``` r
totals_df = 
  homicide_df %>% 
  group_by(city) %>% 
  summarize(
    homicides_total = n(),
    homicides_unsolved = sum(status == "Unsolved")
  )

unsolved_baltimore = 
  totals_df %>% 
  filter(city == "Baltimore")

balt_result = 
  prop.test(x = unsolved_baltimore %>% pull(homicides_unsolved), 
            n = unsolved_baltimore %>% pull(homicides_total)) %>% 
  broom::tidy() %>% 
  mutate(
    p_hat = estimate,
    lower_ci_limit = conf.low,
    upper_ci_limit = conf.high
  ) %>% 
  select(p_hat, lower_ci_limit, upper_ci_limit)
```

``` r
results_df = 
  totals_df %>% 
  mutate(
    prop_test = purrr::map2(homicides_unsolved, homicides_total, ~ prop.test(x = .x, n = .y))
  ) %>% 
  mutate(results = map(prop_test, broom::tidy)) %>% 
  unnest(results) %>% 
  mutate(
    p_hat = estimate,
    lower_ci_limit = conf.low,
    upper_ci_limit = conf.high
  ) %>% 
  select(city, p_hat, lower_ci_limit, upper_ci_limit) %>% 
  mutate(city = fct_reorder(city, p_hat))

ggplot(results_df, aes(x = city, y = p_hat)) +
  geom_point() + 
  geom_errorbar(aes(ymin = lower_ci_limit, ymax = upper_ci_limit)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "city", 
       y = "Sample Proportion (p hat)",
       title = "Proportion of Unsolved Homicides by City",
       caption = "Sample proportions with 95% confidence interval shown"
       )
```

![](HW5_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->
