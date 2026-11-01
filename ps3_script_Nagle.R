# EC 422 Problem Set 3 - Lauren Nagle
library(tidyverse)
library(fpp3)

#2.2a
retail_raw <- read_csv("data/retail_sales_raw.csv")
head(retail_raw)

#2.2b
retail_raw <- read_csv(
  "data/retail_sales_raw.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS = col_double(),
    RSAFSNA = col_double()
  )
)

#2.3a
retail_raw_long <- retail_raw |>
  select(observation_date, RSAFS, RSAFSNA) |>
  pivot_longer(cols = c(RSAFS, RSAFSNA), names_to = "series_id", values_to = "sales")

head(retail_raw_long)

#2.4a
retail_ts <- retail_raw_long |> 
  mutate(Month = yearmonth(observation_date)) |>
  as_tsibble(index = Month, key = series_id)

#2.4b
head(retail_ts)
has_gaps(retail_ts)

#2.5a
key_vars(canadian_gas)
index_var(canadian_gas)

#2.5b
can_gas <- canadian_gas |>
  select(Month, Volume)

has_gaps(can_gas)
head(can_gas)

#2.6a
can_gas_recent <- can_gas |>
  filter(Month >= yearmonth("1990 Jan"))

head(can_gas_recent)

#2.6b
can_gas_recent <- can_gas_recent |>
  mutate(year = year(Month),
         month = month(Month, label = TRUE),
         quarter = quarter(Month))

# 2.6b ANSWER: December has the highest average production (17.1)
# and June has the lowest (14.9).

can_gas_recent |>
  as_tibble() |>
  group_by(month) |>
  summarize(avg_volume = mean(Volume))

#2.6c
can_gas_recent |>
  mutate(next_month = Month + 1,
        same_month_last_year = Month - 12) |>
  head()

#2.6d
can_gas_q <- can_gas |>
  filter(Month <= yearmonth("2004 Dec")) |>
  index_by(Quarter = yearquarter(Month)) |>
  summarize(Volume = sum(Volume))

head(can_gas_q)

can_gas_q |> autoplot(Volume)

# 2.6d ANSWER: We use sum() because Volume measures gas produced during each month,
# so the total produced in a quarter is the sum of its three months. The mean
# would give the production of an average month instead of the whole quarter.
# The quarterly series is still seasonal. Q1 and Q4 are high and Q3 is low every year.

# 2.7a
retail_ts |> autoplot(sales)

# 2.7b
retail_ts |>
  autoplot(sales) +
  labs(title = "U.S. Retail and Food Services Sales",
       subtitle = "Seasonally adjusted vs. not seasonally adjusted",
       x = NULL, y = "Millions of dollars", color = NULL) +
  theme_minimal()

#2.7b ANSWER: The NSA series spikes every December from holiday spending and drops
# in January when spending returns to normal. The SA series does not have this
# pattern because seasonal adjustment removes it. Over the long run, both series
# follow the same path, with the SA line running through the middle of the NSA
# swings. Both series show the drops in 2008-09 and in 2020, because seasonal
# adjustment only removes patterns that repeat every year, not one-time events
# like recessions.

# 2.7c
can_gas |>
  autoplot(Volume) +
  labs(title = "Canadian Natural Gas Production",
       x = NULL, y = "Billions of cubic meters") +
  theme_minimal()

# 2.7d
can_gas |>
  autoplot(Volume) +
  geom_smooth(se = FALSE) +
  labs(title = "Canadian Natural Gas Production with Smoothed Trend",
       x = NULL, y = "Billions of cubic meters") +
  theme_minimal()

# 2.7d ANSWER: I added geom_smooth() to show the long-run trend underneath the
# seasonal swings.

# 3.1a ANSWER: The seasonal swings in can_gas are fairly small in the 60s and grow much
# larger from the mid-1970s to around 1990 and then shrink in the 1990s-2000s
# even though production keeps rising. So the swings do not consistently grow
# with the level of the series.

# 3.1b
can_gas <- can_gas |>
  mutate(log_volume = log(Volume))

can_gas |>
  autoplot(log_volume) +
  labs(title = "Log of Canadian Gas Production", x = NULL, y = "log(Volume)") +
  theme_minimal()

#3.1b ANSWER: After taking logs, the trend appears to be more curved. It rises sharply 
# in the 60s and flattens later. This means the growth rate of production slowed over time.
# The seasonal swings are now largest in the 1960s and smallest in the 2000s, so logs don't 
# make the swings a constant size. Logs are really only a good idea for can_gas since they
# let us interpret changes as percentages, but they don't give a straight trend or stable 
# seasonal swings.

# 3.1c
retail_nsa <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  mutate(log_sales = log(sales))

retail_nsa |> autoplot(sales) +
  labs(title = "NSA Retail Sales (Level)", x = NULL) + theme_minimal()
retail_nsa |> autoplot(log_sales) +
  labs(title = "NSA Retail Sales (Log)", x = NULL) + theme_minimal()

#3.1c ANSWER: Logging retail sales makes more sense here than for logging can_gas. In
# levels, the December spikes get larger as sales grow. However, in logs the spikes are roughly
# the same size throughout, meaning the seasonal swings are proportional to the level 
# of the series.

# 3.2a
can_gas <- can_gas |>
  mutate(
    growth_direct  = 100 * difference(Volume) / lag(Volume),
    growth_logdiff = 100 * difference(log_volume)
  )

can_gas |> autoplot(growth_direct)

can_gas <- can_gas |>
  mutate(growth_gap = abs(growth_direct - growth_logdiff))

can_gas |> autoplot(growth_gap)

# 3.2a ANSWER: The log-difference approximation is fairly good in most months, with a
# gap under about 0.5 percentage points, but it reaches about 3.5 points in
# months with very large growth rates (near 20-28%), since the approximation
# is only accurate when growth rates are small.

# 3.3
retail_ts <- retail_ts |>
  group_by_key() |>
  mutate(growth_mom = 100 * difference(log(sales))) |>
  ungroup()

retail_ts |>
  autoplot(growth_mom) +
  labs(title = "Month-over-Month Growth: SA vs. NSA Retail Sales",
       x = NULL, y = "Growth rate (%)", color = NULL) +
  theme_minimal()

# 3.3 ANSWER: The NSA growth rate is much noisier than the SA growth rate. NSA
# growth jumps roughly +15-20% every December and falls roughly 25-30% every
# January because of the holiday spending pattern, while SA growth
# stays near 0-1% because that seasonal pattern has been removed. The large SA
# movements happen around 2020, since seasonal adjustment removes the repeating
# yearly pattern but not one-time shocks like COVID.

# 4.1a
retail_ts |>
  filter(series_id == "RSAFSNA") |>
  gg_season(sales) +
  labs(title = "Seasonal Pattern in NSA Retail Sales", y = "Millions of dollars") +
  theme_minimal()

# 4.1b
retail_ts |>
  filter(series_id == "RSAFS") |>
  gg_season(sales) +
  labs(title = "Seasonal Pattern in SA Retail Sales", y = "Millions of dollars") +
  theme_minimal()

# 4.1b ANSWER: In the NSA season plot, every year has roughly the same shape of low in
# January to February, a step up in March, a dip in September, and a large peak in
# December. In the SA season plot, the lines are nearly flat across the year with
# no repeating shape, because the seasonal pattern has been removed. The April
# 2020 COVID drop appears in both, since it is a one-time shock, not seasonal.

# 4.1c
can_gas |>
  gg_season(log_volume) +
  labs(title = "Seasonal Pattern in Canadian Gas Production", y = "log(Volume)") +
  theme_minimal()

# 4.1c ANSWER: Gas production is highest in the winter months (December-January)
# and lowest in the summer (June-July). This matches intuition, since natural gas
# is largely used for heating, so demand peaks in Canada's cold months. The
# seasonal pattern is also much weaker in recent years than in the 1960s.

# 4.2
can_gas |>
  gg_subseries(log_volume) +
  labs(title = "Subseries Plot: Canadian Gas Production", y = "log(Volume)")

# 4.3a
retail_nsa_x13 <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  model(x13 = X_13ARIMA_SEATS(sales)) |>
  components()

retail_nsa_x13 |> autoplot()

# 4.3a ANSWER: The seasonal factor is highest in December (about 1.15), so December
# sales are typically about 15% above trend. The seasonal swings shrink over time:
# the gap between the highest and lowest seasonal factor falls from about 0.31 in
# 1992 to about 0.19 in 2024.

# 4.3b
sa_compare <- retail_nsa_x13 |>
  as_tibble() |>
  select(Month, mine = season_adjust) |>
  left_join(
    retail_ts |>
      filter(series_id == "RSAFS") |>
      as_tibble() |>
      transmute(Month, census = sales),
    by = "Month"
  ) |>
  mutate(pct_diff = 100 * (log(mine) - log(census)))

sa_compare |>
  ggplot(aes(x = Month, y = pct_diff)) +
  geom_line() +
  labs(title = "My X-13 Seasonal Adjustment Minus the Census Bureau's",
       x = NULL, y = "Difference (%)") +
  theme_minimal()

# 4.3b ANSWER: My X-13 adjustment matches the Census Bureau's very closely. The
# median difference is about 0% and half of all months are within +/-0.15%. The
# largest differences (up to about 1.8%) occur in January of 2021-2024 and December
# 2019, around the COVID period, where the Census Bureau's tailored settings for
# outliers and holidays, and its adjustment of categories separately, matter most.

# 4.4a
can_gas_fit <- can_gas |>
  model(TSLM(log_volume ~ trend() + season()))

report(can_gas_fit)

# 4.4a ANSWER: The trend coefficient is 0.004, so gas production grew by about
# 0.4% per month on average. June has the most significant seasonal effect
# (t = -4.78): June production is about 0.25 log points (roughly 25%) below
# January, relative to trend, consistent with the summer low in gas production.

# 4.4b
can_gas_fit |>
  augment() |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = log_volume, color = "Actual")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  labs(title = "Linear Trend + Seasonal Dummies Fit: Canadian Gas Production",
       y = "log(Volume)", color = NULL) +
  theme_minimal()

# 4.4b ANSWER: The fitted line tracks the actual series well from about 1980 to
# 1995, but it overshoots in the early 1960s, undershoots through the 1970s, and
# overshoots again after 2000 when production levels off, because a straight-line
# trend cannot follow the curved trend in log production.

# 4.4c
can_gas_fit |> gg_tsresiduals()

can_gas_fit |>
  augment() |>
  features(.innov, ljung_box, lag = 24)

# 4.4c ANSWER: The residuals do not look like white noise. They follow a slow
# wave over time instead of bouncing randomly around zero, and the ACF bars start
# near 1 and decline slowly, staying far outside the blue bands. The Ljung-Box
# test has a p-value of 0, so at the 5% level we reject the null that the
# residuals are white noise. The main leftover structure comes from the
# straight-line trend failing to capture the curved trend in log production.

# Helpers for 4.3a and 4.4a
retail_nsa_x13 |>
  as_tibble() |>
  mutate(m = month(Month, label = TRUE)) |>
  group_by(m) |>
  summarize(avg_seasonal = mean(seasonal)) |>
  arrange(desc(avg_seasonal))

can_gas_fit |>
  tidy() |>
  select(term, estimate, std.error, statistic, p.value) |>
  knitr::kable(digits = 3)

# Helper for 4.3a: size of the seasonal swing (max - min factor) in selected years
retail_nsa_x13 |>
  as_tibble() |>
  mutate(yr = year(Month)) |>
  group_by(yr) |>
  summarize(swing = max(seasonal) - min(seasonal)) |>
  filter(yr %in% c(1992, 2000, 2008, 2016, 2024))

# Helpers for 4.3b: how close my X-13 is to the Census Bureau's
summary(sa_compare$pct_diff)

sa_compare |>
  arrange(desc(abs(pct_diff))) |>
  head(5)


