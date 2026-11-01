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
