library(tidyverse)
library(tsibble)

fedhead <-
  tsibble(date = seq(
    from = date("1950-01-01"),
    to = date("2024-01-01"),
    by = 1
  ))

fedhead_martin <-
  fedhead |>
  filter_index("April 2, 1951" ~ "January 31, 1970") |>
  mutate(fed_head = "Martin") |> 
  as_tibble()

fedhead_burns <-
  fedhead |>
  filter_index("February 1, 1970" ~ "January 31, 1978") |>
  mutate(fed_head = "Burns") |> 
  as_tibble()

fedhead_miller <-
  fedhead |>
  filter_index("March 8, 1978" ~ "August 5, 1979") |>
  mutate(fed_head = "Miller") |>
  as_tibble()

fedhead_volcker <-
  fedhead |>
  filter_index("August 6, 1979" ~ "August 11, 1987") |>
  mutate(fed_head = "Volcker") |>
  as_tibble()

fedhead_greenspan <-
  fedhead |>
  filter_index("August 12, 1987" ~ "January 31, 2006") |>
  mutate(fed_head = "Greenspan") |> 
  as_tibble()

fedhead_bernanke <-
  fedhead |>
  filter_index("February 1, 2006" ~ "January 31, 2014") |>
  mutate(fed_head = "Bernanke") |>
  as_tibble()


fedhead_yellen <-
  fedhead |>
  filter_index("February 3, 2014" ~ "February 3, 2018") |>
  mutate(fed_head = "Yellen") |>
  as_tibble()

fedhead_powell <-
  fedhead |>
  filter_index("February 5, 2018" ~ .) |>
  mutate(fed_head = "Powell") |>
  as_tibble()

fedhead_bind <-
  bind_rows(
    fedhead_martin,
    fedhead_burns,
    fedhead_miller,
    fedhead_volcker,
    fedhead_greenspan,
    fedhead_bernanke,
    fedhead_yellen,
    fedhead_powell
  ) |> as_tsibble()



fedhead_daily <-
  fedhead_bind |> fill_gaps() |>
  fill(fed_head, .direction = "down") |>
  mutate(
    fed_head_1 =
      case_when(
        fed_head %in% c("Martin", "Burns", "Miller") ~ "Pre-Volchker",
        fed_head %in% c("Yellen", "Powell") ~ "Post-Bernanke",
        .default = fed_head
      )
  )

fedhead_start_end <-
  fedhead_daily |>
  as_tibble() |>
  group_by(fed_head_1) |>
  summarize(start = min(date), end = max(date))|> 
  arrange(start)

vistime::gg_vistime(fedhead_start_end, col.event="fed_head_1", optimize_y = T)



fedhead_monthly <-
  fedhead_daily |> index_by(year_month = ~ yearmonth(.)) |>
  summarise(fed_head = factor(fed_head[1]))


fedhead_quarterly <-
  fedhead_daily |> index_by(year_quarter = ~ yearquarter(.))|>
  summarise(fed_head = factor(fed_head_1[1]))


fedhead_yearly <-
  fedhead_daily |> index_by(year = ~ year(.)) |>
  summarise(fed_head = fed_head[1])


write_csv(fedhead_quarterly, file = "data/Intermediate_Data/fedhead_quarterly.csv")

