## Checking HANK
## Plots creation
## Author: Alexander Vlasov
##
##

# Packages -----
library(tidyverse)
library(scales)
library(tsibble)

# Datasets -----
full_dataset_tbl <- read_csv("data/full_dataset.csv")
full_dataset_ts <-
  full_dataset_tbl |> mutate(year_quarter = yearquarter(year_quarter)) |> tsibble()
size_persistence_consumption_shorter_tbl <-
  read.csv("data/size_persistence_consumption_shorter.csv") |> 
  tibble() |> 
  mutate(year_quarter = yearquarter(year_quarter))
size_persistence_consumption_shorter_ts <-
  size_persistence_consumption_shorter_tbl |> tsibble()

size_persistence_consumption_longer_tbl <-
  read.csv("data/size_persistence_consumption_longer.csv") |> 
  tibble() |> 
  mutate(year_quarter = yearquarter(year_quarter))

irfs_longer <- read.csv("data/irfs_longer.csv") |> tibble()|> select(-X) |> 
  mutate(time = yearquarter(time))
irfs_shorter <- read.csv("data/irfs_shorter.csv") |> tibble()|> select(-X) |> 
  mutate(time = yearquarter(time))

size_persistence_consumption_ts <-
  size_persistence_consumption_tbl  |> tsibble()


# Recessions -----


getSymbols('USREC',  src = 'FRED')
recession <- USREC
drecession <- diff(recession)
recession.start <- time(drecession[drecession == 1])
recession.end <- time(drecession[drecession == -1])
recession.end <- recession.end
recession.df <- tibble(recession.start , recession.end[2:length(recession.end)])
colnames(recession.df) <- c("start", "end")
recession.df

rec_data_1 <-
  recession.df |> filter(end >= head(yq(
    size_persistence_consumption_longer_tbl$year_quarter
  ), 1),
  start <= tail(yq(
    size_persistence_consumption_longer_tbl$year_quarter
  ), 1))


rec_data_2 <-
  recession.df |> filter(end >=yq("1986 Q1") ,
  start <= yq("2018 Q4"))


# Expected Inflation Plot -----
expected_deflator_inflation_plot <-
  full_dataset_ts |> 
  autoplot(expected_inflation) +
  theme_light() +
  labs(x = "", y = "Expected Inflation (Deflator)") +
  geom_rect(
    data = rec_data_1,
    inherit.aes = F,
    aes(
      xmin = start,
      xmax = end,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = '#155F83FF' ,
    alpha = 0.2
  )

expected_unemployment_plot <-
  full_dataset_ts |> 
  select(expected_unemployment) |> 
  na.omit() |> 
  autoplot() +
  theme_light() +
  labs(x = "", y = "Expected Unemployment") +
  geom_rect(
    data = rec_data_1,
    inherit.aes = F,
    aes(
      xmin = start,
      xmax = end,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = '#155F83FF' ,
    alpha = 0.2
  )



# Expected CPI Inflation Plot -----
expected_cpi_inflation_plot <-
  full_dataset_ts|> filter(!is.na(expected_CPI_inflation)) |>
  autoplot(expected_CPI_inflation) +
  theme_light() +
  labs(x = "", y = "Expected Inflation (CPI)") +
  geom_rect(
    data = rec_data_2,
    inherit.aes = F,
    aes(
      xmin = start,
      xmax = end,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = '#155F83FF' ,
    alpha = 0.2
  )

expected_gap_plot <-
  full_dataset_ts |> 
  select(expected_gap) |> 
  na.omit() |> 
  autoplot() +
  theme_light() +
  labs(x = "", y = "Expected Output Gap") +
  geom_rect(
    data = rec_data,
    inherit.aes = F,
    aes(
      xmin = start,
      xmax = end,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = '#155F83FF' ,
    alpha = 0.2
  )



# IRF Plot -----

irfs_longer_plot <-
  ggplot(irfs_longer, aes(
    x = quart,
    y = irf,
    color = date(time),
    group = date(time)
  )) +
  geom_hline(aes(yintercept = 0),  color = "darkred") +
  geom_line() +
  theme_light() + 
  labs(y = "Percentage Points", x = "Quarter", color = "Year-Quarter") +  
  scale_x_continuous(breaks = pretty_breaks())
irfs_longer_plot

irfs_shorter_plot <-
  ggplot(irfs_shorter, aes(
    x = quart,
    y = irf,
    color = date(time),
    group = date(time)
  )) +
  geom_hline(aes(yintercept = 0),  color = "darkred") +
  geom_line() +
  theme_light() + 
  labs(y = "Percentage Points", x = "Quarter", color = "Year-Quarter") +  
  scale_x_continuous(breaks = pretty_breaks())
irfs_shorter_plot


irf_wide_1 <- irfs |> pivot_wider(names_from = time, values_from = irf)
irf_wide_2 <- irfs |> pivot_wider(names_from = quart, values_from = irf)|> group_by(time)

irf_wide_2_ts <- irf_wide_2 |> as_tsibble()

irf_pre_volcker <-
  irf_wide_2_ts |>
  filter_index(. ~ "August 6, 1979") |>
  as_tibble() |>
  select(-time) |>
  summarise_all(mean)

irf_volcker <-  irf_wide_2_ts |>
  filter_index("August 6, 1979" ~ "August 11, 1987") |>
  as_tibble() |>
  select(-time) |>
  summarise_all(mean)

irf_greenspan <-  irf_wide_2_ts |>
  filter_index("August 11, 1987" ~ "January 31, 2006") |>
  as_tibble() |>
  select(-time) |>
  summarise_all(mean)

irf_bernanke <-  irf_wide_2_ts |>
  filter_index("February 1, 2006" ~ "January 31, 2014") |>
  as_tibble() |>
  select(-time) |>
  summarise_all(mean)

irf_post_bernanke <-  irf_wide_2_ts |>
  filter_index("January 31, 2014" ~ .) |>
  as_tibble() |>
  select(-time) |>
  summarise_all(mean)


irf_by_chair <-
  tibble(
    pre_volcker = irf_pre_volcker |> t() |> c(),
    volcker = irf_volcker |> t() |> c(),
    greenspan = irf_greenspan |> t() |> c(),
    bernanke = irf_bernanke |> t() |> c(),
    post_bernanke = irf_post_bernanke |> t() |> c()
  ) |> 
  mutate(quarter=row_number())



irf_by_chair_long <- irf_by_chair |> pivot_longer(cols=-quarter)

ggplot(irf_by_chair_long, aes(x = quarter, y = value,  color = name)) +
  geom_line() +
  theme(legend.position = "bottom")

#heatmap(t(as.matrix(irf_wide_1))[-1,], scale ="row", Rowv = NA, Colv = NA)


irfs_plot_3d <- plot_ly(z = t(as.matrix(irf_wide))[-1,], type = "surface")
#irfs_plot_3d


size_vs_persistence <- 
  ggplot(size_persistence_consumption_shorter_tbl,
       aes(x = size, y = persistence, color = year(yq(year_quarter)))) +
  geom_point(size = 1.3) +
  labs(x = "Size", y = "Persistence", color = "Year-Quarter") +
  theme_light()

size_vs_persistence


mean(size_persistence_consumption_tbl$size <= 0)



size_plot <-
  size_persistence_consumption_shorter_ts |> autoplot(size) +
  theme_light() +
  labs(x = "", y = "Size") +
  geom_rect(
    data = rec_data_1,
    inherit.aes = F,
    aes(
      xmin = start,
      xmax = end,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = '#155F83FF' ,
    alpha = 0.2
  )

size_plot





persistence_plot <-
  size_persistence_consumption_shorter_ts |> autoplot(persistence) +
  theme_light() +
  labs(x = "", y = "Persistence") +
  geom_rect(
    data = rec_data_1,
    inherit.aes = F,
    aes(
      xmin = start,
      xmax = end,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = '#155F83FF' ,
    alpha = 0.2
  )


persistence_plot






HAWK_plot <-
  ggplot(full_dataset_ts, aes(x = yq(year_quarter))) +
  geom_line(aes(y = HAWK, color = "HAWK")) +
  geom_line(aes(y = HAWK_IV, color = "HAWK IV")) +
  theme_light() +
  labs(x = "", y = "HAWK", color = "") +
  geom_rect(
    data = rec_data_1,
    inherit.aes = F,
    aes(
      xmin = start,
      xmax = end,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = '#155F83FF' ,
    alpha = 0.2
  ) +
  scale_y_continuous(breaks = breaks_extended()) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_date(breaks = breaks_pretty(n = 6)) 





persistence_plot <-
  size_persistence_consumption_ts |> autoplot(persistence) +
  theme_light() +
  labs(x = "", y = "Persistence") +
  geom_rect(
    data = rec_data_1,
    inherit.aes = F,
    aes(
      xmin = start,
      xmax = end,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = '#155F83FF' ,
    alpha = 0.2
  )




ggsave(
  "irfs_plot_shorter.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  irfs_shorter_plot,
  width = 220,
  height = 140,
  units = "mm"
)
ggsave(
  "irfs_plot_longer.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  irfs_longer_plot,
  width = 220,
  height = 140,
  units = "mm"
)


ggsave(
  "size_vs_persistence.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  size_vs_persistence,
  width = 220/1.2,
  height = 140/1.2,
  units = "mm"
)

ggsave(
  "size_plot.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  size_plot,
  width = 220/1.7,
  height = 140/1.7,
  units = "mm"
)


ggsave(
  "persistence_plot.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  persistence_plot,
  width = 220/1.7,
  height = 140/1.7,
  units = "mm"
)


ggsave(
  "HAWK_plot.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  HAWK_plot,
  width = 220/1.7,
  height = 140/1.7,
  units = "mm"
)




