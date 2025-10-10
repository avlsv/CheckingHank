## Checking HANK.
## The IRF predictions
## Author: Alexander Vlasov
##
##


# Libraries -----
required_Packages_Install <-
  c(
    "tidyverse",
    "lpirfs",
    "sandwich",
    "zoo",
    "quantmod",
    "tsibble",
    "readxl",
    "fable",
    "fabletools",
    "AER",
    "broom",
    "stargazer",
    "ivreg",
    "car"
  )


for (Package in required_Packages_Install) {
  if (!require(Package, character.only = TRUE)) {
    install.packages(Package, dependencies = TRUE)
  }
  
  library(Package, character.only = TRUE)
}


# Datasets ---- 

load("data/intermediate_data/coefs_short.RData")
full_dataset_tbl <- read_csv("data/intermediate_data/full_dataset.csv")
full_dataset_ts <-
  full_dataset_tbl |> mutate(year_quarter = yearquarter(year_quarter)) |> tsibble()

# Looking at the data -----


full_dataset_ts |> autoplot(vars(expected_cpi_inflation, expected_gap))
full_dataset_ts |> filter_index("2007"~"2011") |> autoplot(vars(expected_cpi_inflation, expected_gap))
library(patchwork)

full_dataset_ts |> filter_index("2007"~"2011") |> autoplot(vars(expected_cpi_inflation,expected_gap))+
  full_dataset_ts |> filter_index("2007"~"2011") |> autoplot(vars(expected_inflation,expected_unemployment_gap))+ plot_layout(ncol = 1)

# 2008Q4
shock_inflation <- 
  full_dataset_ts |> 
  filter_index("2008Q1"~"2009Q1") |> 
  as_tibble() |> 
  select(delta_expected_cpi_inflation)


shock_gap <- 
  full_dataset_ts |> 
  filter_index("2008Q1"~"2009Q1") |> 
  as_tibble() |>
  select(delta_expected_gap)|>
  min()
#  2.01 -0.0950 
#
## IRFs preditions ----


size_persistence_tbl <- tibble()
irf_t_list <- c()
len = 20
for (t in 1:dim(full_dataset_ts)[1]) {
  irf_t = (
    coefs_cpi_inflation$estimate[1:len] +
      full_dataset_ts$demeaned_HAWK[t] * coefs_HAWK_cpi_inflation$estimate[1:len]
  ) *  shock_inflation + (
    coefs_gap$estimate[1:len] +
      full_dataset_ts$demeaned_HAWK[t] * coefs_HAWK_gap$estimate[1:len]
  ) *  shock_gap 
  
  
  
  irf_t_list <- c(irf_t_list, irf_t)

}



irfs_shock_short <-
  tibble(
    irf = irf_t_list,
    quart = rep(1:len, dim(full_dataset_ts)[1]),
    time = rep(full_dataset_ts$year_quarter, each = len), 
    HAWK=rep(full_dataset_ts$HAWK, each=len)
  )




irfs_short_plot <-
  ggplot(predicted_short_tbl|>filter(yearquarter("2008Q1") <= quarter & 
                                       quarter <= yearquarter("2009Q3")),
         aes(
           x = horizon,
           y = fitted / 100,
           group = quarter,
           color=yq(quarter)
         )) +
  geom_hline(aes(yintercept = 0), color = "darkred") +
  geom_line() +
  theme_light() +
  labs(y = "Percentage Points", x = "Quarter") +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_reverse(labels = label_percent())

irfs_short_plot



install.packages("socviz")


devtools::install_github("kjhealy/socviz")
