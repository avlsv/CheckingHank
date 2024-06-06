## Checking HANK.
## The IRF for both models
## 
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

load("data/Intermediate_Data/coefs_longer.RData")
full_dataset_tbl <- read_csv("data/Intermediate_Data/full_dataset.csv")
full_dataset_ts <-
  full_dataset_tbl |> mutate(year_quarter = yearquarter(year_quarter)) |> tsibble()

# Looking at the data -----

full_dataset_ts$delta_expected_inflation |> mean(na.rm = T)
full_dataset_ts$delta_expected_unemployment |> mean(na.rm = T)
full_dataset_ts |> autoplot(vars(expected_inflation, expected_unemployment))
full_dataset_ts |> filter_index("2007"~"2011") |> autoplot(vars(expected_inflation, expected_unemployment))
full_dataset_ts |> filter_index("2007"~"2011") |> autoplot(vars(delta_expected_inflation, delta_expected_unemployment_gap))

# Shocks of variables of interest in 2008Q4 --------
# 


shock_cpi_inflation <-
  full_dataset_ts |>
  filter_index("2008Q4") |>
  as_tibble() |>
  select(delta_expected_cpi_inflation) |>
  as.numeric()


shock_gdp_gap <-
  full_dataset_ts |>
  filter_index("2008Q4") |>
  as_tibble() |>
  select(delta_expected_gap) |>
  as.numeric()


shock_deflator_inflation <-
  full_dataset_ts |>
  filter_index("2008Q4") |>
  as_tibble() |>
  select(delta_expected_inflation) |>
  as.numeric()


shock_unemployment_gap <-
  full_dataset_ts |>
  filter_index("2008Q4") |>
  as_tibble() |>
  select(expected_unemployment_gap) |>
  as.numeric()




## IRFs preditions ----

irf_short_list <- c()
irf_long_list <- c()


for (t in 1:dim(full_dataset_ts)[1]) {
  
  irf_short_t = (
    coefs_cpi_inflation$estimate +
      full_dataset_ts$demeaned_HAWK[t] * coefs_HAWK_cpi_inflation$estimate
  ) *  shock_cpi_inflation + (
    coefs_unemployment$estimate +
      full_dataset_ts$demeaned_HAWK[t] * coefs_HAWK_gap$estimate
  ) *  shock_gdp_gap
  
  irf_short_se_t = (
    coefs_cpi_inflation +
      full_dataset_ts$demeaned_HAWK[t] * coefs_HAWK_cpi_inflation$estimate
  ) *  shock_cpi_inflation + (
    coefs_unemployment$estimate +
      full_dataset_ts$demeaned_HAWK[t] * coefs_HAWK_gap$estimate
  ) *  shock_gdp_gap
  
  
  irf_long_t = (
    coefs_inflation$estimate +
      full_dataset_ts$demeaned_HAWK[t] * coefs_HAWK_inflation$estimate
  ) *  shock_deflator_inflation + (
    coefs_unemployment$estimate +
      full_dataset_ts$demeaned_HAWK[t] * coefs_HAWK_unemployment$estimate
  ) *  shock_unemployment_gap
  
  
  
  irfs_t

  irfs_tbl<- tibble()  
  
}





irfs_long_plot <-
  ggplot(predicted_long_tbl|>filter(yearquarter("2008Q1") <= quarter & 
                                      quarter <= yearquarter("2009Q1")),
         aes(
           x = horizon,
           y = fitted / 100,
           color = yq(quarter),
           group = quarter
         )) +
  geom_hline(aes(yintercept = 0), color = "darkred") +
  geom_line() +
  theme_light() +
  labs(y = "Percentage Points", x = "Quarter") +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_reverse(labels = label_percent())

irfs_long_plot
irfs_short_plot
