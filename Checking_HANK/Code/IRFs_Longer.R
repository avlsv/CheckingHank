## Checking HANK.
## The IRF
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
full_dataset_tbl <- read_csv("data/full_dataset.csv")
full_dataset_ts <-
  full_dataset_tbl |> mutate(year_quarter = yearquarter(year_quarter)) |> tsibble()

# Looking at the data -----

full_dataset_ts$delta_expected_inflation |> mean(na.rm = T)
full_dataset_ts$delta_expected_unemployment |> mean(na.rm = T)
full_dataset_ts |> autoplot(vars(expected_inflation, expected_unemployment))
full_dataset_ts |> filter_index("2007"~"2011") |> autoplot(vars(expected_inflation, expected_unemployment))
full_dataset_ts |> filter_index("2007"~"2011") |> autoplot(vars(delta_expected_inflation, delta_expected_unemployment))

# 2008Q4
full_dataset_ts |> filter_index("2008Q4") |> select(delta_expected_inflation, delta_expected_unemployment)
#  2.01 -0.0950 
#
## IRFs preditions ----


size_persistence_tbl <- tibble()
irf_t_list <- c()
len = 13
for (t in 1:dim(full_dataset_ts)[1]) {
  irf_t = (
    coefs_inflation$estimate[1:len] +
      full_dataset_ts$demeaned_HAWK[t] * coefs_HAWK_inflation$estimate[1:len]
  ) *  (0.575) + (
    coefs_unemployment$estimate[1:len] +
      full_dataset_ts$demeaned_HAWK[t] * coefs_HAWK_unemployment$estimate[1:len]
  ) *  (-1.12)
  
  
  
  irf_t_cond <-
    irf_t[irf_t > 0] #irf_t[1:(7+which.min(irf_t[8:len]))]
  irf_t_cond_f <- c(irf_t_cond, rep(NaN, len - length(irf_t_cond)))
  
  irf_t_list <- c(irf_t_list, irf_t_cond_f)
  size = sum(irf_t_cond[irf_t_cond > 0], na.rm = T)
  persistence =  acf(irf_t_cond, lag = 1, plot = F)$acf[2] # length(irf_t_cond[irf_t_cond>0])
  size_persistence_tbl <-
    bind_rows(size_persistence_tbl,
              tibble(size = size, persistence = persistence))
}



irfs <-
  tibble(
    irf = irf_t_list,
    quart = rep(1:len, dim(full_dataset_ts)[1]),
    time = rep(full_dataset_ts$year_quarter, each = len)
  )




## Size-Persistence estimation ----

size_persistence_consumption_tbl <-
  size_persistence_tbl |> 
  mutate(
    delta_log_consumption = full_dataset_tbl$delta_log_consumption,
    log_consumption = full_dataset_tbl$log_consumption,
    size_dmnd = size - mean(size),  
    persistence_dmnd = persistence - mean(persistence), 
    year_quarter = yearquarter(full_dataset_tbl$year_quarter),
    inflation = full_dataset_tbl$core_inflation,
    expected_inflation = full_dataset_tbl$expected_inflation
  )



write.csv(size_persistence_consumption_tbl, file = "data/size_persistence_consumption_longer.csv")
write.csv(irfs, file = "data/irfs_longer.csv")
