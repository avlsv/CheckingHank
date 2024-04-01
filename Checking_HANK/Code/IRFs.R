## Checking HANK.
## The IRF
## Author: Alexander Vlasov
##
##

load("coefs.RData")
read.csv("")


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


full_dataset_ts$delta_expected_inflation |> mean(na.rm = T)
full_dataset_ts$delta_expected_unemployment |> mean(na.rm = T)
full_dataset_ts |> autoplot(vars(expected_inflation, expected_unemployment))
full_dataset_ts |> filter_index("1978"~"1981") |> autoplot(vars(expected_inflation, expected_unemployment))


#  2.01 -0.0950 
#
## IRFs preditions ----

size_persistence_tbl <- tibble()
irf_t_list <- c()
len = 15
for (t in 1:dim(full_dataset_ts)[1]) {
  irf_t =(coefs_inflation$estimate[1:len] + 
            full_dataset_ts$demeaned_HAWK[t] * coefs_HAWK_inflation$estimate[1:len]
  )*(1) 
  (coefs_unemployment$estimate[1:len] +
      full_dataset_ts$demeaned_HAWK[t] * coefs_HAWK_unemployment$estimate[1:len]
  ) * (-1/3) 
  
  
  irf_t_cond <-  irf_t
  irf_t_cond_f <- irf_t# c(irf_t_cond, rep(NaN, len - length(irf_t_cond)))
  
  irf_t_list <- c(irf_t_list, irf_t_cond_f)
  size = sum(irf_t_cond[irf_t_cond>0], na.rm=T)
  persistence = acf(irf_t_cond, lag = 1, plot = F)$acf[2] # length(irf_t_cond[irf_t_cond>0])
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

write.csv(size_persistence_consumption_tbl, file = "data/size_persistence_consumption1.csv")
write.csv(irfs, file = "data/irfs1.csv")

