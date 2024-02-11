



## Libraries -----
##

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
    "boot",
    "stargazer"
  )


for (Package in required_Packages_Install) {
  if (!require(Package, character.only = TRUE)) {
    install.packages(Package, dependencies = TRUE)
  }
  
  library(Package, character.only = TRUE)
}


full_dataset_tbl <- read_csv("data/full_dataset.csv")
full_dataset_ts <-
  full_dataset_tbl |> mutate(year_quarter = yearquarter(year_quarter)) |> select(-...1) |> tsibble()

full_dataset_df <- as.data.frame(full_dataset_tbl)

# Estimator Definition -----
# horizon <- 8
estimator <- function(full_dataset_df, horizon) {
  ## LP-IV ----
  ##

  full_dataset_tbl <- as_tibble(full_dataset_df)
  
  coefs_inflation <- tibble()
  coefs_HAWK_inflation <- tibble()
  
  for (i in 1:horizon) {
    reg <-
      AER::ivreg(
        lead(dR, i) ~
          expected_inflation * demeaned_HAWK + lag(dR, 1) + lag(dR, 2) +
          lag(dR, 3) + lag(dR, 4) +
          lag(expected_inflation, 1) + lag(expected_inflation, 2) + lag(expected_inflation, 3) +
          lag(expected_inflation, 4) |
          expected_inflation * demeaned_HAWK_IV +  lag(dR, 1) +
          lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
          lag(expected_inflation, 1) + lag(expected_inflation, 2) + lag(expected_inflation, 3) +
          lag(expected_inflation, 4),
        data = full_dataset_ts
      )
    
    output <- reg$coefficients |> as_tibble() |>
      slice(c(2, length(reg$coefficients)))
    
    names(output) <- c("estimate")
    
    coefs_inflation <-
      bind_rows(coefs_inflation, output |> slice(1))
    
    coefs_HAWK_inflation <-
      bind_rows(coefs_HAWK_inflation, output |> slice(2))
    
  }
  
  
  
  ## Size-Persistence Estimation ----
  size_persistence_tbl <- tibble()
  for (t in 1:dim(full_dataset_tbl)[1]) {
    irf_t = coefs_inflation$estimate + full_dataset_tbl$demeaned_HAWK[t] * coefs_HAWK_inflation$estimate
    irf_t_cond <- if_else(irf_t < 0, 0, irf_t)
    size = mean(irf_t_cond)
    persistence = acf(irf_t_cond, plot = F, lag.max = 1)$acf[2]
    size_persistence_tbl <-
      bind_rows(size_persistence_tbl,
                tibble(size = size, persistence = persistence))
  }
  
  size_persistence_consumption_tbl <-
    size_persistence_tbl |>
    mutate(
      consumption = full_dataset_tbl$consumption,
      size_dmnd = size - mean(size),
      persistence_dmnd = persistence - mean(persistence),
      time = full_dataset_tbl$...1,
      log_consumption = full_dataset_tbl$log_consumption,
      delta_log_consumption = full_dataset_tbl$delta_log_consumption
    )
  
  
  model_1 <-
    lm(delta_log_consumption ~ size + size:persistence,
       size_persistence_consumption_tbl)
  model_2 <-
    lm(
      delta_log_consumption ~ size + persistence:size + size:I(persistence ^ 2),
      size_persistence_consumption_tbl
    )
  
  a <- model_1$coefficients
  b <- model_2$coefficients
  
  
  retr <- c(a, b)
  ret_vect <- retr |> as.vector()
  
  return(ret_vect)
}

names(retr)

# Bootstrap Sample Creation  -----

full_dataset_ts <- full_dataset_tbl |> as.ts()

bootstrapped_8 <-
  tsboot(
    full_dataset_ts,
    \(x) estimator(x, 8),
    R = 1e4,
    sim = "geom",
    l = 16,
    parallel =  "multicore",
    ncpus = 4
  ) # parallel does not work in windows

save(bootstrapped_8, file = "data/boot_8.Rdata")

bootstrapped_10 <-
  tsboot(
    full_dataset_ts,
    \(x) estimator(x, 10),
    R = 1e4,
    sim = "geom",
    l = 16,
    parallel =  "multicore",
    ncpus = 4 #4
  ) # parallel does not work in windows

save(bootstrapped_10, file = "data/boot_10.Rdata")


bootstrapped_12 <-
  tsboot(
    full_dataset_ts,
    \(x) estimator(x, 12),
    R = 1e4,
    sim = "geom",
    l = 16,
    parallel =  "multicore",
    ncpus = 4 #4
  ) # parallel does not work in windows

save(bootstrapped_12, file = "data/boot_12.Rdata")



as<-lm(log(consumption)~ X, size_persistence_consumption)$residuals
