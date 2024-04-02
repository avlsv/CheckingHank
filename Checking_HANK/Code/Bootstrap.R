## Checking HANK.
## Size-Persistence trade off estimation
## Author: Alexander Vlasov
##
##


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


estimator <- function(full_dataset_df) {
  ## LP-IV ----
  ##
  horizon <- 12
  full_dataset_tbl <- full_dataset_df |> as_tibble()
  
  
  coefs_inflation <- tibble()
  coefs_HAWK_inflation <- tibble()
  coefs_unemployment <- tibble()
  coefs_HAWK_unemployment <- tibble()
  coefs_intercept <- tibble()
  coefs_HAWK <- tibble()
  
  for (i in 1:horizon) {
    reg <-
      AER::ivreg(
        lead(dR, i) ~
          expected_inflation * demeaned_HAWK + expected_unemployment * demeaned_HAWK +
          lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
          lag(expected_inflation, 1) + lag(expected_inflation, 2) +
          lag(expected_inflation, 3) + lag(expected_inflation, 4) +
          lag(expected_unemployment, 1) + lag(expected_unemployment, 2) +
          lag(expected_unemployment, 3) + lag(expected_unemployment, 4) |
          expected_inflation * demeaned_HAWK_IV + expected_unemployment * demeaned_HAWK_IV +
          lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
          lag(expected_inflation, 1) + lag(expected_inflation, 2) +
          lag(expected_inflation, 3) + lag(expected_inflation, 4) +
          lag(expected_unemployment, 1) + lag(expected_unemployment, 2) +
          lag(expected_unemployment, 3) + lag(expected_unemployment, 4),
        data = full_dataset_tbl
      )
    
    output <-
      summary(reg)$coefficients |> as_tibble() |>
      slice(c(
        1,
        2,
        3,
        4,
        length(reg$coefficients) - 1,
        length(reg$coefficients)
      ))
    # expected_inflation, demeaned_HAWK, expected_unemployment
    
    names(output) <- c("estimate", "std_error", "t_stat", "p_value")
    coefs_intercept <-
      bind_rows(coefs_intercept, output |> slice(1))
    
    coefs_inflation <-
      bind_rows(coefs_inflation, output |> slice(2))
    
    coefs_HAWK <-
      bind_rows(coefs_HAWK, output |> slice(3))
    
    coefs_unemployment <-
      bind_rows(coefs_unemployment, output |> slice(4))
    
    
    coefs_HAWK_inflation <-
      bind_rows(coefs_HAWK_inflation, output |> slice(5))
    
    coefs_HAWK_unemployment <-
      bind_rows(coefs_HAWK_unemployment, output |> slice(6))
    
    
    
  }
  
  
  
  ## Size-Persistence Estimation ----
  len = 12
  size_persistence_tbl <- tibble()
  for (t in 1:dim(full_dataset_tbl)[1]) {
    irf_t = coefs_intercept$estimate[1:len] +
      (
        coefs_inflation$estimate[1:len] +
          full_dataset_tbl$demeaned_HAWK[t] * coefs_HAWK_inflation$estimate[1:len]
      ) * (2.01) +
      (
        coefs_unemployment$estimate[1:len] +
          full_dataset_tbl$demeaned_HAWK[t] * coefs_HAWK_unemployment$estimate[1:len]
      ) * (-0.0950) + coefs_HAWK$estimate[1:len] * full_dataset_ts$demeaned_HAWK[t]
    
    irf_t_cond <-  irf_t[1:which.min(pmax(0, irf_t))]
    
    #irf_t_cond_f <- c(irf_t_cond, rep(NaN, len - length(irf_t_cond)))
    #irf_t_list <- c(irf_t_list, irf_t_cond_f)
    
    size = sum(irf_t_cond[irf_t_cond > 0], na.rm = T)
    persistence = acf(irf_t[1:10], lag = 1, plot = F)$acf[2] #length(irf_t_cond[irf_t_cond > 0]) #length(irf_t_cond)#  # 
    size_persistence_tbl <-
      bind_rows(size_persistence_tbl,
                tibble(size = size, persistence = persistence))
    
  }
  

  size_persistence_consumption_tbl <-
    size_persistence_tbl |>
    mutate(
      year_quarter=yearquarter(full_dataset_tbl$year_quarter),
      consumption = full_dataset_tbl$consumption,
      size_dmnd = size - mean(size),
      persistence_dmnd = persistence - mean(persistence),
      time = full_dataset_tbl$...1,
      log_consumption = full_dataset_tbl$log_consumption,
      delta_log_consumption = full_dataset_tbl$delta_log_consumption
    )
  
  model_0 <- lm(delta_log_consumption ~ size,
                size_persistence_consumption_tbl)
  
  model_1 <-
    lm(delta_log_consumption ~ size + size:persistence,
       size_persistence_consumption_tbl)
  
  model_2 <-
    lm(
      delta_log_consumption ~ size + size:persistence + size:I(persistence ^ 2),
      size_persistence_consumption_tbl
    )
  
  a <-
    model_0$coefficients |> tidy()  
  b <-
    model_1$coefficients |> tidy() #> filter( !grepl( 'lag', names))
  c <-
    model_2$coefficients |> tidy() #> filter(!grepl('lag', names))
  
  
  retr <- c(a$x, b$x, c$x)
  ret_vect <- retr |> as.vector()
  
  return(ret_vect)
}

#names(retr)

# Bootstrap Sample Creation  -----


bootstrapped <-
  tsboot(
    ts(full_dataset_df),
    estimator,
    R = 1000,
    sim = "geom",
    l = 16,
    parallel =  "multicore",
    ncpus = 4
  ) # parallel does not work in windows

#bootstrapped$t


save(bootstrapped, file = "data/boot_1k.Rdata")

#save(bootstrapped, file = "data/boot_10k.Rdata")
