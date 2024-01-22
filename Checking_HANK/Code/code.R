##
## The general coding enviroment
## Author: Alexander Vlasov
##
##

## Libraries -----
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
    "boot"
  )

for (Package in required_Packages_Install) {
  if (!require(Package, character.only = TRUE)) {
    install.packages(Package, dependencies = TRUE)
  }
  
  library(Package, character.only = TRUE)
}


## Estimation procedure ----
## 

full_dataset_tbl <- full_dataset |> as_tibble()
full_dataset_ts$core_inflation |> mean()


full_dataset_tbl |> summarize(ses = sd(demeaned_HAWK * core_inflation))



# Estimated predictive regression
mod <- AER::ivreg(
  lead(dR, 1) ~
    core_inflation * demeaned_HAWK + lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
    lag(core_inflation, 1) + lag(core_inflation, 2) + lag(core_inflation, 3) + lag(core_inflation, 4) |
    core_inflation * demeaned_HAWKIV +  lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
    lag(core_inflation, 1) + lag(core_inflation, 2) + lag(core_inflation, 3) + lag(core_inflation, 4),
  data = full_dataset_ts
)

summary(mod, vcov. = NeweyWest(mod))$coefficients |> as_tibble() |> slice(c(2, length(mod$coefficients)))



coefs_inflation <- tibble()
coefs_HAWK_inflation <- tibble()
for (i in 1:20) {
  reg <-
    AER::ivreg(
      lead(dR, i) ~
        core_inflation * demeaned_HAWK + lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
        lag(core_inflation, 1) + lag(core_inflation, 2) + lag(core_inflation, 3) + lag(core_inflation, 4) |
        core_inflation * demeaned_HAWKIV +  lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
        lag(core_inflation, 1) + lag(core_inflation, 2) + lag(core_inflation, 3) + lag(core_inflation, 4),
      data = full_dataset_ts
    )
  
  output <-
    summary(reg, vcov. = vcovHAC(reg))$coefficients |> as_tibble() |>
    slice(c(2, length(reg$coefficients)))
  
  coefs_inflation <-
    bind_rows(coefs_inflation, output |> slice(1))
  
  coefs_HAWK_inflation <-
    bind_rows(coefs_HAWK_inflation, output |> slice(2))
  
}



coefs_inflation$Estimate |> plot()
coefs_HAWK_inflation$Estimate |> plot()


size_persistence_tbl <- tibble()
for (t in 1:dim(full_dataset)[1]) {
  irf_t = coefs_inflation$Estimate + full_dataset_ts$demeaned_HAWK[t] * coefs_HAWK_inflation$Estimate
  size = sum(irf_t)
  persistence = ar(irf_t, order = 1)$ar
  size_persistence_tbl <-
    bind_rows(size_persistence_tbl,
              tibble(size = size, persistence = persistence))
}

size_persistence_ts <-
  size_persistence_tbl |> mutate(year_quarter = full_dataset_ts$year_quarter) |> as_tsibble(index = year_quarter)

size_persistence_consumption_ts <- inner_join(size_persistence_ts,consumption_ts)

lm(size~persistence, size_persistence_ts) |> summary()

lm(log(consumption)~ size*persistence, size_persistence_consumption_ts)|> summary()
lm(log(consumption)~ size*persistence+size*I(persistence^2), size_persistence_consumption_ts)|> summary()


