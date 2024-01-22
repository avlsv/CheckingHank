





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
  full_dataset_tbl |> mutate(year_quarter = yearquarter(year_quarter)) |> tsibble()

full_dataset_df <- as.data.frame(full_dataset_tbl)



estimator <- function(full_dataset_df) {
  ## LP-IV ----
  ##
  ##
  
  full_dataset_tbl <- as_tibble(full_dataset_df)
  
  coefs_inflation <- tibble()
  coefs_HAWK_inflation <- tibble()
  
  for (i in 1:8) {
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
    size = mean(irf_t)
    persistence = acf(irf_t, plot = F, lag.max = 1)$acf[2]
    size_persistence_tbl <-
      bind_rows(size_persistence_tbl,
                tibble(size = size, persistence = persistence))
  }
  
  size_persistence_consumption_tbl <-
    size_persistence_tbl |> mutate(consumption = full_dataset_tbl$consumption)
  
  
 
  a <- lm(log(consumption) ~ size * persistence,
          size_persistence_consumption_tbl)$coefficients
  b <- lm(
    log(consumption) ~ size * persistence + size * I(persistence ^ 2),
    size_persistence_consumption_tbl
  )$coefficients
  
  retr <- c(a, b)
  ret_vect <- retr |> as.vector()
  
  
  
  return(ret_vect)
}

names(retr)


full_dataset_ts <- full_dataset_tbl |> as.ts()
bootstrapped <-
  tsboot(
    full_dataset_ts,
    estimator,
    R = 10000,
    sim = "geom",
    l = 16,
    parallel =  "multicore",
    ncpus = 4
  ) # parallel does not work in windows


save(bootstrapped, file = "data/boot.Rdata")
#load("data/boot1.Rdata")


boot.ci(
  bootstrapped,
  type = "perc",
  index = 1,
  conf = c(0.90, 0.95, 0.99)
)
boot.ci(
  bootstrapped,
  type = "perc",
  index = 2,
  conf = c(0.90, 0.95, 0.99)
)
boot.ci(
  bootstrapped,
  type = "perc",
  index = 3,
  conf = c(0.90, 0.95, 0.99)
)
boot.ci(
  bootstrapped,
  type = "perc",
  index = 4,
  conf =  c(0.90, 0.95, 0.99)
)
boot.ci(
  bootstrapped,
  type = "perc",
  index = 5,
  conf =  c(0.90, 0.95, 0.99)
)
boot.ci(
  bootstrapped,
  type = "basic",
  index = 6,
  conf =  c(0.90, 0.95, 0.99)
)
boot.ci(
  bootstrapped,
  type = "perc",
  index = 7,
  conf =  c(0.90, 0.95, 0.99)
)
boot.ci(
  bootstrapped,
  type = "perc",
  index = 8,
  conf =  c(0.90, 0.95, 0.99)
)
boot.ci(
  bootstrapped,
  type = "perc",
  index = 9,
  conf =  c(0.90, 0.95, 0.99)
)
boot.ci(
  bootstrapped,
  type = "perc",
  index = 10,
  conf =  c(0.90, 0.95, 0.99)
)



plot(bootstrapped, index = 6, nclass = 15)
matrix(v, nrow=2, ncol=length(v), byrow=TRUE)

w_b <- as.matrix(bootstrapped$t)-matrix(t(bootstrapped$t0), nrow=10000, ncol=length(t(bootstrapped$t0)), byrow=T)

w <- matrix(t(bootstrapped$t0), nrow=10000, ncol=length(t(bootstrapped$t0)), byrow=T)
colMeans(abs(w_b)>abs(w))
colMeans(w_b>w)


mean(bootstrapped$t[, 4] > 0)


bootstrapped$t0[6]



bootstrapped |> tidy() |> mutate(stat_name =  names(c(a, b, c)))



m1 <-
  lm(log(consumption) ~ size * persistence,
     size_persistence_consumption_tbl)
m2 <-
  lm(
    log(consumption) ~ size * persistence + size * I(persistence ^ 2),
    size_persistence_consumption_tbl
  )
stargazer(m1, m2, df=F)
