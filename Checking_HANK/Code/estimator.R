## Checking HANK.
## The main estimation procedure (without bootstrap) 
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


full_dataset_tbl <- read_csv("data/full_dataset.csv")
full_dataset_ts <-
  full_dataset_tbl |> mutate(year_quarter = yearquarter(year_quarter)) |> tsibble()


## LP-IV ----

coefs_inflation <- tibble()
coefs_HAWK_inflation <- tibble()
coefs_intercept <- tibble()
coefs_HAWK <- tibble()
for (i in 1:16) {
  reg <-
    AER::ivreg(
      lead(dR, i) ~
        expected_inflation * demeaned_HAWK +
        lag(dR, 1) + lag(dR, 2) +lag(dR, 3) +lag(dR, 4)+
        lag(expected_inflation, 1) + lag(expected_inflation, 2)+lag(expected_inflation, 3) + lag(expected_inflation, 4)|
        expected_inflation * demeaned_HAWK_IV+
          lag(dR, 1) + lag(dR, 2) +lag(dR, 3) + lag(dR, 4) + 
        lag(expected_inflation, 1) + lag(expected_inflation, 2) + lag(expected_inflation, 3) + lag(expected_inflation, 4),
      data = full_dataset_ts
    )
  
  output <-
    summary(reg, vcov. = vcovHAC(reg))$coefficients |> as_tibble() |>
    slice(c(1, 2, 3, length(reg$coefficients)))
  
  names(output) <- c("estimate", "std_error", "t_value", "p_ratio")
  
  coefs_intercept <- 
    bind_rows(coefs_intercept, output |> slice(1))
  
  coefs_inflation <-
    bind_rows(coefs_inflation, output |> slice(2))
  
  coefs_HAWK <- bind_rows(coefs_HAWK, output |> slice(3))
  
  coefs_HAWK_inflation <-
    bind_rows(coefs_HAWK_inflation, output |> slice(4))

  
  se_plus <-
    sqrt(vcovHAC(reg)[1, 1] + (2 / 12) ^ 2 * vcovHAC(reg)[length(reg$coefficients), length(reg$coefficients)] +
           2 * 2 / 12 * vcovHAC(reg)[1, length(reg$coefficients)])
  
  se_minus <-
    sqrt(vcovHAC(reg)[1, 1] + (2 / 12) ^ 2 * vcovHAC(reg)[length(reg$coefficients), length(reg$coefficients)] -
           2 * 2 / 12 * vcovHAC(reg)[1, length(reg$coefficients)])
  
  
  
}



coefs_inflation$estimate
coefs_HAWK_inflation$estimate
coefs_inflation <- 
  coefs_inflation |> mutate(quarter = row_number())
coefs_HAWK_inflation <-
  coefs_HAWK_inflation |> mutate(quarter = row_number())
coefs_HAWK <-
  coefs_HAWK |> mutate(quarter = row_number())
coefs_intercept <-
  coefs_intercept |> mutate(quarter = row_number())


average_responce_plot <-
  ggplot(coefs_inflation, aes(x = quarter, y = estimate)) +
  geom_hline(aes(yintercept = 0),  color = "darkred") +
  geom_ribbon(
    aes(
      ymin = estimate - qnorm(1-0.05/2) * std_error,
      ymax = estimate + qnorm(1-0.05/2) * std_error
    ),
    alpha = 0.13,
    linetype = 0,
    fill = "#477998"
  ) +
  geom_ribbon(
    aes(ymin = estimate - std_error, ymax = estimate + std_error),
    alpha = 0.13,
    linetype = 0,
    fill = "#477998"
  ) +
  geom_ribbon(
    aes(
      ymin = estimate - qnorm(1-.10/2) * std_error,
      ymax = estimate + qnorm(1-.10/2) * std_error
    ),
    alpha = 0.123,
    linetype = 0,
    fill = "#477998"
  ) +  geom_line() + 
  labs(x = "Quarter", y = "Percentage Points") +
  theme_light()


average_responce_plot

differential_responce_plot <-
  ggplot(coefs_HAWK_inflation, aes(x = quarter, y = 2 / 12 * estimate)) +  geom_ribbon(
    aes(
      ymin = 2 / 12 * estimate - qnorm(1-0.05/2) * 2 / 12 * std_error,
      ymax = 2 / 12 * estimate + qnorm(1-0.05/2)* 2 / 12 * std_error
    ),
    alpha = 0.13,
    linetype = 0,
    fill = "#477998"
  ) +
  geom_ribbon(
    aes(
      ymin = 2 / 12 * estimate - 2 / 12 * std_error,
      ymax = 2 / 12 * estimate + 2 / 12 * std_error
    ),
    alpha = 0.13,
    linetype = 0,
    fill = "#477998"
  ) +
  geom_ribbon(
    aes(
      ymin = 2 / 12 * estimate - 1.644 * 2 / 12 * std_error,
      ymax = 2 / 12 * estimate + 1.644 * 2 / 12 * std_error
    ),
    alpha = 0.13,
    linetype = 0,
    fill = "#477998"
  ) +
  labs(x = "Quarter", y = "Percentage Points") +
  geom_hline(aes(yintercept = 0), color = "darkred") +  geom_line() +
  theme_light()

differential_responce_plot

ggsave(
  "Average.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  average_responce_plot,
  width = 148.5 / 2*1.5,
  height =  210 / 4*1.5,
  units = "mm"
)

ggsave(
  "Differential.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  differential_responce_plot,
  width = 148.5 / 2 * 1.5 ,
  height = 210 / 4 * 1.5 ,
  units = "mm"
)



LP_2 <-
  ivreg(
    lead(dR, 2) ~
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


LP_4 <-
  ivreg(
    lead(dR, 4) ~
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


LP_6 <-
  ivreg(
    lead(dR, 6) ~
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

LP_8 <-
  ivreg(
    lead(dR, 8) ~
      expected_inflation * demeaned_HAWK +I(demeaned_HAWK^2)*expected_inflation+ lag(dR, 1) + lag(dR, 2) +
      lag(dR, 3) + lag(dR, 4) +
      lag(expected_inflation, 1) + lag(expected_inflation, 2) + lag(expected_inflation, 3) +
      lag(expected_inflation, 4) |
      expected_inflation * demeaned_HAWK_IV+I(demeaned_HAWK_IV^2)*expected_inflation +  lag(dR, 1) +
      lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_inflation, 1) + lag(expected_inflation, 2) + lag(expected_inflation, 3) +
      lag(expected_inflation, 4),
    data = full_dataset_ts
  )

LP_10 <-
  ivreg(
    lead(dR, 10) ~
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

fs_1 <-
  lm(
    demeaned_HAWK ~ expected_inflation * demeaned_HAWK_IV +  lag(dR, 1) +
      lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_inflation, 1) + lag(expected_inflation, 2) +
      lag(expected_inflation, 3) + lag(expected_inflation, 4),
    data = full_dataset_ts
  )


fs_2 <-
  lm(
    I(demeaned_HAWK * expected_inflation) ~ expected_inflation * demeaned_HAWK_IV +  lag(dR, 1) +
      lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_inflation, 1) + lag(expected_inflation, 2) +
      lag(expected_inflation, 3) + lag(expected_inflation, 4),
    data = full_dataset_ts
  )



stargazer(
  LP_2,
  LP_4,
  LP_6,
  LP_8,
  LP_10,
  fs_1,
  fs_2,
  se = list(
    summary(LP_2, vcov = vcovHAC(LP_2))$coef[, 2],
    summary(LP_4, vcov = vcovHAC(LP_4))$coef[, 2],
    summary(LP_6, vcov = vcovHAC(LP_6))$coef[, 2],
    summary(LP_8, vcov = vcovHAC(LP_8))$coef[, 2],
    summary(LP_10, vcov = vcovHAC(LP_10))$coef[, 2],
    summary(fs_1, vcov = vcovHAC(fs_1))$coef[, 2],
    summary(fs_2, vcov = vcovHAC(fs_2))$coef[, 2]
  ),
  df = F
)

LP_2 |> summary(diagnostics = T)
LP_4 |> summary(diagnostics = T)
LP_6 |> summary(diagnostics = T)
LP_8 |> summary(diagnostics = T)
LP_10 |> summary(diagnostics = T)



## Size-Persistence Estimation ----
size_persistence_tbl <- tibble()
irf_t_list <- c()
len = 15

for (t in 1:dim(full_dataset_ts)[1]) {
  irf_t = (coefs_inflation$estimate[1:len] + full_dataset_ts$demeaned_HAWK[t] * coefs_HAWK_inflation$estimate[1:len])*full_dataset_ts$expected_inflation[t] + full_dataset_ts$demeaned_HAWK[t] * coefs_HAWK$estimate[1:len]
  irf_t_cond <- irf_t[1:(ifelse(which.min(irf_t[3:len] >= -0.1)+2 == 1,len+1,which.min(irf_t >= 0.05))-1)]
  irf_t_cond_f <- c(irf_t_cond, rep(NaN, len-length(irf_t_cond)))
  irf_t_list <- c(irf_t_list, irf_t_cond_f)
  size = sum(irf_t_cond)
  persistence = length(irf_t_cond)#acf(irf_t_cond, lag = 1, plot = F)$acf[2]
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

