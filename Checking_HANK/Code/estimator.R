




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

for (i in 1:16) {
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
  
  output <-
    summary(reg, vcov. = vcovHAC(reg))$coefficients |> as_tibble() |>
    slice(c(2, length(reg$coefficients)))
  
  names(output) <- c("estimate", "std_error", "t_value", "p_ratio")
  
  coefs_inflation <-
    bind_rows(coefs_inflation, output |> slice(1))
  
  coefs_HAWK_inflation <-
    bind_rows(coefs_HAWK_inflation, output |> slice(2))
  
  se_plus <-
    sqrt(vcovHAC(reg)[1, 1] + (2 / 12) ^ 2 * vcovHAC(reg)[length(reg$coefficients), length(reg$coefficients)] +
           2 * 2 / 12 * vcovHAC(reg)[1, length(reg$coefficients)])
  
  se_minus <-
    sqrt(vcovHAC(reg)[1, 1] + (2 / 12) ^ 2 * vcovHAC(reg)[length(reg$coefficients), length(reg$coefficients)] -
           2 * 2 / 12 * vcovHAC(reg)[1, length(reg$coefficients)])
  
  
  
}



coefs_inflation$estimate
coefs_HAWK_inflation$estimate
coefs_inflation <- coefs_inflation |> mutate(quarter = row_number())
coefs_HAWK_inflation <-
  coefs_HAWK_inflation |> mutate(quarter = row_number())




average_responce_plot <-
  ggplot(coefs_inflation, aes(x = quarter, y = estimate)) +
  geom_line() +  geom_ribbon(
    aes(
      ymin = estimate - 1.96 * std_error,
      ymax = estimate + 1.96 * std_error
    ),
    alpha = 0.1,
    linetype = 0,
    color = "grey"
  ) +
  geom_ribbon(
    aes(ymin = estimate - std_error, ymax = estimate + std_error),
    alpha = 0.1,
    linetype = 0,
    color = "grey"
  ) +
  labs(x = "Quarter", y = "Percentage Points") +
  geom_hline(aes(yintercept = 0),  color = "darkred") +
  theme_light()


differential_responce_plot <-
  ggplot(coefs_HAWK_inflation, aes(x = quarter, y = 2 / 12 * estimate)) +
  geom_line() +  geom_ribbon(
    aes(
      ymin = 2 / 12 * estimate - 1.96 * 2 / 12 * std_error,
      ymax = 2 / 12 * estimate + 1.96 * 2 / 12 * std_error
    ),
    alpha = 0.1,
    linetype = 0,
    color = "grey"
  ) +
  geom_ribbon(
    aes(
      ymin = 2 / 12 * estimate - 2 / 12 * std_error,
      ymax = 2 / 12 * estimate + 2 / 12 * std_error
    ),
    alpha = 0.1,
    linetype = 0,
    color = "grey"
  ) + labs(x = "Quarter", y = "Percentage Points") +
  geom_hline(aes(yintercept = 0), color = "darkred") +
  theme_light()



differential_responce_plot <-
  ggplot(coefs_HAWK_inflation, aes(x = quarter, y = 2 / 12 * estimate)) +
  geom_line() +  geom_ribbon(
    aes(
      ymin = 2 / 12 * estimate - 1.96 * 2 / 12 * std_error,
      ymax = 2 / 12 * estimate + 1.96 * 2 / 12 * std_error
    ),
    alpha = 0.1,
    linetype = 0,
    color = "grey"
  ) +
  geom_ribbon(
    aes(
      ymin = 2 / 12 * estimate - 2 / 12 * std_error,
      ymax = 2 / 12 * estimate + 2 / 12 * std_error
    ),
    alpha = 0.1,
    linetype = 0,
    color = "grey"
  ) + labs(x = "Quarter", y = "Percentage Points") +
  geom_hline(aes(yintercept = 0)) +
  theme_light()



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
for (t in 1:dim(full_dataset_ts)[1]) {
  irf_t = coefs_inflation$estimate + full_dataset_ts$demeaned_HAWK[t] * coefs_HAWK_inflation$estimate
  size = sum(irf_t)
  persistence = ar(irf_t, order = 1)$ar
  size_persistence_tbl <-
    bind_rows(size_persistence_tbl,
              tibble(size = size, persistence = persistence))
}

size_persistence_ts <-
  size_persistence_tbl |> mutate(year_quarter = full_dataset_ts$year_quarter) |> as_tsibble(index = year_quarter)

size_persistence_consumption_ts <-
  inner_join(size_persistence_ts, consumption_ts)

stargazer::stargazer(size_persistence_tbl |> as.data.frame(), summary = T)

lm(size ~ persistence, size_persistence_ts) |> summary()


lm(log(consumption) ~ size * persistence,
   size_persistence_consumption_ts) |> summary()
lm(
  log(consumption) ~ size * persistence + size * I(persistence ^ 2),
  size_persistence_consumption_ts
) |> summary()
