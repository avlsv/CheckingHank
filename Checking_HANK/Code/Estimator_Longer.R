## Checking HANK.
## The SD-LP-IV procedure
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

full_dataset_tbl <- read_csv("data/full_dataset.csv")
full_dataset_ts <-
  full_dataset_tbl |> mutate(year_quarter = yearquarter(year_quarter)) |> tsibble()


# LP-IV ----

coefs_inflation <- tibble()
coefs_HAWK_inflation <- tibble()
coefs_unemployment <- tibble()
coefs_HAWK_unemployment <- tibble()
#coefs_intercept <- tibble()
#coefs_HAWK <- tibble()
for (i in 0:20) {
  reg <-
    AER::ivreg(
      lead(dR, i) ~
        expected_inflation * demeaned_HAWK +  demeaned_expected_unemployment * demeaned_HAWK +
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
      data = full_dataset_ts
    )
  
  output <-
    summary(reg, vcov. = vcovHAC(reg))$coefficients |> as_tibble() |>
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
  
  #coefs_intercept <-
  #  bind_rows(coefs_intercept, output |> slice(1))
  
  coefs_inflation <-
    bind_rows(coefs_inflation, output |> slice(2))
  
  #coefs_HAWK <-
  #  bind_rows(coefs_HAWK, output |> slice(3))
  
  coefs_unemployment <-
    bind_rows(coefs_unemployment, output |> slice(4))
  
  
  coefs_HAWK_inflation <-
    bind_rows(coefs_HAWK_inflation, output |> slice(5))
  
  coefs_HAWK_unemployment <-
    bind_rows(coefs_HAWK_unemployment, output |> slice(6))
  
}



coefs_inflation$estimate
coefs_HAWK_inflation$estimate

coefs_unemployment$estimate
coefs_HAWK_unemployment$estimate


### Saving and transforming coefficients -----

#coefs_intercept <-
#  coefs_intercept |>  mutate(quarter = row_number())

#coefs_HAWK <-
#  coefs_HAWK |> mutate(quarter = row_number())


coefs_inflation <-
  coefs_inflation |> mutate(quarter = row_number()-1)
coefs_HAWK_inflation <-
  coefs_HAWK_inflation |> mutate(quarter = row_number()-1)

coefs_unemployment <-
  coefs_unemployment |> mutate(quarter = row_number()-1)
coefs_HAWK_unemployment <-
  coefs_HAWK_unemployment |> mutate(quarter = row_number()-1)


save(
  coefs_inflation,
  coefs_HAWK_inflation,
  coefs_unemployment,
  coefs_HAWK_unemployment,
  file = "coefs_longer.RData"
)

## LP-IV coefficient plots -----

average_inflation_responce_plot <-
  ggplot(coefs_inflation, aes(x = quarter, y = estimate)) +
  geom_hline(aes(yintercept = 0),  color = "darkred") +
  geom_ribbon(
    aes(
      ymin = estimate - qnorm(1 - 0.05 / 2) * std_error,
      ymax = estimate + qnorm(1 - 0.05 / 2) * std_error
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
      ymin = estimate - qnorm(1 - .10 / 2) * std_error,
      ymax = estimate + qnorm(1 - .10 / 2) * std_error
    ),
    alpha = 0.123,
    linetype = 0,
    fill = "#477998"
  ) +  geom_line() +
  labs(x = "Quarter", y = "Percentage Points") +
  theme_light()


average_inflation_responce_plot

differential_inflation_responce_plot <-
  ggplot(coefs_HAWK_inflation, aes(x = quarter, y = 2 / 12 * estimate)) +  geom_ribbon(
    aes(
      ymin = 2 / 12 * estimate - qnorm(1 - 0.05 / 2) * 2 / 12 * std_error,
      ymax = 2 / 12 * estimate + qnorm(1 - 0.05 / 2) * 2 / 12 * std_error
    ),
    alpha = 0.13,
    linetype = 0,
    fill = "#477998"
  ) +
  geom_ribbon(
    aes(
      ymin = 2 / 12 * estimate - qnorm(1 - 0.1 / 2) * 2 / 12 * std_error,
      ymax = 2 / 12 * estimate + qnorm(1 - 0.1 / 2) * 2 / 12 * std_error
    ),
    alpha = 0.13,
    linetype = 0,
    fill = "#477998"
  ) +
  geom_ribbon(
    aes(
      ymin = 2 / 12 * estimate -   2 / 12 * std_error,
      ymax = 2 / 12 * estimate +  2 / 12 * std_error
    ),
    alpha = 0.13,
    linetype = 0,
    fill = "#477998"
  ) +
  labs(x = "Quarter", y = "Percentage Points") +
  geom_hline(aes(yintercept = 0), color = "darkred") +  geom_line() +
  theme_light()

differential_inflation_responce_plot




average_unemployment_responce_plot <-
  ggplot(coefs_unemployment, aes(x = quarter, y = estimate)) +
  geom_hline(aes(yintercept = 0),  color = "darkred") +
  geom_ribbon(
    aes(
      ymin = estimate - qnorm(1 - 0.05 / 2) * std_error,
      ymax = estimate + qnorm(1 - 0.05 / 2) * std_error
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
      ymin = estimate - qnorm(1 - .10 / 2) * std_error,
      ymax = estimate + qnorm(1 - .10 / 2) * std_error
    ),
    alpha = 0.123,
    linetype = 0,
    fill = "#477998"
  ) +  geom_line() +
  labs(x = "Quarter", y = "Percentage Points") +
  theme_light()

average_unemployment_responce_plot




differential_unemployment_responce_plot <-
  ggplot(coefs_HAWK_unemployment,
         aes(x = quarter, y =  2 / 12 * estimate)) +
  geom_hline(aes(yintercept = 0),  color = "darkred") +
  geom_ribbon(
    aes(
      ymin =
        2 / 12 * estimate -
        2 / 12 * qnorm(1 - 0.05 / 2) * std_error,
      ymax =
        2 / 12 * estimate +
        2 / 12 * qnorm(1 - 0.05 / 2) * std_error
    ),
    alpha = 0.13,
    linetype = 0,
    fill = "#477998"
  ) +
  geom_ribbon(
    aes(
      ymin =
        2 / 12 * estimate -
        2 / 12 * std_error,
      ymax =
        2 / 12 * estimate +
        2 / 12 * std_error
    ),
    alpha = 0.13,
    linetype = 0,
    fill = "#477998"
  ) +
  geom_ribbon(
    aes(
      ymin =
        2 / 12 * estimate -
        2 / 12 * qnorm(1 - .10 / 2) * std_error,
      ymax =
        2 / 12 * estimate +
        2 / 12 * qnorm(1 - .10 / 2) * std_error
    ),
    alpha = 0.123,
    linetype = 0,
    fill = "#477998"
  ) +  geom_line() +
  labs(x = "Quarter", y = "Percentage Points") +
  theme_light()

differential_unemployment_responce_plot


### Saving plots -----

ggsave(
  "average_deflator_inflation_longer.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  average_inflation_responce_plot,
  width = 148.5 / 2 * 1.5,
  height =  210 / 4 * 1.5,
  units = "mm"
)

ggsave(
  "differential_deflator_inflation_longer.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  differential_inflation_responce_plot,
  width = 148.5 / 2 * 1.5 ,
  height = 210 / 4 * 1.5 ,
  units = "mm"
)


ggsave(
  "average_unemployment_longer.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  average_unemployment_responce_plot,
  width = 148.5 / 2 * 1.5,
  height =  210 / 4 * 1.5,
  units = "mm"
)

ggsave(
  "differential_unemployment_longer.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  differential_unemployment_responce_plot,
  width = 148.5 / 2 * 1.5 ,
  height = 210 / 4 * 1.5 ,
  units = "mm"
)

## LP-IV coefficient tables -----


LP_2 <-
  ivreg(
    lead(dR, 2) ~
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
    data = full_dataset_ts
  )


LP_4 <-
  ivreg(
    lead(dR, 4) ~
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
    data = full_dataset_ts
  )


LP_6 <-
  ivreg(
    lead(dR, 6) ~
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
    data = full_dataset_ts
  )

LP_8 <-
  ivreg(
    lead(dR, 8) ~
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
    data = full_dataset_ts
  )

LP_10 <-
  ivreg(
    lead(dR, 10) ~
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
    data = full_dataset_ts
  )


LP_12 <-
  ivreg(
    lead(dR, 12) ~
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
    data = full_dataset_ts
  )

fs_1 <-
  lm(
    demeaned_HAWK ~ expected_inflation * demeaned_HAWK_IV + expected_unemployment * demeaned_HAWK_IV +
      lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_inflation, 1) + lag(expected_inflation, 2) +
      lag(expected_inflation, 3) + lag(expected_inflation, 4) +
      lag(expected_unemployment, 1) + lag(expected_unemployment, 2) +
      lag(expected_unemployment, 3) + lag(expected_unemployment, 4),
    data = full_dataset_ts
  )


fs_2 <-
  lm(
    I(demeaned_HAWK * expected_inflation) ~ expected_inflation * demeaned_HAWK_IV + expected_unemployment * demeaned_HAWK_IV +
      lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_inflation, 1) + lag(expected_inflation, 2) +
      lag(expected_inflation, 3) + lag(expected_inflation, 4) +
      lag(expected_unemployment, 1) + lag(expected_unemployment, 2) +
      lag(expected_unemployment, 3) + lag(expected_unemployment, 4),
    data = full_dataset_ts
  )

fs_3 <-
  lm(
    expected_unemployment ~ expected_inflation * demeaned_HAWK_IV + expected_unemployment * demeaned_HAWK_IV +
      lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_inflation, 1) + lag(expected_inflation, 2) +
      lag(expected_inflation, 3) + lag(expected_inflation, 4) +
      lag(expected_unemployment, 1) + lag(expected_unemployment, 2) +
      lag(expected_unemployment, 3) + lag(expected_unemployment, 4),
    data = full_dataset_ts
  )

fs_4 <-
  lm(
    I(demeaned_HAWK * expected_unemployment) ~ expected_inflation * demeaned_HAWK_IV + expected_unemployment * demeaned_HAWK_IV +
      lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_inflation, 1) + lag(expected_inflation, 2) +
      lag(expected_inflation, 3) + lag(expected_inflation, 4) +
      lag(expected_unemployment, 1) + lag(expected_unemployment, 2) +
      lag(expected_unemployment, 3) + lag(expected_unemployment, 4),
    data = full_dataset_ts
  )

stargazer(
  LP_2,
  LP_4,
  LP_6,
  LP_8,
  LP_10,
  LP_12,
  se =
    list(
      summary(LP_2, vcov = vcovHAC(LP_2))$coef[, 2],
      summary(LP_4, vcov = vcovHAC(LP_4))$coef[, 2],
      summary(LP_6, vcov = vcovHAC(LP_6))$coef[, 2],
      summary(LP_8, vcov = vcovHAC(LP_8))$coef[, 2],
      summary(LP_10, vcov = vcovHAC(LP_10))$coef[, 2],
      summary(LP_12, vcov = vcovHAC(LP_10))$coef[, 2]
    ),
  df = F
)

stargazer(
  fs_1,
  fs_2,
  fs_3,
  fs_4,
  se = list(
    summary(fs_1, vcov = vcovHAC(fs_1))$coef[, 2],
    summary(fs_2, vcov = vcovHAC(fs_2))$coef[, 2],
    summary(fs_3, vcov = vcovHAC(fs_3))$coef[, 2],
    summary(fs_4, vcov = vcovHAC(fs_4))$coef[, 2]
  ),
  df = F
)

LP_2 |> summary(diagnostics = T)
LP_4 |> summary(diagnostics = T)
LP_6 |> summary(diagnostics = T)
LP_8 |> summary(diagnostics = T)
LP_10 |> summary(diagnostics = T)
LP_12 |> summary(diagnostics = T)
