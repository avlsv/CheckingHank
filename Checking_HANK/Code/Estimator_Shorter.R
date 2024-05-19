## Checking HANK.
## The SD-LP-IV procedure for
## Author: Alexander Vlasov
##
##



# Libraries -----
required_packages_install <-
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


for (package in required_packages_install) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
  
  library(package, character.only = TRUE)
}



# Datasets ----

full_dataset_tbl <- read_csv("data/intermediate_data/full_dataset.csv")
full_dataset_ts <-
  full_dataset_tbl |> mutate(year_quarter = yearquarter(year_quarter)) |> tsibble()


# LP-IV ----

coefs_cpi_inflation <- tibble()
coefs_HAWK_cpi_inflation <- tibble()
coefs_gap <- tibble()
coefs_HAWK_gap <- tibble()
r_squares_short <- c()
predicted_short_tbl <- tibble()
hausman_list_short <- c()
#coefs_intercept <- tibble()
#coefs_HAWK <- tibble()
for (i in 0:20) {
  reg <-
    ivreg(
      lead(dR, i) ~
        expected_cpi_inflation * demeaned_HAWK +  expected_gap * demeaned_HAWK +
        lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
        lag(expected_cpi_inflation, 1) + lag(expected_cpi_inflation, 2) +
        lag(expected_cpi_inflation, 3)  + lag(expected_cpi_inflation, 4) +
        lag(expected_gap, 1) + lag(expected_gap, 2) +
        lag(expected_gap, 3) + lag(expected_gap, 4) |
        expected_cpi_inflation * demeaned_HAWK_IV + expected_gap * demeaned_HAWK_IV +
        lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
        lag(expected_cpi_inflation, 1) + lag(expected_cpi_inflation, 2) +
        lag(expected_cpi_inflation, 3) + lag(expected_cpi_inflation, 4) +
        lag(expected_gap, 1) + lag(expected_gap, 2) +
        lag(expected_gap, 3) + lag(expected_gap, 4),
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
  
  coefs_cpi_inflation <-
    bind_rows(coefs_cpi_inflation, output |> slice(2))
  
  #coefs_HAWK <-
  # bind_rows(coefs_HAWK, output |> slice(3))
  
  coefs_gap <-
    bind_rows(coefs_gap, output |> slice(4))
  
  
  coefs_HAWK_cpi_inflation <-
    bind_rows(coefs_HAWK_cpi_inflation, output |> slice(5))
  
  coefs_HAWK_gap <-
    bind_rows(coefs_HAWK_gap, output |> slice(6))
  
  r_squares_short <- c(r_squares_short, summary(reg, vcov. = vcovHAC(reg))$r.squared)
  
  
  predicted_i <-
    tibble(
      horizon = i,
      fitted = reg$fitted.values,
      quarter = full_dataset_ts$year_quarter[reg$fitted.values |> names() |> as.numeric()]
    )
  
  predicted_short_tbl <- bind_rows(predicted_short_tbl, predicted_i)
  
  hausman_list_short <-
    c(hausman_list_short,
      summary(reg, vcov. = vcovHAC(reg))$diagnostics[4, 3])
  
}






### Saving and transforming coefficients -----




coefs_cpi_inflation <-
  coefs_cpi_inflation |> mutate(quarter = row_number() - 1)
coefs_HAWK_cpi_inflation <-
  coefs_HAWK_cpi_inflation |> mutate(quarter = row_number() - 1)

coefs_gap <-
  coefs_gap |> mutate(quarter = row_number() - 1)
coefs_HAWK_gap <-
  coefs_HAWK_gap |> mutate(quarter = row_number() - 1)

r_squares_short_tbl <- tibble(r_squares = r_squares_short,
                              horizon = 1:length(r_squares_short) - 1)



hausman_short_tbl <- tibble(hausman = hausman_list_short,
                            horizon = 1:length(hausman_list_short) - 1)
save(
  coefs_cpi_inflation,
  coefs_HAWK_cpi_inflation,
  coefs_gap,
  coefs_HAWK_gap,
  r_squares_short_tbl,
  hausman_short_tbl,
  file = "data/intermediate_data/coefs_short.RData"
)

## LP-IV coefficient plots -----

average_inflation_responce_plot <-
  ggplot(coefs_cpi_inflation, aes(x = quarter, y = estimate)) +
  geom_hline(aes(yintercept = 0), color = "darkred") +
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
  scale_x_continuous("Horizon [1Q]") +
  scale_y_continuous("Percentage Points") +
  theme_light()


average_inflation_responce_plot

differential_inflation_responce_plot <-
  ggplot(coefs_HAWK_cpi_inflation, aes(x = quarter, y = 2 / 12 * estimate)) +  geom_ribbon(
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
  scale_x_continuous("Horizon [1Q]") +
  scale_y_continuous("Percentage Points") +
  geom_hline(aes(yintercept = 0), color = "darkred") +  geom_line() +
  theme_light()

differential_inflation_responce_plot




average_gap_responce_plot <-
  ggplot(coefs_gap, aes(x = quarter, y = estimate)) +
  geom_hline(aes(yintercept = 0), color = "darkred") +
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
  scale_x_continuous("Horizon [1Q]") +
  scale_y_continuous("Percentage Points") +
  theme_light()

average_gap_responce_plot




differential_gap_responce_plot <-
  ggplot(coefs_HAWK_gap, aes(x = quarter, y =  2 / 12 * estimate)) +
  geom_hline(aes(yintercept = 0), color = "darkred") +
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
  scale_x_continuous("Horizon [1Q]") +
  scale_y_continuous("Percentage Points") +
  theme_light()

differential_gap_responce_plot





#size_persistence_tbl |> tsibble() |> autoplot(size)
#size_persistence_tbl |> tsibble() |> autoplot(persistence)

### Saving coefficient plots -----

ggsave(
  "average_cpi_inflation_short.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  average_inflation_responce_plot,
  width = 148.5 / 2 * 1.5,
  height =  210 / 4 * 1.5,
  units = "mm"
)

ggsave(
  "differential_cpi_inflation_short.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  differential_inflation_responce_plot,
  width = 148.5 / 2 * 1.5 ,
  height = 210 / 4 * 1.5 ,
  units = "mm"
)


ggsave(
  "average_gap_short.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  average_gap_responce_plot,
  width = 148.5 / 2 * 1.5,
  height =  210 / 4 * 1.5,
  units = "mm"
)

ggsave(
  "differential_gap_short.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  differential_gap_responce_plot,
  width = 148.5 / 2 * 1.5 ,
  height = 210 / 4 * 1.5 ,
  units = "mm"
)




## LP-IV coefficient tables -----



LP_0 <-
  ivreg(
    lead(dR, 0) ~
      expected_cpi_inflation * demeaned_HAWK +  expected_gap * demeaned_HAWK +
      lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_cpi_inflation, 1) + lag(expected_cpi_inflation, 2) +
      lag(expected_cpi_inflation, 3)  + lag(expected_cpi_inflation, 4) +
      lag(expected_gap, 1) + lag(expected_gap, 2) +
      lag(expected_gap, 3) + lag(expected_gap, 4) |
      expected_cpi_inflation * demeaned_HAWK_IV + expected_gap * demeaned_HAWK_IV +
      lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_cpi_inflation, 1) + lag(expected_cpi_inflation, 2) +
      lag(expected_cpi_inflation, 3) + lag(expected_cpi_inflation, 4) +
      lag(expected_gap, 1) + lag(expected_gap, 2) +
      lag(expected_gap, 3) + lag(expected_gap, 4),
    data = full_dataset_ts
  )

LP_2 <-
  ivreg(
    lead(dR, 2) ~
      expected_cpi_inflation * demeaned_HAWK +  expected_gap * demeaned_HAWK +
      lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_cpi_inflation, 1) + lag(expected_cpi_inflation, 2) +
      lag(expected_cpi_inflation, 3)  + lag(expected_cpi_inflation, 4) +
      lag(expected_gap, 1) + lag(expected_gap, 2) +
      lag(expected_gap, 3) + lag(expected_gap, 4) |
      expected_cpi_inflation * demeaned_HAWK_IV + expected_gap * demeaned_HAWK_IV +
      lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_cpi_inflation, 1) + lag(expected_cpi_inflation, 2) +
      lag(expected_cpi_inflation, 3) + lag(expected_cpi_inflation, 4) +
      lag(expected_gap, 1) + lag(expected_gap, 2) +
      lag(expected_gap, 3) + lag(expected_gap, 4),
    data = full_dataset_ts
  )


LP_4 <-
  ivreg(
    lead(dR, 4) ~
      expected_cpi_inflation * demeaned_HAWK +  expected_gap * demeaned_HAWK +
      lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_cpi_inflation, 1) + lag(expected_cpi_inflation, 2) +
      lag(expected_cpi_inflation, 3)  + lag(expected_cpi_inflation, 4) +
      lag(expected_gap, 1) + lag(expected_gap, 2) +
      lag(expected_gap, 3) + lag(expected_gap, 4) |
      expected_cpi_inflation * demeaned_HAWK_IV + expected_gap * demeaned_HAWK_IV +
      lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_cpi_inflation, 1) + lag(expected_cpi_inflation, 2) +
      lag(expected_cpi_inflation, 3) + lag(expected_cpi_inflation, 4) +
      lag(expected_gap, 1) + lag(expected_gap, 2) +
      lag(expected_gap, 3) + lag(expected_gap, 4),
    data = full_dataset_ts
  )


LP_6 <-
  ivreg(
    lead(dR, 6) ~
      expected_cpi_inflation * demeaned_HAWK +  expected_gap * demeaned_HAWK +
      lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_cpi_inflation, 1) + lag(expected_cpi_inflation, 2) +
      lag(expected_cpi_inflation, 3)  + lag(expected_cpi_inflation, 4) +
      lag(expected_gap, 1) + lag(expected_gap, 2) +
      lag(expected_gap, 3) + lag(expected_gap, 4) |
      expected_cpi_inflation * demeaned_HAWK_IV + expected_gap * demeaned_HAWK_IV +
      lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_cpi_inflation, 1) + lag(expected_cpi_inflation, 2) +
      lag(expected_cpi_inflation, 3) + lag(expected_cpi_inflation, 4) +
      lag(expected_gap, 1) + lag(expected_gap, 2) +
      lag(expected_gap, 3) + lag(expected_gap, 4),
    data = full_dataset_ts
  )

LP_8 <-
  ivreg(
    lead(dR, 8) ~
      expected_cpi_inflation * demeaned_HAWK +  expected_gap * demeaned_HAWK +
      lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_cpi_inflation, 1) + lag(expected_cpi_inflation, 2) +
      lag(expected_cpi_inflation, 3)  + lag(expected_cpi_inflation, 4) +
      lag(expected_gap, 1) + lag(expected_gap, 2) +
      lag(expected_gap, 3) + lag(expected_gap, 4) |
      expected_cpi_inflation * demeaned_HAWK_IV + expected_gap * demeaned_HAWK_IV +
      lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_cpi_inflation, 1) + lag(expected_cpi_inflation, 2) +
      lag(expected_cpi_inflation, 3) + lag(expected_cpi_inflation, 4) +
      lag(expected_gap, 1) + lag(expected_gap, 2) +
      lag(expected_gap, 3) + lag(expected_gap, 4),
    data = full_dataset_ts
  )

LP_10 <-
  ivreg(
    lead(dR, 10) ~
      expected_cpi_inflation * demeaned_HAWK +  expected_gap * demeaned_HAWK +
      lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_cpi_inflation, 1) + lag(expected_cpi_inflation, 2) +
      lag(expected_cpi_inflation, 3)  + lag(expected_cpi_inflation, 4) +
      lag(expected_gap, 1) + lag(expected_gap, 2) +
      lag(expected_gap, 3) + lag(expected_gap, 4) |
      expected_cpi_inflation * demeaned_HAWK_IV + expected_gap * demeaned_HAWK_IV +
      lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_cpi_inflation, 1) + lag(expected_cpi_inflation, 2) +
      lag(expected_cpi_inflation, 3) + lag(expected_cpi_inflation, 4) +
      lag(expected_gap, 1) + lag(expected_gap, 2) +
      lag(expected_gap, 3) + lag(expected_gap, 4),
    data = full_dataset_ts
  )


LP_12 <-
  ivreg(
    lead(dR, 12) ~
      expected_cpi_inflation * demeaned_HAWK +  expected_gap * demeaned_HAWK +
      lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_cpi_inflation, 1) + lag(expected_cpi_inflation, 2) +
      lag(expected_cpi_inflation, 3)  + lag(expected_cpi_inflation, 4) +
      lag(expected_gap, 1) + lag(expected_gap, 2) +
      lag(expected_gap, 3) + lag(expected_gap, 4) |
      expected_cpi_inflation * demeaned_HAWK_IV + expected_gap * demeaned_HAWK_IV +
      lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_cpi_inflation, 1) + lag(expected_cpi_inflation, 2) +
      lag(expected_cpi_inflation, 3) + lag(expected_cpi_inflation, 4) +
      lag(expected_gap, 1) + lag(expected_gap, 2) +
      lag(expected_gap, 3) + lag(expected_gap, 4),
    data = full_dataset_ts
  )

fs_1 <-
  lm(
    demeaned_HAWK ~
      expected_cpi_inflation * demeaned_HAWK_IV + expected_gap * demeaned_HAWK_IV +
      lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_cpi_inflation, 1) + lag(expected_cpi_inflation, 2) +
      lag(expected_cpi_inflation, 3) + lag(expected_cpi_inflation, 4) +
      lag(expected_gap, 1) + lag(expected_gap, 2) +
      lag(expected_gap, 3) + lag(expected_gap, 4),
    data = full_dataset_ts
  )


fs_2 <-
  lm(
    I(demeaned_HAWK * expected_cpi_inflation) ~
      expected_cpi_inflation * demeaned_HAWK_IV + expected_gap * demeaned_HAWK_IV +
      lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_cpi_inflation, 1) + lag(expected_cpi_inflation, 2) +
      lag(expected_cpi_inflation, 3) + lag(expected_cpi_inflation, 4) +
      lag(expected_gap, 1) + lag(expected_gap, 2) +
      lag(expected_gap, 3) + lag(expected_gap, 4),
    data = full_dataset_ts
  )



fs_3 <-
  lm(
    I(demeaned_HAWK * expected_unemployment_gap) ~
      expected_cpi_inflation * demeaned_HAWK_IV + expected_gap * demeaned_HAWK_IV +
      lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_cpi_inflation, 1) + lag(expected_cpi_inflation, 2) +
      lag(expected_cpi_inflation, 3) + lag(expected_cpi_inflation, 4) +
      lag(expected_gap, 1) + lag(expected_gap, 2) +
      lag(expected_gap, 3) + lag(expected_gap, 4),
    data = full_dataset_ts
  )



stargazer(
  LP_0,
  LP_2,
  LP_4,
  LP_6,
  LP_8,
  LP_10,
  LP_12,
  se =
    list(
      summary(LP_0, vcov = vcovHAC(LP_0))$coef[, 2],
      summary(LP_2, vcov = vcovHAC(LP_2))$coef[, 2],
      summary(LP_4, vcov = vcovHAC(LP_4))$coef[, 2],
      summary(LP_6, vcov = vcovHAC(LP_6))$coef[, 2],
      summary(LP_8, vcov = vcovHAC(LP_8))$coef[, 2],
      summary(LP_10, vcov = vcovHAC(LP_10))$coef[, 2],
      summary(LP_12, vcov = vcovHAC(LP_12))$coef[, 2]
    ),
  df = F
)

stargazer(
  fs_1,
  fs_2,
  fs_3,
  se = list(
    summary(fs_1, vcov = vcovHAC(fs_1, weights = weightsLumley))$coef[, 2],
    summary(fs_2, vcov = vcovHAC(fs_2, weights = weightsLumley))$coef[, 2],
    summary(fs_3, vcov = vcovHAC(fs_3, weights = weightsLumley))$coef[, 2]
  ),
  df = F,
  align = T
)


LP_0 |> summary(diagnostics = T, vcov = vcovHAC(LP_0))
LP_2 |> summary(diagnostics = T, vcov = vcovHAC(LP_2))
LP_4 |> summary(diagnostics = T, vcov = vcovHAC(LP_4))
LP_6 |> summary(diagnostics = T, vcov = vcovHAC(LP_6))
LP_8 |> summary(diagnostics = T, vcov = vcovHAC(LP_8))
LP_10 |> summary(diagnostics = T, vcov = vcovHAC(LP_10))
LP_12 |> summary(diagnostics = T, vcov = vcovHAC(LP_12))




## Predictive plots  ------
##



predicted_paths_short <-
  ggplot(
    predicted_short_tbl,
    aes(
      x = horizon,
      y = fitted / 100,
      group = quarter,
      color = yq(quarter),
      label = yearquarter(yq(quarter))
    )
  ) +
  geom_line() +
  labs(color = "") +
  scale_y_continuous("Predicted FFR", labels = label_percent(), n.breaks = 8) +
  scale_x_continuous("Horizon [1Q]") +
  geom_hline(aes(yintercept = 0), color = "darkred") +
  theme_light()


ggsave(
  "predicted_paths_short.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  predicted_paths_short,
  height =  148.5 / 1.5,
  width = 210 / 1.5,
  units = "mm"
)



size_persistence_short_tbl <-
  predicted_short_tbl |>
  filter(horizon <= 12) |>
  group_by(quarter) |>
  summarize(size =  mean(fitted, na.rm = T),
            persistence = lm(I(log(fitted / fitted[2])) ~  horizon)$coef[2] |>
              exp(), 
            persistence_1= ar(fitted, order.max = 1)$ar[1])




actual_size_persistence_short <-
  ggplot(
    size_persistence_short_tbl|> filter(persistence<3),
    aes(
      x = size / 100,
      y = persistence,
      color = yq(quarter),
      label = quarter
    )
  ) +
  geom_hline(aes(yintercept = 1), color = "darkred") +
  geom_vline(aes(xintercept = 0), color = "darkred") +
  geom_point(size = 1.3) +
  geom_text(
    hjust = 0,
    vjust = 0,
    size = 2.2,
    check_overlap = T
  ) +
  labs(x = "Size", y = "Persistence", color = "") +
  scale_x_continuous(labels = label_percent(), n.breaks = 8) +
  scale_y_continuous(n.breaks = 8) +
  theme_light() +
  theme(legend.position = "none")



ggsave(
  "actual_size_persistence_short.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  actual_size_persistence_short,
  width = 210 / 1.7  ,
  height = 148.5 / 1.7 ,
  units = "mm"
)


library(strucchange)


sctest(LP_0)
sctest(LP_2)
sctest(LP_4)
sctest(LP_6)
sctest(LP_8)
sctest(LP_10)
sctest(LP_12, type = "supF")





size_persistence_short_tbl |> filter(size > 0, persistence > 1) |> count() /
  size_persistence_short_tbl |> count() * 100
size_persistence_short_tbl |> filter(size < 0, persistence > 1) |> count() /
  size_persistence_short_tbl |> count() * 100
size_persistence_short_tbl |> filter(size > 0, persistence < 1) |> count() /
  size_persistence_short_tbl |> count() * 100
size_persistence_short_tbl |> filter(size < 0, persistence < 1) |> count() /
  size_persistence_short_tbl |> count() * 100




size_short_plot <-
  ggplot(size_persistence_short_tbl, aes(x = yq(quarter), y = size / 100)) +
  geom_line() + theme_light() +
  scale_x_date(
    NULL,
    breaks = scales::breaks_width("4 years"),
    labels = scales::label_date("'%y")
  ) +
  scale_y_continuous("Size", labels = label_percent()) +
  geom_rect(
    data = rec_data_2,
    inherit.aes = F,
    aes(
      xmin = start,
      xmax = end,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = '#155F83FF' ,
    alpha = 0.2
  ) +
  geom_hline(aes(yintercept = 0), color = "darkred")

ggsave(
  "size_short_plot.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  size_short_plot,
  width = 210 /  1.7  ,
  height = 148.5 /  1.7 ,
  units = "mm"
)




persistence_short_plot <-
  ggplot(size_persistence_short_tbl, aes(x = yq(quarter), y = persistence)) +
  geom_line() + theme_light() +
  scale_x_date(
    NULL,
    breaks = scales::breaks_width("4 years"),
    labels = scales::label_date("'%y")
  ) +
  scale_y_continuous("Persistence", n.breaks = 10) +
  geom_rect(
    data = rec_data_2,
    inherit.aes = F,
    aes(
      xmin = start,
      xmax = end,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = '#155F83FF' ,
    alpha = 0.2
  ) +
  geom_hline(aes(yintercept = 1), color = "darkred")

ggsave(
  "persistence_short_plot.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  persistence_short_plot,
  width = 210 /  1.7  ,
  height = 148.5 /  1.7 ,
  units = "mm"
)




ggplot(size_persistence_short_tbl, aes(x = yq(quarter), y = size_1 / 100)) +
  geom_line() + theme_light() +
  scale_x_date(
    NULL,
    breaks = scales::breaks_width("4 years"),
    labels = scales::label_date("'%y")
  ) +
  scale_y_continuous("Size", labels = label_percent()) +
  geom_rect(
    data = rec_data_2,
    inherit.aes = F,
    aes(
      xmin = start,
      xmax = end,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = '#155F83FF' ,
    alpha = 0.2
  ) +
  geom_hline(aes(yintercept = 0), color = "darkred")

ggplot(size_persistence_short_tbl, aes(x = yq(quarter), y = persistence_1)) +
  geom_line() + theme_light() +
  scale_x_date(
    NULL,
    breaks = scales::breaks_width("4 years"),
    labels = scales::label_date("'%y")
  ) +
  scale_y_continuous("Size") +
  geom_rect(
    data = rec_data_2,
    inherit.aes = F,
    aes(
      xmin = start,
      xmax = end,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = '#155F83FF' ,
    alpha = 0.2
  ) +
  geom_hline(aes(yintercept = 1), color = "darkred")

