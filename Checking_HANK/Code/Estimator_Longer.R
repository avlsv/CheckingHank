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
    "car",
    "scales"
  )


for (Package in required_Packages_Install) {
  if (!require(Package, character.only = TRUE)) {
    install.packages(Package, dependencies = TRUE)
  }
  
  library(Package, character.only = TRUE)
}

# Datasets ----

full_dataset_tbl <- read_csv("data/intermediate_data/full_dataset.csv")
full_dataset_ts <-
  full_dataset_tbl |> mutate(year_quarter = yearquarter(year_quarter)) |> tsibble()


# LP-IV ----

coefs_inflation <- tibble()
coefs_HAWK_inflation <- tibble()
coefs_unemployment <- tibble()
coefs_HAWK_unemployment <- tibble()
predicted_long_tbl <- tibble()
r_squares_long <- c()
hausman_list_long <- c()
#coefs_intercept <- tibble()
#coefs_HAWK <- tibble()
for (i in 0:20) {
  reg <-
    ivreg(
      lead(dR, i) ~
        expected_inflation * demeaned_HAWK +  expected_unemployment_gap * demeaned_HAWK +
        lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
        lag(expected_inflation, 1) + lag(expected_inflation, 2) +
        lag(expected_inflation, 3) + lag(expected_inflation, 4) +
        lag(expected_unemployment_gap, 1) + lag(expected_unemployment_gap, 2) +
        lag(expected_unemployment_gap, 3) + lag(expected_unemployment_gap, 4) |
        expected_inflation * demeaned_HAWK_IV + expected_unemployment_gap * demeaned_HAWK_IV +
        lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
        lag(expected_inflation, 1) + lag(expected_inflation, 2) +
        lag(expected_inflation, 3) + lag(expected_inflation, 4) +
        lag(expected_unemployment_gap, 1) + lag(expected_unemployment_gap, 2) +
        lag(expected_unemployment_gap, 3) + lag(expected_unemployment_gap, 4),
      data = full_dataset_ts
    )
  
  output <-
    summary(reg, vcov. = vcovHAC(reg, weights = weightsLumley))$coefficients |> as_tibble() |>
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
  
  
  
  r_squares_long <- c(r_squares_long, summary(reg, vcov. = vcovHAC(reg))$r.squared)
  
  
  predicted_i <-
    tibble(
      horizon = i,
      fitted = reg$fitted.values,
      quarter = full_dataset_ts$year_quarter[reg$fitted.values |> names() |> as.numeric()]
    )
  
  predicted_long_tbl <- bind_rows(predicted_long_tbl, predicted_i)
  
  hausman_list_long <- c(hausman_list_long, summary(reg)$diagnostics[4, 3])
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
  coefs_inflation |> mutate(quarter = row_number() - 1)

coefs_HAWK_inflation <-
  coefs_HAWK_inflation |> mutate(quarter = row_number() - 1)

coefs_unemployment <-
  coefs_unemployment |> mutate(quarter = row_number() - 1)
coefs_HAWK_unemployment <-
  coefs_HAWK_unemployment |> mutate(quarter = row_number() - 1)

r_squares_long_tbl <-
  tibble(r_squares = r_squares_long,
         horizon = 1:length(r_squares_long) - 1)


hausman_long_tbl <- tibble(hausman = hausman_list_long,
                           horizon = 1:length(hausman_list_long) - 1)


save(
  coefs_inflation,
  coefs_HAWK_inflation,
  coefs_unemployment,
  coefs_HAWK_unemployment,
  r_squares_long_tbl,
  hausman_long_tbl,
  file = "data/intermediate_data/coefs_long.RData"
)




## LP-IV coefficient plots -----

average_inflation_responce_plot <-
  ggplot(coefs_inflation, aes(x = quarter, y = estimate)) +
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
  scale_x_continuous("Horizon [1Q]") +
  scale_y_continuous("Percentage Points") +
  geom_hline(aes(yintercept = 0), color = "darkred") +  geom_line() +
  theme_light()

differential_inflation_responce_plot




average_unemployment_responce_plot <-
  ggplot(coefs_unemployment, aes(x = quarter, y = -estimate)) +
  geom_hline(aes(yintercept = 0), color = "darkred") +
  geom_ribbon(
    aes(
      ymin = -estimate - qnorm(1 - 0.05 / 2) * std_error,
      ymax = -estimate + qnorm(1 - 0.05 / 2) * std_error
    ),
    alpha = 0.13,
    linetype = 0,
    fill = "#477998"
  ) +
  geom_ribbon(
    aes(
      ymin = -estimate - std_error,
      ymax = -estimate + std_error
    ),
    alpha = 0.13,
    linetype = 0,
    fill = "#477998"
  ) +
  geom_ribbon(
    aes(
      ymin = -estimate - qnorm(1 - .10 / 2) * std_error,
      ymax = -estimate + qnorm(1 - .10 / 2) * std_error
    ),
    alpha = 0.123,
    linetype = 0,
    fill = "#477998"
  ) +  geom_line() +
  scale_x_continuous("Horizon [1Q]") +
  scale_y_continuous("Percentage Points") +
  theme_light()

average_unemployment_responce_plot




differential_unemployment_responce_plot <-
  ggplot(coefs_HAWK_unemployment, aes(x = quarter, y =  -2 / 12 * estimate)) +
  geom_hline(aes(yintercept = 0), color = "darkred") +
  geom_ribbon(
    aes(
      ymin =
        -2 / 12 * estimate -
        2 / 12 * qnorm(1 - 0.05 / 2) * std_error,
      ymax =
        -2 / 12 * estimate +
        2 / 12 * qnorm(1 - 0.05 / 2) * std_error
    ),
    alpha = 0.13,
    linetype = 0,
    fill = "#477998"
  ) +
  geom_ribbon(
    aes(
      ymin =
        -2 / 12 * estimate -
        2 / 12 * std_error,
      ymax =
        -2 / 12 * estimate +
        2 / 12 * std_error
    ),
    alpha = 0.13,
    linetype = 0,
    fill = "#477998"
  ) +
  geom_ribbon(
    aes(
      ymin =
        -2 / 12 * estimate -
        2 / 12 * qnorm(1 - .10 / 2) * std_error,
      ymax =
        -2 / 12 * estimate +
        2 / 12 * qnorm(1 - .10 / 2) * std_error
    ),
    alpha = 0.123,
    linetype = 0,
    fill = "#477998"
  ) +  geom_line() +
  scale_x_continuous("Horizon [1Q]") +
  scale_y_continuous("Percentage Points") +
  theme_light()

differential_unemployment_responce_plot


### Saving plots -----

ggsave(
  "average_deflator_inflation_long.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  average_inflation_responce_plot,
  width = 148.5 / 2 * 1.5,
  height =  210 / 4 * 1.5,
  units = "mm"
)

ggsave(
  "differential_deflator_inflation_long.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  differential_inflation_responce_plot,
  width = 148.5 / 2 * 1.5 ,
  height = 210 / 4 * 1.5 ,
  units = "mm"
)


ggsave(
  "average_unemployment_long.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  average_unemployment_responce_plot,
  width = 148.5 / 2 * 1.5,
  height =  210 / 4 * 1.5,
  units = "mm"
)

ggsave(
  "differential_unemployment_long.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  differential_unemployment_responce_plot,
  width = 148.5 / 2 * 1.5 ,
  height = 210 / 4 * 1.5 ,
  units = "mm"
)




## LP-IV coefficient tables -----



LP_0 <-
  ivreg(
    lead(dR, 0) ~
      expected_inflation * demeaned_HAWK + expected_unemployment_gap * demeaned_HAWK +
      lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_inflation, 1) + lag(expected_inflation, 2) +
      lag(expected_inflation, 3) + lag(expected_inflation, 4) +
      lag(expected_unemployment_gap, 1) + lag(expected_unemployment_gap, 2) +
      lag(expected_unemployment_gap, 3) + lag(expected_unemployment_gap, 4) |
      expected_inflation * demeaned_HAWK_IV + expected_unemployment_gap * demeaned_HAWK_IV +
      lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_inflation, 1) + lag(expected_inflation, 2) +
      lag(expected_inflation, 3) + lag(expected_inflation, 4) +
      lag(expected_unemployment_gap, 1) + lag(expected_unemployment_gap, 2) +
      lag(expected_unemployment_gap, 3) + lag(expected_unemployment_gap, 4),
    data = full_dataset_ts
  )


LP_2 <-
  ivreg(
    lead(dR, 2) ~
      expected_inflation * demeaned_HAWK + expected_unemployment_gap * demeaned_HAWK +
      lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_inflation, 1) + lag(expected_inflation, 2) +
      lag(expected_inflation, 3) + lag(expected_inflation, 4) +
      lag(expected_unemployment_gap, 1) + lag(expected_unemployment_gap, 2) +
      lag(expected_unemployment_gap, 3) + lag(expected_unemployment_gap, 4) |
      expected_inflation * demeaned_HAWK_IV + expected_unemployment_gap * demeaned_HAWK_IV +
      lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_inflation, 1) + lag(expected_inflation, 2) +
      lag(expected_inflation, 3) + lag(expected_inflation, 4) +
      lag(expected_unemployment_gap, 1) + lag(expected_unemployment_gap, 2) +
      lag(expected_unemployment_gap, 3) + lag(expected_unemployment_gap, 4),
    data = full_dataset_ts
  )


LP_4 <-
  ivreg(
    lead(dR, 4) ~
      expected_inflation * demeaned_HAWK + expected_unemployment_gap * demeaned_HAWK +
      lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_inflation, 1) + lag(expected_inflation, 2) +
      lag(expected_inflation, 3) + lag(expected_inflation, 4) +
      lag(expected_unemployment_gap, 1) + lag(expected_unemployment_gap, 2) +
      lag(expected_unemployment_gap, 3) + lag(expected_unemployment_gap, 4) |
      expected_inflation * demeaned_HAWK_IV + expected_unemployment_gap * demeaned_HAWK_IV +
      lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_inflation, 1) + lag(expected_inflation, 2) +
      lag(expected_inflation, 3) + lag(expected_inflation, 4) +
      lag(expected_unemployment_gap, 1) + lag(expected_unemployment_gap, 2) +
      lag(expected_unemployment_gap, 3) + lag(expected_unemployment_gap, 4),
    data = full_dataset_ts
  )


LP_6 <-
  ivreg(
    lead(dR, 6) ~
      expected_inflation * demeaned_HAWK + expected_unemployment_gap * demeaned_HAWK +
      lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_inflation, 1) + lag(expected_inflation, 2) +
      lag(expected_inflation, 3) + lag(expected_inflation, 4) +
      lag(expected_unemployment_gap, 1) + lag(expected_unemployment_gap, 2) +
      lag(expected_unemployment_gap, 3) + lag(expected_unemployment_gap, 4) |
      expected_inflation * demeaned_HAWK_IV + expected_unemployment_gap * demeaned_HAWK_IV +
      lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_inflation, 1) + lag(expected_inflation, 2) +
      lag(expected_inflation, 3) + lag(expected_inflation, 4) +
      lag(expected_unemployment_gap, 1) + lag(expected_unemployment_gap, 2) +
      lag(expected_unemployment_gap, 3) + lag(expected_unemployment_gap, 4),
    data = full_dataset_ts
  )

LP_8 <-
  ivreg(
    lead(dR, 8) ~
      expected_inflation * demeaned_HAWK + expected_unemployment_gap * demeaned_HAWK +
      lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_inflation, 1) + lag(expected_inflation, 2) +
      lag(expected_inflation, 3) + lag(expected_inflation, 4) +
      lag(expected_unemployment_gap, 1) + lag(expected_unemployment_gap, 2) +
      lag(expected_unemployment_gap, 3) + lag(expected_unemployment_gap, 4) |
      expected_inflation * demeaned_HAWK_IV + expected_unemployment_gap * demeaned_HAWK_IV +
      lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_inflation, 1) + lag(expected_inflation, 2) +
      lag(expected_inflation, 3) + lag(expected_inflation, 4) +
      lag(expected_unemployment_gap, 1) + lag(expected_unemployment_gap, 2) +
      lag(expected_unemployment_gap, 3) + lag(expected_unemployment_gap, 4),
    data = full_dataset_ts
  )

LP_10 <-
  ivreg(
    lead(dR, 10) ~
      expected_inflation * demeaned_HAWK + expected_unemployment_gap * demeaned_HAWK +
      lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_inflation, 1) + lag(expected_inflation, 2) +
      lag(expected_inflation, 3) + lag(expected_inflation, 4) +
      lag(expected_unemployment_gap, 1) + lag(expected_unemployment_gap, 2) +
      lag(expected_unemployment_gap, 3) + lag(expected_unemployment_gap, 4) |
      expected_inflation * demeaned_HAWK_IV + expected_unemployment_gap * demeaned_HAWK_IV +
      lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_inflation, 1) + lag(expected_inflation, 2) +
      lag(expected_inflation, 3) + lag(expected_inflation, 4) +
      lag(expected_unemployment_gap, 1) + lag(expected_unemployment_gap, 2) +
      lag(expected_unemployment_gap, 3) + lag(expected_unemployment_gap, 4),
    data = full_dataset_ts
  )


LP_12 <-
  ivreg(
    lead(dR, 12) ~
      expected_inflation * demeaned_HAWK + expected_unemployment_gap * demeaned_HAWK +
      lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_inflation, 1) + lag(expected_inflation, 2) +
      lag(expected_inflation, 3) + lag(expected_inflation, 4) +
      lag(expected_unemployment_gap, 1) + lag(expected_unemployment_gap, 2) +
      lag(expected_unemployment_gap, 3) + lag(expected_unemployment_gap, 4) |
      expected_inflation * demeaned_HAWK_IV + expected_unemployment_gap * demeaned_HAWK_IV +
      lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_inflation, 1) + lag(expected_inflation, 2) +
      lag(expected_inflation, 3) + lag(expected_inflation, 4) +
      lag(expected_unemployment_gap, 1) + lag(expected_unemployment_gap, 2) +
      lag(expected_unemployment_gap, 3) + lag(expected_unemployment_gap, 4),
    data = full_dataset_ts
  )

fs_1 <-
  lm(
    demeaned_HAWK ~
      expected_inflation * demeaned_HAWK_IV + expected_unemployment_gap * demeaned_HAWK_IV +
      lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_inflation, 1) + lag(expected_inflation, 2) +
      lag(expected_inflation, 3) + lag(expected_inflation, 4) +
      lag(expected_unemployment_gap, 1) + lag(expected_unemployment_gap, 2) +
      lag(expected_unemployment_gap, 3) + lag(expected_unemployment_gap, 4),
    data = full_dataset_ts
  )


fs_2 <-
  lm(
    I(demeaned_HAWK * expected_inflation) ~
      expected_inflation * demeaned_HAWK_IV + expected_unemployment_gap * demeaned_HAWK_IV +
      lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_inflation, 1) + lag(expected_inflation, 2) +
      lag(expected_inflation, 3) + lag(expected_inflation, 4) +
      lag(expected_unemployment_gap, 1) + lag(expected_unemployment_gap, 2) +
      lag(expected_unemployment_gap, 3) + lag(expected_unemployment_gap, 4),
    data = full_dataset_ts
  )


fs_3 <-
  lm(
    I(demeaned_HAWK * expected_unemployment_gap) ~
      expected_inflation * demeaned_HAWK_IV + expected_unemployment_gap * demeaned_HAWK_IV +
      lag(dR, 1) + lag(dR, 2) + lag(dR, 3) + lag(dR, 4) +
      lag(expected_inflation, 1) + lag(expected_inflation, 2) +
      lag(expected_inflation, 3) + lag(expected_inflation, 4) +
      lag(expected_unemployment_gap, 1) + lag(expected_unemployment_gap, 2) +
      lag(expected_unemployment_gap, 3) + lag(expected_unemployment_gap, 4),
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
      summary(LP_0, vcov = vcovHAC(LP_0, weights = weightsLumley))$coef[, 2],
      summary(LP_2, vcov = vcovHAC(LP_2, weights = weightsLumley))$coef[, 2],
      summary(LP_4, vcov = vcovHAC(LP_4, weights = weightsLumley))$coef[, 2],
      summary(LP_6, vcov = vcovHAC(LP_6, weights = weightsLumley))$coef[, 2],
      summary(LP_8, vcov = vcovHAC(LP_8, weights = weightsLumley))$coef[, 2],
      summary(LP_10, vcov = vcovHAC(LP_10, weights = weightsLumley))$coef[, 2],
      summary(LP_12, vcov = vcovHAC(LP_12, weights = weightsLumley))$coef[, 2]
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
  df = F
)



LP_0 |> summary(diagnostics = T)
LP_2 |> summary(diagnostics = T)
LP_4 |> summary(diagnostics = T)
LP_6 |> summary(diagnostics = T)
LP_8 |> summary(diagnostics = T)
LP_10 |> summary(diagnostics = T)
LP_12 |> summary(diagnostics = T)






## Predictive plots  ------
##




predicted_paths_long <-
  ggplot(
    predicted_long_tbl |>
      filter(quarter >= yearquarter("1988 Q3")),
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
  "predicted_paths_long.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  predicted_paths_long,
  width = 210 / 1.5  ,
  height = 148.5 / 1.5 ,
  units = "mm"
)




size_persistence_long_tbl <-
  predicted_long_tbl |>
  filter(horizon <= 12) |>
  group_by(quarter) |>
  summarize(size = mean(fitted , na.rm = T), 
            persistence =
              exp(lm(I(
                log(fitted / fitted[2])
              ) ~  horizon)$coef[2]))



actual_size_persistence_long <-
  ggplot(
    size_persistence_long_tbl |> filter(quarter >=
                                          yearquarter("1988 Q3")),
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
  "actual_size_persistence_long.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  actual_size_persistence_long,
  width = 210 /  1.7  ,
  height = 148.5 /  1.7 ,
  units = "mm"
)



size_persistence_long_tbl |>
  filter(size > 0, persistence > 1, quarter >= yearquarter("1988 Q3")) |>
  count() /
  size_persistence_long_tbl |>
  filter(quarter >= yearquarter("1988 Q3")) |>
  count() * 100



size_persistence_long_tbl |>
  filter(size < 0, persistence > 1, quarter >= yearquarter("1988 Q3")) |>
  count() /
  size_persistence_long_tbl |>
  filter(quarter >= yearquarter("1988 Q3")) |>
  count() * 100

size_persistence_long_tbl |>
  filter(size > 0, persistence < 1, quarter >= yearquarter("1988 Q3")) |>
  count() /
  size_persistence_long_tbl |>
  filter(quarter >= yearquarter("1988 Q3")) |>
  count() * 100


size_persistence_long_tbl |>
  filter(size < 0, persistence < 1, quarter >= yearquarter("1988 Q3")) |>
  count() /
  size_persistence_long_tbl |>
  filter(quarter >= yearquarter("1988 Q3")) |>
  count() * 100


size_long_plot <-
  ggplot(size_persistence_long_tbl, aes(x = yq(quarter), y = size / 100)) +
  geom_line() + theme_light() +
  scale_x_date(
    NULL,
    breaks = scales::breaks_width("4 years"),
    labels = scales::label_date("'%y")
  ) +
  scale_y_continuous("Size", labels = label_percent()) +
  geom_rect(
    data = rec_data_3,
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
  "size_long_plot.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  size_long_plot,
  width = 210 /  1.7  ,
  height = 148.5 /  1.7 ,
  units = "mm"
)




persistence_long_plot <-
  ggplot(size_persistence_long_tbl, aes(x = yq(quarter), y = persistence)) +
  geom_line() + theme_light() +
  scale_x_date(
    NULL,
    breaks = scales::breaks_width("4 years"),
    labels = scales::label_date("'%y")
  ) +
  scale_y_continuous("Persistence", n.breaks = 10) +
  geom_rect(
    data = rec_data_3,
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
  "persistence_long_plot.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  persistence_long_plot,
  width = 210 /  1.7  ,
  height = 148.5 /  1.7 ,
  units = "mm"
)

