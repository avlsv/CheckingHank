library(tidyverse)
library(tsibble)
library(quantmod)

getSymbols(
  c(
    "DFF",
    "DGS1MO",
    "DGS3MO",
    "DGS6MO",
    "DGS1",
    "DGS2",
    "DGS3",
    "DGS5",
    "DGS7",
    "DGS10"
  ),
  src = 'FRED'
)





yield_curve <- fortify(DFF) |>
  full_join(fortify(DGS1MO)) |>
  full_join(fortify(DGS3MO)) |>
  full_join(fortify(DGS6MO)) |>
  full_join(fortify(DGS1)) |>
  full_join(fortify(DGS2)) |>
  full_join(fortify(DGS3)) |>
  full_join(fortify(DGS5)) |>
  full_join(fortify(DGS7)) |>
  full_join(fortify(DGS10)) |> as_tsibble() |>
  filter_index("1962-01-01" ~ .)

yield_curve_long_tbl <-
  yield_curve |> pivot_longer(!Index) |> mutate(
    horizon_months =
      case_when(
        name == "DFF" ~ 0,
        name == "DGS1MO" ~ 1,
        name == "DGS3MO" ~ 3,
        name == "DGS6MO" ~ 6,
        name == "DGS1" ~ 12,
        name == "DGS2" ~ 12 * 2,
        name == "DGS3" ~ 12 * 3,
        name == "DGS5" ~ 12 * 5,
        name == "DGS7" ~ 12 * 7,
        name == "DGS10" ~ 12 * 10
      )
  ) |>
  mutate(
    horizon = horizon_months / 3,
    date = Index,
    quarter = yearquarter(date),
    day_of_quarter = as.numeric(date - yq(quarter))
  ) |>
  as_tsibble(key = horizon) |>
  select(-horizon_months, -name)

yield_curve_restr <-
  yield_curve_long_tbl |>
  filter(horizon <= 20)




estimates_of_liquidity_premia <-
  full_join(
    yield_curve_long_tbl |>
      as_tibble() |>
      group_by(quarter, horizon) |>
      summarize(
        rate = mean(value, na.rm = T),
        rate_sd = sd(value, na.rm = T)
      ),
    predicted_ffr_tbl  |>
      select(quarter, ffr_hat, model, ci_lower, ci_upper, horizon) |>
      pivot_wider(
        names_from = model,
        values_from = c(ffr_hat, ci_lower, ci_upper)
      ),
    by = join_by(quarter, horizon)
  ) |>
  group_by(quarter, horizon) |>
  reframe(
    premium_short = rate - ffr_hat_Short,
    premium_long = rate - ffr_hat_Long,
    premium_short_upper = rate - ci_lower_Short,
    premium_short_lower = rate - ci_upper_Short,
    premium_long_upper = rate - ci_lower_Long,
    premium_long_lower = rate - ci_upper_Long
  ) |>
  select(starts_with("premium"), quarter, horizon) |>
  filter(if_any(starts_with("premium"), ~ !is.na(.))) |>
  pivot_longer(!c(horizon, quarter)) |>
  mutate(
    model =
      case_when(
        str_detect(name, "short") ~ "Short",
        str_detect(name, "long") ~ "Long"
      ),
    name = case_when(
      str_detect(name, "upper") ~ "CI_upper",
      str_detect(name, "lower") ~ "CI_lower",
      str_detect(name, "premium_short") ~ "prediction",
      str_detect(name, "premium_long") ~ "prediction"
    )
  ) |>
  pivot_wider(names_from = name, values_from = value) |>
  mutate(
    horizon_years =
      case_when(
        horizon / 4 >= 1 ~ glue::glue("T+{horizon/4} Years"),
        .default =
          glue::glue("T+{horizon*3} Month")
      ) |>
      as_factor() |>
      fct_relevel("T+3 Month", after = 1) |>
      fct_relevel("T+6 Month", after = 2)|>
      fct_relevel("T+2 Years", after = 4)
  )




estimates_of_liquidity_premia_plot <-
  ggplot(
    filter(
      estimates_of_liquidity_premia,
      horizon < 20,
      quarter >= yearquarter("1990")
    ),
    aes(
      x = yq(quarter),
      linetype = as_factor(model),
      group = model
    )
  ) +
  geom_line(aes(y = prediction / 100)) +
  geom_ribbon(aes(
    ymin = CI_lower / 100,
    ymax = CI_upper / 100,
    fill = as_factor(model)
  ),
  alpha = 0.3) +
  geom_rect(
    data = rec_data_long |> 
      slice_tail(n = -4),
    inherit.aes = F,
    aes(
      xmin = start,
      xmax = end,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = '#155F83FF' ,
    alpha = 0.1
  ) +
  facet_wrap(~ horizon_years, ncol=2) +
  scale_y_continuous("Predicted Liquidity Premia",
                     labels = percent_format(),
                     n.breaks = 5) +
  scale_x_date(NULL, date_breaks = "4 years", labels = label_date("'%y")) +
  labs(fill = "Model", linetype = "Model") +
  geom_hline(aes(yintercept = 0), color = "darkred") +
  theme_light() +
  theme(legend.position = "bottom")

estimates_of_liquidity_premia_plot

ggsave(
  "estimates_of_liquidity_premia_plot.pdf",
  estimates_of_liquidity_premia_plot,
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  width = 210 / 1.1  ,
  height = 148.5 / 1.1 ,
  units = "mm"
)




share_inverted <-
  yield_curve_long_tbl |>
  filter(horizon <= 20) |>
  group_by(date) |>
  summarize(coef = lm(value ~ horizon)$coef[2], inverted = coef < 0) |>
  as_tibble() |>
  mutate(year_quarter = yearquarter(date)) |>
  group_by(year_quarter) |>
  summarize(
    share_inverted = mean(inverted, na.rm = T),
    days_inverted = sum(inverted, na.rm = T)
  )



share_inverted_plot <-
  ggplot(share_inverted, aes(x = yq(year_quarter), y = share_inverted)) +
  geom_line() +
  geom_rect(
    data = filter(recession.df, yearquarter(end) >= yearquarter("1962-05-01")),
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
  scale_y_continuous("Share of Days per Quarter with Inverted Yeld Curves", labels = label_percent()) +
  scale_x_date(NULL, date_breaks = "4 years", labels = label_date("'%y")) +
  theme_light()






# Plot of Yield Curves -----

starting_quarter =  yearquarter("2010q1")
number_of_years = 3



yield_prediction_plot <-
  ggplot(yield_curve_restr |>
           filter(between(
             quarter - starting_quarter, 0 , 4 * number_of_years - 1
           ), horizon<=12)) +
  geom_point(
    alpha = 0.3,
    size = .7,
    aes(
      x = horizon / 4,
      y = value / 100,
      color = day_of_quarter,
      group = day_of_quarter
    )
  ) +
  geom_line(
    alpha = 0.3,
    linewidth = 0.3,
    aes(
      x = horizon / 4,
      y = value / 100,
      group = date,
      color = day_of_quarter
    )
  ) +
  geom_line(data = predicted_ffr_tbl |>
              filter(between(
                quarter - starting_quarter, 0 , 4 * number_of_years - 1
              ),  horizon<=12),
            aes(
              x = horizon / 4,
              y = ffr_hat / 100,
              linetype = fct_rev(model)
            )) +
  geom_ribbon(
    data = predicted_ffr_tbl |>
      filter(between(
        quarter - starting_quarter, 0 , 4 * number_of_years - 1
      ),  horizon<=12),
    aes(
      x = horizon / 4,
      ymin = ci_lower / 100,
      ymax = ci_upper / 100,
      y = ffr_hat / 100,
      fill = fct_rev(model)
    ),
    alpha = 0.3
  ) +
  scale_y_continuous("Interest Rate", labels = percent_format(), n.breaks =
                       6) +
  scale_color_viridis_c("Day in Quarter", option = "D") +
  scale_x_continuous("Horizon [1Y]", breaks = seq(0, 5, by = 1)) +
  geom_hline(aes(yintercept = 0), color = "darkred") +
  facet_wrap(~ quarter, scales = "fixed", ncol = 4) +
  labs(linetype = "Model", fill = "Model") +
  theme_light()



yield_prediction_plot

ggsave(
  "yield_prediction_plot.pdf",
  yield_prediction_plot,
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  width = 210 / 1.1  ,
  height = 148.5 / 1.1 ,
  units = "mm"
)





predicted_ffr_path_plot <-
  ggplot(
    predicted_ffr_tbl |>
      filter(between(
        quarter - starting_quarter, 0 , 4 * number_of_years - 1
      )),
    aes(
      x = horizon / 4,
      y = ffr_hat / 100,
      linetype = model,
      fill = model
    )
  ) +
  geom_line() +
  geom_ribbon(aes(
    ymin = ci_lower / 100,
    ymax = ci_upper / 100,
    fill = model
  ), alpha = 0.2) +
  scale_y_continuous("Interest Rate", labels = percent_format(), n.breaks = 6) +
  scale_color_viridis_c("Day in Quarter", option = "D") +
  scale_x_continuous("Horizon [1Y]", breaks = seq(0, 5, by = 1)) +
  geom_hline(aes(yintercept = 0), color = "darkred") +
  facet_wrap( ~ quarter, scales = "fixed", ncol = 4) +
  theme_light()



ggsave(
  "predicted_ffr_path_plot.pdf",
  predicted_ffr_path_plot,
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  width = 210 / 1.1  ,
  height = 148.5 / 1.1 ,
  units = "mm"
)
