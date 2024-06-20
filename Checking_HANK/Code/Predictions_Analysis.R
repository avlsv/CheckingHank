## Checking HANK
## Alexander Vlasov
##
## Plots creation
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
    "scales",
    "viridis",
    "ggrepel",
    "patchwork",
    "latex2exp",
    "glue"
  )


for (Package in required_Packages_Install) {
  if (!require(Package, character.only = TRUE)) {
    install.packages(Package, dependencies = TRUE)
  }
  
  library(Package, character.only = TRUE)
}


# Datasets -----

full_dataset_tbl <-
  read_csv("data/Intermediate_data/full_dataset.csv")

full_dataset_ts <-
  full_dataset_tbl |>
  mutate(year_quarter = yearquarter(year_quarter)) |>
  tsibble()

fedhead_quarterly <-
  read_csv("data/Intermediate_data/fedhead_quarterly.csv") |>
  mutate(year_quarter = yearquarter(year_quarter))



predicted_short_tbl <-
  read_csv("data/Intermediate_data/predicted_short.csv") |>
  tibble() |>
  mutate(quarter = yearquarter(quarter))

predicted_long_tbl <-
  read_csv("data/Intermediate_data/predicted_long.csv") |>
  tibble() |>
  mutate(quarter = yearquarter(quarter))



load("data/intermediate_data/coefs_short.RData")
load("data/intermediate_data/coefs_long.RData")



# Predictive Plots ----



predicted_ffr_short_tbl <-
  left_join(predicted_short_tbl,
            full_dataset_ts,
            by = join_by(quarter == year_quarter)) |>
  select(horizon,
         fitted,
         quarter,
         standard_error,
         ci_lower,
         ci_upper,
         r_star) |>
  mutate(
    ffr_hat = r_star + fitted,
    ci_lower = ci_lower + r_star,
    ci_upper = ci_upper + r_star,
    model = "Short"
  ) |> select(-fitted, -r_star) |>
  relocate(ffr_hat, .after = quarter)




predicted_ffr_long_tbl <-
  left_join(predicted_long_tbl,
            full_dataset_ts,
            by = join_by(quarter == year_quarter)) |>
  select(horizon,
         fitted,
         quarter,
         standard_error,
         ci_lower,
         ci_upper,
         r_star) |>
  mutate(
    ffr_hat = r_star + fitted,
    ci_lower = ci_lower + r_star,
    ci_upper = ci_upper + r_star,
    model = "Long"
  ) |> select(-fitted, -r_star) |>
  relocate(ffr_hat, .after = quarter)



predicted_ffr_tbl <-
  bind_rows(predicted_ffr_short_tbl, predicted_ffr_long_tbl)


write_csv(predicted_ffr_tbl,
          "data/Intermediate_data/predicted_ffr.csv")



predicted_dR_tbl <-
  bind_rows(
    predicted_short_tbl |> mutate(model = "Short"),
    predicted_long_tbl |> mutate(model = "Long")
  ) |>
  mutate(date_of_check = quarter + horizon) |>
  left_join(full_dataset_ts |> select(dR, year_quarter),
            by = join_by(date_of_check == year_quarter))

r_squared_tbl_0 <-
  predicted_dR_tbl |>
  filter(quarter >= yearquarter("1988 Q3")) |>
  group_by(horizon, model) |>
  summarize(r_squared = 1 -
              sum((dR - fitted) ^ 2) /
              sum((dR - mean(dR)) ^ 2)) |>
  mutate(model = glue::glue("{model} since 1988 Q3"))

r_squared_tbl_1 <-
  predicted_dR_tbl |>
  filter(model == "Long") |>
  group_by(horizon) |>
  summarize(r_squared = 1 -
              sum((dR - fitted) ^ 2) /
              sum((dR - mean(dR)) ^ 2)) |>
  mutate(model = "Long since 1969 Q1")

r_squared_tbl_2 <-
  predicted_dR_tbl |>
  filter(model == "Long", 
         quarter < yearquarter("1988 Q3")) |>
  group_by(horizon) |>
  summarize(r_squared = 1 -
              sum((dR - fitted) ^ 2) /
              sum((dR - mean(dR)) ^ 2)) |>
  mutate(model = "Long 1969 Q1 — 1988 Q3")

r_squared_tbl <-
  bind_rows(r_squared_tbl_1, r_squared_tbl_0)


## Short Specification FFR Predictive Plot --------

predicted_ffr_paths_short <-
  ggplot(
    left_join(
      predicted_ffr_short_tbl,
      full_dataset_ts,
      by = join_by(quarter == year_quarter)
    ),
    aes(
      x = horizon / 4,
      y = ffr_hat / 100,
      group = quarter,
      color = yq(quarter),
      label = as.character(format(quarter, "'%y:q%q"))
    )
  ) +
  geom_line() +
  scale_y_continuous(TeX("Predicted FFR"),
                     labels = label_percent(),
                     n.breaks = 8) +
  scale_x_continuous("Horizon [1Y]",
                     breaks = seq(0, 20, by = 4) / 4,
                     minor_breaks = (0:20) / 4) +
  scale_colour_viridis_c("Date",
                         trans = "date",
                         end = .9,
                         n.breaks = 8) +
  geom_hline(aes(yintercept = 0), color = "darkred") +
  theme_light()

predicted_ffr_paths_short

ggsave(
  "predicted_ffr_paths_short.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  predicted_ffr_paths_short,
  height =  148.5 / 1.5,
  width = 210 / 1.5,
  units = "mm"
)




## Long Specification FFR Predictive Plot  --------

predicted_ffr_paths_long <-
  ggplot(
    predicted_ffr_long_tbl |>
      filter(quarter >= yearquarter("1988 Q3")),
    aes(
      x = horizon / 4,
      y = ffr_hat / 100,
      group = quarter,
      color = yq(quarter),
      label = yearquarter(yq(quarter))
    )
  ) +
  geom_line() +
  scale_y_continuous("Predicted FFR", labels = label_percent(), n.breaks = 8) +
  scale_x_continuous("Horizon [1Y]",
                     breaks = seq(0, 20, by = 4) / 4,
                     minor_breaks = (0:20) / 4) +
  scale_colour_viridis_c("Date",
                         trans = "date",
                         end = .9,
                         n.breaks = 8) +
  geom_hline(aes(yintercept = 0), color = "darkred") +
  theme_light() + theme(legend.position = "right")




ggsave(
  "predicted_ffr_paths_long.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  predicted_ffr_paths_long,
  width = 210 / 1.5  ,
  height = 148.5 / 1.5 ,
  units = "mm"
)

## Short Specification r-r^* Predictive Plot  --------


predicted_paths_short <-
  ggplot(
    left_join(
      predicted_short_tbl,
      full_dataset_ts,
      by = join_by(quarter == year_quarter)
    ),
    aes(
      x = horizon / 4,
      y = fitted / 100,
      group = quarter,
      color = yq(quarter),
      label = as.character(format(quarter, "'%y:q%q"))
    )
  ) +
  geom_line() +
  scale_y_continuous(TeX("Predicted $r-r^*$"),
                     labels = label_percent(),
                     n.breaks = 8) +
  scale_x_continuous("Horizon [1Y]",
                     breaks = seq(0, 20, by = 4) / 4,
                     minor_breaks = (0:20) / 4) +
  scale_colour_viridis_c("Date",
                         trans = "date",
                         end = .9,
                         n.breaks = 8) +
  geom_hline(aes(yintercept = 0), color = "darkred") +
  theme_light()

predicted_paths_short

ggsave(
  "predicted_paths_short.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  predicted_paths_short,
  height =  148.5 / 1.5,
  width = 210 / 1.5,
  units = "mm"
)


## Long Specification r-r^* Predictive Plot  --------


predicted_paths_long <-
  ggplot(
    left_join(
      predicted_long_tbl,
      full_dataset_ts,
      by = join_by(quarter == year_quarter)
    ) |>
      filter(quarter >= yearquarter("1988 Q3")),
    aes(
      x = horizon / 4,
      y = fitted / 100,
      group = quarter,
      color = yq(quarter),
      label = as.character(format(quarter, "'%y:q%q"))
    )
  ) +
  geom_line() +
  scale_y_continuous(TeX("Predicted $r-r^*$"),
                     labels = label_percent(),
                     n.breaks = 8) +
  scale_x_continuous("Horizon [1Y]",
                     breaks = seq(0, 20, by = 4) / 4,
                     minor_breaks = (0:20) / 4) +
  scale_colour_viridis_c("Date",
                         trans = "date",
                         end = .9,
                         n.breaks = 8) +
  geom_hline(aes(yintercept = 0), color = "darkred") +
  theme_light()

predicted_paths_long

ggsave(
  "predicted_paths_long.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  predicted_paths_long,
  height =  148.5 / 1.5,
  width = 210 / 1.5,
  units = "mm"
)





# Size-Persistence  -----

## Size-Persistence Calculation ----

### Size-Persistence for Short Specification -----

size_persistence_short_tbl <-
  predicted_short_tbl |>
  filter(horizon <= 12) |>
  group_by(quarter) |>
  summarize(
    size =  mean(fitted, na.rm = T),
    persistence = lm(I(log(fitted / fitted[1])) ~ -1+ horizon)$coef[1] |>
      exp()
  ) |>
  left_join(fedhead_quarterly, by = join_by(quarter == year_quarter))



### Size-Persistence for Long Specification -----



size_persistence_long_tbl <-
  predicted_long_tbl |>
  filter(horizon <= 12) |>
  group_by(quarter) |>
  summarize(size = mean(fitted , na.rm = T),
            persistence =
              exp(lm(I(
                log(fitted / fitted[2])
              ) ~ -1 +  horizon)$coef[1])) |>
  left_join(fedhead_quarterly, by = join_by(quarter == year_quarter))



size_persistence_tbl <-
  bind_rows(
    size_persistence_short_tbl |> mutate(model = "Short"),
    size_persistence_long_tbl |> mutate(model = "Long")
  ) |>
  mutate(model = as_factor(model))


size_persistence_long_tbl_restr <-
  size_persistence_long_tbl |>
  filter(quarter >= yearquarter("1988Q3"))




## Size-Persistence Plots ----



### Size-Persistence Scater Plots -----

#### Short Specification ----
actual_size_persistence_short <-
  ggplot(
    size_persistence_short_tbl,
    aes(
      x = size / 100,
      y = persistence,
      color = yq(quarter),
      label = as.character(format(quarter, "'%yq%q")),
      shape = as_factor(fed_head)
    )
  ) +
  geom_hline(aes(yintercept = 1), color = "darkred") +
  geom_vline(aes(xintercept = 0), color = "darkred") +
  geom_point(size = 1.4) +
  geom_text_repel(size = 2.1,
                  segment.size = 0.2,
                  box.padding = 0.2) +
  scale_x_continuous("Size", labels = label_percent(), n.breaks = 8) +
  scale_y_continuous("Persistence", n.breaks = 8) +
  scale_colour_viridis_c("Date", trans = "date", end = .85) +
  theme_light() +
  labs(shape = "Chairman")

actual_size_persistence_short


ggsave(
  "actual_size_persistence_short.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  actual_size_persistence_short,
  width = 210 / 1.2  ,
  height = 148.5 / 1.2 ,
  units = "mm"
)






#### Long Specification ----




actual_size_persistence_long <-
  ggplot(
    size_persistence_long_tbl_restr
    ,
    aes(
      x = size / 100,
      y = persistence,
      color = yq(quarter),
      label = as.character(format(quarter, "'%yq%q")),
      shape = as_factor(fed_head)
    )
  ) +
  geom_hline(aes(yintercept = 1), color = "darkred") +
  geom_vline(aes(xintercept = 0), color = "darkred") +
  geom_point(size = 1.4) +
  geom_text_repel(size = 2.1, segment.size = 0.2) +
  labs(shape = "Chairman", color = "Date") +
  scale_x_continuous("Size", labels = label_percent(), n.breaks = 8) +
  scale_y_continuous("Persistence", n.breaks = 8) +
  scale_colour_viridis_c(trans = "date", end = .85) +
  theme_light()

actual_size_persistence_long


ggsave(
  "actual_size_persistence_long.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  actual_size_persistence_long,
  width = 210 /  1.2  ,
  height = 148.5 /  1.2 ,
  units = "mm"
)




## Quarters ------

quarters_short <-
  size_persistence_short_tbl |>
  summarize(
    first_quarter = sum(size > 0 & persistence > 1, na.rm=T) / n(),
    second_quarter = sum(size < 0 & persistence > 1, na.rm=T) / n(),
    third_quarter = sum(size < 0 & persistence < 1, na.rm=T) / n(),
    fourth_quarter = sum(size > 0 & persistence < 1, na.rm=T) / n()
  ) |> pivot_longer(everything()) |>
  rename(quarter = name) |>
  mutate(model = "Short")

quarters_long_restr <-
  size_persistence_long_tbl_restr |>
  summarize(
    first_quarter = sum(size > 0 & persistence > 1) / n(),
    second_quarter = sum(size < 0 & persistence > 1) / n(),
    third_quarter = sum(size < 0 & persistence < 1) / n(),
    fourth_quarter = sum(size > 0 & persistence < 1) / n()
  ) |> pivot_longer(everything()) |>
  rename(quarter = name) |>
  mutate(model = "Long (1988 Q3 — 2018Q4)")



quarters_long <-
  size_persistence_long_tbl |>
  summarize(
    first_quarter = sum(size > 0 & persistence > 1, na.rm = T) / n(),
    second_quarter = sum(size < 0 & persistence > 1, na.rm = T) / n(),
    third_quarter = sum(size < 0 & persistence < 1, na.rm = T) / n(),
    fourth_quarter = sum(size > 0 & persistence < 1, na.rm = T) / n()
  ) |> pivot_longer(everything()) |>
  rename(quarter = name) |>
  mutate(model = "Long (1969 Q1 — 2018Q4)")




quarter_stats <-
  ggplot(
    bind_rows(quarters_short, quarters_long_restr) |>
      mutate(
        quarter = case_when(
          startsWith(quarter, "fi") ~ "size > 0,\n persistence > 1",
          startsWith(quarter, "se") ~ "size < 0 \n persistence > 1",
          startsWith(quarter, "th") ~ "size < 0 \n persistence < 1",
          startsWith(quarter, "fo") ~ "size > 0 \n persistence < 1"
        ),
        quarter = as_factor(quarter)
      ),
    aes(x = model, y = value, fill = quarter)
  ) +
  geom_col(position = "dodge2") +
  scale_y_continuous(
    NULL,
    labels = percent_format(),
    n.breaks = 8,
    sec.axis = sec_axis(
      transform = ~ . * 122,
      name = "Number of Quarters",
      breaks = seq(0, 60, by = 10)
    )
  ) +
  scale_fill_viridis_d(begin = .2, end = .9) +
  labs(x = "Specification", fill = "Quarter") +
  theme_light() +
  theme(legend.position = "bottom")

quarter_stats



ggsave(
  "quarter_stats.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  quarter_stats,
  width = 210 /  1.35  ,
  height = 148.5 /  1.35 ,
  units = "mm"
)




## Size-Persistence Time Plots ----








size_plot <-
  ggplot(
    size_persistence_tbl ,
    aes(
      x = yq(quarter),
      y = size / 100,
      color = model,
      group = NULL
    )
  ) +
  geom_line()  +
  scale_x_date(
    NULL,
    breaks = scales::breaks_width("4 years"),
    labels = scales::label_date("'%y")
  ) +
  scale_y_continuous("Size", labels = label_percent()) +
  geom_rect(
    data = rec_data_long,
    inherit.aes = F,
    aes(
      xmin = start,
      xmax = end,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = '#155F83FF',
    alpha = 0.2
  ) +
  geom_hline(aes(yintercept = 0), color = "darkred") +
  theme_light() +
  labs(color = "Model") +
  theme(
    legend.position = c(0.99, 0.99),
    legend.justification = c(1, 1),
    legend.frame  = element_blank()
  )

size_plot


ggsave(
  "size_plot.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  size_plot,
  width = 210 /  1.8  ,
  height = 148.5 /  1.8 ,
  units = "mm",
  device = cairo_pdf
)



persistence_plot <-
  ggplot(
    size_persistence_tbl,
    aes(
      x = yq(quarter),
      y = persistence,
      color = as_factor(model)
    )
  ) +
  geom_line() +
  geom_hline(aes(yintercept = 1), color = "darkred") +
  geom_rect(
    data = rec_data_long,
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
  scale_x_date(NULL, breaks = breaks_width("4 years"), labels = label_date("'%y")) +
  scale_y_continuous("Persistence", n.breaks = 10) +
  theme_light() +
  labs(color = "Model") +
  theme(
    legend.position = c(0.01, 0.01),
    legend.justification = c(0, 0),
    legend.frame  = element_blank()
  )

persistence_plot


ggsave(
  "persistence_plot.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  persistence_plot,
  width = 210 /  1.8  ,
  height = 148.5 /  1.8 ,
  units = "mm",
  device = cairo_pdf
)









# Additional Statistics -----

## R Squares -----

predicted_short_tbl



r_squares_plot <-
  ggplot(r_squared_tbl, aes(
    x = horizon / 4,
    y = r_squared,
    colour = fct_rev(model)
  )) +
  geom_line() +
  theme_light() +
  scale_y_continuous(TeX("$R^2$"), n.breaks = 8) +
  scale_x_continuous(
    "Horizon [1Y]",
    minor_breaks = seq(0, 20 / 4, by = 1 / 4),
    breaks = seq(0, 20 / 4, by = 4 / 4)
  ) +
  labs(color = "Specification") +
  theme(legend.position = c(0.98, 0.98),
        legend.justification = c(1, 1))



r_squares_plot

ggsave(
  "r_squares_plot.pdf",
  r_squares_plot,
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  width = 210 / 1.6  ,
  height = 148.5 / 1.6 ,
  units = "mm"
)





## Wu-Hausman Statistic -----


hausman_full <- rows_append(
  hausman_long_tbl |> mutate(specification = "Long"),
  hausman_short_tbl |> mutate(specification = "Short")
)



hausman_plot <-
  ggplot(hausman_full,
         aes(x = horizon / 4, y = hausman, colour = specification)) +
  geom_line() +
  theme_light() +
  scale_y_continuous("Wu-Hausman Statistic", n.breaks = 8) +
  scale_x_continuous("Horizon [1Y]", minor_breaks = (0:20) / 4) +
  labs(color = "Specification") +
  theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c(1, 1),
    legend.frame  = element_blank()
  ) +
  geom_ribbon(
    aes(ymin = -Inf, ymax = qchisq(1 - 0.05, df = 3) / 3),
    alpha = 0.1,
    linetype = 0,
    fill = "#477998"
  ) +
  geom_ribbon(
    aes(ymin = -Inf, ymax = qchisq(1 - 0.01, df = 3) / 3),
    alpha = 0.1,
    linetype = 0,
    fill = "#477998"
  ) +
  guides(color =
           guide_legend(override.aes =
                          list(fill = NA)))


ggsave(
  "hausman_plot.pdf",
  hausman_plot,
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  width = 210 / 1.6  ,
  height = 148.5 / 1.6 ,
  units = "mm"
)
