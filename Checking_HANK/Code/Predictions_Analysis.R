## Checking HANK
## Plots creation
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
    "scales",
    "viridis",
    "ggrepel",
    "patchwork"
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

## Short Specification FFR Predictive Plot --------

predicted_ffr_paths_short <-
  ggplot(
    left_join(
      predicted_ffr_short_tbl,
      full_dataset_ts,
      by = join_by(quarter == year_quarter)
    ),
    aes(
      x = horizon,
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
  scale_x_continuous("Horizon [1Q]",
                     breaks = seq(0, 20, by = 4),
                     minor_breaks = (0:20)) +
  scale_colour_viridis_c("Date", trans = "date", end = .9) +
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
      x = horizon,
      y = ffr_hat / 100,
      group = quarter,
      color = yq(quarter),
      label = yearquarter(yq(quarter))
    )
  ) +
  geom_line() +
  scale_y_continuous("Predicted FFR", labels = label_percent(), n.breaks = 8) +
  scale_x_continuous("Horizon [1Q]",
                     breaks = seq(0, 20, by = 4),
                     minor_breaks = (0:20))+
  scale_colour_viridis_c("Date", trans = "date", end = .9) +
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

## Short Specification Deviation Predictive Plot  --------


predicted_paths_short <-
  ggplot(
    left_join(
      predicted_short_tbl,
      full_dataset_ts,
      by = join_by(quarter == year_quarter)
    ),
    aes(
      x = horizon,
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
  scale_x_continuous("Horizon [1Q]",
                     breaks = seq(0, 20, by = 4),
                     minor_breaks = (0:20))+
  scale_colour_viridis_c("Date", trans = "date", end = .9) +
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


## Long Specification Deviation Predictive Plot  --------


predicted_paths_long <-
  ggplot(
    left_join(
      predicted_long_tbl,
      full_dataset_ts,
      by = join_by(quarter == year_quarter)
    ) |>
      filter(quarter >= yearquarter("1988 Q3")),
    aes(
      x = horizon,
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
  scale_x_continuous("Horizon [1Q]",
                     breaks = seq(0, 20, by = 4),
                     minor_breaks = (0:20))+
  scale_colour_viridis_c("Date", trans = "date", end = .9) +
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
    persistence = lm(I(log(fitted / fitted[2])) ~  horizon)$coef[2] |>
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
              ) ~  horizon)$coef[2])) |>
  left_join(fedhead_quarterly, by = join_by(quarter == year_quarter))



size_persistence_tbl <-
  left_join(
    size_persistence_long_tbl,
    size_persistence_short_tbl,
    by = join_by(quarter, fed_head),
    suffix = c(".Long", ".Short")
  ) |>
  pivot_longer(!quarter & !fed_head) |>
  mutate(model = word(name, 2, sep = fixed(".")),
         type = word(name, 1, sep = fixed("."))) |>
  select(-name)


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
      shape = fed_head
    )
  ) +
  geom_hline(aes(yintercept = 1), color = "darkred") +
  geom_vline(aes(xintercept = 0), color = "darkred") +
  geom_point(size = 1.4) +
  geom_text_repel(size = 2.1,
                  segment.size = 0.2,
                  box.padding = 0.2) +
  scale_shape_manual(values = c(15, 16, 17)) +
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
      shape = fed_head
    )
  ) +
  geom_hline(aes(yintercept = 1), color = "darkred") +
  geom_vline(aes(xintercept = 0), color = "darkred") +
  geom_point(size = 1.4) +
  geom_text_repel(size = 2.1, segment.size = 0.2) +
  scale_shape_manual(values = c(15, 16, 17)) +
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
    first_quarter = sum(size > 0 & persistence > 1) / n(),
    second_quarter = sum(size < 0 & persistence > 1) / n(),
    third_quarter = sum(size < 0 & persistence < 1) / n(),
    fourth_quarter = sum(size > 0 & persistence < 1) / n()
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
    first_quarter = sum(size > 0 & persistence > 1) / n(),
    second_quarter = sum(size < 0 & persistence > 1) / n(),
    third_quarter = sum(size < 0 & persistence < 1) / n(),
    fourth_quarter = sum(size > 0 & persistence < 1) / n()
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
    size_persistence_tbl |>
      filter(type == "size", quarter >= yearquarter("1988Q3")),
    aes(
      x = yq(quarter),
      y = value / 100,
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
    data = rec_data_2,
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
  width = 210 /  1.5  ,
  height = 148.5 /  1.5 ,
  units = "mm", 
  device=cairo_pdf
)



persistence_plot <-
  ggplot(
    size_persistence_tbl |>
      filter(type == "persistence", quarter >= yearquarter("1988 Q3")),
    aes(
      x = yq(quarter),
      y = value,
      color = model
    )
  ) +
  geom_line() +
  geom_hline(aes(yintercept = 1), color = "darkred") +
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
  "persistence_short_plot.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  persistence_short_plot,
  width = 210 /  1.7  ,
  height = 148.5 /  1.7 ,
  units = "mm"
)









# Additional Statistics -----

## R Squares -----

r_squares_full <- rows_append(
  r_squares_long_tbl |> mutate(specification = "Long"),
  r_squares_short_tbl |> mutate(specification = "Short")
)




r_squares_plot <-
  ggplot(r_squares_full,
         aes(x = horizon / 4, y = r_squares, colour = specification)) +
  geom_line() +
  theme_light() +
  scale_y_continuous(TeX("$R^2$"), n.breaks = 8) +
  scale_x_continuous(
    "Horizon [1Y]",
    minor_breaks = seq(0, 20 / 4, by = 1 / 4),
    breaks = seq(0, 20 / 4, by = 4 / 4)
  ) +
  labs(color = "Specification") +
  theme(legend.position = c(0.95, 0.95),
        legend.justification = c(1, 1))



ggsave(
  "r_squares_plot.pdf",
  r_squares_plot,
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  width = 210 / 1.7  ,
  height = 148.5 / 1.7 ,
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
    alpha = 0.12,
    linetype = 0,
    fill = "#477998"
  ) +
  geom_ribbon(
    aes(ymin = -Inf, ymax = qchisq(1 - 0.01, df = 3) / 3),
    alpha = 0.12,
    linetype = 0,
    fill = "#477998"
  ) +
  guides(color = guide_legend(override.aes = list(fill = NA)))


ggsave(
  "hausman_plot.pdf",
  hausman_plot,
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  width = 210 / 1.7  ,
  height = 148.5 / 1.7 ,
  units = "mm"
)
