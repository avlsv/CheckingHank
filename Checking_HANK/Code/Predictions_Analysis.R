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
    "ggrepel"
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
  read_csv("data/Intermediate_data/fedhead_quarterly.csv")|>
  mutate(year_quarter=yearquarter(year_quarter))


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

size_persistence_short_tbl <-
  predicted_short_tbl |>
  filter(horizon <= 12) |>
  group_by(quarter) |>
  summarize(
    size =  mean(fitted, na.rm = T),
    persistence = lm(I(log(fitted / fitted[2])) ~  horizon)$coef[2] |>
      exp()
  ) |> 
  left_join(fedhead_quarterly, by=join_by(quarter == year_quarter))





# IRF Plot -----





persistence_plot <-
  size_persistence_consumption_ts |> autoplot(persistence) +
  theme_light() +
  labs(x = "", y = "Persistence") +
  geom_rect(
    data = rec_data_1,
    inherit.aes = F,
    aes(
      xmin = start,
      xmax = end,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = '#155F83FF' ,
    alpha = 0.2
  )




ggsave(
  "irfs_plot_shorter.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  irfs_shorter_plot,
  width = 220,
  height = 140,
  units = "mm"
)
ggsave(
  "irfs_plot_longer.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  irfs_longer_plot,
  width = 220,
  height = 140,
  units = "mm"
)


ggsave(
  "size_vs_persistence.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  size_vs_persistence,
  width = 220 / 1.2,
  height = 140 / 1.2,
  units = "mm"
)

ggsave(
  "size_plot.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  size_plot,
  width = 220 / 1.7,
  height = 140 / 1.7,
  units = "mm"
)


ggsave(
  "persistence_plot.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  persistence_plot,
  width = 220 / 1.7,
  height = 140 / 1.7,
  units = "mm"
)


ggsave(
  "HAWK_plot.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  HAWK_plot,
  width = 220 / 1.7,
  height = 140 / 1.7,
  units = "mm"
)






r_squares_full <- rows_append(
  r_squares_long_tbl |> mutate(specification = "Long"),
  r_squares_short_tbl |> mutate(specification = "Short")
)

hausman_full <- rows_append(
  hausman_long_tbl |> mutate(specification = "Long"),
  hausman_short_tbl |> mutate(specification = "Short")
)





r_squares_plot <-
  ggplot(r_squares_full, aes(x = horizon, y = r_squares, colour = specification)) +
  geom_line() +
  theme_light() +
  scale_y_continuous(TeX("$R^2$"), n.breaks = 8) +
  scale_x_continuous("Horizon [1Q]", minor_breaks = (0:20)) +
  labs(color = "Specification") +
  theme(legend.position.inside = c(0.95, 0.95), 
        legend.justification = c(1,1))



ggsave(
  "r_squares_plot.pdf",
  r_squares_plot,
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  width = 210 / 1.7  ,
  height = 148.5 / 1.7 ,
  units = "mm")






hausman_plot <-
  ggplot(hausman_full, aes(x = horizon, y = hausman, colour = specification)) +
  geom_line() +
  theme_light() +
  scale_y_continuous("Wu-Hausman Statistic",n.breaks = 8) +
  scale_x_continuous("Horizon [1Q]", minor_breaks = (0:20)) +
  labs(color = "Specification") +
  theme(
    legend.position = c(0.95, 0.95), 
    legend.justification = c(1,1),
    legend.frame  = element_blank()
  ) +
  geom_ribbon(
    aes(ymin = 0, ymax = qchisq(1 - 0.05, df = 3) / 3, ),
    alpha = 0.12,
    linetype = 0,
    fill = "#477998"
  ) +
  geom_ribbon(
    aes(ymin = 0, ymax = qchisq(1 - 0.01, df = 3) / 3, ),
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
  units = "mm")



