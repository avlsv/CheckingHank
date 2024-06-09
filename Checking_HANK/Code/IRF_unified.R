## Checking HANK.
## The IRF for both models
##
## Author: Alexander Vlasov
##
##




# Libraries -----
required_Packages_Install <-
  c(
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
    "multcomp",
    "tidyverse",
    "scales",
    "gghalves"
  )


for (Package in required_Packages_Install) {
  if (!require(Package, character.only = TRUE)) {
    install.packages(Package, dependencies = TRUE)
  }
  
  library(Package, character.only = TRUE)
}


# Datasets ----

load("data/Intermediate_Data/coefs_long.RData")
load("data/Intermediate_Data/coefs_short.RData")

full_dataset_tbl <- read_csv("data/Intermediate_Data/full_dataset.csv")
full_dataset_ts <-
  full_dataset_tbl |> mutate(year_quarter = yearquarter(year_quarter)) |> tsibble()

# Looking at the data -----

full_dataset_ts$delta_expected_inflation |> mean(na.rm = T)
full_dataset_ts$delta_expected_unemployment |> mean(na.rm = T)
full_dataset_ts |> autoplot(vars(expected_inflation, expected_unemployment))
full_dataset_ts |> filter_index("2007" ~ "2011") |> autoplot(vars(expected_inflation, expected_unemployment))
full_dataset_ts |> filter_index("2007" ~ "2011") |> autoplot(vars(delta_expected_inflation, delta_expected_unemployment_gap))

# Shocks of variables of interest in 2008Q4 --------
#


shock_cpi_inflation <-
  full_dataset_ts |>
  filter_index("2008Q4") |>
  as_tibble() |>
  select(delta_expected_cpi_inflation) |>
  as.numeric()


shock_gdp_gap <-
  full_dataset_ts |>
  filter_index("2008Q4") |>
  as_tibble() |>
  select(delta_expected_gap) |>
  as.numeric()


shock_deflator_inflation <-
  full_dataset_ts |>
  filter_index("2008Q4") |>
  as_tibble() |>
  select(delta_expected_inflation) |>
  as.numeric()


shock_unemployment_gap <-
  full_dataset_ts |>
  filter_index("2008Q4") |>
  as_tibble() |>
  select(delta_expected_unemployment_gap) |>
  as.numeric()







## IRFs preditions ----

irf_short_list <- c()
irf_long_list <- c()


full_dataset_mod_tbl <-
  full_dataset_tbl |>
  mutate(
    year_quarter = yearquarter(year_quarter),
    fed_head_0 = case_when(
      fed_head == "Bernanke" &
        year_quarter < yearquarter("2009 Q1") ~ "Bernanke Pre-GFC",
      fed_head == "Bernanke" &
        year_quarter >= yearquarter("2009 Q1") ~ "Bernanke Post-GFC",
      .default = fed_head
    ) |> as_factor()
  )

hawk_by_fedhead <-
  full_dataset_mod_tbl |>
  group_by(fed_head_0) |>
  summarize(HAWK = mean(demeaned_HAWK),
            HAWK_median = median(demeaned_HAWK)) |>
  rename(fed_head = fed_head_0)




hawk_by_chair_plot <-
  ggplot(
    full_dataset_mod_tbl |>
      mutate(fed_head = as_factor(fed_head)) |>
      group_by(fed_head) |>
      mutate(quarter_in_power = row_number() / n()),
    aes(
      x = fct_rev(fed_head_0),
      y = demeaned_HAWK,
      fill = fed_head_0,
      group = fed_head_0
    )
  ) +  
  geom_half_boxplot(side = "r") +
  geom_half_point_panel(side = "l",
                        alpha = .8,
                        aes(color = quarter_in_power)) +
  scale_fill_viridis_d(begin = .4) +
  scale_color_viridis_c(
    begin = .2,
    end = .9,
    option = "mako",
    breaks = 0.25 * 0:4,
    labels = percent(0.25 * 0:4)
  ) +
  scale_y_continuous(n.breaks = 8) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 20,
    size = 3,
    color = "red",
    alpha = 0.6
  ) +
  coord_flip() +
  theme_light() +
  guides(fill = "none") +
  theme(legend.position = "bottom") +
  labs(x = NULL, 
       y = "Demeaned Hawk Index", 
       color = "Time in Ofice")


hawk_by_chair_plot

ggsave(
  "hawk_by_chair_plot.pdf",
  hawk_by_chair_plot,
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  width = 210 / 1.1  ,
  height = 148.5 / 1.1 ,
  units = "mm"
)




short_combined <- tibble()

for (i in 1:dim(hawk_by_fedhead)[1]) {
  for (t in 1:21) {
    hawk <- hawk_by_fedhead$HAWK_median[i]
    
    combined <-
      glht(
        reg_list_short[[t]],
        glue::glue(
          "{shock_cpi_inflation}*expected_cpi_inflation + {shock_cpi_inflation}*{hawk}*expected_cpi_inflation:demeaned_HAWK + {shock_gdp_gap}*expected_gap+ {shock_gdp_gap}*{hawk}*demeaned_HAWK:expected_gap= 0"
        ),
        vcov = vcovHAC(reg_list_short[[t]])
      ) |>
      summary() |>
      tidy() |>
      select(-contrast, -null.value) |>
      mutate(horizon = t - 1, fed_head = hawk_by_fedhead$fed_head[i])
    
    short_combined <- bind_rows(short_combined, combined)
    
    
  }
  
}



long_combined <- tibble()

for (i in 1:dim(hawk_by_fedhead)[1]) {
  hawk <- hawk_by_fedhead$HAWK_median[i]
  
  for (t in 1:21) {
    combined <-
      glht(
        reg_list_long[[t]],
        glue::glue(
          "{shock_deflator_inflation} * expected_inflation + {shock_deflator_inflation} * {hawk} * expected_inflation:demeaned_HAWK + {shock_unemployment_gap} * expected_unemployment_gap + {shock_unemployment_gap} * {hawk} * demeaned_HAWK:expected_unemployment_gap = 0"
        ),
        vcov = vcovHAC(reg_list_long[[t]])
      ) |>
      summary() |>
      tidy() |>
      select(-contrast, -null.value) |>
      mutate(horizon = t - 1, fed_head = hawk_by_fedhead$fed_head[i])
    
    long_combined <- bind_rows(long_combined, combined)
    
    
  }
  
}



combined_irf <-
  bind_rows(short_combined |> mutate(model = "Short"),
            long_combined |> mutate(model = "Long"))









irfs_combined_plot <-
  ggplot(combined_irf,
         aes(
           x = horizon / 4,
           y = estimate,
           color = fct_rev(model),
           group = fct_rev(model)
         )) +
  geom_line() +
  geom_hline(aes(yintercept = 0), color = "darkred") +
  geom_ribbon(
    aes(
      ymin = estimate - qnorm(1 - 0.05 / 2) * std.error,
      ymax = estimate + qnorm(1 - 0.05 / 2) * std.error,
      fill = fct_rev(model)
    ),
    alpha = 0.13,
    linetype = 0,
  ) +
  facet_wrap(~ c(fed_head)) +
  theme_light() +
  labs(y = "Percentage Points", x = "Horizon [1Y]") +
  scale_x_continuous() +
  scale_y_reverse(n.breaks = 6) +
  labs(color = "Model", fill = "Model")

irfs_combined_plot



ggsave(
  "irfs_combined_plot.pdf",
  irfs_combined_plot,
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  width = 210 / 1.1  ,
  height = 148.5 / 1.1 ,
  units = "mm"
)

