



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

# Recessions -----


getSymbols('USREC', src = 'FRED')
recession <- USREC
drecession <- diff(recession)
recession.start <- time(drecession[drecession == 1])
recession.end <- time(drecession[drecession == -1])
recession.end <- recession.end
recession.df <- tibble(recession.start , recession.end[2:length(recession.end)])
colnames(recession.df) <- c("start", "end")
recession.df


rec_data_short <-
  recession.df |> filter(end >= yq("1986 Q1") , start <= yq("2018 Q4"))
rec_data_long <-
  recession.df |> filter(end >= yq("1963 Q1") , start <= yq("2018 Q4"))



width_chosen = 210 / 1.7 
height_chosen = 148.5 / 1.7

# Rates -----


rate_plot <-
  ggplot(full_dataset_ts |>
           select(fed_funds_rate, r_star, dR) |>
           na.omit(),
         aes(x = yq(year_quarter))) +
  geom_line(aes(y = fed_funds_rate / 100, color = "Fed Funds Rate")) +
  geom_line(aes(y = r_star / 100, color = "Natural Rate of Interest")) +
  geom_line(aes(y = dR / 100, color = "Excess Rate")) +
  geom_hline(aes(yintercept = 0), color = "darkred") +
  scale_x_date(date_breaks = "5 years", labels = label_date("'%y")) +
  scale_y_continuous(labels = label_percent()) +
  theme_light() +
  theme(legend.position = "bottom") +
  labs(x = "", y = "", color = "Rate")


ggsave(
  "rate_plot.pdf",
  rate_plot,
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  width = 210 / 1.7  ,
  height = 148.5 / 1.7 ,
  units = "mm"
)




# Plots of Tealbook Projections -----

## Expected Inflation Plot -----

expected_deflator_inflation_plot <-
  ggplot(
    full_dataset_ts |>
      select(expected_inflation) |>
      na.omit(),
    aes(y = expected_inflation / 100, x = yq(year_quarter))
  ) +
  geom_line() +
  theme_light() +
  labs(x = "", y = "Tealbook Projection of GDP Deflator Inflation") +
  scale_x_date(date_breaks = "5 years", labels = label_date("'%y")) +
  scale_y_continuous(labels = label_percent()) +
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
  )



ggsave(
  "expected_deflator_inflation_plot.pdf",
  expected_deflator_inflation_plot,
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  width = 210 / 1.7  ,
  height = 148.5 / 1.7 ,
  units = "mm"
)



## Expected Unemployement Plot -------

expected_unemployment_plot <-
  ggplot(
    full_dataset_ts |>
      select(
        expected_unemployment,
        expected_nairu,
        expected_unemployment_gap
      ) |> na.omit(),
    aes(x = yq(year_quarter))
  ) +
  geom_line(aes(y = expected_unemployment / 100, color = "Unemployment")) +
  geom_line(aes(y = expected_nairu / 100, color = "NAIRU")) +
  geom_line(aes(y = expected_unemployment_gap / 100, color = "Unemployment Gap")) +
  geom_hline(aes(yintercept = 0), color = "darkred") +
  theme_light() +
  theme(legend.position = "bottom") +
  scale_x_date(date_breaks = "5 years", labels = label_date("'%y")) +
  scale_y_continuous(labels = label_percent()) +
  labs(x = "", y = "", color = "Tealbook Projected ...") +
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
  )


ggsave(
  "expected_unemployment_plot.pdf",
  expected_unemployment_plot,
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  width = 210 / 1.7  ,
  height = 148.5 / 1.7 ,
  units = "mm"
)


## Expected CPI Inflation Plot -----



expected_cpi_inflation_plot <-
  ggplot(
    full_dataset_ts |>
      select(expected_cpi_inflation) |>
      na.omit(),
    aes(y = expected_cpi_inflation / 100, x = yq(year_quarter))
  ) +
  geom_line() +
  theme_light() +
  labs(x = "", y = "CPI Inflation") +
  scale_y_continuous("Expected CPI Inflation", labels = label_percent()) +
  geom_rect(
    data = rec_data_long |> filter(start > yq("1980 Q1")),
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
  "expected_cpi_inflation_plot.pdf",
  expected_cpi_inflation_plot,
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  width = 210 / 1.7  ,
  height = 148.5 / 1.7 ,
  units = "mm"
)

## Expected GDP Gap Plot -----


expected_gap_plot <-
  ggplot(full_dataset_ts |>
           select(expected_gap) |>
           na.omit(),
         aes(y = expected_gap / 100, x = yq(year_quarter))) +
  geom_line() +
  scale_x_date(date_breaks = "5 years", labels = label_date("'%y")) +
  scale_y_continuous(labels = label_percent()) +
  theme_light() +
  labs(x = "", y = "GDP Gap") +
  geom_rect(
    data = rec_data_short,
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
  "expected_gap_plot.pdf",
  expected_gap_plot,
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  width = 210 / 1.7  ,
  height = 148.5 / 1.7 ,
  units = "mm"
)


# Hawk plot --------


HAWK_plot <-
  ggplot(full_dataset_ts |>
           select(HAWK, HAWK_IV) |>
           na.omit(),
         aes(x = yq(year_quarter))) +
  geom_line(aes(y = HAWK, color = "HAWK")) +
  geom_line(aes(y = HAWK_IV, color = "HAWK IV")) +
  theme_light() +
  labs(x = "", y = "HAWK Indices", color = "Type:") +
  geom_rect(
    data = recession.df |>
      filter(start > yq("1965 Q1"), end < yq("2021 Q1")) ,
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
  scale_y_continuous(breaks = breaks_extended()) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_date(date_breaks = "4 years", labels = label_date("'%y")) +
  theme(
    legend.position = c(0.1, 0.01),
    legend.justification = c(0, 0),
    legend.frame  = element_blank()
  )


ggsave(
  "HAWK_plot.pdf",
  HAWK_plot,
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  width = 210 / 1.7  ,
  height = 148.5 / 1.7 ,
  units = "mm"
)
