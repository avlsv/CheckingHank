






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
    "latex2exp",
    "patchwork",
    "vistime"
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



# Rates -----


rate_plot <-
  ggplot(full_dataset_ts |>
           select(fed_funds_rate, r_star, dR) |>
           na.omit(),
         aes(x = yq(year_quarter))) +
  geom_line(aes(y = fed_funds_rate / 100, color = "A")) +
  geom_line(aes(y = r_star / 100, color = "B")) +
  geom_line(aes(y = dR / 100, color = "C")) +
  geom_hline(aes(yintercept = 0), color = "darkred") +
  scale_x_date(date_breaks = "5 years", labels = label_date("'%y")) +
  scale_y_continuous(labels = label_percent()) +
  theme_light() +
  scale_color_discrete(labels = unname(TeX(
    c(
      "Fed Funds Rate, $r_t$",
      "Natural Rate of Interest, $r_t^*$",
      "Excess Rate, $r_t-r_t^*$"
    )
  ))) +
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
  theme(legend.position = "bottom") +
  labs(x = NULL, y = NULL, color = "Rate")


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


expected_inflation_plot <-
  ggplot(full_dataset_ts |>
           select(expected_inflation, expected_cpi_inflation),
         aes(x = yq(year_quarter))) +
  geom_line(aes(y = expected_inflation / 100, color = "A")) +
  geom_line(aes(y = expected_cpi_inflation / 100, color = "B")) +
  theme_light() +
  labs(x = NULL, y = NULL, color = "Tealbook Projected ... Inflation") +
  scale_x_date(date_breaks = "5 years", labels = label_date("'%y")) +
  scale_y_continuous(labels = label_percent(), n.breaks = 7) +
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
  scale_color_discrete(labels = c("Deflator", "CPI")) +  
  theme(legend.position = "bottom")



ggsave(
  "expected_inflation_plot.pdf",
  expected_inflation_plot,
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
  scale_y_continuous(labels = label_percent(), n.breaks = 7) +
  labs(x = NULL, y = NULL, color = "Tealbook Projected ...") +
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
  labs(x = NULL) +
  scale_y_continuous(TeX("Tealbook Projected CPI Inflation"), labels = label_percent()) +
  scale_x_date(date_breaks = "5 years", labels = label_date("'%y")) +
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
  geom_hline(aes(yintercept = 0), color = "darkred") +
  scale_x_date(date_breaks = "5 years", labels = label_date("'%y")) +
  scale_y_continuous(labels = label_percent()) +
  theme_light() +
  labs(x = NULL, y = TeX("Tealbook Projected GDP Gap, $x$")) +
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


fedhead_start_end_hawk <- fedhead_start_end
fedhead_start_end_hawk$start[1] <- yq("1968 Q1 ")
fedhead_start_end_hawk$end[length(fedhead_start_end_hawk$end)] <- yq("2020 Q4")
fedhead_start_end_hawk$fed_head_1[5] <- "Post-B"

HAWK_plot <-
  ggplot(
    full_dataset_ts |>
      select(HAWK, HAWK_IV) |>
      na.omit() |>
      as_tibble() |>
      pivot_longer(!year_quarter) |>
      mutate(name = str_replace(name, "_", " ")),
    aes(
      x = yq(year_quarter),
      color = name,
      y = value
    )
  ) +
  geom_line(aes()) +
  theme_light() +
  labs(x = NULL, y = NULL, color = "Index:") +
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
  facet_wrap(
    ~ fct_rev(name),
    ncol = 1,
    strip.position = "right",
    scales = "free_y"
  ) +
  scale_x_date(date_breaks = "4 years", labels = label_date("'%y")) +
  theme(legend.position = "none")


fedhead_start_end<-
  read_csv("data/Intermediate_Data/fedhead_start_end.csv")


hawk_heads <-
  gg_vistime(fedhead_start_end_hawk,
             col.event = "fed_head_1",
             optimize_y = T) + scale_x_datetime(NULL, date_breaks = "4 years", labels = label_date("'%y"))


HAWK_plot_w_heads <-
  HAWK_plot + hawk_heads + plot_layout(ncol = 1, heights = c(0.94, .06))




ggsave(
  "HAWK_plot.pdf",
  HAWK_plot,
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  width = 210 / 1.5  ,
  height = 148.5 / 1.5 ,
  units = "mm"
)




ggsave(
  "HAWK_plot_w_heads.pdf",
  HAWK_plot_w_heads,
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  width = 210 / 1.4  ,
  height = 148.5 / 1.4 ,
  units = "mm"
)
