
library(tidyverse)

library(scales)
library(tsibble)

size_persistence_consumption_tbl <-
  read.csv("data/size_persistence_consumption.csv") |> 
  tibble() |> 
  mutate(year_quarter = yearquarter(year_quarter))

irfs <- read.csv("data/irfs.csv") |> tibble()


size_persistence_consumption_ts <-
  size_persistence_consumption_tbl  |> tsibble()

irfs_plot <-
  ggplot(irfs, aes(
    x = quart,
    y = irf,
    color = date(time),
    group = date(time)
  )) +
  geom_hline(aes(yintercept = 0),  color = "darkred") +
  geom_line() +
  theme_light() + 
  labs(y = "Percentage Points", x = "Quarter", color = "Year-Quarter") +  
  scale_x_continuous(breaks = pretty_breaks())
irfs_plot

ggsave(
  "irfs_plot.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  irfs_plot,
  width = 220,
  height = 140,
  units = "mm"
)



size_vs_persistence <- 
  ggplot(size_persistence_consumption_tbl,
       aes(x = size, y = persistence, color = year(yq(year_quarter)))) +
  geom_point(size = 1.3) +
  labs(x = "Size", y = "Persistence", color = "Year-Quarter") +
  theme_light()


size_vs_persistence


ggsave(
  "size_vs_persistence.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  size_vs_persistence,
  width = 220,
  height = 140,
  units = "mm"
)


mean(size_persistence_consumption_tbl$size < 0)



getSymbols('USREC',  src = 'FRED')
recession <- USREC
drecession <- diff(recession)
recession.start <- time(drecession[drecession == 1])
recession.end <- time(drecession[drecession == -1])
recession.end <- recession.end
recession.df <- tibble(recession.start , recession.end[2:length(recession.end)])
colnames(recession.df) <- c("start", "end")
recession.df



rec_data <-
  recession.df |> filter(end >= head(yq(
    size_persistence_consumption_ts$year_quarter
  ), 1),
  start <= tail(yq(
    size_persistence_consumption_ts$year_quarter
  ), 1))

size_plot <-
  size_persistence_consumption_ts |> autoplot(size) +
  theme_light() +
  labs(x = "", y = "Size") +
  geom_rect(
    data = rec_data,
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

size_plot


ggsave(
  "size_plot.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  size_plot,
  width = 220,
  height = 140,
  units = "mm"
)



persistence_plot <-
  size_persistence_consumption_ts |> autoplot(persistence) +
  theme_light() +
  labs(x = "", y = "Persistence") +
  geom_rect(
    data = rec_data,
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

persistence_plot


ggsave(
  "persistence_plot.pdf",
  path = "~/Documents/CheckingHank/Checking_HANK/Figures/",
  persistence_plot,
  width = 220,
  height = 140,
  units = "mm"
)


