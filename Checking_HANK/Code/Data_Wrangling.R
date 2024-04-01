##
##
## Author: Alexander Vlasov
##




# Libraries -----
required_Packages_Install <-
  c(
    "tidyverse",
    "sandwich",
    "zoo",
    "quantmod",
    "tsibble",
    "readxl",
    "fable",
    "fabletools"
  )

for (Package in required_Packages_Install) {
  if (!require(Package, character.only = TRUE)) {
    install.packages(Package, dependencies = TRUE)
  }
  
  library(Package, character.only = TRUE)
}


# Data reading and manipulations -----

## HAWK and HAWK_IV data from Hack, Isterfi, Meier (2024) ------
HAWK <- merge(
  read_csv("Data/HAWK.csv", col_names = F),
  read_csv("Data/HAWKIV.csv", col_names = F),
  "X1"
) |> as_tibble()

names(HAWK) <- c("point_date", "HAWK", "HAWK_IV")
HAWK_ts <-
  HAWK |>
  mutate(date = as.yearqtr(point_date) |> yq()) |>
  tsibble(index = date) |>
  select(date, HAWK, HAWK_IV) |>
  index_by(year_quarter = ~ yearquarter(.))

## Shadow Rate from Wu-Xia -------


shadow_rate_raw <- read_xls("data/Shadowrate US.xls", col_names = F)

names(shadow_rate_raw) <- c("yearmonth", "Shadow_Rate")

shadow_rate_ts <-
  shadow_rate_raw |>
  mutate(date = ym(yearmonth)) |>
  tsibble(index = date) |>
  select(date, Shadow_Rate) |>
  index_by(year_quarter = ~ yearquarter(.)) |>
  summarise(Shadow_Rate = mean(Shadow_Rate))



natural_rate_raw <-
  read_xlsx(
    "data/Natural Rate Estimates.xlsx",
    sheet = "data",
    skip = 5,
    col_names = T
  )

natural_rate_ts <-
  natural_rate_raw |>
  select(Date, rstar...3) |>
  mutate(date = yearquarter(Date)) |>
  as_tsibble(index = date) |>
  select(rstar...3) |>
  rename(r_star = rstar) |>
  index_by(year_quarter = ~ yearquarter(.))





getSymbols(c("A794RX0Q048SBEA", "DFF", "PCEPILFE"), src = "FRED")

consumption_raw <- A794RX0Q048SBEA
consumption_ts <-
  consumption_raw |> fortify.zoo(melt = TRUE) |>
  select(-Series) |>
  mutate(year_quarter = yearquarter(Index)) |>
  as_tsibble(index = year_quarter) |> 
  select(-Index) |> 
  rename(consumption = Value)



effective_ffr <-
  DFF |>
  fortify.zoo(melt = TRUE) |>
  as_tsibble() |>
  rename(date = Index, ffr = Value) |>
  select(-Series)

effective_ffr_ts <- effective_ffr  |>
  index_by(year_quarter = ~ yearquarter(.)) |>
  summarise(ffr = mean(ffr))

fed_funds_rate_ts <-
  full_join(effective_ffr_ts, shadow_rate_ts, by = "year_quarter") |>
  mutate(fed_funds_rate = pmin(ffr, Shadow_Rate, na.rm = T)) |> 
  select(year_quarter, fed_funds_rate)


inflation_ts <-
  PCEPILFE |>
  fortify.zoo(melt = TRUE) |>
  select(-Series) |>
  as_tsibble(index = Index) |>
  index_by(year_quarter = ~ yearquarter(.)) |>
  summarize(price = last(Value)) |>
  mutate(core_inflation = 100 * (log(price) - log(lag(price, 12)))) |> select(-price)


## Tealbook information -----

### Expected Deflator Inflation ------
### gPGDP	Greenbook projections for Q/Q growth in price index for GDP, 
### chain weight (annualized percentage points)

expected_inflation_raw <-
  read_xlsx("data/Tealbook Row Format.xlsx", sheet = "gPGDP") |>
  select(DATE, gPGDPF1, gPGDPF2) |>
  mutate(expected_inflation_at_event =
           if_else(is.na(gPGDPF2),
                   gPGDPF1,
                   (gPGDPF1 + gPGDPF2) / 2)) |>
  na.omit() |>
  select(-gPGDPF1,-gPGDPF2) |>
  mutate(year_quarter = yearquarter(yq((DATE)))) |> select(-DATE)


expected_inflation_tbl <-
  expected_inflation_raw |>
  group_by(year_quarter) |> 
  summarize(expected_inflation = mean(expected_inflation_at_event))

expected_inflation_ts <-
  expected_inflation_tbl |> 
  as_tsibble() |> 
  fill_gaps() |> 
  fill(expected_inflation, .direction = "down")


### Expected Unemployment Growth ------
### UNEMP Greenbook projections for the unemployment rate (percentage points).


expected_unemployment_raw <-
  read_xlsx("data/Tealbook Row Format.xlsx", sheet = "UNEMP") |>
  select(DATE, UNEMPF1, UNEMPF2) |>
  mutate(
    expected_unemployment_at_event = 
      if_else(
        is.na(UNEMPF2), 
        UNEMPF1, 
        (UNEMPF1 + UNEMPF2) / 2
        )
    ) |> na.omit() |>
  select(-UNEMPF1,-UNEMPF2) |>
  mutate(year_quarter = yearquarter(yq((DATE)))) |> select(-DATE)

expected_unemployment_tbl <-
  expected_unemployment_raw |>
  group_by(year_quarter) |> 
  summarize(expected_unemployment = mean(expected_unemployment_at_event))

expected_unemployment_ts <-
  expected_unemployment_tbl |> 
  as_tsibble() |> 
  fill_gaps() |> 
  fill(expected_unemployment, .direction = "down")



### GDP growth ------
### gRGDP	Greenbook projections for Q/Q growth in real GDP, chain weight (annualized percentage points)


expected_gdp_raw <-
  read_xlsx("data/Tealbook Row Format.xlsx", sheet = "gRGDP") |>
  select(DATE, gRGDPF1, gRGDPF2)  |>
  mutate(expected_gdp_at_event = if_else(
    is.na(gRGDPF2), 
    gRGDPF1, 
    (gRGDPF1 + gRGDPF2) / 2
  )) |>
  na.omit() |>
  select(-gRGDPF1, -gRGDPF2) |>
  mutate(year_quarter = yearquarter(yq((DATE)))) |> select(-DATE)

expected_gdp_tbl <-
  expected_gdp_raw |> 
  group_by(year_quarter) |> 
  summarize(expected_gdp = mean(expected_gdp_at_event))

expected_gdp_ts <-
  expected_gdp_tbl |> 
  as_tsibble() |> 
  fill_gaps() |> 
  fill(expected_gdp, .direction = "down")




# Final Dataset Compilation ------

full_dataset_ts <-
  inner_join(fed_funds_rate_ts, natural_rate_ts, by = "year_quarter") |> 
  select(-date) |>
  inner_join(HAWK_ts, by = "year_quarter") |> select(-date) |>
  inner_join(inflation_ts, by = "year_quarter") |>
  inner_join(consumption_ts, by = "year_quarter") |>
  inner_join(expected_inflation_ts, by = "year_quarter") |>
  inner_join(expected_unemployment_ts, by = "year_quarter") |>
  inner_join(expected_gdp_ts, by = "year_quarter") |>
  mutate(
    row_number = 1:n(),
    dR = fed_funds_rate - r_star,
    demeaned_HAWK = HAWK - mean(HAWK),
    demeaned_HAWK_IV = HAWK_IV - mean(HAWK_IV),
    stance = dR >= 0,
    log_consumption = log(consumption), 
    delta_log_consumption = difference(log_consumption), 
    delta_expected_inflation = difference(expected_inflation),
    delta_expected_unemployment = difference(expected_unemployment)
  )

full_dataset_tbl <- full_dataset_ts |> as_tibble()


write.csv(full_dataset_tbl, file = "data/full_dataset.csv")

