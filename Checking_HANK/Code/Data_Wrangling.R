##
## Author: Alexander Vlasov
##
## This code reads and constructs the datasets used further in the work




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
HAWK <- full_join(
  read_csv("Data/Initial_Data/HAWK.csv", col_names = F),
  read_csv("Data/Initial_Data/HAWKIV.csv", col_names = F), by = join_by(X1)
) 

names(HAWK) <- c("point_date", "HAWK", "HAWK_IV")

HAWK_ts <-
  HAWK |>
  mutate(year_quarter = as.yearqtr(point_date) |> yearquarter()) |>
  tsibble(index = year_quarter) |>
  select(year_quarter, HAWK, HAWK_IV)

## Shadow Rate from Wu-Xia -------


shadow_rate_raw <- read_xls("data/Initial_Data/Shadowrate US.xls", col_names = F)

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
    "data/Initial_Data/Natural Rate Estimates.xlsx",
    sheet = "data",
    skip = 5,
    col_names = T
  )

natural_rate_ts <-
  natural_rate_raw |>
  select(Date, rstar...3) |>
  mutate(year_quarter = yearquarter(Date)) |>
  as_tsibble(index = year_quarter) |>
  select(rstar...3) |>
  rename(r_star = rstar)





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
  read_xlsx("data/Initial_Data/Tealbook Row Format.xlsx", sheet = "gPGDP") |>
  select(DATE, gPGDPF1, gPGDPF2) |>
  mutate(expected_inflation_at_event =
           if_else(is.na(gPGDPF2), gPGDPF1, (gPGDPF1 + gPGDPF2) / 2)) |>
  na.omit() |>
  select(-gPGDPF1, -gPGDPF2) |>
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


### Expected Unemployment ------
### UNEMP Greenbook projections for the unemployment rate (percentage points).


expected_unemployment_raw <-
  read_xlsx("data/Initial_Data/Tealbook Row Format.xlsx", sheet = "UNEMP") |>
  select(DATE, UNEMPF1, UNEMPF2) |>
  mutate(expected_unemployment_at_event =
           if_else(is.na(UNEMPF2), UNEMPF1, (UNEMPF1 + UNEMPF2) / 2)) |> na.omit() |>
  select(-UNEMPF1, -UNEMPF2) |>
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



### NAIRU ------
### UNEMP Greenbook projections for the unemployment rate (percentage points).


nairu_raw <-
  read_xlsx("data/Initial_Data/NAIRU 1997-Recent Web.xlsx")

event_dates_raw <-
  read_xlsx("data/Initial_Data/GreenBook Publication Dates.xlsx")

event_dates_ts <-
  event_dates_raw |>
  select(-Source, -ORDER) |>
  rename(FOMC_meeting = `FOMC Meeting`, Publication_date = `Greenbook Publication Date`) |>
  mutate(
    event = str_replace(FOMC_meeting, "mtg", "_"),
    event_quarter = yearquarter(Publication_date)
  ) |> select(-Publication_date, -FOMC_meeting)



nairu_long <-
  nairu_raw |>
  mutate(projection_quarter = yearquarter(Date)) |>
  select(-date1, -Date) |>
  pivot_longer(-projection_quarter) |>
  mutate(event =
           substr(name, 7, 12)) |>
  select(-name) |>
  left_join(event_dates_ts, by = join_by(event))



nairu_expected_97 <-
  nairu_long |>
  select(-event) |>
  filter(projection_quarter == event_quarter + 1 |
           projection_quarter == event_quarter + 2) |>
  group_by(event_quarter) |>
  summarize(expected_nairu = mean(as.numeric(value), na.rm = T)) |>
  rename(year_quarter = event_quarter)


nairu_expected_before <-
  nairu_long |>
  filter(event == "1997_4",
         projection_quarter < min(nairu_expected_97$year_quarter)) |>
  select(-event, -event_quarter) |>
  rename(expected_nairu = value, year_quarter = projection_quarter)


nairu_expected_tbl <- bind_rows(nairu_expected_97, nairu_expected_before)

nairu_expected_ts <- nairu_expected_tbl |> as_tsibble()

### CPI Inflation ------
<<<<<<< HEAD
### gRGDP	Greenbook projections for Q/Q growth in real GDP, chain weight (annualized percentage points)

=======
### gPCPI Greenbook projections for Q/Q headline CPI inflation
>>>>>>> Monetary-Policy-Rules


expected_cpi_inflation_raw <-
  read_xlsx("data/Initial_Data/Tealbook Row Format.xlsx", sheet = "gPCPI") |>
  select(DATE, gPCPIF1, gPCPIF2)  |>
  mutate(expected_cpi_inflation_at_event = if_else(is.na(gPCPIF2), gPCPIF1, (gPCPIF1 + gPCPIF2) / 2)) |>
  na.omit() |>
  select(-gPCPIF1, -gPCPIF2) |>
  mutate(year_quarter = yearquarter(yq((DATE)))) |> select(-DATE)

expected_cpi_inflation_tbl <-
  expected_cpi_inflation_raw |>
  group_by(year_quarter) |>
  summarize(expected_cpi_inflation = mean(expected_cpi_inflation_at_event))

expected_cpi_inflation_ts <-
  expected_cpi_inflation_tbl |>
  as_tsibble() |>
  fill_gaps() |>
  fill(expected_cpi_inflation, .direction = "down")



### Expected GDP Gap ------


expected_output_gap_ts <-
  read_xlsx("data/Initial_Data/Output Gap DH Web.xlsx") |>
  select(...1, last_col()) |>
  mutate(year_quarter = yearquarter(yq(...1))) |>
  select(-...1) |>
  rename(expected_gap = GBgap_181207) |>
  tsibble(index = year_quarter)

expected_output_gap_raw <-
  read_xlsx("data/Initial_Data/Output Gap DH Web.xlsx") |>
  mutate(year_quarter = yearquarter(yq(...1))) |> relocate(year_quarter) |>
  select(-...1)

expected_output_gap_tr <-
  as_tibble(cbind(event = names(expected_output_gap_raw), t(expected_output_gap_raw)))[-1, ]

names(expected_output_gap_tr) <-
  c("event", as.character(expected_output_gap_raw$year_quarter))

expected_output_gap_tr_1 <-
  expected_output_gap_tr |>
  mutate(event_quarter = as.character(yearquarter(ymd(substr(
    event, 7, 20
  ))))) |>
  select(-event) |>
  group_by(event_quarter) |>
  summarize_all( ~ mean(as.numeric(.), na.rm = T))




expected_gap_long <-
  expected_output_gap_tr_1 |>
  pivot_longer(c(-event_quarter)) |>
  rename(projection_quarter = name) |>
  mutate(
    event_quarter = yearquarter(event_quarter),
    projection_quarter = yearquarter(projection_quarter)
  )




expected_gap_96 <-
  expected_gap_long |>
  filter(projection_quarter == event_quarter + 1 |
           projection_quarter == event_quarter + 2) |>
  group_by(event_quarter) |>
  summarise(expected_gap = mean(value)) |>
  rename(year_quarter = event_quarter)



expected_gap_87 <-
  read_xlsx("data/Initial_Data/Output Gap Greenbook.xlsx",
            sheet = "Output Gap",
            skip = 2) |>
  select(3, `T+1`, `T+2`) |>
  rename(year_quarter = T...3) |>
  mutate(year_quarter = yearquarter(year_quarter)) |>
  mutate(expected_gap = (`T+1` + `T+2`) / 2) |>
  select(-c(`T+1`, `T+2`)) |>
  group_by(year_quarter) |>
  summarize(expected_gap = mean(expected_gap)) |>
  as_tsibble() |> 
  filter_index(. ~ as.character(min(expected_gap_96$year_quarter)-1))



expected_gap_75 <-
  expected_gap_long |>
  na.omit() |>
  filter(
    projection_quarter < min(expected_gap_87$year_quarter) &
      yq(event_quarter) == yq("1996 Q2")
  ) |>
  select(-event_quarter) |>
  rename(year_quarter = projection_quarter, expected_gap = value)


expected_gap_ts <-
  bind_rows(expected_gap_87, expected_gap_96) |>
  as_tsibble()



fedhead_quarterly_ts <- 
  read_csv("data/Intermediate_Data/fedhead_quarterly.csv")|> 
  mutate(year_quarter=yearquarter(year_quarter))|> as_tsibble()

# Final Dataset Compilation ------

full_dataset_ts <-
  full_join(fed_funds_rate_ts, natural_rate_ts, by = "year_quarter") |>
  full_join(HAWK_ts, by = "year_quarter") |>
  full_join(inflation_ts, by = "year_quarter") |>
  full_join(consumption_ts, by = "year_quarter") |>
  full_join(expected_inflation_ts, by = "year_quarter") |>
  full_join(expected_unemployment_ts, by = "year_quarter") |>
  full_join(expected_cpi_inflation_ts, by = "year_quarter") |>
  full_join(expected_gap_ts, by = "year_quarter") |>
  full_join(nairu_expected_ts, by = "year_quarter") |>
  full_join(fedhead_quarterly_ts,  by = "year_quarter") |>
  filter_index("1968 Q1" ~ "2020 Q4") |>
  mutate(
    row_number = row_number(),
    dR = fed_funds_rate - r_star,
    demeaned_HAWK = HAWK - mean(HAWK, na.rm = T),
    demeaned_HAWK_IV = HAWK_IV - mean(HAWK_IV, na.rm = T),
    stance = dR >= 0,
    expected_unemployment_gap = expected_unemployment - expected_nairu,
    log_consumption = log(consumption),
    delta_log_consumption = difference(log_consumption),
    delta_expected_inflation = difference(expected_inflation),
    delta_expected_unemployment_gap = difference(expected_unemployment_gap),
    delta_expected_cpi_inflation = difference(expected_cpi_inflation),
    delta_expected_gap = difference(expected_gap),
  )



full_dataset_tbl <- full_dataset_ts |> as_tibble()



write.csv(as_tibble(expected_gap_ts), 
          file = "data/Intermediate_Data/expected_gap.csv")
write.csv(as_tibble(nairu_expected_ts), 
          file = "data/Intermediate_Data/expected_nairu.csv")
write.csv(as_tibble(HAWK_ts), 
          file = "data/Intermediate_Data/HAWK_Index.csv")

write.csv(full_dataset_tbl, 
          file = "data/Intermediate_Data/full_dataset.csv")

