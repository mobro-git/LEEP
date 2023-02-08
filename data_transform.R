#### transforming bistline database into tall format

library(tidyverse)
library(readxl)
library(xlsx)

data.emissions <- read_xlsx("data-raw/model-runs/ira_comparison.xlsx", sheet = "Emissions") %>%
  gather("year", "value", 4:9) %>%
  mutate(table = "emissions")

data.co2captured <- read_xlsx("data-raw/model-runs/ira_comparison.xlsx", sheet = "CO2 Captured") %>%
  gather("year", "value", 4:9) %>%
  mutate(table = "co2_captured")

data.costs <- read_xlsx("data-raw/model-runs/ira_comparison.xlsx", sheet = "Costs") %>%
  gather("year", "value", 4:9) %>%
  mutate(table = "costs")

data.elecdemand <- read_xlsx("data-raw/model-runs/ira_comparison.xlsx", sheet = "Electricity Demand") %>%
  gather("year", "value", 4:9) %>%
  mutate(table = "elec_demand")

data.nox <- read_xlsx("data-raw/model-runs/ira_comparison.xlsx", sheet = "NOx") %>%
  gather("year", "value", 4:9) %>%
  mutate(table = "nox")

data.so2 <- read_xlsx("data-raw/model-runs/ira_comparison.xlsx", sheet = "SO2") %>%
  gather("year", "value", 4:9) %>%
  mutate(table = "so2")

data.ffconsump <- read_xlsx("data-raw/model-runs/ira_comparison.xlsx", sheet = "Fossil Fuel Consumption") %>%
  gather("year", "value", 4:10) %>%
  mutate(table = "ff_consump")

combined.data <- full_join(data.emissions, data.co2captured) %>%
  full_join(data.costs) %>%
  full_join(data.elecdemand) %>%
  full_join(data.nox) %>%
  full_join(data.so2) %>%
  full_join(data.ffconsump) %>%
  #write.csv("bistline_ira.csv")
  write.xlsx("bistline_ira_tall.xlsx", sheetName = "data", col.names = TRUE)
