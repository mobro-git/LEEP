#####
##
## this code prepares the data_long dataframe for processing in Tableau
##
#####

library(tidyverse)
library(readxl)
library(xlsx)

tableau_input <- data_long


write.csv(tableau_input, "data-extra/data_long_tableau.csv", row.names = FALSE)

## create economy-wide sum for emissions|CO2

tableau_input_emissions <- data_long %>%
  filter(variable %in% c("Emissions|CO2|Energy|Demand|Buildings",
                         "Emissions|CO2|Energy|Demand|Transportation",
                         "Emissions|CO2|Energy|Demand|Industry",
                         "Emissions|CO2|Energy|Supply|Electricity",
                         "Emissions|CO2|Industrial Processes",
                         "Emissions|CO2|Other")) %>%
  group_by(model, scenario, region, unit, year, datasrc) %>%
  summarise(value = sum(value)) %>%
  mutate(variable = "Emissions|CO2_calc")

tableau_input <- full_join(tableau_input, tableau_input_emissions)
