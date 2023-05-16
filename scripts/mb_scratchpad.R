
tar_load(clean_data)

hist = clean_data %>%
  filter(scenario == "Historic") %>%
  mutate(concat = paste(variable,year))

duplicates = hist[duplicated(hist$concat),]

duplicates_compare_unit = duplicates %>%
  select(model,variable,unit,datasrc) %>%
  distinct()

vartest = hist %>% filter(variable == "Emissions|CO2|Energy|Coal")

devtools::load_all()
library(targets)
library(tidyverse)
tar_load(data_raw)
glimpse(data_raw)

tar_load(data_min)
glimpse(data_min)

tar_load(data_long_read)
test = data_long_read %>% mutate(concat = paste(model,scenario,unit,year,datasrc,variable,region))
duplicates = test[duplicated(test$concat),]
view(duplicates)
unique(duplicates$model)
unique(duplicates$datasrc)
unique(duplicates$variable)

tar_load(data_long)
test = data_long %>% mutate(concat = paste(model,scenario,unit,year,datasrc,variable,region))
duplicates = test[duplicated(test$concat),]
view(duplicates)
unique(duplicates$model)
unique(duplicates$datasrc)

tar_load(clean_data)
test = clean_data %>% mutate(concat = paste(model,scenario,unit,year,datasrc,variable,region))
duplicates = test[duplicated(test$concat),]
unique(duplicates$model)
unique(duplicates$datasrc)


