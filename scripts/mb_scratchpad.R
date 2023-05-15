
tar_load(clean_data)

hist = clean_data %>%
  filter(scenario == "Historic") %>%
  mutate(concat = paste(variable,year))

duplicates = hist[duplicated(hist$concat),]

duplicates_compare_unit = duplicates %>%
  select(model,variable,unit,datasrc) %>%
  distinct()

vartest = hist %>% filter(variable == "Emissions|CO2|Energy|Coal")
