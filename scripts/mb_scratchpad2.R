library(targets)
tar_load(data_raw)
tar_load(template_original)
tar_load(template)
tar_load(data_long)

data_raw = data_raw %>% arrange(variable)

raw_vars = unique(data_raw$variable)
orig_vars = unique(template_original$variable)
new_vars = unique(template$variable)

unmatched_orig = setdiff(raw_vars,orig_vars)
unmatched_orig

unmatched = setdiff(raw_vars,new_vars)
unmatched

unmatched_data = data_raw %>%
  filter(variable %in% unmatched)
unmatched_source = unique(unmatched_data$datasrc)
unmatched_source

##########################
workbook = unmatched_source[4]
print(workbook)

data = unmatched_data %>% filter(datasrc == workbook)
data_vars = unique(data$variable)
data_vars

vars_data_long = data_long %>% filter(datasrc == workbook)












#####################################

bev = clean_data %>% filter(variable == "Energy Service|Transportation|Passenger|BEV|Sales Share") #datasrc == "bistline_ira_tall.csv" & 7y
bev_raw = data_raw %>% filter(variable == "Energy Service|Transportation|Passenger|BEV|Sales Share")
bev_min = data_min %>% filter(variable == "Energy Service|Transportation|Passenger|BEV|Sales Share")












