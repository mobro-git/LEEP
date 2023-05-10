# LTS data

# load reporting template to check against
tar_load(template)
template_var = unique(template$variable)

# all modeled and historic data from ncs-lts work with unnecessary variables removed
lts_raw = readxl::read_xlsx("data-extra/LTS_alldata.xlsx")

lts_data = lts_raw %>%
  filter(!str_detect(variable,
                     "Agricultural|
                     CC|
                     CT|
                     ST|
                     Commercial|
                     Residential|
                     Value|
                     GDP|
                     Population|
                     Output|
                     Price|
                     retrofits|
                     Cofired")) %>%
  arrange(variable)

# list of variables reported in LTS
lts_var = unique(lts_data$variable)
lts_var

# variables reported for LTS that dont match reporting template
diff = setdiff(lts_var, template_var)
diff

# subset of lts_data that doesnt match reporting template
lts_mismatch = lts_data %>%
  filter(variable %in% diff)

# variable name fixes to match reporting template
lts_rename = lts_mismatch %>%
  mutate(variable = str_replace(variable, "Biomass liquids", "Biomass Liquids"))

diff_check = setdiff(unique(lts_rename$variable), template_var)
diff_check


# lts_rename = lts_mismatch %>%
#   mutate(variable = case_when(
#     str_detect(variable, "Biomass liquids") ~ str_replace(variable, "Biomass liquids", "Biomass Liquids"),
#     str_detect(variable, " w/ CCS") ~ str_replace(variable, "|w/ CCS"),
#     str_detect(variable, " w/o CCS") ~ str_replace(variable, "|w/o CCS"),
#     TRUE~variable
#   ))








