# packages.R

library(targets)
library(tarchetypes)

library(conflicted)
conflict_prefer("filter", "dplyr")

library(here)
library(fs)
library(glue)

library(plotly)
library(data.table)
library(tidyverse)
library(patchwork)
library(ggrepel)

library(kableExtra)
library(DT)

# devtools::install_github("coolbutuseless/ggpattern")
# library(ggpattern)

if(FALSE) {
  # explicitly mention packages needed to assist renv::snapshot discovery
  library(visNetwork) # used for targets::tar_visnetwork()
}
