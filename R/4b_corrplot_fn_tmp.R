library(corrplot)
library(tidyverse)
library(tibble)
# library(PerformanceAnalytics)
# library(Hmisc)

corrplot_fn_tmp <- function(presentation_title, presentation_plot_type, config, emf_data_long) {

  # plot_list <- read_csv(plot_list, col_types = cols())

  # standard_corrplot_cols <- c("figure_type","figure_no","title_name","regions","models","years",
  #                           "scenarios","source","source_var","target","target_var","link_var")

  overall_path = paste("./output/round",config$round_num,"/", presentation_title, "/", presentation_plot_type, "/", sep = "")
  type = presentation_plot_type

  # corr_plot <- function(filepath) {
  # df <- readr::read_csv(filepath)
  # head(emf_data_long)

  # Old vars

  df <- emf_data_long %>%
    filter(year == "2050") %>%
    filter(region == "United States" & str_detect(scenario, "0by50")) %>%
    filter(variable %in% c('Price|Final Energy|Electricity', 'Price|Final Energy|Electricity|Wholesale', 'Capacity|Electricity|Peak Demand|Summer|Level', 'Capacity|Electricity|Peak Demand|Winter|Level', 'Capacity|Electricity|Transmissions Grid')) %>% # 'Emissions Intensity|CO2|Electricity',
    select(-`unit`) %>%
    pivot_wider(names_from = "variable", values_from = "value") %>%
    select(-`year`)
  # df <- df[c('Secondary Energy|Electricity|Solar', 'Secondary Energy|Electricity|Wind', 'Final Energy|Transportation|Electricity', 'Carbon Sequestration|Direct Air Capture|Net', 'Secondary Energy|Hydrogen'),]
  # df <- pivot_wider(df, names_from = "variable", values_from = "value")
  head(df[, sapply(df, is.numeric)])
  M <- cor(df[, sapply(df, is.numeric)],
           use = "pairwise.complete.obs", method = "pearson")
  # M <- cor(as.numeric(unlist(df[6])), as.numeric(unlist(df[7])))
  # print(M)
  # head(round(M,2))
  # print(M)
  # return(df)
  # }

  # Performance Analytics plotting

  jpeg(file = paste(overall_path, "corrplot_elc.jpeg", sep = ""), units="in", width=5, height=5, res=1200)
  # chart.Correlation(M)
  dev.off()

  # corrplot plotting

  jpeg(file = paste(overall_path, "corrplot_old_without_diag.jpeg", sep = ""), units="in", width=5, height=5, res=1200)
  corrplot(M, method = 'color', type = 'lower', tl.cex = 0.6, tl.srt=70, diag = F)
  dev.off()
  jpeg(file = paste(overall_path, "corrplot_old_circle.jpeg", sep = ""), units="in", width=5, height=5, res=1200)
  corrplot(M, method = 'circle', order = 'FPC', type = 'lower', diag = FALSE, tl.cex = 0.6, tl.srt=70)
  # corrplot.mixed(M, lower = 'shade', upper = 'pie', order = 'hclust', tl.cex = 0.6, tl.srt=70)
  dev.off()

  # New vars

  # df replaces NAs with zero

  df <- emf_data_long %>%
    filter(year == "2050") %>%
    filter(region == "United States" & str_detect(scenario, "0by50")) %>%
    filter(variable %in% c("Final Energy|Biogas", "Final Energy|Biomass Liquids", "Final Energy|Biomass Solids",
                           "Final Energy|Coal", "Final Energy|Gas", "Final Energy|Oil", "Final Energy|Hydrogen",
                           "Final Energy|Transportation|Electricity", "Final Energy|Synthetic Gas")) %>%
    select(-`unit`) %>%
    pivot_wider(names_from = "variable", values_from = "value", values_fill = list(Value = 0)) %>%
    select(-`year`) %>%
    replace(is.na(.), 0)
  df <- df %>% replace(is.na(.), 0)

  # df2 doesn't replace NAs with zero and is meant for testing with the Hmisc package

  df2 <- emf_data_long %>%
    filter(year == "2050") %>%
    filter(region == "United States" & str_detect(scenario, "0by50")) %>%
    filter(variable %in% c("Final Energy|Biogas", "Final Energy|Biomass Liquids", "Final Energy|Biomass Solids",
                           "Final Energy|Coal", "Final Energy|Gas", "Final Energy|Oil", "Final Energy|Hydrogen",
                           "Final Energy|Transportation|Electricity", "Final Energy|Synthetic Gas")) %>%
    select(-`unit`) %>%
    pivot_wider(names_from = "variable", values_from = "value") %>%
    select(-`year`)

  # df <- df[c('Secondary Energy|Electricity|Solar', 'Secondary Energy|Electricity|Wind', 'Final Energy|Transportation|Electricity', 'Carbon Sequestration|Direct Air Capture|Net', 'Secondary Energy|Hydrogen'),]
  # df <- pivot_wider(df, names_from = "variable", values_from = "value")

  # M <- cor(df[, sapply(df, is.numeric)],
  #          use = "pairwise.complete.obs", method = "pearson")
  # M <- cor(as.numeric(unlist(df[6])), as.numeric(unlist(df[7])))
  # print(M)
  # head(round(M,2))
  # print(M)
  # return(df)
  # }

  # Performance Analytics plotting

  # jpeg(file = paste(overall_path, "corrplot_pmetrics_new.jpeg", sep = ""), units="in", width=5, height=5, res=1200)
  # chart.Correlation(M)
  # dev.off()

  # corrplot plotting

  jpeg(file = paste(overall_path, "corrplot_new_without_diag.jpeg"), units="in", width=5, height=5, res=1200)
  corrplot(M, method = 'color', type = 'lower', tl.cex = 0.6, tl.srt=70, diag = F)
  dev.off()
  jpeg(file = paste(overall_path, "corrplot_new_circle.jpeg"), units="in", width=5, height=5, res=1200)
  corrplot(M, method = 'circle', order = 'FPC', type = 'lower', diag = FALSE, tl.cex = 0.6, tl.srt=70)
  # corrplot.mixed(M, lower = 'shade', upper = 'pie', order = 'hclust', tl.cex = 0.6, tl.srt=70)
  dev.off()

  # Trying out the Hmisc package -- Commented out for now as it produces errors that need to be handled

  # t <- df2[, sapply(df2, is.numeric)]
  # rcorr <- Hmisc::rcorr(as.matrix(df2[, sapply(df2, is.numeric)]), type="pearson")

  # The mixed version doesn't look good as the variable names are long and overlap with the figure, but we can work to correct this later if desired
  # jpeg(file = "cor4.jpeg", units="in", width=5, height=5, res=1200)
  # corrplot.mixed(M, lower = 'shade', upper = 'pie', order = 'hclust')
  # dev.off()
  # Trying with variation on year
  # print(emf_data_long[, sapply(df, is.numeric)])
  # M2 <- cor(emf_data_long[, sapply(emf_data_long, is.numeric)],
  #           use = "na.or.complete", method = "pearson")
  # print(M2)
  # corrplot(M2, method = 'color', type = 'lower')
  # corrplot.mixed(M2, lower = 'shade', upper = 'pie', order = 'hclust')
}

# Below function call is only to be uncommented for standalone execution of this code, will cause errors if used when running the pipeline so please do NOT uncomment :)

#corrplot_fn_tmp("overview", "corrplot", config, emf_data_long)

