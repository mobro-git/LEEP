
# creates figmap csv paths based on mapping csv
figmap_csv_path <- function(fig_subject, fig_type, config) {

  paste0("plot_mapping/round", config$round_num, "/", fig_subject, "_", fig_type, ".csv")

}
