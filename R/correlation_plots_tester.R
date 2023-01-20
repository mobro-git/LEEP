# source('/Users/shahabmousavi/Desktop/EMF37/EMF37viz/R/correlation_plots.R')

# df = corr_plot('/Users/shahabmousavi/Desktop/EMF37/EMF37viz/data-raw/iiasa-db-data/scout_US.csv')

# write.csv(df,"/Users/shahabmousavi/Desktop/test/overview_timeseries_autofilled.csv")

# df <- readr::read_csv('/Users/shahabmousavi/Desktop/EMF37/EMF37viz/data-raw/iiasa-db-data/scout_US.csv')
# head(df)
# ndf <- select(df[df$year == 2025,], c('variable', 'value'))
# head(ndf)
# M<-cor(ndf$variable, lapply(ndf$value, as.numeric))
# head(round(M,2))

# M<-cor(mtcars)
# head(round(M,2))
#
# corrplot(M, method = 'color', type = 'lower')
