
# Themes

theme_emf <- function(fig_size = "auto") {

  if(fig_size == "auto") {
    theme <- theme_light() +
      theme(text = element_text(size = 12),
            legend.background = element_rect(fill="white",color=NA),
            legend.title=element_blank(),
            legend.position="right",
            plot.margin = margin(8,10,8,8),
            axis.text.y = element_text(size=9),
            axis.text.x = element_text(size=9),
            #axis.line = element_line(color="black"),
            axis.title.y = element_text(face="bold",vjust=2),
            strip.text.x = element_text(size = 10, color = "black",face = "bold"),
            strip.text.y = element_text(size = 9, color = "black",face = "bold"),
            strip.background = element_rect(fill=NA, size=1))#C5CFE3
  }
  else if(fig_size == "small") {
    theme <- theme_light() +
      theme(text = element_text(size = 11),
            plot.subtitle = element_text(face="bold",size = 10,hjust=0.5),
            axis.text.y = element_text(size=10),
            axis.title.y = element_text(size=10,face="bold",vjust=2))
    theme
  }}

xup <- theme(axis.text.x = element_text(vjust = 12))
xup_light <- theme(axis.text.x = element_text(vjust = 10))
slantx <- theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))
bottom1 <- theme(legend.position = "bottom", legend.title = element_blank())
nolegend <- theme(legend.position='none')

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x=element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

# Color Palette

master_palette <- c(

  # official RTI LEEP Report color palette
  "Lsalmon" = "#DD614A",
  "Lteal" = "#6a959b",
  "Lblue" = "#2A546C",
  "Lgold" = "#AE7F2C",
  "Lpurple" = "#8B507E",
  "Lgreen" = "#488C49",
  "Lmagenta" = "#af499b",
  "Lnavy" = "#25266b",
  "Laqua" = "#11897d",
  "Lsky" = "#0083ca",
  "Lorange" = "#f15a22",
  "Lindigo" = "#524fa1",
  "Lpink" = "#af499b",
  "Lbrown" = "#876123",
  "black" = "black",

  # old colors
  "light salmon" = "#F28063",
  "teal blue" = "#0388B3"
  )

# Color Mapping - labels matched to hex codes

color_map <- c(
  # scenarios
  "Historic" = "black",
  # "IRA" = "Lgold",
  # "Reference" = "Lblue",
  # "No IRA" = "Lblue",
  # "IRA.High" = "Lteal",
  # "IRA.Low" = "Lsky",

  "IRA" = "teal blue",
  "Reference" = "light salmon",
  "No IRA" = "light salmon",

  # models
  "NEMS-EIA" = "black", #AEO 2023 outputs from OnLocation
  "EIA"= "black",
  "EIA-LTS"= "black",
  "EIA-STEO"= "black",
  "EPA-GHGI" = "black",
  "EPS-EI" = "Lsalmon",
  "GCAM-CGS" = "Lteal",
  "GCAM-LTS" = "Lteal",
  "GCAM-PNNL" = "Lblue",
  "Haiku-RFF" = "Lgold",
  "IPM-EPA" = "Lpurple",
  "IPM-NRDC" = "Lgreen",
  "MARKAL-NETL" = "Lmagenta",
  "NEMS-RHG" = "Lnavy",
  "NEMS-OP" = "Laqua",
  "NEMS-OP-LTS" = "Laqua",
  "REGEN-EPRI" = "Lorange",
  "RIO-REPEAT" = "Lindigo",
  "ReEDS-NREL" = "Lpink",
  "Scout-LEEP" = "Lbrown",
  "USREP-ReEDS" = "Lsky",

  # technology
  "Biomass w/o CCS" = "Lindigo",
  "Biomass w/ CCS" = "Lpink",
  "Coal" = "Lblue",
  "Coal w/ CCS" = "black",
  "Coal w/o CCS" = "Lpurple",
  "Gas" = "Lgold",
  "Gas w/ CCS" = "Lsalmon",
  "Gas w/o CCS" = "Lmagenta",
  "Geothermal" = "Lblue",
  "Hydro" = "Laqua",
  "Hydrogen" = "Lorange",
  "Nuclear" = "Lsky",
  "Petroleum" = "Lpurple",
  "Oil" = "Lbrown",
  "Solar" = "Lgold",
  "Wind" = "Lgreen",
  "Storage" = "Lnavy",
  "Other" = "Lteal",

  # sectors
  "Total" = "black",
  "Transportation: Direct" = "Lteal",
  "Transportation: Indirect" = "Lblue",
  "Industry: Direct" = "Lgreen",
  "Industry: Indirect" = "Lnavy",
  "Buildings: Direct" = "Lgold",
  "Buildings: Indirect" = "Lpurple",

  # industrial emissions
  "Cement" = "Lsalmon",
  "Food Processing" = "Lteal",
  "Other Heavy Industry" = "Lblue",
  "Iron and Steel" = "Lgold",
  "Paper" = "Lpurple",
  "Other Light Industry" = "Lgreen",
  "Refining" = "Lmagenta",
  "Chemicals" = "Lnavy",
  "Light" = "Laqua",
  "Heavy" = "Lsky"
  ) %>%
  find_color(master_palette)

sub_palettes <- list(
  `test` = c(
    "ADAGE"  = "ADAGE" ,
    "AnyMOD" = "AnyMOD"
  )
) %>%
  map( ~ find_color(.x, color_map))


