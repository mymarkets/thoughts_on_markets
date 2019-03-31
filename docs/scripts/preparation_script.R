
rm(list = ls())

# lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)
Sys.setlocale("LC_TIME", "usa")

###### load library ######

# devtools::install_github("jkaupp/nord")

# install packages
package_list <- c(
  # "pinp",
  # "httr",
  # "devtools",
  "tidyverse",
  "tsibble",
  "stringr",
  "purrr",
  "magrittr",
  "stringr",
  "lubridate",
  "dplyr",
  "tidyquant",
  "tibbletime",
  "broom" ,
  "PerformanceAnalytics" ,
  "readxl",
  "openxlsx",
  #"doParallel" ,
  #"foreach" ,
  #"janitor",
  # "rebus",
  "quantmod" ,
  "ggplot2" ,
  "forcats" ,
  #"coefplot", 
  # "gapminder", 
  #  "here", 
  # "interplot", 
  #  "margins", 
 # "maps",
 # "mapproj",
 #  "mapdata", 
 #  "MASS", 
 #  "quantreg",
  "rlang", 
  "scales",
 # "survey", 
 #"srvyr", 
  "viridis", 
  "viridisLite" ,
  "GGally", 
  "ggrepel", 
 # "ggridges", 
  "ggthemes" ,
 #  "gtable" ,
  "grid" ,
  "gridExtra" ,
  "patchwork",
 #  "extrafont" ,
 # "cowplot",  #for arranging plots
  "hrbrthemes" ,
 #  "Quandl" ,
 # "tint",
 #  "swatches",
  "nord" )
 #  "datapasta",
 #  "clipr",
 # "officer",
 #  "svglite" )# ,
  # "rvg")

new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
if(length(new_packages)) lapply( new_packages,install.packages )

# load packages
lapply(package_list, library, character.only = TRUE)

# library(conflicted)
# 
# conflict_prefer("filter", "dplyr")
# conflict_prefer("map", "purrr")
# conflict_prefer("lag", "dplyr")
# conflict_prefer("set_names", "rlang")

source("./scripts/functions/run_event_dates_script.R")
source("./scripts/functions/utilities_script.R",keep.source = TRUE, chdir = TRUE)
source("./scripts/functions/my_theme_script.R")

#*****************************************************************
#
# load data
#******************************************************************


my_download_flag <-    FALSE # TRUE # 

source("./scripts/02_download_econ_data.R")
source("./scripts/03_download_etf_data.R")
source("./scripts/04_download_bloomberg_data.R")
source("./scripts/05_read_fomc_data.R")
source("./scripts/07_construct_riskfactors.R")

econ_df  <- econ_data %>% 
  filter( name %in% "data" ) %>%
  pull(value) %>% .[[1]]

bloomberg_df  <- bloomberg_data %>% 
  filter( name %in% "data") %>% 
  pull(value) %>% .[[1]]

etf_df  <- etf_data %>% 
  filter( name %in% "data") %>% 
  pull(value) %>% .[[1]]


#*****************************************************************
#
# load fonts required for market commentary
#******************************************************************

# http://blog.revolutionanalytics.com/2012/09/how-to-use-your-favorite-fonts-in-r-charts.html
# font_import(paths = NULL, recursive = TRUE, prompt = TRUE,pattern = NULL)
# fonts() # view available fonts
# loadfonts()
windowsFonts(Times=windowsFont("Arial Narrow"))



#*****************************************************************
# Set up Colors
#******************************************************************


par(mfrow=c(8, 2), lheight = 2, mar=rep(1, 4), adj = 0)

walk(names(nord_palettes), nord_show_palette)

dev.off()

# install.packages("tinytex")
# tinytex::install_tinytex()  # install TinyTeX

#*****************************************************************
# Create a list to save pictures
#******************************************************************

pic_list <- list()


# rmarkdown::render("rmd/new_market_commentary.Rmd" , output_dir = "docs")
# rmarkdown::render("rmd/yield_dollar_study.Rmd" , output_dir = "docs")

