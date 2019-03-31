#   ____________________________________________________________________________
#   Packaging packages                                                      ####
# https://www.digitalocean.com/community/tutorials/how-to-set-up-r-on-ubuntu-14-04#step-4-%E2%80%94-installing-devtools-package

# packages to be installed
pkgs <-  c(
  "devtools",
  "pinp",
  "httr",
  "devtools",
  "tidyverse" ,
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
  "pryr",
  "leaps" ,
  "janitor",
  "rebus",
  "quantmod" ,
  "ggplot2" ,
  "forcats" ,
  "finalfit",
  "stargazer",
  "viridis" ,
  "ggthemes" ,
  "ggrepel"  ,
  "gtable" ,
  "grid" ,
  "gridExtra" ,
  "extrafont" ,
  "hrbrthemes" ,
  "Quandl" ,
  "tint",
  "swatches",
  "nord",
  "datapasta",
  "clipr",
  "officer",
  "svglite",
  "rvg",
  "here")
# install packages


check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
  pkgs.missing <- pkgs[!check]
  install.packages(pkgs.missing)
  check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
}

devtools::install_github("thomasp85/patchwork")
devtools::install_github("gadenbuie/regexplain")
# devtools::install_github("kjhealy/socviz")
devtools::install_github("slowkow/ggrepel")
# http://o2r.info/2017/05/30/containerit-package/

# http://rstudio.github.io/packrat/walkthrough.html
# library(packrat)
# init()
#
# packrat::status()
# restore()
# packrat::unused_packages()
# packrat::clean()


# https://andrewbtran.github.io/NICAR/2018/workflow/docs/01-workflow_intro.html?utm_content=buffer858fd&utm_medium=social&utm_source=twitter.com&utm_campaign=buffer




#   ____________________________________________________________________________
#   Creating folders    
####
# https://andrewbtran.github.io/NICAR/2018/workflow/docs/01-workflow_intro.html?utm_content=buffer858fd&utm_medium=social&utm_source=twitter.com&utm_campaign=buffer
# http://o2r.info/2017/05/30/containerit-package/

library(here)
here()
# list files in the current directory
list.dirs()

# create required directories

folder_names <- c("raw_data", "output_data", "rmd", "docs", "scripts")
req_dir_names <-  folder_names[ dir.exists(folder_names) == FALSE ]

req_dir_names <- req_dir_names[ req_dir_names %>% dir.exists() == FALSE ]

if(!is_empty(req_dir_names)) dir.create(req_dir_names)

#   ____________________________________________________________________________
#   Creating folder shortcut                                                ####



sapply(req_folder_names , dir.create)



