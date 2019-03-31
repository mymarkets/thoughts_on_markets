# https://github.com/abresler/fundManageR

# https://github.com/ropensci/pdftools
install.packages("pdftools")

packages <- 
  c("curl", "curlconverter", "dplyr", "formattable", "httr", "jsonlite", 'devtools',
    "lazyeval", "lubridate", "magrittr", "pdftools", "purrr", "readr",  'quantmod',
    "readxl", "rvest", "stringi", "stringr", "tibble", "tidyr", 'tidyverse',
    "xml2")

lapply(packages, install.packages)

devtools::install_github("abresler/fundManageR")