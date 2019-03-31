
library(tidyverse)
library(rvest)
# library(RSelenium)
# https://mwaldstein.github.io/edgarWebR/
# devtools::install_github("mwaldstein/edgarWebR")
# https://github.com/sewardlee337/finreportr
# devtools::install_github("sewardlee337/finreportr")
library(edgarWebR)

my_url <- "https://www.sec.gov/files/money_market_fund_data_dictionary.csv"
download.file( my_url , destfile = "./raw_data/edgar/money_market_fund_data_dictionary.csv" )


my_url <- "https://www.sec.gov/files/investment/data/other/money-market-fund-information/mmf-2018-10.csv"
download.file( my_url , destfile = "./raw_data/edgar/data/mmf-2018-10.csv" )

file_lsts <- list.files("./raw_data/edgar/data", full.names = TRUE)
edgar_data <- file_lsts  %>% 
  map( readr::read_csv )

edgar_data <- edgar_data %>% 
  set_names(
    file_lsts %>% 
   stringr::str_replace( pattern = ".*/data/" , replacement = "") %>% 
   stringr::str_replace( pattern = ".csv" , replacement = "") %>% 
   str_squish()

  )




edgar_dic <- readr::read_csv(  file = "./raw_data/edgar/money_market_fund_data_dictionary.csv")

edgar_data[[1]] %>%
   select(registrant_name, series_id)



# company_filings("AAPL", type = "10-K", count = 10)

funds_key_tbl <- fund_fast_search("S000000048") 
funds_key_tbl <- fund_fast_search("S000000074") 
funds_key_tbl %>% names()

funds_key_tbl %>% select(series_filings_href)

funds_key_tbl %>% View()

filings <- company_filings( funds_key_tbl$series_id, type = "N-Q", count = 100)

filings %>% 
  slice(1) %>% 
  print() %>% 
  View()

x <- filings %>% 
  slice(1) %>% 
  pull(href) %>% 
  paste0()

# x <-  paste0("https://www.sec.gov/Archives/edgar/data/742212/000119312518333720/0001193125-18-333720-index.htm")

x <- x %>% 
filing_documents() %>% 
  slice(1) %>% 
  pull(href)
# filing_funds(x)
# filing_details(x)
# parse_submission(x)
x
parsed_docs <- parse_filing(x, strip = FALSE,  include.raw = FALSE)

parsed_docs %>% names()

parsed_docs %>% head()

parsed_docs$item.name %>% unique()
parsed_docs$part.name %>% unique()
# parsed_docs$raw[[1]] %>% head(100)

parsed_docs %>% glimpse()
parsed_docs %>% skimr::skim()

txt_df <- parsed_docs %>% 
  as.tibble() %>% 
  mutate( line = row_number())

my_start_line <- txt_df %>% 
  filter( str_detect( item.name , "Item 1") ) %>% 
  slice(1) %>% 
  pull(line)

my_end_line <- txt_df %>% 
  filter( line >= my_start_line  ) %>% 
  filter( str_detect( text, "Description")) %>% 
  slice(1) %>% 
  pull(line)

txt_df %>% 
  filter( between( line, my_start_line , my_end_line)) %>% 
  select(text) %>% 
  View()

parsed_docs %>% 
  as.tibble() %>% 
  mutate( line = row_number()) %>% 
  filter( line >= 804) %>% 
  filter( text )

txt_tbl <- parsed_docs$text %>% 
  as.tibble()

txt_tbl %>% head(100) %>% View()

which( txt_tbl$value == "Government Money Market Portfolio")

txt_tbl %>% 
  +   slice( 525) %>% pull()
txt_tbl %>% 
  slice( 522:540) %>% 
  View()



library( tidyverse )
library(finreportr)
AnnualReports("FB")
 
 install.packages("edgar")
 library(edgar)

getFilings(2018, 0000742212, 'N-K')

# http://rankandfiled.com/#/

 # Parsing of HTML/XML files  
    library(rvest)    

    # String manipulation
    library(stringr)   

    # Verbose regular expressions
    library(rebus)     

    # Eases DateTime manipulation
    library(lubridate)

 url <-'https://www.sec.gov/Archives/edgar/data/742212/000119312518333720/d634005dnq.htm'
 
 first_page <- read_html(url)
 
 first_page %>% html_attrs()
 
 
 page <- read_html("https://www.dndbeyond.com/monsters")

 page %>%
		html_nodes(".link") %>%
		html_attr(name = "href")
 
