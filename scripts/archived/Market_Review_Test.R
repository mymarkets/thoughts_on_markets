
# http://www.business-science.io/business-science-labs/2018/05/31/backtesting-quantopian-zipline-tibbletime-furrr-flyingfox.html

#   ____________________________________________________________________________
#   Engineer Features Function                                              ####

print("Loading packages and functions.")


# https://cvxr.rbind.io/post/examples/cvxr_using-other-solvers/


# https://www.tradingview.com/script/Q1O23zJP-20-years-old-turtles-strategy-still-work/

# library(conflicted)
library(tidyverse)
library(tibbletime)
library(stringr)
library(furrr)
# library(flyingfox)
library(tidyquant)
library(ggrepel)
library(hrbrthemes)
library(nord)

library(recipes)
library(rsample)

source("scripts/functions/momentum_function.R")


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Load Data                                                ####

print("Load Data.")

# file names
file_names <- c(#"crypto_raw_data" ,
  #"coinmkt_data" ,
  #"btc_data" ,
  #"bitcoin_google_data" ,
  "etf_data")

my_files <- file_names %>%
  str_c("raw_data/", ., ".rds") %>%
  # load data
  map( readRDS ) %>%
  set_names(file_names )


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Pull Data                                                 ####

select_data <-   "etf_data" # "crypto_raw_data"

select_data <- my_files[[select_data]]

select_data %>% glimpse()

select_data <- select_data # %>% dplyr::filter(category %in% "Multi Asset" )

select_data %>% glimpse()
select_data %>% skimr::skim()

select_data %>% tail()
##  ............................................................................
##  Create Rolling data          


select_data %>% distinct(names) %>% pull()

my_end_date <- select_data$date %>% max( na.rm = TRUE)

my_start_date <- my_end_date - weeks(52*1)

select_names <- c("Treasury 20+ Yr ","US High Yield")

select_data %>%
  filter( names %in% select_names ) %>% 
  filter( between( date, my_start_date  , my_end_date ) ) %>%
  ggplot(aes(x = date, y = close, volume = volume, group = symbol)) +
  geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
  geom_ma(ma_fun = VWMA, n = 15, wilder = TRUE, linetype = 5) +
  geom_ma(ma_fun = VWMA, n = 50, wilder = TRUE, color = "red") + 
  labs(title = "Fixed Income Class Review", 
       subtitle = "50 and 200-Day EMA", 
       y = "Closing Price", x = "") + 
  coord_x_date(xlim = c(my_start_date, my_end_date)) +
  facet_wrap(~ names, ncol = 1, scales = "free_y") + 
  theme_ipsum()


select_data %>%
  filter( names %in% select_names ) %>% 
  filter( between( date, my_start_date  , my_end_date ) ) %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "daily", 
               col_rename = "daily_returns") %>% 
  na.omit() %>% 
  mutate( performance = cumprod( 1+ daily_returns)) %>% 
  ungroup() %>% 
  ggplot(aes(x = date, y = performance , color = symbol)) +
  geom_line( size = 1.2 ) +
  scale_color_manual( values =  c( nord_palettes$aurora[1], nord_palettes$frost[4] )) +
  labs(title = "Asset Class Momentum Review", 
       subtitle = "Relative Performance Comparison", 
       y = "Cumulative Performance", x = "") + 
  coord_x_date(xlim = c(my_start_date, my_end_date)) +
  theme_ipsum()  +
  theme( legend.position=c(.7,.85) ,
         legend.title = element_blank(),
         legend.direction = "vertical")


select_data %>%
  filter( symbol %in% c("EEM","EWJ", "IEV", "SPY") ) %>% 
  filter( between( date, my_start_date  , my_end_date ) ) %>%
  ggplot(aes(x = date, y = close, volume = volume, group = symbol)) +
  geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
  geom_ma(ma_fun = VWMA, n = 15, wilder = TRUE, linetype = 5) +
  geom_ma(ma_fun = VWMA, n = 50, wilder = TRUE, color = "red") + 
  labs(title = "Asset Class Momentum Review", 
       subtitle = "50 and 200-Day EMA, Experimenting with Multiple Stocks", 
       y = "Closing Price", x = "") + 
  coord_x_date(xlim = c(my_start_date, my_end_date)) +
  facet_wrap(~ symbol, ncol = 2, scales = "free_y") + 
  theme_ipsum()
