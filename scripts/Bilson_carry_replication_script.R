
library( tidyverse )
library( tidyquant )
library(readxl )
library(lubridate)
library("hrbrthemes")
#   ____________________________________________________________________________
#  Read raw data                                                     ####

file_name <- "C:/Users/hyungjin/Documents/Trading/mymarkets.github.io/docs/International Finance Theory/Section 5/Carry-1.xlsx"

data_bank <- read_excel( path = file_name
          , sheet = "DataBank"
          , range = "A19:AQ156"
          , col_names = TRUE)
data_bank %>% glimpse()

data_bank <-  data_bank %>%
  mutate( year  = str_sub( Date, start = 1, end = 4) 
         ,quarter = str_sub( Date, start = 6, end = 7) 
         ,dates = str_c(year, quarter, sep = "-")
         ,dates =  as.yearqtr(dates )) %>% 
  select( - year, -quarter)

data_bank


carry_data <- read_excel( path = file_name
                         , sheet = "Carry-1"
                         , range = "A30:Z167"
                         , col_names = TRUE)
carry_data %>% glimpse()

carry_data <- carry_data %>%
  mutate( year  = str_sub( Date, start = 1, end = 4) 
          ,quarter = str_sub( Date, start = 6, end = 7) 
          ,dates = str_c(year, quarter, sep = "-")
          ,dates =  as.yearqtr(dates )) %>% 
  select( - year, -quarter)

carry_data

#   ____________________________________________________________________________
#  process data                                                   ####

fx_df <- data_bank %>% 
  select(dates, 2:7) %>% 
  set_names(
    nm = names(.) %>% str_remove( pattern = "\\..*")

  ) %>% 
  print()

fx_df %>% 
  gather( variables, values, - dates ) %>% 
  ggplot() +
  geom_line( aes( x = dates, y = values, color = variables )) +
  theme_ipsum() +
  NULL

short_rates_df <- data_bank %>% 
  select(dates, 8:14) %>% 
  set_names(
    nm = names(.) %>% str_remove( pattern = "\\..*")
    
  ) %>% 
  print()

short_rates_df %>% 
gather( variables, values, - dates ) %>% 
  ggplot() +
  geom_line( aes( x = dates, y = values, color = variables )) +
  theme_ipsum() +
  NULL

long_rates_df <- data_bank %>% 
  select(dates, 15:21) %>% 
  set_names(
    nm = names(.) %>% str_remove( pattern = "\\..*")
    
  ) %>% 
  print()


long_rates_df %>% 
  gather( variables, values, - dates ) %>% 
  ggplot() +
  geom_line( aes( x = dates, y = values, color = variables )) +
  theme_ipsum() +
  NULL


cpi_df <- data_bank %>% 
  select(dates, 22:28) %>% 
  set_names(
    nm = names(.) %>% str_remove( pattern = "\\..*")
    
  ) %>% 
  print()

cpi_df  %>% 
  gather( variables, values, - dates ) %>% 
  ggplot() +
  geom_line( aes( x = dates, y = values, color = variables )) +
  theme_ipsum() +
  NULL

ppi_df <- data_bank %>% 
  select(dates, 29:34) %>% 
  set_names(
    nm = names(.) %>% str_remove( pattern = "\\..*")
    
  ) %>% 
  print()

ppi_df  %>% 
  gather( variables, values, - dates ) %>% 
  ggplot() +
  geom_line( aes( x = dates, y = values, color = variables )) +
  theme_ipsum() +
  NULL

#   ____________________________________________________________________________
#  Replicate Carry tab                                                ####

# Excess Returns
fx_rets_df <-  fx_df %>% 
  mutate( dates = as.Date(dates)) %>% 
  gather( variables, values, - dates) %>% 
  group_by( variables ) %>% 
  tq_mutate( select = values,
             mutate_fun = ROC,
             col_rename = "returns",
             type = "discrete") %>%
  ungroup() %>% 
  select(dates, variables, returns) %>% 
  drop_na( returns ) %>% 
  #spread(variables, returns) %>% 
  print()

carry_df <- short_rates_df %>% 
  mutate( dates = as.Date(dates)) %>%
  gather( variables, values, -dates,-USD) %>% 
  mutate( carry = values - USD,
          carry = carry/4) %>% 
  group_by( variables) %>% 
  mutate( values = dplyr::lag( values, 1)) %>% 
  ungroup() %>% 
  print()

fx_rets_df <- fx_rets_df %>% 
  inner_join(
    short_rates_df %>% 
      mutate( dates = as.Date(dates)) %>% 
      gather( variables, values, - dates) 
    , by = c("variables", "dates")
  ) %>% 
  group_by( dates, variables ) %>% 
  mutate( excess_returns = returns *( 1+ values/4)) %>% 
  ungroup() %>% 
  select(dates,variables, excess_returns) %>% 
  print()


fx_rets_df %>% 
  inner_join(
    carry_df %>% 
      select(dates, variables, carry),
    by = c("variables","dates")
  ) %>% 
  mutate( excess_returns = excess_returns + carry)

