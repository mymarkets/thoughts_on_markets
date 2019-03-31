
library( tidyverse )
library( tidyquant )
library(Quandl)

auth_token <- 'cRKygqsgVqfFWr82euz2'      # Sign up with Quandl to get a token
auth_token <- 'kYiiDrxrKsbBb9Vk8Dqu'
Quandl.api_key(auth_token)


bis_metadata <- readr::read_csv("raw_data/BIS_metadata.csv")


#   ____________________________________________________________________________
#  Creat key table                                                      ####

bis_key_tbl <- "raw_data/BIS_nomenclature.csv" %>% readr::read_csv() 
bis_key_tbl <- bis_key_tbl %>% mutate( lines = row_number())



empty_lines <- bis_key_tbl %>% filter( is.na(.[[1]]) & is.na(.[[2]])) %>% pull()
# add list row

empty_lines <- empty_lines %>% c( nrow(bis_key_tbl ))

#   ____________________________________________________________________________
#  Creat key table                                                      ####


bis_key_lst <- list()


##  ............................................................................
##  prepare first table                                              ####

temp_tbl <- bis_key_tbl %>% 
  filter( between( lines, 1, empty_lines[1] -1 ) ) %>% 
  select(1:2) 

bls_temp_name <- colnames(temp_tbl)[[1]] 

colnames( temp_tbl ) <- c("variables" , "values")


bis_key_lst[[ bls_temp_name  ]] <- temp_tbl

##  ............................................................................
##  collect tables                                              ####


for (i in 2:( length( empty_lines )-1)){
  
  
  temp_tbl <- bis_key_tbl %>% 
    filter( between( lines, empty_lines[i] + 1, empty_lines[i+1] -1 ) )
  
  
  temp_tbl <- temp_tbl %>% 
    slice(-1) %>% 
    select(1:2) %>% 
    set_names(
      
      nm =   temp_tbl %>% slice(1) %>% select(1:2) %>% unlist( use.names = FALSE) %>% c() 
      
    )
  
  bls_temp_name <- colnames(temp_tbl)[[1]] 
  
  
  colnames( temp_tbl ) <- c("variables" , "values")
  
  bis_key_lst[[  bls_temp_name  ]] <- temp_tbl
  
}


##  save                      
bis_key_tbl <- bis_key_lst %>% 
  bind_rows( .id = "category")
# check
bis_key_tbl %>% distinct( category )

bis_key_tbl %>% 
  filter( category %>% 
            str_detect( fixed( "Currency", ignore_case=TRUE) )
  ) %>% 
  View()


policy_rates_data <- bis_metadata %>% 
  filter( 
    name  %>% str_detect( pattern = "Policy Rate")
  ) %>% 
  distinct( code ) %>% 
  filter( code %>% str_detect( "PD_D")) %>% 
  mutate( code = str_c("BIS/", code)) %>% 
  pull() %>% 
  tq_get(  get  = "quandl"  ) 


policy_rates_data %>% 
  group_by( symbol ) %>% 
  skimr::skim()

library( hrbrthemes )

policy_rates_data %>%
  # mutate(date = as.Date(as.yearmon(date))) %>%
  filter(  
    symbol %>% str_detect( pattern = "(US|EU|JP|GB|CA|SE|CH)")
  )    %>% 
  filter( between( date, today()-years(3), today())) %>% 
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "grey70", size = 0.02) +
  geom_line( aes(date,  value, color = symbol ), show.legend = FALSE ) +
  facet_wrap(~  symbol) +
  theme_ipsum() +
  theme(panel.grid = element_blank()) +
  labs(x = NULL, y = NULL,
       title = "Central bank policy rates",
       subtitle = "% per annum")



bis_metadata %>% 
  filter( 
    name  %>% str_detect( pattern = "Eff")
  )
