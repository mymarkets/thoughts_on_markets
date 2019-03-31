
etf_download_flag <-  my_download_flag 
#  etf_download_flag <-  TRUE
# convenient function to create table

my_table_fnc <- function( tickers, names, category , df  ){
  
  if ( missing(df) ){
    
    
    df <- tribble( 
      ~ symbols , ~ names, ~ category, 
      tickers ,  names,  category ) %>% 
      unnest() 
    
  } else{
    
    df <- tribble( 
      ~ symbols , ~ names, ~ category, 
      tickers ,  names,  category ) %>% 
      unnest() %>% 
      bind_rows(
        df
      )
    
  }
  
  
  
  return(df)
  
}

# download data if the flag is true
if (etf_download_flag == TRUE) {
  
  
  #*****************************************************************
  # global asset class sector
  #******************************************************************
  
  
  tickers <- c("SPY",
               "IEV","EWJ","EEM",
               "SHY","TLT","TIP","JNK","IGOV","EMB",
               "UUP","DBC","GLD" )
  
  names <- c("S&P 500",
             "S&P Europe 350","MSCI Japan","MSCI EM",
             "Treasury 1-3 Yr","Treasury 20+ Yr ",
             "Treasury Inflation Protected","US High Yield", 
             "International Treasury Bond","Emer Mkt Bnd",
             "USD","Commodity","Gold")
  
  category <-   "Multi Asset"
  
  etf_table <-  my_table_fnc( tickers, names, category =    category  , df = NULL )
  
  
  #*****************************************************************
  # braod equity indices
  #******************************************************************
  
  
  tickers <- c("SPY","DIA", "QQQ","IWM")
  
  names <- c("S&P 500","Dow Jones", "NASDAQ 100","Russell 2000")
  
  category <-  "Broad Equity" 
  
  
  etf_table <- my_table_fnc( tickers, names, category = category , df = etf_table )
  
  #*****************************************************************
  # equity sector
  #******************************************************************
  
  tickers  <- c( "XLY","XLP","XLE","XLF","XLV","XLI","XLB","XLK","XLU")
  
  names <- c("ConsumerCyclicals","ConsumerStaples",
             "Energy","Financials","HealthCare","Industrials",
             "Materials","Technology","Utilities")
  
  category <-  "Equity Sector"
  
  
  etf_table <- my_table_fnc( tickers, names, category = category , df = etf_table )
  
  #*****************************************************************
  # equity style
  #******************************************************************
  
  tickers  <- c( "IWB","IWM","IWD","IWF","IWS","IWP","IWO","IWN")
  
  names <- c("Russell 1000","Russell 2000",
             "Russell 1000 Value","Russell 1000 Growth",
             "Russell Mid-Cap Value","Russell Mid-Cap Growth",
             "Russell 2000 Growth", "Russell 2000 Value")
  
  category <-  "Equity Style"
  
  etf_table <- my_table_fnc( tickers, names, category = category , df = etf_table )
  
  
  #*****************************************************************
  # equity region
  #******************************************************************
  
  
  tickers  <- c("IWV","IEV","EWJ","EEM")
  
  names <- c("Russell 3000",  "S&P Europe 350","MSCI Japan","MSCI EM")
  
  category <-  "Equity Region"
  
  
  etf_table <- my_table_fnc( tickers, names, category = category , df = etf_table )
  
  
  #*****************************************************************
  # fixed income sector
  #******************************************************************
  
  tickers  <- c( "AGG","SHY","IEF","TLT","TIP","JNK","IGOV","EMB")
  
  names <- c(  "Barclays Aggregate","Treasury 1-3 Yr","Treasury 7-10 Yr",
               "Treasury 20+ Yr ","Treasury Inflation Protected","US High Yield",
               "International Treasury Bond","Emer Mkt Bnd")
  
  category <-   "Fixed Sector" 
  
  
  etf_table <- my_table_fnc( tickers, names, category = category , df = etf_table )
  
  
  #*****************************************************************
  # Alternatives
  #******************************************************************
  
  tickers <- c("UUP","DBC","GLD")
  
  names <- c( "USD","Commodity","Gold")
  
  category <-  "Alternatives"
  
  etf_table <- my_table_fnc( tickers, names, category = category , df = etf_table )
  
  #*****************************************************************
  # REITS
  #******************************************************************
  
  tickers <- c("IYR","RWX")
  
  names  <- c(  "Dow Jones US Real Estate","Dow Jones International Real Estate")
  
  category <- "REIT"
  
  
  etf_table <- my_table_fnc( tickers, names, category = category , df = etf_table )
  
  

  
  #*****************************************************************
  # Trading
  #******************************************************************

  tickers  <- c(   "KRE", "IYR" ,"TIP" , "IEF" ,"JNK" ,
                   "XLY","XLP","XLE","XLF","XLV","XLI","XLB","XLK","XLU")
  
  names <- c( "Regional Banking", "US Real Estate" ,
              "Treasury Inflation Protected", "Treasury 7-10 Yr","US High Yield",
              "ConsumerCyclicals","ConsumerStaples",
              "Energy","Financials","HealthCare","Industrials",
              "Materials","Technology","Utilities")
  
  category <-  "Trading"
  
  
  etf_table <- my_table_fnc( tickers, names, category = category , df = etf_table )
  
  
  
  #*****************************************************************
  # tidyquant : Data Download
  #******************************************************************
  # devtools::install_github("mdancho84/tidyquant", build_vignettes = TRUE)
  library(tidyquant)
  
  
  
  
  etf_df <- etf_table %>% 
    distinct(symbols) %>% 
    pull() %>% 
    tq_get( get = "stock.prices") 
  
  etf_df <-   etf_df %>% 
    left_join(   etf_table,
                 by = c("symbol" = "symbols") ) 
  # rename column name
  # etf_df <- rename(etf_df, symbols = symbol.x )
  # save results for later use in case internet is not available
  
  etf_data <- list( data =  etf_df , info = etf_table) %>% 
    enframe()
  
  saveRDS( etf_data, file="raw_data/etf_data.rds")
  
  # save(etf_df, etf_table, file="Data/etf_data.RData")
  
  
} else {
  
  etf_data <-  readRDS("raw_data/etf_data.rds")
 #  load("Data/etf_data.RData")
  
}

