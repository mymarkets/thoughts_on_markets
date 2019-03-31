
#*****************************************************************
# Bloomberg Data Download
#******************************************************************



bloom_download_flag <-  my_download_flag
# bloom_download_flag <- TRUE # FALSE
telliter <- 10




if(bloom_download_flag == TRUE){
  
  
  
  prime_download_start_date <- as.Date("1950-01-01")
  
  #*****************************************************************
  # Load library
  #******************************************************************
  
  
  library(tidyverse)
  library(tidyquant)
  library(lubridate)
  library(Rblpapi) # install.packages("Rblpapi")
  # library(timeDate)
  library(timetk) # install.packages("timekit")
  
  con <- blpConnect()     # automatic if option("blpAutoConnect") is TRUE
  
  ###############################################################################
  # Process ticker information to extract relevant asset classes for analysis
  ###############################################################################
  
  equity_index_tickers <- c('SPX Index', # S&P 500 Index
                            'MID Index',  # S&P Midcap 400 Index
                            'SML Index', # S&P Smallcap 600 Index
                            'RU30INTR Index', # Russell 3000 Total Return Index
                            'RU10INTR Index', # Russell 1000 Total Return Index
                            'RU20INTR Index', # Russell 2000 Total Return Index
                            'RU30VATR Index', # Russell 3000 Total Return Value Index
                            'RU30GRTR Index', # Russell 3000 Total Return Growth Index
                            "RAY Index", # Russell 3000 Index
                            "MXWDU Index", #MSCI ACWI Excluding United States Index
                            'NDUEACWF Index', # MSCI ACWI Net TR USD Index
                            'NDDUEAFE Index', # MSCI EAFE Net TR USD Index
                            'M1WDLC Index', # MSCI ACWI Large Cap Net TR USD Index
                            'MXWDSC Index', # MSCI ACWI Small Cap Net TR USD Index
                            'M1WD000V Index', # MSCI ACWI Value Net USD Index
                            'M1WD000G Index', # MSCI ACWI Growth Net USD Index
                            'M1WOMOM Index', # MSCI World Momentum Net USD
                            'M1WD000$ Index', # MSCI ACWI MOMENTUM USD Net Total Return
                            'NDDUE15 Index', # MSCI Daily TR Net Europe USD
                            'NDDUJN Index', # MSCI Daily TR Net Japan USD
                            'NDDUPFXJ Index', # MSCI Daily TR Net Pacific Free Ex Japan USD
                            'NDDUEMEA Index', # MSCI Daily TR Emerging Markets Europe & Middle East & Africa Net USD
                            'NDUEEGF Index',# MSCI Daily TR Net Emerging Markets USD;'
                            "REIT Index", # Dow Jones Equity REIT Total Return Index
                            "S5UTIL Index", # S&P 500 Utilities Sector Index GICS Level 1
                            "S5FINL Index", # S&P 500 Financials Sector Index GICS Level 1
                            "S5INDU Index", # S&P 500 Industrials Sector Index GICS Level 1
                            "S5BANKX Index", # S&P 500 Banks Industry Group Index GICS Level 2
                            "S5RLST Index", # S&P 500 Real Estate Sector GICS Level 1
                            "IXU Index", # Utilities Select Sector Index
                            "IXM Index", # Financial Select Sector Index
                            "RAV Index", # Russell 3000 Value Index
                            "RAG Index", # Russell 3000 Growth Index
                            "ICJ Index",
                            'KCJ Index',
                            'JCJ Index',
                            "MVOLNU Index",
                            "MVOLNUA Index",
                            "NYSINYSE Index",
                            "NYSEVOL Index",
                            'INDU Index',
                            'SPX Index',
                            'CCMP Index',
                            'NYA Index',
                            'DAX Index',
                            'NKY Index',
                            'RTY Index',
                            'MXEF Index',
                            'SHCOMP Index',
                            '.BULLBEAR Index',
                            'CSFB Index',
                            "PCUSTOTL Index", # cboe total put call
                            "PCUSEQTR Index" , # cboe equity put call
                            'TRADCNYC Index',
                            'UXG6UXH6 Index',
                            'VVIX Index',
                            "VIX INDEX",
                            "UX1 Index",
                            "UX2 Index",
                            'UX4 Index',
                            'SMART Index',
                            'RU30INTR Index', # Russell 3000 Total Return Index
                            'RU10INTR Index', # Russell 1000 Total Return Index
                            'RU20INTR Index', # Russell 2000 Total Return Index
                            'RU30VATR Index', # Russell 3000 Total Return Value Index
                            'RU30GRTR Index', # Russell 3000 Total Return Growth Index
                            'NDUEACWF Index', # MSCI ACWI Net TR USD Index
                            'NDDUEAFE Index', # MSCI EAFE Net TR USD Index
                            'M1WDLC Index', # MSCI ACWI Large Cap Net TR USD Index
                            'MXWDSC Index', # MSCI ACWI Small Cap Net TR USD Index
                            'M1WD000V Index', # MSCI ACWI Value Net USD Index
                            'M1WD000G Index', # MSCI ACWI Growth Net USD Index
                            'M1WD000$ Index', # MSCI ACWI MOMENTUM USD Net Total Return
                            'NDUEEGF Index', # MSCI Emerging Net Total Return USD Index
                            "SOX Index", # Philadelphia Stock Exchange Semiconductor Index
                            "MXWD0BK Index" , # MSCI ACWI Banks Index
                            'NDUEACWF Index',# MSCI ACWI Net TR USD Index
                            'TRADPAUS Index') # 'Bloomberg Percentage of NYSE Stocks Closing Above 200 Day Moving Average'
  
  
  bond_index_tickers <- c(
    'LBUSTRUU Index', # Bloomberg Barclays US Agg Total Return Value Unhedged USD
    'LD24TRUU Index', # Bloomberg Barclays 1-3 Yr Gov Total Return Index Value Unhedged USD
    'LT01TRUU Index', # # Bloomberg Barclays US Agg 1-3 Year Total Return Value Unhedged USD
    'LU71TRUU Index', # Bloomberg Barclays US Agg 7-10 Year Total Return Value Unhedged USD
    'LD20TRUU Index', # Bloomberg Barclays US Treasury Bills Total Return Index Value Unhedged USD
    'LT01TRUU Index', # Bloomberg Barclays U.S. Treasury: 1-3 Year Total Return Index Value Unhe..
    'LT09TRUU Index', # Bloomberg Barclays U.S. Treasury: 7-10 Year TR Index Value Unhedged USD
    'LT08TRUU Index', # Bloomberg Barclays U.S. Treasury: Intermediate TR Index Value Unhedged...
    'LT11TRUU Index', # Bloomberg Barclays U.S. Treasury: 20+ Year Total Return Index Value
    'BXIIUSTP Index', # Barclays US Treasury 2Y/10Y Yield Curve Index
    'LUTLTRUU Index', # Bloomberg Barclays US Agg Long Treasury Total Return Index Value
    'LUGITRUU Index', # Bloomberg Barclays US Government Intermediate TR Index Value Unhedged...
    'LGL1TRUU Index', # Bloomberg Barclays U.S. Government: Long Total Return Index Value Unhed..
    'LUATTRUU Index', # Bloomberg Barclays US Treasury Total Return Unhedged USD
    'LF98TRUU Index', # Bloomberg Barclays US Corporate High Yield Total Return Index Value Unhe..
    'LBUTTRUU Index', # Bloomberg Barclays US Treasury Inflation Notes TR Index Value Unhedged USD
    'LEGATRUU Index',  # Bloomberg Barclays GlobalAgg Total Return Index Value Unhedged USD
    'LF94TRUU Index', # Bloomberg Barclays Global Inflation-Linked Total Return Index Value Unhedg...
    'LG30TRUU Index', # Bloomberg Barclays Global High Yield Total Return Index Value Unhedged USD
    'LUACTRUU Index', # Bloomberg Barclays US Corporate Total Return Value Unhedged USD
    'LUMSTRUU Index', # Bloomberg Barclays US MBS Index Total Return Value Unhedged USD
    'LTXUTRUU Index', # Bloomberg Barclays Global Treasury ex-U.S. Capped TR Index Value Unhedged USD
    'LG30TRUH Index', # Bloomberg Barclays Global High Yield Total Return Index Value Hedged USD
    'LGT1TRUH Index', # Bloomberg Barclays Global Treasury x US Total Return Index Value Hedged USD
    'LF94TRUH Index', # Bloomberg Barclays Global Inflation-Linked Total Return Index Value Hedged
    'JPEICORE Index', # J.P. Morgan EMBI Global Core
    'LT08TRUU Index', # Bloomberg Barclays U.S. Treasury: Intermediate TR Index Value Unhedged...
    'LF98TRUU Index', # Bloomberg Barclays US Corporate High Yield Total Return Index Value Unhe..
    'LBUTTRUU Index', # Bloomberg Barclays US Treasury Inflation Notes TR Index Value Unhedged USD
    'LUACTRUU Index', # Bloomberg Barclays US Corporate Total Return Value Unhedged USD
    'LUMSTRUU Index', # Bloomberg Barclays US MBS Index Total Return Value Unhedged USD
    'JPEICORE Index',
    'LTXUTRUU Index', # Bloomberg Barclays Global Treasury ex-U.S. Capped TR Index Value Unhedged USD
    "MOODCBAA Index",
    "MOODCAAA Index",
    "JPEICORE Index", # 
    'LBUSTRUU Index', # Bloomberg Barclays US Agg Total Return Value Unhedged USD
    'LD20TRUU Index', # Bloomberg Barclays US Treasury Bills Total Return Index Value Unhedged USD
    'LT01TRUU Index', # Bloomberg Barclays U.S. Treasury: 1-3 Year Total Return Index Value Unhe..
    'LT09TRUU Index', # Bloomberg Barclays U.S. Treasury: 7-10 Year TR Index Value Unhedged USD
    'LT08TRUU Index', # Bloomberg Barclays U.S. Treasury: Intermediate TR Index Value Unhedged...
    'LT11TRUU Index', # Bloomberg Barclays U.S. Treasury: 20+ Year Total Return Index Value
    'BXIIUSTP Index', # Barclays US Treasury 2Y/10Y Yield Curve Index
    'LUATTRUU Index', # Bloomberg Barclays US Treasury Total Return Unhedged USD
    'LF98TRUU Index', # Bloomberg Barclays US Corporate High Yield Total Return Index Value Unhe..
    'LBUTTRUU Index', # Bloomberg Barclays US Treasury Inflation Notes TR Index Value Unhedged USD
    'LEGATRUU Index',  # Bloomberg Barclays GlobalAgg Total Return Index Value Unhedged USD
    'LG30TRUU Index', # Bloomberg Barclays Global High Yield Total Return Index Value Unhedged USD
    'LUACTRUU Index', # Bloomberg Barclays US Corporate Total Return Value Unhedged USD
    'LUMSTRUU Index', # Bloomberg Barclays US MBS Index Total Return Value Unhedged USD
    'LTXUTRUU Index', # Bloomberg Barclays Global Treasury ex-U.S. Capped TR Index Value Unhedged USD
    'LG30TRUH Index', # Bloomberg Barclays Global High Yield Total Return Index Value Hedged USD
    'LGT1TRUH Index', # Bloomberg Barclays Global Treasury x US Total Return Index Value Hedged USD
    'LF94TRUH Index', # Bloomberg Barclays Global Inflation-Linked Total Return Index Value Hedged
    "BCOMRST Index" , # Bloomberg Roll Select Commodity Total Return Index
    "BCIT5Y Index"  ,# Bloomberg Barclays US Inflation Linked 7 to 10 Years Avg Real Yield Annual
    'JPEICORE Index',
    "USGG3M Index",
    "USGG10YR Index",
    "FAILTRED Index",
    'GDBR10 Index',
    'GUKG10 Index',
    'GJGB10 Index',
    "GCAN10YR Index",
    "GACGB10 Index",
    "GCNY10YR Index",
    'GT2 Govt',
    'GT5 Govt',
    'GT10 Govt',
    'GT30 Govt',
    'H15T2Y Index',
    'CDX HY CDSI GEN 5Y SPRD Corp',
    'CDX IG CDSI GEN 5Y Corp' ,
    "DB CDS EUR SR 5Y D14 Corp" ,
    "DB CDS EUR SR 1Y D14 Corp",
    "MOVE Index", # Merrill Lynch Option Volatility Estimate MOVE Index
    'FEDL01 Index')
  
  
  alt_index_tickers <- c(
    'DWRTFT Index', # Dow Jones US Select REIT Total Return Index
    'DWGRSN Index', # Dow Jones Global Select Real Estate Securities Total Return Net Index
    'BCOMTR Index', # Bloomberg Commodity Index Total Return,
    'FXCARRSP Index', # D e u t s c h e   B a n k   G 1 0   F X   C a r r y   B a s k e t   S p o t
    'SPGTIND Index', # S&P Global Infrastructure Index
    'XAU BGN Curncy',
    'DWRTFT Index', # Dow Jones US Select REIT Total Return Index
    'DWGRSN Index', # Dow Jones Global Select Real Estate Securities Total Return Net Index
    'BCOMTR Index', # Bloomberg Commodity Index Total Return,
    'FXCARRSP Index', # D e u t s c h e   B a n k   G 1 0   F X   C a r r y   B a s k e t   S p o t
    'SPGTIND Index', # S&P Global Infrastructure Index
    'XAU BGN Curncy' ,
    'DXY Curncy',
    "BBDXY Index" ,
    "BBDXT Index")# Gold
  
  
  
  commodity_tickers <- c("GC1 Comdty","GC4 Comdty","UX1 Index","UX2 Index","UX4 Index",
                         "CL1 Comdty",'BCOMTR Index', 'XAU BGN Curncy', 
                         "CL1 Comdty",   "CL4 Comdty","SPGSCI Index",
                         "XBTUSD BGN Curncy" ,"XLCUSD BGN Curncy","XETUSD BGN Curncy")
  
  
  economic_tickers <- c('CFNAI Index', #Chicago Fed National Activity Index
                        "NAPMPMI Index","GDGCAFJP Index","CESIUSD Index","CESIEUR Index",
                        "CESIJPY Index","CESIGBP Index","OUTFGAF Index",
                        "BDIY INDEX",          "DGNOTOT Index",         "DGSHTOT Index",
                        'IP YOY Index',
                        'GDP CURY Index',
                        'EHGDUSY Index',
                        'EHGDEUY Index',
                        'EHGDJPY Index',
                        'CNGDPYOY Index',
                        'EHGDCNY Index',
                        'USHEYOY Index', # average hourly earnings
                        'CPI YOY Index',
                        'CPI XYOY Index',
                        "CPI INDX Index",
                        "PCE CYOY Index",
                        'FARBAST Index',
                        'DOETCRUD Index',
                        "AHE YOY% Index", # US Average Hourly Earnings All Employees Total Private Yearly Percent Change SA
                        "ECI YOY Index", # Bureau of Labor Statistics Employment Cost Civilian Workers YoY NSA
                        "NOWC17Q1 Index",
                        "GDGCAFJP Index",
                        "GDP CQOQ Index",
                        "CONCCONF Index", # Conference Board Consumer Confidence SA 1985=100
                        'MC MB Index','RR10CUS Index','RR2YCUS Index' ) # 'GDP CUR$Index',
  
  
  
  cot_tickers <- c(
    "NYC2UNCN Index", # DXY non-com
    "NYC2UCON Index",# DXY com
    "CVXFTNCN Index", # vix  non-com
    "CVXFTCON Index", # vix  com
    "NYM1CNCN Index", # WTI non-com
    "NYM1CCON Index", # WTI com
    "IMM0ENCN Index", # ES non-com
    "IMM0ECON Index", # ES com
    "CBT4TNCN Index", # TY 10 Year non-com
    "CBT4TCON Index" ,# TY 10 Year com
    "CEI1GNCN Index", # gold non-com
    "CEI1GCON Index" ) # GOLD  com
  
  
  us_swap_spread_tickers <- c(
    "USSP2 CMPN Curncy",
    "USSP5 CMPN Curncy",
    "USSP10 CMPN Curncy",
    "USSP30 CMPN Curncy")
  
  libor_ois_spread_tickers <- c("US0003M Index",
                                "USSOC Curncy",
                                'GB03 Govt',
                                "BASPTDSP Index",
                                ".USLIBOIS Index",
                                ".EULIBOIS Index",
                                ".JPLIBOIS Index",
                                ".SFLIBOIS Index")
  
  eurodollar_tickers <- c('ED1 COMB Comdty',
                          'ED2 COMB Comdty',
                          'ED3 COMB Comdty',
                          'ED4 COMB Comdty',
                          'ED5 COMB Comdty',
                          'ED6 COMB Comdty',
                          'ED7 COMB Comdty',
                          'ED8 COMB Comdty',
                          'ED9 COMB Comdty',
                          'ED10 COMB Comdty',
                          'ED11 COMB Comdty',
                          'ED12 COMB Comdty')
  
  
  
  
  
  cross_currency_basis_1year_tickers <- c("EUBS1 CMPN Curncy",
                                          "JYBS1 CMPN Curncy",
                                          "BPBS1 CMPN Curncy",
                                          "CCBS1 CMPN Curncy",
                                          "CGUSSW1 CMPN Curncy",
                                          "CDBS1 CMPN Curncy",
                                          "ADBS1 CMPN Curncy",
                                          "SFBS1 CMPN Curncy")
  
  cross_currency_basis_5year_tickers <- c("EUBS5 CMPN Curncy",
                                          "JYBS5 CMPN Curncy",
                                          "BPBS5 CMPN Curncy",
                                          "CCBS5 CMPN Curncy",
                                          "CGUSSW5 CMPN Curncy",
                                          "CDBS5 CMPN Curncy",
                                          "ADBS5 CMPN Curncy",
                                          "SFBS5 CMPN Curncy")
  
  
  cross_currency_basis_tickers <- c(cross_currency_basis_1year_tickers,
                                    cross_currency_basis_5year_tickers)
  
  currency_tickers <- c(
    "USTWBROA Index", # US Trade Weighted Broad Dollar
    'EURJPY BGN Curncy',
    'ADXY Index',
    'FXJPEMCS Index',
    "USTWBROA Index",
    'DXY Curncy',
    'XAU BGN Curncy',
    'CNY BGN Curncy',
    'EUR BGN Curncy',
    'JPY BGN Curncy',
    'GBP BGN Curncy',
    'CAD BGN Curncy',
    'AUD BGN Curncy',
    'CHF BGN Curncy',
    'CNH BGN Curncy',
    'KRW BGN Curncy',
    'SAR BGN Curncy')
  
  breakeven_inflation_tickers = c('USGG5Y5Y Index',
                                  'USGGBE01 Index',
                                  'USGGBE02 Index',
                                  'USGGBE03 Index',
                                  'USGGBE05 Index',
                                  'USGGBE07 Index',
                                  'USGGBE10 Index',
                                  'USGGBE20 Index',
                                  'USGGBE30 Index')
  
  
  
  us_govt_yld_tickers <- c('USGG1M Index',
                           'USGG3M Index',
                           'USGG6M Index',
                           'USGG12M Index',
                           'USGG2YR Index',
                           'USGG3YR Index',
                           'USGG5YR Index',
                           'USGG7YR Index',
                           'USGG10YR Index',
                           'USGG30YR Index')
  
  
  us_gov_ryld_tickers <-c('GTII05 Govt',
                          'CTII10 Govt',
                          'GTII30 Govt')
  
  
  us_mm_yld_tickers <-c('USRGCY1T CMPN Curncy', # USD Overnight GC CMO Agency Repo
                        'USRGCGC CMPN Curncy') # USD 3 Month GC Govt Repo
  
  
  
  cb_tickers = c( 'MECIGLT Index',# BofA Merrill Lynch Global Liquidity Tracker
                  'VELOM2 Index',
                  'M2 Index',
                  'FARWTSRF Index', # US total bank balance sheet
                  'GDP CUR$ Index',  # GDP US Nominal Dollars SAAR ; Billions
                  'ECOXEAS Index',# Eurozone Nominal GDP (Billion USD) SA
                  'ECOXUKS Index', #  UK Nominal GDP (Billion USD) SA ; US DOLLAR
                  'ECOXJPS Index', # Japan Nominal GDP (Billion USD) SA  ; US DOLLAR
                  'ECOXCNN Index', # China Nominal GDP (Billion USD) NSA ; US DOLLAR
                  'CNBMTTAS Index', # China Central Bank Balance Sheet - Total Assets ; CNY; Billions
                  'EBBSTOTA Index',
                  'ECMAM2YY Index',
                  'EUGDEMU Index',
                  'CNNGPQ$ Index',
                  'CNMS2YOY Index', # money supply
                  'CNLNTTFU Index', # bank total assets
                  'JGDOSGDP Index',
                  'JMNSM2Y Index', # M2
                  'BJACTOTL Index', # JPY; Billions
                  'ECMAM2 Index','CNMSM2 Index','JMNSM2SA Index') # bank total asset
  
  
  us_bond_fut_tickers <- c('TU1 COMB Comdty', # 2-Year US Treasury Note Futures
                           'FV1 COMB Comdty', # 5-Year US Treasury Note Futures
                           'TY1 COMB Comdty', # 10-Year US Treasury Note Futures
                           'US1 COMB Comdty') # # U.S. Treasury Long Bond Futures
  
  global_yld_tickers <- c(
    # Japan - 
    "GJTB3MO Index" ,
    # US Generic Govt 3 Month Yield
    "USGG3M Index" , 
    # USD 3 Month Deposit
    "USDRC CMPN Curncy" , 
    # Euribor 3 Month ACT/360
    "EUR003M Index" , 
    # EUR 3 Month Deposit 
    "EUDRC Curncy" , 
    # Germany 3 Month Bubill Maturing in 3 Month
    "GETB1 Index" , 
    # ICE LIBOR GBP 3 Month
    "BP0003M Index" , 
    # GBP 3 Month Deposit 
    "BPDRC CMPN Curncy" , 
    # Japan Treasury Discount Bills 3 Month
    "GJTB3MO Index" , 
    # ICE LIBOR JPY 3 Month
    "JY0003M Index" , 
    # JPY 3 Month Deposit 
    "JYDRC CMPN Curncy" , 
    #   Canadian Govt Bonds 3 Month Bill   
    "GCAN3M Index" , 
    # AUD 3 Month Deposit 
    "ADDRC Curncy" , 
    # Australia Govt Bonds Generic Bid Yield 3 Month
    "GACGB3M Index" , 
    # France Treasury Bills 3 Month Intraday
    "GBTF3MO Index" 
  )
  
  all_tickers <- c( equity_index_tickers , bond_index_tickers,alt_index_tickers,
                    commodity_tickers, economic_tickers,cot_tickers,
                    us_swap_spread_tickers, libor_ois_spread_tickers, eurodollar_tickers,
                    cross_currency_basis_tickers, currency_tickers,
                    breakeven_inflation_tickers, us_govt_yld_tickers, us_gov_ryld_tickers,
                    us_mm_yld_tickers,cb_tickers , us_bond_fut_tickers ,global_yld_tickers )
  
  
  all_tickers <- all_tickers %>% unique()
  
  
  ###############################################################################
  # Update Data
  ###############################################################################
  
  
  
  
  bl_data_list <- list()
  
  
  bl_tickers <- all_tickers %>% unique
  
  
  
  for (i in 1:length(bl_tickers)){
    #do some cool statistics
    if( i %% telliter == 0 ) cat(paste("iteration", i, "complete\n"))
    
    temp_ticker <- bl_tickers[i]
    
    # check if this is a new ticker
    
    bl_data_list[[temp_ticker]] -> test_new_ticker
    
    # if it does not exist
    if(is.null(test_new_ticker) == TRUE){
      # download full data
      
      temp_ticker_ <- gsub(" ",".",temp_ticker)
      temp_start_Date <- prime_download_start_date
      temp_end_Date <-  as.Date(Sys.Date())
      
      temp_data <- bdh(securities = temp_ticker ,
                       fields = "PX_LAST",
                       start.date =  temp_start_Date,end.date = temp_end_Date,
                       include.non.trading.days = FALSE)
      
      temp_data %>%
        #set_names( nm = c("date", temp_ticker)) %>%
        tbl_df() ->  temp_data
      
      #bl_data_list[[temp_ticker_]] <-  temp_data
      bl_data_list[[temp_ticker]] <-  temp_data
      
      temp_data <- NULL
      
    }
    
    
  }
  
  # temp_names <- names(bl_data_list)
  
  # names(bl_data_list) <- str_replace_all(temp_names,"\\.", " ")
  
  # check data that were not downloaded
  x <- sort(bl_tickers)
  y <- sort(names(bl_data_list))
  ## True for all possible x & y :
  test <- setequal( union(x, y),
                    c(setdiff(x, y), intersect(x, y), setdiff(y, x)))
  
  if (test == FALSE) stop("Not all data are downloaded!")
  
  bl_all_df <- bl_data_list %>%
    map(purrr::set_names, nm = c("dates","values")) %>%
    bind_rows(.id = "symbol") 
  
  
  bloomberg_data <- list( data =  bl_all_df ,
                          raw_data =  bl_data_list,
                          info = bl_tickers ) %>% 
    enframe()
  
  
  saveRDS( bloomberg_data, file="raw_data/bloomberg_data.rds")
  
} else{
  
  
  bloomberg_data<-  readRDS("raw_data/bloomberg_data.rds")
  
  
  
}


