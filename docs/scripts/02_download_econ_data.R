
econ_download_flag <- my_download_flag 
#  econ_download_flag <- TRUE

if(econ_download_flag == TRUE){
  
  #####################################################################################
  ## Step 0: Load Libraries ##
  #####################################################################################
  library(tidyverse)
  library(tidyquant)
  library(Quandl)
  # Load the Quandl API token you want to use (from a local file).
  
  auth_token <- 'cRKygqsgVqfFWr82euz2'      # Sign up with Quandl to get a token
  auth_token <- 'kYiiDrxrKsbBb9Vk8Dqu'
  Quandl.api_key(auth_token)
  
  #####################################################################################
  ## Step 1: Prepare for data  ##
  #####################################################################################
  
  
  # install.packages(c("rgeos","maptools","albersusa","ggalt","viridis","readxl","htmlTable","animation","tweenr"))
  #
  
  # Real Gross Domestic Product (GDPC1)
  # Real Government Consumption Expenditures and Gross Investment  GCECA  Current                Bil. of Chn. 2009 $             A                NSA       2016-07-29
  # Real Gross Private Domestic Investment           GPDICA                Current                Bil. of Chn. 2009 $             A             NSA                2016-07-29
  # Real Net Exports of Goods and Services             NETEXC                Current                Bil. of Chn. 2009 $             Q             SAAR                2016-08-26
  # Real Personal Consumption Expenditures         PCECC96              Current                Bil. of Chn. 2009 $             Q             SAAR                2016-08-26
  
  ###### Download GDP contributions data ######
  
  # https://fred.stlouisfed.org/release/tables?rid=53&eid=38375
  # https://fred.stlouisfed.org/release/tables?rid=53&eid=13562 - Right one
  # Gross Domestic Product (A191RP1Q027SBEA)
  
  # Real Gross Domestic Product (A191RL1Q225SBEA)
  # Personal consumption expenditures (DPCERY2Q224SBEA)
  # Personal consumption expenditures: Goods (DGDSRY2Q224SBEA)
  # Personal consumption expenditures: Services (DSERRY2Q224SBEA)
  # Real Gross Private Domestic Investment (A006RY2Q224SBEA)
  # Real Gross Private Domestic Investment: Fixed Investment (A007RY2Q224SBEA)
  # Real Gross Private Domestic Investment: Fixed Investment: Nonresidential (A008RY2Q224SBEA)
  # Real Gross Private Domestic Investment: Fixed Investment: Residential (A011RY2Q224SBEA)
  # Gross private domestic investment: Change in private inventories (A014RY2Q224SBEA)
  # Net exports of goods and services (A019RY2Q224SBEA)
  # Government consumption expenditures and gross investment (A822RY2Q224SBEA)
  
  
  ###### Download norminal data ######
  
  # Gross Domestic Product (A191RP1Q027SBEA)
  # https://fred.stlouisfed.org/series/A191RP1Q027SBEA
  # Industrial Production Index (INDPRO)
  
  
  
  
  
  
  #####################################################################################
  ## GDP Data
  #####################################################################################
  
  # Employment Cost Index: Wages and Salaries: Private Industry Workers (ECIWAG)
  
  
  
  tickers  <-c('GDPC96',
               'A191RL1Q225SBEA',
               'DPCERY2Q224SBEA',
               'DGDSRY2Q224SBEA',
               'DSERRY2Q224SBEA',
               'A006RY2Q224SBEA',
               'A007RY2Q224SBEA',
               'A008RY2Q224SBEA',
               'A011RY2Q224SBEA',
               'A014RY2Q224SBEA',
               'A019RY2Q224SBEA',
               'A822RY2Q224SBEA')
  
  
  myvars  <-c( 'Annual Real GDP Growth',
               'Real Gross Domestic Product',
               'Personal Consumption Expenditures',
               'Personal Consumption Expenditures - Goods',
               'Personal Consumption Expenditures - Services',
               'Private Domestic Investment',
               'Fixed Investment',
               'Nonresidential Fixed Investment',
               'Residential Fixed Investment',
               'Private Inventories',
               'Net exports of goods and services',
               'Government')
  
  
  gdp_tbl <- tibble( symbol = tickers , variables = myvars, category  = "GDP")
  
  
  
  #####################################################################################
  ## GDPNow - Sources > Federal Reserve Bank of Atlanta
  #####################################################################################
  # GDPNow
  # https://fred.stlouisfed.org/release?rid=386
  # https://www.frbatlanta.org/cqer/research/gdpnow.aspx?panel=1
  
  # Contributions to percent change in GDPNow:
  
  tickers  <-c('GDPNOW' , #Percent Change at Annual Rate, Quarterly, Seasonally Adjusted Annual Rate
               'PCECONTRIBNOW', #  Real Personal Consumption Expenditures
               'PCEGOODSCONTRIBNOW', #  Real Personal Consumption Expenditures: Goods
               'PCESERVICESCONTRIBNOW',#  Real Personal Consumption Expenditures: Services
               'GDPICONTRIBNOW', #  Real Gross Private Domestic Investment 
               'FIXINVESTCONTRIBNOW', #  Real Gross Private Domestic Investment: Fixed Investment 
               'BUSFIXINVESTCONTRIBNOW', # Real Gross Private Domestic Investment: Fixed Investment: Business
               'RESCONTRIBNOW', # Real Gross Private Domestic Investment: Fixed Investment: Residential
               'CHNGNETINVENTCONTRIBNOW', #  Real Change of Inventory Investment
               'CHNGNETEXPORTSCONTRIBNOW', #  Real Change of Net Exports of Goods and Services
               'GOVCONTRIBNOW') # Real Gross Government Investment
  
  # 'EQUIPCONTRIBNOW', # Real Gross Private Domestic Investment: Fixed Investment: Business: Equipment
  # 'STRUCTCONTRIBNOW', #  Real Gross Private Domestic Investment: Fixed Investment: Business: Structures
  #            'EXPORTSCONTRIBNOW', #  Real Exports of Goods and Services
  #            'IPPCONTRIBNOW', #  Real Gross Private Domestic Investment: Fixed Investment: Business: Intellectual Property Products 
  #             'FEDGOVCONTRIBNOW' , #  Real Gross Government Investment: Federal Government
  #            'EXPORTSGOODSCONTRIBNOW', # Real Exports of Goods
  #            'SLGOVCONTRIBNOW', #  Real Gross Government Investment: State and Local Government
  #            'FINSALESCONTRIBNOW', # Real Final Sales of Domestic Product  
  #            'IMPORTSGOODSCONTRIBNOW', # Real Imports of Goods
  #            'IMPORTSCONTRIBNOW', #  Real Imports of Goods and Services
  #            'EXPORTSSERVICESCONTRIBNOW', # Real Exports of Services
  #            'IMPORTSERVICESCONTRIBNOW') #  Real Imports of Services
  
  
  
  
  myvars  <-c(
    'Real Gross Domestic Product',
    'Personal Consumption Expenditures',
    'Personal Consumption Expenditures - Goods',
    'Personal Consumption Expenditures - Services',
    'Private Domestic Investment',
    'Fixed Investment',
    'Nonresidential Fixed Investment',
    'Residential Fixed Investment',
    'Private Inventories',
    'Net exports of goods and services',
    'Government')
  
  
  gdp_now_tbl <- tibble( symbol = tickers , variables = myvars, category  = "GDP Now")
  
  
  #####################################################################################
  ## Employment Data
  #####################################################################################
  
  # Employment Cost Index: Wages and Salaries: Private Industry Workers (ECIWAG)
  
  tickers <- c('UNRATE',
               "U6RATE",
               "FRBLMCI",
               'PAYEMS',
               "EMRATIO", 
               'CIVPART',
               'LNU01373395',
               'LNS11300060',
               'LNS11300001',
               'LNS11300002',
               "NROU" )
  
  # Next, list human readable variable names
  myvars <- c('Civilian Unemployment Rate',
              "Total Civilian Unemployment Rate",
              "Labor Market Conditions Index",
              "Nonfarm Payroll Employment" ,#  'All Employees: Total nonfarm',
              "Employment-to-Population Ratio",
              'Civilian Labor Force Participation Rate',
              'Civilian Labor Force Participation Rate: Foreign Born',
              'Civilian Labor Force Participation Rate: 25 to 54 years',
              'Civilian Labor Force Participation Rate: Men',
              'Civilian Labor Force Participation Rate: Women',
              "Natural Rate of Unemployment")
  
  emp_tbl <- tibble( symbol = tickers , variables = myvars ,  category  = "EMPLOYMENT")
  
  #####################################################################################
  ## Housing Data
  #####################################################################################
  
  tickers <- c(
    'HOUST',
    'MORTGAGE30US',
    'HPIPONM226S',
    'HSN1F')
  
  # Next, list human readable variable names
  myvars <- c(
    'Housing Starts: Total New Privately Owned',
    '30-Year Fixed Rate Mortgage Average in the United States',
    'Purchase Only House Price Index for the United States',
    'New Home Sales')
  
  house_tbl <- tibble( symbol = tickers , variables = myvars , category  = "HOUSING")
  
  
  #####################################################################################
  ## Inflation data
  #####################################################################################
  
  ###### Download Inflation data ######
  
  # CPIAUCSL        Consumer Price Index for All Urban Consumers: All Items, Index 1982-1984=100, Monthly, Seasonally Adjusted
  # CPILFESL           Consumer Price Index for All Urban Consumers: All Items Less Food and Energy, Index 1982-1984=100, Monthly, Seasonally Adjusted
  # PCEPI Personal Consumption Expenditures: Chain-type Price Index, Index 2009=100, Monthly, Seasonally Adjusted
  # Personal Consumption Expenditures Excluding Food and Energy (Chain-Type Price Index) (PCEPILFE)
  # GDPDEF            Gross Domestic Product: Implicit Price Deflator, Index 2009=100, Quarterly, Seasonally Adjusted
  
  tickers  <- c('CPIAUCSL',
                'CPILFESL',
                'PCEPI',
                "PCEPILFE",
                'GDPDEF',
                'PCETRIM12M159SFRBDAL',
                'T5YIFR',
                'T10YIE',
                "ECIALLCIV",
                "CES0500000003")
  
  
  myvars  <-       c( 'CPI : All Items',
                      'Consumer Price Index for All Urban Consumers: All Items Less Food and Energy',
                      'Personal Consumption Expenditures',
                      "Personal Consumption Expenditures Excluding Food and Energy",
                      'Gross Domestic Product: Implicit Price Deflator',
                      'Trimmed Mean PCE Inflation Rate',
                      '5/5 Year Forward Inflation Expectation',
                      '10-Year Breakeven Inflation Rate',
                      "Employment Cost Index: Total compensation: All Civilian",
                      "Average Hourly Earnings of All Employees: Total Private"
  )
  
  
  # Trimmed Mean PCE Inflation Rate (PCETRIM12M159SFRBDAL)
  # 5-Year, 5-Year Forward Inflation Expectation Rate (T5YIFR)
  # 10-Year Breakeven Inflation Rate (T10YIE)
  
  infl_tbl <- tibble( symbol = tickers , variables = myvars , category  = "INFLATION")
  
  #####################################################################################
  ## Rates data
  #####################################################################################
  
  
  # Effective Federal Funds Rate (FEDFUNDS)
  # 5-Year Treasury Inflation-Indexed Security, Constant Maturity (DFII5)
  # 10-Year Treasury Inflation-Indexed Security, Constant Maturity (DFII10)
  
  # 1-Year Treasury Constant Maturity Rate (DGS1)
  # 5-Year Treasury Constant Maturity Rate (DGS5)
  # 10-Year Treasury Constant Maturity Rate (DGS10)
  
  
  
  tickers <- c("FEDFUNDS",
               'DFII5',
               'DFII10',
               "DGS1MO",
               "DTB3",
               "DTB6",
               'DGS1',
               'DGS2',
               'DGS3',
               'DGS5',
               'DGS7',
               'DGS10',
               'DGS20',
               'DGS30',
               "MORTGAGE30US")
  
  myvars <-c("Effective Federal Funds Rate",
             '5-Year TIPS CMT',
             '10-Year TIPS CMT',
             "1-Month Tbill",
             "3-Month Tbill",
             "6-Month Tbill",
             '1-Year CMT',
             '2-Year CMT',
             '3-Year CMT',
             '5-Year CMT',
             '7-Year CMT',
             '10-Year CMT',
             '20-Year CMT',
             '30-Year CMT',
             "30-year fixed mortgage rate")
  
  rate_tbl <- tibble( symbol = tickers , variables = myvars , category  = "RATES")
  
  
  
  ############################################################
  # Data Download
  ############################################################
  
  econ_tbl <- bind_rows(
    gdp_tbl ,
    gdp_now_tbl ,
    emp_tbl ,
    house_tbl,
    infl_tbl,
    rate_tbl
    
  )
  
  start_date <- "1950-01-01"
  
  econ_df <- econ_tbl$symbol  %>%
    tq_get( get = "economic.data" ,from= start_date )  # we start from January 2000
  
  
  econ_df  <-  left_join(econ_df, econ_tbl ,by="symbol") %>%
    rename(values = price) %>%
    map_if(is.factor, as.character) %>%  #convert factors to character!
    as.tibble()
  
  
  ############################################################
  #  PMI Data Download
  ############################################################
  
  library(Quandl)   # For using the Quandl API.
  # Load the Quandl API token you want to use (from a local file).
  
  auth_token <- 'cRKygqsgVqfFWr82euz2'      # Sign up with Quandl to get a token
  auth_token <- 'kYiiDrxrKsbBb9Vk8Dqu'
  Quandl.api_key(auth_token)
  
  tickers <- c( "ISM/MAN_PMI" ,
                "ISM/NONMAN_NMI")
  
  myvars <- c( "ISM Manufacturing Index" ,
               "ISM Non-Manufacturing Index")
  
  pmi_tbl <- tibble( symbol = tickers ,variables = myvars , category = "SENTIMENT" )
  
  pmi_df <- pmi_tbl$symbol %>%  tq_get( get = "quandl"  ,from= start_date )
  
  pmi_df <- pmi_df %>% mutate( index = if_else( is.na(index) , pmi, index ))
  
  pmi_df   <-  left_join(pmi_df, pmi_tbl ,by="symbol") %>%
    rename(values = index) %>%
    map_if(is.factor, as.character) %>%  #convert factors to character!
    as.tibble()
  
  
  
  ############################################################
  #  PMI Data Download
  ############################################################
  
  econ_df <- econ_df %>%
    bind_rows(
      pmi_df %>% select( one_of( names (econ_df)))
    ) %>%
    rename(dates = date)
  
  econ_tbl <- econ_tbl %>%
    bind_rows(
      pmi_tbl %>% select( one_of( names ( econ_tbl)))
    )
  
  econ_data <- list( data =  econ_df , info = econ_tbl) %>% 
    enframe()
  
  saveRDS( econ_data, file="raw_data/economic_data.rds")
  
} else {
  
  econ_data <-  readRDS("raw_data/economic_data.rds")
  
}
