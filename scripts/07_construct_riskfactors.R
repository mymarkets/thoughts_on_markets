
rf_process_flag <- my_download_flag

if(rf_process_flag == TRUE){
  
  #*****************************************************************
  #  Risk Factor Analysis Process
  #******************************************************************
  
  bloomberg_df  <- bloomberg_data %>% 
    filter( name %in% "data") %>% 
    pull(value) %>% .[[1]]
  
  risk_factors_lst <- list(
    
    US_MKT = c( 'RU30INTR Index', # Russell 3000 Total Return Index
                'LD20TRUU Index'), # Bloomberg Barclays US Treasury Bills Total Return Index Value Unhedged USD
    
    GBL_MKT =  c( 'NDUEACWF Index',
                  'LD20TRUU Index'), # Bloomberg Barclays US Treasury Bills Total Return Index Value Unhedged USD
    
    GBL_SMB =  c( 'M1WDLC Index', # MSCI ACWI Large Cap Net TR USD Index
                  'MXWDSC Index'), # MSCI ACWI Small Cap Net TR USD Index
    
    GBL_HML = c(  'M1WD000V Index', # MSCI ACWI Value Net USD Index
                  'M1WD000G Index'), # MSCI ACWI Growth Net USD Index,
    
    
    GBL_MOM =   c(  'M1WD000$ Index', # MSCI ACWI MOMENTUM USD Net Total Return
                    'NDUEACWF Index'), # MSCI ACWI Net TR USD Index
    
    GBL_INT =   c(  'NDDUEAFE Index', # MSCI EAFE Net TR USD Index
                    'RU30INTR Index'), # Russell 3000 Total Return Index
    
    GBL_EM = c(  'NDUEEGF Index', # MSCI Daily TR Net Emerging Markets USD;'
                 'RU30INTR Index'), # Russell 3000 Total Return Index
    
    CASH  =  c( 'LD20TRUU Index'),
    
    AINF =  c( 'LBUTTRUU Index') ,
    
    EINF =  c('LT08TRUU Index', # Bloomberg Barclays U.S. Treasury: Intermediate TR Index Value Unhedged...
              'LBUTTRUU Index'), # Bloomberg Barclays US Treasury Inflation Notes TR Index Value Unhedged USD
    
    
    DEF =  c( 'LF98TRUU Index', # Bloomberg Barclays US Corporate High Yield Total Return Index Value Unhe..
              'LUATTRUU Index'), # Bloomberg Barclays US Corporate Total Return Value Unhedged USD
    
    CREDIT = c( 'LUACTRUU Index', # Bloomberg Barclays US Corporate Total Return Value Unhedged USD
                'LUATTRUU Index'), # Bloomberg Barclays US Treasury Total Return Unhedged USD
    
    
    Duration = c( 'LT11TRUU Index', # Bloomberg Barclays U.S. Treasury: 20+ Year Total Return Index Value
                  'LT01TRUU Index'), # Bloomberg Barclays U.S. Treasury: 1-3 Year Total Return Index Value Unhe..
    
    
    US_MBS = c( 'LUMSTRUU Index', # Bloomberg Barclays US MBS Index Total Return Value Unhedged USD
                'LT08TRUU Index'), # Bloomberg Barclays U.S. Treasury: Intermediate TR Index Value Unhedged...
    
    # 'LG30TRUU Index', # Bloomberg Barclays Global High Yield Total Return Index Value Unhedged USD
    GBL_HY = c( 'LG30TRUH Index', # Bloomberg Barclays Global High Yield Total Return Index Value Hedged USD
                'LT08TRUU Index'), # Bloomberg Barclays U.S. Treasury: Intermediate TR Index Value Unhedged...
    
    
    EMBI = c( 'JPEICORE Index', # J.P. Morgan EMBI Global Core
              'LT08TRUU Index'), # Bloomberg Barclays U.S. Treasury: Intermediate TR Index Value Unhedged...
    
    
    GBL_Rate = c( 'LGT1TRUH Index', # Bloomberg Barclays Global Treasury x US Total Return Index Value Hedged USD
                  'LT08TRUU Index'), # Bloomberg Barclays U.S. Treasury: Intermediate TR Index Value Unhedged...
    
    LQD  =  c( "BASPTDSP Index" ),# TED spread
    
    #  TERM  =     c('BXIIUSTP Index'), # Barclays US Treasury 2Y/10Y Yield Curve Index
    
    # D e u t s c h e   B a n k   G 1 0   F X   C a r r y   B a s k e t   S p o t
    CARRY  =  c( 'FXCARRSP Index'),
    
    CRNCY =  c(  'DXY Curncy'),
    
    COMM = c( 'BCOMTR Index'), # Bloomberg Commodity Index Total Return,
    
    
    INFRA  =  c('SPGTIND Index') # S&P Global Infrastructure Index
    
  )
  
  
  my_start_date <- "2000-01-01" %>% as.Date()
  my_end_date <-  today() #"2018-06-30" %>% as.Date()
  
  riskfactors_df <- map_df( 
    .x = risk_factors_lst %>% names %>% as.list() , 
    ~ {construct_factor_fnc(bloomberg_df, 
                            assets_lst = risk_factors_lst, 
                            select_assets_name = .x,
                            my_start_date= my_start_date, 
                            my_end_date = my_end_date  )}
  )
  
  riskfactors_df <- riskfactors_df %>% rename( symbol = variables )
 
  riskfactors_df %>% skimr::skim()
  
  # Monetary_Symbols_Names
  # select interested variables
  
  
  

  
  
  saveRDS( riskfactors_df , file= "raw_data/riskfactors_data.rds")
  
} else{
  
  
  riskfactors_df <- readRDS(file= "raw_data/riskfactors_data.rds")
  
  
  
}