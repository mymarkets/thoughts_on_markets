uig_download_flag <- FALSE
# uig_download_flag <- TRUE

# https://www.newyorkfed.org/research/policy/underlying-inflation-gauge

if(uig_download_flag == TRUE){
  
  #*****************************************************************
  # FOMC Data Process
  #******************************************************************
  
  # the url for the online Excel file
  url <- "https://www.newyorkfed.org/medialibrary/media/research/policy/underlying_inflation_gauge_uig/uig_uig-measures_chart_data.xlsx?la=en"
  
  file_name <- "raw_data/raw_uig_data.xlsx"
  download.file( url, destfile= file_name , mode="wb")
  
  uig_data <- read_excel( path = file_name , sheet = "ch1_uig" , skip = 13) %>% 
    mutate( dates = as.numeric( Date ) , 
            dates = excel_numeric_to_date(dates )) %>% 
    select( - Date) %>% 
    gather( variables, values , -dates) %>% 
    mutate( values = values / 100 )
  
  
  saveRDS( uig_data , file= "raw_data/uig_data.rds")
  
} else{
  
  
  uig_data <- readRDS(file= "raw_data/uig_data.rds")
  
  
  
}