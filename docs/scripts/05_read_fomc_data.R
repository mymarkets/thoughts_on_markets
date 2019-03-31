fomc_process_flag <- my_download_flag 
# fomc_process_flag <- TRUE


if(fomc_process_flag == TRUE){
  
  #*****************************************************************
  # FOMC Data Process
  #******************************************************************
  
  my_file_names <- list.files( "raw_data/", full.names = TRUE )
  
  my_file_names <- my_file_names [
    str_detect(my_file_names, pattern = "FOMC_Dotplot")
    ] 
  
  
  fomc_lst <- my_file_names %>% 
    map(
      ~{  .x %>% 
          excel_sheets() %>% 
          purrr::set_names() %>% 
          map(read_excel, path = .x )
      }
    ) 
  
  
  my_file_names <- my_file_names %>% 
    str_remove_all( pattern = (".*/" ) ) %>% 
    str_remove_all( pattern = (".xlsx" ) ) 
  
  fomc_data <- fomc_lst %>% 
    purrr::set_names( my_file_names) %>% 
    enframe()
  
  
  fomc_data <- fomc_data %>% 
    mutate( 
      dates = map( value, ~{ names(.)[1] %>% 
          str_replace_all("FOMC Dot Plot" ,"") %>% 
          str_trim() %>% 
          mdy()
      }
      ))
  
  saveRDS( fomc_data , file="raw_data/fomc_data.rds")
  
} else{
  
  
  fomc_data <-readRDS(file="raw_data/fomc_data.rds")
  
  
  
}