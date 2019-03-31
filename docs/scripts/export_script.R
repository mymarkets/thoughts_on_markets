

# d
#*****************************************************************
# Export
#******************************************************************


walk( .x = pic_list %>%
        names() %>%
        as.list( ) ,
      .f =  my_pic_save_fnc ,
      pic_list = pic_list ,
      my_path = "Result")

# my_pic_save_fnc(  pic_list ,  "yldcur_p1"       , my_path = "Result")

#*****************************************************************
# Write to powerpoints
#******************************************************************


my_pres <- read_pptx()


# function for adding slides
myp2<- function(my_g){
  my_pres %>%
    add_slide(layout = "Blank", master = "Office Theme") %>%
    ph_with_vg_at( code=print(my_g) , 0.1, 0.1, 9.8,7.3) ->
    my_pres
}



#*****************************************************************
# clean up workspace
#******************************************************************
# rmarkdown::render('Market_Commentary.Rmd',"pdf_document")

# rmarkdown::render('Market_Commentary.Rmd',
#                  output_file = paste('Market_Commentary.html', sep=''))

my_pres <-  myp2(pic_list$rgdp_yoy_p)
my_pres <-  myp2(pic_list$rgdp_cont_p)
my_pres <-  myp2(pic_list$ism_p)
my_pres <-  myp2(pic_list$cpi_p)
my_pres <-  myp2(pic_list$fed_dot_p)
my_pres <-  myp2(pic_list$fed_vs_mkt_p)
my_pres <-  myp2(pic_list$eq_vg1)
my_pres <-  myp2(pic_list$eq_int_p)
my_pres <-  myp2(pic_list$vix_p)
my_pres <-  myp2(pic_list$tytb_p)
my_pres <-  myp2(pic_list$yld_curv_p)
my_pres <-  myp2(pic_list$yldcur_p1)
# save the .pptx file
my_pres %>%
  print( target = "Report Graphs.pptx") %>%
  invisible()
