# http://lenkiefer.com/2016/11/21/consumer-price-viz
# https://download.bls.gov/pub/time.series/cu/cu.txt
# https://github.com/hrbrmstr/hrbrthemes
# https://cran.r-project.org/web/packages/ggalt/vignettes/ggalt_examples.html
# https://rud.is/b/2017/02/18/putting-it-all-together/
# devtools::install_github("hrbrmstr/hrbrthemes")

setwd("~/Toolbox/R/Market_Commentary-master/Market_Commentary-master")

###### load library ######
pkgs = c("quantmod","tidyverse" ,"stringr","forcats","ggplot2","scales" ,"ggalt", "lubridate","ggthemes",
         "ggrepel","grid","gridExtra" ,"extrafont","Quandl","viridis","hrbrthemes","viridis","scales" ) # package names

# Install
install_flag <- FALSE

if (install_flag == TRUE) install.packages(pkgs)

# load packages
inst = lapply(pkgs, library, character.only = TRUE) # load them

###### download data ######
download_flag <- TRUE

if ( download_flag == TRUE){
  
  
  #read cpi files from BLS.gov
  
  cpi_raw_data <- read_tsv('http://download.bls.gov/pub/time.series/cu/cu.data.2.Summaries')
  # View(cpi_raw_data)
  
  cpi_item <- read_tsv("http://download.bls.gov/pub/time.series/cu/cu.item")
  # View(cpi_item)
  
  cpi_series <- read_tsv("http://download.bls.gov/pub/time.series/cu/cu.series")
  # View(cpi_series)
  
  # save the data
  cpi_list <- list("raw_data" = cpi_raw_data,
                   "item" = cpi_item,
                   "series" = cpi_series)
  # save results for later use in case internet is not available
  save(cpi_list ,file="Data/cpi_list.RData")
  
  
} else{
  
  load("Data/cpi_list.RData")
  
}

###### Process data ######
cpi_table <- inner_join(cpi_list$item , cpi_list$series ,by = c("item_code"))
# View(cpi_table)

# merge on series_id variable:
cpi_list$raw_data %>%
  left_join( cpi_table , by = "series_id") %>%
  # extract numerical month values
  mutate( month =as.numeric(substr(period,2,3)),
          #set up date variable
          date = as.Date(ISOdate(year,month,1) )) -> cpi_raw_data 

# View(cpi_raw_data)

unique(cpi_raw_data$item_code)  #Get list of item codes

# select interested data sets : adjusted series
cpi_df <- cpi_raw_data %>%
  filter(area_code=="0000" & seasonal=="S"
         & item_code!= "SAA1" & item_code !="SAA2")

# View(cpi_df)  # df <- cpi_df
# create cpi data process function
my_cpi_fnc <- function(df,select_year){
  
  # Create a variable with the index normalized to 100 in January 2000:
  bdata <- filter(df , year == select_year,month == 1) %>%
    dplyr::rename( value00=value) %>%
    select(value00,series_id)
  
  df <- left_join(df,bdata,by="series_id")  %>% #merge back to original data
    mutate(cpi00 = 100*value/value00) %>%
    group_by(series_id) %>%
    mutate(cpi12 = (value - lag(value,12))/lag(value,12)) %>%
    ungroup() %>%
    filter(year >= select_year)
  # View(cpi_df )
  return(df)
}



cpi_df %>%
  my_cpi_fnc(2000) -> df
# View( df)
# set x-axis limits
xlim <- c(as.Date(min(df$date,na.rm = TRUE)),as.Date(max(df$date,na.rm = TRUE))+years(1))

ggplot(df, aes(x=date,y=cpi00,color=item_name))+
  geom_line() + 
  scale_y_log10(limits=c(90,200),breaks=c(90,100,120,140,160,180,200)) +
  #scale_x_date(limits =xlim)+
  scale_x_date(labels= scales::date_format("%Y") , limits =xlim)+
  theme(legend.justification=c(0,0), legend.position="none") +
  geom_text_repel(
    data = filter(df, date== last(date)),
    aes(label = item_name),
    size = 3.5,
    nudge_x = 1) +
  labs(x="", y="Consumer Price Index (log scale, Jan 2000=100, SA)",
       title="Consumer Prices",
       subtitle="by major category",
       caption="@Hyungjin Lim Source: U.S. Bureau of Labor Statistics") +
  theme_ipsum_rc(grid = FALSE,axis = TRUE)+
  scale_color_ipsum() +
  theme(legend.position = "none")


#  cpi_raw_data %>% #get unadjusted index:
#    filter( area_code=="0000" & seasonal=="U" & item_code!= "SAA1" & item_code !="SAA2" &
#                  !(period %in% c("S01", "S02", "S03"))) %>%
#    my_cpi_fnc(2000) -> df




df %>% # date<= "2016-12-31" &
  filter( !(item_name %in% c("Transportation","Services","Other goods and services"))) %>%
  ggplot(aes(x= date,y= cpi12,color=item_name))+
  geom_area(aes(fill=item_name),alpha=0.5)+
  scale_y_percent()+  #use scale_y_percent function from hrbrtheme
  theme_ipsum_rc(grid="Y")+
  # geom_hline(yintercept=0,linetype=2,color="black")+
  #scale_y_log10(limits=c(90,200),breaks=c(90,100,120,140,160,180,200))+
  scale_x_date(limits =xlim)+
  #geom_text_repel(    data = subset(cpi6[date<=dd[90]], date == max(date)),    aes(label = item.name, y=180),    size = 5,    nudge_x = 45,    segment.color = NA) +
  labs(x="", y="Consumer Price Index (y/y % change NSA)",
       title="Consumer Price Inflation (y/y %)",
       subtitle="by major category",
       caption="@lenkiefer Source: U.S. Bureau of Labor Statistics")+
  #theme(plot.title=element_text(size=18))+
  #theme(plot.caption=element_text(hjust=0,vjust=1,margin=margin(t=10)))+
  #theme(plot.margin=unit(c(0.25,0.25,0.25,0.25),"cm")) +
  facet_wrap(~item_name,ncol=2)+
  theme(legend.position="none")+
  theme(panel.grid.major=element_line(color="lightgray",size = .01))+
  geom_hline(yintercept=0,linetype =  "dashed") -> gg#darken the x axis at 0

gg+ theme(axis.title.y=element_text(hjust=.5))


first_date <- tail(df$date,2)[1]
last_date <- tail(df$date,2)[2]

df %>%
  filter(date >= first_date & !(item_name %in%
                                  c("Transportation","Services","Other goods and services"))) %>%
  select(cpi12,date,item_name) %>%
  spread(date,cpi12) %>%
  mutate(item_name = as.factor(item_name)) %>%
  set_names(nm = c("item_name","first_date","last_date")) -> range_df

filter(df, item_name %in% "All items" ) %>%
  arrange(date) %>%
  slice(c(n()-1, n())) %>%
  mutate(lab=lubridate::year(date)) -> lab_df

ggplot(range_df)+
  geom_dumbbell(aes(y= reorder(item_name,last_date), x = first_date , xend = last_date ),
                size=3, color="#e3e2e1",
                colour_x = "#a3c4dc", colour_xend = "#0e668b",
                dot_guide=TRUE, dot_guide_size=0.25) +
  geom_text(data=lab_df, aes(x=cpi12, y= "All items", label=lab), vjust=0) +
  scale_x_percent(limits=c(-0.02, 0.05)) +
  labs(x=NULL, y=NULL,
       title=sprintf("U.S. Labor Force Participation Rate %s-Present", lab_df$lab[1]),
       caption="Source: EPI analysis of basic monthly Current Population Survey microdata.") +
  theme_ipsum_rc(grid="X")


df %>%
  filter(date >= last_date & !(item_name %in%
                                 c("Transportation","Services","Other goods and services"))) %>%
  ggplot(aes(x=item_name,y=cpi12,color=cpi12)) +
  scale_color_viridis(option="D",name="Annual Inflation\nRate (%) ",discrete=F,direction=-1,end=0.85,
                      label=scales::percent)+
  geom_segment(aes(xend=item_name,yend=0),size=1.2)+coord_flip()+
  geom_text(aes(label=paste(" ",percent(round(cpi12,3))," "),
                hjust=ifelse(cpi12>0,0,1)))+  #flip justification if point postiive or negative
  geom_point(size=3)+
  theme_minimal()+  
  theme(legend.position="top",legend.text=element_text(size=7))+
  theme(legend.key.width=unit(3,"cm"))+
  scale_y_continuous(label=percent,limits=c(-0.02,.05),breaks=seq(-0.2,.08,.01))  +
  labs(x="", y="Consumer Price Index (y/y % change NSA)",
       title="Consumer Price Inflation (y/y %)",
       subtitle="by major category",
       caption="@lenkiefer Source: U.S. Bureau of Labor Statistics")+
  theme(plot.title=element_text(size=18))+
  theme(plot.caption=element_text(hjust=0))


update_geom_font_defaults(font_rc_light)


df %>%
  filter(date >= last_date & !(item_name %in%
                                 c("Transportation","Services","Other goods and services"))) %>%
  mutate(n = cpi12) %>%
  arrange(n) %>%
  mutate(class=factor(item_name, levels=item_name)) %>%
  ggplot(aes(class, n)) +
  geom_col() +
  geom_text(aes(label= scales::percent(n), 
                hjust = ifelse(n >= 0, 0, 1)), vjust=0) +
  scale_y_percent(limits = c(-0.01,0.045)) +
  coord_flip() +
  labs(x="Fuel effiiency (mpg)", y="Weight (tons)",
       title="Seminal ggplot2 column chart example with commas",
       subtitle="A plot that is only useful for demonstration purposes, esp since you'd never\nreally want direct labels and axis labels",
       caption="Brought to you by the letter 'g'") +
  theme_ipsum_rc(grid="X")