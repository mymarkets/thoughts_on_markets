
#####################################################################################
## Step 0: Load Libraries ##
#####################################################################################
library(tidyverse)
library(tidyquant)
library(tibbletime)
library(quantmod)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(stringr)
library(forcats)
library(viridis)
library(ggthemes)
library(ggrepel)
library(gtable)
library(grid)
library(gridExtra)
library(extrafont)
library(hrbrthemes)
# install.packages("Quandl")
# Load required packages.
library(Quandl)   # For using the Quandl API.
library(tint)


#*****************************************************************
# 
# load fonts required for market commentary 
#******************************************************************

# font_import(paths = NULL, recursive = TRUE, prompt = TRUE,pattern = NULL)
# fonts() # view available fonts
# loadfonts()

windowsFonts(Times=windowsFont("Arial Narrow"))

source("Econ_Data_PReparation_1.R")
source("./Functions/Run_Event_Dates_Script.R")
source("./Functions/my_theme_script.R")
## Plot function

######################################################################################
# subset cbs data that matches the subject data frame by closest dates
######################################################################################

match_cbs_df <- function(cbs_df,temp){
  
  # http://stackoverflow.com/questions/31818444/join-two-data-frames-in-r-based-on-closest-timestamp
  
  # in case dates do not mismatch between two data i.e., daily vs. weekly : find the closest dates in one data set
  d   <- function(x,y) abs(x-y) # define the distance function
  # which_mins <- function(x){which(x == min(x) ,arr.ind = TRUE) } # require multiple non unique minimum points
  idx <- map_dbl(cbs_df$date, function(x) which.min( d(x,temp$dates) )) # find matches
  #idx <- map(cbs_df$date, function(x) which_mins( d(x,temp$dates) )) # find matches
  # idx  <-purrr::flatten_int(idx_lst)
  
  sub_cbs_df <- bind_cols(cbs_df, temp[idx,"dates",drop=FALSE])
  sub_cbs_df <- left_join(sub_cbs_df, temp, by =c("dates"="dates")) 
  
  return(sub_cbs_df)
  
  
}



# Make a function to handle transformations
mytransf <- function(x,trans="none"){
  switch(trans,
         none=x,
         diff=c(NA,diff(x)),
         pdiff12= 100*quantmod::Delt(x,k=12) # use Quantmod Delt() function 
  )
}

# make a function to save images (here in ~/img directory)
my_plot <- function( in_vars ="UNRATE", in_trans = "none",  in_units , save="N"){
  
  dfp <- econ_df %>% filter(variables == in_vars)
  
  g<-
    ggplot(data=dfp, aes(x=dates,y=mytransf(values,in_trans)))+
    geom_line(color="royalblue")+
    labs(title=paste0(head(dfp,1)$variables," (",in_units,")"),
         caption="@lenkiefer Source: St. Louis Federal Reserve Economic Database (FRED)\nTHIS IS A TEST ONLY",
         x="",y="")+
    theme_minimal()+
    theme(plot.caption=element_text(hjust=0))
  if (save=="N") {print(g)} # plot image}
  if (save=="Y") {ggsave(filename=paste0("img/",in.var,".png"),
                         width=8,height=6)}
}


# my_plot("Annual Real GDP Growth" , "none"  )

params <- econ_tbl %>%
  filter(variables %in% "Real Gross Domestic Product" ) %>%
  mutate(trans = "none" ,
         units =   '% change')


params %>%
  mutate( test = pmap(list(in_vars = variables,
                           in_trans = trans ,
                           in_units = units),
                      my_plot))

econ_df %>% 
  filter( symbol %in% "DPCERY2Q224SBEA", dates >= start_dates )

econ_tbl %>% 
  filter(category %in% "GDP") %>% 
  View()
############################################################
# Extract Real GDP data
############################################################
# convert df.state into a tibbletime object

# Rolling mean, but with function syntax

# select interested variables
select_Symbols_names <- "Real Gross Domestic Product"
start_dates <- "2013-01-01"



select_colors_scale <-  c("#FF0000","#3366ff") # c("#666699","#000080")
select_colors_fill <- c( "#999999","#3366ff") #

df <- econ_df %>% 
  filter( variables %in% select_Symbols_names , dates >= start_dates ) %>% 
  mutate(values = values / 100)


# set x-axis limits

xlim <- c(as.Date(min(df$dates,na.rm = TRUE)),as.Date(max(df$dates,na.rm = TRUE))+months(3))

rgdp_yoy_p <- ggplot( df )+
  geom_col(aes(x=dates ,y= values , fill= values,color= values),alpha=0.8 ) +
  geom_text(data = filter(df, dates == last(dates)),
            aes(x= dates,y= values, label=scales::percent(values) ),
            fontface = 'bold', color ="black",  vjust = -.5, size = 11 ) +
  scale_y_percent(limits=c(-0.015,0.055)) +  #use scale_y_percent function from hrbrtheme
  scale_x_date(limits = xlim, date_breaks = "1 year" , date_labels = "%Y" ) +
  # add reccesion shade
  #geom_rect(data=filter(reccesion_df,start>=select_dates),
  #          aes(xmin= start, xmax= end, ymin=-Inf, ymax=+Inf), fill="gray", alpha=0.35)+
  scale_colour_gradientn(colours=select_colors_scale) +
  scale_fill_gradientn(colours= select_colors_scale) +
  #scale_color_viridis(option="D",end=0.9)+
  #scale_fill_viridis(option="D",end=0.9)+
  theme_Publication() +
  labs(x="",y="",title="Quarter-to-Quarter Growth in US Real GDP",
       
       caption="Source: U.S. Bureau of Economic Analysis\nNote: Real GDP growth is measured at seasonally adjusted annual rates.")+
  theme(  legend.position = "none",
          plot.caption=element_text(hjust=0)) 

rgdp_yoy_p

 
ggsave('Img/rgdp_yoy_p.png', width = 16, height = 9)#, width = 16, height = 9, dpi = 100)



## Economic Activity  - Industrial Production


# ```{r Industrial Production Chart,eval=FALSE, fig.margin=TRUE,echo=FALSE,  warning=FALSE, cache=TRUE}



############################################################
# Extract Industrial Production  data
############################################################

# select interested variables
select_Symbols_names <- c("ISM Manufacturing Index","ISM Non-Manufacturing Index")
select_date <- "2014-01-01"

df <-  econ_df %>% 
  filter(variables %in% select_Symbols_names ) %>%
  mutate(year = as.factor(year(dates)), month =
           as.factor(month(dates))) %>%  # add year variable
  group_by(year , month) %>%
  # compute year over year growth rate
  arrange(dates) %>%
  mutate(ann_g = (values-lag(values,1))/lag(values,1)) %>%
  ungroup() %>%
  select(dates,variables,values,ann_g)

df <- df %>%
  filter(dates>= start_date) %>%
  group_by(variables) %>%
  mutate(pos = min(values,na.rm=TRUE))

#*****************************************************************
# subset event dates
#******************************************************************

cbs_dates <-   c(
  "2010-11-08")

cbs_labels <- c(
  "Trump Win-Reflation Hope")

cbs_df <- data.frame(date=as.Date(cbs_dates), event= cbs_labels)



# match cbs dates
cbs_df_sub <- match_cbs_df(cbs_df,df )

begin_date <- start_date
end_date <-  tail(df,1)$dates

#*****************************************************************
# Graph
#******************************************************************

ism_p <- df  %>%
  ggplot(aes(x=dates,y=values,fill= variables,color= variables))+geom_line(alpha=1,size=1.0)+
  scale_color_manual(values = rev(select_colors_scale ), 
                     labels=c("Manufacturing ISM", "Service ISM")) +
  # facet_wrap(~variables,scale="free_y",ncol=1)+
  # scale_y_continuous(label=scales::percent)+
  #  scale_color_viridis(option="D",end=0.9,discrete = TRUE)+
  #  scale_fill_viridis(option="D",end=0.9,discrete = TRUE)+
  theme(plot.caption=element_text(hjust=0))+
  geom_vline(  xintercept = as.numeric(filter(cbs_df_sub ,
                                              date >= select_date, date <= end_date)$dates),
               size = 1, color='grey',linetype="dashed")  +
  geom_text_repel(data = filter(cbs_df_sub ,date >= select_date, date <= end_date) ,aes(x=  as.Date(dates),y= values,  label=event),
                  family = 'Arial Narrow',
                  fontface = 'bold', color ="#666699",
                  # Add extra padding around each text label.
                  box.padding = unit(0.5, 'lines'),
                  # Add extra padding around each data point.
                  point.padding = unit(1.6, 'lines'),
                  # Color of the line segments.
                  segment.color = 'grey',
                  # Width of the line segments.
                  segment.size = 0.5,
                  # Draw an arrow from the label to the data point.
                  arrow = arrow(length = unit(0.01, 'npc')),
                  # Strength of the repulsion force.
                  force = 1,
                  # Maximum iterations of the naive repulsion algorithm O(n^2).
                  max.iter = 3e3,
                  nudge_x = -100,
                  nudge_y = 10
  ) +
  labs(x="",y="",title="US Business Outlook",
       caption="Source: Institute For Supply Management")+
  theme_Publication() +
  theme(legend.text = element_text(size=18)) 
ism_p

ggsave('Result/ism_p.png', width = 16, height = 9) #, width = 16, height = 9, dpi =





## US Inflation Outlook

Headline CPI inflation accelerated to 2.1% year-over-year in December from 1.7% in November, close to the 2.2% year-over-year core inflation rate due to the rise in shelter cost and recovery in crude oil prices.
Tightening labor market condition has supported service inflation at a 3% annualized rate.  With energy moving higher and rents and medical costs continuing to firm up, price pressures are gaining traction, strengthening the case for the Fed to keep raising interest rates this year.
However, the Fed's preferred gauge of inflation, the core PCE index, which excludes the volatile food and energy components, came in at 1.7 per cent and hasn't met the goal of Fed's 2 percent goal yet, a case for Fed's more dovish stance. See the US Inflation Outlook chart.


# ```{r Inflation Outlook chart,  warning=FALSE, cache=TRUE, echo=FALSE}


select_colors_scale <-  getPalette(10)[c(3,1,10)]
############################################################
# Extract CPI Data
############################################################

# Monetary_Symbols_Names
# select interested variables
select_Symbols_names <- c("CPIAUCSL" ,"CPILFESL" ,"PCEPILFE") #
select_names <- c("Headline CPI","Core CPI", "Core PCE") #

start_date <- "2001-01-01"
end_date <-  "2017-09-30"

df <- econ_df %>% 
  filter(symbol %in% select_Symbols_names ) %>% 
  filter(dates >= start_date, dates <= end_date  ) %>%
  mutate(variables = as.factor(variables),
         year = as.factor(year(dates)), month =as.factor(month(dates))) %>%  #
  group_by(variables,month) %>%
  arrange(dates) %>%
  # compute annual growth rate
  mutate(ann_g = (values-lag(values,1))/lag(values,1)) %>%
  ungroup()

# https://www.ft.com/content/7d97f1b2-e492-11e6-8405-9e5580d6e5fb
df %>%
  filter(dates >= "2010-01-01") %>%
  ggplot(aes(x=dates,y=ann_g,color=variables))+geom_line(alpha=1,size=1.1)+
  geom_hline(yintercept = .02,linetype = "dashed") +
  scale_color_manual(values = select_colors_scale,labels = select_names)+
  scale_x_date(date_breaks = "1 year" , date_labels = "%Y" ) +
  scale_y_continuous(label=scales::percent)+
  # scale_color_viridis(option="D",end=0.9,discrete = TRUE)+
  # theme_minimal()+
  labs(x="",y="",title="US Inflation Outlook",
       caption="Source: U.S. Bureau of Economic Analysis") +
  theme_Publication() +
  theme(plot.caption=element_text(hjust=0),
        legend.justification = c(1, 1), legend.position = c(1, 1)) -> cpi_p
cpi_p +
  theme(plot.caption=element_text(hjust=0),
        legend.justification = c(1, 1), legend.position = c(0.8, .9),
        legend.text = element_text(size=18))-> cpi_p
ggsave('Result/cpi_p.png', width = 16, height = 9) #, width = 16, height = 9, dpi =

