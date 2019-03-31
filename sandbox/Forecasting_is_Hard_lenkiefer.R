
rm(list = ls())
#####################################################################################
## Load Libraries ##
#####################################################################################
library(tidyverse)
library(readxl)
library(data.table)
library(tidyverse)
library(cowplot)
library(readxl)
library(viridis)
library(stringr)
library(ggbeeswarm)
#####################################################################################

#####################################################################################
# Import data
#####################################################################################

# http://lenkiefer.com/2017/08/27/forecast

################################################################
# List of spreadsheets we'll import
################################################################

file_list <- list.files("etc", pattern =".xlsx")

################################################################
# List of variables we want (corresponds to worksheet names)
# If you want MOAR variables, adjust the list
################################################################
vars_list <-c("HOUSING","TBOND","EMP","TBILL","UNEMP","RGDP",
         "CPI","CORECPI","PCE","COREPCE")

################################################################
# Import data function (don't ask about my.import1)
my_import <-function( file_name,path="etc"){
  
  file_path <- str_c(path, file_name, sep = "/")
  
  vars_list %>% 
    set_names() %>% 
    map_df(~ read_excel(path = file_path, sheet = .x, na ="#N/A"),
           .id = "sheet") -> df.out
  
  return(df.out)
}
################################################################



################################################################
# we probably could use purrr better here, 
# but four copy + paste isn't so bad
################################################################

# stack the data
df_all <-  map_df(file_list,my_import)

###############################################################################








# I would print the header of our dataset but at the moment there are 97 variables. Let¡¯s do some transposing and gathering to tidy these data into a more manageable format.

################################################################
# Tidy the data 
################################################################

df_all %>%
  filter(YEAR>1989) %>%  # restrict to just 1989
  gather(var,value, -1,-2,-3,-4,-5) %>%   # gather variables starting in column 6
  
  mutate(horizon=as.numeric(str_sub(var, start= -1)), # we'll explain below
         value=as.numeric(value),                     # turn values into numbers
         date=as.Date(ISOdate(YEAR,QUARTER*3,1)),     # dates
         ydate=date+months(horizon*3-6)               # we'll explain below
  ) %>% 
  select(date,ydate,horizon, everything()) %>%        # i like date to be on the left
  filter(!is.na(horizon)) -> df_all2                  # get rid of annual forecasts

################################################################
# Now look at data
################################################################
str(df_all2)

knitr::kable(head(df_all2)) 




################################################################
# Make some plots
################################################################


################################################################
# Summarize by medians
################################################################
df_all2  %>% group_by(YEAR,QUARTER,sheet,horizon) %>% 
  dplyr::summarize(value.sd=sd(value,na.rm=T),
                   value.iqr=quantile(value,0.75,na.rm=T)-quantile(value,0.25,na.rm=T),
                   value=median(value,na.rm=T)      ) %>%  
  mutate( date=as.Date(ISOdate(YEAR,QUARTER*3,1)),
          ydate=date+months(horizon*3-6)) -> df_median

################################################################
# Now look at data
################################################################
knitr::kable(tail(df_median))




################################################################
# Create a function to compare forecasts to actuals
################################################################

f.plot<-function(s="HOUSING", h=6,ymin=1999){
  
  df_p<-filter(df_all2, YEAR>=ymin & sheet==s & horizon==h & ! is.na(value))
  
  g<-
    ggplot(data=df_p, aes(x=ydate,y=value))+
    geom_boxplot(aes(group=ydate), outlier.size=0.5)+
    geom_point(alpha=0.15,size=0.9)+guides(color=F)+
    theme_minimal()+
    scale_color_viridis(discrete=T,end=0.85)+
    geom_path(data=filter(df_median, YEAR>=ymin & sheet==s & 
                            horizon==1 & !is.na(value)),
              aes(x=ydate),size=1.05,color="red")+
    labs(x="",y="")
  return(g)
}

################################################################
# Gonna use these for annotations
################################################################
df_p<-filter(df_all2, YEAR>=1999 & sheet=="HOUSING" & horizon==1 & ! is.na(value))

# this will give us the red line actual
dfps<-filter(df_median,date==median(df.p$date) & sheet=="HOUSING" & horizon==1)

# this will give us the dots/box
dfb<-filter(df.p, date==median(df.p$ydate)-months(24) & 
              sheet=="HOUSING" & horizon==6 )

# Combine plot with annotations
f.plot(s="HOUSING")+
  annotate(geom="text",x=median((df_p$date))-months(24),hjust=1, 
           y =0.75,
           label="actual estimates\nas observed in real time\n(without
           revisions)",
           color="red")+
  geom_segment(data=dfps, 
               aes(x=ydate, xend=ydate-months(24),
                   yend=0.8,y=value),color="red",linetype=2)+
  geom_segment(aes(x=median(dfb$ydate), xend=median(dfb$ydate)+months(24),
                   y=median(dfb$value), yend=1.8),linetype=3)+
  annotate(geom="text",x=median((df_p$date))+months(24), hjust=0,
           y =1.82, label="boxplots show\ndistribution of 4-quarter\nahead forecasts",
           color="black")+
  labs(x="",
       y="Housing starts (quarterly average of monthly estimates in millions, SAAR)",
       title="4-quarter ahead forecasts for U.S. Housing Starts",
       subtitle="Solid line historical esimates of housing starts (without revisions), dots individual forecasts",
       caption="@lenkiefer Source: U.S. Census Bureau and Department of Housing and Urban Development\nForecasts from Survey of Professional Forecasters\nhttps://www.philadelphiafed.org/research-and-data/real-time-center/survey-of-professional-forecasters")+
  theme(plot.caption=element_text(hjust=0),
        plot.subtitle=element_text(face="italic"))



################################################################
# Create a function to compare forecasts to actuals
################################################################
f.plot2<-function(s="HOUSING",ymin=1999){
  df.p<-filter(df.median, YEAR>=ymin & sheet==s & ! is.na(value))
  g<-
    ggplot(data=df.p, aes(x=ydate,y=value, group=date,color=factor(date)))+
    geom_line(alpha=0.75,linetype=2,size=1.02)+
    guides(color=F)+
    theme_minimal()+
    geom_path(data=filter(df.median, YEAR>=ymin & sheet==s & horizon==1 & !
                            is.na(value)), 
              aes(x=ydate,group="actual"),size=1.05,color="black")+
    labs(x="",y="")
  return(g)
}

################################################################
# Make a plot
# Adjusted on 8/28/2017 to fix y axis label
# label had been 1000s but should be millions
################################################################

f.plot2() +
  labs(x="",y="Housing Starts(millions, SAAR)",
       title="4-quarter ahead forecasts for U.S. Housing Starts",
       subtitle="Solid line historical esimates of housing starts (without revisions), dotted lines median forecast",
       caption="@lenkiefer Source: U.S. Census Bureau and Department of Housing and Urban Development\nForecasts median forecasts from Survey of Professional Forecasters\nhttps://www.philadelphiafed.org/research-and-data/real-time-center/survey-of-professional-forecasters")+
  theme(plot.caption=element_text(hjust=0),
        plot.subtitle=element_text(face="italic"))



