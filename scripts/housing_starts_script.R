# http://lenkiefer.com/2017/04/18/housing-good-start


##################################################################################
## Load libraries we will need
library(tidyverse,quietly=T,warn.conflicts = F)
library(data.table,quietly=T,warn.conflicts = F)
library(extrafont,quietly=T,warn.conflicts = F)
library(gridExtra,quietly=T,warn.conflicts = F)
library(scales,quietly=T,warn.conflicts = F)
##################################################################################

# https://www.census.gov/econ/currentdata/dbsearch?program=RESCONST&startYear=1959&endYear=2017&categories=APERMITS&dataType=TOTAL&geoLevel=US&adjusted=1&notAdjusted=0&errorData=0


##################################################################################
## Load the data

library(readr)

file_name <- "RESCONST-mf.csv"

df <- read_csv(file_name, skip=747)


##################################################################################
# Construct a lookup table for dates
df_lookup<- data.frame(per_idx=seq(1,699),
                       date=seq.Date(as.Date("1959-01-01"),
                                     as.Date("2017-03-01"),by="month")) %>% tbl_df()

##################################################################################


##################################################################################
# The following information comes straight from the .csv file
# and describes the keys in the data file
##################################################################################




##################################################################################
# CATEGORIES
# cat_idx              cat_code             cat_desc
# 1          APERMITS           Annual Rate for Housing Units Authorized in Permit-Issuing Places
# 2          PERMITS              Housing Units Authorized in Permit-Issuing Places
# 3          AUTHNOTSTD    Housing Units Authorized But Not Started
# 4          ASTARTS              Annual Rate for Housing Units Started
# 5          STARTS Housing Units Started
# 6          UNDERCONST   Housing Units Under Construction
# 7          ACOMPLETIONS               Annual Rate for Housing Units Completed
# 8          COMPLETIONS  Housing Units Completed
##################################################################################

##################################################################################
#DATA TYPES
# dt_idx dt_code dt_desc            dt_unit
# 1          TOTAL   Total Units          K
# 2          SINGLE Single-family Units          K
# 3          MULTI   Units in Buildings with 5 Units or More   K
##################################################################################

##################################################################################
#ERROR TYPES
# et_idx               et_code               et_desc                et_unit
# 1          E_TOTAL              Relative Standard Error for Total Units    PCT
# 2          E_SINGLE            Relative Standard Error for Single-family Units    PCT
# 3          E_MULTI              Relative Standard Error for Units in Buildings with 5 Units or More            PCT
##################################################################################

##################################################################################
# GEO LEVELS
# geo_idx            geo_code            geo_desc
# 1          US          United States
# 2          NE          Northeast
# 3          MW       Midwest
# 4          SO          South
# 5          WE         West
##################################################################################



##################################################################################
# Dates are indexed one a month from 1959-01-01 to 2017-03-01
# e. g.
# TIME PERIODS
# per_idx             per_name
# 1          1/1/1959
# 2          2/1/1959
# ....
# 699 3/1/2017
##################################################################################



##################################################################################


##################################################################################
# Merge dates

df %>%
  inner_join(
    df_lookup,
    by = "per_idx"
  ) -> df

# df <- merge(df,df_lookup,by="per_idx")
df$date <-as.Date(df$date, format="%m/%d/%Y")
##################################################################################


##################################################################################


df %>%
  mutate(dt_idf = factor(dt_idx)) -> df

levels(df$dt_idf) <-c("N/A","Total","Single Family (1 Unit)","Multifamily (5+ Units)")

##################################################################################



# Now that we¡¯ve loaded the data,
# let¡¯s make a time series plot. Let¡¯s plot the history of total housing starts
# for the United States at a seasonally adjusted annual rate This corresponds to
# filtering on dt_id==1 for total, geo_idx==1 for U.S., and cat_idx==4 for starts
# at a seasonally adjusted annual rate. Let¡¯s also add some recession shading.






##################################################################################
# Construct a lookup table for recessions (1959-2017)
# original source see: https://www.r-bloggers.com/use-geom_rect-to-add-recession-bars-to-your-time-series-plots-rstats-ggplot/
recessions.df = read.table(textConnection(
  "Peak, Trough
  1960-04-01, 1961-02-01
  1969-12-01, 1970-11-01
  1973-11-01, 1975-03-01
  1980-01-01, 1980-07-01
  1981-07-01, 1982-11-01
  1990-07-01, 1991-03-01
  2001-03-01, 2001-11-01
  2007-12-01, 2009-06-01"), sep=',',
  colClasses=c('Date', 'Date'), header=TRUE)
##################################################################################

df %>%
  filter(
    cat_idx==4 & geo_idx==1 & dt_idx==1
  ) %>%
  ggplot( aes(x=date,y=val))+
  geom_rect(data=recessions.df , inherit.aes = F,
            aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf),
            fill="gray50", alpha=0.5)+
  geom_line(color="#00B0F0",size=1.05)+
  theme_minimal()+
  scale_y_continuous(labels=comma)+
  labs(y="",title="Housing Starts",x="",
       subtitle="Thousands at seasonally adjusted annual rate",
       caption="@lenkiefer Source: U.S. Census Bureau/Department of Housing and Urban Development, NBER.\nShaded regions recessions.")




##################################################################################
# Define our dark theme, called theme_dark2
theme_dark2 = function(base_size = 12, base_family = "Courier New") {
  
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    
    theme(
      # Specify axis options
      axis.line = element_blank(),
      axis.text.x = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),
      axis.text.y = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),
      axis.ticks = element_line(color = "white", size  =  0.2),
      axis.title.x = element_text(size = base_size, color = "white",
                                  margin = margin(0, 10, 0, 0)),
      axis.title.y = element_text(size = base_size, color = "white", angle = 90,
                                  margin = margin(0, 10, 0, 0)),
      axis.ticks.length = unit(0.3, "lines"),
      # Specify legend options
      legend.background = element_rect(color = NA, fill = " gray10"),
      legend.key = element_rect(color = "white",  fill = " gray10"),
      legend.key.size = unit(1.2, "lines"),
      legend.key.height = NULL,
      legend.key.width = NULL,
      legend.text = element_text(size = base_size*0.8, color = "white"),
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),
      legend.position = "right",
      legend.text.align = NULL,
      legend.title.align = NULL,
      legend.direction = "vertical",
      legend.box = NULL,
      # Specify panel options
      panel.background = element_rect(fill = " gray10", color  =  NA),
      panel.border = element_rect(fill = NA, color = "white"),
      panel.grid.major = element_line(color = "grey35"),
      panel.grid.minor = element_line(color = "grey20"),
      panel.spacing = unit(2, "lines"),
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),
      strip.text.x = element_text(size = base_size*0.8, color = "white",
                                  margin=margin(2,2,2,2)),
      strip.text.y = element_text(size = base_size*0.8, color = "white",
                                  margin=margin(2,2,2,2),
                                  angle = -90),
      # Specify plot options
      plot.background = element_rect(color = " gray10", fill = " gray10"),
      plot.title = element_text(size = base_size*1.5, color = "white",face="bold",
                                hjust=0,lineheight=1.25,
                                margin=margin(2,2,2,2)),
      plot.subtitle = element_text(size = base_size*1, color = "white",face="italic",
                                   hjust=0,  margin=margin(2,2,2,2)),
      plot.caption = element_text(size = base_size*0.8, color = "white",hjust=0),
      plot.margin = unit(rep(1, 4), "lines")
      
    )
  
}

##################################################################################

df %>%
  filter(
    cat_idx==4 & geo_idx==1 & dt_idx==1
  ) %>%
  ggplot( aes(x=date,y=val))+
  geom_rect(data=recessions.df , inherit.aes = F,
            aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf),
            fill="gray80", alpha=0.3)+
  geom_line(color="#00B0F0",size=1.05)+
  facet_wrap(~dt_idf,ncol=1,scales="free_y")+theme_dark2()+
  
  scale_y_continuous(labels=scales::comma)+
  labs(y="",title="Housing Starts",x="",
       subtitle="Thousands at seasonally adjusted annual rate",
       caption="@lenkiefer Source: U.S. Census Bureau/Department of Housing and Urban Development, NBER.\nShaded regions recessions.")



# We can see that while housing starts are trending up, they are well below hisotrical averages.
# Census breaks out construction activity between 1-unit properties (single family), 2 to 4 unit properties and 5+ unit properties
# (multifamily). Let¡¯s see how single family and multifamily starts trends compare:

df %>%
  filter(
    cat_idx==4 & geo_idx==1 & dt_idx %in% c(2,3)
  ) %>%
  ggplot( aes(x=date,y=val))+
  facet_wrap(~dt_idf,ncol=2,scales="free_y")+theme_dark2()+
  geom_rect(data=recessions.df , inherit.aes = F,
            aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf),
            fill="gray80", alpha=0.25)+
  geom_line(color="#00B0F0",size=1.05)+
  scale_y_continuous(labels=scales::comma)+
  labs(y="",title="Housing Starts",x="",
       subtitle="Thousands at seasonally adjusted annual rate",
       caption="@lenkiefer Source: U.S. Census Bureau/Department of Housing and Urban Development, NBER.\nShaded regions recessions.")





# Off to a good start
# But how does 2017 data compare to prior years? The media often focuses on the month-over-month changes, but if we look closely at the Census/HUD report we see that even large percentage changes are often within the margin of error (not really different from no change).
#
# By averaging over a few months of data we can get a better signal. Seasonal adjustment is also quite complicated, so we might examine the year-to-date sum of the non seasonally adjusted data.
#
#So let¡¯s sum the seasonally unadjusted data for the first three months of 2017 and compare this sum to the same YTD sums for recent years.



#create year factor:
df %>%
  mutate(year = year(date),
         yearf = factor(year(date),levels=seq(2017,1959))) %>%
  group_by(cat_idx,geo_idx,year,dt_idx) %>%
  # Compute cumulative YTD totals:
  mutate(c_val = cumsum(val)) %>%
  ungroup() -> df


df %>%
  filter(
    cat_idx==5 & geo_idx==1 &
      dt_idx==1 &
      month(date)==3 & year(date)>1999
  ) %>%
  ggplot(aes(x=yearf,y=c_val))+
  facet_wrap(~dt_idf,ncol=3)+
  geom_segment(color="#00B0F0",aes(yend=0,xend=yearf),size=1.05) +
  geom_hline(yintercept=
               df %>%
               filter(
                 cat_idx==5 & geo_idx==1 &  dt_idx==1 &
                   month(date)==3 & year(date)==2017) %>% .$c_val,
             linetype=2,color="white")+
  geom_point(color="#00B0F0",size=3)+theme_dark2()+coord_flip()+
  theme(axis.ticks.y=element_blank(),
        # Note need to shrink the margin to get the axis labels closer to the segments
        axis.text.y = element_text(margin = margin(r = -10)),
        panel.grid.major.y=element_blank(),
        panel.border=element_blank()
  )+
  labs(y="",title="Housing off to a good start",x="",
       subtitle="First quarter total housing starts in thousands, not seasonally adjusted",
       caption="@lenkiefer Source: U.S. Census Bureau/Department of Housing and Urban Development.")



# How does it break out between singel family and multifamily starts?
df %>%
  filter(
    cat_idx==5 & geo_idx==1 &
      dt_idx %in% c(2,3) &
      month(date)==3 & year(date)>1999
  ) %>%
  ggplot( aes(x=yearf,y=c_val))+
  facet_wrap(~dt_idf,ncol=2,scales="free_x")+
  geom_segment(color="#00B0F0",aes(yend=0,xend=yearf),size=1.05)+
  geom_hline(data=df %>%
               filter(cat_idx==5 & geo_idx==1 &
                        dt_idx %in% c(2,3) &
                        month(date)==3 & year(date)==2017) ,aes(yintercept=c_val),
             linetype=2,color="white")+
  geom_point(color="#00B0F0",size=3)+theme_dark2()+coord_flip()+
  theme(axis.ticks.y=element_blank(),
        # Note need to shrink the margin to get the axis labels closer to the segments
        axis.text.y = element_text(margin = margin(r = -10)),
        panel.grid.major.y=element_blank(),
        panel.border=element_blank()
  )+
  labs(y="",title="Housing off to a good start",x="",
       subtitle="First quarter total housing starts in thousands, not seasonally adjusted",
       caption="@lenkiefer Source: U.S. Census Bureau/Department of Housing and Urban Development.")


# Here we see as above, that while multifamily starts are exceeding pre-recession averages,
# single family starts are still lagging. However, starts are on the rise (and note that per the Census report the YTD
#  sum is up 5.9 percent with a range of plus/minus 4.4 percent). So slowly and steadily, housing starts are rebounding.








