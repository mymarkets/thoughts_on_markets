# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


multiple_double_y_plot <- function(temp.df.melt,select_assets_1,select_colors_1,select_assets_2,select_colors_2) {
  
  # http://stackoverflow.com/questions/26727741/how-to-show-a-legend-on-dual-y-axis-ggplot?rq=1
  library(ggplot2)
  library(gtable)
  library(grid)
  library(data.table)
  library(scales)
  
  Plots <- list()  # new empty list
  num_assets <- length(select_assets_1)
  
  for (i in 1:num_assets) {
    
    
    
    
    # http://stackoverflow.com/questions/26727741/how-to-show-a-legend-on-dual-y-axis-ggplot?rq=1
    
    
    
    p1 <- ggplot(subset(temp.df.melt,assets %in% select_assets_1[i]), aes(x = dates, y = values,
                                                                          fill = assets))  +
      geom_line(aes(colour=assets),size=1) +   labs(x="Performance", y="Time") +
      #scale_color_fivethirtyeight() +
      theme_fivethirtyeight() +
      scale_fill_identity(name="", guide="legend", labels=select_assets_1[i]) +
      scale_y_continuous(expand=c(0,0)) +
      scale_colour_manual(name = '',values =select_colors_1[i], labels = c(select_assets_1[i]))+
      theme(legend.title = element_blank(),axis.text.x = element_text(angle = 90, hjust = 1),
            axis.text.y = element_text(colour="#4B92DB"),
            legend.position="bottom")
    
    
    
    
    p2 <- ggplot(subset(temp.df.melt,assets %in% select_assets_2), aes(x = dates, y = values,
                                                                       fill = assets))  +
      #stat_summary(fun.y=mean,geom="bar") +
      # scale_fill_manual(values=c("purple")) +
      geom_line(aes(colour=assets),size=1)+
      labs(x="", y=" ") + #expand_limits(y=0) +
      scale_y_continuous(labels=comma, expand=c(0,0)) +
      scale_colour_manual(name = '',values =select_colors_2, labels = c(select_assets_2))+
      theme(axis.text.y = element_text(colour = "red")) +
      theme(panel.background = element_rect(fill = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_rect(fill=NA,colour="grey50"),
            legend.position="bottom")
    
    # extract gtable
    g1 <- ggplot_gtable(ggplot_build(p1))
    g2 <- ggplot_gtable(ggplot_build(p2))
    
    
    pp <- c(subset(g1$layout, name == "panel", se = t:r))
    g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t,
                         pp$l, pp$b, pp$l)
    # axis tweaks
    ia <- which(g2$layout$name == "axis-l")
    ga <- g2$grobs[[ia]]
    ax <- ga$children[[2]]
    ax$widths <- rev(ax$widths)
    ax$grobs <- rev(ax$grobs)
    ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
    g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
    g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
    # draw it
    
    # extract legend
    leg1 <- g1$grobs[[which(g1$layout$name == "guide-box")]]
    leg2 <- g2$grobs[[which(g2$layout$name == "guide-box")]]
    
    g$grobs[[which(g$layout$name == "guide-box")]] <-
      gtable:::cbind_gtable(leg1, leg2, "first")
    #  grid.draw(g)
    
    Plots[[i]] <- g  # add each plot into plot list
    
  }
  #multiplot(plotlist = plots, cols = 3)
  return(Plots)
}



double_y_plot <- function(temp.df.melt,select_assets_1,select_colors_1,select_assets_2,select_colors_2) {
  
  # http://stackoverflow.com/questions/26727741/how-to-show-a-legend-on-dual-y-axis-ggplot?rq=1
  library(ggplot2)
  library(gtable)
  library(grid)
  library(data.table)
  library(scales)
  
  Plots <- list()  # new empty list
  
  
  
  # http://stackoverflow.com/questions/26727741/how-to-show-a-legend-on-dual-y-axis-ggplot?rq=1
  
  
  
  p1 <- ggplot(subset(temp.df.melt,assets %in% select_assets_1), aes(x = dates, y = values,
                                                                     fill = assets))  +
    geom_line(aes(colour=assets),size=1) +   labs(x="Performance", y="Time") +
    #scale_color_fivethirtyeight() +
    theme_fivethirtyeight() +
    scale_fill_identity(name="", guide="legend", labels=select_assets_1) +
    scale_y_continuous(expand=c(0,0)) +
    scale_colour_manual(name = '',values =select_colors_1, labels = c(select_assets_1))+
    theme(legend.title = element_blank(),axis.text.x = element_text(angle = 90, hjust = 1),
          axis.text.y = element_text(colour="#4B92DB"),
          legend.position="bottom")
  
  
  
  
  p2 <- ggplot(subset(temp.df.melt,assets %in% select_assets_2), aes(x = dates, y = values,
                                                                     fill = assets))  +
    #stat_summary(fun.y=mean,geom="bar") +
    # scale_fill_manual(values=c("purple")) +
    geom_line(aes(colour=assets),size=1)+
    labs(x="", y=" ") + #expand_limits(y=0) +
    scale_y_continuous(labels=comma, expand=c(0,0)) +
    scale_colour_manual(name = '',values =select_colors_2, labels = c(select_assets_2))+
    theme(axis.text.y = element_text(colour = "red")) +
    theme(panel.background = element_rect(fill = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(fill=NA,colour="grey50"),
          legend.position="bottom")
  
  # extract gtable
  g1 <- ggplot_gtable(ggplot_build(p1))
  g2 <- ggplot_gtable(ggplot_build(p2))
  
  
  pp <- c(subset(g1$layout, name == "panel", se = t:r))
  g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t,
                       pp$l, pp$b, pp$l)
  # axis tweaks
  ia <- which(g2$layout$name == "axis-l")
  ga <- g2$grobs[[ia]]
  ax <- ga$children[[2]]
  ax$widths <- rev(ax$widths)
  ax$grobs <- rev(ax$grobs)
  ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
  g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
  g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
  # draw it
  
  # extract legend
  leg1 <- g1$grobs[[which(g1$layout$name == "guide-box")]]
  leg2 <- g2$grobs[[which(g2$layout$name == "guide-box")]]
  
  g$grobs[[which(g$layout$name == "guide-box")]] <-
    gtable:::cbind_gtable(leg1, leg2, "first")
  #  grid.draw(g)
  
  
  #grid.newpage()
  #grid.draw(g)
  
  
  #multiplot(plotlist = plots, cols = 3)
  return(g)
}


multiple_double_y_plot_2 <- function(temp.df.melt,select_assets_1,select_colors_1,select_assets_2,select_colors_2,cbs.df) {
  
  
  
  library(ggplot2)
  library(gtable)
  library(grid)
  library(data.table)
  library(scales)
  library(quantmod)
  
  Plots <- list()  # new empty list
  num_assets <- length(select_assets_1)
  
  temp.df.melt$dates <- as.Date(temp.df.melt$dates)
  cbs.df$date <- as.Date(cbs.df$date)
  
  
  for (i in 1:num_assets) {
    
    # http://stackoverflow.com/questions/26727741/how-to-show-a-legend-on-dual-y-axis-ggplot?rq=1
    
    
    temp <- subset(temp.df.melt,assets %in%  select_assets_1[i])
    temp.points <- temp$dates[temp$dates %in% cbs.df$date]
    
    p1 <- ggplot(subset(temp.df.melt,assets %in% select_assets_1[i]),
                 aes(x = dates, y = values, fill = assets))  +
      geom_line(aes(colour=assets),size=1) +  labs(x="Performance", y="Time") +
      #scale_color_fivethirtyeight() +
      theme_fivethirtyeight() +
      scale_fill_identity(name="", guide="legend", labels=select_assets_1[i]) +
      scale_y_continuous(expand=c(0,0)) +
      scale_colour_manual(name = '',values =select_colors_1[i], labels = c(select_assets_1[i]))+
      theme(legend.title = element_blank(),axis.text.x = element_text(angle = 90, hjust = 1),
            axis.text.y = element_text(colour="#4B92DB"),
            legend.position="bottom")
    
    p1 <- p1 +  #scale_x_date(limits=as.Date(c(cbs.df$date[1], temp$dates[last(cbs.df$date)]))) +
      geom_vline(xintercept=as.numeric(temp.points), colour="blue")
    
    
    
    p2 <- ggplot(subset(temp.df.melt,assets %in% select_assets_2), aes(x = dates, y = values,
                                                                       fill = assets))  +
      #stat_summary(fun.y=mean,geom="bar") +
      # scale_fill_manual(values=c("purple")) +
      geom_line(aes(colour=assets),size=1)+
      labs(x="", y=" ") + #expand_limits(y=0) +
      scale_y_continuous(labels=comma, expand=c(0,0)) +
      scale_colour_manual(name = '',values =select_colors_2, labels = c(select_assets_2))+
      theme(axis.text.y = element_text(colour = "red")) +
      theme(panel.background = element_rect(fill = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_rect(fill=NA,colour="grey50"),
            legend.position="bottom")
    
    # extract gtable
    g1 <- ggplot_gtable(ggplot_build(p1))
    g2 <- ggplot_gtable(ggplot_build(p2))
    
    
    pp <- c(subset(g1$layout, name == "panel", se = t:r))
    g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t,
                         pp$l, pp$b, pp$l)
    # axis tweaks
    ia <- which(g2$layout$name == "axis-l")
    ga <- g2$grobs[[ia]]
    ax <- ga$children[[2]]
    ax$widths <- rev(ax$widths)
    ax$grobs <- rev(ax$grobs)
    ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
    g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
    g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
    # draw it
    
    # extract legend
    leg1 <- g1$grobs[[which(g1$layout$name == "guide-box")]]
    leg2 <- g2$grobs[[which(g2$layout$name == "guide-box")]]
    
    g$grobs[[which(g$layout$name == "guide-box")]] <-
      gtable:::cbind_gtable(leg1, leg2, "first")
    #  grid.draw(g)
    
    Plots[[i]] <- g  # add each plot into plot list
    
  }
  #multiplot(plotlist = plots, cols = 3)
  return(Plots)
  
  
  
  
}






######################################################################################
# horizon_panel_ggplot
######################################################################################

horizon_panel_ggplot <- function(df,title,dMean=FALSE,origin = 0,horizonscale = 10) {
  
  # de-mean data to have negative and positive values for positive only data, vice versa
  if (dMean == TRUE){
    
    df <- df %>%
      group_by(variables) %>%
      mutate(values = values - mean(values,na.rm=TRUE)) %>%
      ungroup()
    
  }
  
  #df parameter should be in form of date (x), grouping, and a value (y)
  colnames(df) <- c("date","grouping","y")
  #get some decent colors from RColorBrewer
  #we will use colors on the edges so 2:4 for red and 7:9 for blue
  # require(RColorBrewer)
  # col.brew <- brewer.pal(name="RdBu",n=10)
  
  #get number of bands for the loop
  #limit to 3 so it will be much more manageable
  nbands = 3
  
  #loop through nbands to add a column for each of the positive and negative bands
  for (i in 1:nbands) {
    #do positive
    df[,paste("ypos",i,sep="")] <- ifelse(df$y > origin,
                                          ifelse(abs(df$y) > horizonscale * i,
                                                 horizonscale,
                                                 ifelse(abs(df$y) - (horizonscale * (i - 1) - origin) > origin, abs(df$y) - (horizonscale * (i - 1) - origin), origin)),
                                          origin)
    #do negative
    df[,paste("yneg",i,sep="")] <- ifelse(df$y < origin,
                                          ifelse(abs(df$y) > horizonscale * i,
                                                 horizonscale,
                                                 ifelse(abs(df$y) - (horizonscale * (i - 1) - origin) > origin, abs(df$y) - (horizonscale * (i - 1) - origin), origin)),
                                          origin)
  }
  #melt data frame now that we have added a column for each band
  #this will fit ggplot2 expectations and make it much easier
  df_melt <- df %>%
    select(-y) %>%
    gather(band,value,-date,-grouping)
  
  
  # df_melt <- reshape2::melt(df[,c(1:2,4:9)],id.vars=1:2)
  
  #name the columns for reference
  #try to be generic
  colnames(df_melt) <- c("date","grouping","band","value")
  
  #use ggplot to produce an area plot
  p <- ggplot(data=df_melt) +
    geom_area(aes(x = date, y = value, fill=band),
              #alpha=0.25,
              position="identity") +  #this means not stacked
    scale_fill_viridis(discrete = T) +
    #scale_fill_manual(values=c("ypos1"=col.brew[7],  #assign the colors to each of the bands; colors get darker as values increase
    #                        "ypos2"=col.brew[8],
    #                            "ypos3"=col.brew[9],
    #                            "yneg1"=col.brew[4],
    #                            "yneg2"=col.brew[3],
    #                            "yneg3"=col.brew[2])) +
    # ylim(origin,horizonscale) +   #limit plot to origin and horizonscale
    facet_grid(grouping ~ .,scale='free_y') +    #do new subplot for each group
    labs(title = Title) +
    theme_tufte()   +  #this is optional, but I prefer to default
    theme(legend.position = "none",    #remove legend
          strip.text.y = element_text(),#rotate strip text to horizontal
          axis.text.y = element_blank(),#remove y axis labels
          axis.ticks = element_blank(), #remove tick marks
          axis.title.y = element_blank(),#remove title for the y axis
          axis.title.x = element_blank(),#remove title for the x axis
          title = element_text(),               #add a title from function parameter
          plot.title = element_text(size=16, face="bold", hjust=0))  #format title
  
  return(p)
}

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




#*****************************************************************
# Financial Turbulence Plot function
#******************************************************************


regime_plot <- function(df,interested_date, shade_df, shade_df_colors,geom_type,select_title)
{
  
  # df$variables <- desired_name_func( df$variables)
  
  p <- df %>% filter(dates >= interested_date ) %>%
    ggplot() +
    geom_line( aes(x=dates, y=values,color= variables),  size=1) +
    facet_wrap(~ variables, scales='free_y',ncol=1)+
    scale_color_viridis(discrete=TRUE)+
    labs(
      color = "",
      fill = "",
      x = " ",
      y = "Dates",
      title = paste0(select_title," Technique"),
      subtitle =  'Regime Detection Analysis',
      caption = ""
    )+
    theme_tufte() +   theme(legend.position="none") +  theme(legend.title = element_blank())
  
  if (geom_type=="vline"){
    
    
    
    p <- p +  geom_vline( xintercept = as.numeric( as.Date(shade_df)), size = 1, color='grey',linetype="dashed")
  }else{
    
    p <- p +  geom_rect(data= filter(shade_df, start >= interested_date),
                        aes(xmin=start, xmax=end, ymin=-Inf, ymax=+Inf), fill= shade_df_colors, alpha=0.35)
  }
  
  
  return(p)
  
}



###############################################################################
# process data with readable name and convert to long form
###############################################################################


my_process_df_fnc <- function(df,All_Tickers,All_assets_names){
  
  
  select_assets <- str_replace_all(All_Tickers," ",".")
  
  select_assets_names <- All_assets_names
  
  Num_Tickers <- length(select_assets_names)
  
  df <- data_master %>% select(dates,one_of(select_assets)) %>%
    rename_(.dots=setNames(names(.), c('dates',select_assets_names)))
  
  df <- df %>% gather(variables,values,-dates) %>%
    arrange(variables,dates)
  
  df$variables <- factor(df$variables,levels= select_assets_names )
  
  return(df)
  
}



###############################################################################
# process long from data with category info
###############################################################################




my_process_category_fnc <- function(df,Category_list){
  
  
  Category_names <- names(Category_list)
  # assign sector information
  
  df$category <- NA
  
  for (i in 1:length(Category_names)){
    
    df <- df %>% mutate( category = ifelse(variables %in% Category_list[[Category_names[i]]],
                                           Category_names[i],
                                           category ) )
  }
  
  df$category  <- factor(df$category)
  
  
  
  return(df)
  
  
}



#*****************************************************************
# share the same y-axis
#******************************************************************


# https://github.com/baptiste/gridextra/wiki/arranging-ggplot
my_arrange_2x1_shared_legend <- function(p1, p2,Category) {
  
  g1 <- ggplotGrob(p1)
  id.legend <- grep("guide", g1$layout$name)
  legend <- g1[["grobs"]][[id.legend]]
  lwidth <- sum(legend$width)
  
  grid.arrange(p1 + theme(legend.position="none"),
               p2 + theme(legend.position="none"), top= paste0(as.character(Category)),
               legend,
               layout_matrix = rbind(c(1,3), c(2,3)),
               widths = unit.c(unit(1, "npc") - lwidth, lwidth))
}




#*****************************************************************
# remove unnecessary days (from monday to sunday) for wide data form
#******************************************************************

my_removDays_fnc <- function(df,dates_to_remove){
  
  # remove weekends
  work_day_idx <- !(weekdays(as.Date( df$dates)) %in% dates_to_remove )
  df  <- df[work_day_idx,]
  
  return(df)
}


#*****************************************************************
# include date factors for analysis and visualization such as calendar heatmap
#******************************************************************

my_dateFactor_fnc <- function(df){
  
  ## Date transformation process
  
  # http://stackoverflow.com/questions/8510888/calculating-the-number-of-weeks-for-each-year-based-on-dates-using-r
  # temp$nweek <- (as.numeric(strftime(as.POSIXct(temp$dates,     format="%d-%m-%Y"),   format="%j")) %/% 7)  + 1
  
  df$nweek <- lubridate::week(df$dates)
  df$weekday <- lubridate::wday(df$dates)
  # temp$date <- as.Date(temp$dates, format = "%Y-%m-%d")
  # temp$time <-as.Date(as.yearmon(temp$dates, "%b %Y"))
  df$day <-  as.numeric(format(df$dates,"%d") )
  df$week <- as.numeric(format(df$dates,"%w") )
  
  df$month <- as.numeric(format(df$dates,"%m") )
  df$monthf <- factor(df$month,levels=as.character(1:12),
                      labels=c("Jan","Feb","Mar","Apr","May","Jun",
                               "Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
  
  df$year <- as.numeric(format(df$dates,"%Y") )
  df$yearf <-  factor(df$year)
  df$yearmonth <- as.yearmon(df$dates)
  df$yearmonthf <- factor(df$yearmonth)
  
  return(df)
  
  
}

#*****************************************************************
#
## Visualizationsuch geom_col

#******************************************************************

my_geom_col_fnc <- function(temp){
  
  temp %>%
    # filter(variables %in% subject_assets_names) %>%
    filter(!is.na(chgs)) %>%
    ggplot(aes(x=dates,y=chgs, color=chgs,fill=chgs))+ #
    geom_col()+
    scale_fill_viridis(name="52-week\nChange (pp)",discrete=F,option="B")+
    scale_color_viridis(name="52-week\nChange (pp)",discrete=F,option="B")+
    theme_minimal()+
    theme(axis.text.x=element_text(size=6))+
    labs(x="", y="",
         title="52-week change in Global Asset Classes",
         #subtitle="52-week change in mortgage rates",
         caption="@HLim Source: Bloomberg")+
    scale_x_date(date_breaks="1 year",date_label="%Y")+
    theme(plot.title=element_text(size=14))+
    theme(axis.text=element_text(size=8))+
    theme(plot.caption=element_text(hjust=0), legend.position="none") +
    facet_wrap(~variables,scale='free_y',ncol=1) -> g
  # + geom_point(aes(monthweek, weekdayf, size=transactions,alpha=transactions),color="firebrick")
  
  return(g)
}

# library("viridis")


#*****************************************************************
#
## Visualizationsuch as calendar heatmap
#******************************************************************

my_geom_heatmap_fnc <- function(temp){
  
  # create a year indicator in reverse order
  # we want it in reverse order so year will go down instead of up
  temp %>%
    # filter(variables %in% subject_assets_names) %>%
    filter(!is.na(chgs)) %>%
    ggplot(aes(x=nweek ,y= fct_rev(yearf),color=chgs,fill=chgs))+
    geom_tile(color="gray")+
    scale_x_continuous(breaks=seq(0,50,10))+
    scale_fill_viridis(name=" ",discrete=F,option="B")+
    scale_color_viridis(name=" ",discrete=F,option="B")+
    theme_tufte()+
    theme(legend.position="top",plot.caption=element_text(hjust=0))+
    theme(axis.text.y=element_text(size=6),
          axis.text.x=element_text(size=6))+
    theme(legend.position="none") -> g
  
  return(g)
}




#*****************************************************************
#
## list of multiple ggplots togather
#******************************************************************


my_multiple_plots <- function(plots_list,Title){
  # http://www.sthda.com/english/wiki/ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page-r-software-and-data-visualization
  
  
  grobs <- list()
  widths <- list()
  
  
  for (i in 1:length(plots_list)){
    grobs[[i]] <-  ggplotGrob(plots_list[[i]])
    widths[[i]] <- grobs[[i]]$widths[2:5]
  }
  grobs[['top']] <- textGrob(Title,gp=gpar(fontsize=14))
  
  maxwidth <- do.call(grid::unit.pmax, widths)
  
  for (i in 1:length(grobs)){
    grobs[[i]]$widths[2:5] <- as.list(maxwidth)
  }
  
  do.call("grid.arrange", c(grobs, ncol = 1 ))
  
  
}












#*****************************************************************
# create double y axis ggplot2
#******************************************************************


my_2yx_plot <- function(p1,p2,p1_title,p2_title,p_title,p_subtitle,
                        p_caption_1,p_caption_2){
  
  
  
  #*****************************************************************
  # ggbuild
  #******************************************************************
  
  
  # Get the plot grobs
  g1 <- ggplotGrob(p1)
  g2 <- ggplotGrob(p2)
  
  # Get the locations of the plot panels in g1.
  pp <- c(subset(g1$layout, name == "panel", se = t:r))
  
  # Overlap panel for second plot on that of the first plot
  g1 <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, pp$l, pp$b, pp$l)
  
  # ggplot contains many labels that are themselves complex grob;
  # usually a text grob surrounded by margins.
  # When moving the grobs from, say, the left to the right of a plot,
  # make sure the margins and the justifications are swapped around.
  # The function below does the swapping.
  # Taken from the cowplot package:
  # https://github.com/wilkelab/cowplot/blob/master/R/switch_axis.R
  hinvert_title_grob <- function(grob){
    
    # Swap the widths
    widths <- grob$widths
    grob$widths[1] <- widths[3]
    grob$widths[3] <- widths[1]
    grob$vp[[1]]$layout$widths[1] <- widths[3]
    grob$vp[[1]]$layout$widths[3] <- widths[1]
    
    # Fix the justification
    grob$children[[1]]$hjust <- 1 - grob$children[[1]]$hjust
    grob$children[[1]]$vjust <- 1 - grob$children[[1]]$vjust
    grob$children[[1]]$x <- unit(1, "npc") - grob$children[[1]]$x
    grob
  }
  
  # Get the y axis from g2 (axis line, tick marks, and tick mark labels)
  index <- which(g2$layout$name == "axis-l")  # Which grob
  yaxis <- g2$grobs[[index]]                  # Extract the grob
  
  # yaxis is a complex of grobs containing the axis line, the tick marks, and the tick mark labels.
  # The relevant grobs are contained in axis$children:
  #   axis$children[[1]] contains the axis line;
  #   axis$children[[2]] contains the tick marks and tick mark labels.
  
  # Second, swap tick marks and tick mark labels
  ticks <- yaxis$children[[2]]
  ticks$widths <- rev(ticks$widths)
  ticks$grobs <- rev(ticks$grobs)
  
  # Third, move the tick marks
  # Tick mark lengths can change.
  # A function to get the original tick mark length
  # Taken from the cowplot package:
  # https://github.com/wilkelab/cowplot/blob/master/R/switch_axis.R
  # plot_theme <- function(p) {
  #   plyr::defaults(p$theme, theme_get())
  # }
  #
  # tml <- plot_theme(p1)$axis.ticks.length   # Tick mark length
  # ticks$grobs[[1]]$x <- ticks$grobs[[1]]$x - unit(1, "npc") + tml
  
  # Fourth, swap margins and fix justifications for the tick mark labels
  ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])
  
  # Fifth, put ticks back into yaxis
  yaxis$children[[2]] <- ticks
  
  # Put the transformed yaxis on the right side of g1
  g1 <- gtable_add_cols(g1, g2$widths[g2$layout[index, ]$l], pp$r)
  g1 <- gtable_add_grob(g1, yaxis, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off", name = "axis-r")
  
  # Labels grob
  left = textGrob(p1_title, x = 0, y = 0.9, just = c("left", "top"), gp = gpar(fontsize = 14, col =  select_colors_scale[1], fontfamily = "Arial"))
  right =  textGrob(p2_title, x = 1, y = 0.9, just = c("right", "top"), gp = gpar(fontsize = 14, col =   select_colors_scale[2], fontfamily = "Arial"))
  labs = gTree("Labs", children = gList(left, right))
  
  # New row in the gtable for labels
  height = unit(3, "grobheight", left)
  g1 <- gtable_add_rows(g1, height, 2)
  
  # Put the label in the new row
  g1 = gtable_add_grob(g1, labs, t=3, l=3, r=5)
  
  # Turn off clipping in the plot panel
  g1$layout[which(g1$layout$name == "panel"), ]$clip = "off"
  
  
  # Add borders
  # Create grey rectangle
  rect = rectGrob(gp = gpar(col = NA, fill = "grey90"))
  
  # Put the grey rectangles into the margin columns and rows
  # for(i in c(1,6)) g1 = gtable_add_grob(g1, rect, t = 1, b = 7, l=i)
  # for(i in c(1,7)) g1 = gtable_add_grob(g1, rect, t = i, l = 1, r=6)
  # Put the grey rectangles into the margin columns and rows
  g1 = gtable_add_grob(g1, list(rect, rect), t = 1, b = length(g1$heights), l = c(1, length(g1$widths)))
  g1 = gtable_add_grob(g1, list(rect, rect), t = c(1, length(g1$heights)), l = 1, r = length(g1$widths))
  
  # Add texts
  # Labels grob
  left.title = textGrob(p_title, x = 0, y = 0.9, just = c("left", "top"),
                        gp = gpar(fontsize = 18, fontfamily = "Arial", fontface = "bold"))
  labs.title = gTree("LabsTitle", children = gList(left.title))
  
  left.sub = textGrob(p_subtitle, x = 0, y = 0.9, just = c("left", "top"),
                      gp = gpar(fontsize = 14, fontfamily = "Arial"))
  labs.sub = gTree("LabsSub", children = gList(left.sub))
  
  left.head = matrix(list(left.title, left.sub), nrow = 2)
  head = gtable_matrix(name = "Heading", grobs = left.head,
                       widths = unit(1, "null"),
                       heights = unit.c(unit(1.1, "grobheight", left.title) + unit(0.5, "lines"),
                                        unit(1.1,  "grobheight", left.sub) + unit(0.5, "lines")))
  
  left.foot.1 = textGrob(p_caption_1, x = 0, y = 0.8, just = c("left", "top"),
                         gp = gpar(fontsize = 12, fontfamily = "Arial"))
  labs.foot.1 = gTree("LabsFoot1", children = gList(left.foot.1))
  
  left.foot.2 = textGrob(p_caption_2, x = 0, y = 0.8, just = c("left", "top"), gp = gpar(fontsize = 12, col =  "black", fontfamily = "Arial"))
  labs.foot.2 = gTree("LabsFoot2", children = gList(left.foot.2))
  
  left.foot = matrix(list(left.foot.1, left.foot.2), nrow = 2)
  labs.foot = gtable_matrix(name = "Footnote", grobs = left.foot,
                            widths = unit(1, "null"),
                            heights = unit.c(unit(1.1, "grobheight", left.foot.1) + unit(0.5, "lines"),
                                             unit(1.1,  "grobheight", left.foot.2) + unit(0.5, "lines")))
  
  
  # Put the labels in their place
  g1 <- gtable_add_grob(g1, labs.foot, t=11, l=2, r=4)
  g1 <- gtable_add_grob(g1, head, t=1, l=2, r=4)
  
  # ggsave("plot.pdf", g1, width=5, height=5.5)
  
  
  # Draw it
  grid.newpage()
  grid.draw(g1)
  
  
}



# set axis limits function
my_axis_fun <- function( df , x  ){
  
  # Setup expressions
  x_expr     <- rlang::enquo(x)
  
  my_lim <- df %>%
    summarise(
      
      min( !!x_expr  , na.rm = TRUE) ,
      
      max( !!x_expr , na.rm = TRUE)
      
    ) %>% as.matrix() %>% c()
  
  return(my_lim)
  
}

# add area plot
my_area_func <- function(p, area_df , start_date ){
  
  
  start_date <- rlang::enquo(start_date)
  
  my_shade_df <- filter(area_df, start >= !!start_date )
  
  if (nrow(my_shade_df ) >=1 ){
    
    p <- p + geom_rect(data=  my_shade_df,
                       aes(xmin= start, xmax= end, ymin=-Inf, ymax=+Inf),
                       fill="gray", alpha=0.35)
  }
  
  return(p)
  
}



## export ggplot with high resolution

my_pic_save_fnc <- function( pic_list, p_name , my_path = "Result" ){
  
  p_name <- ( p_name )
  
  
  file_name <- str_c(my_path ,( p_name ), sep = "/") %>%
    str_c(".png")
  
  p <- pic_list %>%
    pluck( !!( p_name ))
  
  png( file_name ,  width = 12.2, height = 6.86 , units = 'in',
       type="windows", res=400)
  
  print( p )
  
  dev.off()
}





#######################################################################################################
# plot functions

#######################################################################################################
# Some functions

# Create plotting function
my_ribbon_plotf<-function(df, benchmark_name){
  
  
  
  df %>%
    ggplot(aes(x=dates,y= values))+
    geom_line(color="black") +
    geom_line(linetype=2, aes(y=benchmark))+
    geom_ribbon(aes(ymin=values,ymax=down),fill="#d73027",alpha=0.5)+
    geom_ribbon(aes(ymin=values,ymax=up),fill="#4575b4",alpha=0.5) +
    facet_wrap(~variablesf,scales="free_y")+
    theme_minimal()+
    theme(legend.position="top",
          plot.caption=element_text(hjust=0),
          plot.subtitle=element_text(face="italic"),
          plot.title=element_text(size=16,face="bold"))+
    labs(x="",y="Cumulative Performance",
         title= my_title,
         subtitle= my_subtitle,
         caption=my_caption)+
    geom_rug(aes(color=ifelse(values> benchmark,"Same or Better","Worse")),sides="b")+
    scale_color_manual(values= c("#4575b4","#d73027"),
                       label = str_c("Better or worse than ", benchmark_name))+
    theme_Publication() +
    theme(legend.position = "none") -> g
  return(g)
}




# Data process & subsetting function

myf<-function(df , benchmark_name, my_start_date, my_end_date){
  
  benchmark_name <- (  benchmark_name  )
  new_benchmark_name <- df %>% filter( symbol ==  benchmark_name  ) %>% distinct(names) %>% pull()
  
  my_start_date <- enquo( my_start_date  )
  my_end_date <- enquo( my_end_date )
  # process data
  df %>%
    # We'll subset data
    filter( between( dates, !!my_start_date, !!my_end_date )) %>%
    group_by( symbol ) %>%
    # remove nas for cumprod calculation
    na.omit() %>%
    # calc cumulative returns
    mutate(values = cumprod(1+values)) %>%
    ungroup() %>%
    select( dates, names, values ) %>% 
    distinct(dates, names, .keep_all = TRUE ) %>% 
    spread( names ,values) %>%
    # keep benchmark out
    gather(variables,values,-dates,-one_of( new_benchmark_name ) ) %>%
    # rename benchmark name to generic "benchmark name" for universal use
    rename_( "benchmark" = as.name( new_benchmark_name )) %>%
    # calc min and max for ribbon chart
    mutate(up = ifelse(values < benchmark , values, benchmark)) %>%
    mutate(down = ifelse(values > benchmark,  values, benchmark)) -> df
  
  df %>%
    map_if(is.factor,as.character) %>% 
    as_data_frame() %>%
    # to order df according performance first
    group_by(variables) %>%
    mutate(max_pos = max(values,na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(variablesf = factor(variables)) %>%
    mutate(variablesf = reorder(variablesf,desc(max_pos))) -> df
  
  return(df)
}


#######################################################################################################





my_bar_plot_fnc <- function( df , variables, values, select_colors_scale ,  my_title = NULL , my_subtitle = NULL,  my_caption  = NULL) {
  
  
  #*****************************************************************
  #  process data 
  #******************************************************************
  
  df <- df %>% 
    rename( "variables"  := !!variables, 
            "values"  := !!values) 
  
  df <- df %>% 
    mutate(variablesf =  factor( variables),
           variablesf = reorder(variablesf,values) ,
           quarterf = factor(as.yearqtr(date, format = "%Y-%m-%d"))
    ) %>%
    group_by(quarterf,variablesf) %>%
    arrange(desc(values)) %>%
    ungroup() %>%
    mutate(variablesf = factor(paste( variablesf,quarterf, sep = "__")
                               ,   levels = rev(paste(variablesf,quarterf,  sep = "__"))  ))
  
  
  # --ggplot here--
  p <-  ggplot(data = df, aes(x= variablesf, y= values)) +
    geom_bar(stat='identity', aes(fill=values), width=.8)  +
    
    facet_wrap(~quarterf,scales="free")+
    geom_text(aes(label=  scales::percent(values),
                  hjust = ifelse(values > 0, -.15, 1.1)),
              color= "black" ) +
    scale_y_percent(limits = my_ylim) +
    coord_flip() +
    scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
    scale_fill_gradient(low = select_colors_scale[1] , high =  select_colors_scale[2] ) +
    labs(x="",y="",
         subtitle = my_subtitle,
         title= my_title ,
         caption= my_caption)+
    theme_Publication()+
    theme(legend.position = "none" ,
          plot.title = element_text( color = nord_palettes$frost[4] ) ,
          plot.subtitle = element_text(  nord_palettes$frost[4] ) ,
          strip.background=element_rect(fill="white")
    )
  
  return(p)
  
  
}


#######################################################################################################

# construct risk factor function

construct_factor_fnc <- function( df, assets_lst,select_assets_name,my_start_date, my_end_date ){
  
  print(select_assets_name )
  
  select_symbols_names <- assets_lst[[ select_assets_name ]]
  
  num_select_symbols_names <- length(select_symbols_names)
  
  print( num_select_symbols_names )
  
  df <-   bloomberg_df %>% 
    filter( symbol %in% select_symbols_names ) %>% 
    filter( between( dates, my_start_date, my_end_date )) %>% 
    group_by( symbol ) %>% 
    tq_transmute( selec = values,
                  mutate_fun = periodReturn,
                  period = "daily",
                  type = "log",
                  col_rename = "returns") %>% 
    ungroup() %>% 
    spread( symbol, returns ) 
  
  if (num_select_symbols_names > 1) {
    
    df <- df %>% 
      mutate( spread =   .[[select_symbols_names[1]]] - 
                .[[select_symbols_names[2]]]   
      )
    
  } else{
    
    df <- df %>% 
      mutate( spread =   .[[select_symbols_names[1]]])
    
  }
  
  df <-  df %>% 
    na.omit() %>% 
    mutate( values = 100* cumprod( 1 + spread ), 
            variables = quo_name(select_assets_name)) %>% 
    select( dates, spread, values, variables) 
  
  
  return(df)
  
}

