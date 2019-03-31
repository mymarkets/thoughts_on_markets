theme_Publication <- function(base_size = 16, base_family="Arial Narrow") {
  library(grid)
  library(ggthemes)
  library(hrbrthemes)
  
  (
    theme_minimal(base_size = base_size, base_family = base_family) %+replace%
      theme_ipsum(base_size = base_size,
                  base_family = base_family,
                  grid=FALSE,
                  axis_title_just = "m"
      ) +
      theme( axis.line = element_line(colour= "#999999"),
             panel.grid = element_line( colour = "#f2efef") ,
             plot.caption=element_text(hjust=0))
  )
  
}




theme_Publication <-function(base_size = 16, base_family="Arial Narrow") {
  library(grid)
  library(ggthemes)
  library(hrbrthemes)
  
  (
    theme_minimal(base_size = base_size, base_family = base_family) %+replace%
      theme(
        
        plot.title = element_text(face = "bold",margin=margin(0,0,20,0),
                                  size = base_size*1.6, hjust = 0.0),
        plot.subtitle=element_text(face="italic",size= base_size*1.4,
                                   hjust=0,  margin=margin(2,2,2,2)),
        plot.caption=element_text(hjust=0,vjust =-1,size=  base_size*1.2,
                                  margin=margin(2,2,2,2)),
        
        axis.text = element_text(),
        axis.ticks = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.key = element_rect(colour = NA),
        legend.text = element_text(size=12),
        legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.direction = "vertical",
        legend.key.size= unit(0.7, "cm"),
        # legend.margin = unit(0.2, "cm"),
        legend.title = element_blank(), #element_text(face="italic"),
        # plot.margin=unit(c(10,5,5,5),"mm"),
        plot.margin = unit(rep(1, 4), "lines"),
        strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
        strip.text = element_text(face="bold"),
        axis.title = element_text(face = "bold",size = rel(1)),
        axis.title.y = element_text(angle=90,vjust =2),
        axis.title.x = element_text(vjust = -0.2),
        axis.line = element_line(colour="black"),
        axis.line.y = element_line(color="black"),
        axis.line.x = element_line(color="black")
      ) +
      theme(
        panel.grid.major = element_line(colour = "grey92"),
        panel.grid.minor = element_line(colour = "grey92",
                                        size = 0.25),
        axis.line = element_line(colour="black")
      )
  )
  
}


# Function for colors ----
#####################################################################################
## Make Color Scale ----  ##
#####################################################################################
my_colors <- c(
  "red"   = "#AF1900" ,
  "red2"      = "#BF616A",
  'orange'      = "#D08770",
  'yellow'     = "#ffc425",
  'yellow2'     = "#EBCB8B",
  "green" =  "#A3BE8C",
  "green2" =  "#647D4B" ,
  "lightblue"  ="#81A1C1"  ,
  "lightblue2" = "#5E81AC",
  'blue'       ="#306489",
  'blue2'       ="#222B4C",
  'gold'       = "#FFD700",
  'light grey' = "#cccccc",
  'purple'     = "#551A8B",
  'dark grey'  = "#8c8c8c"
)



#' Function to extract drsimonj colors as hex codes
#'
#' @param ... Character names of drsimonj_colors 
#'
my_cols <- function(...) {
  cols <- c(...)
  if (is.null(cols))
    return (my_colors)
  my_colors[cols]
}



my_palettes <- list(
  `main`  = my_cols("red", "green", "blue"),
  `cool`  = my_cols("blue", "green"),
  `hot`   = my_cols("yellow", "orange", "red"),
  `mixed` = my_cols("lightblue", "green", "yellow", "orange", "red"),
  `mixed2` = my_cols("lightblue2","lightblue", "green", "green2","yellow","gold", "orange", "red"),
  `mixed3` = my_cols("lightblue2","lightblue", "green", "yellow","gold", "orange", "red"),
  `mixed4` = my_cols("lightblue2","lightblue", "green", "green2","yellow","gold", "orange", "red","purple"),
  `mixed5` = my_cols("lightblue","green", "green2","yellow","gold", "orange", "red","purple","blue"),
  `mixed6` = my_cols("green", "gold", "orange", "red","purple","blue"),
  `grey`  = my_cols("light grey", "dark grey")
)


#' Return function to interpolate a drsimonj color palette
#'
#' @param palette Character name of palette in drsimonj_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'

my_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- my_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}



#' Color scale constructor for drsimonj colors
#'
#' @param palette Character name of palette in drsimonj_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
#'
scale_color_mycol <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- my_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("my_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}


#' Fill scale constructor for drsimonj colors
#'
#' @param palette Character name of palette in drsimonj_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'


scale_fill_mycol <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- my_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("my_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

