# install.packages("dtwclust")

# https://cran.r-project.org/web/packages/dtwclust/vignettes/timing-experiments.html

# https://github.com/asardaes/dtwclust

# http://www.grappa.univ-lille3.fr/~mary/cours/stats/telescopic/

# https://medium.com/@panda061325/stock-clustering-with-time-series-clustering-in-r-63fe1fabe1b6

library(dtwclust)
library(recipes) 
library(rsample) 



my_start_date <- "2000-01-01" %>% as.Date()
my_end_date <- today() #"2018-06-30" %>% as.Date()

key_tbl  <- tribble(   ~symbol ,                     ~variables ,
                       "SPX Index" ,                   "S&P 500 Index" )

df <- bloomberg_df %>% 
  filter( symbol %in% key_tbl$symbol ) %>% 
  left_join( key_tbl , by  = c("symbol")) %>%
  select( dates, variables, values ) %>% 
  distinct( variables, dates , .keep_all = TRUE ) %>% 
  filter( between( dates ,my_start_date, my_end_date) )

##  ............................................................................
##  Set Parameters                                                          ####

# rolling initial horizon 
ini_horizon <- (1 * 90)
assess_period <- (1 * 90)
num_skim_day <- 21

# create rolling window df
roll_rs <- rolling_origin(
  df , 
  initial = ini_horizon, 
  assess = assess_period,
  skip = num_skim_day,
  cumulative = FALSE
)

# For plotting, let's index each split by the first day of the assessment set:
get_date <- function(x) min(assessment(x)$dates)

start_date <- map(roll_rs$splits, get_date)
roll_rs$start_date <- do.call("c", start_date)
tail(roll_rs$start_date)

roll_rs <- roll_rs %>% 
  # slice(1:100) %>% 
  # create data
  mutate( data = map( splits , ~{ assessment(.x)  }
  ) 
 ) %>% 
  mutate( data = map( data,  ~{ .x %>%
      tq_transmute( select = values ,
                    mutate_fun = periodReturn,
                    period = "daily",
                    type = "arithmetic",
                    col_rename = "values") %>% 
      drop_na( values ) %>% 
      mutate( values = cumprod( 1+ values )) %>% 
      mutate( values =  log(values)) }
  )
  ) %>% 
  mutate( data = map( data , ~{  .x %>%
      select( values) %>% 
      as.matrix() %>% 
      as.numeric()} ) 
  ) %>% 
 print()


data <- roll_rs %>% pull(data) 
data <- data %>% set_names( roll_rs$ start_date )

data_names <- data %>% names()

# Partitional
cluster_obj <- tsclust( data , type = "partitional", k = 4, 
              distance = "dtw_basic", centroid = "dba", 
              seed = 3247L, trace = TRUE,
              args = tsclust_args(dist = list(window.size = 20L)))

plot(cluster_obj, type = "centroids") # , clus = 1L
plot(cluster_obj,type="series")

cluster_obj@clusinfo

cluster_obj@cluster

tibble( dates = data_names , cluster =  cluster_obj@cluster) %>% 
  filter( dates== last( dates))


# Exclude a series as an example

database <- data[-length(data)]
names(database) %>% tail()

classify_series <- function(database, query) {
  # Nearest neighbor
  nn <- which.min(dtw_lb(database, query,
                         window.size = 60,
                         nn.margin = 2L))
  # Return a label
  database_labels <- names(database)
  database_labels[nn]
}
# 100-th ser

my_target_point <- data[length(data)] %>% names() 
my_nearest_points <- classify_series(database, data[my_target_point])

roll_rs %>% 
  filter( start_date %in% c(  as.Date(my_target_point) , as.Date(my_nearest_points )) ) %>% 
  mutate( raw_data = map( splits, ~assessment(.x))) %>% 
  mutate( raw_data = map2( .x = raw_data, .y = start_date, ~{ .x  %>% mutate( period = .y  ) 
  })) %>% 
  mutate( raw_data = map( .x = raw_data,  ~{ .x  %>% mutate( id = 1:nrow(.x)) })) %>% 
  pull( raw_data ) %>% 
  bind_rows() %>% 
  mutate( period = as.factor(period)) %>% #tail()
  #.[[1]] %>% 
  ggplot() +
  geom_line( aes( dates, values, color = period, group = period )) +
  facet_wrap(~ period, ncol = 1, scale = "free") +
  theme_ipsum()


roll_rs %>% 
  filter( start_date == "2018-06-13" ) %>% 
  mutate( raw_data = map( splits, ~assessment(.x))) %>% 
  pull( raw_data ) %>% 
  .[[1]] %>% 
  ggplot() +
  geom_line( aes( dates, values, color = variables )) +
  theme_ipsum()


hc <- tsclust( data ,  type = "partitional", k = 4, 
              distance = "dtw2", trace = TRUE, centroid = "shape")
plot(hc, type = "centroids")
plot(hc,type="series")



pc
plot(pc)
# Hierarchical
hc <- tsclust(test, type = "hierarchical", k = 20L, 
              distance = "sbd", trace = TRUE,centroid = shape_extraction,
              control = hierarchical_control(method = "average"))

plot(hc)
plot(hc , type = "series", clus = 1L)
plot(hc, type = "centroids", clus = 1L)
Fuzzy
# Calculate autocorrelation up to 50th lag, considering a list of time series as input
acf_fun <- function(series, ...) {
  lapply(series, function(x) { as.numeric(acf(x, lag.max = 50L, plot = FALSE)$acf) })
}
# Autocorrelation-based fuzzy c-means
fc <- tsclust(test[1L:25L], type = "fuzzy", k = 5L,
              preproc = acf_fun, distance = "L2",
              seed = 123L)
fc

# Some multivariate support

# Multivariate series provided as a list of matrices, using GAK distance
mvc <- tsclust(test[1L:20L], k = 4L, distance = "gak", seed = 390L)
# Note how the variables of each series are appended one after the other in the plot
plot(mvc, labels = list(nudge_x = -10, nudge_y = 1))

# Reinterpolate to same length
data <- reinterpolate(test, new.length = max(lengths(test)))
# Calculate the DTW distances between all elements
system.time(D1 <- proxy::dist(test[5L], test[6L:20L],
                              method = "dtw_basic",
                              window.size = 20L))
## user system elapsed
## 0.063 0.000 0.063
# Nearest neighbors
NN1 <- apply(D1, 1L, which.min)