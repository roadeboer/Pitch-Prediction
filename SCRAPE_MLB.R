library(ggplot2)
library(dplyr)
library(maps)
library(ggmap)
library(lubridate)
library(gridExtra)
library(tictoc)
library(devtools)
library(tidyverse)
#Use Bill Petti's Package baseballr
#http://billpetti.github.io/baseballr/
#install_github("BillPetti/baseballr")
library(baseballr)



#Load dates from csv file
dates_reduced <- read.csv("https://raw.githubusercontent.com/BillPetti/baseball_research_notebook/master/dates_statcast_build.csv")

#Subset dates by year
x2008season <- dates_reduced %>%
  filter(substr(GameDate, 1, 4) == 2008)

x2009season <- dates_reduced %>%
  filter(substr(GameDate, 1, 4) == 2009)

x2010season <- dates_reduced %>%
  filter(substr(GameDate, 1, 4) == 2010)

x2011season <- dates_reduced %>%
  filter(substr(GameDate, 1, 4) == 2011)

x2012season <- dates_reduced %>%
  filter(substr(GameDate, 1, 4) == 2012)

x2013season <- dates_reduced %>%
  filter(substr(GameDate, 1, 4) == 2013)

x2014season <- dates_reduced %>%
  filter(substr(GameDate, 1, 4) == 2014)

x2015season <- dates_reduced %>%
  filter(substr(GameDate, 1, 4) == 2015)

x2016season <- dates_reduced %>%
  filter(substr(GameDate, 1, 4) == 2016)

x2017season <- dates_reduced %>%
  filter(substr(GameDate, 1, 4) == 2017)

#Function that scrapes the data from 2008-2015
scrape_statcast_savant_pitcher_date <- function(start_date, end_date) {
  
  # extract year
  year <- substr(start_date, 1,4)
  
  # Base URL.
  url <- paste0("https://baseballsavant.mlb.com/statcast_search/csv?all=true&hfPT=&hfAB=&hfBBT=&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=&hfGT=R%7C&hfC=&hfSea=",year,"%7C&hfSit=&player_type=pitcher&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt=",start_date,"&game_date_lt=",end_date,"&team=&position=&hfRO=&home_road=&hfFlag=&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=pitches&player_event_sort=h_launch_speed&sort_order=desc&min_abs=0&type=details&")
  
  payload <- utils::read.csv(url)
  
  if (length(payload$pitch_type) > 0) {
    
    # Clean up formatting.
    payload$game_date <- as.Date(payload$game_date, "%Y-%m-%d")
    payload$des <- as.character(payload$des)
    payload$game_pk <- as.character(payload$game_pk) %>% as.numeric()
    payload$on_1b <- as.character(payload$on_1b) %>% as.numeric()
    payload$on_2b <- as.character(payload$on_2b) %>% as.numeric()
    payload$on_3b <- as.character(payload$on_3b) %>% as.numeric()
    payload$release_pos_x <- as.character(payload$release_pos_x) %>% as.numeric()
    payload$release_pos_z <- as.character(payload$release_pos_z) %>% as.numeric()
    payload$release_pos_y <- as.character(payload$release_pos_y) %>% as.numeric()
    payload$pfx_x <- as.character(payload$pfx_x) %>% as.numeric()
    payload$pfx_z <- as.character(payload$pfx_z) %>% as.numeric()
    payload$hc_x <- as.character(payload$hc_x) %>% as.numeric()
    payload$hc_y <- as.character(payload$hc_y) %>% as.numeric()
    payload$woba_denom <- as.character(payload$woba_denom) %>% as.numeric()
    payload$woba_value <- as.character(payload$woba_value) %>% as.numeric()
    payload$babip_value <- as.character(payload$babip_value) %>% as.numeric()
    payload$iso_value <- as.character(payload$iso_value) %>% as.numeric()
    payload$plate_z <- as.character(payload$plate_z) %>% as.numeric()
    payload$plate_x <- as.character(payload$plate_x) %>% as.numeric()
    payload$vx0 <- as.character(payload$vx0) %>% as.numeric()
    payload$vy0 <- as.character(payload$vy0) %>% as.numeric()
    payload$vz0 <- as.character(payload$vz0) %>% as.numeric()
    payload$ax <- as.character(payload$ax) %>% as.numeric()
    payload$ay <- as.character(payload$ay) %>% as.numeric()
    payload$az <- as.character(payload$az) %>% as.numeric()
    payload$sz_top <- as.character(payload$sz_top) %>% as.numeric()
    payload$sz_bot <- as.character(payload$sz_bot) %>% as.numeric()
    payload$hit_distance_sc <- as.character(payload$hit_distance_sc) %>% as.numeric()
    payload$launch_speed <- as.character(payload$launch_speed) %>% as.numeric()
    payload$launch_speed_angle <- as.character(payload$launch_speed_angle) %>% as.numeric()
    payload$launch_angle <- as.character(payload$launch_angle) %>% as.numeric()
    payload$estimated_ba_using_speedangle <- as.character(payload$estimated_ba_using_speedangle) %>% as.numeric()
    payload$estimated_woba_using_speedangle <- as.character(payload$estimated_woba_using_speedangle) %>% as.numeric()
    payload$effective_speed <- as.character(payload$effective_speed) %>% as.numeric()
    payload$release_speed <- as.character(payload$release_speed) %>% as.numeric()
    payload$zone <- as.character(payload$zone) %>% as.numeric()
    payload$release_spin_rate <- as.character(payload$release_spin_rate) %>% as.numeric()
    payload$release_extension <- as.character(payload$release_extension) %>% as.numeric()
    payload$barrel <- with(payload, ifelse(launch_angle <= 50 & launch_speed >= 98 & launch_speed * 1.5 - launch_angle >= 117 & launch_speed + launch_angle >= 124, 1, 0))
    payload$home_team <- as.character(payload$home_team)
    payload$away_team <- as.character(payload$away_team)
    
    return(payload)
  }
  
  else {
    vars <- names(payload)
    df <- lapply(vars, function(x) x <- NA)
    names(df) <- names(payload)
    payload_na <- bind_rows(df)
    
    return(payload_na)
    
    Sys.sleep(sample(x = runif(20, min = .01, max = 1), size = 1))
  }
}

#Modified function from above that scrapes the data for 2016-2018
scrape_statcast_savant_pitcher_date_modified <- function(start_date, end_date) {
  
  # extract year
  year <- substr(start_date, 1,4)
  
  # Base URL.
  url <- paste0("https://baseballsavant.mlb.com/statcast_search/csv?all=true&hfPT=&hfAB=&hfBBT=&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=&hfGT=R%7C&hfC=&hfSea=",year,"%7C&hfSit=&player_type=pitcher&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt=",start_date,"&game_date_lt=",end_date,"&team=&position=&hfRO=&home_road=&hfFlag=&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=pitches&player_event_sort=h_launch_speed&sort_order=desc&min_abs=0&type=details&")
  
  payload <- utils::read.csv(url)
  
  if ("fielder_2" %in% names(payload)) {
    
    payload$pitcher   <- as.character(payload$pitcher)   %>% as.integer
    payload$fielder_2 <- as.character(payload$fielder_2) %>% as.integer
    payload$fielder_3 <- as.character(payload$fielder_3) %>% as.integer
    payload$fielder_4 <- as.character(payload$fielder_4) %>% as.integer
    payload$fielder_5 <- as.character(payload$fielder_5) %>% as.integer
    payload$fielder_6 <- as.character(payload$fielder_6) %>% as.integer
    payload$fielder_7 <- as.character(payload$fielder_7) %>% as.integer
    payload$fielder_8 <- as.character(payload$fielder_8) %>% as.integer
    payload$fielder_9 <- as.character(payload$fielder_9) %>% as.integer
  }
  if (("fielder_2.1" %in% names(payload))) {
    payload$fielder_2.1 <- as.character(payload$fielder_2) %>% as.integer
  }
  
  if (length(payload$pitch_type) > 0) {
    
    # Clean up formatting.
    payload$game_date <- as.Date(payload$game_date, "%Y-%m-%d")
    payload$des <- as.character(payload$des)
    payload$game_pk <- as.character(payload$game_pk) %>% as.numeric()
    payload$on_1b <- as.character(payload$on_1b) %>% as.numeric()
    payload$on_2b <- as.character(payload$on_2b) %>% as.numeric()
    payload$on_3b <- as.character(payload$on_3b) %>% as.numeric()
    payload$release_pos_x <- as.character(payload$release_pos_x) %>% as.numeric()
    payload$release_pos_z <- as.character(payload$release_pos_z) %>% as.numeric()
    payload$release_pos_y <- as.character(payload$release_pos_y) %>% as.numeric()
    payload$pfx_x <- as.character(payload$pfx_x) %>% as.numeric()
    payload$pfx_z <- as.character(payload$pfx_z) %>% as.numeric()
    payload$hc_x <- as.character(payload$hc_x) %>% as.numeric()
    payload$hc_y <- as.character(payload$hc_y) %>% as.numeric()
    payload$woba_denom <- as.character(payload$woba_denom) %>% as.numeric()
    payload$woba_value <- as.character(payload$woba_value) %>% as.numeric()
    payload$babip_value <- as.character(payload$babip_value) %>% as.numeric()
    payload$iso_value <- as.character(payload$iso_value) %>% as.numeric()
    payload$plate_z <- as.character(payload$plate_z) %>% as.numeric()
    payload$plate_x <- as.character(payload$plate_x) %>% as.numeric()
    payload$vx0 <- as.character(payload$vx0) %>% as.numeric()
    payload$vy0 <- as.character(payload$vy0) %>% as.numeric()
    payload$vz0 <- as.character(payload$vz0) %>% as.numeric()
    payload$ax <- as.character(payload$ax) %>% as.numeric()
    payload$ay <- as.character(payload$ay) %>% as.numeric()
    payload$az <- as.character(payload$az) %>% as.numeric()
    payload$sz_top <- as.character(payload$sz_top) %>% as.numeric()
    payload$sz_bot <- as.character(payload$sz_bot) %>% as.numeric()
    payload$hit_distance_sc <- as.character(payload$hit_distance_sc) %>% as.numeric()
    payload$launch_speed <- as.character(payload$launch_speed) %>% as.numeric()
    payload$launch_speed_angle <- as.character(payload$launch_speed_angle) %>% as.numeric()
    payload$launch_angle <- as.character(payload$launch_angle) %>% as.numeric()
    payload$estimated_ba_using_speedangle <- as.character(payload$estimated_ba_using_speedangle) %>% as.numeric()
    payload$estimated_woba_using_speedangle <- as.character(payload$estimated_woba_using_speedangle) %>% as.numeric()
    payload$effective_speed <- as.character(payload$effective_speed) %>% as.numeric()
    payload$release_speed <- as.character(payload$release_speed) %>% as.numeric()
    payload$zone <- as.character(payload$zone) %>% as.numeric()
    payload$release_spin_rate <- as.character(payload$release_spin_rate) %>% as.numeric()
    payload$release_extension <- as.character(payload$release_extension) %>% as.numeric()
    payload$barrel <- with(payload, ifelse(launch_angle <= 50 & launch_speed >= 98 & launch_speed * 1.5 - launch_angle >= 117 & launch_speed + launch_angle >= 124, 1, 0))
    payload$home_team <- as.character(payload$home_team)
    payload$away_team <- as.character(payload$away_team)
    
    return(payload)
  }
  
  else {
    vars <- names(payload)
    df <- lapply(vars, function(x) x <- NA)
    names(df) <- names(payload)
    payload_na <- bind_rows(df)
    
    return(payload_na)
    
    Sys.sleep(sample(x = runif(20, min = .01, max = 1), size = 1))
  }
}

#Function that scrapes data from 2019
Savant_Scrape = function(weekstart, weekend, type){
  df = baseballr::scrape_statcast_savant(start_date = weekstart, 
                                         end_date = weekend, player_type = type)
}

#Retrieve yearly MLB pitch data using the dates from that specific year in the scrape function
x2008data <- x2008season %>%
  group_by(start_date) %>%
  do(scrape_statcast_savant_pitcher_date(.$start_date, .$end_date)) %>%
  ungroup() %>%
  select(-start_date)


(missing_2008 <- x2008data %>%
    filter(is.na(pitch_type)) %>%
    distinct(game_date) %>%
    select(game_date))

x2009data <- x2009season %>%
  group_by(start_date) %>%
  do(scrape_statcast_savant_pitcher_date(.$start_date, .$end_date)) %>%
  ungroup() %>%
  select(-start_date)

gc()

(missing_2009 <- x2009data %>%
    filter(is.na(pitch_type)) %>%
    distinct(game_date) %>%
    select(game_date))

x2010data <- x2010season %>%
  group_by(start_date) %>%
  do(scrape_statcast_savant_pitcher_date(.$start_date, .$end_date)) %>%
  ungroup() %>%
  select(-start_date)

gc()

(missing_2010 <- x2010data %>%
    filter(is.na(pitch_type)) %>%
    distinct(game_date) %>%
    select(game_date))

x2011data <- x2011season %>%
  group_by(start_date) %>%
  do(scrape_statcast_savant_pitcher_date(.$start_date, .$end_date)) %>%
  ungroup() %>%
  select(-start_date)

gc()

(missing_2011 <- x2011data %>%
    filter(is.na(pitch_type)) %>%
    distinct(game_date) %>%
    select(game_date))

x2012data <- x2012season %>%
  group_by(start_date) %>%
  do(scrape_statcast_savant_pitcher_date(.$start_date, .$end_date)) %>%
  ungroup() %>%
  select(-start_date)

gc()

(missing_2012 <- x2012data %>%
    filter(is.na(pitch_type)) %>%
    distinct(game_date) %>%
    select(game_date))

x2013data <- x2013season %>%
  group_by(start_date) %>%
  do(scrape_statcast_savant_pitcher_date(.$start_date, .$end_date)) %>%
  ungroup() %>%
  select(-start_date)

gc()

(missing_2013 <- x2013data %>%
    filter(is.na(pitch_type)) %>%
    distinct(game_date) %>%
    select(game_date))

x2014data <- x2014season %>%
  group_by(start_date) %>%
  do(scrape_statcast_savant_pitcher_date(.$start_date, .$end_date)) %>%
  ungroup() %>%
  select(-start_date)

gc()

(missing_2014 <- x2014data %>%
    filter(is.na(pitch_type)) %>%
    distinct(game_date) %>%
    select(game_date))

x2015data <- x2015season %>%
  group_by(start_date) %>%
  do(scrape_statcast_savant_pitcher_date(.$start_date, .$end_date)) %>%
  ungroup() %>%
  select(-start_date)

gc()

(missing_2015 <- x2015data %>%
    filter(is.na(pitch_type)) %>%
    distinct(game_date) %>%
    select(game_date))

x2016data <- x2016season %>%
  group_by(start_date) %>%
  do(scrape_statcast_savant_pitcher_date_modified(.$start_date, .$end_date)) %>%
  ungroup() %>%
  select(-start_date)

gc()

(missing_2016 <- x2016data %>%
    filter(is.na(pitch_type)) %>%
    distinct(game_date) %>%
    select(game_date))

x2017data <- x2017season %>%
  group_by(start_date) %>%
  do(scrape_statcast_savant_pitcher_date_modified(.$start_date, .$end_date)) %>%
  ungroup() %>%
  select(-start_date)

(missing_2017 <- x2017data %>%
    filter(is.na(pitch_type)) %>%
    distinct(game_date) %>%
    select(game_date))

# Get the data, will include days with no data
info_list<- list()
for (i in 60:360) {
  datei <- as.Date("2018-01-01") + i
  print(c(i, datei))
  day <- scrape_statcast_savant_pitcher_date_modified(datei, datei)
  info_list <- c(info_list, list(day))
}

# Remove days with no data, they have 1 row
keep <- sapply(1:length(info_list), function(j) {nrow(info_list[[j]]) > 2})
info_list <- info_list[keep]

info_list_2 <- lapply(info_list, function(il) {il$fielder_2.1 <- as.integer(as.character(il$fielder_2.1)); il})
x2018data <- bind_rows(info_list_2)
if (any(x2018data$fielder2 != x2018data$fielder_2.1)) {stop("fielder2 not fielder2.1")}
x2018data$fielder_2.1 <- NULL


#Load Statcast data week by week. In order to download the get the 2019 MLB season data
#Iterate through all 26 weeks. 
weeks = 26 #Number of weeks in the MLB Season
datalist = list()
#scrape 10 days at a time
for (i in (1:weeks)) {
  weekname = paste0("Week", i, sep = "")
  Start = as.Date('2019-03-21')
  End = as.Date("2019-03-28")
  dat = Savant_Scrape(Start + i*7,End + i*7, 'batter' )
  dat$i <- i
  datalist[[i]] <- dat
  
}
x2019data = do.call(rbind, datalist)


#Save objects in rds format 
saveRDS(x2008data, "C:/Users/ricar/Documents/Thesis/MLBx2008.rds")
saveRDS(x2009data, "C:/Users/ricar/Documents/Thesis/MLBx2009.rds")
saveRDS(x2010data, "C:/Users/ricar/Documents/Thesis/MLBx2010.rds")
saveRDS(x2011data, "C:/Users/ricar/Documents/Thesis/MLBx2011.rds")
saveRDS(x2012data, "C:/Users/ricar/Documents/Thesis/MLBx2012.rds")
saveRDS(x2013data, "C:/Users/ricar/Documents/Thesis/MLBx2013.rds")
saveRDS(x2014data, "C:/Users/ricar/Documents/Thesis/MLBx2014.rds")
saveRDS(x2015data, "C:/Users/ricar/Documents/Thesis/MLBx2015.rds")
saveRDS(x2016data, "C:/Users/ricar/Documents/Thesis/MLBx2016.rds")
saveRDS(x2017data, "C:/Users/ricar/Documents/Thesis/MLBx2017.rds")
saveRDS(x2018data, "C:/Users/ricar/Documents/Thesis/MLBx2018.rds")
saveRDS(x2019data, "C:/Users/ricar/Documents/Thesis/MLBx2019.rds")




