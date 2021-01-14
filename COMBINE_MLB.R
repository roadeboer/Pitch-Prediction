library(tidyverse)
library(stringr)
library(janitor)

#read yearly MLB season data
MLBx2008 <- readRDS("MLBx2008.rds")
MLBx2009 <- readRDS("MLBx2009.rds")
MLBx2010 <- readRDS("MLBx2010.rds")
MLBx2011 <- readRDS("MLBx2011.rds")
MLBx2012 <- readRDS("MLBx2012.rds")
MLBx2013 <- readRDS("MLBx2013.rds")
MLBx2014 <- readRDS("MLBx2014.rds")
MLBx2015 <- readRDS("MLBx2015.rds")
MLBx2016 <- readRDS("MLBx2016.rds")
MLBx2017 <- readRDS("MLBx2017.rds")
MLBx2018 <- readRDS("MLBx2018.rds")
MLBx2019 <- readRDS("MLBx2019.rds")

#remove entire row or column when empty
MLBx2008 <- remove_empty(MLBx2008, which = c("rows","cols"), quiet = TRUE)
MLBx2009 <- remove_empty(MLBx2009, which = c("rows","cols"), quiet = TRUE)
MLBx2010 <- remove_empty(MLBx2010, which = c("rows","cols"), quiet = TRUE)
MLBx2011 <- remove_empty(MLBx2011, which = c("rows","cols"), quiet = TRUE)
MLBx2012 <- remove_empty(MLBx2012, which = c("rows","cols"), quiet = TRUE)
MLBx2013 <- remove_empty(MLBx2013, which = c("rows","cols"), quiet = TRUE)
MLBx2014 <- remove_empty(MLBx2014, which = c("rows","cols"), quiet = TRUE)
MLBx2015 <- remove_empty(MLBx2015, which = c("rows","cols"), quiet = TRUE)
MLBx2016 <- remove_empty(MLBx2016, which = c("rows","cols"), quiet = TRUE)
MLBx2017 <- remove_empty(MLBx2017, which = c("rows","cols"), quiet = TRUE)
MLBx2018 <- remove_empty(MLBx2018, which = c("rows","cols"), quiet = TRUE)
MLBx2019 <- remove_empty(MLBx2019, which = c("rows","cols"), quiet = TRUE)

#create 'at bat' ID for every batter
MLBx2008$ab_id <- MLBx2008 %>%
  group_by(game_year, game_date, game_pk, inning, at_bat_number, batter) %>% 
  group_indices

MLBx2009$ab_id <- MLBx2009 %>%
  group_by(game_year, game_date, game_pk, inning, at_bat_number, batter) %>% 
  group_indices


MLBx2010$ab_id <- MLBx2010 %>%
  group_by(game_year, game_date, game_pk, inning, at_bat_number, batter) %>% 
  group_indices


MLBx2011$ab_id <- MLBx2011 %>%
  group_by(game_year, game_date, game_pk, inning, at_bat_number, batter) %>% 
  group_indices


MLBx2012$ab_id <- MLBx2012 %>%
  group_by(game_year, game_date, game_pk, inning, at_bat_number, batter) %>% 
  group_indices


MLBx2013$ab_id <- MLBx2013 %>%
  group_by(game_year, game_date, game_pk, inning, at_bat_number, batter) %>% 
  group_indices


MLBx2014$ab_id <- MLBx2014 %>%
  group_by(game_year, game_date, game_pk, inning, at_bat_number, batter) %>% 
  group_indices

MLBx2015$ab_id <- MLBx2015 %>%
  group_by(game_year, game_date, game_pk, inning, at_bat_number, batter) %>% 
  group_indices

MLBx2016$ab_id <- MLBx2016 %>%
  group_by(game_year, game_date, game_pk, inning, at_bat_number, batter) %>% 
  group_indices


MLBx2017$ab_id <- MLBx2017 %>%
  group_by(game_year, game_date, game_pk, inning, at_bat_number, batter) %>% 
  group_indices


MLBx2018$ab_id <- MLBx2018 %>%
  group_by(game_year, game_date, game_pk, inning, at_bat_number, batter) %>% 
  group_indices


MLBx2019$ab_id <- MLBx2019 %>%
  group_by(game_year, game_date, game_pk, inning, at_bat_number, batter) %>% 
  group_indices

#find columns that match by name for 2008 and 2018. 
col_common <- intersect(colnames(MLBx2008), colnames(MLBx2018))

#function to find columns that do not match by name
outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}
#columns names that do not match
col_nocommon <- outersect(colnames(MLBx2008), colnames(MLBx2018))

#keep columns that match
MLBx2008 <- MLBx2008[,col_common]
MLBx2009 <- MLBx2009[,col_common]
MLBx2010 <- MLBx2010[,col_common]
MLBx2011 <- MLBx2011[,col_common]
MLBx2012 <- MLBx2012[,col_common]
MLBx2013 <- MLBx2013[,col_common]
MLBx2014 <- MLBx2014[,col_common]
MLBx2015 <- MLBx2015[,col_common]
MLBx2016 <- MLBx2016[,col_common]
MLBx2017 <- MLBx2017[,col_common]
MLBx2018 <- MLBx2018[,col_common]

#change column name to match
names(MLBx2019)[names(MLBx2019) == "pitcher_1"] <- "pitcher.1"
col_common_19 <- intersect(colnames(MLBx2018), colnames(MLBx2019))
col_nocommon_19 <- outersect(colnames(MLBx2018), colnames(MLBx2019))
MLBx2019 <- MLBx2019[,col_common]

#combine all yearly mlb seasons
MLB_Data <- rbind(MLBx2008, MLBx2009, MLBx2010, MLBx2011, MLBx2012, MLBx2013, MLBx2014, MLBx2015, MLBx2016, MLBx2017, MLBx2018, MLBx2019)

#remove seperate seasons
rm(MLBx2008, MLBx2009, MLBx2010, MLBx2011, MLBx2012, MLBx2013, MLBx2014, MLBx2015, MLBx2016, MLBx2017, MLBx2018, MLBx2019)

#save dataframe of combine seasons 2008 - 2019
#saveRDS(MLB_Data, "MLB_Data.rds")

#remove duplicated rows from the dataset. in this case only keep the first occuring row of the duplicate
MLB_Data <- distinct(MLB_Data)

#save dataframe
#saveRDS(MLB_Data, "MLB_Data_Combine_MLB.rds")


