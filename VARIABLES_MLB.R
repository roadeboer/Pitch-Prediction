.libPaths("C:/R")
library(dplyr)

#MLB_Data <- readRDS("MLB_Data_Combine_MLB.rds")

#************************CREATE AT-BAT ID VARIABLE IF YOU ARE NOT COMBINING THE SEASON DATA
#create at-bat id
# MLB_Data$ab_id <- MLB_Data %>%
#   group_by(game_year, game_date, game_pk, inning, at_bat_number, batter) %>% 
#   group_indices

#variables that will be dropped
drops <- c("babip_value", "barrel", "effective_speed", "estimated_ba_using_speedangle", 
           "estimated_woba_using_speedangle", "fld_score", "game_type","hc_x", "hc_y", "hit_distance_sc",
           "hit_location", "if_fielding_alignment", "iso_value", "launch_angle", "launch_speed", "launch_speed_angle",
           "release_extension", "release_pos_x", "release_pos_y", "release_pos_z", "release_spin_rate", "woba_denom", 
           "woba_value", "fielder_2","bat_score","post_away_score","post_home_score", "post_bat_score", "post_fld_score", "of_fielding_alignment")
#remove variables from dataset
MLB_Data <- MLB_Data[ , !(names(MLB_Data) %in% drops)]


#create count variable: combining amount of balls and strike into one variable
MLB_Data$count <- paste(MLB_Data$balls, MLB_Data$strikes, sep = "-")
#create variable to specify the team the pitcher is playing for
MLB_Data$pitcher_team <- ifelse(MLB_Data$inning_topbot =="Top",MLB_Data$away_team, MLB_Data$home_team)
#remove observations where 4 balls were recorded
MLB_Data<- MLB_Data[MLB_Data$balls != 4, ] 
#Remove the following pitch types. 
MLB_Data <- MLB_Data[MLB_Data$pitch_type != "null", ] 
MLB_Data <- MLB_Data[MLB_Data$pitch_type != "UN", ] #unknown
MLB_Data <- MLB_Data[MLB_Data$pitch_type != "PO" , ] #pitchout
MLB_Data <- MLB_Data[MLB_Data$pitch_type != "", ] #empty
MLB_Data <- MLB_Data[MLB_Data$pitch_type != "IN", ] # intentional balls

MLB_Data$on_1b <- ifelse(MLB_Data$on_1b != "NA",1,0) #dummy variable if runner on first base
MLB_Data$on_2b <- ifelse(MLB_Data$on_2b != "NA",1,0) #dummy variable if runner on second base
MLB_Data$on_3b <- ifelse(MLB_Data$on_3b != "NA",1,0) ##dummy variable if runner on third base

MLB_Data$on_1b[is.na(MLB_Data$on_1b)] <- 0 #change NA to 0 
MLB_Data$on_2b[is.na(MLB_Data$on_2b)] <- 0 #change NA to 0 
MLB_Data$on_3b[is.na(MLB_Data$on_3b)] <- 0 #change NA to 0 
MLB_Data$onbase <- MLB_Data$on_1b+ MLB_Data$on_2b+ MLB_Data$on_3b #amount on base
MLB_Data$score_pos <- ifelse(MLB_Data$on_2b+MLB_Data$on_3b>0,1,0) #dummy variable if runner on second or third

#copy pitch type variable, then reclassify these pitch types in fastball, breakingball, changeup, junkpitch
MLB_Data$pitch_group <- MLB_Data$pitch_type 

MLB_Data$pitch_group[MLB_Data$pitch_type == "FA" ] <- "Fastball" #Fast-ball 
MLB_Data$pitch_group[MLB_Data$pitch_type == "FF" ] <- "Fastball" #Fastball
MLB_Data$pitch_group[MLB_Data$pitch_type == "FT" ] <- "Fastball" #2-seam fastball
MLB_Data$pitch_group[MLB_Data$pitch_type == "FC" ] <- "Fastball" #cutter 
MLB_Data$pitch_group[MLB_Data$pitch_type == "SI" ] <- "Fastball" #Sinker


MLB_Data$pitch_group[MLB_Data$pitch_type == "CH" ] <- "Changeup" #change up
MLB_Data$pitch_group[MLB_Data$pitch_type == "SF" ] <- "Changeup" #split-fingered
MLB_Data$pitch_group[MLB_Data$pitch_type == "FS" ] <- "Changeup" #Splitter
MLB_Data$pitch_group[MLB_Data$pitch_type == "FO" ] <- "Changeup" #forkball

MLB_Data$pitch_group[MLB_Data$pitch_type == "SL" ] <- "Breakingball" #Slider
MLB_Data$pitch_group[MLB_Data$pitch_type == "CU" ] <- "Breakingball" #Curve
MLB_Data$pitch_group[MLB_Data$pitch_type == "KC" ] <- "Breakingball" #knuckle curve
MLB_Data$pitch_group[MLB_Data$pitch_type == "SC" ] <- "Breakingball" #Screwball
MLB_Data$pitch_group[MLB_Data$pitch_type == "KN" ] <- "Junkpitches" #Knuckleball
MLB_Data$pitch_group[MLB_Data$pitch_type == "EP" ] <- "Junkpitches" #Eephus

MLB_Data <- MLB_Data[!(is.na(MLB_Data$zone)), ] #remove NA values from column zone. 
MLB_Data$sz_mid <- (MLB_Data$sz_top - MLB_Data$sz_bot)/2 + MLB_Data$sz_bot #variable middle of strikezone

#variable than mentions in which quadrant the pitch was thrown. 
MLB_Data$pitch_zone <- ifelse(MLB_Data$stand == "R" & MLB_Data$plate_x < 0 & MLB_Data$plate_z > MLB_Data$sz_mid, "IH",
                               ifelse(MLB_Data$stand == "R" & MLB_Data$plate_x < 0 & MLB_Data$plate_z < MLB_Data$sz_mid, "IL",
                                      ifelse(MLB_Data$stand == "R" & MLB_Data$plate_x > 0 & MLB_Data$plate_z > MLB_Data$sz_mid, "OH",
                                             ifelse(MLB_Data$stand == "R" & MLB_Data$plate_x > 0 & MLB_Data$plate_z < MLB_Data$sz_mid, "OL",
                                                    ifelse(MLB_Data$stand == "L" & MLB_Data$plate_x < 0 & MLB_Data$plate_z > MLB_Data$sz_mid, "OH",
                                                           ifelse(MLB_Data$stand == "L" & MLB_Data$plate_x < 0 & MLB_Data$plate_z < MLB_Data$sz_mid, "OL",
                                                                  ifelse(MLB_Data$stand == "L" & MLB_Data$plate_x > 0 & MLB_Data$plate_z > MLB_Data$sz_mid, "IH","IL")
                                                           )
                                                    )
                                             )
                                      )
                               )
)
#pitches thrown that were a strike
MLB_Data$event_strike <- ifelse(MLB_Data$description == "bunt_foul_tip" |  MLB_Data$description == "called_strike"| MLB_Data$description =="foul"| 
                                  MLB_Data$description ==  "foul_bunt"| MLB_Data$description == "foul_pitchout" | MLB_Data$description ==  "foul_tip"|
                                  MLB_Data$description ==  "missed_bunt"|MLB_Data$description == "swinging_strike_blocked"| 
                                  MLB_Data$description == "unknown strike" | MLB_Data$description == "swinging_strike" | MLB_Data$description == "swinging_strike",1,0)
#pitches thrown that were a ball
MLB_Data$event_ball <- ifelse(MLB_Data$description == "ball"| MLB_Data$description == "blocked_ball" | MLB_Data$description == "hit_by_pitch"|
                                MLB_Data$description == "intent_ball" | MLB_Data$description ==  "pitchout",1,0)     
#pitches thrown that were a hit
MLB_Data$event_hit <- ifelse(MLB_Data$description == "hit_into_play"| MLB_Data$description == "hit_into_play_no_out"| 
                               MLB_Data$description == "hit_into_play_score"| MLB_Data$description == "pitchout_hit_into_play"|
                               MLB_Data$description == "pitchout_hit_into_play_score" | MLB_Data$description =="pitchout_hit_into_play_no_out", 1,0)     
#drop even more columns
drop <- c("spin_dir" ,"spin_rate_deprecated","break_angle_deprecated" ,
          "break_length_deprecated","pfx_x"  , "pfx_z"  , "plate_x" ,"plate_z"  ,
          "tfs_deprecated" ,   "tfs_zulu_deprecated", "umpire", "vx0"  , "vy0" ,  "vz0"  ,  "ax" ,
          "ay" ,"az" , "sz_top", "sz_bot" ,'sz_mid', "pitcher.1"  ,   "fielder_3", "fielder_4"  , "fielder_5" ,
          "fielder_6" , "fielder_7","fielder_8"  , "fielder_9") 
#"events", "description", "bb_type", "des"
MLB_Data <- MLB_Data[ , !(names(MLB_Data) %in% drop)]
#combine the created pitch type variable with the created pitch zone variable
MLB_Data$pitch <- paste(MLB_Data$pitch_group, MLB_Data$pitch_zone, sep = "-") 

#remove the junk pitches & for every pitcher in every game mention the amount of balls thrown up until the pitch. 
MLB_Data <-MLB_Data %>%
  filter(pitch_group != "Junkpitches") %>%
  arrange(game_year, game_pk, ab_id, pitch_number)%>%
  group_by(game_pk, pitcher) %>%
  mutate(pitch_count = row_number()) %>%
  ungroup()

#add the innings thrown per pitcher per game
MLB_Data <- MLB_Data %>% 
  group_by(game_pk, pitcher_team, pitcher) %>% 
  mutate(innings_thrown = length(unique(inning))) %>% 
  ungroup()

#create dummy variable to distinguish the type of pitcher. closer, setup, starter.
MLB_Data <- MLB_Data %>% 
  group_by(game_pk, pitcher_team, pitcher) %>% 
  mutate(closer = ifelse(innings_thrown ==1 & inning >= 9, 1, 0),
         setup = ifelse(innings_thrown ==1 & inning == 8, 1, 0),
         starter_first = ifelse(pitch_count == 1 & inning ==1, 1, 0),
         starter = ifelse(any(starter_first==1),1,0)) %>% 
  ungroup()
#create dummy variable for the middle reliever. All pitcher except starters, setup men and closers
MLB_Data$middle_reliever <- ifelse(MLB_Data$closer == 0 & MLB_Data$starter ==0 & MLB_Data$setup ==0, 1,0)

#create variable that mentions the difference in runs (score)
MLB_Data$score_diff <- ifelse(MLB_Data$inning_topbot == "Top", MLB_Data$away_score-MLB_Data$home_score, MLB_Data$home_score-MLB_Data$away_score)

#create previous pitch (type & location) for lag 1-3
MLB_Data <- MLB_Data %>%
  arrange(game_year, game_pk, ab_id, pitch_number) %>% 
  group_by(ab_id) %>% 
  mutate(lag1.zone = lag(pitch_zone, order_by=pitch_number, n = 1),
        # lag2.zone = lag(pitch_zone, order_by=pitch_number, n = 2),
         #lag3.zone = lag(pitch_zone, order_by=pitch_number, n = 3),
         lag1.pitch = lag(pitch_group, order_by=pitch_number, n = 1),
         #lag2.pitch = lag(pitch_group, order_by=pitch_number, n = 2),
         #lag3.pitch = lag(pitch_group, order_by=pitch_number, n = 3)
  ) %>% 
  ungroup()

#Change all NA values to 'None' for the previous pitches
MLB_Data$lag1.pitch <- as.character(MLB_Data$lag1.pitch)
MLB_Data$lag1.pitch[is.na(MLB_Data$lag1.pitch)] <- "None"

# MLB_Data$lag2.pitch <- as.character(MLB_Data$lag2.pitch)
# MLB_Data$lag2.pitch[is.na(MLB_Data$lag2.pitch)] <- "None"
# 
# MLB_Data$lag3.pitch <- as.character(MLB_Data$lag3.pitch)
# MLB_Data$lag3.pitch[is.na(MLB_Data$lag3.pitch)] <- "None"

MLB_Data$lag1.zone <- as.character(MLB_Data$lag1.zone)
MLB_Data$lag1.zone[is.na(MLB_Data$lag1.zone)] <- "None"

# MLB_Data$lag2.zone <- as.character(MLB_Data$lag2.zone)
# MLB_Data$lag2.zone[is.na(MLB_Data$lag2.zone)] <- "None"
# 
# MLB_Data$lag3.zone <- as.character(MLB_Data$lag3.zone)
# MLB_Data$lag3.zone[is.na(MLB_Data$lag3.zone)] <- "None"


#rename abbreviations of pitch result, either being a strike (S), ball(B) or hit (X)
MLB_Data <- MLB_Data %>% 
  mutate(type= ifelse(type == "S", "Strike", 
                      ifelse(type == "B", "Ball", "Hit"))) 

#create previous result of thrown pitch
MLB_Data <- MLB_Data %>%
  arrange(game_year, game_pk, ab_id, pitch_number) %>% 
  group_by(game_pk, pitcher) %>% 
  mutate(lag1.pitch_result = lag(type, order_by=pitch_count, n = 1)) %>% 
  ungroup()

#change NA to None for the variable previous pitch result
MLB_Data$lag1.pitch_result[is.na(MLB_Data$lag1.pitch_result)] <- "None"

#factor variables
MLB_Data <- MLB_Data %>% 
  mutate(pitch_group = factor(pitch_group),
         pitch_zone = factor(pitch_zone),
         count = factor(count),
         stand = factor(stand),
         p_throws = factor(p_throws),
         lag1.pitch = factor(lag1.pitch),
         #lag2.pitch = factor(lag2.pitch),
         #lag3.pitch = factor(lag3.pitch),
         lag1.zone = factor(lag1.zone),
         #lag2.zone = factor(lag2.zone),
         #lag3.zone = factor(lag3.zone),
         lag1.pitch_result = factor(lag1.pitch_result),
         outs_when_up = factor(outs_when_up),
         pitcher = factor(pitcher)
  )

#Use observations that contain information about the throwing arm of the pitcher
MLB_Data <- MLB_Data %>% 
  filter(p_throws == "L" | p_throws == "R")

MLB_Data$p_throws <- factor(MLB_Data$p_throws, level = c("L", "R"))
#set the reference for the pitch location
MLB_Data$pitch_zone<- relevel(MLB_Data$pitch_zone, ref = "OL")
#set the reference for the pitch type
MLB_Data$pitch_group<- relevel(MLB_Data$pitch_group, ref = "Fastball")
#factor pitch variable and set the reference
MLB_Data$pitch <-factor(MLB_Data$pitch)
MLB_Data$pitch<- relevel(MLB_Data$pitch, ref = "Fastball-OL")

#Set the reference for the Previous pitches type or zone to None
MLB_Data$lag1.pitch<- relevel(MLB_Data$lag1.pitch, ref = "None")
#MLB_Data$lag2.pitch<- relevel(MLB_Data$lag2.pitch, ref = "None")
#MLB_Data$lag3.pitch<- relevel(MLB_Data$lag3.pitch, ref = "None")
MLB_Data$lag1.zone<- relevel(MLB_Data$lag1.zone, ref = "None")
#MLB_Data$lag2.zone<- relevel(MLB_Data$lag2.zone, ref = "None")
#MLB_Data$lag3.zone<- relevel(MLB_Data$lag3.zone, ref = "None")
MLB_Data$lag1.pitch_result <- relevel(MLB_Data$lag1.pitch_result, ref = "None")

MLB_Data$lag1.pitch <- relevel(MLB_Data$lag1.pitch, ref = "Fastball")
#combine throwing arm and batter stance in one variable
MLB_Data$p_throws_stand <- paste(MLB_Data$p_throws, MLB_Data$stand)

#create variable that mentions what kind of pitcher 
MLB_Data$type_pitcher <- ifelse(MLB_Data$starter == 1, "starter", 
                                ifelse(MLB_Data$middle_reliever==1, "middle reliever",
                                       ifelse(MLB_Data$setup ==1, "set up","closer")))
MLB_Data$type_pitcher<- relevel(factor(MLB_Data$type_pitcher), ref = "starter") 
#saveRDS(MLB_Data, "MLB_Data_Variables.rds")



