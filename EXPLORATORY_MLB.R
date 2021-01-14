.libPaths("C:/R")
#remove scientific notation
options(scipen=999)

library(ggplot2)
library(dplyr)

#Occuring porpotions of (combined) variables for:
table.1 <- prop.table(table(MLB_Data$pitch_group, MLB_Data$pitch_zone))  #pitch type & location
table.2 <- prop.table(table(MLB_Data$pitch_zone)) #pitch location
table.3 <- prop.table(table(MLB_Data$pitch_group)) #pitch type
table.4 <- prop.table(table(MLB_Data$type_pitcher)) #type of pitcher
table.5 <- prop.table(table(MLB_Data$pitch)) #combined pitch type and location
table.6 <- prop.table(table(MLB_Data$count)) #ball-strike count
table.7 <- prop.table(table(MLB_Data$outs_when_up)) #how many outs when at-bat
table.8 <- prop.table(table(MLB_Data$p_throws_stand)) #combined throwing arm and batter stance
table.9 <- prop.table(table(MLB_Data$p_throws)) #throwing arm
table.10 <- prop.table(table(MLB_Data$stand)) #batter stance
table.11 <- prop.table(table(MLB_Data$lag1.pitch_result)) #previous pitch result

#Frequency of pitch type per count
plot.1 <- ggplot(MLB_Data, aes(x=pitch_group)) + geom_bar(aes(y = ..prop..,group = 1)) + facet_wrap(~count) 
plot.1 + ggtitle("Pitch type frequency \n per count") + xlab("Pitch type") + ylab("Frequency")

#Frequency of pitch zone per count
plot.2 <- ggplot(MLB_Data, aes(x=pitch_zone)) + geom_bar(aes(y = ..prop..,group = 1)) + facet_wrap(~count) 
plot.2 + ggtitle("Pitch zone frequency \n per count") + xlab("Pitch zone") + ylab("Frequency")

#Frequency of pitch type per combination batter stance and throwing arm of pitcher
plot.3 <- ggplot(MLB_Data, aes(x=pitch_group)) + geom_bar(aes(y = ..prop..,group = 1)) + facet_wrap(~p_throws_stand) 
plot.3 + ggtitle("Pitch type frequency \n per interaction throwing arm and batting stance") + xlab("Pitch type") + ylab("Frequency")

#Frequency of pitch zone per combination batter stance and throwing arm of pitcher
plot.4 <- ggplot(MLB_Data, aes(x=pitch_zone)) + geom_bar(aes(y = ..prop..,group = 1)) + facet_wrap(~p_throws_stand) 
plot.4 + ggtitle("Pitch loation frequency \n per interaction throwing arm and batting stance") + xlab("Pitch zone") + ylab("Frequency")


nums <- unlist(lapply(MLB_Data, is.numeric))  
MLB_numerical <-MLB_Data[,nums] %>%  #select numerical variables that are used in the model
  select(inning, pitch_count, score_diff) 
cor(MLB_numerical) #correlation of numberical mlb variables

#summary description
summary(MLB_Data)

mean_pitcher <- tapply(MLB_Data$pitch_count, MLB_Data$type_pitcher, mean)
sd_pitcher <- tapply(MLB_Data$pitch_count, MLB_Data$type_pitcher, sd)
min_pitcher <- tapply(MLB_Data$pitch_count, MLB_Data$type_pitcher, min)
max_pitcher <- tapply(MLB_Data$pitch_count, MLB_Data$type_pitcher, max)
median_pitcher <- tapply(MLB_Data$pitch_count, MLB_Data$type_pitcher, median)


#variable if a hitting attempt occured
MLB_Data$contact_hit <- ifelse(MLB_Data$description == "bunt_foul_tip" | MLB_Data$description =="foul"| MLB_Data$description ==  "foul_bunt"|
                                 MLB_Data$description == "foul_pitchout" | MLB_Data$description ==  "foul_tip"| MLB_Data$description ==  "missed_bunt"|
                                 MLB_Data$description == "swinging_strike_blocked"| MLB_Data$description == "swinging_strike" |
                                 MLB_Data$description == "hit_into_play"| MLB_Data$description == "hit_into_play_no_out"| 
                                 MLB_Data$description == "hit_into_play_score"| MLB_Data$description == "pitchout_hit_into_play"|
                                 MLB_Data$description == "pitchout_hit_into_play_score" | MLB_Data$description =="pitchout_hit_into_play_no_out",1,0)
#how often a hitting attempt occured
attempt_hit <- sum(MLB_Data$contact_hit)
#how often a hit occured
hit <- sum(MLB_Data$event_hit)


#mean and sd for the pitch types
speed_means <- MLB_Data %>% 
  subset(pitch_group %in% c("Fastball","Breakingball","Changeup")) %>% 
  group_by(pitch_group) %>% 
  summarise(Mean = mean(release_speed),
            SD = sd(release_speed))

#plot speed per pitch type
ggplot(subset(MLB_Data), aes(x = release_speed)) +
  geom_histogram(binwidth = 1, color = "grey30") +
  geom_vline(data = speed_means, aes(xintercept = Mean), linetype = "dashed") +
  facet_grid(~ pitch_group) +
  xlim(60,105) +
  ylab("Frequency") +
  xlab("Pitch Speed (mph)") +
  ggtitle("Pitch Velocity by Pitch Type") +
  theme(panel.grid.minor = element_blank(),
        axis.ticks = element_blank())

