.libPaths("C:/R")
library(nnet)
library(caret)
library(stargazer)
library(dplyr)

pitcher_list <- MLB_Data %>% 
  group_by(pitcher) %>% 
  summarise(n=n(), games = length(unique(game_pk)))

pitcher_list <- pitcher_list[order(-pitcher_list$games),]
#rm(MLB_Data)

mlb <- MLB_Data %>% 
  select(pitch, pitch_group, pitch_zone, count , outs_when_up , pitch_count , inning , 
         stand, p_throws ,on_1b, on_2b, on_3b, score_diff, score_pos ,lag1.pitch ,lag1.zone ,lag1.pitch_result,type_pitcher,
         pitcher, release_speed
         #lag2.pitch ,lag3.pitch , 
         #lag2.zone ,lag3.zone,
  )

mlb$pitch_group<- relevel(mlb$pitch_group, ref = "Fastball")
#saveRDS(mlb, "mlb_Regression_MLB.rds")
#rm(MLB_Data)
#rm(pitcher_list)

#formula for the dependent variable pitch type, pitch zone and combined with the dependent variables 
type_1 <- "pitch_group ~ count + outs_when_up + pitch_count + inning + stand*p_throws + on_1b + on_2b + on_3b + score_diff + lag1.pitch + lag1.pitch_result + type_pitcher"
zone_1 <- "pitch_zone ~ count + outs_when_up + pitch_count + inning + stand*p_throws + on_1b + on_2b + on_3b + score_diff + lag1.pitch  + lag1.pitch_result + type_pitcher"
pitch_1 <- "pitch ~ count + outs_when_up + pitch_count + inning + stand*p_throws + on_1b + on_2b + on_3b + score_diff + lag1.pitch + lag1.pitch_result + type_pitcher"

fit.type_1 <-  multinom(type_1, data = mlb) #fit the data to pitch type model 
saveRDS(fit.type_1, file = "type_1.rds")

fit.zone_1 <-  multinom(zone_1, data = mlb) #fit the data to pitch location model
saveRDS(fit.zone_1, file = "zone_1.rds")

fit.pitch_1 <-  multinom(pitch_1, data = mlb, maxit = 500) #fit data to pitch type and pitch location model
saveRDS(fit.pitch_1, file = "pitch_1.rds") 

#get prediction probabilities and label, take exponential of coefficients for pitch type
predict.type_1 <- predict(fit.type_1, data = mlb)
predict.type_1 <-as.data.frame(predict.type_1)
output.type_1.exp <- exp(coefficients(fit.type_1))
predict_class.type_1 <- predict(fit.type_1, type = 'class')

#get prediction probabilities and label, take exponential of coefficients for pitch location
predict.zone_1 <- predict(fit.zone_1, data = mlb)
predict.zone_1 <-as.data.frame(predict.zone_1)
output.zone_1.exp <- exp(coefficients(fit.zone_1))
predict_class.zone_1 <- predict(fit.zone_1, type = 'class')

#get prediction probabilities and label, take exponential of coefficients for pitch type and location
predict.pitch_1 <- predict(fit.pitch_1, data = mlb)
predict.pitch_1 <-as.data.frame(predict.pitch_1)
output.pitch_1.exp <- exp(coefficients(fit.pitch_1))
predict_class.pitch_1 <- predict(fit.pitch_1, type = 'class')

confusion_type <- confusionMatrix(predict_class.type_1, mlb$pitch_group)
(prop.table(confusion_type$table))*100
confusion_zone <- confusionMatrix(predict_class.zone_1, mlb$pitch_zone)
(prop.table(confusion_zone$table))*100
confusion_pitch <- confusionMatrix(predict_class.pitch_1, mlb$pitch)
(prop.table(confusion_pitch$table))*100

#regression output of pitch type model in well-formatted regressin table
html_table.1 <- stargazer(fit.type_1, type="html", coef=list(output.type_1.exp), p.auto=FALSE,
                          single.row = TRUE, covariate.labels = c("Count (0-1)","Count (0-2)","Count (1-0)","Count (1-1)","Count (1-2)",
                                                                  "Count (2-0)","Count (2-1)","Count (2-2)","Count (3-0)","Count (3-1)","Count (3-2)",
                                                                  "1 out when up","2 outs when up","Number of Pitches", "Inning",
                                                                  "Right-handed Batter","Right-handed Pitcher",
                                                                  "runner on first base","runner on second base","runner on third base","Score difference",
                                                                  "Previous Pitch Breakingball","Previous Pitch Changeup","Previous Pitch Fastball",
                                                                  # "Previous Pitch Inside-High", "Previous Pitch Inside-Low", "Previous Pitch Outside-High", "Previous Pitch Outside-Low",
                                                                  "Previous Pitch Ball","Previous Pitch Hit","Previous Pitch Strike",
                                                                  "Closer","Middle Reliever","Setup","Right-handed Batter * Right-handed pitcher", "Constant"),
                          out="type_1.output.html")

#regression output of pitch location model in well-formatted regressin table
html_table.2 <- stargazer(fit.zone_1, type="html", coef=list(output.zone_1.exp), p.auto=FALSE,
                          single.row = TRUE, covariate.labels = c("Count (0-1)","Count (0-2)","Count (1-0)","Count (1-1)","Count (1-2)",
                                                                  "Count (2-0)","Count (2-1)","Count (2-2)","Count (3-0)","Count (3-1)","Count (3-2)",
                                                                  "1 out when up","2 outs when up","Number of Pitches", "Inning",
                                                                  "Right-handed Batter","Right-handed Pitcher",
                                                                  "runner on first base","runner on second base","runner on third base","Score difference",
                                                                  "Previous Pitch Breakingball","Previous Pitch Changeup","Previous Pitch Fastball",
                                                                  # "Previous Pitch Inside-High", "Previous Pitch Inside-Low", "Previous Pitch Outside-High", "Previous Pitch Outside-Low",
                                                                  "Previous Pitch Ball","Previous Pitch Hit","Previous Pitch Strike",
                                                                  "Closer","Middle Reliever","Setup","Right-handed Batter * Right-handed pitcher", "Constant"),
                          out="zone_1.output.htm")

#regression output of combined model in well-formatted regression table
html_table.3 <- stargazer(fit.pitch_1, type="html", coef=list(output.pitch_1.exp), p.auto=FALSE,
                          single.row = TRUE, covariate.labels = c("Count (0-1)","Count (0-2)","Count (1-0)","Count (1-1)","Count (1-2)",
                                                                  "Count (2-0)","Count (2-1)","Count (2-2)","Count (3-0)","Count (3-1)","Count (3-2)",
                                                                  "1 out when up","2 outs when up","Number of Pitches", "Inning",
                                                                  "Right-handed Batter","Right-handed Pitcher",
                                                                  "runner on first base","runner on second base","runner on third base","Score difference",
                                                                  "Previous Pitch Breakingball","Previous Pitch Changeup","Previous Pitch Fastball",
                                                                  # "Previous Pitch Inside-High", "Previous Pitch Inside-Low", "Previous Pitch Outside-High", "Previous Pitch Outside-Low",
                                                                  "Previous Pitch Ball","Previous Pitch Hit","Previous Pitch Strike",
                                                                  "Closer","Middle Reliever","Setup","Right-handed Batter * Right-handed pitcher", "Constant"),
                          out="pitch_1.output.htm")

size <- factor(unique(mlb$pitch_group))
#variable indicating whether the previous pitch type was a certain pitch type and satisfying the speed constraint
indicator_fastball_upper <- which(mlb$lag1.pitch == "Fastball" & mlb$release_speed >= 90)
indicator_breakingball_upper <- which(mlb$lag1.pitch == "Breakingball" & mlb$release_speed >= 82)
indicator_changeup_upper <- which(mlb$lag1.pitch == "Changeup" & mlb$release_speed >= 84)
indicator_fastball_lower <- which(mlb$lag1.pitch == "Fastball" & mlb$release_speed < 90)
indicator_breakingball_lower <- which(mlb$lag1.pitch == "Breakingball" & mlb$release_speed < 82)
indicator_changeup_lower <- which(mlb$lag1.pitch == "Changeup" & mlb$release_speed < 84)
indicator_none <- which(mlb$lag1.pitch == "None")

#sample mistake
mistake_fastball_upper <- sample(size, length(indicator_fastball_upper), prob = c(1,0,0), replace = TRUE)
mistake_breakingball_upper <- sample(size, length(indicator_breakingball_upper), prob = c(0.25,0.5,0.25), replace = TRUE)
mistake_changeup_upper  <- sample(size, length(indicator_changeup_upper), prob = c(0.25,0.25,0.50), replace = TRUE)
mistake_fastball_lower <- sample(size, length(indicator_fastball_lower), prob = c(0.5,0.25,0.25), replace = TRUE)
mistake_breakingball_lower <- sample(size, length(indicator_breakingball_lower), prob = c(0,0.5,0.5), replace = TRUE)
mistake_changeup_lower  <- sample(size, length(indicator_changeup_lower), prob = c(0,0.5,0.5), replace = TRUE)
no_mistake_none <- mlb$lag1.pitch[indicator_none]

#introduce noise (create new previuos pitch type variable with noise)
mlb$noise <- mlb$lag1.pitch
mlb$noise[indicator_fastball_upper]<- mistake_fastball_upper
mlb$noise[indicator_breakingball_upper]<- mistake_breakingball_upper
mlb$noise[indicator_changeup_upper]<- mistake_changeup_upper
mlb$noise[indicator_fastball_lower]<- mistake_fastball_lower
mlb$noise[indicator_breakingball_lower]<- mistake_breakingball_lower
mlb$noise[indicator_changeup_lower]<- mistake_changeup_lower

#fit.pitch_1 can easily be replaced by fit.zone_1 or fit.pitch_1. variable actual should change to the corresponding labels
actual <- mlb$pitch
prediction <- predict(fit.pitch_1, mlb, type = 'class')
mlb$lag1.pitch <- mlb$noise
prediction_noise <-  predict(fit.pitch_1, mlb, type = 'class')
total <- cbind.data.frame(actual, prediction, prediction_noise)
sum(total$prediction==total$prediction_noise)


pitcher <- mlb$pitcher
type_pitcher <- mlb$type_pitcher
prediction_pitcher <- cbind(pitcher,type_pitcher, actual, predict.pitch_1)

#get accuracy per pitcher
accuracy_pitcher <- prediction_pitcher %>%
  group_by(pitcher) %>% 
  mutate(correct = ifelse(actual==predict.pitch_1,1,0)) %>% 
  ungroup()

acc_pitcher <- accuracy_pitcher %>% 
  group_by(pitcher) %>% 
  summarise(accuracy = sum(correct)/n(), n = n()) 

acc_pitcher <- acc_pitcher %>% 
  filter(n >300)

#create histogrom of accuracy per pitcher
hist(acc_pitcher$accuracy, breaks = 50, main = "Combined Model: Histogram of the Accuracy per Pitcher", 
     xlab = 'Accuracy', ylab = 'Frequency', xlim = c(0.1,0.4),ylim = c(0,120))

#get accuracy in a given count
accuracy_count <- prediction_pitcher %>%
  group_by(mlb$count) %>% 
  mutate(correct = ifelse(actual==predict.pitch_1,1,0)) %>% 
  ungroup()

acc_count <- accuracy_count %>% 
  group_by(mlb$count) %>% 
  summarise(accuracy = sum(correct)/n(), n = n()) 

acc_count <- acc_count[order(-acc_count$accuracy),]

acc_count$`mlb$count` <- factor(acc_count$`mlb$count` , levels = acc_count$`mlb$count`[order(-acc_count$accuracy)])
plot <- ggplot(acc_count,aes(x = `mlb$count`, y = accuracy))+geom_bar(stat = "identity") #plot accuracy per count
plot + ggtitle("Combined Model: Accuracy per Count") + xlab("Count") + ylab("Accuracy") 

#Example Game-Situation
#Play for yourself to see the probabilities with certain situations.
example <- mlb[1,]
example$count <- factor("3-2")
example$outs_when_up <- factor(2)
example$pitch_count <- 12
example$inning <- 9
example$stand <- factor("R")
example$p_throws <- factor("L")
example$score_diff <- 0
example$pitch_count <- 18
example$on_1b <- 1
example$on_2b <- 1
example$on_3b <- 1
example$type_pitcher <- "closer"
example$lag1.pitch_result <- "Strike"
example$lag1.pitch <- factor('Fastball')
#Predict outcome in that situation.
example.class <- predict(fit.pitch_1,example, type = 'class')
example.prob <- predict(fit.pitch_1,example, type = 'prob')

predictions_type <- fit.type_1$fitted.values
predictions_type <- as.data.frame(predictions_type)
hist(predictions_type$Fastball, main = "Histogram of Predicted Accuries for Fastball", 
     xlab = "Predicted Probability Fastball",
     xlim=c(0.3,1),ylim = c(0,200000), breaks=200, )
hist(predictions_type$Breakingball,  main = "Histogram of Predicted Accuries for Breakingball", 
     xlab = "Predicted Probability Breakingball",
     xlim=c(0,0.7),ylim = c(0,240000), breaks = 200)
hist(predictions_type$Changeup,  main = "Histogram of Predicted Accuries for Changeup", 
     xlab = "Predicted Probability Change-up",
     xlim=c(0,0.4), ylim = c(0,200000), breaks = 200)

predictions_zone <- fit.zone_1$fitted.values
predictions_zone <- as.data.frame(predictions_zone)
hist(predictions_zone$OL, main = "Histogram of Predicted Accuries for Outside-Low", 
     xlab = "Predicted Probability Outside-Low",
     xlim=c(0.2,0.6),ylim = c(0,200000), breaks= 200)
hist(predictions_zone$IL,  main = "Histogram of Predicted Accuries for Inside-Low", 
     xlab = "Predicted Probability Inside-Low",
     xlim=c(0.1,0.4),ylim = c(0,200000), breaks = 200)
hist(predictions_zone$OH,  main = "Histogram of Predicted Accuries for Outside-High", 
     xlab = "Predicted Probability Outside-High",
     xlim=c(0.1,0.5), ylim = c(0,200000), breaks = 200)
hist(predictions_zone$IH,  main = "Histogram of Predicted Accuries for Inside-High", 
     xlab = "Predicted Probability Inside-High",
     xlim=c(0.1,0.3), ylim = c(0,200000), breaks = 200)

predictions_pitch <- fit.zone_1$fitted.values
predictions_pitch <- as.data.frame(predictions_pitch)
hist(predictions_pitch$OL, main = "Histogram of Predicted Accuries for Outside-Low", 
     xlab = "Predicted Probability Outside-Low",
     xlim=c(0.2,0.6),ylim = c(0,200000), breaks= 200)
hist(predictions_pitch$IL,  main = "Histogram of Predicted Accuries for Inside-Low", 
     xlab = "Predicted Probability Inside-Low",
     xlim=c(0.1,0.4),ylim = c(0,200000), breaks = 200)
hist(predictions_pitch$OH,  main = "Histogram of Predicted Accuries for Outside-High", 
     xlab = "Predicted Probability Outside-High",
     xlim=c(0.1,0.5), ylim = c(0,200000), breaks = 200)
hist(predictions_pitch$IH,  main = "Histogram of Predicted Accuries for Inside-High", 
     xlab = "Predicted Probability Inside-High",
     xlim=c(0.1,0.3), ylim = c(0,200000), breaks = 200)


