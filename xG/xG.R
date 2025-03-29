library(RMariaDB)
library(tidyverse)
library(caret)
library(randomForest)

#### Data Retrievel ####
readRenviron("data/.Renviron")
SQLpassword <- Sys.getenv("SQLpassword")
SQLpassword <- paste0(SQLpassword,'"')
host <- Sys.getenv("host")
port <- Sys.getenv("port")
user <- Sys.getenv("user")
con <- dbConnect(MariaDB(),
                 dbname = "Eksamen",
                 host = host,
                 port = port,
                 user = user,
                 password = SQLpassword)

dbListTables(con)
allshots_raw <- dbReadTable(con, "wyscout_matchevents_shots_sl") 
allevents_raw <- dbReadTable(con, "wyscout_matchevents_common_sl") 

#bind those badboys
allshotevents_raw <- allshots_raw %>%
  left_join(allevents_raw, by = "EVENT_WYID")


allshotevents_filtered <- allshotevents_raw %>%
  filter(SEASON_WYID == 188945)

# make the final df
allshotevents <- allshotevents_filtered

#feature engineering
# length
allshotevents$shot_distance <- sqrt((100 - allshotevents$POSSESSIONENDLOCATIONX)^2 + 
                                   (50 - allshotevents$POSSESSIONENDLOCATIONY)^2)

#vinkel
# define goal parameters
goal_width <- 11.43  # width of the goal in meters
goal_center_y <- 50  # center of the goal
goal_x <- 100        # goal line x-coordinate

# calculate the shot angle using the geometry of shooting method
allshotevents <- allshotevents %>%
  mutate(
    x = abs(goal_x - POSSESSIONENDLOCATIONX),  # distance to goal line
    y = abs(POSSESSIONENDLOCATIONY - goal_center_y),  # lateral distance from goal center
    
    # calculate the goal angle using the geometry method
    shot_angle = atan2(goal_width * x, 
                            x^2 + y^2 - (goal_width / 2)^2) * 180 / pi
  )

allshotevents$SHOTISGOAL <- as.factor(allshotevents$SHOTISGOAL)

# husk shots are ikke frispark, selvom osv


# variabler
  # kan det være muligt at beregne en spillers ELO?


# Forskellige modeller der kan bruges:
# Decision tree
    # Random forest
# glm



#### SET VIARIABLES HERE!! ####
x_variables <- c(
                 "shot_angle", 
                 "SHOTBODYPART", 
                 "shot_distance"
                )

variables <- as.formula(paste("SHOTISGOAL ~", paste(x_variables, collapse = " + ")))


#### SPLITTING DATA ####
set.seed(1980) # for reproducablility
cv_folds <- trainControl(method = "cv", number = 5)
train_index <- createDataPartition(y = allshotevents$SHOTISGOAL,
                                   # times = x
                                   p = 0.8,
                                   list = FALSE)# createDataPartition helps unbalanced datasets maintain a similar ratio of goals

train_data <- allshotevents[train_index,]
test_data <- allshotevents[-train_index,]

round(prop.table(table(train_data$SHOTISGOAL)),4)
round(prop.table(table(test_data$SHOTISGOAL)), 4)


#### beskrivende statistik ####




#### x-variables and influence on y ####



#### algos ####
##### GLM #####
glm <- glm(variables, data = train_data, family = "binomial")
summary(glm)

##### Random Forest #####
rf_model <- randomForest(variables, 
                         data = train_data,
                         ntree = 100,      
                         mtry = 2, 
                         importance = TRUE)
varImpPlot(rf_model)
# Forudsig sandsynligheder fra modellen
rf_test <- predict(rf_model, test_data, type = "prob")[, "1"]
rf_confusion <- confusionMatrix(as.factor(rf_preds))


###### Best Threshold ######
thresholds <- seq(0.1, 1.0, by = 0.001)

# Gem Accuracy for hver threshold
#
#
#
# HUSK AT SE NOGET OM, HVAD DER ER REALTISTISK FOR EN XG, SPARK FORAN MÅL HAR IKKE 100% OSV!!
#
#
#
#
#
#
threshold_results <- data.frame(Threshold = thresholds, Accuracy = NA)

for (i in seq_along(thresholds)) {
  t <- thresholds[i]
  
  # Konverter sandsynligheder til klasser
  pred_class <- ifelse(rf_test > t, "1", "0")
  
  # Beregn Accuracy
  acc_score <- mean(pred_class == test_data$SHOTISGOAL)
  
  # Gem resultater
  threshold_results$Accuracy[i] <- round(acc_score, 5)
  best_threshold <- threshold_results$Threshold[which.max(threshold_results$Accuracy)]
}

rf_preds <- ifelse(rf_test > best_threshold, "1", "0")

rf_confusion <- confusionMatrix(as.factor(rf_preds), as.factor(test_data$SHOTISGOAL))
rf_confusion





