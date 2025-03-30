library(RMariaDB)
library(tidyverse)
library(caret)
library(randomForest)
library(car)
library(pROC)
library(GGally)
library(Boruta)

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
allplayers_raw <- dbReadTable(con, "wyscout_players_sl") 
allteams_raw <- dbReadTable(con, "wyscout_teams_sl") 


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
# clean df
drop_cols <- c("COMPETITION_WYID.x", "MATCH_WYID.x", "EVENT_WYID",
               "PRIMARYTYPE.x", "SHOTONTARGET", "SHOTGOALZONE",
               "SHOTXG", "SHOTPOSTSHOTXG", "SHOTGOALKEEPERACTION_WYID", 
               "SHOTGOALKEEPER_WYID", "SEASON_WYID", "COMPETITION_WYID.y", 
               "MATCH_WYID.y", "MATCHTIMESTAMP", "VIDEOTIMESTAMP", 
               "RELATEDEVENT_WYID", "PRIMARYTYPE.y", "TEAM_WYID", 
               "OPPONENTTEAM_WYID", "PLAYER_WYID", "POSSESSION_WYID", 
               "POSSESSIONTEAM_WYID", "x", "y", "xG_RF","SECOND")

allshotevents_clean <- allshotevents[, !(names(allshotevents) %in% drop_cols)]

train_index_clean <- createDataPartition(y = allshotevents_clean$SHOTISGOAL,
                                   # times = x
                                   p = 0.8,
                                   list = FALSE)# createDataPartition helps unbalanced datasets maintain a similar ratio of goals

train_data_clean <- allshotevents_clean[train_index_clean,]
test_data_clean<- allshotevents_clean[-train_index_clean,]

#### SET VIARIABLES HERE!! ####
x_variables <- c(
  "shot_angle", 
  "SHOTBODYPART", 
  "shot_distance"
)

variables <- as.formula(paste("SHOTISGOAL ~", paste(x_variables, collapse = " + ")))

##### Simple boruta #####
boruta_result <- Boruta(SHOTISGOAL ~ ., data = train_data_clean, doTrace = 1)
plot(boruta_result, las = 2, cex.axis = 0.7)

final_vars <- getSelectedAttributes(boruta_result, withTentative = FALSE)
importance_df <- attStats(boruta_result)
boruta_df <- importance_df[order(-importance_df$meanImp), ]



#### x-variables and influence on y ####

#### Data Exploration ####
pair_data <- allshotevents %>%
  select(all_of(x_variables), SHOTISGOAL)
ggpairs(pair_data, aes(color = SHOTISGOAL, alpha = 0.6))


#### algos ####
##### GLM #####
# Univariative GLM Loop
# Initialiser data frame til resultater
glm_result <- data.frame(
  x_variable = character(),
  coefficient = numeric(),
  p_value = numeric(),
  p_stars = character()
)

# Funktion til at tildele signifikansstjerner
get_significance_stars <- function(p) {
  if (p < 0.001) {
    return("***")
  } else if (p < 0.01) {
    return("**")
  } else if (p < 0.05) {
    return("*")
  } else if (p < 0.1) {
    return(".")
  } else {
    return("")
  }
}

# Loop gennem alle forklarende variable
for (i in x_variables) {
  formula_glm <- as.formula(paste("SHOTISGOAL ~", i))
  glm_model <- glm(formula_glm, data = train_data, family = "binomial")
  
  # Udtræk koefficient og p-værdi for den pågældende variabel
  glm_coeff <- summary(glm_model)$coefficients[2,1]  # Koefficient
  glm_pval <- summary(glm_model)$coefficients[2,4]  # P-værdi
  glm_stars <- get_significance_stars(glm_pval)
  
  # Gem resultater i data frame
  tmp_glm <- data.frame(
    x_variable = i,
    coefficient = as.numeric(glm_coeff),
    p_value = as.numeric(glm_pval),
    p_stars = glm_stars
  )
  
  glm_result <- rbind(glm_result, tmp_glm)
}

glm_result$coefficient <- round(glm_result$coefficient,2)
glm_result$p_value <- round(glm_result$p_value,4)

# Full multivariate GLM
glm_train <- glm(variables, 
                 data = train_data, 
                 family = "binomial")
summary(glm_train)
# multicollinearity
vif(glm_train)


##### Singular Tree #####


##### Random Forest #####
rf_model <- randomForest(variables, 
                         data = train_data,
                         ntree = 10000,      
                         mtry = 2, 
                         importance = TRUE)
varImpPlot(rf_model)
# Forudsig sandsynligheder fra modellen
rf_test <- predict(rf_model, test_data, type = "prob")[, "1"]


###### Best Threshold F1 er bedre ######
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
}

###### F1 Score ######
thresholds <- seq(0.00, 1.0, by = 0.01)

f1_results <- data.frame(Threshold = thresholds, F1 = NA)

for (i in seq_along(thresholds)) {
  t <- thresholds[i]
  
  # binariser prediktionerne
  pred_class <- ifelse(rf_test > t, "1", "0")
  
  # confusion matrix
  cm <- confusionMatrix(as.factor(pred_class), as.factor(test_data$SHOTISGOAL), positive = "1")
  
  # gem F1
  f1_results$F1[i] <- cm$byClass["F1"]
}

# Find bedste threshold baseret på F1
best_f1 <- f1_results[which.max(f1_results$F1), ]
print(best_f1)


###### Make the predicts ######
best_threshold <- f1_results$Threshold[which.max(f1_results$F1)]
print(best_threshold)

rf_preds <- ifelse(rf_test > best_threshold, "1", "0")

# for test
rf_confusion <- confusionMatrix(as.factor(rf_preds), as.factor(test_data$SHOTISGOAL))
rf_confusion

# for total
allshotevents$xG_RF <- predict(rf_model, allshotevents, type = "prob")[, "1"]



xG_Comparison <- allshotevents %>%
  select(EVENT_WYID,LOCATIONX,LOCATIONY,SHOTISGOAL,SHOTXG,xG_RF) %>%
  as.data.frame()



