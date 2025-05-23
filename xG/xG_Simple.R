library(RMariaDB)
library(tidyverse)
library(caret)
library(randomForest)
library(car)
library(pROC)
library(GGally)
library(Boruta)
library(xgboost)
library(rpart)
library(rpart.plot)

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
allshotevents$shot_distance <- sqrt((100 - allshotevents$LOCATIONX)^2 + 
                                      (50 - allshotevents$LOCATIONY)^2)

#vinkel
# define goal parameters
goal_width <- 11.43  # width of the goal
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
x_variables <- c(
  "shot_angle", 
  "shot_distance", 
  "SHOTBODYPART"
)


variables <- as.formula(paste("SHOTISGOAL ~", paste(x_variables, collapse = " + ")))

# clean df
selected_cols <- c(x_variables, "SHOTISGOAL")
allshotevents_clean <- allshotevents[, selected_cols]

train_index_clean <- createDataPartition(
  y = allshotevents_clean$SHOTISGOAL,
  p = 0.8,
  list = FALSE
)

train_data_clean <- allshotevents_clean[train_index_clean, ]
test_data_clean  <- allshotevents_clean[-train_index_clean, ]

round(prop.table(table(train_data_clean$SHOTISGOAL)),4)
round(prop.table(table(test_data_clean$SHOTISGOAL)), 4)

#### x-variables and influence on y ####
#### Data Exploration ####
pair_data <- allshotevents %>%
  select(all_of(x_variables), SHOTISGOAL)
ggpairs(pair_data, aes(color = SHOTISGOAL, alpha = 0.6))

ggpairs(
  pair_data,
  aes(color = SHOTISGOAL, alpha = 0.5),
  lower = list(continuous = wrap("points", alpha = 0.4, size = 0.6)),
  upper = list(continuous = wrap("points", alpha = 0.4, size = 0.6)), # <- scatter in both
  diag = list(continuous = wrap("densityDiag", alpha = 0.6))
) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = c("red", "cyan"))


#### algos ####
##### GLM #####
# Full multivariate GLM
glm_train <- glm(variables, 
                 data = train_data_clean, 
                 family = "binomial")
summary(glm_train)
# multicollinearity
vif(glm_train)

glm_probs <- predict(glm_train, newdata = test_data_clean, type = "response")

glm_auc <- auc(test_data_clean$SHOTISGOAL, glm_probs)
cat("AUC for GLM:", round(glm_auc, 4), "\n")

##### Singular Tree #####
simple_tree <- rpart(variables,
                     data = train_data_clean,
                     method = "class",
                     control = rpart.control(cp = 0.001)
)  
tree_probs <- predict(simple_tree, newdata = test_data_clean, type = "prob")[, "1"]
tree_auc <- auc(test_data_clean$SHOTISGOAL, tree_probs)
cat("AUC for enkelt beslutningstræ:", round(tree_auc, 4), "\n")


##### Random Forest #####
# rf
rf_model_final <- randomForest(
  formula = variables,
  data = train_data_clean,
  ntree = 5000,
  mtry = floor(sqrt(length(x_variables))),
  #mtry = length(x_variables),
  #classwt = c("0" = 1, "1" = 8),  # cirka vægtet omvendt af fordelingen
  sampsize = c("0" = 300, "1" = 300),  # equal number from each class pr træ
  importance = TRUE
)

varImpPlot(rf_model)
# Forudsig sandsynligheder fra modellen
rf_test <- predict(rf_model_final, test_data_clean, type = "prob")[, "1"]
rf_auc <- auc(test_data_clean$SHOTISGOAL, rf_test)
cat("AUC for Random Forest (sampsize):", round(rf_auc, 4), "\n")

##### XGBoost #####
# xgboost vil have det i matrix, og y som numeric
x_train <- model.matrix(variables, data = train_data_clean)[, -1]
y_train <- as.numeric(as.character(train_data_clean$SHOTISGOAL))

x_test <- model.matrix(variables, data = test_data_clean)[, -1]
y_test <- as.numeric(as.character(test_data_clean$SHOTISGOAL))

# Lav DMatrix
dtrain <- xgb.DMatrix(data = x_train, label = y_train)
dtest <- xgb.DMatrix(data = x_test, label = y_test)

# Træn en helt simpel model
xgb_model <- xgboost(data = dtrain,
                     objective = "binary:logistic",
                     eval_metric = "auc",
                     nrounds = 100,
                     verbose = 1)

# Predict og beregn AUC
xgb_pred <- predict(xgb_model, x_test)
xgb_auc <- auc(y_test, xgb_pred)

cat("AUC for XGBoost:", round(xgb_auc, 4), "\n")


###### Advanced ######
xgb_results <- data.frame(
  max_depth = numeric(),
  subsample = numeric(),
  colsample_bytree = numeric(),
  best_iter = numeric(),
  best_auc = numeric()
)

# Loops over hyperparametre
for (depth in c(3, 5, 7)) {
  for (sub in c(0.5, 0.7, 1.0)) {
    for (col in c(0.5, 0.7, 1.0)) {
      
      cat("Træner med max_depth =", depth, ", subsample =", sub, ", colsample_bytree =", col, "\n")
      
      xgb_cv <- xgb.cv(
        set.seed(1980),
        data = dtrain,
        objective = "binary:logistic",
        eval_metric = "auc",
        nrounds = 1000,
        nfold = 5,
        early_stopping_rounds = 25,
        verbose = 0,
        params = list(
          eta = 0.005,
          max_depth = depth,
          subsample = sub,
          colsample_bytree = col
        )
      )
      
      best_iter <- which.max(xgb_cv$evaluation_log$test_auc_mean)
      best_auc <- max(xgb_cv$evaluation_log$test_auc_mean)
      
      xgb_results <- rbind(xgb_results, data.frame(
        max_depth = depth,
        subsample = sub,
        colsample_bytree = col,
        best_iter = best_iter,
        best_auc = round(best_auc, 4)
      ))
      
      cat("Færdig – Bedste AUC:", round(best_auc, 4), "efter", best_iter, "runder\n\n")
    }
  }
}

best_row <- xgb_results[which.max(xgb_results$best_auc), ]
print(best_row)

xgb_model_final <- xgboost(
  set.seed(1980),
  data = dtrain,
  objective = "binary:logistic",
  eval_metric = "auc",
  nrounds = best_row$best_iter,
  eta = 0.005,  # samme learning rate som i tuning
  max_depth = best_row$max_depth,
  subsample = best_row$subsample,
  colsample_bytree = best_row$colsample_bytree,
  verbose = 1
)

xgb_pred <- predict(xgb_model_final, x_test)
xgb_auc <- auc(y_test, xgb_pred)
cat("Endelig AUC for tuned XGBoost:", round(xgb_auc, 4), "\n")

##### WyScout #####
roc_wyscout <- roc(test_data$SHOTISGOAL, test_data$SHOTXG)
auc_wyscout <- auc(roc_wyscout)
print(auc_wyscout)


###### Best Threshold F1 måske ikke relevant? ######
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


#### Evaluating ####
xG_Comparison <- allshotevents %>%
  select(EVENT_WYID,LOCATIONX,LOCATIONY,SHOTISGOAL,SHOTXG,xG_RF) %>%
  as.data.frame()



