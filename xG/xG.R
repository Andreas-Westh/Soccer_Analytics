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
library(zoo)

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

# husk shots are ikke frispark, selvom osv


# variabler
  # kan det være muligt at beregne en spillers ELO?


# Forskellige modeller der kan bruges:
# Decision tree
    # Random forest
# glm


# scrapede team rankeringer 2022 / 2023 (sæsonen før)
team_rankings_2223 <- read.csv("xG/Scraped_Data/Team_Rankings_2223.csv")
team_rankings_2223$Hold <- gsub(" ?FC ?", "", team_rankings_2223$Hold)
team_rankings_2223$Hold <- gsub(" ?FF ?", "", team_rankings_2223$Hold)
team_rankings_2223$Hold <- gsub(" ?IF ?", "", team_rankings_2223$Hold)

team_rankings_2223 <- team_rankings_2223 %>%
  rename(TEAMNAME = Hold)

# team i 2023 / 2024
all_teams_2324 <- allteams_raw %>% filter(SEASON_WYID == "188945")

# merge, and giving the new team 12th place
all_teams_2324 <- all_teams_2324 %>% 
  left_join(team_rankings_2223, by = "TEAMNAME") %>% 
  mutate(Team_Ranking = if_else(is.na(Placering), 12, Placering))

all_teams_2324 <- all_teams_2324 %>% 
  select(TEAMNAME, Team_Ranking, everything())

allshotevents <- allshotevents %>%
  left_join(all_teams_2324 %>% select(TEAM_WYID, TEAMNAME, Team_Ranking),
            by = "TEAM_WYID")

allshotevents <- allshotevents %>%
  left_join(matched_df %>% select(PLAYER_WYID, overall, potential, SHORTNAME),
            by = "PLAYER_WYID")

allshotevents <- allshotevents %>%
  distinct(EVENT_WYID, .keep_all = TRUE)

# recent preformance / hothand
#no fucking clue



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


# Korrelation





##### Simple boruta #####
boruta_result <- Boruta(SHOTISGOAL ~ ., data = train_data, doTrace = 1,)
plot(boruta_result, las = 2, cex.axis = 0.7)

final_vars <- getSelectedAttributes(boruta_result, withTentative = FALSE)
importance_df <- attStats(boruta_result)
boruta_df <- importance_df[order(-importance_df$meanImp), ]
max_shadow <- max(importance_df[grepl("shadow", rownames(importance_df)), "meanImp"])

# Filter through shadows and 100th procentile
#### SET VIARIABLES HERE!! ####
# fjerne nogle af de mange
#x_variables <- c(
#  "shot_angle", 
#  "shot_distance", 
#  "POSSESSIONDURATION",
#  "SHOTBODYPART",
#  "POSSESSIONENDLOCATIONX",
#  "POSSESSIONENDLOCATIONY",
#  "POSSESSIONEVENTSNUMBER",
#  "POSSESSIONEVENTINDEX"
#)

#one-hot encoding
allshotevents$SHOTBODYPART <- as.factor(allshotevents$SHOTBODYPART)
onehots <- dummyVars(~ SHOTBODYPART, data = allshotevents)
dummy_data <- predict(onehots, newdata = allshotevents)
allshotevents <- cbind(allshotevents, dummy_data)


x_variables <- c(
  "shot_angle", 
  "shot_distance", 
  "SHOTBODYPART",
  "Team_Ranking"
)

x_variables <- c(
  "shot_angle", 
  "shot_distance", 
  "SHOTBODYPART.head_or_other",
  "SHOTBODYPART.left_foot",
  "SHOTBODYPART.right_foot",
  "Team_Ranking",
  "overall",
  "potential",
  "LOCATIONX",
  "LOCATIONY"
)

x_variables <- c(
  "shot_angle", 
  "shot_distance", 
  "SHOTBODYPART.head_or_other",
  "SHOTBODYPART.left_foot",
  "SHOTBODYPART.right_foot",
  "LOCATIONX",
  "POSSESSIONDURATION",
  "POSSESSIONEVENTSNUMBER",
  "POSSESSIONEVENTINDEX",
  "LOCATIONY"
)

#XGB AUV: 0.8
x_variables <- c(
  "shot_angle", 
  "shot_distance", 
  "SHOTBODYPART.head_or_other",
  "SHOTBODYPART.left_foot",
  "SHOTBODYPART.right_foot",
  "LOCATIONX",
  "LOCATIONY"
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

train_data_clean <- train_data_clean %>% 
  select(SHOTISGOAL, everything())
test_data_clean <- test_data_clean %>% 
  select(SHOTISGOAL, everything())

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
  glm_model <- glm(formula_glm, data = train_data_clean, family = "binomial")
  
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
view(glm_result)

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
colSums(is.na(train_data_yn))

# tuning
ctrl <- trainControl(method = "cv", 
                     number = 5, 
                     classProbs = TRUE,                    
                     summaryFunction = twoClassSummary, 
                     verboseIter = TRUE)  # <- viser progress i konsollen)
tuned_rf <- train(variables, 
                  data = train_data_yn, 
                  method = "rf", 
                  metric = "ROC",
                  trControl = ctrl, 
                  tuneGrid = expand.grid(mtry = 1:length(x_variables)))

best_mtry <- tuned_rf$bestTune[1,1]
print(best_mtry)

# tree depth
trees <- c(10, 100, 500, 1000, 2000, 5000, 10000)
auc_df <- data.frame(ntree = trees, AUC = NA)

for (i in seq_along(trees)) {
  set.seed(1980)
  cat("Training Random Forest with", trees[i], "trees...\n")
  
  rf_model <- train(variables, 
                    data = train_data_yn, 
                    ntree = trees[i], 
                    method = "rf", 
                    metric = "ROC",
                    trControl = ctrl,
                    tuneGrid = data.frame(mtry = best_mtry))
  
  probs <- predict(rf_model, test_data_yn, type = "prob")[, "yes"]
  
  roc_obj <- roc(test_data_yn$SHOTISGOAL, probs, quiet = TRUE)
  auc_df$AUC[i] <- auc(roc_obj)
  
  cat("Done. AUC:", round(auc_df$AUC[i], 4), "\n\n")
}
best_trees <- auc_df$ntree[which.max(auc_df$AUC)]
print(best_trees)
# print plot
rf_ntree_loop


# rf
rf_model_final <- train(
  variables,
  data = train_data_yn,
  method = "rf",
  metric = "ROC",
  trControl = trainControl(
    method = "none",               # <- no CV for final fit
    classProbs = TRUE,
    summaryFunction = twoClassSummary
  ),
  tuneGrid = data.frame(mtry = best_mtry),
  ntree = best_trees,
  sampsize = c("no" = 300, "yes" = 300),
  importance = TRUE
)

varImpPlot(rf_model)
# Forudsig sandsynligheder fra modellen
rf_test <- predict(rf_model_final, test_data_yn, type = "prob")[, "yes"]
rf_auc <- auc(test_data_yn$SHOTISGOAL, rf_test)
cat("AUC for Random Forest:", round(rf_auc, 4), "\n")

##### XGBoost #####
train_data_yn <- train_data_clean
test_data_yn <- test_data_clean

train_data_yn$SHOTISGOAL <- factor(train_data_yn$SHOTISGOAL,
                                      levels = c(0, 1),
                                      labels = c("no", "yes"))
test_data_yn$SHOTISGOAL <- factor(test_data_yn$SHOTISGOAL,
                                     levels = c(0, 1),
                                     labels = c("no", "yes"))
# https://youtu.be/qjeUhuvkbHY?si=-bYul2KuvoOnusPo&t=763
grid_tune <- expand.grid(
  nrounds = c(500, 1000, 1500),                # Flere iterationer
  max_depth = c(2, 4, 6),                      # Dybere træer
  eta = c(0.005, 0.01),                  # Flere læringsrater
  gamma = c(0.25, 0.5, 0.75),            # Fra ingen pruning til aggressiv pruning
  colsample_bytree = c(0.3, 0.5, 0.7, 0.8),    # Flere trætræks-sampler
  min_child_weight = c(2, 3),               # Mindre = mere kompleks
  subsample = c(0.5, 0.75, 1.0)                # Hvor mange samples pr. træ
)


train_control <- trainControl(method = "cv",
                              number=5,
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary,
                              verboseIter = TRUE,
                              allowParallel = TRUE)
xgb_tune <- train(set.seed(1980),
                 x = train_data_yn[,-1],
                  y = train_data_yn[,1],
                  trControl = train_control,
                  tuneGrid = grid_tune,
                  method = "xgbTree",
                  metric = "ROC",
                  verbose = TRUE)

xgb_tune$bestTune

train_control <- trainControl(method = "cv",
                              number = 3,
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary,
                              verboseIter = TRUE,
                              allowParallel = TRUE)
final_grid <- expand.grid(
                          nrounds = xgb_tune$bestTune$nrounds,
                          #nrounds = 2500,
                          eta = xgb_tune$bestTune$eta,
                          max_depth = xgb_tune$bestTune$max_depth,
                          #max_depth = 3,
                          gamma = xgb_tune$bestTune$gamma,
                          colsample_bytree = xgb_tune$bestTune$colsample_bytree,
                          min_child_weight = xgb_tune$bestTune$min_child_weight,
                          subsample = xgb_tune$bestTune$subsample)
xgb_model <- train(set.seed(1980),
                   x = train_data_yn[,-1],
                   y = train_data_yn[,1],
                   trControl = train_control,
                   tuneGrid = final_grid,
                   method = "xgbTree",
                   metric = "ROC",
                   verbose = TRUE
                   )
summary(predict(xgb_model, test_data_yn, type = "prob")[, "yes"])

xgb_pred <- predict(xgb_model, test_data_yn, type = "prob")[, "yes"]
xgb_auc <- auc(test_data_yn$SHOTISGOAL, xgb_pred)
cat("Endelig AUC for tuned XGBoost:", round(xgb_auc, 4), "\n")



##### WyScout #####
roc_wyscout <- roc(test_data$SHOTISGOAL, test_data$SHOTXG)
auc_wyscout <- auc(roc_wyscout)
print(auc_wyscout)



###### Make the predicts ######
best_threshold <- 0.35

predictors <- allshotevents[, colnames(train_data_yn[,-1])]
xg_values <- predict(xgb_model, newdata = predictors, type = "prob")[, "yes"]
allshotevents$xG_XGB <- xg_values

# for test
rf_confusion <- confusionMatrix(xgb_class, actual)
rf_confusion


#### Evaluating ####
# test nye sæson

xG_Comparison <- allshotevents %>%
  select(EVENT_WYID,LOCATIONX,LOCATIONY,SHOTISGOAL,SHOTXG,xG_XGB) %>%
  as.data.frame()




#### Test on new season ####
allshotevents_filtered_2425 <- allshotevents_raw %>%
  filter(SEASON_WYID != 188945)

# make the final df
allshotevents_2425 <- allshotevents_filtered_2425

#feature engineering
# length
allshotevents_2425$shot_distance <- sqrt((100 - allshotevents_2425$LOCATIONX)^2 + 
                                      (50 - allshotevents_2425$LOCATIONY)^2)

#vinkel
# define goal parameters
goal_width <- 11.43  # width of the goal
goal_center_y <- 50  # center of the goal
goal_x <- 100        # goal line x-coordinate

# calculate the shot angle using the geometry of shooting method
allshotevents_2425 <- allshotevents_2425 %>%
  mutate(
    x = abs(goal_x - LOCATIONX),  # distance to goal line
    y = abs(LOCATIONY - goal_center_y),  # lateral distance from goal center
    
    # calculate the goal angle using the geometry method
    shot_angle = atan2(goal_width * x, 
                       x^2 + y^2 - (goal_width / 2)^2) * 180 / pi
  )

allshotevents_2425$SHOTISGOAL <- as.factor(allshotevents_2425$SHOTISGOAL)

allshotevents_2425$SHOTBODYPART <- as.factor(allshotevents_2425$SHOTBODYPART)
onehots <- dummyVars(~ SHOTBODYPART, data = allshotevents_2425)
dummy_data <- predict(onehots, newdata = allshotevents_2425)
allshotevents_2425 <- cbind(allshotevents_2425, dummy_data)


selected_cols <- c(x_variables, "SHOTISGOAL")
allshotevents_clean_2425 <- allshotevents_2425[, selected_cols]

test_2425 <- allshotevents_clean_2425 %>% 
  select(SHOTISGOAL, everything())

round(prop.table(table(test_2425$SHOTISGOAL)),4)


xgb_pred_2425 <- predict(xgb_model, test_2425, type = "prob")[, "yes"]
xgb_auc_2425 <- auc(test_2425$SHOTISGOAL, xgb_pred_2425)
cat("Endelig AUC for tuned XGBoost 2024 / 2025:", round(xgb_auc_2425, 4), "\n")


roc_wyscout_2425 <- roc(allshotevents_2425$SHOTISGOAL, allshotevents_2425$SHOTXG)
auc_wyscout_2425 <- auc(roc_wyscout_2425)
print(auc_wyscout_2425)



#### Extras and other things probs not needed ####
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

###### Old XGBoost ######
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


Advanced 
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

xgb_pred <- predict(xgb_model_final, x_test, type = "prob")
xgb_auc <- auc(y_test, xgb_pred)
cat("Endelig AUC for tuned XGBoost:", round(xgb_auc, 4), "\n")