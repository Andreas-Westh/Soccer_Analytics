library(RMariaDB)
library(tidyverse)



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

allteams_raw_unique <- allteams_raw %>%
  distinct(TEAM_WYID, .keep_all = TRUE)


allshotsbrøndby <- left_join(allshotevents, allteams_raw_unique[, c("TEAM_WYID", "TEAMNAME")], by = "TEAM_WYID") %>% 
  filter(TEAMNAME == "Brøndby")

#### Test with a single match ####
shots_per_match <- allshotsbrøndby %>%
  group_by(MATCH_WYID.x) %>%
  summarise(
    shot_count = n(),
    goals = sum(SHOTISGOAL == 1)
  )
mean(shots_per_match$shot_count)

single_match <-  allshotsbrøndby %>%
  select(MATCH_WYID.x, SHOTISGOAL, SHOTXG,EVENT_WYID) %>% 
  filter(MATCH_WYID.x == "5466028")


