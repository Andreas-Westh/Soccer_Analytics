library(RMariaDB)
library(tidyverse)
library(gganimate)


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


allshotevents <- left_join(allshotevents, allteams_raw_unique[, c("TEAM_WYID", "TEAMNAME")], by = "TEAM_WYID")

all_brøndby_matches <- allshotevents %>% filter(TEAMNAME == "Brøndby") %>% 
  distinct(MATCH_WYID.x, .keep_all = TRUE) %>% select(MATCH_WYID.x)


shots_per_match <- allshotsbrøndby %>%
  group_by(MATCH_WYID.x) %>%
  summarise(
    shot_count = n(),
    goals = sum(SHOTISGOAL == 1)
  )
mean(shots_per_match$shot_count)

allshotmatches_brøndby <-  allshotevents %>%
  select(MATCH_WYID.x, SHOTISGOAL, SHOTXG,EVENT_WYID, TEAMNAME) %>% 
  filter(MATCH_WYID.x %in% all_brøndby_matches$MATCH_WYID.x)

single_match <- allshotmatches_brøndby %>% 
  filter(MATCH_WYID.x == "5466028")


#### Loop ####
xp_results <- data.frame()

brøndby_matches <- all_brøndby_matches$MATCH_WYID.x

for (match_id in brøndby_matches) {
  print(match_id)
  
  match_data <- allshotmatches_brøndby %>% 
    filter(MATCH_WYID.x == match_id)
  
  #summere xP per kamp per hold
  match_xg <- match_data %>% 
    group_by(TEAMNAME) %>%
    summarise(xG = sum(SHOTXG)) %>%
    arrange(desc(TEAMNAME == "Brøndby"))
  
  # lav vores matrix
  score_matrix <- expand.grid(
    team_goals = 0:5,
    opp_goals = 0:5
  )
  
  #sandsynligheder for Brøndby
  probs <- dpois(score_matrix$team_goals, match_xg$xG[1]) * #dpois laver selv poisson-fordeling
    dpois(score_matrix$opp_goals, match_xg$xG[2])
  
  p_win  <- sum(probs[score_matrix$team_goals > score_matrix$opp_goals])
  p_draw <- sum(probs[score_matrix$team_goals == score_matrix$opp_goals])
  p_loss <- sum(probs[score_matrix$team_goals < score_matrix$opp_goals])
  
  #xP formel
  xP_brøndby <- 3 * p_win + 1 * p_draw + 0 * p_loss
  
  # sandsynligheder for modstanderen
  probs_opp <- dpois(score_matrix$opp_goals, match_xg$xG[2]) *
    dpois(score_matrix$team_goals, match_xg$xG[1])
  
  p_win_opp  <- sum(probs_opp[score_matrix$opp_goals > score_matrix$team_goals])
  p_draw_opp <- sum(probs_opp[score_matrix$opp_goals == score_matrix$team_goals])
  p_loss_opp <- sum(probs_opp[score_matrix$opp_goals < score_matrix$team_goals])
  
  #xP formel (igen :3)
  xP_opponent <- 3 * p_win_opp + 1 * p_draw_opp + 0 * p_loss_opp
  
  # Smæk det ind i en df
  xp_results <- rbind(xp_results, data.frame(
    Brøndby = "Brøndby",
    Brøndby_xP = round(xP_brøndby, 3),
    Modstander = match_xg$TEAMNAME[2],
    Modstander_xP = round(xP_opponent, 3),
    MATCH_WYID = match_id
  ))
}

xp_sum <- xp_results %>% 
  summarise(total_points = sum(Brøndby_xP))




#### For all Teams ####
xp_results <- data.frame()
all_teams <- unique(allshotevents$TEAMNAME)

for (team in all_teams) {
  team_matches <- allshotevents %>%
    filter(TEAMNAME == team) %>%
    distinct(MATCH_WYID.x, .keep_all = TRUE) %>%
    pull(MATCH_WYID.x)
  
  for (match_id in team_matches) {
    print(paste("Hold:", team, "| Match:", match_id))
    
    match_data <- allshotevents %>%
      filter(MATCH_WYID.x == match_id)
    
    match_xg <- match_data %>%
      group_by(TEAMNAME) %>%
      summarise(xG = sum(SHOTXG, na.rm = TRUE)) %>%
      arrange(desc(TEAMNAME == team))
    
    score_matrix <- expand.grid(
      team_goals = 0:5,
      opp_goals = 0:5
    )
    
    probs <- dpois(score_matrix$team_goals, match_xg$xG[1]) * #dpois laver selv poisson-fordeling
      dpois(score_matrix$opp_goals, match_xg$xG[2])
    
    p_win  <- sum(probs[score_matrix$team_goals > score_matrix$opp_goals])
    p_draw <- sum(probs[score_matrix$team_goals == score_matrix$opp_goals])
    p_loss <- sum(probs[score_matrix$team_goals < score_matrix$opp_goals])
    
    #xP formel
    xP_team <- 3 * p_win + 1 * p_draw + 0 * p_loss
    
    # sandsynligheder for modstanderen
    probs_opp <- dpois(score_matrix$opp_goals, match_xg$xG[2]) *
      dpois(score_matrix$team_goals, match_xg$xG[1])
    
    p_win_opp  <- sum(probs_opp[score_matrix$opp_goals > score_matrix$team_goals])
    p_draw_opp <- sum(probs_opp[score_matrix$opp_goals == score_matrix$team_goals])
    p_loss_opp <- sum(probs_opp[score_matrix$opp_goals < score_matrix$team_goals])
    
    #xP formel (igen :3)
    xP_opp <- 3 * p_win_opp + 1 * p_draw_opp + 0 * p_loss_opp
    
    xp_results <- rbind(xp_results, data.frame(
      Team = match_xg$TEAMNAME[1],
      Team_xP = round(xP_team, 3),
      Opponent = match_xg$TEAMNAME[2],
      Opponent_xP = round(xP_opp, 3),
      MATCH_WYID = match_id
    ))
  }
}

xp_sum <- xp_results %>%
  group_by(Team) %>%
  summarise(Total_xP = round(sum(Team_xP, na.rm = TRUE), 2)) %>%
  arrange(desc(Total_xP))

# kamp nr
allshotevents_kampe <- allshotevents %>%
  distinct(TEAMNAME, MATCH_WYID.x) %>%  # én række per hold per kamp
  group_by(TEAMNAME) %>%
  mutate(kamp_nummer = row_number())

allshotevents <- allshotevents %>%
  left_join(allshotevents_kampe,
            by = c("TEAMNAME", "MATCH_WYID.x"))


kamp_rækkefølge <- allshotevents %>%
  distinct(TEAMNAME, MATCH_WYID.x) %>%
  group_by(TEAMNAME) %>%
  mutate(kamp_nummer = row_number()) %>%
  rename(Team = TEAMNAME, MATCH_WYID = MATCH_WYID.x)

xp_results_matches <- xp_results %>%
  left_join(kamp_rækkefølge, by = c("Team", "MATCH_WYID"))

xp_tidsserie <- xp_results_matches %>%
  arrange(Team, kamp_nummer) %>%
  group_by(Team) %>%
  mutate(cumulative_xP = cumsum(Team_xP))

maks_kampe <- max(xp_tidsserie$kamp_nummer) 

for (kamp in 1:length(maks_kampe)) { 
Sys.sleep(0.5)
  p <- ggplot(xp_tidsserie, aes(x = kamp, y = cumulative_xP, color = Team, group = Team)) +
  geom_line(size = 1) +
  labs(title = "Akkumuleret xP per kamp i sæsonen",
       x = "Kampnummer",
       y = "Akkumuleret Expected Points") +
  theme_minimal() +
  
print(p)
}

# check avg xg
allshotevents %>% 
  group_by(MATCH_WYID.x) %>%
  summarise(total_xG = sum(SHOTXG)) %>%
  summarise(avg_xG = mean(total_xG))

# matches per team
match_count <- allshotevents %>%
  distinct(TEAMNAME, MATCH_WYID.x) %>%  # sikrer at samme kamp ikke tælles flere gange pr hold
  count(TEAMNAME, name = "Antal_kampe") %>%
  arrange(desc(Antal_kampe))

print(match_count)





hvidovre_xg <- allshotevents %>%
  filter(TEAMNAME == "Hvidovre") %>%
  summarise(total_xG = sum(SHOTXG),
            total_goals = sum(as.numeric(as.character(SHOTISGOAL))))

hvidovre_xg



#### test ####
match_xg <- single_match %>%
  group_by(TEAMNAME) %>%
  summarise(xG = sum(SHOTXG)) %>%
  arrange(desc(xG))


#Poisson-model: grid over mulige mål 0-5
score_matrix <- expand.grid(
  team_goals = 0:5,
  opp_goals = 0:5
)


# Beregn xP for hold 1
#dpois laver selv poisson-fordeling
xp1 <- sum( # summerer expected points over alle mulige scorekombinationer
  dpois(score_matrix$team_goals, match_xg$xG[1]) * # sandsynlighed for at holdet scorer x mål
    dpois(score_matrix$opp_goals, match_xg$xG[2]) * # sandsynlighed for at modstanderen scorer y mål
    ifelse(score_matrix$team_goals > score_matrix$opp_goals, 3, # 3 point hvis sejr
           ifelse(score_matrix$team_goals == score_matrix$opp_goals, 1, 0)) # 1 ved uafgjort, ellers 0
)

# Og hold 2
xp2 <- sum(
  dpois(score_matrix$team_goals, match_xg$xG[2]) *
    dpois(score_matrix$opp_goals, match_xg$xG[1]) *
    ifelse(score_matrix$team_goals > score_matrix$opp_goals, 3,
           ifelse(score_matrix$team_goals == score_matrix$opp_goals, 1, 0))
)

# Resultat
expected_points <- match_xg %>%
  mutate(xP = round(c(xp1, xp2), 3))

