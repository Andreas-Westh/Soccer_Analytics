library(RMariaDB)
library(tidyverse)
library(gganimate)
library(ggimage)


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
      summarise(xG = sum(xG_XGB, na.rm = TRUE)) %>%
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

#### For all Teams - WyScout version ####
xp_results_wyscout <- data.frame()
all_teams <- unique(allshotevents$TEAMNAME.x)

for (team in all_teams) {
  team_matches <- allshotevents %>%
    filter(TEAMNAME.x == team) %>%
    distinct(MATCH_WYID.x, .keep_all = TRUE) %>%
    pull(MATCH_WYID.x)
  
  for (match_id in team_matches) {
    print(paste("Hold:", team, "| Match:", match_id))
    
    match_data_wyscout <- allshotevents %>%
      filter(MATCH_WYID.x == match_id)
    
    match_xg_wyscout <- match_data_wyscout %>%
      group_by(TEAMNAME.x) %>%
      summarise(xG_wyscout = sum(SHOTXG, na.rm = TRUE)) %>%
      arrange(desc(TEAMNAME.x == team))
    
    score_matrix_wyscout <- expand.grid(
      team_goals = 0:5,
      opp_goals = 0:5
    )
    
    probs_wyscout <- dpois(score_matrix_wyscout$team_goals, match_xg_wyscout$xG_wyscout[1]) *
      dpois(score_matrix_wyscout$opp_goals, match_xg_wyscout$xG_wyscout[2])
    
    p_win_wyscout  <- sum(probs_wyscout[score_matrix_wyscout$team_goals > score_matrix_wyscout$opp_goals])
    p_draw_wyscout <- sum(probs_wyscout[score_matrix_wyscout$team_goals == score_matrix_wyscout$opp_goals])
    p_loss_wyscout <- sum(probs_wyscout[score_matrix_wyscout$team_goals < score_matrix_wyscout$opp_goals])
    
    xP_team_wyscout <- 3 * p_win_wyscout + 1 * p_draw_wyscout
    
    probs_opp_wyscout <- dpois(score_matrix_wyscout$opp_goals, match_xg_wyscout$xG_wyscout[2]) *
      dpois(score_matrix_wyscout$team_goals, match_xg_wyscout$xG_wyscout[1])
    
    p_win_opp_wyscout  <- sum(probs_opp_wyscout[score_matrix_wyscout$opp_goals > score_matrix_wyscout$team_goals])
    p_draw_opp_wyscout <- sum(probs_opp_wyscout[score_matrix_wyscout$opp_goals == score_matrix_wyscout$team_goals])
    p_loss_opp_wyscout <- sum(probs_opp_wyscout[score_matrix_wyscout$opp_goals < score_matrix_wyscout$team_goals])
    
    xP_opp_wyscout <- 3 * p_win_opp_wyscout + 1 * p_draw_opp_wyscout
    
    xp_results_wyscout <- rbind(xp_results_wyscout, data.frame(
      Team = match_xg_wyscout$TEAMNAME.x[1],
      Team_xP_wyscout = round(xP_team_wyscout, 3),
      Opponent = match_xg_wyscout$TEAMNAME.x[2],
      Opponent_xP_wyscout = round(xP_opp_wyscout, 3),
      MATCH_WYID = match_id
    ))
  }
}

xp_sum_wyscout <- xp_results_wyscout %>%
  group_by(Team) %>%
  summarise(Total_xP_wyscout = round(sum(Team_xP_wyscout, na.rm = TRUE), 2)) %>%
  arrange(desc(Total_xP_wyscout))





# get actual points
team_rankings_2324 <- read_csv("xG/Scraped_Data/Team_Rankings_2324.csv")
team_rankings_2324$Hold <- gsub(" ?FC ?", "", team_rankings_2324$Hold)
team_rankings_2324$Hold <- gsub(" ?FF ?", "", team_rankings_2324$Hold)
team_rankings_2324$Hold <- gsub(" ?IF ?", "", team_rankings_2324$Hold)
team_rankings_2324$Hold <- gsub(" ?Boldklub ?", "", team_rankings_2324$Hold)
team_rankings_2324 <- team_rankings_2324 %>% rename("Team" = Hold)

xp_sum <- xp_sum %>% 
  left_join(team_rankings_2324, "Team")


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

team_logos <- all_teams_2324 %>%
  select(TEAMNAME, IMAGEDATAURL) %>%
  rename(Team = TEAMNAME, logo_url = IMAGEDATAURL)

xp_tidsserie <- xp_tidsserie %>%
  left_join(team_logos, by = "Team")

maks_kampe <- max(xp_tidsserie$kamp_nummer) 

team_colors <- c(
  "Brøndby" = "#ffe100",
  "AGF" = "white",
  "København" = "white",
  "Midtjylland" = "white",
  "Nordsjælland" = "white",
  "Silkeborg" = "white",
  "OB" = "white",
  "Randers" = "white",
  "Viborg" = "white",
  "Lyngby" = "white",
  "Hvidovre" = "white",
  "Vejle" = "white"
)


# as loop
for (kamp in 1:maks_kampe) {
  Sys.sleep(0.5)
  
  current_data <- xp_tidsserie %>%
    filter(kamp_nummer <= kamp) %>%
    group_by(Team, logo_url) %>%
    summarise(cumulative_xP = sum(Team_xP, na.rm = TRUE)) %>%
    arrange(desc(cumulative_xP))
  
  p <- ggplot(current_data, aes(x = reorder(Team, cumulative_xP), y = cumulative_xP)) +
    geom_col(aes(fill = Team), color = "black") +
    geom_image(aes(image = logo_url), size = 0.05, asp = 1.2) +
    coord_flip() +
    labs(
      title = paste("xP efter", kamp, "kampe"),
      x = NULL,
      y = "Akkumuleret xP"
    ) +
    theme_minimal(base_size = 14)
  
  print(p)
}


# with gganimate
xp_tidsserie_full <- xp_tidsserie %>%
  group_by(Team) %>%
  complete(kamp_nummer = 1:maks_kampe) %>%
  fill(cumulative_xP, logo_url, .direction = "down") %>%
  replace_na(list(cumulative_xP = 0))

rank_order <- xp_tidsserie_full %>%
  group_by(Team) %>%
  filter(kamp_nummer == max(kamp_nummer, na.rm = TRUE)) %>%
  arrange(cumulative_xP) %>%
  pull(Team)

xp_tidsserie_full <- xp_tidsserie_full %>%
  mutate(Team = factor(Team, levels = rank_order))

p <- ggplot(xp_tidsserie_full, aes(x = Team, y = cumulative_xP)) +
  geom_col(aes(fill = Team), color = "black") +
  geom_image(aes(image = logo_url), size = 0.06, asp = 1.2) +
  scale_fill_manual(values = team_colors) +
  coord_flip() +
  labs(
    title = "xP Udvikling gennem sæsonen – Kamp {closest_state}",
    x = NULL,
    y = "Akkumuleret Expected Points"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.y = element_text(face = "bold"),
    axis.text.x = element_text(size = 12),
    legend.position = "none"
  ) +
  transition_states(kamp_nummer, transition_length = 2, state_length = 1) +
  ease_aes("linear")

xp_anim <- animate(p, width = 800, height = 600, fps = 10, duration = maks_kampe * 0.4, loop = FALSE)
anim_save("xP/Shiny/www/xp_animation.gif", xp_anim)


# Find sidste kamp per hold
xp_last_frame <- xp_tidsserie_full %>%
  group_by(Team) %>%
  filter(kamp_nummer == max(kamp_nummer, na.rm = TRUE)) %>%
  ungroup()

xp_last_frame <- xp_last_frame %>%
  left_join(xp_sum %>% select(Team, Point), by = "Team")


# Lav statisk barplot med logo og farver
ggplot(xp_last_frame, aes(x = reorder(Team, cumulative_xP), y = cumulative_xP)) +
  geom_col(aes(fill = Team), color = "black") +
  geom_image(aes(image = logo_url), size = 0.06, asp = 1.2) +
  scale_fill_manual(values = team_colors) +
  coord_flip() +
  labs(
    title = "Slutstilling xP – baseret på antal spillede kampe per hold",
    x = NULL,
    y = "Akkumuleret Expected Points"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.y = element_text(face = "bold"),
    axis.text.x = element_text(size = 12),
    legend.position = "none"
  )

# Sørg for at xp_last_frame også indeholder den faktiske point-kolonne
ggplot(xp_last_frame, aes(x = reorder(Team, cumulative_xP), y = cumulative_xP)) +
  geom_col(aes(fill = Team), color = "black") +
  # Tilføj punkt for faktiske point
  geom_point(aes(y = Point), color = "black", size = 3) +
  geom_image(aes(image = logo_url), size = 0.06, asp = 1.2) +
  scale_fill_manual(values = team_colors) +
  coord_flip() +
  labs(
    title = "Slutstilling xP – baseret på antal spillede kampe per hold",
    subtitle = "Hold med laveste points, underpræsterede ift. deres skudchancer",
    x = NULL,
    y = "Akkumuleret Expected Points"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic"),
    axis.text.y = element_text(face = "bold"),
    axis.text.x = element_text(size = 12),
    legend.position = "none"
  )




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





# Gem animation (loop = FALSE)
xp_anim <- animate(p, width = 800, height = 600, fps = 10, duration = maks_kampe * 0.4, loop = FALSE)
anim_save("xP/Shiny/www/xp_animation.gif", xp_anim)

# Gem sidste frame som billede
last_frame <- tail(xp_tidsserie_full$kamp_nummer, 1)
static_plot <- p + transition_states(NULL) + 
  labs(title = paste("xP efter", maks_kampe, "kampe"))  # override animation

ggsave("xP/Shiny/www/xp_static.png", plot = static_plot, width = 8, height = 6, dpi = 100)


