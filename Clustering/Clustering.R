#### OPGAVE 3 - Overblik over afleveringer i superligaen ####
library(dplyr)
library(ggplot2)
library(factoextra)
library(cluster)
library(ggsoccer)
library(plotly)
library(RMariaDB)
library(corrplot)


#-------------------------------------------------------------
#### OPGAVE 3.1 - Clustering af afleveringer i Superligaen ####
# Formål: Identificere forskellige typer afleveringer på baggrund af:
# Lænde, vinkel, position, præcision, navngive dem informativt

####################
### DATARETRIVAL ###
####################
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
allpasses_raw <- dbReadTable(con, "wyscout_matchevents_passes_sl")


#bind those badboys
allpassesevent_raw <- allpasses_raw %>%
  left_join(allevents_raw, by = "EVENT_WYID")

allpasses <- allpassesevent_raw %>% filter(PRIMARYTYPE.x == "pass")

allpasses <- allpasses %>%
  left_join(allplayers_raw, by = "PLAYER_WYID")

allpasses <- allpasses %>%
  left_join(allteams_raw, by = "TEAM_WYID")

allpasses <- allpasses %>%
  distinct(EVENT_WYID, .keep_all = TRUE)

clustering_vars <- allpasses[, c("ANGLE", "LENGTH","LOCATIONX","LOCATIONY")]

# Skalere data, så det kan anvendes til clustering 
allpasses_scaled <- scale(clustering_vars)

## Correlation     
passes_Corr <- cor(allpasses_scaled)
corrplot(passes_Corr,addCoef.col = "black",method = "square",type = "lower")

allpasses_test <- allpasses %>% 
  group_by(SHORTNAME, MATCH_WYID.x) %>%
  summarise(player_passes = n(),
            .groups = "drop") %>%  # Antal afleveringer pr. kamp
  group_by(SHORTNAME) %>%
  summarise(player_avgpass = mean(player_passes))  # Gennemsnit pr. spiller

allpasses <- allpasses %>%
  left_join(allpasses_test, by = "SHORTNAME")

# evt add the player avg passes, if cluster is unclean
pass_data <- allpasses[, c("ANGLE","LENGTH","LOCATIONX","LOCATIONY","player_avgpass")]
pass_data_scaled <- as.data.frame(scale(pass_data))

set.seed(123)
sample_size <- 50000
df_sampled <- pass_data_scaled[sample(1:nrow(pass_data_scaled), sample_size), ]

dftwss <- data.frame(k = 1:20, twss = NA)

# Kmeans
for (i in 1:20) {
  tmod <- kmeans(df_sampled, centers = i, nstart = 10, iter.max = 500)
  dftwss[i, 'twss'] <- tmod$tot.withinss
}

ggplot(dftwss, aes(x = k, y = twss)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Elbow Method for Optimal K",
       x = "Number of Clusters (K)",
       y = "Total Within-Cluster Sum of Squares") +
  scale_x_continuous(breaks = scales::breaks_width(1)) +
  theme_minimal()

#kmeans cluster
kmod <- kmeans(pass_data_scaled, nstart = 10, centers = 5)
fviz_cluster(kmod, data = pass_data_scaled)
pass_data$cluster <- as.factor(kmod$cluster)

clusters <- pass_data %>% group_by(cluster) %>%
  summarise(
    Angle = mean(ANGLE),
    Length = mean(LENGTH),
    Player_avgpass = mean(player_avgpass),
    Player_avg_y = mean(LOCATIONY),
    Player_avg_x = mean(LOCATIONX),
    count = n()
  )

#TILFØJ TEAMNAME TIL ALLPASSES OG I PLAYER CLUSTER (SKAL BRUGES TIL PLOTLY)!!!!!!!!!
allpasses$cluster <- as.factor(kmod$cluster)
player_cluster <- allpasses[,c("TEAM_WYID", "TEAMNAME", "ROLENAME")]
player_cluster <- allpasses %>% group_by(SHORTNAME)


##################
#####  PCA  #####
#################

data.pca <- princomp(pass_data_scaled)
summary(data.pca)
data.pca$loadings[, 1:3] 
fviz_pca_var(data.pca, col.var = "black")

# opsummering i spiler statestik
player_stats <- allpasses %>%
  group_by(SHORTNAME, PLAYER_WYID) %>%
  filter(n() > 100) %>% 
  summarise(
    matches_played = n_distinct(MATCH_WYID.x),    
    total_passes = n(),                      
    avg_passes_per_match = total_passes / matches_played, 
    # giver fejl: sd_passes_per_match = sd(player_passes_per_match$total_passes_per_match),
    avg_LENGTH = mean(LENGTH),
    sd_pass_lenght = sd(LENGTH),
    avg_pass_angle = mean(ANGLE),
    sd_pass_angle = sd(ANGLE),
    avg_y = mean(LOCATIONX),
    avg_x = mean(LOCATIONY),
    pass_acc = (sum(ACCURATE == TRUE) / total_passes) * 100,
    cluster_1 = sum(cluster == 1),
    cluster_2 = sum(cluster == 2), 
    cluster_3 = sum(cluster == 3),
    cluster_4 = sum(cluster == 4),
    cluster_5 = sum(cluster == 5)
  )

# finde en spillers main cluster
player_stats <- player_stats %>%
  mutate(main_cluster = max.col(across(starts_with("cluster_"))))

# Måske også finde spillerens primære position? 

############################
#####     HEATMAPS    ######
############################

########### Loop for hver enkel heatmap ###########
for (k in 1:5) {
  ggplot(allpasses %>% filter(cluster == k)) +
    annotate_pitch(colour = "white", fill = "gray") +  
    stat_density_2d_filled(aes(x = LOCATIONX, y = LOCATIONY), 
                           alpha = 0.7, contour_var = "ndensity") +  
    theme_pitch() +
    scale_fill_viridis_d(option = "magma") +  
    labs(title = paste("Passes Positions Heatmap - Cluster", k),
         x = "Pitch Length", y = "Pitch Width") +
    theme(legend.position = "right") -> p
  
  print(p)
}

########## Samlet heatmaps  ######################
ggplot(allpasses, aes(x = LOCATIONX, y = LOCATIONY)) +
  annotate_pitch(colour = "white", fill = "gray") +
  stat_density_2d_filled(alpha = 0.7, contour_var = "ndensity") +
  theme_pitch() +
  scale_fill_viridis_d(option = "magma") +
  labs(title = "Passes Positions Heatmaps per Cluster",
       x = "Pitch Length", y = "Pitch Width") +
  theme(legend.position = "right") +
  facet_wrap(~cluster)  # Her opdeles plottet efter cluster

########### Loop for hver enkel heatmap ###########

for (k in 1:5) {
  ggplot(allpasses %>% filter(cluster == k)) +
    annotate_pitch(colour = "white", fill = "gray") +  
    stat_density_2d_filled(aes(x = POSSESSIONENDLOCATIONX, y = POSSESSIONENDLOCATIONY), 
                           alpha = 0.7, contour_var = "ndensity") +  
    theme_pitch() +
    scale_fill_viridis_d(option = "magma") +  
    labs(title = paste("Passes End Positions Heatmap - Cluster", k),
         x = "Pitch Length", y = "Pitch Width") +
    theme(legend.position = "right") -> p_end
  
  print(p_end)
}

########## Samlet heatmaps  ######################

ggplot(allpasses, aes(x = POSSESSIONENDLOCATIONX, y = POSSESSIONENDLOCATIONY)) +
  annotate_pitch(colour = "white", fill = "gray") +
  stat_density_2d_filled(alpha = 0.7, contour_var = "ndensity") +
  theme_pitch() +
  scale_fill_viridis_d(option = "magma") +
  labs(title = "Passes End Positions Heatmaps per Cluster",
       x = "Pitch Length", y = "Pitch Width") +
  theme(legend.position = "right") +
  facet_wrap(~cluster)

################################
#####    GENERAL PLOTS     #####
################################

player_stats <- player_stats %>%
  left_join(allpasses %>% select(PLAYER_WYID, ROLENAME) %>% distinct(), 
            by = "PLAYER_WYID")

# Tæl antal spillere per rolle
ggplot(player_stats, aes(x = ROLENAME, fill = ROLENAME)) +
  geom_bar() +
  labs(title = "Number of Players per Role",
       x = "Role",
       y = "Number of Players") +
  theme_minimal()

# number of player per role
ggplot(player_stats, aes(x = as.factor(ROLENAME), fill = ROLENAME)) +
  geom_bar() +
  labs(title = "number in every role",
       x = "Main Cluster",
       y = "Number of Players") +
  theme_minimal()

# roles in each cluster
ggplot(player_stats, aes(x = as.factor(main_cluster), fill = ROLENAME)) +
  geom_bar() +
  labs(title = "Number of Players per Main Cluster",
       x = "Main Cluster",
       y = "Number of Players") +
  theme_minimal()


###############################
####    PLOTLY & HOVERING #####
###############################

plot_ly(
  data = player_stats,
  x=~avg_x,y=~avg_y,z=~main_cluster,
  type = "scatter3d",
  mode = "markers",
  color = ~as.factor(main_cluster),
  text = ~paste0(
    "Player: ",SHORTNAME,"<br>",
    "Main cluster: ",main_cluster,"<br>",
    "Cluster 1: ",cluster_1, ", Cluster 2: ", cluster_2, ", Cluster 3: ",cluster_3, ", Cluster 4: ", cluster_4,"<br>",
    "Total passes: ",total_passes,"<br>",
    "Avg pass length: ",round(avg_LENGTH,1),"<br>",
    "Avg pass angle: ",round(avg_pass_angle,1),"<br>"
  ),
  hoverinfo="text")

plot_ly(
  data = player_stats,
  x=~total_passes,y=~avg_LENGTH,z=~avg_pass_angle,
  type = "scatter3d",
  mode = "markers",
  color = ~as.factor(main_cluster),
  text = ~paste0(
    "Player: ",SHORTNAME,"<br>",
    "Main cluster: ",main_cluster,"<br>",
    "Cluster 1: ",cluster_1, ", Cluster 2: ", cluster_2, ", Cluster 3: ",cluster_3, ", Cluster 4: ", cluster_4,"<br>",
    "Total passes: ",total_passes,"<br>",
    "Avg pass length: ",round(avg_LENGTH,1),"<br>",
    "Avg pass angle: ",round(avg_pass_angle,1),"<br>"
  ),
  hoverinfo="text")

plot_ly(
  data = player_stats,
  x=~total_passes,y=~sd_pass_lenght,z=~avg_pass_angle,
  type = "scatter3d",
  mode = "markers",
  color = ~as.factor(main_cluster),
  text = ~paste0(
    "Player: ",SHORTNAME,"<br>",
    "Main cluster: ",main_cluster,"<br>",
    "Cluster 1: ",cluster_1, ", Cluster 2: ", cluster_2, ", Cluster 3: ",cluster_3, ", Cluster 4: ", cluster_4,"<br>",
    "Total passes: ",total_passes,"<br>",
    "Avg pass length: ",round(avg_LENGTH,1),"<br>",
    "Avg pass angle: ",round(avg_pass_angle,1),"<br>"
  ),
  hoverinfo="text")
