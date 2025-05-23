#### OPGAVE 3 - Overblik over afleveringer i superligaen ####
library(dplyr)
library(ggplot2)
library(factoextra)
library(cluster)
library(ggsoccer)
library(plotly)
library(RMariaDB)
library(corrplot)
library(viridis)


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

#bind raw data - joinsession
allpassesevent_raw <- allpasses_raw %>%
  left_join(allevents_raw, by = "EVENT_WYID")

allpasses <- allpassesevent_raw %>% filter(PRIMARYTYPE.x == "pass")

allpasses <- allpasses %>%
  left_join(allplayers_raw, by = "PLAYER_WYID")

allpasses <- allpasses %>%
  left_join(allteams_raw, by = "TEAM_WYID")

allpasses <- allpasses %>%
  distinct(EVENT_WYID, .keep_all = TRUE)

#udvalg af vars som vi skal bruge til cluster analysen
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
  summarise(player_avgpass = mean(player_passes))  # Gennemsnit pr. spiller pr. kamp

allpasses <- allpasses %>%
  left_join(allpasses_test, by = "SHORTNAME")

# evt add the player avg passes, if cluster is unclean
#pass_data <- allpasses[, c("ANGLE","LENGTH","LOCATIONX","LOCATIONY")]
allpasses_1match <- allpasses %>% filter(MATCH_WYID.x == 5585724) 
pass_data <- allpasses_1match[, c("ANGLE","LENGTH","LOCATIONX","LOCATIONY","player_avgpass")]
pass_data <- allpasses[, c("ANGLE","LENGTH","LOCATIONX","LOCATIONY","player_avgpass")]
pass_data_scaled <- as.data.frame(scale(pass_data))


season1 <- allpasses %>% filter(SEASON_WYID.x == 189918)
nrow(season1)
length(unique(season1$MATCH_WYID.x))

season2 <- allpasses %>% filter(SEASON_WYID.x == 188945)
nrow(season2)
length(unique(season2$MATCH_WYID.x))

#### beskrivende statistik ####
sample_size <- 50000
allpasses_sample <- allpasses[sample(1:nrow(allpasses), sample_size), ]
ggplot(allpasses_sample, aes(x = LOCATIONX, y = LOCATIONY)) +
  annotate_pitch(colour = "white", fill = "gray20") +
  stat_density_2d_filled(alpha = 0.7, contour_var = "ndensity") +
  theme_pitch() +
  scale_fill_viridis_d(option = "magma") +
  labs(title = "Heatmap: Startpositioner for afleveringer",
       x = "Banens længde", y = "Banens bredde") +
  theme(legend.position = "right")

ggplot(allpasses_sample, aes(x = POSSESSIONENDLOCATIONX, y = POSSESSIONENDLOCATIONY)) +
  annotate_pitch(colour = "white", fill = "gray20") +
  stat_density_2d_filled(alpha = 0.7, contour_var = "ndensity") +
  theme_pitch() +
  scale_fill_viridis_d(option = "magma") +
  labs(title = "Heatmap: Stoppositioner for afleveringer",
       x = "Banens længde", y = "Banens bredde") +
  theme(legend.position = "right")


fviz_cluster(kmod, data = pass_data_scaled)

### 1. Find hoved-cluster (flest afleveringer pr. spiller) ###
player_main_cluster <- allpasses %>%
  group_by(SHORTNAME, cluster) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(SHORTNAME) %>%
  slice_max(n, n = 1, with_ties = FALSE)  # én dominerende cluster per spiller


### 2. Lav gennemsnit per spiller ###
player_summary <- allpasses %>%
  group_by(SHORTNAME) %>%
  summarise(
    avg_angle = mean(ANGLE, na.rm = TRUE),
    avg_length = mean(LENGTH, na.rm = TRUE),
    avg_x = mean(LOCATIONX, na.rm = TRUE),
    avg_y = mean(LOCATIONY, na.rm = TRUE)
  )

### 3. Skalér data ###
player_scaled <- scale(player_summary[,-1])
rownames(player_scaled) <- player_summary$SHORTNAME

### 4. Kombinér med hoved-cluster ###
player_scaled_df <- as.data.frame(player_scaled) %>%
  mutate(SHORTNAME = rownames(player_scaled)) %>%
  left_join(player_main_cluster, by = "SHORTNAME")

### 5. Visualisering med fviz_cluster ###
fviz_cluster(
  list(data = player_scaled_df[,1:4], cluster = player_scaled_df$cluster),
  geom = "point",
  labelsize = 8,
  show.clust.cent = FALSE
) +
  labs(title = "Spillere fordelt efter dominerende afleveringsmønster")


#### CLUSTER begynder ####

#albuemetoden
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
  labs(title = "The elbow starts bending at around 8.",
       x = "Number of Clusters (K)",
       y = "Total Within-Cluster Sum of Squares") +
  scale_x_continuous(breaks = scales::breaks_width(1)) +
  theme_minimal()

ggplot(dftwss, aes(x = k, y = twss)) +
  geom_line(color = "#1f77b4", size = 1) +
  geom_point(color = "#d62728", size = 3) +
  
  # Lodret linje ved elbow point
  geom_vline(xintercept = 4, linetype = "dashed", color = "gray40", linewidth = 1) +
  geom_vline(xintercept = 10, linetype = "dashed", color = "gray40", linewidth = 1) +
  #annotate("text", x = 8.2, y = max(dftwss$twss)*0.9, label = "Elbow point ≈ 8", hjust = 0, size = 4.5) +
  
  labs(
    title = "Elbow Method: Vælg antal klynger (K)",
    subtitle = "K ≈ 4 - 10 giver markant fald i within-cluster variation",
    x = "Antal clusters (K)",
    y = "Total Within-Cluster Sum of Squares (TWSS)"
  ) +
  
  scale_x_continuous(breaks = 1:20) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray30"),
    axis.title = element_text(face = "bold")
  )


#Silhouette 4????
set.seed(123)
sample_small <- df_sampled[sample(nrow(df_sampled), 5000), ]
kmod_small <- kmeans(sample_small, centers = 9, nstart = 10)

sil <- silhouette(kmod_small$cluster, dist(sample_small))
mean(sil[, 3])  # gennemsnitlig silhouette score

#kmeans cluster
kmod <- kmeans(pass_data_scaled, nstart = 10, centers = 8)
fviz_cluster(kmod, data = pass_data_scaled)
pass_data$cluster <- as.factor(kmod$cluster)
allpasses$cluster <- pass_data$cluster

clusters <- pass_data %>% group_by(cluster) %>%
  summarise(
    Angle = mean(ANGLE),
    Length = mean(LENGTH),
    Player_avgpass = mean(player_avgpass),
    Player_AVG_Y_start = mean(LOCATIONY),
    Player_AVG_X_start = mean(LOCATIONX),
    count = n()
  )

#TILFØJ TEAMNAME TIL ALLPASSES OG I PLAYER CLUSTER (SKAL BRUGES TIL PLOTLY)!!!!!!!!!
allpasses$cluster <- as.factor(kmod$cluster)
player_cluster <- allpasses[,c("TEAM_WYID", "TEAMNAME", "ROLENAME")]
player_cluster <- allpasses %>% group_by(SHORTNAME)
player_cluster <- allpasses %>% group_by(TEAMNAME)


##################
#####  PCA  #####
#################

data.pca <- princomp(pass_data_scaled)
summary(data.pca)
data.pca$loadings[, 1:5] 
fviz_pca_var(data.pca, col.var = "red")

# opsummering i spiler statestik
player_stats <- allpasses %>%
  group_by(SHORTNAME, PLAYER_WYID) %>%
  filter(n() > 100) %>% 
  summarise(
    matches_played = n_distinct(MATCH_WYID.x),    
    total_passes = n(),                      
    avg_passes_per_match = total_passes / matches_played, 
    avg_LENGTH = mean(LENGTH),
    sd_pass_lenght = sd(LENGTH),
    avg_pass_angle = mean(ANGLE),
    sd_pass_angle = sd(ANGLE),
    AVG_Y_start = mean(LOCATIONX),
    AVG_X_start = mean(LOCATIONY),
    pass_acc = (sum(ACCURATE == TRUE) / total_passes) * 100,
    cluster_1 = sum(cluster == 1),
    cluster_2 = sum(cluster == 2), 
    cluster_3 = sum(cluster == 3),
    cluster_4 = sum(cluster == 4),
    cluster_5 = sum(cluster == 5),
    cluster_6 = sum(cluster == 6),
    cluster_7 = sum(cluster == 7),
    cluster_8 = sum(cluster == 8)
  )

# finde en spillers main cluster
player_stats <- player_stats %>%
  mutate(main_cluster = max.col(across(starts_with("cluster_"))))

###### PCA PLAYER STATS ######
PCA_PLAYER_STATS <- player_stats[,3:12]
PCA_PLAYER_STATS <- scale(PCA_PLAYER_STATS)
data.pca <- princomp(PCA_PLAYER_STATS)
summary(data.pca)
data.pca$loadings[, 1:10] 
fviz_pca_var(data.pca, col.var = "red")


# Brug samme PCA som tidligere
pca_input <- player_stats[, c("avg_pass_angle", "avg_LENGTH", "AVG_X_start", "AVG_Y_start", "avg_passes_per_match")]
pca_scaled <- scale(pca_input)
pca_result <- prcomp(pca_scaled)

# Data til punkterne (spillere)
ind_df <- as.data.frame(pca_result$x[, 1:2])
ind_df$cluster <- as.factor(player_stats$main_cluster)

# Data til variabel-pile (loadings)
loadings <- as.data.frame(pca_result$rotation[, 1:2])
loadings$varname <- rownames(loadings)
# Ændr variabelnavne til noget mere menneskeligt
loadings$varname <- recode(loadings$varname,
                           "avg_pass_angle" = "Gns. vinkel",
                           "avg_LENGTH" = "Gns. længde",
                           "AVG_X_start" = "Startpos. X",
                           "AVG_Y_start" = "Startpos. Y",
                           "avg_passes_per_match" = "Afl. pr. kamp"
)


# Lav plot
ggplot() +
  # Spillere
  geom_point(data = ind_df, aes(x = PC1, y = PC2, color = cluster), alpha = 0.7) +
  
  # Variabelpile
  geom_segment(data = loadings, aes(x = 0, y = 0, xend = PC1 * 3, yend = PC2 * 3),
               arrow = arrow(length = unit(0.3, "cm")), color = "black") +
  
  # Variabelnavne
  geom_label(data = loadings, aes(x = PC1 * 3.2, y = PC2 * 3.2, label = varname),
             size = 4, fill = "white", alpha = 0.7, label.size = NA)

  labs(title = "PCA – Spillere + variabel-retning", x = "PC1", y = "PC2") +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.title = element_blank())


  
  
  player_stats$sd_total <- (player_stats$sd_pass_lenght + player_stats$sd_pass_angle) / 2
  ggplot(player_stats, aes(x = as.factor(main_cluster), y = sd_total, fill = as.factor(main_cluster))) +
    geom_violin(trim = FALSE, alpha = 0.7) +
    stat_summary(fun = mean, geom = "point", shape = 20, size = 2, color = "black") +
    labs(title = "Variation i afleveringsstil pr. cluster (violin plot)",
         x = "Main cluster", y = "Gns. standardafvigelse") +
    theme_minimal() +
    theme(legend.position = "none")
  
  ggplot(player_stats, aes(x = as.factor(main_cluster), y = sd_total, color = as.factor(main_cluster))) +
    geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
    stat_summary(fun = mean, geom = "crossbar", width = 0.4, color = "black") +
    labs(title = "Spillernes variation i afleveringsstil",
         x = "Main cluster", y = "Gns. standardafvigelse") +
    theme_minimal() +
    theme(legend.position = "none")
  
  
  cluster_variation <- player_stats %>%
    group_by(main_cluster) %>%
    summarise(mean_sd = mean(sd_total), sd_sd = sd(sd_total))
  
  ggplot(cluster_variation, aes(x = as.factor(main_cluster), y = mean_sd, fill = as.factor(main_cluster))) +
    geom_col(alpha = 0.7) +
    geom_errorbar(aes(ymin = mean_sd - sd_sd, ymax = mean_sd + sd_sd), width = 0.2) +
    labs(title = "Gns. variation i afleveringer pr. cluster",
         x = "Main cluster", y = "Gennemsnitlig SD") +
    theme_minimal() +
    theme(legend.position = "none")
  
  
  

############################
#####     HEATMAPS    ######
############################

########### Loop for hver enkel heatmap ud fra start på pass ###########
for (k in 1:8) {
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




# Tom data frame til cluster-stats
cluster_stats <- data.frame()

# Total antal afleveringer til procentberegning
total_passes <- nrow(allpasses)

for (k in 1:8) {
  
  df_cluster <- allpasses %>% filter(cluster == k)
  
  # Gennemsnitlig start- og slutposition
  avg_start_x <- mean(df_cluster$LOCATIONX, na.rm = TRUE)
  avg_start_y <- mean(df_cluster$LOCATIONY, na.rm = TRUE)
  avg_end_x   <- mean(df_cluster$POSSESSIONENDLOCATIONX, na.rm = TRUE)
  avg_end_y   <- mean(df_cluster$POSSESSIONENDLOCATIONY, na.rm = TRUE)
  
  # Beregn og gem stats
  cluster_stats <- rbind(cluster_stats, data.frame(
    cluster = k,
    n_passes = nrow(df_cluster),
    pct_passes = round(nrow(df_cluster) / total_passes * 100, 2),
    mean_length = mean(df_cluster$LENGTH, na.rm = TRUE),
    sd_length = sd(df_cluster$LENGTH, na.rm = TRUE),
    mean_angle = mean(df_cluster$ANGLE, na.rm = TRUE),
    sd_angle = sd(df_cluster$ANGLE, na.rm = TRUE)
  ))
  
  # Plot heatmap + pil
  ggplot(df_cluster) +
    annotate_pitch(colour = "white", fill = "gray20") +
    
    stat_density_2d_filled(aes(x = LOCATIONX, y = LOCATIONY), 
                           alpha = 0.7, contour_var = "ndensity") +
    
    geom_segment(aes(x = avg_start_x, y = avg_start_y,
                     xend = avg_end_x, yend = avg_end_y),
                 arrow = arrow(length = unit(0.4, "cm")),
                 color = "lightgray", size = 1.2) +
    
    scale_fill_viridis_d(option = "magma") +
    theme_pitch() +
    labs(title = paste("Cluster", k, "- Heatmap + Gennemsnitlig Retning"),
         x = "Pitch Length", y = "Pitch Width") +
    theme(legend.position = "right") -> p
  
  print(p)
}

# Se resultater
print(cluster_stats)


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

########### Loop for hver enkel heatmap af pass endlocation ###########

for (k in 1:8) {
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

player_stats <- player_stats[-375,]


ggplot(player_stats, aes(x = ROLENAME, fill = ROLENAME)) +
  geom_bar() +
  labs(title = "Number of Players per Role",
       x = "Role",
       y = "Number of Players") +
  theme_minimal()

# number of player per role
ggplot(player_stats, aes(x = as.factor(ROLENAME), fill = ROLENAME)) +
  geom_bar(position = "dodge") +
  labs(title = "number in every role",
       x = "Main Cluster",
       y = "Number of Players") +
  theme_minimal()

# roles in each cluster
ggplot(player_stats, aes(x = as.factor(main_cluster), fill = ROLENAME)) +
  geom_bar(position = "dodge") +
  labs(title = "Number of Players per Main Cluster",
       x = "Main Cluster",
       y = "Number of Players") +
  theme_minimal()



####   PLOTLY & HOVERING  #####


plot_ly(
  data = player_stats,
  x=~AVG_X_start,y=~AVG_Y_start,z=~main_cluster,
  type = "scatter3d",
  mode = "markers",
  color = ~as.factor(main_cluster),
  text = ~paste0(
    "Player: ",SHORTNAME,"<br>",
    "Main cluster: ",main_cluster,"<br>",
    "Cluster 1: ",cluster_1, 
    ", Cluster 2: ", cluster_2, 
    ", Cluster 3: ",cluster_3, 
    ", Cluster 4: ", cluster_4,
    ", Cluster 5: ", cluster_5,
    ", Cluster 6: ", cluster_6,
    ", Cluster 7: ", cluster_7,
    ", Cluster 8: ", cluster_8,
    "<br>",
    "Total passes: ",total_passes,"<br>",
    "Avg pass length: ",round(avg_LENGTH,1),"<br>",
    "Avg pass angle: ",round(avg_pass_angle,1),"<br>",
    "Player-possition on field: ", player_stats$ROLENAME
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
    "Cluster 1: ",cluster_1, 
    ", Cluster 2: ", cluster_2, 
    ", Cluster 3: ",cluster_3, 
    ", Cluster 4: ", cluster_4,
    ", Cluster 5: ", cluster_5,
    ", Cluster 6: ", cluster_6,
    ", Cluster 7: ", cluster_7,
    ", Cluster 8: ", cluster_8,
    "<br>",
    "<br>",
    "Total passes: ",total_passes,"<br>",
    "Avg pass length: ",round(avg_LENGTH,1),"<br>",
    "Avg pass angle: ",round(avg_pass_angle,1),"<br>",
    "Player-possition on field: ", player_stats$ROLENAME
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
    "Cluster 1: ",cluster_1, 
    ", Cluster 2: ", cluster_2, 
    ", Cluster 3: ",cluster_3, 
    ", Cluster 4: ", cluster_4,
    ", Cluster 5: ", cluster_5,
    ", Cluster 6: ", cluster_6,
    ", Cluster 7: ", cluster_7,
    ", Cluster 8: ", cluster_8,
    "<br>",
    "<br>",
    "Total passes: ",total_passes,"<br>",
    "Avg pass length: ",round(avg_LENGTH,1),"<br>",
    "Avg pass angle: ",round(avg_pass_angle,1),"<br>",
    "Player-possition on field: ", player_stats$ROLENAME
  ),
  hoverinfo="text")


#### shiny ####

library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggsoccer)
library(plotly)
library(dplyr)
library(viridis)
library(corrplot)
library(factoextra)

# Beskrivende navne og forklaringer til hvert cluster
cluster_labels <- list(
  "1" = list(
    title = "Højre midtbane assistspillere",
    desc = "Spillere der ofte afleverer fra højre midt ind i feltet med høj vinkel."
  ),
  "2" = list(
    title = "Venstre midtbane assistspillere",
    desc = "Afleveringerne kommer typisk fra venstre midt og slutter i feltet – vinklen er lavere."
  ),
  "3" = list(
    title = "Lange fremskudte afleveringer",
    desc = "Lang afstand, typisk fra midten af banen og direkte mod feltet."
  ),
  "4" = list(
    title = "Midtbanens boldflyttere",
    desc = "Centrale afleveringer der dækker moderat afstand."
  ),
  "5" = list(
    title = "Højre kant – afleveringer i feltet",
    desc = "Spillere der afleverer fra højre side dybt i banen og ind i feltet."
  ),
  "6" = list(
    title = "Venstre kant – afleveringer i feltet",
    desc = "Afleveringer fra venstre angribende zone og ind mod midten."
  ),
  "7" = list(
    title = "Centrale indlæg fra midten",
    desc = "Mange afleveringer fra midten mod kanten af feltet."
  ),
  "8" = list(
    title = "Defensive afleveringer fra midt",
    desc = "Afleveringer med lav vinkel og kortere afstand, fra midtbanen og tilbage."
  )
)


#         UI             #

library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggsoccer)
library(plotly)
library(dplyr)
library(viridis)
library(corrplot)
library(factoextra)

# UI som dashboard layout
ui <- dashboardPage(
  dashboardHeader(title = "K-Means Clustering Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Info", tabName = "info", icon = icon("info-circle")),
      menuItem("Cluster Explorer", tabName = "explorer", icon = icon("search")),
      menuItem("Metode", tabName = "method", icon = icon("flask"))
    )
  ),
  dashboardBody(
    tabItems(
      
      # Info tab
      tabItem(tabName = "info",
              fluidRow(
                box(title = "Velkommen til vores K-Means App", status = "primary", solidHeader = TRUE, width = 12,
                    HTML("<h3>Formål</h3>
                  <p>Her præsenteres et overblik over afleveringsmønstre i Superligaen, baseret på K-Means clustering.</p>
                  <ul>
                  <li>Vælg hvilket cluster du vil undersøge</li>
                  <li>Se start- og slut-heatmaps</li>
                  <li>Vælg mellem forskellige 3D-visualiseringer</li>
                  </ul>
                  <p>Fanen <strong>Metode</strong> viser hvordan clustrene er fundet frem til.</p>")
                )
              )
      ),
      
      # Cluster Explorer tab
      tabItem(tabName = "explorer",
              fluidRow(
                box(width = 3, title = "Indstillinger", status = "info", solidHeader = TRUE,
                    selectInput("cluster_choice", "Vælg cluster:", choices = 1:8, selected = 1),
                    uiOutput("cluster_title_box"),
                    uiOutput("cluster_summary_box"),
                    selectInput("plotly_type", "Vælg Plotly-visning:",
                                choices = c("3D Afleverings-plot (X,Y,Angle)" = "default",
                                            "Total passes vs Længde vs Angle" = "tla",
                                            "Total passes vs Sd længde vs Angle" = "tsda"))
                ),
                box(width = 9, title = "Heatmaps og 3D plot", status = "primary", solidHeader = TRUE,
                    fluidRow(
                      column(6, plotOutput("heatmap_start")),
                      column(6, plotOutput("heatmap_end"))
                    ),
                    br(),
                    plotlyOutput("plotly_cluster")
                )
              )
      ),
      
      # Metode tab
      tabItem(tabName = "method",
              fluidRow(
                box(title = "Elbow Plot", width = 6, plotOutput("elbowPlot")),
                box(title = "Korrelationsmatrix", width = 6, plotOutput("corrPlot"))
              ),
              fluidRow(
                box(title = "PCA-variabler (fviz_pca_var)", width = 6, plotOutput("pcaPlotVar")),
                box(title = "K-Means cluster plot", width = 6, plotOutput("kmeansPlot"))
              )
      )
    )
  )
)


#       SERVER           #

server <- function(input, output, session) {
  
  # Elbow-plot
  output$elbowPlot <- renderPlot({
    ggplot(dftwss, aes(x = k, y = twss)) +
      geom_line(color = "blue") +
      geom_point(color = "red") +
      labs(title = "Elbow Method for Optimal K", x = "Number of Clusters (K)", y = "Total Within-Cluster Sum of Squares") +
      scale_x_continuous(breaks = 1:10) +
      theme_minimal()
  })
  
  output$corrPlot <- renderPlot({
    corrplot(passes_Corr, addCoef.col = "black", method = "square", type = "lower")
  })
  
  output$pcaPlotVar <- renderPlot({
    fviz_pca_var(data.pca, col.var = "red")
  })
  
  output$kmeansPlot <- renderPlot({
    fviz_cluster(kmod, data = pass_data_scaled)
  })
  
  selected_cluster <- reactive({ as.character(input$cluster_choice) })
  
  output$heatmap_start <- renderPlot({
    ggplot(allpasses %>% filter(cluster == selected_cluster())) +
      annotate_pitch(colour = "white", fill = "gray") +
      stat_density_2d_filled(aes(x = LOCATIONX, y = LOCATIONY), alpha = 0.7, contour_var = "ndensity") +
      theme_pitch() +
      scale_fill_viridis_d(option = "magma") +
      labs(title = paste("Startpositioner - Cluster", selected_cluster()))
  })
  
  output$heatmap_end <- renderPlot({
    ggplot(allpasses %>% filter(cluster == selected_cluster())) +
      annotate_pitch(colour = "white", fill = "gray") +
      stat_density_2d_filled(aes(x = POSSESSIONENDLOCATIONX, y = POSSESSIONENDLOCATIONY), alpha = 0.7, contour_var = "ndensity") +
      theme_pitch() +
      scale_fill_viridis_d(option = "magma") +
      labs(title = paste("Endpositioner - Cluster", selected_cluster()))
  })
  
  output$plotly_cluster <- renderPlotly({
    cluster_id <- as.numeric(selected_cluster())
    df <- player_stats %>% filter(main_cluster == cluster_id)
    
    if (input$plotly_type == "default") {
      plot_ly(data = df, x = ~AVG_X_start, y = ~AVG_Y_start, z = ~avg_pass_angle, type = "scatter3d", mode = "markers",
              color = ~as.factor(main_cluster), text = ~paste0("Player: ", SHORTNAME, "<br>", "Total passes: ", total_passes, "<br>", "Avg pass length: ", round(avg_LENGTH,1), "<br>", "Pass acc: ", round(pass_acc,1), "%<br>", "Role: ", ROLENAME), hoverinfo = "text")
    } else if (input$plotly_type == "tla") {
      plot_ly(data = df, x = ~total_passes, y = ~avg_LENGTH, z = ~avg_pass_angle, type = "scatter3d", mode = "markers",
              color = ~as.factor(main_cluster), text = ~paste0("Player: ", SHORTNAME, "<br>", "Total passes: ", total_passes, "<br>", "Avg pass length: ", round(avg_LENGTH,1), "<br>", "Avg pass angle: ", round(avg_pass_angle,1), "<br>", "Role: ", ROLENAME), hoverinfo = "text")
    } else {
      plot_ly(data = df, x = ~total_passes, y = ~sd_pass_lenght, z = ~avg_pass_angle, type = "scatter3d", mode = "markers",
              color = ~as.factor(main_cluster), text = ~paste0("Player: ", SHORTNAME, "<br>", "Total passes: ", total_passes, "<br>", "Sd pass length: ", round(sd_pass_lenght,1), "<br>", "Avg pass angle: ", round(avg_pass_angle,1), "<br>", "Role: ", ROLENAME), hoverinfo = "text")
    }
  })
  
  output$cluster_title_box <- renderUI({
    tagList(
      tags$h4(cluster_labels[[selected_cluster()]]$title),
      tags$p(cluster_labels[[selected_cluster()]]$desc)
    )
  })
  
  output$cluster_summary_box <- renderUI({
    cl_data <- clusters %>% filter(cluster == selected_cluster())
    
    avg_accuracy <- allpasses %>%
      filter(cluster == selected_cluster()) %>%
      summarise(pass_acc = mean(ACCURATE, na.rm = TRUE) * 100) %>%
      pull(pass_acc)
    
    tagList(
      tags$div(
        style = "background-color: #f7f7f7; padding: 10px; border-radius: 5px;",
        HTML(
          paste0(
            "<h4>Cluster ", selected_cluster(), " info</h4>",
            "<p><strong>Gennemsnitlig længde: </strong>", round(cl_data$Length,1), "</p>",
            "<p><strong>Gns. vinkel: </strong>", round(cl_data$Angle,1), "</p>",
            "<p><strong>Gns. spillerens passes pr kamp: </strong>", round(cl_data$Player_avgpass,1), "</p>",
            "<p><strong>Antal pass i cluster: </strong>", cl_data$count, "</p>",
            "<p><strong>Gennemsnitlig præcision: </strong>", round(avg_accuracy, 1), "%</p>"
          )
        )
      )
    )
  })
}

###########################
#       KØR APPEN         #
###########################
shinyApp(ui, server)


