library(ggsoccer)
library(skimr)
#### beskrivende statistik ####
# Location 
ggplot(allshotevents, aes(x = LOCATIONX, y = LOCATIONY)) +
  annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "white") +
  geom_point(alpha = 0.6, color = "black") +
  theme_pitch() +
  coord_flip(xlim = c(50, 100), ylim = c(0, 100)) +
  labs(title = "Placering af skud på banen")

ggplot(allshotevents, aes(x = LOCATIONX, y = LOCATIONY)) +
  annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "white") +
  stat_density_2d(aes(fill = after_stat(density)), 
                  geom = "raster", 
                  contour = FALSE, 
                  alpha = 0.8) +
  scale_fill_viridis_c(option = "C") +
  theme_pitch() +
  labs(title = "Heatmap over skudpositioner")


# shot_angle
ggplot(allshotevents, aes(x = shot_angle)) +
  geom_histogram(fill = "steelblue") +
  labs(x = "Skudvinkel (grader)", y = "Tæthed", title = "Fordeling af skudvinkler") +
  theme_minimal()

ggplot(allshotevents, aes(x = shot_distance)) +
  geom_histogram(fill = "darkorange") +
  labs(x = "Afstand til mål", y = "Tæthed", title = "Fordeling af skuddistance") +
  theme_minimal()

ggplot(allshotevents, aes(x = SHOTBODYPART)) +
  geom_bar(fill = "mediumseagreen") +
  labs(x = "Kropsdel", y = "Antal skud", title = "Fordeling af skud pr. kropsdel") +
  theme_minimal()



skim(select(allshotevents, shot_angle, shot_distance, LOCATIONX, LOCATIONY, SHOTBODYPART))


# avg shot
# Gennemsnitlige værdier
mean_x <- 85.8
mean_y <- 49.3
shot_distance <- 18.7
shot_angle <- 34

# Mål
goal_x <- 100
goal_y_center <- 50
goal_half_width <- 11.43 / 2
goal_y_left <- goal_y_center - goal_half_width
goal_y_right <- goal_y_center + goal_half_width

ggplot() +
  annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "white") +
  
  # Rød prik = gennemsnitligt skud
  geom_point(aes(x = mean_x, y = mean_y), size = 3, color = "red") +
  
  # Tekst under prik
  geom_text(aes(x = mean_x, y = mean_y - 2), 
            label = paste0("(", mean_x, ", ", mean_y, ")"), 
            color = "red", size = 3.5) +
  
  # Vinkellinjer
  geom_segment(aes(x = mean_x, y = mean_y, xend = goal_x, yend = goal_y_left),
               linetype = "dashed", color = "darkred") +
  geom_segment(aes(x = mean_x, y = mean_y, xend = goal_x, yend = goal_y_right),
               linetype = "dashed", color = "darkred") +
  
  # Vinkel label
  geom_text(aes(x = mean_x + 3, y = mean_y + 6),
            label = paste0("Vinkel ≈ ", shot_angle, "°"),
            color = "darkred", size = 3.5) +
  
  # Pil til målcenter (afstand)
  geom_segment(aes(x = mean_x, y = mean_y, xend = goal_x, yend = goal_y_center),
               color = "black", arrow = arrow(length = unit(0.15, "inches"))) +
  
  # Afstand label
  geom_text(aes(x = (mean_x + goal_x) / 2, y = (mean_y + goal_y_center) / 2 - 2),
            label = paste0("Afstand ≈ ", shot_distance, "%"),
            color = "black", size = 3.5) +
  
  coord_cartesian(xlim = c(80, 101), ylim = c(35, 65)) +
  theme_pitch() +
  labs(title = "Gennemsnitlig skudposition med vinkel og afstand")


# for only goals
allshotevents_goals <- allshotevents %>% 
  filter(SHOTISGOAL == "1")

skim(select(allshotevents_goals, shot_angle, shot_distance, LOCATIONX, LOCATIONY, SHOTBODYPART))


#### Models ####
# sinuglar tree plot
simple_tree_plot <- rpart.plot(tree_model, type = 2, extra = 104, fallen.leaves = TRUE,
           box.palette = "RdYlGn", main = "Beslutningstræ (singular)")

# Gennemsnit for mål
mean_x_goal <- 90.2
mean_y_goal <- 49.6
shot_distance_goal <- 12.8
shot_angle_goal <- 53.1

# Målplacering
goal_x <- 100
goal_y_center <- 50
goal_half_width <- 11.43 / 2
goal_y_left <- goal_y_center - goal_half_width
goal_y_right <- goal_y_center + goal_half_width

ggplot() +
  annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "white") +
  
  # Prik for gennemsnitligt målskud
  geom_point(aes(x = mean_x_goal, y = mean_y_goal), size = 3, color = "forestgreen") +
  
  # Tekst under prik
  geom_text(aes(x = mean_x_goal, y = mean_y_goal - 2), 
            label = paste0("(", mean_x_goal, ", ", mean_y_goal, ")"), 
            color = "forestgreen", size = 3.5) +
  
  # Vinkellinjer
  geom_segment(aes(x = mean_x_goal, y = mean_y_goal, xend = goal_x, yend = goal_y_left),
               linetype = "dashed", color = "darkgreen") +
  geom_segment(aes(x = mean_x_goal, y = mean_y_goal, xend = goal_x, yend = goal_y_right),
               linetype = "dashed", color = "darkgreen") +
  
  # Vinkel label
  geom_text(aes(x = mean_x_goal + 3, y = mean_y_goal + 6),
            label = paste0("Vinkel ≈ ", shot_angle_goal, "°"),
            color = "darkgreen", size = 3.5) +
  
  # Afstand (pil til center)
  geom_segment(aes(x = mean_x_goal, y = mean_y_goal, xend = goal_x, yend = goal_y_center),
               color = "black", arrow = arrow(length = unit(0.15, "inches"))) +
  
  # Afstand label
  geom_text(aes(x = (mean_x_goal + goal_x) / 2, y = (mean_y_goal + goal_y_center) / 2 - 2),
            label = paste0("Afstand ≈ ", shot_distance_goal, " m"),
            color = "black", size = 3.5) +
  
  coord_cartesian(xlim = c(85, 101), ylim = c(35, 65)) +
  theme_pitch() +
  labs(title = "Gennemsnitlig skudposition – kun mål")


# RF ntree loop
rf_ntree_loop <- ggplot(auc_df, aes(x = ntree, y = AUC)) +
  geom_line() +
  geom_point() +
  labs(title = "AUC bliver stabil ved 5000 træer",
       x = "Number of Trees",
       y = "AUC") +
  theme_minimal()
