library(ggsoccer)
library(skimr)
library(ggimage)
library(tidyverse)
#### beskrivende statistik ####


# Vis som tabel
kable(stat_table, digits = 2, caption = "Beskrivende statistik for valgte variable")

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

# distance
ggplot(allshotevents, aes(x = shot_distance)) +
  geom_histogram(fill = "darkorange") +
  labs(x = "Afstand til mål", y = "Tæthed", title = "Fordeling af skuddistance") +
  theme_minimal()


goal_x <- 100
goal_y <- 50
radii <- c(5, 10, 20, 30, 50)
colors <- c("#00296b", "#003f88", "#00509d", "#fdc500", "#ffd500")

# Funktion til halvcirkel-koordinater
make_semicircle <- function(radius, center_x = goal_x, center_y = goal_y, n = 300) {
  angles <- seq(-pi/2, pi/2, length.out = n)
  data.frame(
    x = center_x - radius * cos(angles),
    y = center_y + radius * sin(angles)
  )
}

# Generér alle cirkler
circles <- purrr::map2_dfr(radii, colors, ~{
  make_semicircle(.x) %>%
    mutate(group = .x, color = .y)
})

# Plot
ggplot(allshotevents, aes(x = LOCATIONX, y = LOCATIONY)) +
  annotate_pitch(dimensions = pitch_wyscout, colour = "WHITE", fill = "gray") +
  
  # Bin-baseret heatmap
  geom_bin2d(binwidth = c(2, 2), aes(fill = after_stat(count)), alpha = 0.6) +
  
  # Afstandscirkler i farver
  geom_path(data = circles, aes(x = x, y = y, group = group, color = color), linewidth = 1, alpha = 0.8) +
  geom_text(
    data = circles %>% group_by(group, color) %>% slice(1),  # ét label pr. cirkel
    aes(x = x, y = y, label = paste0(group, "m"), color = color),
    fontface = "bold",
    size = 2.5,
    vjust = -0.5,
    hjust = 0.8  # flyt teksten lidt til højre
  ) +
  scale_color_identity() +
  
  # Farveskala og styling
  scale_fill_viridis_c(option = "A", direction = -1) +
  
  coord_flip(xlim = c(50, 100), ylim = c(0, 100)) +
  theme_pitch() +
  labs(
    title = "Placering af skud – blokvis",
    fill = "Antal skud"
  ) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# Kropsdel
ggplot(allshotevents, aes(x = SHOTBODYPART)) +
  geom_bar(fill = "mediumseagreen") +
  labs(x = "Kropsdel", y = "Antal skud", title = "Fordeling af skud pr. kropsdel") +
  theme_minimal()

# Team_Ranking
# Aggreger antal skud pr. hold
team_shots <- allshotevents %>%
  group_by(TEAMNAME.x, Team_Ranking, IMAGEDATAURL) %>%
  summarise(total_shots = n(), .groups = "drop") %>%
  mutate(label = paste0(TEAMNAME.x, " (#", Team_Ranking, ")")) %>%
  arrange(Team_Ranking)  # Så vi ved 1 er øverst

# Plot: sortér label så 1 er øverst (reorder + desc)
ggplot(team_shots, aes(x = reorder(label, -Team_Ranking), y = total_shots)) +
  geom_col(fill = "lightgray", color = "black", width = 0.8) +
  geom_image(aes(image = IMAGEDATAURL), size = 0.06, asp = 1.2) +
  coord_flip() +
  labs(
    title = "Antal skud pr. hold (lavere ranking øverst)",
    x = NULL,
    y = "Antal skud"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.y = element_text(face = "bold"),
    axis.text.x = element_text(size = 12),
    legend.position = "none"
  )



# overall
ggplot(allshotevents, aes(x = overall)) +
  geom_histogram(fill = "mediumseagreen") +
  labs(x = "overall", y = "Antal skud", title = "Fordeling af skud pr. overall") +
  theme_minimal()

# potential
ggplot(allshotevents, aes(x = potential)) +
  geom_histogram(fill = "mediumseagreen") +
  labs(x = "potential", y = "Antal skud", title = "Fordeling af skud pr. potential") +
  theme_minimal()

# POSSESSIONEVENTSNUMBER
ggplot(allshotevents, aes(x = POSSESSIONEVENTSNUMBER)) +
  geom_histogram(fill = "mediumseagreen") +
  labs(x = "POSSESSIONEVENTSNUMBER", y = "Antal skud", title = "Fordeling af skud pr. POSSESSIONEVENTSNUMBER") +
  theme_minimal()

# POSSESSIONEVENTINDEX
ggplot(allshotevents, aes(x = POSSESSIONEVENTINDEX)) +
  geom_histogram(fill = "mediumseagreen") +
  labs(x = "POSSESSIONEVENTINDEX", y = "Antal skud", title = "Fordeling af skud pr. POSSESSIONEVENTINDEX") +
  theme_minimal()

# POSSESSIONDURATION
ggplot(allshotevents, aes(x = POSSESSIONDURATION)) +
  geom_histogram(fill = "mediumseagreen") +
  labs(x = "POSSESSIONDURATION", y = "Antal skud", title = "Fordeling af skud pr. POSSESSIONDURATION") +
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


#### Models ####
# sinuglar tree plot
simple_tree_plot <- rpart.plot(tree_model, type = 2, extra = 104, fallen.leaves = TRUE,
                               box.palette = "RdYlGn", main = "Beslutningstræ (singular)")

# RF ntree loop
rf_ntree_loop <- ggplot(auc_df, aes(x = ntree, y = AUC)) +
  geom_line() +
  geom_point() +
  labs(title = "AUC bliver stabil ved 5000 træer",
       x = "Number of Trees",
       y = "AUC") +
  theme_minimal()
