library(ggsoccer)
library(skimr)
library(ggimage)
library(tidyverse)
#### beskrivende statistik ####
dbListTables(con)
allmatch_raw <- dbReadTable(con, "wyscout_matchdetail_base_sl") 


allshotevents <- allshotevents %>%
  mutate(MATCH_WYID = MATCH_WYID.x)

# Join med kamp-info
allshotevents_matches <- allshotevents %>%
  left_join(allmatch_raw, by = "MATCH_WYID")

allshotevents_matches <- allshotevents_matches %>% filter(MATCH_WYID == 5465920)

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
mean_angle <- mean(allshotevents$shot_angle, na.rm = TRUE)

ggplot(allshotevents, aes(x = shot_angle)) +
  geom_histogram(
    bins = 30,
    fill = "#0D1C8A",  # dyb blå
    color = "white",
    alpha = 0.9
  ) +
  geom_vline(
    xintercept = mean_angle,
    color = "#FDBA21",  # gul
    linewidth = 1.3
  ) +
  annotate("text",
           x = mean_angle + 2,
           y = Inf,
           label = paste0("Gennemsnitlig vinkel: ", round(mean_angle, 1), "°"),
           vjust = 2,
           hjust = -0.1,
           color = "#FDBA21",
           fontface = "bold",
           size = 3.5) +
  labs(
    x = "Vinkel mod mål (grader)",
    y = "Antal skud",
    title = "Fordeling af skudvinkler"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


# Målstolper (bruges som pile-endepunkter)
goal_left <- c(x = 100, y = 44.285)
goal_right <- c(x = 100, y = 55.715)

# Dine skud med forskellige vinkler
vinkel_eksempler <- data.frame(
  label = c("Skarp vinkel (7°)", "Gennemsnitlig vinkel (35°)", "Åben vinkel (78°)"),
  x = c(69, 91, 93),
  y = c(91, 60, 50)
)

ggplot() +
  annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "gray90") +
  
  # Linjer fra bolden til begge målstolper, med farve pr. label
  geom_segment(data = vinkel_eksempler,
               aes(x = x, y = y, xend = goal_left["x"], yend = goal_left["y"], color = label),
               arrow = arrow(length = unit(0.15, "cm"))) +
  geom_segment(data = vinkel_eksempler,
               aes(x = x, y = y, xend = goal_right["x"], yend = goal_right["y"], color = label),
               arrow = arrow(length = unit(0.15, "cm"))) +
  
  # Punkt for selve skuddet
  geom_point(data = vinkel_eksempler, aes(x = x, y = y, color = label), size = 4) +
  
  # Labels for hvert skud
  geom_text(data = vinkel_eksempler, aes(x = x - 1.5, y = y, label = label, color = label),
            hjust = 1, fontface = "bold", show.legend = FALSE) +
  
  coord_flip(xlim = c(65, 105), ylim = c(0, 100)) +
  theme_pitch() +
  labs(title = "Visuel sammenligning af skudvinkler", color = NULL) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"  # Slår legenden fra, kan sættes til "right" hvis ønsket
  )




# distance
mean_dist <- mean(allshotevents$shot_distance, na.rm = TRUE)

ggplot(allshotevents, aes(x = shot_distance)) +
  geom_histogram(
    bins = 30,
    fill = "#0D1C8A",  # dyb blå
    color = "white",
    alpha = 0.9
  ) +
  geom_vline(
    xintercept = mean_dist,
    color = "#FDBA21",  # gul
    linewidth = 1.3
  ) +
  annotate("text",
           x = mean_dist + 2,
           y = Inf,
           label = paste0("Gennemsnitlig afstand: ", round(mean_dist, 1), "m"),
           vjust = 2,
           hjust = -0.1,
           color = "#FDBA21",
           fontface = "bold",
           size = 3.5) +
  labs(
    x = "Afstand til mål (meter)",
    y = "Antal skud",
    title = "Fordeling af skuddistance"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )



mean_dist <- mean(allshotevents$shot_distance, na.rm = TRUE)

goal_x <- 100
goal_y <- 50
radii <- c(5, 10, mean_dist, 30, 50)
colors <- c("#00296b", "#003f88", "#FF5733", "#fdc500", "#ffd500")  # Brug tydelig farve til gennemsnit

# Funktion til halvcirkel-koordinater
make_semicircle <- function(radius, center_x = goal_x, center_y = goal_y, n = 300) {
  angles <- seq(-pi/2, pi/2, length.out = n)
  data.frame(
    x = center_x - radius * cos(angles),
    y = center_y + radius * sin(angles)
  )
}

# Tilføj linjetype (dotted for faste, solid for gennemsnit)
linetypes <- ifelse(radii == mean_dist, "solid", "solid")

# Generér alle cirkler med farve og linetype
circles <- purrr::pmap_dfr(list(r = radii, col = colors, lt = linetypes), function(r, col, lt) {
  make_semicircle(r) %>%
    mutate(group = r, color = col, linetype = lt)
})

# Plot
ggplot(allshotevents, aes(x = LOCATIONX, y = LOCATIONY)) +
  annotate_pitch(dimensions = pitch_wyscout, colour = "WHITE", fill = "gray") +
  
  # Bin-baseret heatmap
  geom_bin2d(binwidth = c(2, 2), aes(fill = after_stat(count)), alpha = 0.4) +
  
  # Afstandscirkler
  geom_path(data = circles, aes(x = x, y = y, group = group, color = color, linetype = linetype),
            linewidth = 1, alpha = 0.8) +
  
  # Labels til cirkler
  geom_text(
    data = circles %>% group_by(group, color) %>% slice(1),
    aes(x = x, y = y, label = paste0(round(group, 1), "m"), color = color),
    fontface = "bold",
    size = 2.5,
    vjust = -0.5,
    hjust = 0.8
  ) +
  scale_color_identity() +
  scale_linetype_identity() +
  
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



# overall og potential
# Sørg for at allplayers_raw har unikke spiller-ID'er
allplayers_roles <- allplayers_raw %>%
  select(PLAYER_WYID, ROLENAME) %>%
  distinct(PLAYER_WYID, .keep_all = TRUE)

allshotevents <- allshotevents %>%
  left_join(allplayers_roles, by = "PLAYER_WYID")
allshotevents <- allshotevents %>%
  distinct(EVENT_WYID, .keep_all = TRUE)


# Sørg for at overall findes i allshotevents – ellers merge fra spillerdata først
# Lav rating bins (eksempelvis i trin á 5)
# Fjern NA'er og lav rating-bins
allshotevents_clean <- allshotevents %>%
  filter(!is.na(overall), !is.na(ROLENAME.y)) %>%
  mutate(overall_bin = cut(overall, breaks = seq(50, 90, by = 5), include.lowest = TRUE))

# Plot
ggplot(allshotevents_clean, aes(x = overall_bin, fill = ROLENAME.y)) +
  geom_bar(position = "dodge") +  # viser andel pr. bin
  labs(
    title = "Fordeling af spillernes roller på tværs af overalls",
    x = "Overall rating",
    y = "Antal skud",
    fill = "Rolle"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )




allshotevents_clean <- allshotevents %>%
  filter(!is.na(overall), !is.na(ROLENAME.y)) %>%
  mutate(overall_bin = cut(overall, breaks = seq(50, 90, by = 5), include.lowest = TRUE))

# Beregn andel pr. rolle i hver bin
rolle_fordeling <- allshotevents_clean %>%
  count(overall_bin, ROLENAME.y) %>%
  group_by(overall_bin) %>%
  mutate(procent = n / sum(n)) %>%
  ungroup()


ggplot(rolle_fordeling, aes(x = overall_bin, y = procent, fill = ROLENAME.y)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(
    title = "Fordeling af spillernes roller på tværs af overalls",
    subtitle = "Angibere er relativt ligeligt fordel",
    x = "Overall rating (i bins af 5)",
    y = "Andel spillere",
    fill = "Rolle"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "italic", size = 12)
  )


# Bin ratings
allshotevents_binned <- allshotevents %>%
  mutate(
    overall_bin = cut(overall, breaks = seq(50, 90, by = 5), right = FALSE),
    potential_bin = cut(potential, breaks = seq(50, 95, by = 5), right = FALSE)
  )

# Tæl skud per bin
overall_counts <- allshotevents_binned %>%
  count(overall_bin) %>%
  mutate(type = "Overall", bin = overall_bin) %>%
  select(bin, n, type)

potential_counts <- allshotevents_binned %>%
  count(potential_bin) %>%
  mutate(type = "Potential", bin = potential_bin) %>%
  select(bin, n, type)

# Combine data
binned_df <- bind_rows(overall_counts, potential_counts)

# -- Gennemsnit --
mean_overall <- mean(allshotevents$overall, na.rm = TRUE)
mean_potential <- mean(allshotevents$potential, na.rm = TRUE)

# -- Konverter bin labels til numeriske midtpunkter for placering --
binned_df <- binned_df %>%
  mutate(
    bin_mid = as.character(bin) %>%
      gsub("\\[|\\)", "", .) %>%
      strsplit(",") %>%
      sapply(function(x) mean(as.numeric(x)))
  )


# Plot med gennemsnitslinjer
ggplot(binned_df, aes(x = bin_mid, y = n, fill = type)) +
  geom_col(position = "dodge", color = "black", width = 4.5) +
  geom_vline(xintercept = mean_overall, color = "lightgray", linewidth = 1.5, linetype = "dashed") +
  geom_vline(xintercept = mean_potential, color = "darkgray", linewidth = 1.5, linetype = "dashed") +
  annotate("text", x = mean_overall - 6, y = max(binned_df$n + 10),
           label = paste0("Gns. overall: ", round(mean_overall, 1)),
           color = "#0D1C8A", fontface = "bold", hjust = 0) +
  annotate("text", x = mean_potential + 0.5, y = max(binned_df$n) + 40,
           label = paste0("Gns. potential: ", round(mean_potential, 1)),
           color = "#FDBA21", fontface = "bold", hjust = 0) +
  scale_fill_manual(values = c("Overall" = "#0D1C8A", "Potential" = "#FDBA21")) +
  scale_x_continuous(breaks = seq(50, 95, by = 5)) +
  labs(
    title = "Fordeling af skud baseret på spiller-rating",
    subtitle = "Inkl. gennemsnitlig rating for Overall og Potential",
    x = "FIFA-rating (bin-midtpunkt)",
    y = "Antal skud",
    fill = "Ratingtype"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(margin = margin(b = 10))
  )


# potential
ggplot(allshotevents, aes(x = potential)) +
  geom_histogram(fill = "mediumseagreen") +
  labs(x = "potential", y = "Antal skud", title = "Fordeling af skud pr. potential") +
  theme_minimal()


# Team_Ranking
# Træk unikke kombinationer
team_ranking_df <- allshotevents %>%
  select(TEAM_WYID, Team_Ranking, TEAMNAME.x) %>%
  distinct()

# Join logoer på
team_ranking_df <- team_ranking_df %>%
  left_join(all_teams_2324 %>% select(TEAM_WYID, IMAGEDATAURL), by = "TEAM_WYID")

ggplot(team_ranking_df, aes(x = reorder(TEAMNAME.x, Team_Ranking), y = Team_Ranking)) +
  geom_col(fill = "steelblue", color = "black") +
  geom_image(aes(image = IMAGEDATAURL), size = 0.06, asp = 1.2) +
  coord_flip() +
  labs(
    title = "Holdenes styrker ifølge FIFAIndex",
    subtitle = "Baseret på FIFA's Team Rating – lavere værdi = stærkere hold",
    x = NULL,
    y = "FIFA Team Rating"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic"),
    axis.text.y = element_text(face = "bold"),
    legend.position = "none"
  )



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


# Beregn konverteringsrate i procent
conversion_data <- allshotevents %>%
  group_by(SHOTISGOAL) %>%
  summarise(antal = n()) %>%
  mutate(
    kategori = ifelse(SHOTISGOAL == 1, "Mål", "Ikke mål"),
    procent = round(antal / sum(antal) * 100, 1)
  )

#### Mål ####
ggplot() +
  annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "gray95") +
  
  # Først: ikke-mål (grå)
  geom_point(data = subset(allshotevents, SHOTISGOAL == 0),
             aes(x = LOCATIONX, y = LOCATIONY, color = "Ikke mål"), alpha = 0.5) +
  
  # Så: mål (rød)
  geom_point(data = subset(allshotevents, SHOTISGOAL == 1),
             aes(x = LOCATIONX, y = LOCATIONY, color = "Mål"), alpha = 0.8) +
  
  scale_color_manual(
    name = "Skudresultat",
    values = c("Ikke mål" = "#002F6C", "Mål" = "#F9E300")
  ) +
  
  coord_flip(xlim = c(50, 100), ylim = c(0, 100)) +
  theme_pitch() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )

ggplot(conversion_data, aes(x = kategori, y = procent, fill = kategori)) +
  geom_col(color = "black", width = 0.6) +
  scale_fill_manual(values = c("Ikke mål" = "#002F6C", "Mål" = "#F9E300")) +
  geom_text(aes(label = paste0(procent, "%")), vjust = -0.5, size = 5, fontface = "bold") +
  ylim(0, 100) +
  labs(
    title = "Meget stor bias til ikke-mål",
    x = NULL,
    y = "Andel af alle skud (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none"
  )



#### Distance heatmap ####
# -- Beregn gennemsnitsafstande (brug shot_distance)
mean_goal_dist <- allshotevents %>%
  filter(SHOTISGOAL == 1) %>%
  summarise(m = mean(shot_distance, na.rm = TRUE)) %>%
  pull(m)

mean_nogoal_dist <- allshotevents %>%
  filter(SHOTISGOAL == 0) %>%
  summarise(m = mean(shot_distance, na.rm = TRUE)) %>%
  pull(m)

# -- Halvcirkelfunktion
make_semicircle <- function(radius, center_x = 100, center_y = 50, n = 300) {
  angles <- seq(-pi/2, pi/2, length.out = n)
  data.frame(
    x = center_x - radius * cos(angles),
    y = center_y + radius * sin(angles)
  )
}

# -- Generér de 2 halvcirkler + labels
goal_circle <- make_semicircle(mean_goal_dist) %>%
  mutate(type = "Gns. afstand (mål)", color = "#ffd500")

nogoal_circle <- make_semicircle(mean_nogoal_dist) %>%
  mutate(type = "Gns. afstand (ikke mål)", color = "#00296b")

circles_avg <- bind_rows(goal_circle, nogoal_circle)

# -- Tilføj labels ét sted pr. cirkel
circle_labels <- circles_avg %>%
  group_by(type, color) %>%
  slice(1) %>%
  mutate(label = type)

# -- Plot
ggplot(allshotevents, aes(x = LOCATIONX, y = LOCATIONY)) +
  annotate_pitch(dimensions = pitch_wyscout, colour = "white", fill = "gray") +
  
  # Blokheatmap
  geom_bin2d(binwidth = c(2, 2), aes(fill = after_stat(count)), alpha = 0.5) +
  
  # Cirkel-linjer
  geom_path(data = circles_avg, aes(x = x, y = y, group = type, color = color), linewidth = 1.2) +
  
  # Labels på banen
  geom_text(data = circle_labels %>% 
              mutate(y = ifelse(label == "Gns. afstand (mål)", y + 25, y - 25)),
            aes(x = x, y = y, label = label, color = color),
            fontface = "bold", size = 6,vjust = -0.4, hjust = 0) +

  
  scale_color_identity() +
  scale_fill_viridis_c(option = "A", direction = -1) +
  
  coord_flip(xlim = c(50, 100), ylim = c(0, 100)) +
  theme_pitch() +
  labs(
    title = "Klar tendens til, at en lavere afstand giver flere mål",
    fill = "Antal skud"
  ) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))




#### Models ####
# sinuglar tree plot
simple_tree_plot <- rpart.plot(simple_tree, type = 2, extra = 104, fallen.leaves = TRUE,
                               box.palette = "RdYlGn", main = "Beslutningstræ (singular)")

# RF ntree loop
# Sørg for at data er i stigende rækkefølge
auc_df <- auc_df[order(auc_df$ntree), ]

# Marker bedste model
best_row <- auc_df[which.max(auc_df$AUC), ]

ggplot(auc_df, aes(x = ntree, y = AUC)) +
  geom_line(color = "#00509d", size = 1.2) +
  geom_point(color = "#fdc500", size = 3) +
  
  # Tilføj alle labels undtagen 10 og 100
  geom_text(data = subset(auc_df, ntree > 100),
            aes(label = ntree), 
            vjust = -1, fontface = "bold", color = "gray20", size = 4) +
  
  # Tilføj labels for 10 og 100 og ryk dem lidt til højre
  geom_text(data = subset(auc_df, ntree <= 100),
            aes(label = ntree),
            vjust = -1, hjust = -0.2, fontface = "bold", color = "gray20", size = 4) +
  
  # Fremhæv den bedste model
  geom_point(data = best_row, aes(x = ntree, y = AUC), 
             color = "darkgreen", size = 4) +
  geom_text(
    data = best_row,
    aes(label = paste0("Bedste AUC: ", round(AUC, 4))),
    hjust = 0.1, vjust = 2.3, fontface = "bold", color = "darkgreen"
  ) +
  
  labs(
    title = "Den bedste AUC findes ved 1000 træer",
    subtitle = "AUC-score for Random Forest med mtry = 2",
    x = "Antal træer (ntree)",
    y = "AUC-score"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )



# ROC
#Beregn ROC-objekt
rf_roc <- roc(response = test_data_yn$SHOTISGOAL, predictor = rf_test)
tree_roc <- roc(response = test_data_$SHOTISGOAL, predictor = tree_probs)



# Træk sensitivitet og 1-specificitet ud til brug i ggplot
rf_df <- data.frame(
  FPR = 1 - rf_roc$specificities,
  TPR = rf_roc$sensitivities
)
tree_df <- data.frame(
  FPR = 1 - tree_roc$specificities,
  TPR = tree_roc$sensitivities
)

# Simpelt ROC-plot
ggplot() +
  geom_line(data = tree_df, aes(x = FPR, y = TPR), 
            color = "#999999", size = 1.5) +
  geom_line(data = rf_df, aes(x = FPR, y = TPR), 
            color = "#00509d", size = 1.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray60") +
  
  # Tekst for AUC
  annotate("text", x = 0.65, y = 0.3, 
           label = paste0("Random Forest AUC: ", round(auc(rf_roc), 4)),
           color = "#00509d", fontface = "bold", size = 5) +
  annotate("text", x = 0.65, y = 0.2, 
           label = paste0("Beslutningstræ AUC: ", round(auc(tree_roc), 4)),
           color = "#999999", fontface = "bold", size = 5) +
  
  labs(
    title = "Random Forest har markant bedre til at skelne mellem skud udfaldet",
    subtitle = "Begge modeller evalueret på testdata",
    x = "False Positive Rate",
    y = "True Positive Rate"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold")
  )



# ROC-objekter
rf_roc   <- roc(response = test_data_yn$SHOTISGOAL, predictor = rf_test)
tree_roc <- roc(response = test_data_yn$SHOTISGOAL, predictor = tree_probs)
xgb_roc  <- roc(response = test_data_yn$SHOTISGOAL, predictor = xgb_pred)
glm_roc  <- roc(response = test_data_yn$SHOTISGOAL, predictor = glm_probs)

# Sensitivitet / Specificitet dataframes
rf_df   <- data.frame(FPR = 1 - rf_roc$specificities, TPR = rf_roc$sensitivities)
tree_df <- data.frame(FPR = 1 - tree_roc$specificities, TPR = tree_roc$sensitivities)
xgb_df  <- data.frame(FPR = 1 - xgb_roc$specificities, TPR = xgb_roc$sensitivities)
glm_df  <- data.frame(FPR = 1 - glm_roc$specificities, TPR = glm_roc$sensitivities)

# Plot
ggplot() +
  geom_line(data = tree_df, aes(x = FPR, y = TPR), color = "#999999", size = 1.3) +
  geom_line(data = rf_df,   aes(x = FPR, y = TPR), color = "#00509d", size = 1.3) +
  geom_line(data = xgb_df,  aes(x = FPR, y = TPR), color = "#FDBA21", size = 1.3) +
  geom_line(data = glm_df,  aes(x = FPR, y = TPR), color = "#4682B4", size = 1.3) +
  
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray60") +
  
  # Tekst: AUC
  annotate("text", x = 0.65, y = 0.45,
           label = paste0("GLM AUC: ", round(auc(glm_roc), 4)),
           color = "#4682B4", fontface = "bold", size = 5) +
  annotate("text", x = 0.65, y = 0.38,
           label = paste0("XGBoost AUC: ", round(auc(xgb_roc), 4)),
           color = "#FDBA21", fontface = "bold", size = 5) +
  annotate("text", x = 0.65, y = 0.31,
           label = paste0("Random Forest AUC: ", round(auc(rf_roc), 4)),
           color = "#00509d", fontface = "bold", size = 5) +
  annotate("text", x = 0.65, y = 0.24,
           label = paste0("Beslutningstræ AUC: ", round(auc(tree_roc), 4)),
           color = "#999999", fontface = "bold", size = 5) +
  
  labs(
    title = "XGBoost er den mest frokalrende model",
    subtitle = "Evalueret på testdatasæt",
    x = "False Positive Rate",
    y = "True Positive Rate"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text  = element_text(face = "bold")
  )

# --- Gini Importance fra beslutningstræ (rpart) ---
importance_tree <- simple_tree$variable.importance

# --- Gini Importance fra Random Forest (caret wrapper til ranger eller randomForest) ---
importance_rf <- varImp(rf_model_final)$importance

# --- Gini Importance fra XGBoost ---
importance_xgb <- xgb.importance(model = xgb_model$finalModel)


# -- Decision Tree (rpart) --
importance_tree <- simple_tree$variable.importance %>% 
  enframe(name = "Variable", value = "Importance") %>%
  mutate(Model = "Decision Tree")

importance_rf <- importance_rf %>%
  rownames_to_column("Variable") %>%
  mutate(Importance = rowMeans(across(c("no", "yes")))) %>%
  select(Variable, Importance) %>%
  mutate(Model = "Random Forest")


# -- XGBoost (xgb.importance) --
importance_xgb <- importance_xgb 

# Normalisér alle modeller så deres importance summerer til 100
normalize_importance <- function(df) {
  df %>%
    mutate(Importance = Importance / sum(Importance) * 100)
}

# Gør det for hver model
importance_tree_norm <- normalize_importance(importance_tree)
importance_rf_norm <- normalize_importance(importance_rf)
importance_xgb_norm <- normalize_importance(importance_xgb)

# Bind dem sammen
importance_combined <- bind_rows(
  importance_tree_norm,
  importance_rf_norm,
  importance_xgb_norm
)

importance_combined <- importance_combined %>%
  mutate(Variable = case_when(
    Variable == "shot_distance" ~ "Afstand til mål",
    Variable == "shot_angle" ~ "Vinkel mod mål",
    Variable == "SHOTBODYPART.head_or_other" ~ "Hoved eller andet",
    Variable == "SHOTBODYPART.left_foot" ~ "Venstre fod",
    Variable == "SHOTBODYPART.right_foot" ~ "Højre fod",
    TRUE ~ Variable
  ))


# -- Plot --
ggplot(importance_combined, aes(x = reorder(Variable, Importance), y = Importance, fill = Model)) +
  geom_col(position = "dodge", color = "black") +
  coord_flip() +
  labs(
    title = "Afstand og vinkel, er altid de mest vigtigste",
    x = NULL,
    y = "Gini Importance"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  scale_fill_manual(values = c("Decision Tree" = "#a1d99b", "Random Forest" = "#fc9272", "XGBoost" = "#9ecae1"))


ggplot(allshotevents, aes(x = LOCATIONX, y = LOCATIONY)) +
  annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "white") +
  geom_point(aes(size = xG_XGB, color = xG_XGB), alpha = 0.6) +
  scale_color_gradient(low = "#0D1C8A", high = "#FDBA21") +
  scale_size(range = c(1.5, 6)) +
  coord_fixed(xlim = c(0, 100), ylim = c(0, 100)) +
  theme_pitch() +
  labs(
    title = "xGs størrelse udvikler sig som forventet ift. afstand til mål",
    subtitle = "Størrelse og farve repræsenterer xG-værdien",
    color = "xG"
  ) +
  guides(size = "none") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5)
  )

ggplot(
  allshotevents %>% arrange(SHOTISGOAL),
  aes(x = LOCATIONX, y = LOCATIONY)
) +
  annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "white") +
  geom_point(
    aes(
      color = factor(SHOTISGOAL),
      alpha = ifelse(SHOTISGOAL == 1, 0.1, 0.8)
    ),
    size = 3
  ) +
  scale_color_manual(
    values = c("0" = "#0D1C8A", "1" = "#FDBA21"),
    labels = c("0" = "Ikke mål", "1" = "Mål"),
    name = "Resultat"
  ) +
  scale_alpha_identity() +  # Brug præcis den alpha vi angiver
  coord_fixed(xlim = c(0, 100), ylim = c(0, 100)) +
  theme_pitch() +
  labs(
    title = "Skud i Superligen sæson 2023/2024",
    subtitle = "Klart størstedelen af alle skud der blev til mål, blev gjort tættere ved målet"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5),
    legend.position = "top"
  )





# ROC-objekter
xgb_roc     <- roc(response = test_data_yn$SHOTISGOAL, predictor = xgb_pred)
wyscout_roc <- roc(response = test_data$SHOTISGOAL, predictor = test_data$SHOTXG)

# Dataframes til plotting
xgb_df     <- data.frame(FPR = 1 - xgb_roc$specificities, TPR = xgb_roc$sensitivities)
wyscout_df <- data.frame(FPR = 1 - wyscout_roc$specificities, TPR = wyscout_roc$sensitivities)

# Plot
ggplot() +
  geom_line(data = xgb_df, aes(x = FPR, y = TPR), color = "#FDBA21", size = 1.5) +
  geom_line(data = wyscout_df, aes(x = FPR, y = TPR), color = "#2C3E50", linetype = "dotdash", size = 1.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray60") +
  
  # AUC-annotering
  annotate("text", x = 0.65, y = 0.42,
           label = paste0("XGBoost AUC: ", round(auc(xgb_roc), 4)),
           color = "#FDBA21", fontface = "bold", size = 5) +
  annotate("text", x = 0.65, y = 0.35,
           label = paste0("WyScout AUC: ", round(auc(wyscout_roc), 4)),
           color = "#2C3E50", fontface = "bold", size = 5) +
  
  labs(
    title = "XGBoost er kun lidt mere præcis i test",
    subtitle = "Evalueret på testdatasættet",
    x = "False Positive Rate",
    y = "True Positive Rate"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5),
    axis.title    = element_text(face = "bold"),
    axis.text     = element_text(face = "bold")
  )

# Sørg for at SHOTISGOAL er factor eller character
allshotevents$SHOTISGOAL <- as.factor(allshotevents$SHOTISGOAL)

# Beregn gennemsnit og standardafvigelse for hver model, opdelt på mål/ikke-mål
xg_stats <- allshotevents %>%
  group_by(SHOTISGOAL) %>%
  summarise(
    Mean_SHOTXG = mean(SHOTXG, na.rm = TRUE),
    SD_SHOTXG = sd(SHOTXG, na.rm = TRUE),
    Mean_xG_XGB = mean(xG_XGB, na.rm = TRUE),
    SD_xG_XGB = sd(xG_XGB, na.rm = TRUE),
    .groups = "drop"
  )

# Se resultatet
view(xg_stats)




# 1. Lav team_summary df
team_summary <- allshotevents %>%
  group_by(TEAMNAME.x) %>%
  summarise(
    TeamRanking = mean(Team_Ranking, na.rm = TRUE),
    TotalShots = n(),
    UniqueGames = n_distinct(MATCH_WYID.x),
    AvgShotsPerGame = round(TotalShots / UniqueGames, 2),
    ImageURL = first(IMAGEDATAURL),
    .groups = "drop"
  ) %>%
  mutate(Label = paste0(TEAMNAME.x, " (", round(TeamRanking, 1), ")")) %>%
  arrange(TeamRanking)  # sortér efter laveste ranking

# 2. Plot: Total antal skud (sorteret efter ranking)
ggplot(team_summary, aes(x = TotalShots, y = factor(Label, levels = Label))) +
  geom_col(fill = "#FDBA21", color = "black") +
  geom_image(aes(image = ImageURL), size = 0.07, asp = 1.5, by = "height", x = 0) +
  labs(
    title = "Total antal skud pr. hold",
    x = "Antal skud",
    y = "Hold (ranking)"
  ) +
  theme_minimal(base_size = 14)

# 3. Plot: Gennemsnitlige skud pr. kamp (samme sortering)
ggplot(team_summary, aes(x = AvgShotsPerGame, y = factor(Label, levels = Label))) +
  geom_col(fill = "#4682B4", color = "black") +
  geom_image(aes(image = ImageURL), size = 0.07, asp = 1.5, by = "height", x = 0) +
  labs(
    title = "Der er ingen klar sammenhæng, mellem holdets rankering og antal skud i en kamp",
    x = "Skud pr. kamp",
    y = "Hold (ranking)"
  ) +
  theme_minimal(base_size = 14)



