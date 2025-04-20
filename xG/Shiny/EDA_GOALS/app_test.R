library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggsoccer)
library(viridis)
library(purrr)
library(tidyverse)
library(ggimage)
library(gridExtra)


allshotevents <- readRDS("allshotevents_Shiny_EDA_1.rds")
train_data <- readRDS("train_data.rds")
test_data <- readRDS("test_data.rds")


# -- Farver til body_parts -----------------------------------------------------------
body_colors <- c(
  "head_or_other" = "#FDBA21",  # gul
  "left_foot" = "#0D1C8A",      # blå
  "right_foot" = "#FC7753"      # rødlig
)


# Plot
ggplot(allshotevents, aes(x = LOCATIONX, y = LOCATIONY)) +
  annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "white") +
  geom_point(aes(color = SHOTBODYPART), alpha = 0.25, size = 3) +
  scale_color_manual(values = body_colors) +
  coord_flip(xlim = c(50, 100), ylim = c(0, 100)) +
  theme_pitch() +
  labs(title = "Skudpositioner opdelt efter kropsdel", color = "Kropsdel") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )

# Antag at allshotevents er din dataframe
brondby_data <- allshotevents %>%
  filter(TEAMNAME.x == "Brøndby") %>%
  count(SHOTISGOAL, name = "antal") %>%
  mutate(procent = 100 * antal / sum(antal))

# Vis resultatet
print(brondby_data)

# -- UI -----------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Måldata i Superligaen"),
  dashboardSidebar(
    sidebarMenu(
      id = "plot_choice",
      menuItem("📘 Introduktion", tabName = "intro", icon = icon("info-circle")),
      menuItem("Skudposition", tabName = "location_points", icon = icon("crosshairs")),
      menuItem("Skudvinkel", tabName = "shot_angle", icon = icon("angle-right")),
      menuItem("Afstand til mål", tabName = "shot_distance", icon = icon("ruler-horizontal")),
      menuItem("Kropsdel", tabName = "body_part", icon = icon("running")),
      menuItem("Team Ranking", tabName = "team_ranking", icon = icon("sort-amount-down-alt")),
      menuItem("Spiller-rating", tabName = "player_rating", icon = icon("chart-bar")),
      menuItem("Antal events i possession", tabName = "possession_events", icon = icon("list-ol")),
      menuItem("Index for possession", tabName = "possession_index", icon = icon("fingerprint")),
      menuItem("Varighed af possession", tabName = "possession_duration", icon = icon("clock")),
      br(),
      checkboxInput("split", "Vis opdelt på træning/test", value = FALSE),
      checkboxInput("avg_per_game", "Vis gennemsnit pr. kamp", value = FALSE)
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "shortcut icon", href = "favicon.ico")
    ),
    tabItems(
      # Intro-tab
      tabItem(tabName = "intro",
              h2("Velkommen til skuddata-dashboardet"),
              p("Denne applikation giver dig mulighed for at udforske skuddata fra Superligaens 2023/2024-sæson med særligt fokus på, hvilke faktorer der adskiller mål fra ikke-mål."),
              p("Ved at undersøge fordelingen af skud ud fra forskellige variabler som afstand, vinkel og kropsdel – opdelt på om afslutningen førte til mål eller ej – kan du få indsigt i, hvad der øger sandsynligheden for en succesfuld afslutning."),
              tags$ul(
                tags$li("Brug menuen i venstre side til at vælge én variabel ad gangen."),
                tags$li("Tjek 'Vis gennemsnit pr. kamp' for at se mønstre baseret på holdniveau."),
                tags$li("Tjek 'Vis opdelt på træning/test' for at se forskelle i dine splits."),
                tags$li("Hver variabel vises som plot, en forklarende konklusion og en oversigtstabel.")
              ),
              br(),
              h3("Hvad viser de forskellige variabler?"),
              tags$ul(
                tags$li(strong("Skudposition:"), " Hvor på banen afslutningerne bliver taget fra."),
                tags$li(strong("Skudvinkel:"), " I hvilken vinkel spilleren skyder mod målet."),
                tags$li(strong("Afstand til mål:"), " Hvor langt der er fra spilleren til målet ved afslutning."),
                tags$li(strong("Kropsdel:"), " Hvilken kropsdel spilleren bruger til at afslutte."),
                tags$li(strong("Team Ranking:"), " Holdenes placering i ligaen sidste sæson."),
                tags$li(strong("Spiller-rating:"), " Den individuelle spiller-rating fra FIFA-data."),
                tags$li(strong("Antal events i possession:"), " Hvor mange 'aktioner'events' der er i et angreb før afslutning."),
                tags$li(strong("Index for possession:"), " Hvornår i kampen possessionen (angrebet) finder sted."),
                tags$li(strong("Varighed af possession:"), " Hvor mange sekunder possessionen varer før afslutning.")
              ),
              br(),
              p("God fornøjelse!")
      )
      ,
      
      # Din eksisterende visualisering
      tabItem(tabName = "location_points",
              fluidRow(
                box(title = "Plot", width = 8, solidHeader = TRUE, status = "primary",
                    uiOutput("plots_ui")),
                box(title = "Konklusion", width = 4, solidHeader = TRUE, status = "info",
                    textOutput("conclusion_text"))
              ),
              fluidRow(
                conditionalPanel(
                  condition = "input.plot_choice == 'location_points'",
                  box(title = "Oversigt over skud fordelt på baneområder", width = 12, solidHeader = TRUE, status = "warning",
                      tableOutput("position_area_table"))
                )
              ),
              fluidRow(
                conditionalPanel(
                  condition = "input.plot_choice == 'shot_distance'",
                  box(title = "Afstandslinjer på banen", width = 12, solidHeader = TRUE, status = "warning",
                      plotOutput("distance_lines_plot", height = "300px"))
                )
              ),
              fluidRow(
                conditionalPanel(
                  condition = "input.plot_choice == 'shot_angle'",
                  box(title = "Visuel forklaring på skudvinkler", width = 12, solidHeader = TRUE, status = "warning",
                      plotOutput("shot_angle_visual", height = "400px"))
                )
              ),
              fluidRow(
                conditionalPanel(
                  condition = "input.plot_choice == 'body_part'",
                  
                  # Først tabellen
                  box(title = "Afstande fordelt på kropsdel", width = 12, solidHeader = TRUE, status = "warning",
                      tableOutput("body_distance_table")),
                  
                  # Så plot
                  box(title = "Skudpositioner fordelt på kropsdel", width = 12, solidHeader = TRUE, status = "warning",
                      plotOutput("body_location_plot", height = "350px"))
                )
              ),
              fluidRow(
                conditionalPanel(
                  condition = "input.plot_choice == 'team_ranking'",
                  box(title = "Oversigt over holdenes skudstatistik", width = 12, solidHeader = TRUE, status = "warning",
                      htmlOutput("team_ranking_summary_ui"))
                )
              ),
              fluidRow(
                conditionalPanel(
                  condition = "input.plot_choice == 'player_rating'",
                  
                  # Tabel med statistik
                  box(title = "Gennemsnit og variation i spiller-ratings", width = 12, solidHeader = TRUE, status = "info",
                      tableOutput("rating_table"))
                )
              ),
              fluidRow(
                conditionalPanel(
                  condition = "input.plot_choice == 'possession_events'",
                  box(title = "Opsummerende statistik for antal events i possession", 
                      width = 12, solidHeader = TRUE, status = "warning",
                      tableOutput("possession_events_table"))
                )
              ),
              fluidRow(
                conditionalPanel(
                  condition = "input.plot_choice == 'possession_index'",
                  box(
                    title = "Beskrivende statistik for possession index",
                    width = 12,
                    solidHeader = TRUE,
                    status = "warning",
                    tableOutput("possession_index_table")
                  )
                )
              ),
              fluidRow(
                conditionalPanel(
                  condition = "input.plot_choice == 'possession_duration'",
                  
                  # -- Tabel-boks til Wyscout-opdeling --
                  box(
                    title = "Opsummerende statistik opdelt efter Wyscout-varighedskategorier",
                    width = 12,
                    solidHeader = TRUE,
                    status = "warning",
                    tableOutput("duration_summary")
                  )
                )
              )
              
              
              
              
      ))))


# -- Konklusionstekster -------------------------------------------
get_conclusion <- function(var) {
  switch(var,
         "location_points" = "",
         "shot_angle" = "",
         "shot_distance" = "",
         "body_part" = "",
         "team_ranking" = "",
         "player_rating" = "",
         "possession_events" = "",
         "possession_index" = "",
         "possession_duration" = "",
         "Ukendt valg"
  )
}

# -- Plotfunktion -------------------------------------------------
make_plot <- function(data, var, avg_on) {
  switch(var,
         
         # Punktplot
         "location_points" = {
           ggplot(
             allshotevents %>% arrange(SHOTISGOAL),
             aes(x = LOCATIONX, y = LOCATIONY)
           ) +
             annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "white") +
             geom_point(
               aes(
                 color = factor(SHOTISGOAL),
                 alpha = ifelse(SHOTISGOAL == 1, 0.2, 0.8)
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
           
         }
         ,
         
         
         # Heatmap
         "location_heatmap" = {
           ggplot(data, aes(x = LOCATIONX, y = LOCATIONY)) +
             annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "white") +
             stat_density_2d(aes(fill = after_stat(density)), 
                             geom = "raster", contour = FALSE, alpha = 0.8) +
             scale_fill_viridis_c(option = "C") +
             theme_pitch() +
             labs(title = "Heatmap over skudpositioner")
         },
         
         # shot angle
         "shot_angle" = {
           breaks <- c(seq(0, 90, by = 10), Inf)
           labels <- c(paste0("[", seq(0, 80, by = 10), ",", seq(10, 90, by = 10), ")"), "[90,+)")
           
           if (avg_on) {
             data_summary <- data %>%
               group_by(MATCH_WYID.x, SHOTISGOAL) %>%
               summarise(mean_angle = mean(shot_angle, na.rm = TRUE), .groups = "drop") %>%
               mutate(angle_bin = cut(mean_angle, breaks = breaks, labels = labels, right = FALSE)) %>%
               filter(!is.na(angle_bin))
             
             binned_data <- data_summary %>%
               group_by(angle_bin, SHOTISGOAL) %>%
               summarise(count = n(), .groups = "drop") %>%
               group_by(angle_bin) %>%
               mutate(total = sum(count),
                      percentage = 100 * count / total)
             
             ggplot(binned_data, aes(x = angle_bin, y = percentage, fill = factor(SHOTISGOAL))) +
               geom_col(position = position_dodge(width = 0.8), width = 0.7) +
               geom_text(aes(label = sprintf("%.1f%%", percentage)),
                         position = position_dodge(width = 0.8),
                         vjust = -0.3, size = 3) +
               scale_fill_manual(values = c("0" = "#FDBA21", "1" = "#0D1C8A"),
                                 labels = c("Ikke mål", "Mål")) +
               scale_y_continuous(limits = c(0, 100)) +
               labs(
                 x = "Gns. skudvinkel pr. kamp (grader)",
                 y = "Procent (%)",
                 fill = "Udfald",
                 title = "Skudvinkel – Procentvis fordeling pr. vinkelgruppe"
               ) +
               theme_minimal(base_size = 12) +
               theme(
                 plot.title = element_text(hjust = 0.5, face = "bold"),
                 axis.text.x = element_text(angle = 45, hjust = 1)
               )
           } else {
             data <- data %>%
               mutate(angle_bin = cut(shot_angle, breaks = breaks, labels = labels, right = FALSE)) %>%
               filter(!is.na(angle_bin))
             
             binned_data <- data %>%
               group_by(angle_bin, SHOTISGOAL) %>%
               summarise(count = n(), .groups = "drop") %>%
               group_by(angle_bin) %>%
               mutate(total = sum(count),
                      percentage = 100 * count / total)
             
             ggplot(binned_data, aes(x = angle_bin, y = percentage, fill = factor(SHOTISGOAL))) +
               geom_col(position = position_dodge(width = 0.8), width = 0.7) +
               geom_text(aes(label = sprintf("%.1f%%", percentage)),
                         position = position_dodge(width = 0.8),
                         vjust = -0.3, size = 3) +
               scale_fill_manual(values = c("0" = "#FDBA21", "1" = "#0D1C8A"),
                                 labels = c("Ikke mål", "Mål")) +
               scale_y_continuous(limits = c(0, 100)) +
               labs(
                 x = "Skudvinkel (grader)",
                 y = "Procent (%)",
                 fill = "Udfald",
                 title = "Skudvinkel – Procentvis fordeling pr. vinkelgruppe"
               ) +
               theme_minimal(base_size = 12) +
               theme(
                 plot.title = element_text(hjust = 0.5, face = "bold"),
                 axis.text.x = element_text(angle = 45, hjust = 1)
               )
           }
         }
         ,
         
         # Shot distance
         # Afstand til mål
         "shot_distance" = {
           breaks <- c(seq(0, 45, by = 5), Inf)
           labels <- c(paste0("[", seq(0, 40, by = 5), ",", seq(5, 45, by = 5), ")"), "[45,+)")
           
           if (avg_on) {
             data_summary <- data %>%
               group_by(MATCH_WYID.x, SHOTISGOAL) %>%
               summarise(mean_dist = mean(shot_distance, na.rm = TRUE), .groups = "drop") %>%
               mutate(dist_bin = cut(mean_dist, breaks = breaks, labels = labels, right = FALSE)) %>%
               filter(!is.na(dist_bin))
             
             binned_data <- data_summary %>%
               group_by(dist_bin, SHOTISGOAL) %>%
               summarise(count = n(), .groups = "drop") %>%
               group_by(dist_bin) %>%
               mutate(total = sum(count),
                      percentage = 100 * count / total)
             
             ggplot(binned_data, aes(x = dist_bin, y = percentage, fill = factor(SHOTISGOAL))) +
               geom_col(position = position_dodge(width = 0.8), width = 0.7) +
               geom_text(aes(label = sprintf("%.1f%%", percentage)),
                         position = position_dodge(width = 0.8),
                         vjust = -0.3, size = 3) +
               scale_fill_manual(values = c("0" = "#FDBA21", "1" = "#0D1C8A"),
                                 labels = c("Ikke mål", "Mål")) +
               scale_y_continuous(limits = c(0, 100)) +
               labs(
                 x = "Gns. skudafstand pr. kamp (meter)",
                 y = "Procent (%)",
                 fill = "Udfald",
                 title = "Skudafstand – Procentvis fordeling pr. afstandsgruppe"
               ) +
               theme_minimal(base_size = 12) +
               theme(
                 plot.title = element_text(hjust = 0.5, face = "bold"),
                 axis.text.x = element_text(angle = 45, hjust = 1)
               )
           } else {
             data <- data %>%
               mutate(dist_bin = cut(shot_distance, breaks = breaks, labels = labels, right = FALSE)) %>%
               filter(!is.na(dist_bin))
             
             binned_data <- data %>%
               group_by(dist_bin, SHOTISGOAL) %>%
               summarise(count = n(), .groups = "drop") %>%
               group_by(dist_bin) %>%
               mutate(total = sum(count),
                      percentage = 100 * count / total)
             
             ggplot(binned_data, aes(x = dist_bin, y = percentage, fill = factor(SHOTISGOAL))) +
               geom_col(position = position_dodge(width = 0.8), width = 0.7) +
               geom_text(aes(label = sprintf("%.1f%%", percentage)),
                         position = position_dodge(width = 0.8),
                         vjust = -0.3, size = 3) +
               scale_fill_manual(values = c("0" = "#FDBA21", "1" = "#0D1C8A"),
                                 labels = c("Ikke mål", "Mål")) +
               scale_y_continuous(limits = c(0, 100)) +
               labs(
                 x = "Skudafstand (meter)",
                 y = "Procent (%)",
                 fill = "Udfald",
                 title = "Skudafstand – Procentvis fordeling pr. afstandsgruppe"
               ) +
               theme_minimal(base_size = 12) +
               theme(
                 plot.title = element_text(hjust = 0.5, face = "bold"),
                 axis.text.x = element_text(angle = 45, hjust = 1)
               )
           }
         },
         
         
         
         
         # Body part
         "body_part" = {
           if (avg_on) {
             data %>%
               mutate(SHOTISGOAL = factor(SHOTISGOAL, levels = c(1, 0), labels = c("Mål", "Ikke mål"))) %>%
               group_by(MATCH_WYID.x, SHOTBODYPART, SHOTISGOAL) %>%
               summarise(count = n(), .groups = "drop") %>%
               group_by(SHOTBODYPART, SHOTISGOAL) %>%
               summarise(avg = mean(count), .groups = "drop") %>%
               ggplot(aes(x = SHOTBODYPART, y = avg, fill = SHOTISGOAL)) +
               geom_col(position = position_dodge()) +
               scale_fill_manual(values = c("Mål" = "#FDBA21", "Ikke mål" = "#0D1C8A")) +
               labs(x = "Kropsdel", y = "Gennemsnit pr. kamp", title = "Skud pr. kropsdel (gennemsnit)", fill = "Resultat") +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
             
           } else {
             data %>%
               mutate(SHOTISGOAL = factor(SHOTISGOAL, levels = c(1, 0), labels = c("Mål", "Ikke mål"))) %>%
               count(SHOTBODYPART, SHOTISGOAL, name = "antal") %>%
               group_by(SHOTBODYPART) %>%
               mutate(procent = 100 * antal / sum(antal)) %>%
               ungroup() %>%
               ggplot(aes(x = SHOTBODYPART, y = procent, fill = SHOTISGOAL)) +
               geom_col(position = position_dodge()) +
               scale_fill_manual(values = c("Mål" = "#FDBA21", "Ikke mål" = "#0D1C8A")) +
               labs(x = "Kropsdel", y = "Andel (%)", title = "Skud pr. kropsdel (fordelt procentvis)", fill = "Resultat") +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
           }
           
         },
         
         # Team ranking (med logo og labels)
         "team_ranking" = {
           if (avg_on) {
             data %>%
               group_by(TEAMNAME.x, Team_Ranking, IMAGEDATAURL, MATCH_WYID.x) %>%
               summarise(shots = n(), .groups = "drop") %>%
               group_by(TEAMNAME.x, Team_Ranking, IMAGEDATAURL) %>%
               summarise(avg_shots = mean(shots), .groups = "drop") %>%
               mutate(label = paste0(TEAMNAME.x, " (#", Team_Ranking, ")")) %>%
               arrange(Team_Ranking) %>%
               ggplot(aes(x = reorder(label, -Team_Ranking), y = avg_shots)) +
               geom_col(fill = "steelblue", color = "black") +
               geom_image(aes(image = IMAGEDATAURL), size = 0.06, asp = 1.2) +
               coord_flip() +
               labs(x = NULL, y = "Gennemsnit pr. kamp", title = "Skud pr. hold pr. kamp") +
               theme_minimal(base_size = 16)
             
           } else {
             df_plot <- data %>%
               mutate(SHOTISGOAL = factor(SHOTISGOAL, levels = c(1, 0), labels = c("Mål", "Ikke mål"))) %>%
               count(TEAMNAME.x, Team_Ranking, IMAGEDATAURL, SHOTISGOAL, name = "antal") %>%
               group_by(TEAMNAME.x) %>%
               mutate(procent = 100 * antal / sum(antal)) %>%
               ungroup() %>%
               mutate(label = paste0(TEAMNAME.x, " (#", Team_Ranking, ")")) %>%
               arrange(Team_Ranking)
             
             # Målrate-data til tekst
             målrate_df <- df_plot %>%
               filter(SHOTISGOAL == "Mål") %>%
               select(label, Team_Ranking, procent) %>%
               mutate(label_text = paste0(round(procent, 1), "%"))
             
             ggplot(df_plot, aes(x = reorder(label, -Team_Ranking), y = procent, fill = SHOTISGOAL)) +
               geom_col(position = position_dodge(width = 0.8), color = "black") +
               
               # Tekst med målrate (%)
               geom_text(
                 data = målrate_df,
                 aes(
                   x = reorder(label, -Team_Ranking),
                   y = 1.5,
                   label = label_text
                 ),
                 inherit.aes = FALSE,
                 hjust = 0,
                 size = 4.5,
                 fontface = "bold",
                 color = "white"
               ) +
               
               # Logoer
               geom_image(
                 data = df_plot %>% distinct(label, IMAGEDATAURL, Team_Ranking),
                 aes(x = reorder(label, -Team_Ranking), y = 95, image = IMAGEDATAURL),
                 inherit.aes = FALSE,
                 size = 0.06,
                 asp = 1.2
               ) +
               
               coord_flip(ylim = c(0, 110)) +
               scale_fill_manual(values = c("Mål" = "#FDBA21", "Ikke mål" = "#0D1C8A")) +
               labs(x = NULL, y = "Andel (%)", 
                    title = "Andel mål vs. ikke mål pr. hold", 
                    subtitle = "Det hvide tal, viser holdets målrate",
                    fill = "Resultat") +
               theme_minimal(base_size = 16) +
               theme(
                 plot.title = element_text(hjust = 0.5, face = "bold")
               )
           }
           
           
         },
         
         "player_rating" = {
           # Opsætning: målrate og rating-type
           data_ratings <- data %>%
             mutate(SHOTISGOAL = factor(SHOTISGOAL, levels = c(1, 0), labels = c("Mål", "Ikke mål")))
           
           # Funktion til at lave binning + procent
           bin_rating_data <- function(df, rating_col) {
             df %>%
               select(rating = {{ rating_col }}, SHOTISGOAL) %>%
               drop_na() %>%
               mutate(bin = cut(rating, breaks = seq(40, 100, by = 5), right = FALSE)) %>%
               group_by(bin, SHOTISGOAL) %>%
               summarise(count = n(), .groups = "drop") %>%
               group_by(bin) %>%
               mutate(procent = 100 * count / sum(count))
           }
           
           # Lav datasæt for begge
           df_overall <- bin_rating_data(data_ratings, overall)
           df_potential <- bin_rating_data(data_ratings, potential)
           
           # Gennemsnit til vertikale linjer 
           mean_overall <- mean(data$overall, na.rm = TRUE)
           mean_potential <- mean(data$potential, na.rm = TRUE)
           
           # Plot 1: Overall
           plot_overall <- ggplot(df_overall, aes(x = bin, y = procent, fill = SHOTISGOAL)) +
             geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "white") +
             scale_fill_manual(values = c("Mål" = "#FDBA21", "Ikke mål" = "#0D1C8A")) +
             labs(
               title = "Skudfordeling – Overall-rating",
               subtitle = "Andel mål/ikke mål pr. rating-bin",
               x = "Overall-rating", y = "Andel (%)", fill = "Resultat"
             ) +
             theme_minimal() +
             theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                   plot.subtitle = element_text(hjust = 0.5))
           
           # Plot 2: Potential
           plot_potential <- ggplot(df_potential, aes(x = bin, y = procent, fill = SHOTISGOAL)) +
             geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "white") +
             scale_fill_manual(values = c("Mål" = "#FDBA21", "Ikke mål" = "#0D1C8A")) +
             labs(
               title = "Skudfordeling – Potential-rating",
               subtitle = "Andel mål/ikke mål pr. rating-bin",
               x = "Potential-rating", y = "Andel (%)", fill = "Resultat"
             ) +
             theme_minimal() +
             theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                   plot.subtitle = element_text(hjust = 0.5))
           
           # Kombinér plots
           gridExtra::grid.arrange(plot_overall, plot_potential, ncol = 2)
         }
         
         ,
         
         
         
         # Possession events
         "possession_events" = {
           # Definér bin-grænser og labels
           breaks <- c(0, 5, 10, 15, 20, 30, 50, Inf)
           labels <- c("0–4", "5–9", "10–14", "15–19", "20–29", "30–49", "50+")
           
           data_binned <- data %>%
             filter(!is.na(POSSESSIONEVENTSNUMBER)) %>%
             mutate(SHOTISGOAL = factor(SHOTISGOAL, levels = c(1, 0), labels = c("Mål", "Ikke mål")),
                    events_bin = cut(POSSESSIONEVENTSNUMBER, breaks = breaks, labels = labels, right = FALSE)) %>%
             group_by(events_bin, SHOTISGOAL) %>%
             summarise(count = n(), .groups = "drop") %>%
             group_by(events_bin) %>%
             mutate(procent = 100 * count / sum(count))
           
           # Lav plottet
           ggplot(data_binned, aes(x = events_bin, y = procent, fill = SHOTISGOAL)) +
             geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "white") +
             geom_text(aes(label = sprintf("%.1f%%", procent)),
                       position = position_dodge(width = 0.8),
                       vjust = -0.3, size = 3) +
             scale_fill_manual(values = c("Mål" = "#FDBA21", "Ikke mål" = "#0D1C8A")) +
             labs(
               title = "Mål vs. ikke mål fordelt efter antal events i possession",
               subtitle = "",
               x = "Antal events i angreb (binned)",
               y = "Andel (%)",
               fill = "Resultat"
             ) +
             theme_minimal(base_size = 13) +
             theme(
               plot.title = element_text(hjust = 0.5, face = "bold"),
               plot.subtitle = element_text(hjust = 0.5),
               axis.text.x = element_text(face = "bold")
             )
         }
         ,
         
         
         # Possession index
         "possession_index" = {
           # -- Definér binning --
           breaks <- c(0, 5, 10, 15, 20, 30, 40, Inf)
           labels <- c("0–4", "5–9", "10–14", "15–19", "20–29", "30–39", "40+")
           
           data_binned <- data %>%
             filter(!is.na(POSSESSIONEVENTINDEX)) %>%
             mutate(
               SHOTISGOAL = factor(SHOTISGOAL, levels = c(1, 0), labels = c("Mål", "Ikke mål")),
               index_bin = cut(POSSESSIONEVENTINDEX, breaks = breaks, labels = labels, right = FALSE)
             ) %>%
             group_by(index_bin, SHOTISGOAL) %>%
             summarise(count = n(), .groups = "drop") %>%
             group_by(index_bin) %>%
             mutate(procent = 100 * count / sum(count))
           
           # -- Plot --
           ggplot(data_binned, aes(x = index_bin, y = procent, fill = SHOTISGOAL)) +
             geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "white") +
             geom_text(aes(label = sprintf("%.1f%%", procent)),
                       position = position_dodge(width = 0.8),
                       vjust = -0.3, size = 3) +
             scale_fill_manual(values = c("Mål" = "#FDBA21", "Ikke mål" = "#0D1C8A")) +
             labs(
               title = "Hvornår i kampen bliver der afsluttet?",
               subtitle = "Skud fordelt på hvor sent i kampen possession startede",
               x = "Possession index i kampen (binned)",
               y = "Andel (%)",
               fill = "Resultat"
             ) +
             theme_minimal(base_size = 13) +
             theme(
               plot.title = element_text(hjust = 0.5, face = "bold"),
               plot.subtitle = element_text(hjust = 0.5),
               axis.text.x = element_text(face = "bold")
             )
         }
         ,
         
         
         
         # Possession duration
         "possession_duration" = {
           # -- Definér Wyscout-bins --
           data_binned <- data %>%
             filter(!is.na(POSSESSIONDURATION)) %>%
             mutate(
               SHOTISGOAL = factor(SHOTISGOAL, levels = c(1, 0), labels = c("Mål", "Ikke mål")),
               duration_bin = cut(
                 POSSESSIONDURATION,
                 breaks = c(0, 10, 20, 45, Inf),
                 labels = c("Short (0–10s)", "Medium (10–20s)", "Long (20–45s)", "Very long (45s+)"),
                 right = FALSE
               )
             ) %>%
             group_by(duration_bin, SHOTISGOAL) %>%
             summarise(count = n(), .groups = "drop") %>%
             group_by(duration_bin) %>%
             mutate(procent = 100 * count / sum(count))
           
           # -- Plot --
           ggplot(data_binned, aes(x = duration_bin, y = procent, fill = SHOTISGOAL)) +
             geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "white") +
             geom_text(
               aes(label = sprintf("%.1f%%", procent)),
               position = position_dodge(width = 0.8),
               vjust = -0.3, size = 3
             ) +
             scale_fill_manual(values = c("Mål" = "#FDBA21", "Ikke mål" = "#0D1C8A")) +
             labs(
               title = "Varighed af angrebet før afslutning",
               subtitle = "Fordeling af skud opdelt efter possessionens længde",
               x = "Varighed (sekunder, Wyscout-kategorier)",
               y = "Andel (%)",
               fill = "Resultat"
             ) +
             theme_minimal(base_size = 13) +
             theme(
               plot.title = element_text(hjust = 0.5, face = "bold"),
               plot.subtitle = element_text(hjust = 0.5),
               axis.text.x = element_text(face = "bold")
             )
         }
         
         
         
         
  )
}


# -- Server -------------------------------------------------------
server <- function(input, output, session) {
  
  observe({
    updateTabItems(session, "plot_choice", selected = "intro")
  })
  
  output$plots_ui <- renderUI({
    if (input$split) {
      fluidRow(
        column(6, plotOutput("plot_train")),
        column(6, plotOutput("plot_test"))
      )
    } else {
      plotOutput("plot_combined")
    }
  })
  
  
  output$plot_combined <- renderPlot({
    make_plot(allshotevents, input$plot_choice, input$avg_per_game)
  })
  
  output$plot_train <- renderPlot({
    make_plot(train_data, input$plot_choice, input$avg_per_game) + ggtitle("Træningsdata")
  })
  
  output$plot_test <- renderPlot({
    make_plot(test_data, input$plot_choice, input$avg_per_game) + ggtitle("Testdata")
  })
  
  output$position_area_table <- renderTable({
    allshotevents %>%
      mutate(
        område = case_when(
          LOCATIONX > 94 & LOCATIONY >= 37 & LOCATIONY <= 63 ~ "Målfelt",
          LOCATIONX > 84 & LOCATIONY >= 19 & LOCATIONY <= 81 ~ "Store felt",
          TRUE ~ "Uden for feltet"
        ),
        SHOTISGOAL = factor(SHOTISGOAL, levels = c(1, 0), labels = c("Mål", "Ikke mål"))
      ) %>%
      count(område, SHOTISGOAL, name = "Antal skud") %>%
      group_by(SHOTISGOAL) %>%
      mutate(
        `Andel (%)` = round(100 * `Antal skud` / sum(`Antal skud`), 1)
      ) %>%
      ungroup() %>%
      arrange(factor(område, levels = c("Målfelt", "Store felt", "Uden for feltet")),
              SHOTISGOAL)
  })
  
  
  
  
  # distance lines
  output$distance_lines_plot <- renderPlot({
    goal_x <- 100
    goal_y <- 50
    radii <- c(5, 10, 15)
    colors <- c("#00296b", "#003f88", "#00509d")  # Farver til cirklerne
    
    make_semicircle <- function(radius, center_x = goal_x, center_y = goal_y, n = 300) {
      angles <- seq(-pi/2, pi/2, length.out = n)
      data.frame(
        x = center_x - radius * cos(angles),
        y = center_y + radius * sin(angles)
      )
    }
    
    # Lav cirkeldata
    circles <- purrr::pmap_dfr(list(r = radii, col = colors), function(r, col) {
      make_semicircle(r) %>%
        mutate(group = r, color = col)
    })
    
    label_data <- circles %>%
      group_by(group, color) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(label = paste0(group, "m"))
    
    # Tegn pitch + cirkler + skud
    ggplot() +
      annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "white") +
      
      # Skud: mål/ikke-mål
      geom_point(
        data = allshotevents %>% arrange(SHOTISGOAL),
        aes(x = LOCATIONX, y = LOCATIONY, color = factor(SHOTISGOAL),
            alpha = ifelse(SHOTISGOAL == 1, 0.2, 0.8)),
        size = 3
      ) +
      
      # Cirkler og labels
      geom_path(data = circles, aes(x = x, y = y, group = group, color = color), linewidth = 1) +
      geom_text(data = label_data, aes(x = x, y = y, label = label, color = color),
                vjust = -0.5, hjust = 0.8, fontface = "bold", size = 3.5) +
      
      scale_color_manual(
        values = c("0" = "#0D1C8A", "1" = "#FDBA21", colors),  # Farver til skud og cirkler
        labels = c("0" = "Ikke mål", "1" = "Mål"),
        name = "Resultat"
      ) +
      scale_alpha_identity() +
      coord_flip(xlim = c(50, 100), ylim = c(0, 100)) +
      theme_pitch() +
      labs(
        title = "Skud og afstandscirkler i Superligaen 2023/2024",
        subtitle = "Klart størstedelen af alle skud der blev til mål, blev gjort tættere ved målet"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "italic"),
        legend.position = "top"
      )
  })
  
  
  
  
  
  
  output$conclusion_text <- renderText({
    get_conclusion(input$plot_choice)
  })
  
  output$shot_angle_visual <- renderPlot({
    goal_left <- c(x = 100, y = 44.285)
    goal_right <- c(x = 100, y = 55.715)
    
    vinkel_eksempler <- data.frame(
      vinkel_label = c("12°", "33°", "97°"),
      x = c(88, 86, 95),
      y = c(27, 59, 49)
    )
    
    ggplot() +
      annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "gray90") +
      
      # Pile til stolper (venstre + højre) med farver
      geom_segment(data = vinkel_eksempler,
                   aes(x = x, y = y, xend = goal_left["x"], yend = goal_left["y"], color = vinkel_label),
                   arrow = arrow(length = unit(0.15, "cm"))) +
      geom_segment(data = vinkel_eksempler,
                   aes(x = x, y = y, xend = goal_right["x"], yend = goal_right["y"], color = vinkel_label),
                   arrow = arrow(length = unit(0.15, "cm"))) +
      
      # Skudpunkter
      geom_point(data = vinkel_eksempler, aes(x = x, y = y, color = vinkel_label), size = 4) +
      
      coord_flip(xlim = c(80, 105), ylim = c(0, 100)) +
      theme_pitch() 
  })
  
  output$body_location_plot <- renderPlot({
    plot_data <- allshotevents %>%
      mutate(body_group = case_when(
        SHOTBODYPART == "head_or_other" ~ "Hoved/andet",
        SHOTBODYPART %in% c("left_foot", "right_foot") ~ "Ben"
      ))
    
    ggplot(plot_data, aes(x = LOCATIONX, y = LOCATIONY, fill = body_group)) +
      annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "white") +
      geom_bin2d(binwidth = c(2, 2), alpha = 0.6) +
      scale_fill_manual(
        values = c("Hoved/andet" = "#FDBA21", "Ben" = "#0D1C8A"),
        name = "Kropsdel"
      ) +
      coord_flip(xlim = c(50, 100), ylim = c(0, 100)) +
      theme_pitch() +
      labs(title = "Skudpositioner opdelt i ben og hoved/andet") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  output$body_distance_table <- renderTable({
    allshotevents %>%
      group_by(SHOTBODYPART) %>%
      summarise(
        Antal = n(),
        `Gns. afstand (m)` = round(mean(shot_distance, na.rm = TRUE), 1),
        `SD (afstand)` = round(sd(shot_distance, na.rm = TRUE), 1)
      )
  })
  
  output$team_ranking_summary_ui <- renderUI({
    df <- allshotevents %>%
      group_by(TEAMNAME.x, MATCH_WYID.x, IMAGEDATAURL) %>%
      summarise(shots = n(), .groups = "drop") %>%
      group_by(TEAMNAME.x, IMAGEDATAURL) %>%
      summarise(
        antal_kampe = n(),
        gennemsnit_skud = mean(shots),
        sd_skud = sd(shots),
        .groups = "drop"
      ) %>%
      arrange(desc(gennemsnit_skud))
    
    # lav HTML-rækker med logo
    html_rows <- apply(df, 1, function(row) {
      sprintf(
        '<tr>
        <td><img src="%s" height="30px" style="margin-right:10px;"> %s</td>
        <td>%.0f</td>
        <td>%.2f</td>
        <td>%.2f</td>
      </tr>',
        row["IMAGEDATAURL"],
        row["TEAMNAME.x"],
        as.numeric(row["antal_kampe"]),
        as.numeric(row["gennemsnit_skud"]),
        as.numeric(row["sd_skud"])
      )
    })
    
    # wrap i en HTML-tabel
    HTML(sprintf('
    <table style="width:100%%; font-size:14px;">
      <thead>
        <tr>
          <th>Hold</th>
          <th>Antal kampe</th>
          <th>Gns. skud</th>
          <th>Std. afvigelse</th>
        </tr>
      </thead>
      <tbody>
        %s
      </tbody>
    </table>', paste(html_rows, collapse = "")
    ))
  })
  
  output$rating_table <- renderTable({
    allshotevents %>%
      pivot_longer(cols = c(overall, potential), names_to = "Ratingtype", values_to = "rating") %>%
      group_by(Ratingtype) %>%
      summarise(
        `Gns. rating` = round(mean(rating, na.rm = TRUE), 1),
        `Standardafvigelse` = round(sd(rating, na.rm = TRUE), 1),
        `Antal skud` = n()
      )
  })
  
  output$possession_events_table <- renderTable({
    allshotevents %>%
      summarise(
        Antal_skud = n(),
        Gns_varighed = round(mean(POSSESSIONEVENTSNUMBER, na.rm = TRUE), 2),
        SD_varighed = round(sd(POSSESSIONEVENTSNUMBER, na.rm = TRUE), 2),
        Min = min(POSSESSIONEVENTSNUMBER, na.rm = TRUE),
        Max = max(POSSESSIONEVENTSNUMBER, na.rm = TRUE)
      )
  })
  
  output$possession_index_table <- renderTable({
    data <- if (input$split) train_data else allshotevents
    
    data %>%
      summarise(
        "Gennemsnit" = round(mean(POSSESSIONEVENTINDEX, na.rm = TRUE), 1),
        "Median" = round(median(POSSESSIONEVENTINDEX, na.rm = TRUE), 1),
        "Standardafvigelse" = round(sd(POSSESSIONEVENTINDEX, na.rm = TRUE), 1)
      )
  })
  
  output$duration_summary <- renderTable({
    allshotevents %>%
      mutate(duration_bin = cut(POSSESSIONDURATION,
                                breaks = c(0, 10, 20, 45, Inf),
                                labels = c("Short (0-10s)", "Medium (10-20s)", "Long (20-45s)", "Very long (45s+)"),
                                right = FALSE)) %>%
      group_by(duration_bin) %>%
      summarise(
        `Antal skud` = n(),
        `Gns. varighed` = round(mean(POSSESSIONDURATION, na.rm = TRUE), 1),
        `SD` = round(sd(POSSESSIONDURATION, na.rm = TRUE), 1),
        .groups = "drop"
      )
  })
  
  
}



shinyApp(ui, server)