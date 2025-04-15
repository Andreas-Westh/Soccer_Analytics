library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggsoccer)
library(viridis)
library(purrr)
library(tidyverse)
library(ggimage)

# -- UI -----------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Måldata Superligaen"),
  dashboardSidebar(
    sidebarMenu(
      id = "plot_choice",
      menuItem("📘 Introduktion", tabName = "intro", icon = icon("info-circle")),
      menuItem("Skudposition (punktplot)", tabName = "location_points", icon = icon("crosshairs")),
      menuItem("Skudposition (heatmap)", tabName = "location_heatmap", icon = icon("fire")),
      menuItem("Skudvinkel", tabName = "shot_angle", icon = icon("angle-right")),
      menuItem("Afstand til mål", tabName = "shot_distance", icon = icon("ruler-horizontal")),
      menuItem("Kropsdel", tabName = "body_part", icon = icon("running")),
      menuItem("Team Ranking", tabName = "team_ranking", icon = icon("sort-amount-down-alt")),
      menuItem("Overall", tabName = "overall", icon = icon("user-check")),
      menuItem("Potential", tabName = "potential", icon = icon("star-half-alt")),
      menuItem("Antal events i possession", tabName = "possession_events", icon = icon("list-ol")),
      menuItem("Index for possession", tabName = "possession_index", icon = icon("fingerprint")),
      menuItem("Varighed af possession", tabName = "possession_duration", icon = icon("clock")),
      br(),
      checkboxInput("split", "Vis opdelt på træning/test", value = FALSE),
      checkboxInput("avg_per_game", "Vis gennemsnit pr. kamp", value = FALSE)
    )
  ),
  dashboardBody(
    tabItems(
      # -- INTRO --
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
              p("God fornøjelse med analysen!")
      ),
      
      # -- SKUDPOSITION --
      tabItem(tabName = "location_points",
              fluidRow(
                box(title = "Plot", width = 8, solidHeader = TRUE, status = "primary",
                    uiOutput("plots_ui")),
                box(title = "Konklusion", width = 4, solidHeader = TRUE, status = "info",
                    textOutput("conclusion_text"))
              ),
              
              # -- SHOT ANGLE VISUALER --
              conditionalPanel(
                condition = "input.plot_choice == 'shot_angle'",
                fluidRow(
                  column(6, plotOutput("shot_angle_barplot")),
                  column(6, plotOutput("shot_angle_visual"))
                )
              ),
              
              # -- DISTANCE LINES VISUAL --
              conditionalPanel(
                condition = "input.plot_choice == 'shot_distance'",
                fluidRow(
                  box(title = "Afstandslinjer på banen", width = 12, solidHeader = TRUE, status = "warning",
                      plotOutput("distance_lines_plot", height = "300px"))
                )
              ),
              
              # -- SUMMARY TABLE FOR ALLE --
              fluidRow(
                box(title = "Gennemsnit og standardafvigelse", width = 12, solidHeader = TRUE, status = "warning",
                    tableOutput("summary_table"))
              )
      )
    )
  )
)

# -- Konklusionstekster -------------------------------------------
get_conclusion <- function(var) {
  switch(var,
         "location_points" = "Konklusion: Skud tættest på målet har langt højere sandsynlighed for at blive til mål – især i feltet omkring det lille målfelt.",
         "location_heatmap" = "Konklusion: Langt de fleste skud tages fra centrale positioner i og omkring feltet. Der ses tydelige hotspots tæt foran mål. Og det kan ses, at ved ikke mål er skudene spredt mere ud, og derved ikke danne samme synlige 'hotzones'",
         "shot_angle" = "Konklusion: Sandsynligheden for at score falder under gennemsnitet ved vinkler under 30°, mens vinkler fra 70° fører til mål 3x så oftes.",
         "shot_distance" = "Konklusion: Næsten alle mål scores fra under 20 meter. Chancen for mål falder støt med stigende afstand.",
         "body_part" = "Konklusion: Skud med hovedet er det mest effektive, men dette er også stærkt korreleret med, at hovedstød oftes finder sted MEGET tæt ved målet.",
         "team_ranking" = "",
         "overall" = "",
         "potential" = "",
         "possession_events" = "",
         "possession_index" = "",
         "possession_duration" = "",
         "Ukendt valg" = ""
  )
}

# -- Plotfunktion -------------------------------------------------
make_plot <- function(data, var, avg_on) {
  # Definér farver: lysegrå for ikke-mål, blød blå for mål
  colors <- c("0" = "#D3D3D3", "1" = "#4F94CD")
  
  switch(var,
         # Punktplot for skudpositioner (ingen procentlabels, da det er punkter)
         "location_points" = {
           if (avg_on) {
             # Kode for gennemsnit pr. kamp visning (aktuelt samme som standard visning)
             ggplot(data, aes(x = LOCATIONX, y = LOCATIONY, color = factor(SHOTISGOAL))) +
               annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "white") +
               geom_point(alpha = 0.7, size = 3) +
               scale_color_manual(values = colors, labels = c("Ikke mål", "Mål")) +
               theme_pitch() +
               coord_flip(xlim = c(50, 100), ylim = c(0, 100)) +
               labs(title = "Skudpositioner fordelt på mål og ikke-mål (gennemsnit pr. kamp)", 
                    color = "Resultat") +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                     legend.position = "bottom")
           } else {
             # Standard visning
             ggplot(data, aes(x = LOCATIONX, y = LOCATIONY, color = factor(SHOTISGOAL))) +
               annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "white") +
               geom_point(alpha = 0.7, size = 3) +
               scale_color_manual(values = colors, labels = c("Ikke mål", "Mål")) +
               theme_pitch() +
               coord_flip(xlim = c(50, 100), ylim = c(0, 100)) +
               labs(title = "Skudpositioner fordelt på mål og ikke-mål", 
                    color = "Resultat") +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                     legend.position = "bottom")
           }
         }
         ,
         # Heatmap for skudpositioner (ingen procentlabels, da det er tæthed)
         "location_heatmap" = {
           ggplot(data, aes(x = LOCATIONX, y = LOCATIONY)) +
             annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "white") +
             stat_density_2d(aes(fill = after_stat(density)), geom = "raster", contour = FALSE, alpha = 0.6) +
             scale_fill_viridis_c(option = "inferno", begin = 0.2, end = 0.8) +
             theme_pitch() +
             facet_wrap(~SHOTISGOAL, labeller = labeller(SHOTISGOAL = c("0" = "Ikke mål", "1" = "Mål"))) +
             labs(title = "Heatmap over skudpositioner")
         },
         # Skudvinkel
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
         }
         
         ,
         # Kropsdel
         "body_part" = {
           if (avg_on) {
             data_summary <- data %>%
               group_by(MATCH_WYID.x, SHOTBODYPART, SHOTISGOAL) %>%
               summarise(count = n(), .groups = "drop") %>%
               group_by(SHOTBODYPART, SHOTISGOAL) %>%
               summarise(avg_count = mean(count, na.rm = TRUE), .groups = "drop")
             
             total_per_part <- data_summary %>%
               group_by(SHOTBODYPART) %>%
               summarise(total = sum(avg_count), .groups = "drop")
             
             plot_data <- left_join(data_summary, total_per_part, by = "SHOTBODYPART") %>%
               mutate(percentage = 100 * avg_count / total)
             
             ggplot(plot_data, aes(x = SHOTBODYPART, y = percentage, fill = factor(SHOTISGOAL))) +
               geom_col(position = position_dodge(width = 0.8), width = 0.7) +
               geom_text(aes(label = sprintf("%.1f%%", percentage)),
                         position = position_dodge(width = 0.8),
                         vjust = -0.3, size = 3) +
               scale_fill_manual(values = colors, labels = c("Ikke mål", "Mål")) +
               labs(
                 x = "Kropsdel",
                 y = "Procent (%)",
                 fill = "Udfald",
                 title = "Kropsdel – Procentvis fordeling af skud (gns. pr. kamp)"
               ) +
               theme_minimal(base_size = 12) +
               theme(
                 plot.title = element_text(hjust = 0.5, face = "bold")
               )
             
           } else {
             data_summary <- data %>%
               group_by(SHOTBODYPART, SHOTISGOAL) %>%
               summarise(count = n(), .groups = "drop")
             
             total_per_part <- data_summary %>%
               group_by(SHOTBODYPART) %>%
               summarise(total = sum(count), .groups = "drop")
             
             plot_data <- left_join(data_summary, total_per_part, by = "SHOTBODYPART") %>%
               mutate(percentage = 100 * count / total)
             
             ggplot(plot_data, aes(x = SHOTBODYPART, y = percentage, fill = factor(SHOTISGOAL))) +
               geom_col(position = position_dodge(width = 0.8), width = 0.7) +
               geom_text(aes(label = sprintf("%.1f%%", percentage)),
                         position = position_dodge(width = 0.8),
                         vjust = -0.3, size = 3) +
               scale_fill_manual(values = colors, labels = c("Ikke mål", "Mål")) +
               labs(
                 x = "Kropsdel",
                 y = "Procent (%)",
                 fill = "Udfald",
                 title = "Kropsdel – Procentvis fordeling af skud"
               ) +
               theme_minimal(base_size = 12) +
               theme(
                 plot.title = element_text(hjust = 0.5, face = "bold")
               )
           }
         }
         ,
         # Team ranking
         "team_ranking" = {
           if (avg_on) {
             plot_data <- data %>%
               group_by(TEAMNAME.x, Team_Ranking, IMAGEDATAURL, MATCH_WYID.x, SHOTISGOAL) %>%
               summarise(shots = n(), .groups = "drop") %>%
               group_by(TEAMNAME.x, Team_Ranking, IMAGEDATAURL, SHOTISGOAL) %>%
               summarise(avg_shots = mean(shots), .groups = "drop") %>%
               group_by(TEAMNAME.x, Team_Ranking, IMAGEDATAURL) %>%
               mutate(total = sum(avg_shots),
                      percentage = avg_shots / total * 100) %>%
               mutate(label = paste0(TEAMNAME.x, " (#", Team_Ranking, ")"))
             p <- ggplot(plot_data, aes(x = reorder(label, -Team_Ranking), y = avg_shots, fill = factor(SHOTISGOAL))) +
               geom_col(position = "fill") +
               geom_text(
                 aes(label = sprintf("%.1f%%", percentage)),
                 position = position_fill(vjust = 0.5),
                 size = 3
               ) +
               scale_fill_manual(values = colors, labels = c("Ikke mål", "Mål")) +
               labs(
                 x = "Hold (rangering)",
                 y = "Procent (%)",
                 fill = "Er det mål?",
                 title = "Fordeling af skud pr. hold (gennemsnit pr. kamp)"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                     axis.text.x = element_text(angle = 45, hjust = 1))
             p
           } else {
             plot_data <- data %>%
               group_by(TEAMNAME.x, Team_Ranking, IMAGEDATAURL, SHOTISGOAL) %>%
               summarise(count = n(), .groups = "drop") %>%
               group_by(TEAMNAME.x, Team_Ranking, IMAGEDATAURL) %>%
               mutate(total = sum(count),
                      percentage = count / total * 100) %>%
               mutate(label = paste0(TEAMNAME.x, " (#", Team_Ranking, ")"))
             p <- ggplot(plot_data, aes(x = reorder(label, -Team_Ranking), y = count, fill = factor(SHOTISGOAL))) +
               geom_col(position = "fill") +
               geom_text(
                 aes(label = sprintf("%.1f%%", percentage)),
                 position = position_fill(vjust = 0.5),
                 size = 3
               ) +
               scale_fill_manual(values = colors, labels = c("Ikke mål", "Mål")) +
               labs(
                 x = "Hold (rangering)",
                 y = "Procent (%)",
                 fill = "Er det mål?",
                 title = "Fordeling af skud pr. hold"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                     axis.text.x = element_text(angle = 45, hjust = 1))
             p
           }
         },
         # Overall
         "overall" = {
           if (avg_on) {
             # FIX: Konverter navngivne vektorer til lister
             data_summary <- data %>%
               group_by(MATCH_WYID.x, SHOTISGOAL) %>%
               summarise(mean_overall = mean(overall, na.rm = TRUE), .groups = "drop")
             
             # Brug as.list() for at sikre at breaks er en liste og ikke en navngiven vektor
             breaks_min <- floor(min(data_summary$mean_overall, na.rm = TRUE))
             breaks_max <- ceiling(max(data_summary$mean_overall, na.rm = TRUE))
             breaks <- as.list(seq(breaks_min, breaks_max, by = 1))
             
             data_summary <- data_summary %>%
               mutate(value_bin = cut(mean_overall, breaks = unlist(breaks), right = FALSE, include.lowest = TRUE))
             
             binned_data <- data_summary %>%
               group_by(value_bin, SHOTISGOAL) %>%
               summarise(count = n(), .groups = "drop") %>%
               group_by(value_bin) %>%
               mutate(total = sum(count),
                      percentage = count / total * 100) %>%
               ungroup()
             
             # Beregn bin_mid som en liste i stedet for en navngiven vektor
             bin_mids <- as.list(unlist(breaks)[-length(unlist(breaks))] + 0.5)
             binned_data$bin_mid <- bin_mids[as.numeric(binned_data$value_bin)]
             
             p <- ggplot(binned_data, aes(x = bin_mid, y = count, fill = factor(SHOTISGOAL))) +
               geom_col(position = "fill", width = 1) +
               geom_text(
                 aes(label = sprintf("%.1f%%", percentage)),
                 position = position_fill(vjust = 0.5),
                 size = 3
               ) +
               scale_fill_manual(values = colors, labels = c("Ikke mål", "Mål")) +
               labs(
                 x = "Gns. overall pr. kamp",
                 y = "Procent (%)",
                 fill = "Er det mål?",
                 title = "Fordeling af overall pr. kamp"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
             p
           } else {
             # FIX: Konverter navngivne vektorer til lister
             breaks_min <- floor(min(data$overall, na.rm = TRUE))
             breaks_max <- ceiling(max(data$overall, na.rm = TRUE))
             breaks <- as.list(seq(breaks_min, breaks_max, by = 1))
             
             data <- data %>%
               mutate(value_bin = cut(overall, breaks = unlist(breaks), right = FALSE, include.lowest = TRUE))
             
             binned_data <- data %>%
               group_by(value_bin, SHOTISGOAL) %>%
               summarise(count = n(), .groups = "drop") %>%
               group_by(value_bin) %>%
               mutate(total = sum(count),
                      percentage = count / total * 100) %>%
               ungroup()
             
             # Beregn bin_mid som en liste i stedet for en navngiven vektor
             bin_mids <- as.list(unlist(breaks)[-length(unlist(breaks))] + 0.5)
             binned_data$bin_mid <- bin_mids[as.numeric(binned_data$value_bin)]
             
             p <- ggplot(binned_data, aes(x = bin_mid, y = count, fill = factor(SHOTISGOAL))) +
               geom_col(position = "fill", width = 1) +
               geom_text(
                 aes(label = sprintf("%.1f%%", percentage)),
                 position = position_fill(vjust = 0.5),
                 size = 3
               ) +
               scale_fill_manual(values = colors, labels = c("Ikke mål", "Mål")) +
               labs(
                 x = "Overall",
                 y = "Procent (%)",
                 fill = "Er det mål?",
                 title = "Fordeling af overall"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
             p
           }
         },
         # Potential
         "potential" = {
           if (avg_on) {
             # FIX: Konverter navngivne vektorer til lister
             data_summary <- data %>%
               group_by(MATCH_WYID.x, SHOTISGOAL) %>%
               summarise(mean_potential = mean(potential, na.rm = TRUE), .groups = "drop")
             
             breaks_min <- floor(min(data_summary$mean_potential, na.rm = TRUE))
             breaks_max <- ceiling(max(data_summary$mean_potential, na.rm = TRUE))
             breaks <- as.list(seq(breaks_min, breaks_max, by = 1))
             
             data_summary <- data_summary %>%
               mutate(value_bin = cut(mean_potential, breaks = unlist(breaks), right = FALSE, include.lowest = TRUE))
             
             binned_data <- data_summary %>%
               group_by(value_bin, SHOTISGOAL) %>%
               summarise(count = n(), .groups = "drop") %>%
               group_by(value_bin) %>%
               mutate(total = sum(count),
                      percentage = count / total * 100) %>%
               ungroup()
             
             # Beregn bin_mid som en liste i stedet for en navngiven vektor
             bin_mids <- as.list(unlist(breaks)[-length(unlist(breaks))] + 0.5)
             binned_data$bin_mid <- bin_mids[as.numeric(binned_data$value_bin)]
             
             p <- ggplot(binned_data, aes(x = bin_mid, y = count, fill = factor(SHOTISGOAL))) +
               geom_col(position = "fill", width = 1) +
               geom_text(
                 aes(label = sprintf("%.1f%%", percentage)),
                 position = position_fill(vjust = 0.5),
                 size = 3
               ) +
               scale_fill_manual(values = colors, labels = c("Ikke mål", "Mål")) +
               labs(
                 x = "Gns. potential pr. kamp",
                 y = "Procent (%)",
                 fill = "Er det mål?",
                 title = "Fordeling af potential pr. kamp"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
             p
           } else {
             # FIX: Konverter navngivne vektorer til lister
             breaks_min <- floor(min(data$potential, na.rm = TRUE))
             breaks_max <- ceiling(max(data$potential, na.rm = TRUE))
             breaks <- as.list(seq(breaks_min, breaks_max, by = 1))
             
             data <- data %>%
               mutate(value_bin = cut(potential, breaks = unlist(breaks), right = FALSE, include.lowest = TRUE))
             
             binned_data <- data %>%
               group_by(value_bin, SHOTISGOAL) %>%
               summarise(count = n(), .groups = "drop") %>%
               group_by(value_bin) %>%
               mutate(total = sum(count),
                      percentage = count / total * 100) %>%
               ungroup()
             
             # Beregn bin_mid som en liste i stedet for en navngiven vektor
             bin_mids <- as.list(unlist(breaks)[-length(unlist(breaks))] + 0.5)
             binned_data$bin_mid <- bin_mids[as.numeric(binned_data$value_bin)]
             
             p <- ggplot(binned_data, aes(x = bin_mid, y = count, fill = factor(SHOTISGOAL))) +
               geom_col(position = "fill", width = 1) +
               geom_text(
                 aes(label = sprintf("%.1f%%", percentage)),
                 position = position_fill(vjust = 0.5),
                 size = 3
               ) +
               scale_fill_manual(values = colors, labels = c("Ikke mål", "Mål")) +
               labs(
                 x = "Potential",
                 y = "Procent (%)",
                 fill = "Er det mål?",
                 title = "Fordeling af potential"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
             p
           }
         },
         # Antal events i possession
         "possession_events" = {
           if (avg_on) {
             # FIX: Konverter navngivne vektorer til lister
             data_summary <- data %>%
               group_by(MATCH_WYID.x, SHOTISGOAL) %>%
               summarise(mean_events = mean(POSSESSIONEVENTSNUMBER, na.rm = TRUE), .groups = "drop")
             
             breaks_min <- floor(min(data_summary$mean_events, na.rm = TRUE))
             breaks_max <- ceiling(max(data_summary$mean_events, na.rm = TRUE))
             breaks <- as.list(seq(breaks_min, breaks_max, by = 1))
             
             data_summary <- data_summary %>%
               mutate(value_bin = cut(mean_events, breaks = unlist(breaks), right = FALSE, include.lowest = TRUE))
             
             binned_data <- data_summary %>%
               group_by(value_bin, SHOTISGOAL) %>%
               summarise(count = n(), .groups = "drop") %>%
               group_by(value_bin) %>%
               mutate(total = sum(count),
                      percentage = count / total * 100) %>%
               ungroup()
             
             # Beregn bin_mid som en liste i stedet for en navngiven vektor
             bin_mids <- as.list(unlist(breaks)[-length(unlist(breaks))] + 0.5)
             binned_data$bin_mid <- bin_mids[as.numeric(binned_data$value_bin)]
             
             p <- ggplot(binned_data, aes(x = bin_mid, y = count, fill = factor(SHOTISGOAL))) +
               geom_col(position = "fill", width = 1) +
               geom_text(
                 aes(label = sprintf("%.1f%%", percentage)),
                 position = position_fill(vjust = 0.5),
                 size = 3
               ) +
               scale_fill_manual(values = colors, labels = c("Ikke mål", "Mål")) +
               labs(
                 x = "Gns. antal hændelser pr. possession (pr. kamp)",
                 y = "Procent (%)",
                 fill = "Er det mål?",
                 title = "Fordeling af hændelser i possession pr. kamp"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
             p
           } else {
             # FIX: Konverter navngivne vektorer til lister
             breaks_min <- floor(min(data$POSSESSIONEVENTSNUMBER, na.rm = TRUE))
             breaks_max <- ceiling(max(data$POSSESSIONEVENTSNUMBER, na.rm = TRUE))
             breaks <- as.list(seq(breaks_min, breaks_max, by = 1))
             
             data <- data %>%
               mutate(value_bin = cut(POSSESSIONEVENTSNUMBER, breaks = unlist(breaks), right = FALSE, include.lowest = TRUE))
             
             binned_data <- data %>%
               group_by(value_bin, SHOTISGOAL) %>%
               summarise(count = n(), .groups = "drop") %>%
               group_by(value_bin) %>%
               mutate(total = sum(count),
                      percentage = count / total * 100) %>%
               ungroup()
             
             # Beregn bin_mid som en liste i stedet for en navngiven vektor
             bin_mids <- as.list(unlist(breaks)[-length(unlist(breaks))] + 0.5)
             binned_data$bin_mid <- bin_mids[as.numeric(binned_data$value_bin)]
             
             p <- ggplot(binned_data, aes(x = bin_mid, y = count, fill = factor(SHOTISGOAL))) +
               geom_col(position = "fill", width = 1) +
               geom_text(
                 aes(label = sprintf("%.1f%%", percentage)),
                 position = position_fill(vjust = 0.5),
                 size = 3
               ) +
               scale_fill_manual(values = colors, labels = c("Ikke mål", "Mål")) +
               labs(
                 x = "Antal hændelser i possession",
                 y = "Procent (%)",
                 fill = "Er det mål?",
                 title = "Fordeling af hændelser i possession"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
             p
           }
         },
         # Index for possession
         "possession_index" = {
           if (avg_on) {
             # FIX: Konverter navngivne vektorer til lister
             data_summary <- data %>%
               group_by(MATCH_WYID.x, SHOTISGOAL) %>%
               summarise(mean_index = mean(POSSESSIONEVENTINDEX, na.rm = TRUE), .groups = "drop")
             
             breaks_min <- floor(min(data_summary$mean_index, na.rm = TRUE))
             breaks_max <- ceiling(max(data_summary$mean_index, na.rm = TRUE))
             breaks <- as.list(seq(breaks_min, breaks_max, by = 5))
             
             data_summary <- data_summary %>%
               mutate(index_bin = cut(mean_index, breaks = unlist(breaks), right = FALSE, include.lowest = TRUE))
             
             binned_data <- data_summary %>%
               group_by(index_bin, SHOTISGOAL) %>%
               summarise(count = n(), .groups = "drop") %>%
               group_by(index_bin) %>%
               mutate(total = sum(count),
                      percentage = count / total * 100) %>%
               ungroup()
             
             # Beregn bin_mid som en liste i stedet for en navngiven vektor
             bin_mids <- as.list(unlist(breaks)[-length(unlist(breaks))] + 2.5)
             binned_data$bin_mid <- bin_mids[as.numeric(binned_data$index_bin)]
             
             p <- ggplot(binned_data, aes(x = bin_mid, y = count, fill = factor(SHOTISGOAL))) +
               geom_col(position = "fill", width = 5) +
               geom_text(
                 aes(label = sprintf("%.1f%%", percentage)),
                 position = position_fill(vjust = 0.5),
                 size = 3
               ) +
               scale_fill_manual(values = colors, labels = c("Ikke mål", "Mål")) +
               labs(
                 x = "Gns. possession-index pr. kamp",
                 y = "Procent (%)",
                 fill = "Er det mål?",
                 title = "Fordeling af possession-index pr. kamp"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
             p
           } else {
             # FIX: Konverter navngivne vektorer til lister
             breaks_min <- floor(min(data$POSSESSIONEVENTINDEX, na.rm = TRUE))
             breaks_max <- ceiling(max(data$POSSESSIONEVENTINDEX, na.rm = TRUE))
             breaks <- as.list(seq(breaks_min, breaks_max, by = 5))
             
             data <- data %>%
               mutate(index_bin = cut(POSSESSIONEVENTINDEX, breaks = unlist(breaks), right = FALSE, include.lowest = TRUE))
             
             binned_data <- data %>%
               group_by(index_bin, SHOTISGOAL) %>%
               summarise(count = n(), .groups = "drop") %>%
               group_by(index_bin) %>%
               mutate(total = sum(count),
                      percentage = count / total * 100) %>%
               ungroup()
             
             # Beregn bin_mid som en liste i stedet for en navngiven vektor
             bin_mids <- as.list(unlist(breaks)[-length(unlist(breaks))] + 2.5)
             binned_data$bin_mid <- bin_mids[as.numeric(binned_data$index_bin)]
             
             p <- ggplot(binned_data, aes(x = bin_mid, y = count, fill = factor(SHOTISGOAL))) +
               geom_col(position = "fill", width = 5) +
               geom_text(
                 aes(label = sprintf("%.1f%%", percentage)),
                 position = position_fill(vjust = 0.5),
                 size = 3
               ) +
               scale_fill_manual(values = colors, labels = c("Ikke mål", "Mål")) +
               labs(
                 x = "Possession index",
                 y = "Procent (%)",
                 fill = "Er det mål?",
                 title = "Fordeling af possession-index"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
             p
           }
         },
         # Varighed af possession
         "possession_duration" = {
           if (avg_on) {
             # FIX: Konverter navngivne vektorer til lister
             data_summary <- data %>%
               group_by(MATCH_WYID.x, SHOTISGOAL) %>%
               summarise(mean_dur = mean(POSSESSIONDURATION, na.rm = TRUE), .groups = "drop")
             
             breaks_min <- floor(min(data_summary$mean_dur, na.rm = TRUE))
             breaks_max <- ceiling(max(data_summary$mean_dur, na.rm = TRUE))
             breaks <- as.list(seq(breaks_min, breaks_max, by = 2))
             
             data_summary <- data_summary %>%
               mutate(dur_bin = cut(mean_dur, breaks = unlist(breaks), right = FALSE, include.lowest = TRUE))
             
             binned_data <- data_summary %>%
               group_by(dur_bin, SHOTISGOAL) %>%
               summarise(count = n(), .groups = "drop") %>%
               group_by(dur_bin) %>%
               mutate(total = sum(count),
                      percentage = count / total * 100) %>%
               ungroup()
             
             # Beregn bin_mid som en liste i stedet for en navngiven vektor
             bin_mids <- as.list(unlist(breaks)[-length(unlist(breaks))] + 1)
             binned_data$bin_mid <- bin_mids[as.numeric(binned_data$dur_bin)]
             
             p <- ggplot(binned_data, aes(x = bin_mid, y = count, fill = factor(SHOTISGOAL))) +
               geom_col(position = "fill", width = 2) +
               geom_text(
                 aes(label = sprintf("%.1f%%", percentage)),
                 position = position_fill(vjust = 0.5),
                 size = 3
               ) +
               scale_fill_manual(values = colors, labels = c("Ikke mål", "Mål")) +
               labs(
                 x = "Gns. varighed af possession (sekunder)",
                 y = "Procent (%)",
                 fill = "Er det mål?",
                 title = "Fordeling af possession-varighed pr. kamp"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
             p
           } else {
             # FIX: Konverter navngivne vektorer til lister
             breaks_min <- floor(min(data$POSSESSIONDURATION, na.rm = TRUE))
             breaks_max <- ceiling(max(data$POSSESSIONDURATION, na.rm = TRUE))
             breaks <- as.list(seq(breaks_min, breaks_max, by = 2))
             
             data <- data %>%
               mutate(dur_bin = cut(POSSESSIONDURATION, breaks = unlist(breaks), right = FALSE, include.lowest = TRUE))
             
             binned_data <- data %>%
               group_by(dur_bin, SHOTISGOAL) %>%
               summarise(count = n(), .groups = "drop") %>%
               group_by(dur_bin) %>%
               mutate(total = sum(count),
                      percentage = count / total * 100) %>%
               ungroup()
             
             # Beregn bin_mid som en liste i stedet for en navngiven vektor
             bin_mids <- as.list(unlist(breaks)[-length(unlist(breaks))] + 1)
             binned_data$bin_mid <- bin_mids[as.numeric(binned_data$dur_bin)]
             
             p <- ggplot(binned_data, aes(x = bin_mid, y = count, fill = factor(SHOTISGOAL))) +
               geom_col(position = "fill", width = 2) +
               geom_text(
                 aes(label = sprintf("%.1f%%", percentage)),
                 position = position_fill(vjust = 0.5),
                 size = 3
               ) +
               scale_fill_manual(values = colors, labels = c("Ikke mål", "Mål")) +
               labs(
                 x = "Varighed af possession (sekunder)",
                 y = "Procent (%)",
                 fill = "Er det mål?",
                 title = "Fordeling af possession-varighed"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
             p
           }
         }
  )
}

# -- Server -------------------------------------------------------
server <- function(input, output, session) {
  allshotevents <- readRDS("allshotevents_Shiny_EDA_1.rds")
  train_data <- readRDS("train_data.rds")
  test_data <- readRDS("test_data.rds")
  
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
  
  output$distance_lines_plot <- renderPlot({
    # -- Beregn gennemsnitsafstande
    mean_goal_dist <- allshotevents %>%
      filter(SHOTISGOAL == 1) %>%
      summarise(m = mean(shot_distance, na.rm = TRUE)) %>%
      pull(m)
    
    mean_nogoal_dist <- allshotevents %>%
      filter(SHOTISGOAL == 0) %>%
      summarise(m = mean(shot_distance, na.rm = TRUE)) %>%
      pull(m)
    
    # -- Halvcirkler
    make_semicircle <- function(radius, center_x = 100, center_y = 50, n = 300) {
      angles <- seq(-pi/2, pi/2, length.out = n)
      data.frame(
        x = center_x - radius * cos(angles),
        y = center_y + radius * sin(angles)
      )
    }
    
    goal_circle <- make_semicircle(mean_goal_dist) %>%
      mutate(type = "Gns. afstand (mål)", color = "#ffd500")
    
    nogoal_circle <- make_semicircle(mean_nogoal_dist) %>%
      mutate(type = "Gns. afstand (ikke mål)", color = "#00296b")
    
    circles_avg <- bind_rows(goal_circle, nogoal_circle)
    
    circle_labels <- circles_avg %>%
      group_by(type, color) %>%
      slice_head(n = 1) %>%
      mutate(label = type)
    
    
    # -- Plot
    ggplot(allshotevents, aes(x = LOCATIONX, y = LOCATIONY)) +
      annotate_pitch(dimensions = pitch_wyscout, colour = "white", fill = "gray90") +
      geom_bin2d(binwidth = c(2, 2), aes(fill = after_stat(count)), alpha = 0.5) +
      geom_path(data = circles_avg, aes(x = x, y = y, group = type, color = color), linewidth = 1.2) +
      geom_text(data = circle_labels %>%
                  mutate(y = ifelse(label == "Gns. afstand (mål)", y + 25, y - 25)),
                aes(x = x, y = y, label = label, color = color),
                fontface = "bold", size = 6, vjust = -0.4, hjust = 0) +
      scale_color_identity() +
      scale_fill_viridis_c(option = "A", direction = -1) +
      coord_flip(xlim = c(50, 100), ylim = c(0, 100)) +
      theme_pitch() +
      labs(
        title = "Klar tendens til, at en lavere afstand giver flere mål",
        fill = "Antal skud"
      ) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  output$summary_table <- renderTable({
    var <- input$plot_choice
    
    numeric_vars <- c(
      "shot_distance" = "shot_distance",
      "shot_angle" = "shot_angle",
      "overall" = "overall",
      "potential" = "potential",
      "possession_events" = "POSSESSIONEVENTSNUMBER",
      "possession_index" = "POSSESSIONEVENTINDEX",
      "possession_duration" = "POSSESSIONDURATION"
    )
    
    if (!(var %in% names(numeric_vars))) return(NULL)
    
    col <- numeric_vars[[var]]
    data <- allshotevents
    
    total <- nrow(data)
    total_goals <- sum(data$SHOTISGOAL == 1, na.rm = TRUE)
    total_nongoals <- sum(data$SHOTISGOAL == 0, na.rm = TRUE)
    
    tibble(
      Gruppe = c("Alle skud", "Mål", "Ikke mål"),
      Gns = c(
        mean(data[[col]], na.rm = TRUE),
        mean(data[[col]][data$SHOTISGOAL == 1], na.rm = TRUE),
        mean(data[[col]][data$SHOTISGOAL == 0], na.rm = TRUE)
      ),
      SD = c(
        sd(data[[col]], na.rm = TRUE),
        sd(data[[col]][data$SHOTISGOAL == 1], na.rm = TRUE),
        sd(data[[col]][data$SHOTISGOAL == 0], na.rm = TRUE)
      ),
      `Andel mål (%)` = c(
        100 * total_goals / total,
        100,
        0
      )
    )
  }, digits = 2)
  
  
  
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
      theme_pitch() +
      labs(
        title = "Visuel sammenligning af skudvinkler",
        color = "Skudvinkel"
      ) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  
  
  output$conclusion_text <- renderText({
    get_conclusion(input$plot_choice)
  })
}

shinyApp(ui, server)
