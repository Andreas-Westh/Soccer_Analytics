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
  dashboardHeader(title = "Skuddata Superligaen"),
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
      tabItem(tabName = "intro",
              h2("Velkommen til skuddata-dashboardet"),
              p("Denne applikation giver dig mulighed for at udforske skuddata fra Superligens 2023/2024 sæson."),
              tags$ul(
                tags$li("Brug menuen i venstre side til at vælge en variabel."),
                tags$li("Tjek 'Vis opdelt på træning/test' for at sammenligne splits."),
                tags$li("Hver variabel vises som et plot og med en tilhørende konklusion.")
              ),
              p("God fornøjelse!")
      ),
      tabItem(tabName = "location_points",
              fluidRow(
                box(title = "Plot", width = 8, solidHeader = TRUE, status = "primary",
                    uiOutput("plots_ui")),
                box(title = "Konklusion", width = 4, solidHeader = TRUE, status = "info",
                    textOutput("conclusion_text"))
              ),
              fluidRow(
                conditionalPanel(
                  condition = "input.plot_choice == 'shot_distance'",
                  box(title = "Afstandslinjer på banen", width = 12, solidHeader = TRUE, status = "warning",
                      plotOutput("distance_lines_plot", height = "300px"))
                )
              )
      )
    )
  )
)

# -- Konklusionstekster -------------------------------------------
get_conclusion <- function(var) {
  switch(var,
         "location_points" = "Konklusion: Skud tæt på målet har højere sandsynlighed for at være mål.",
         "location_heatmap" = "Konklusion: Heatmap viser koncentrationen af skud og deres udfald.",
         "shot_angle" = "Konklusion: Skudvinkler påvirker sandsynligheden for mål.",
         "shot_distance" = "Konklusion: Afstand til mål er en vigtig faktor for målscoring.",
         "body_part" = "Konklusion: Forskellige kropsdele har forskellige succesrater for mål.",
         "team_ranking" = "Konklusion: Holdrangering kan indikere deres evne til at score mål.",
         "overall" = "Konklusion: Spillerens overall rating kan påvirke målscoring.",
         "potential" = "Konklusion: Spillerens potentiale kan have en indvirkning på målscoring.",
         "possession_events" = "Konklusion: Antallet af hændelser i possession kan påvirke målscoring.",
         "possession_index" = "Konklusion: Tidspunktet for possession i kampen kan have en effekt på målscoring.",
         "possession_duration" = "Konklusion: Varigheden af possession kan påvirke målscoring.",
         "Ukendt valg"
  )
}

# -- Plotfunktion -------------------------------------------------
make_plot <- function(data, var, avg_on) {
  # Definér farver: lysegrå for ikke-mål, blød blå for mål
  colors <- c("0" = "#D3D3D3", "1" = "#4F94CD")
  
  switch(var,
         # Punktplot for skudpositioner
         "location_points" = {
           ggplot(data, aes(x = LOCATIONX, y = LOCATIONY, color = factor(SHOTISGOAL))) +
             annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "white") +
             geom_point(alpha = 0.6) +
             scale_color_manual(values = colors, labels = c("Ikke mål", "Mål")) +
             theme_pitch() +
             coord_flip(xlim = c(50, 100), ylim = c(0, 100)) +
             labs(title = "Skudpositioner", color = "Er det mål?") +
             theme(plot.title = element_text(hjust = 0.5, face = "bold"))
         },
         # Heatmap for skudpositioner
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
           if (avg_on) {
             data %>%
               group_by(MATCH_WYID.x, SHOTISGOAL) %>%
               summarise(mean_angle = mean(shot_angle, na.rm = TRUE), .groups = "drop") %>%
               ggplot(aes(x = mean_angle, fill = factor(SHOTISGOAL))) +
               geom_histogram(position = "fill", binwidth = 5, color = "white") +
               scale_fill_manual(values = colors, labels = c("Ikke mål", "Mål")) +
               scale_x_continuous(breaks = seq(0, 180, 30)) +
               labs(
                 x = "Gns. skudvinkel pr. kamp (grader)",
                 y = "Procent (%)",
                 fill = "Er det mål?",
                 title = "Fordeling af skudvinkler pr. kamp"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
           } else {
             ggplot(data, aes(x = shot_angle, fill = factor(SHOTISGOAL))) +
               geom_histogram(position = "fill", binwidth = 5, color = "white") +
               scale_fill_manual(values = colors, labels = c("Ikke mål", "Mål")) +
               scale_x_continuous(breaks = seq(0, 180, 30)) +
               labs(
                 x = "Skudvinkel (grader)",
                 y = "Procent (%)",
                 fill = "Er det mål?",
                 title = "Fordeling af skudvinkler"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
           }
         },
         # Afstand til mål
         "shot_distance" = {
           if (avg_on) {
             data %>%
               group_by(MATCH_WYID.x, SHOTISGOAL) %>%
               summarise(mean_dist = mean(shot_distance, na.rm = TRUE), .groups = "drop") %>%
               ggplot(aes(x = mean_dist, fill = factor(SHOTISGOAL))) +
               geom_histogram(position = "fill", binwidth = 2, color = "white") +
               scale_fill_manual(values = colors, labels = c("Ikke mål", "Mål")) +
               scale_x_continuous(breaks = seq(0, 70, 10)) +
               labs(
                 x = "Gns. afstand til mål pr. kamp (meter)",
                 y = "Procent (%)",
                 fill = "Er det mål?",
                 title = "Fordeling af skudafstande pr. kamp"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
           } else {
             ggplot(data, aes(x = shot_distance, fill = factor(SHOTISGOAL))) +
               geom_histogram(position = "fill", binwidth = 2, color = "white") +
               scale_fill_manual(values = colors, labels = c("Ikke mål", "Mål")) +
               scale_x_continuous(breaks = seq(0, 70, 10)) +
               labs(
                 x = "Afstand til mål (meter)",
                 y = "Procent (%)",
                 fill = "Er det mål?",
                 title = "Fordeling af skudafstande"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
           }
         },
         # Kropsdel
         "body_part" = {
           if (avg_on) {
             data %>%
               group_by(MATCH_WYID.x, SHOTBODYPART, SHOTISGOAL) %>%
               summarise(count = n(), .groups = "drop") %>%
               group_by(SHOTBODYPART, SHOTISGOAL) %>%
               summarise(avg = mean(count), .groups = "drop") %>%
               group_by(SHOTBODYPART) %>%
               mutate(percentage = avg / sum(avg) * 100) %>%
               ggplot(aes(x = SHOTBODYPART, y = percentage, fill = factor(SHOTISGOAL))) +
               geom_bar(stat = "identity", position = "fill") +
               scale_fill_manual(values = colors, labels = c("Ikke mål", "Mål")) +
               labs(
                 x = "Kropsdel",
                 y = "Procent (%)",
                 fill = "Er det mål?",
                 title = "Målfordeling pr. kropsdel (gennemsnit pr. kamp)"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
           } else {
             data %>%
               group_by(SHOTBODYPART, SHOTISGOAL) %>%
               summarise(count = n(), .groups = "drop") %>%
               group_by(SHOTBODYPART) %>%
               mutate(percentage = count / sum(count) * 100) %>%
               ggplot(aes(x = SHOTBODYPART, y = percentage, fill = factor(SHOTISGOAL))) +
               geom_bar(stat = "identity", position = "fill") +
               scale_fill_manual(values = colors, labels = c("Ikke mål", "Mål")) +
               labs(
                 x = "Kropsdel",
                 y = "Procent (%)",
                 fill = "Er det mål?",
                 title = "Målfordeling pr. kropsdel"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
           }
         },
         # Team ranking
         "team_ranking" = {
           if (avg_on) {
             data %>%
               group_by(TEAMNAME.x, Team_Ranking, IMAGEDATAURL, MATCH_WYID.x, SHOTISGOAL) %>%
               summarise(shots = n(), .groups = "drop") %>%
               group_by(TEAMNAME.x, Team_Ranking, IMAGEDATAURL, SHOTISGOAL) %>%
               summarise(avg_shots = mean(shots), .groups = "drop") %>%
               group_by(TEAMNAME.x, Team_Ranking, IMAGEDATAURL) %>%
               mutate(percentage = avg_shots / sum(avg_shots) * 100) %>%
               mutate(label = paste0(TEAMNAME.x, " (#", Team_Ranking, ")")) %>%
               arrange(Team_Ranking) %>%
               ggplot(aes(x = reorder(label, -Team_Ranking), y = percentage, fill = factor(SHOTISGOAL))) +
               geom_col(position = "fill") +
               scale_fill_manual(values = colors, labels = c("Ikke mål", "Mål")) +
               geom_image(aes(image = IMAGEDATAURL), size = 0.06, asp = 1.2) +
               coord_flip() +
               labs(
                 x = NULL,
                 y = "Procent (%)",
                 fill = "Er det mål?",
                 title = "Målfordeling pr. hold pr. kamp"
               ) +
               theme_minimal(base_size = 16)
           } else {
             data %>%
               group_by(TEAMNAME.x, Team_Ranking, IMAGEDATAURL, SHOTISGOAL) %>%
               summarise(total = n(), .groups = "drop") %>%
               group_by(TEAMNAME.x, Team_Ranking, IMAGEDATAURL) %>%
               mutate(percentage = total / sum(total) * 100) %>%
               mutate(label = paste0(TEAMNAME.x, " (#", Team_Ranking, ")")) %>%
               arrange(Team_Ranking) %>%
               ggplot(aes(x = reorder(label, -Team_Ranking), y = percentage, fill = factor(SHOTISGOAL))) +
               geom_col(position = "fill") +
               scale_fill_manual(values = colors, labels = c("Ikke mål", "Mål")) +
               geom_image(aes(image = IMAGEDATAURL), size = 0.06, asp = 1.2) +
               coord_flip() +
               labs(
                 x = NULL,
                 y = "Procent (%)",
                 fill = "Er det mål?",
                 title = "Målfordeling pr. hold"
               ) +
               theme_minimal(base_size = 16)
           }
         },
         # Overall
         "overall" = {
           if (avg_on) {
             data %>%
               group_by(MATCH_WYID.x, SHOTISGOAL) %>%
               summarise(mean_value = mean(overall, na.rm = TRUE), .groups = "drop") %>%
               ggplot(aes(x = mean_value, fill = factor(SHOTISGOAL))) +
               geom_histogram(position = "fill", binwidth = 1, color = "white") +
               scale_fill_manual(values = colors, labels = c("Ikke mål", "Mål")) +
               geom_vline(aes(xintercept = mean(mean_value, na.rm = TRUE)), color = "darkgreen", linetype = "dashed", linewidth = 1) +
               labs(
                 x = "Gns. FIFA-overall pr. kamp",
                 y = "Procent (%)",
                 fill = "Er det mål?",
                 title = "Fordeling af FIFA-overall pr. kamp"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
           } else {
             ggplot(data, aes(x = overall, fill = factor(SHOTISGOAL))) +
               geom_histogram(position = "fill", binwidth = 1, color = "white") +
               scale_fill_manual(values = colors, labels = c("Ikke mål", "Mål")) +
               geom_vline(aes(xintercept = mean(overall, na.rm = TRUE)), color = "darkgreen", linetype = "dashed", linewidth = 1) +
               labs(
                 x = "FIFA-overall",
                 y = "Procent (%)",
                 fill = "Er det mål?",
                 title = "Fordeling af FIFA-overall"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
           }
         },
         # Potential
         "potential" = {
           if (avg_on) {
             data %>%
               group_by(MATCH_WYID.x, SHOTISGOAL) %>%
               summarise(mean_value = mean(potential, na.rm = TRUE), .groups = "drop") %>%
               ggplot(aes(x = mean_value, fill = factor(SHOTISGOAL))) +
               geom_histogram(position = "fill", binwidth = 1, color = "white") +
               scale_fill_manual(values = colors, labels = c("Ikke mål", "Mål")) +
               geom_vline(aes(xintercept = mean(mean_value, na.rm = TRUE)), color = "darkorange", linetype = "dashed", linewidth = 1) +
               labs(
                 x = "Gns. FIFA-potential pr. kamp",
                 y = "Procent (%)",
                 fill = "Er det mål?",
                 title = "Fordeling af FIFA-potential pr. kamp"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
           } else {
             ggplot(data, aes(x = potential, fill = factor(SHOTISGOAL))) +
               geom_histogram(position = "fill", binwidth = 1, color = "white") +
               scale_fill_manual(values = colors, labels = c("Ikke mål", "Mål")) +
               geom_vline(aes(xintercept = mean(potential, na.rm = TRUE)), color = "darkorange", linetype = "dashed", linewidth = 1) +
               labs(
                 x = "FIFA-potential",
                 y = "Procent (%)",
                 fill = "Er det mål?",
                 title = "Fordeling af FIFA-potential"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
           }
         },
         # Antal events i possession
         "possession_events" = {
           if (avg_on) {
             data %>%
               group_by(MATCH_WYID.x, SHOTISGOAL) %>%
               summarise(mean_value = mean(POSSESSIONEVENTSNUMBER, na.rm = TRUE), .groups = "drop") %>%
               ggplot(aes(x = mean_value, fill = factor(SHOTISGOAL))) +
               geom_histogram(position = "fill", binwidth = 1, color = "white") +
               scale_fill_manual(values = colors, labels = c("Ikke mål", "Mål")) +
               geom_vline(aes(xintercept = mean(mean_value, na.rm = TRUE)), color = "purple", linetype = "dashed", linewidth = 1) +
               labs(
                 x = "Gns. antal hændelser pr. possession (pr. kamp)",
                 y = "Procent (%)",
                 fill = "Er det mål?",
                 title = "Fordeling af hændelser i possession pr. kamp"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
           } else {
             ggplot(data, aes(x = POSSESSIONEVENTSNUMBER, fill = factor(SHOTISGOAL))) +
               geom_histogram(position = "fill", binwidth = 1, color = "white") +
               scale_fill_manual(values = colors, labels = c("Ikke mål", "Mål")) +
               geom_vline(aes(xintercept = mean(POSSESSIONEVENTSNUMBER, na.rm = TRUE)), color = "purple", linetype = "dashed", linewidth = 1) +
               labs(
                 x = "Antal hændelser i possession",
                 y = "Procent (%)",
                 fill = "Er det mål?",
                 title = "Fordeling af hændelser i possession"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
           }
         },
         # Index for possession
         "possession_index" = {
           if (avg_on) {
             data %>%
               group_by(MATCH_WYID.x, SHOTISGOAL) %>%
               summarise(mean_index = mean(POSSESSIONEVENTINDEX, na.rm = TRUE), .groups = "drop") %>%
               ggplot(aes(x = mean_index, fill = factor(SHOTISGOAL))) +
               geom_histogram(position = "fill", binwidth = 5, color = "white") +
               scale_fill_manual(values = colors, labels = c("Ikke mål", "Mål")) +
               geom_vline(aes(xintercept = mean(mean_index, na.rm = TRUE)), color = "#0077b6", linetype = "dashed", linewidth = 1) +
               labs(
                 x = "Gns. possession-index pr. kamp",
                 y = "Procent (%)",
                 fill = "Er det mål?",
                 title = "Fordeling af possession-index pr. kamp"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
           } else {
             ggplot(data, aes(x = POSSESSIONEVENTINDEX, fill = factor(SHOTISGOAL))) +
               geom_histogram(position = "fill", binwidth = 5, color = "white") +
               scale_fill_manual(values = colors, labels = c("Ikke mål", "Mål")) +
               geom_vline(aes(xintercept = mean(POSSESSIONEVENTINDEX, na.rm = TRUE)), color = "#0077b6", linetype = "dashed", linewidth = 1) +
               labs(
                 x = "Possession index",
                 y = "Procent (%)",
                 fill = "Er det mål?",
                 title = "Fordeling af possession-index"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
           }
         },
         # Varighed af possession
         "possession_duration" = {
           if (avg_on) {
             data %>%
               group_by(MATCH_WYID.x, SHOTISGOAL) %>%
               summarise(mean_dur = mean(POSSESSIONDURATION, na.rm = TRUE), .groups = "drop") %>%
               ggplot(aes(x = mean_dur, fill = factor(SHOTISGOAL))) +
               geom_histogram(position = "fill", binwidth = 2, color = "white") +
               scale_fill_manual(values = colors, labels = c("Ikke mål", "Mål")) +
               geom_vline(aes(xintercept = mean(mean_dur, na.rm = TRUE)), color = "#d00000", linetype = "dashed", linewidth = 1) +
               labs(
                 x = "Gns. varighed af possession (sekunder)",
                 y = "Procent (%)",
                 fill = "Er det mål?",
                 title = "Fordeling af possession-varighed pr. kamp"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
           } else {
             ggplot(data, aes(x = POSSESSIONDURATION, fill = factor(SHOTISGOAL))) +
               geom_histogram(position = "fill", binwidth = 2, color = "white") +
               scale_fill_manual(values = colors, labels = c("Ikke mål", "Mål")) +
               geom_vline(aes(xintercept = mean(POSSESSIONDURATION, na.rm = TRUE)), color = "#d00000", linetype = "dashed", linewidth = 1) +
               labs(
                 x = "Varighed af possession (sekunder)",
                 y = "Procent (%)",
                 fill = "Er det mål?",
                 title = "Fordeling af possession-varighed"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
           }
         }
  )
}

# -- Server -------------------------------------------------------
server <- function(input, output, session) {
  #allshotevents <- readRDS("allshotevents_Shiny_EDA_1.rds")
  #train_data <- readRDS("train_data.rds")
  #test_data <- readRDS("test_data.rds")
  
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
    ggplot() +
      annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "white") +
      geom_vline(xintercept = 90, linetype = "dashed", color = "#00296b", size = 1) +
      geom_vline(xintercept = 80, linetype = "dashed", color = "#003f88", size = 1) +
      geom_vline(xintercept = 70, linetype = "dashed", color = "#00509d", size = 1) +
      geom_vline(xintercept = 60, linetype = "dashed", color = "#fdc500", size = 1) +
      geom_vline(xintercept = 50, linetype = "dashed", color = "#ffd500", size = 1) +
      annotate("text", x = 90, y = 50, label = "10m", angle = 90, vjust = -0.5, size = 4, fontface = "bold", color = "#00296b") +
      annotate("text", x = 80, y = 50, label = "20m", angle = 90, vjust = -0.5, size = 4, fontface = "bold", color = "#003f88") +
      annotate("text", x = 70, y = 50, label = "30m", angle = 90, vjust = -0.5, size = 4, fontface = "bold", color = "#00509d") +
      annotate("text", x = 60, y = 50, label = "40m", angle = 90, vjust = -0.5, size = 4, fontface = "bold", color = "#fdc500") +
      annotate("text", x = 50, y = 50, label = "50m", angle = 90, vjust = -0.5, size = 4, fontface = "bold", color = "#ffd500") +
      coord_flip(xlim = c(50, 100), ylim = c(0, 100)) +
      theme_pitch() +
      labs(title = "Afstandsstreger fra mållinjen") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  output$conclusion_text <- renderText({
    get_conclusion(input$plot_choice)
  })
}

shinyApp(ui, server)