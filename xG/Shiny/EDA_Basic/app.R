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
    tags$head(
      tags$link(rel = "shortcut icon", href = "favicon.ico")
    ),
    tabItems(
      # Intro-tab
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
                  condition = "input.plot_choice == 'shot_distance'",
                  box(title = "Afstandslinjer på banen", width = 12, solidHeader = TRUE, status = "warning",
                      plotOutput("distance_lines_plot", height = "300px"))
                )
              )
  ))))
  

# -- Konklusionstekster -------------------------------------------
get_conclusion <- function(var) {
  switch(var,
         "location_points" = "Konklusion: xxxx",
         "location_heatmap" = "Konklusion: xxxx",
         "shot_angle" = "Konklusion: xxxx",
         "shot_distance" = "Konklusion: xxxx",
         "body_part" = "Konklusion: xxxx",
         "team_ranking" = "Konklusion: xxxx",
         "overall" = "Konklusion: xxxx",
         "potential" = "Konklusion: xxxx",
         "possession_events" = "Konklusion: xxxx",
         "possession_index" = "Konklusion: xxxx",
         "possession_duration" = "Konklusion: xxxx",
         "Ukendt valg"
  )
}

# -- Plotfunktion -------------------------------------------------
make_plot <- function(data, var, avg_on) {
  switch(var,
         
         # Punktplot
         "location_points" = {
           ggplot(data, aes(x = LOCATIONX, y = LOCATIONY)) +
             annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "white") +
             geom_bin2d(binwidth = c(1, 1), alpha = 0.6, color = "black") +
             theme_pitch() +
             coord_flip(xlim = c(50, 100), ylim = c(0, 100)) +
             labs(title = "Der er den højeste koncentration af mål for skud i målfeltet") +
             theme(plot.title = element_text(hjust = 0.5, face = "bold"))
           
         },
         
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
           if (avg_on) {
             data %>%
               group_by(MATCH_WYID.x) %>%
               summarise(mean_angle = mean(shot_angle, na.rm = TRUE)) %>%
               ggplot(aes(x = mean_angle)) +
               geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
               scale_x_continuous(breaks = seq(0, 180, 30)) +
               labs(
                 x = "Gns. skudvinkel pr. kamp (grader)",
                 y = "Antal kampe",
                 title = "Hvor skarpe vinkler skyder holdene fra pr. kamp?"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
             
           } else {
             ggplot(data, aes(x = shot_angle)) +
               geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
               scale_x_continuous(breaks = seq(0, 180, 30)) +
               labs(
                 x = "Skudvinkel (grader)",
                 y = "Antal skud",
                 title = "Hvor skarpe vinkler skyder spillerne fra?"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
           }
         },
         
         # Shot distance
         "shot_distance" = {
           if (avg_on) {
             data %>%
               group_by(MATCH_WYID.x) %>%
               summarise(mean_dist = mean(shot_distance, na.rm = TRUE)) %>%
               mutate(distance_group = cut(mean_dist,
                                           breaks = c(0,10,20,30,40,Inf),
                                           labels = c("0–10m", "10–20m", "20–30m", "30–40m", "40m+"),
                                           right = FALSE)) %>%
               ggplot(aes(x = mean_dist, fill = distance_group)) +
               geom_histogram(binwidth = 2, color = "white") +
               scale_fill_manual(values = c("#00296b", "#003f88", "#00509d", "#fdc500", "#ffd500")) +
               scale_x_continuous(breaks = seq(0, 70, 10)) +
               labs(
                 x = "Gns. afstand til mål pr. kamp (meter)",
                 y = "Antal kampe",
                 fill = "Afstand",
                 title = "Hvor langt fra målet afslutter holdene pr. kamp?"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
             
           } else {
             data %>%
               mutate(distance_group = cut(shot_distance,
                                           breaks = c(0,10,20,30,40,Inf),
                                           labels = c("0–10m", "10–20m", "20–30m", "30–40m", "40m+"),
                                           right = FALSE)) %>%
               ggplot(aes(x = shot_distance, fill = distance_group)) +
               geom_histogram(binwidth = 2, color = "white") +
               scale_fill_manual(values = c("#00296b", "#003f88", "#00509d", "#fdc500", "#ffd500")) +
               scale_x_continuous(breaks = seq(0, 70, 10)) +
               labs(
                 x = "Afstand til mål (meter)",
                 y = "Antal skud",
                 fill = "Afstand",
                 title = "Hvor langt fra målet afslutter spillerne?"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
           }
         },
         

         
         # Body part
         "body_part" = {
           if (avg_on) {
             data %>%
               group_by(MATCH_WYID.x, SHOTBODYPART) %>%
               summarise(count = n(), .groups = "drop") %>%
               group_by(SHOTBODYPART) %>%
               summarise(avg = mean(count)) %>%
               ggplot(aes(x = SHOTBODYPART, y = avg)) +
               geom_col(aes(fill = "SHOTBODYPART")) +
               labs(x = "Kropsdel", y = "Gennemsnit pr. kamp", title = "Skud pr. kropsdel (gennemsnit)") +
               theme_minimal()
           } else {
             ggplot(data, aes(x = SHOTBODYPART)) +
               geom_bar(aes(fill = "SHOTBODYPART")) +
               labs(x = "Kropsdel", y = "Antal skud", title = "Fordeling af skud pr. kropsdel") +
               theme_minimal()
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
             data %>%
               group_by(TEAMNAME.x, Team_Ranking, IMAGEDATAURL) %>%
               summarise(total = n(), .groups = "drop") %>%
               mutate(label = paste0(TEAMNAME.x, " (#", Team_Ranking, ")")) %>%
               arrange(Team_Ranking) %>%
               ggplot(aes(x = reorder(label, -Team_Ranking), y = total)) +
               geom_col(fill = "lightgray", color = "black") +
               geom_image(aes(image = IMAGEDATAURL), size = 0.06, asp = 1.2) +
               coord_flip() +
               labs(x = NULL, y = "Antal skud", title = "Antal skud pr. hold") +
               theme_minimal(base_size = 16)
           }
         },
         
         # Overall
         "overall" = {
           if (avg_on) {
             data %>%
               group_by(MATCH_WYID.x) %>%
               summarise(mean_value = mean(overall, na.rm = TRUE)) %>%
               ggplot(aes(x = mean_value)) +
               geom_histogram(binwidth = 1, fill = "mediumseagreen", color = "white") +
               geom_vline(aes(xintercept = mean(mean_value, na.rm = TRUE)), 
                          color = "darkgreen", linetype = "dashed", linewidth = 1) +
               labs(
                 x = "Gns. FIFA-overall pr. kamp",
                 y = "Antal kampe",
                 title = "Gennemsnitlig FIFA-overall for skudspillere pr. kamp"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
             
           } else {
             ggplot(data, aes(x = overall)) +
               geom_histogram(binwidth = 1, fill = "mediumseagreen", color = "white") +
               geom_vline(aes(xintercept = mean(overall, na.rm = TRUE)), 
                          color = "darkgreen", linetype = "dashed", linewidth = 1) +
               labs(
                 x = "FIFA-overall",
                 y = "Antal skud",
                 title = "Fordeling af FIFA-overall for skudspillere"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
           }
         },
         
         
         # Potential
         "potential" = {
           if (avg_on) {
             data %>%
               group_by(MATCH_WYID.x) %>%
               summarise(mean_value = mean(potential, na.rm = TRUE)) %>%
               ggplot(aes(x = mean_value)) +
               geom_histogram(binwidth = 1, fill = "goldenrod", color = "white") +
               geom_vline(aes(xintercept = mean(mean_value, na.rm = TRUE)), 
                          color = "darkorange", linetype = "dashed", linewidth = 1) +
               labs(
                 x = "Gns. FIFA-potential pr. kamp",
                 y = "Antal kampe",
                 title = "Gennemsnitlig FIFA-potential for skudspillere pr. kamp"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
             
           } else {
             ggplot(data, aes(x = potential)) +
               geom_histogram(binwidth = 1, fill = "goldenrod", color = "white") +
               geom_vline(aes(xintercept = mean(potential, na.rm = TRUE)), 
                          color = "darkorange", linetype = "dashed", linewidth = 1) +
               labs(
                 x = "FIFA-potential",
                 y = "Antal skud",
                 title = "Fordeling af FIFA-potential for skudspillere"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
           }
         },
         
         
         # Possession events
         "possession_events" = {
           if (avg_on) {
             data %>%
               group_by(MATCH_WYID.x) %>%
               summarise(mean_value = mean(POSSESSIONEVENTSNUMBER, na.rm = TRUE)) %>%
               ggplot(aes(x = mean_value)) +
               geom_histogram(binwidth = 1, fill = "mediumpurple", color = "white") +
               geom_vline(aes(xintercept = mean(mean_value, na.rm = TRUE)), 
                          color = "purple", linetype = "dashed", linewidth = 1) +
               labs(
                 x = "Gns. antal hændelser pr. possession (pr. kamp)",
                 y = "Antal kampe",
                 title = "Hvor mange hændelser leder op til skud pr. kamp?"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
             
           } else {
             ggplot(data, aes(x = POSSESSIONEVENTSNUMBER)) +
               geom_histogram(binwidth = 1, fill = "mediumpurple", color = "white") +
               geom_vline(aes(xintercept = mean(POSSESSIONEVENTSNUMBER, na.rm = TRUE)), 
                          color = "purple", linetype = "dashed", linewidth = 1) +
               labs(
                 x = "Antal hændelser i possession før skud",
                 y = "Antal skud",
                 title = "Fordeling af possessionslængder før skud"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
           }
         },
         
         
         # Possession index
         "possession_index" = {
           if (avg_on) {
             data %>%
               group_by(MATCH_WYID.x) %>%
               summarise(mean_index = mean(POSSESSIONEVENTINDEX, na.rm = TRUE)) %>%
               ggplot(aes(x = mean_index)) +
               geom_histogram(binwidth = 5, fill = "#00b4d8", color = "white") +
               geom_vline(aes(xintercept = mean(mean_index, na.rm = TRUE)), 
                          color = "#0077b6", linetype = "dashed", linewidth = 1) +
               labs(
                 x = "Gns. possession-index pr. kamp",
                 y = "Antal kampe",
                 title = "Hvornår i kampens possessions bliver der afsluttet (gennemsnit pr. kamp)?"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
             
           } else {
             ggplot(data, aes(x = POSSESSIONEVENTINDEX)) +
               geom_histogram(binwidth = 5, fill = "#00b4d8", color = "white") +
               geom_vline(aes(xintercept = mean(POSSESSIONEVENTINDEX, na.rm = TRUE)), 
                          color = "#0077b6", linetype = "dashed", linewidth = 1) +
               labs(
                 x = "Possession index i kampen",
                 y = "Antal skud",
                 title = "Hvornår i kampen bliver der afsluttet?"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
           }
         },
         
         
         # Possession duration
         "possession_duration" = {
           if (avg_on) {
             data %>%
               group_by(MATCH_WYID.x) %>%
               summarise(mean_dur = mean(POSSESSIONDURATION, na.rm = TRUE)) %>%
               ggplot(aes(x = mean_dur)) +
               geom_histogram(binwidth = 2, fill = "#f9844a", color = "white") +
               geom_vline(aes(xintercept = mean(mean_dur, na.rm = TRUE)), 
                          color = "#d00000", linetype = "dashed", linewidth = 1) +
               labs(
                 x = "Gns. varighed af possession (sekunder)",
                 y = "Antal kampe",
                 title = "Hvor længe varer en possession før skud (gennemsnit pr. kamp)?"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
             
           } else {
             ggplot(data, aes(x = POSSESSIONDURATION)) +
               geom_histogram(binwidth = 2, fill = "#f9844a", color = "white") +
               geom_vline(aes(xintercept = mean(POSSESSIONDURATION, na.rm = TRUE)), 
                          color = "#d00000", linetype = "dashed", linewidth = 1) +
               labs(
                 x = "Varighed af possession før skud (sekunder)",
                 y = "Antal skud",
                 title = "Fordeling af possessionsvarighed før skud"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
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
  
  # distance lines
  output$distance_lines_plot <- renderPlot({
    ggplot() +
      annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "white") +
      
      # Linjer
      geom_vline(xintercept = 90, linetype = "dashed", color = "#00296b", size = 1) +
      geom_vline(xintercept = 80, linetype = "dashed", color = "#003f88", size = 1) +
      geom_vline(xintercept = 70, linetype = "dashed", color = "#00509d", size = 1) +
      geom_vline(xintercept = 60, linetype = "dashed", color = "#fdc500", size = 1) +
      geom_vline(xintercept = 50, linetype = "dashed", color = "#ffd500", size = 1) +
      
      # Tekst
      annotate("text", x = 90, y = 50, label = "10m", angle = 90, vjust = -0.5, size = 4,
               fontface = "bold", color = "#00296b") +
      annotate("text", x = 80, y = 50, label = "20m", angle = 90, vjust = -0.5, size = 4,
               fontface = "bold", color = "#003f88") +
      annotate("text", x = 70, y = 50, label = "30m", angle = 90, vjust = -0.5, size = 4,
               fontface = "bold", color = "#00509d") +
      annotate("text", x = 60, y = 50, label = "40m", angle = 90, vjust = -0.5, size = 4,
               fontface = "bold", color = "#fdc500") +
      annotate("text", x = 50, y = 50, label = "50m", angle = 90, vjust = -0.5, size = 4,
               fontface = "bold", color = "#ffd500") +
      
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
