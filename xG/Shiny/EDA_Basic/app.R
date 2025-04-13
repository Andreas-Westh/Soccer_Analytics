library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggsoccer)
library(viridis)
library(purrr)
library(tidyverse)
library(ggimage)




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
              ),
              fluidRow(
                conditionalPanel(
                  condition = "input.plot_choice == 'shot_angle'",
                  box(title = "Visuel forklaring på skudvinkler", width = 12, solidHeader = TRUE, status = "warning",
                      plotOutput("angle_visual_plot", height = "400px"))
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
              )
              
              
              
      ))))


# -- Konklusionstekster -------------------------------------------
get_conclusion <- function(var) {
  switch(var,
         "location_points" = "Konklusion: xxxx",
         "location_heatmap" = "Konklusion: xxxx",
         "shot_angle" = "Konklusion: xxxx",
         "shot_distance" = "Konklusion: xxxx",
         "body_part" = "Konklusion: Langt de fleste skud tages med fødderne – især højre fod bliver brugt oftest. Skud med hovedet (eller andre dele af kroppen) sker typisk tættere på mål, hvilket giver god mening, da de ofte kommer fra dødbolde som hjørnespark eller frispark. Fodskud sker derimod oftere i åbent spil og fra længere afstande. Derfor giver det god mening at bruge SHOTBODYPART som en forklarende variabel i en xG-model, da den fanger forskelle i både situation og afstand.",
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
             mean_angle <- mean(data$shot_angle, na.rm = TRUE)
             
             ggplot(data, aes(x = shot_angle)) +
               geom_histogram(
                 bins = 30,
                 fill = "#0D1C8A",
                 color = "white",
                 alpha = 0.9
               ) +
               geom_vline(
                 xintercept = mean_angle,
                 color = "#FDBA21",
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
           }
         },
         
         # Shot distance
         "shot_distance" = {
           if (avg_on) {
             kamp_means <- data %>%
               group_by(MATCH_WYID.x) %>%
               summarise(mean_dist = mean(shot_distance, na.rm = TRUE))
             
             total_avg <- mean(kamp_means$mean_dist, na.rm = TRUE)
             
             kamp_means %>%
               mutate(distance_group = cut(mean_dist,
                                           breaks = c(0,10,20,30,40,Inf),
                                           labels = c("0–10m", "10–20m", "20–30m", "30–40m", "40m+"),
                                           right = FALSE)) %>%
               ggplot(aes(x = mean_dist, fill = distance_group)) +
               geom_histogram(binwidth = 2, color = "white") +
               geom_vline(xintercept = total_avg, color = "#FDBA21", linewidth = 1.2) +
               annotate("text", x = total_avg + 1.5, y = Inf,
                        label = paste0("Gns.: ", round(total_avg, 1), "m"),
                        vjust = 2, hjust = 0, fontface = "bold", size = 3.5, color = "#FDBA21") +
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
             total_avg <- mean(data$shot_distance, na.rm = TRUE)
             
             data %>%
               mutate(distance_group = cut(shot_distance,
                                           breaks = c(0,10,20,30,40,Inf),
                                           labels = c("0–10m", "10–20m", "20–30m", "30–40m", "40m+"),
                                           right = FALSE)) %>%
               ggplot(aes(x = shot_distance, fill = distance_group)) +
               geom_histogram(binwidth = 2, color = "white") +
               geom_vline(xintercept = total_avg, color = "#FDBA21", linewidth = 1.2) +
               annotate("text", x = total_avg + 1.5, y = Inf,
                        label = paste0("Gns.: ", round(total_avg, 1), "m"),
                        vjust = 2, hjust = 0, fontface = "bold", size = 3.5, color = "#FDBA21") +
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
               ggplot(aes(x = SHOTBODYPART, y = avg, fill = SHOTBODYPART)) +
               geom_col() +
               scale_fill_manual(values = body_colors) +
               labs(x = "Kropsdel", y = "Gennemsnit pr. kamp", title = "Skud pr. kropsdel (gennemsnit)") +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
           } else {
             ggplot(data, aes(x = SHOTBODYPART, fill = SHOTBODYPART)) +
               geom_bar() +
               scale_fill_manual(values = body_colors) +
               labs(x = "Kropsdel", y = "Antal skud", title = "Fordeling af skud pr. kropsdel") +
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
    goal_x <- 100
    goal_y <- 50
    radii <- c(10, 20, 30, 40, 50)
    colors <- c("#00296b", "#003f88", "#00509d", "#fdc500", "#ffd500")  # De gamle farver
    
    make_semicircle <- function(radius, center_x = goal_x, center_y = goal_y, n = 300) {
      angles <- seq(-pi/2, pi/2, length.out = n)
      data.frame(
        x = center_x - radius * cos(angles),
        y = center_y + radius * sin(angles)
      )
    }
    
    # Kombiner farver og radius
    circles <- purrr::pmap_dfr(list(r = radii, col = colors), function(r, col) {
      make_semicircle(r) %>%
        mutate(group = r, color = col)
    })
    
    label_data <- circles %>%
      group_by(group, color) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(label = paste0(group, "m"))
    
    ggplot() +
      annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "white") +
      geom_path(data = circles, aes(x = x, y = y, group = group, color = color), linewidth = 1) +
      geom_text(data = label_data, aes(x = x, y = y, label = label, color = color),
                vjust = -0.5, hjust = 0.8, fontface = "bold", size = 3.5) +
      scale_color_identity() +  # <- NØGLEN TIL RIGTIGE FARVER
      coord_flip(xlim = c(50, 100), ylim = c(0, 100)) +
      theme_pitch() +
      labs(title = "Afstandscirkler fra mållinjen") +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none"
      )
  })
  
  
  
  
  
  output$conclusion_text <- renderText({
    get_conclusion(input$plot_choice)
  })
  
  output$angle_visual_plot <- renderPlot({
    # Målstolper
    goal_left <- c(x = 100, y = 44.285)
    goal_right <- c(x = 100, y = 55.715)
    
    vinkel_eksempler <- data.frame(
      label = c("Skarp vinkel (7°)", "Gennemsnitlig vinkel (34°)", "Åben vinkel (78°)"),
      x = c(69, 91, 93),
      y = c(91, 60, 50)
    )
    
    ggplot() +
      annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "gray90") +
      geom_segment(data = vinkel_eksempler,
                   aes(x = x, y = y, xend = goal_left["x"], yend = goal_left["y"], color = label),
                   arrow = arrow(length = unit(0.15, "cm"))) +
      geom_segment(data = vinkel_eksempler,
                   aes(x = x, y = y, xend = goal_right["x"], yend = goal_right["y"], color = label),
                   arrow = arrow(length = unit(0.15, "cm"))) +
      geom_point(data = vinkel_eksempler, aes(x = x, y = y, color = label), size = 4) +
      geom_text(data = vinkel_eksempler, aes(x = x - 1.5, y = y, label = label, color = label),
                hjust = 1, fontface = "bold", show.legend = FALSE) +
      coord_flip(xlim = c(65, 105), ylim = c(0, 100)) +
      theme_pitch() +
      labs(title = "Visuel sammenligning af skudvinkler", color = NULL) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none"
      )
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
  
  

}



shinyApp(ui, server)