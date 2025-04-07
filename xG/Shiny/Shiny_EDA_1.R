library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggsoccer)
library(viridis)

# -- UI -----------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Skuddata Superligaen"),
  dashboardSidebar(
    sidebarMenu(
      id = "plot_choice",
      menuItem("📘 Introduktion", tabName = "intro", icon = icon("info-circle")),
      menuItem("Skudposition (punktplot)", tabName = "location_points"),
      menuItem("Skudposition (heatmap)", tabName = "location_heatmap"),
      menuItem("Skudvinkel", tabName = "shot_angle"),
      menuItem("Afstand til mål", tabName = "shot_distance"),
      menuItem("Kropsdel", tabName = "body_part"),
      menuItem("Team Ranking", tabName = "team_ranking"),
      menuItem("Overall", tabName = "overall"),
      menuItem("Potential", tabName = "potential"),
      menuItem("Antal events i possession", tabName = "possession_events"),
      menuItem("Index for possession", tabName = "possession_index"),
      menuItem("Varighed af possession", tabName = "possession_duration"),
      br(),
      checkboxInput("split", "Vis opdelt på træning/test", value = FALSE)
    )
  ),
  dashboardBody(
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
      tabItem(tabName = "location_points",  # Navnet matcher første variable
              fluidRow(
                box(title = "Plot", width = 8, solidHeader = TRUE, status = "primary",
                    uiOutput("plots_ui")),
                box(title = "Konklusion", width = 4, solidHeader = TRUE, status = "info",
                    textOutput("conclusion_text"))
              )
      )
    )
  ))
  

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
make_plot <- function(data, var) {
  switch(var,
         "location_points" = {
           ggplot(data, aes(x = LOCATIONX, y = LOCATIONY)) +
             annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "white") +
             geom_point(alpha = 0.6, color = "black") +
             theme_pitch() +
             coord_flip(xlim = c(50, 100), ylim = c(0, 100)) +
             labs(title = "Placering af skud på banen")
         },
         "location_heatmap" = {
           ggplot(data, aes(x = LOCATIONX, y = LOCATIONY)) +
             annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "white") +
             stat_density_2d(aes(fill = after_stat(density)), 
                             geom = "raster", contour = FALSE, alpha = 0.8) +
             scale_fill_viridis_c(option = "C") +
             theme_pitch() +
             labs(title = "Heatmap over skudpositioner")
         },
         "shot_angle" = {
           ggplot(data, aes(x = shot_angle)) +
             geom_histogram(fill = "steelblue") +
             labs(x = "Skudvinkel (grader)", y = "Tæthed", title = "Fordeling af skudvinkler") +
             theme_minimal()
         },
         "shot_distance" = {
           ggplot(data, aes(x = shot_distance)) +
             geom_histogram(fill = "darkorange") +
             labs(x = "Afstand til mål", y = "Tæthed", title = "Fordeling af skuddistance") +
             theme_minimal()
         },
         "body_part" = {
           ggplot(data, aes(x = SHOTBODYPART)) +
             geom_bar(fill = "mediumseagreen") +
             labs(x = "Kropsdel", y = "Antal skud", title = "Fordeling af skud pr. kropsdel") +
             theme_minimal()
         },
         "team_ranking" = {
           team_shots <- data %>%
             group_by(TEAMNAME.x, Team_Ranking, IMAGEDATAURL) %>%
             summarise(total_shots = n(), .groups = "drop") %>%
             mutate(label = paste0(TEAMNAME.x, " (#", Team_Ranking, ")")) %>%
             arrange(Team_Ranking)
           
           ggplot(team_shots, aes(x = reorder(label, -Team_Ranking), y = total_shots)) +
             geom_col(aes(fill = TEAMNAME.x), color = "black", width = 0.8) +
             geom_image(aes(image = IMAGEDATAURL), size = 0.06, asp = 1.2) +
             coord_flip() +
             labs(
               title = "Antal skud pr. hold",
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
         },
         "overall" = {
           ggplot(data, aes(x = overall)) +
             geom_histogram(fill = "mediumseagreen") +
             labs(x = "overall", y = "Antal skud", title = "Fordeling af skud pr. overall") +
             theme_minimal()
         },
         "potential" = {
           ggplot(data, aes(x = potential)) +
             geom_histogram(fill = "mediumseagreen") +
             labs(x = "potential", y = "Antal skud", title = "Fordeling af skud pr. potential") +
             theme_minimal()
         },
         "possession_events" = {
           ggplot(data, aes(x = POSSESSIONEVENTSNUMBER)) +
             geom_histogram(fill = "mediumseagreen") +
             labs(x = "POSSESSIONEVENTSNUMBER", y = "Antal skud", title = "Fordeling af skud pr. POSSESSIONEVENTSNUMBER") +
             theme_minimal()
         },
         "possession_index" = {
           ggplot(data, aes(x = POSSESSIONEVENTINDEX)) +
             geom_histogram(fill = "mediumseagreen") +
             labs(x = "POSSESSIONEVENTINDEX", y = "Antal skud", title = "Fordeling af skud pr. POSSESSIONEVENTINDEX") +
             theme_minimal()
         },
         "possession_duration" = {
           ggplot(data, aes(x = POSSESSIONDURATION)) +
             geom_histogram(fill = "mediumseagreen") +
             labs(x = "POSSESSIONDURATION", y = "Antal skud", title = "Fordeling af skud pr. POSSESSIONDURATION") +
             theme_minimal()
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
    make_plot(allshotevents, input$plot_choice)
  })
  
  output$plot_train <- renderPlot({
    make_plot(train_data, input$plot_choice) + ggtitle("Træningsdata")
  })
  
  output$plot_test <- renderPlot({
    make_plot(test_data, input$plot_choice) + ggtitle("Testdata")
  })
  
  output$conclusion_text <- renderText({
    get_conclusion(input$plot_choice)
  })
}

shinyApp(ui, server)
