library(shiny)
library(ggplot2)
library(ggsoccer)
library(viridis)

ui <- fluidPage(
  titlePanel("Interaktiv visualisering af skuddata"),
  sidebarLayout(
    sidebarPanel(
      selectInput("plot_choice", "Vælg variabel:", choices = c(
        "Skudposition (punktplot)" = "location_points",
        "Skudposition (heatmap)" = "location_heatmap",
        "Skudvinkel" = "shot_angle",
        "Afstand til mål" = "shot_distance",
        "Kropsdel" = "body_part",
        "Team Ranking" = "team_ranking",
        "Overall" = "overall",
        "Potential" = "potential",
        "Antal events i possession" = "possession_events",
        "Index for possession" = "possession_index",
        "Varighed af possession" = "possession_duration"
      )),
      checkboxInput("split", "Vis opdelt på træning/test", value = FALSE)
    ),
    mainPanel(
      uiOutput("plots_ui"),
      br(),
      textOutput("conclusion_text")
    )
  )
)

server <- function(input, output) {
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
             ggplot(data, aes(x = Team_Ranking)) +
               geom_histogram(fill = "mediumseagreen") +
               labs(x = "Team_Ranking", y = "Antal skud", title = "Fordeling af skud pr. Team_Ranking") +
               theme_minimal()
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
  
  output$plots_ui <- renderUI({
    if (input$split) {
      tagList(
        fluidRow(
          column(6, plotOutput("plot_train")),
          column(6, plotOutput("plot_test"))
        )
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
    paste("Konklusion til", input$plot_choice)
  })
}

shinyApp(ui = ui, server = server)
