library(shiny)
library(ggplot2)
library(ggimage)
library(dplyr)
library(DT)


# Shiny-app ----
ui <- fluidPage(
  titlePanel("xP Model – Expected Points vs Faktiske Point"),
  fluidRow(
    column(
      width = 6,
      plotOutput("xpPlot", height = "700px")
    ),
    column(
      width = 6,
      h4("Statistik per hold"),
      DTOutput("xpTable")
    )
  )
)

server <- function(input, output) {
  xp_sum <- readRDS("xp_sum.rds")
  xp_last_frame <- readRDS("xp_last_frame.rds")
  # Feature engineering til tabellen
  xp_sum <- xp_sum %>%
    mutate(
      Point_difference = round(Total_xP - Point, 2),
      xP_rank = rank(-Total_xP, ties.method = "first"),
      Real_rank = rank(-Point, ties.method = "first"),
      Matches_played = round(Point / (Point / Total_xP), 0),  # ca. antal kampe
      xP_per_match = round(Total_xP / Matches_played, 2)
    )
  
  team_colors <- c(
    "Brøndby" = "#ffe100",
    "AGF" = "white",
    "København" = "white",
    "Midtjylland" = "white",
    "Nordsjælland" = "white",
    "Silkeborg" = "white",
    "OB" = "white",
    "Randers" = "white",
    "Viborg" = "white",
    "Lyngby" = "white",
    "Hvidovre" = "white",
    "Vejle" = "white"
  )
  
  output$xpPlot <- renderPlot({
    ggplot(xp_last_frame, aes(x = reorder(Team, cumulative_xP), y = cumulative_xP)) +
      geom_col(aes(fill = Team), color = "black") +
      geom_segment(aes(x = Team, xend = Team,
                       y = Point, yend = Point),
                   color = "black", size = 1.2) +
      geom_image(aes(image = logo_url), size = 0.06, asp = 1.2) +
      scale_fill_manual(values = team_colors) +
      coord_flip() +
      labs(
        title = "Slutstilling xP – baseret på antal spillede kampe per hold",
        subtitle = "Sort streg markerer reelt antal point",
        x = NULL,
        y = "Akkumuleret Expected Points"
      ) +
      theme_minimal(base_size = 16) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(face = "italic"),
        axis.text.y = element_text(face = "bold"),
        axis.text.x = element_text(size = 12),
        legend.position = "none"
      )
  })
  
  output$xpTable <- renderDT({
    xp_sum %>%
      select(
        Team,
        Total_xP,
        Point,
        Point_difference,
        xP_rank,
        Real_rank,
        xP_per_match
      ) %>%
      arrange(desc(Total_xP)) %>%
      datatable(
        rownames = FALSE,
        colnames = c("Hold", "xP", "Point", "Forskel", "xP Placering", "Placering", "xP/kamp"),
        options = list(pageLength = 14)
      )
  })
}

shinyApp(ui = ui, server = server)