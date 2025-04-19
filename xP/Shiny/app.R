library(shiny)
library(shinyWidgets)
library(ggplot2)
library(ggimage)
library(dplyr)
library(DT)
library(tidyr)

# Shiny-app ----
ui <- fluidPage(
  titlePanel("Expected Points"),
  tags$head(
    # Tilføj Font Awesome for ikoner
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"),
    # Custom CSS for at style slider og play-knap
    tags$style(HTML("
      .irs--shiny .irs-handle { 
        cursor: pointer; 
      }
      .irs--shiny .irs-slider { 
        cursor: pointer; 
      }
      .js-irs-0 .irs-bar { 
        background-color: #007bff; 
        border-color: #007bff; 
      }
      .js-irs-0 .irs-single { 
        background-color: #007bff; 
        color: white; 
      }
      .play-button {
        font-size: 18px; 
        color: #007bff; 
        cursor: pointer;
        padding: 8px 14px;
        border: 2px solid #007bff;
        border-radius: 5px;
        background-color: #ffffff;
      }
      .play-button:hover {
        color: #ffffff;
        background-color: #007bff;
      }
      .slider-container {
        margin-bottom: 40px; /* Afstand mellem slider og knap */
      }
    "))
  ),
  fluidRow(
    column(
      width = 6,
      plotOutput("xpPlot", height = "700px"),
      # Overskrift for at gøre formålet klart
      h4("Afspil historisk udvikling gennem sæsonen"),
      # Slider uden tooltip-ikon
      div(
        class = "slider-container",
        sliderInput(
          "kampSlider", 
          label = "Vælg kampnummer:",
          min = 1, 
          max = 33, 
          value = 33, 
          step = 1,
          animate = animationOptions(
            interval = 500, 
            loop = FALSE,
            playButton = tags$button(
              class = "play-button",
              tags$i(class = "fa fa-play", style = "margin-right: 5px;"),
              "Afspil animation"
            ),
            pauseButton = tags$button(
              class = "play-button",
              tags$i(class = "fa fa-pause", style = "margin-right: 5px;"),
              "Pause"
            )
          )
        )
      )
    ),
    column(
      width = 6,
      h4("Statistik per hold"),
      DTOutput("xpTable")
    )
  )
)


server <- function(input, output) {
  # Load data
  xp_sum <- readRDS("xp_sum.rds")
  xp_last_frame <- readRDS("xp_last_frame.rds")
  xp_sum_wyscout <- readRDS("xp_sum_wyscout.rds")
  xp_results_matches <- readRDS("xp_results_matches.rds")
  
  # Calculate number of matches per team using the highest kamp_nummer
  matches_per_team <- xp_results_matches %>%
    group_by(Team) %>%
    summarise(kamp_nummer = max(kamp_nummer, na.rm = TRUE))
  
  # Get logo_url per team from xp_last_frame (exclude Point to avoid conflict)
  team_logos <- xp_last_frame %>%
    select(Team, logo_url) %>%
    distinct()  # Ensure one logo_url per Team
  
  # Feature engineering for xp_sum table
  xp_sum <- merge(xp_sum, xp_sum_wyscout, by = "Team") %>%
    merge(matches_per_team, by = "Team") %>%
    merge(team_logos, by = "Team") %>%
    mutate(
      Point_difference = round(Total_xP - Point, 2),
      xP_rank = rank(-Total_xP, ties.method = "first"),
      Real_rank = rank(-Point, ties.method = "first"),
      Matches_played = round(Point / (Point / Total_xP), 0),
      xP_per_match = round(Total_xP / Matches_played, 2),
      Percent_Difference = abs(Total_xP - Total_xP_wyscout) / Total_xP * 100,
      Team_with_logo = paste0(
        '<img src="', logo_url, '" width="30" height="30" style="vertical-align:middle; margin-right:5px;">',
        Team
      )
    )
  
  # Team colors
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
  
  # Prepare data for animation
  maks_kampe <- max(xp_results_matches$kamp_nummer, na.rm = TRUE)
  
  # Compute cumulative_xP in xp_results_matches
  xp_results_matches <- xp_results_matches %>%
    arrange(Team, kamp_nummer) %>%
    group_by(Team) %>%
    mutate(cumulative_xP = cumsum(Team_xP)) %>%
    ungroup()
  
  # Join with team_logos to get logo_url
  xp_results_matches <- xp_results_matches %>%
    left_join(team_logos, by = "Team")
  
  # Create xp_tidsserie_full with all kamp_nummer values
  xp_tidsserie_full <- xp_results_matches %>%
    group_by(Team) %>%
    complete(kamp_nummer = 1:maks_kampe) %>%
    fill(cumulative_xP, logo_url, .direction = "down") %>%
    replace_na(list(cumulative_xP = 0)) %>%
    ungroup()
  
  # Define rank order based on final cumulative_xP
  rank_order <- xp_tidsserie_full %>%
    group_by(Team) %>%
    filter(kamp_nummer == max(kamp_nummer, na.rm = TRUE)) %>%
    arrange(cumulative_xP) %>%
    pull(Team)
  
  xp_tidsserie_full <- xp_tidsserie_full %>%
    mutate(Team = factor(Team, levels = rank_order))
  
  # Render the plot based on the slider value
  output$xpPlot <- renderPlot({
    # Filter data for the selected kamp_nummer
    current_data <- xp_tidsserie_full %>%
      filter(kamp_nummer <= input$kampSlider) %>%
      group_by(Team, logo_url) %>%
      summarise(
        cumulative_xP = max(cumulative_xP, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      arrange(desc(cumulative_xP))
    
    # Create the plot
    ggplot(current_data, aes(x = reorder(Team, cumulative_xP), y = cumulative_xP)) +
      geom_col(aes(fill = Team), color = "black") +
      geom_image(aes(image = logo_url), size = 0.06, asp = 1.2) +
      scale_fill_manual(values = team_colors) +
      coord_flip() +
      labs(
        title = paste("xP efter", input$kampSlider, "kampe"),
        x = NULL,
        y = "Akkumuleret Expected Points"
      ) +
      theme_minimal(base_size = 16) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.y = element_text(face = "bold"),
        axis.text.x = element_text(size = 12),
        legend.position = "none"
      )
  })
  
  # Updated stats table
  output$xpTable <- renderDT({
    xp_sum %>%
      select(
        Team_with_logo,
        kamp_nummer,
        Total_xP,
        Total_xP_wyscout,
        Percent_Difference,
        Point,
        xP_rank,
        Real_rank
      ) %>%
      arrange(desc(Total_xP)) %>%
      mutate(
        Total_xP = round(Total_xP, 2),
        Total_xP_wyscout = round(Total_xP_wyscout, 2),
        Percent_Difference = round(Percent_Difference, 2)
      ) %>%
      datatable(
        rownames = FALSE,
        colnames = c("Hold", "Antal kampe", "xP (XGBOOST)", "xP (Wyscout)", "Procentvis forskel", "Aktuelle points", "xP placering", "Aktuelle placering"),
        options = list(
          pageLength = 14,
          columnDefs = list(
            list(
              targets = 0,
              render = JS(
                "function(data, type, row, meta) {",
                "  return type === 'display' ? '<div style=\"white-space: nowrap;\">' + data + '</div>' : data;",
                "}"
              ),
              width = "150px"
            )
          )
        ),
        escape = FALSE
      )
  })
}

shinyApp(ui = ui, server = server)