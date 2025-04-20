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
    # CSS for at flytte play/pause-knapperne ned og style gradient
    tags$style(HTML("
      .irs--shiny .irs-grid-text.js-grid-text-0,
      .irs--shiny .irs-grid-text.js-grid-text-1 {
        margin-top: 10px; /* Flytter knapperne lidt ned */
      }
      .dt-up-arrow {
        color: green !important; /* Grøn pil op for højere xP (XGBOOST) */
      }
      .dt-down-arrow {
        color: red !important; /* Rød pil ned for lavere xP (XGBOOST) */
      }
      .dt-equal {
        color: gray !important; /* Grå for lighed */
      }
      /* Gradient-styling for Placering forskel som tekstfarve */
      .dt-diff-0 {
        color: #228B22 !important; /* Mørk grøn for forskel 0 (bedste) */
      }
      .dt-diff-1 {
        color: #2E8B57 !important; /* Mørkere grøn for forskel ±1 */
      }
      .dt-diff-2 {
        color: #CD5C5C !important; /* Mørkere lys rød for forskel ±2 */
      }
      .dt-diff-3 {
        color: #B22222 !important; /* Mellem rød for forskel ±3 */
      }
      .dt-diff-4 {
        color: #8B0000 !important; /* Mørk rød for forskel ±4 eller mere */
      }
    "))
  ),
  fluidRow(
    column(
      width = 6,
      plotOutput("xpPlot", height = "700px"),
      h4("Afspil en historisk udvikling af xP gennem sæsonen"),
      sliderInput(
        "kampSlider", 
        label = "Kampnummer:",
        min = 1, 
        max = 33, 
        value = 33, 
        step = 1,
        animate = animationOptions(
          interval = 500, 
          loop = FALSE,
          playButton = tags$button(
            tags$i(class = "fa fa-play", style = "margin-right: 5px;"),
            "Afspil animation",
            style = "background-color: #007bff; color: white; padding: 8px 16px; border: none; border-radius: 5px; font-size: 16px;"
          ),
          pauseButton = tags$button(
            tags$i(class = "fa fa-pause", style = "margin-right: 5px;"),
            "Pause",
            style = "background-color: #dc3545; color: white; padding: 8px 16px; border: none; border-radius: 5px; font-size: 16px;"
          )
        )
      )
    ),
    column(
      width = 6,
      h4("Statistik per hold"),
      # Dropdown til at vælge hold
      selectInput(
        "teamFilter",
        label = "Filtrér efter hold:",
        choices = NULL, # Udfyldes dynamisk i serveren
        selected = NULL,
        multiple = FALSE
      ),
      DTOutput("xpTable"),
      # Ny oversigtstabel
      h4("Oversigt over xP på tværs af hold"),
      DTOutput("summaryTable")
    )
  )
)

server <- function(input, output, session) {
  # Load data
  xp_sum <- readRDS("xp_sum.rds")
  xp_last_frame <- readRDS("xp_last_frame.rds")
  xp_sum_wyscout <- readRDS("xp_sum_wyscout.rds")
  xp_results_matches <- readRDS("xp_results_matches.rds")
  xp_sum$Total_xP <- round(xp_sum$Total_xP, 0)
  xp_sum_wyscout$Total_xP_wyscout <- round(xp_sum_wyscout$Total_xP_wyscout, 0)
  
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
      ),
      # Ny kolonne for pil baseret på xP forskel
      xP_Comparison = case_when(
        Total_xP > Total_xP_wyscout ~ '<i class="fas fa-arrow-up dt-up-arrow"></i>',
        Total_xP < Total_xP_wyscout ~ '<i class="fas fa-arrow-down dt-down-arrow"></i>',
        Total_xP == Total_xP_wyscout ~ '<i class="fas fa-equals dt-equal"></i>',
        TRUE ~ ''
      ),
      # Beregn forskel mellem xP_rank og Real_rank
      Rank_Difference = xP_rank - Real_rank
    )
  
  # Opdater dropdown-valgmuligheder dynamisk
  observe({
    updateSelectInput(session, "teamFilter",
                      choices = c("Alle hold", unique(xp_sum$Team)),
                      selected = "Alle hold")
  })
  
  # Team colors som navngivet liste for at undgå jsonlite advarsel
  team_colors <- list(
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
  
  # Updated stats table (per hold)
  output$xpTable <- renderDT({
    # Filtrér data baseret på valgt hold
    table_data <- if (input$teamFilter == "Alle hold") {
      xp_sum
    } else {
      xp_sum %>% filter(Team == input$teamFilter)
    }
    
    table_data %>%
      select(
        Team_with_logo,
        kamp_nummer,
        Total_xP,
        xP_Comparison,
        Total_xP_wyscout,
        Percent_Difference,
        Point,
        xP_rank,
        Rank_Difference,
        Real_rank
      ) %>%
      arrange(desc(Total_xP)) %>%
      mutate(
        Total_xP = round(Total_xP, 2),
        Total_xP_wyscout = round(Total_xP_wyscout, 2),
        Percent_Difference = round(Percent_Difference, 2),
        # Gradient til Rank_Difference (Placering forskel) som tekstfarve
        Rank_Difference = case_when(
          abs(Rank_Difference) == 0 ~ sprintf('<span class="dt-diff-0">%s</span>', Rank_Difference),
          abs(Rank_Difference) == 1 ~ sprintf('<span class="dt-diff-1">%s</span>', Rank_Difference),
          abs(Rank_Difference) == 2 ~ sprintf('<span class="dt-diff-2">%s</span>', Rank_Difference),
          abs(Rank_Difference) == 3 ~ sprintf('<span class="dt-diff-3">%s</span>', Rank_Difference),
          abs(Rank_Difference) >= 4 ~ sprintf('<span class="dt-diff-4">%s</span>', Rank_Difference),
          TRUE ~ as.character(Rank_Difference)
        )
      ) %>%
      datatable(
        rownames = FALSE,
        colnames = c("Hold", "Antal kampe", "xP (XGBOOST)", "", "xP (Wyscout)", "Procentvis forskel", "Aktuelle points", "xP placering", "Placering forskel", "Aktuelle placering"),
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
            ),
            list(
              targets = 3, # xP_Comparison kolonne
              className = "dt-center",
              width = "30px"
            ),
            list(
              targets = 8, # Placering forskel kolonne
              className = "dt-center",
              width = "50px"
            )
          )
        ),
        escape = FALSE,
        # Tilføj tooltips til kolonneoverskrifter
        callback = JS(
          "table.column(2).header().title = 'xP (XGBOOST)<br><span title=\"Forventede point baseret på XGBOOST-modellen\">(?)</span>';",
          "table.column(3).header().title = '';",
          "table.column(4).header().title = 'xP (Wyscout)<br><span title=\"Forventede point baseret på Wyscout-data\">(?)</span>';",
          "table.column(5).header().title = 'Procentvis forskel<br><span title=\"Den procentvise forskel mellem xP (XGBOOST) og xP (Wyscout)\">(?)</span>';",
          "table.column(6).header().title = 'Aktuelle points<br><span title=\"Holdets faktiske point i sæsonen\">(?)</span>';",
          "table.column(7).header().title = 'xP placering<br><span title=\"Holdets placering baseret på forventede point (XGBOOST)\">(?)</span>';",
          "table.column(8).header().title = 'Placering forskel<br><span title=\"Forskel mellem xP placering og Aktuelle placering\">(?)</span>';",
          "table.column(9).header().title = 'Aktuelle placering<br><span title=\"Holdets faktiske placering baseret på point\">(?)</span>';",
          "return table;"
        )
      )
  })
  
  # Ny oversigtstabel
  output$summaryTable <- renderDT({
    # Beregn opsummeringsstatistikker
    summary_data <- xp_sum %>%
      summarise(
        `Gennemsnit xP (XGBOOST)` = round(mean(Total_xP, na.rm = TRUE), 2),
        `Standardafvigelse xP (XGBOOST)` = round(sd(Total_xP, na.rm = TRUE), 2),
        `Gennemsnit xP (Wyscout)` = round(mean(Total_xP_wyscout, na.rm = TRUE), 2),
        `Standardafvigelse xP (Wyscout)` = round(sd(Total_xP_wyscout, na.rm = TRUE), 2),
        `Gennemsnit Procentvis forskel` = round(mean(Percent_Difference, na.rm = TRUE), 2),
        `Min Procentvis forskel` = round(min(Percent_Difference, na.rm = TRUE), 2),
        `Max Procentvis forskel` = round(max(Percent_Difference, na.rm = TRUE), 2)
      )
    
    # Konverter til et format, der kan vises som tabel
    datatable(
      summary_data,
      rownames = FALSE,
      options = list(
        dom = 't', # Kun vis tabellen, ingen søgning eller paginering
        pageLength = 1,
        ordering = FALSE # Deaktiver sortering
      )
    )
  })
}

shinyApp(ui = ui, server = server)