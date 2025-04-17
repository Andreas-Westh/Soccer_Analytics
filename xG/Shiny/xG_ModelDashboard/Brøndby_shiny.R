library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(ggsoccer)
library(ggimage)
library(DT)

allshotevents2425_merged <- readRDS("allshotevents2425_merged.rds")

# Preprocess data for Brøndby IF (Side 1: Over-/Underperformance)
brondby_team_id <- 7453 # Brøndby IF's TEAM_WYID
performance_data <- allshotevents2425_merged %>%
  filter(TEAM_WYID == brondby_team_id) %>%
  mutate(
    DATE = as.Date(DATE),
    SHOTISGOAL = as.numeric(as.character(SHOTISGOAL)),
    xG_XGB = as.numeric(as.character(xG_XGB))
  ) %>%
  group_by(DATE, MATCH_LABEL) %>%
  summarise(
    total_xG = sum(xG_XGB, na.rm = TRUE),
    total_goals = sum(SHOTISGOAL, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(DATE) %>%
  mutate(
    cum_xG = cumsum(total_xG),
    cum_goals = cumsum(total_goals)
  )

# Preprocess data for xG Map (Side 2)
xg_map_data <- allshotevents2425_merged %>%
  filter(TEAM_WYID == brondby_team_id) %>%
  mutate(
    SHOTISGOAL = as.numeric(as.character(SHOTISGOAL)),
    xG_XGB = as.numeric(as.character(xG_XGB)),
    Goal_Label = ifelse(SHOTISGOAL == 1, "Mål", "Ikke mål")
  ) %>%
  select(LOCATIONX, LOCATIONY, xG_XGB, SHOTISGOAL, Goal_Label, MATCH_LABEL, DATE)

# Preprocess data for Player Analysis (Side 3)
player_data <- allshotevents2425_merged %>%
  filter(TEAM_WYID == brondby_team_id) %>%
  mutate(
    SHOTISGOAL = as.numeric(as.character(SHOTISGOAL)),
    xG_XGB = as.numeric(as.character(xG_XGB))
  ) %>%
  group_by(PLAYER_WYID, SHORTNAME, IMAGEDATAURL.x) %>%
  summarise(
    total_xG = sum(xG_XGB, na.rm = TRUE),
    total_goals = sum(SHOTISGOAL, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    performance_diff = total_goals - total_xG,
    performance_label = case_when(
      performance_diff > 0 ~ "Overperformer",
      performance_diff < 0 ~ "Underperformer",
      TRUE ~ "Som forventet"
    )
  ) %>%
  arrange(desc(total_xG)) %>%
  slice_head(n = 10)

# Preprocess data for Match Report (Side 4)
match_data <- allshotevents2425_merged %>%
  mutate(
    SHOTISGOAL = as.numeric(as.character(SHOTISGOAL)),
    xG_XGB = as.numeric(as.character(xG_XGB)),
    SHOTONTARGET = as.numeric(as.character(SHOTONTARGET)),
    Goal_Label = ifelse(SHOTISGOAL == 1, "Mål", "Ikke mål")
  ) %>%
  group_by(MATCH_LABEL, TEAM_WYID, IMAGEDATAURL.y) %>%
  summarise(
    total_xG = sum(xG_XGB, na.rm = TRUE),
    total_shots = n(),
    shots_on_target = sum(SHOTONTARGET, na.rm = TRUE),
    total_goals = sum(SHOTISGOAL, na.rm = TRUE),
    .groups = "drop"
  )

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Brøndby IF xG Analyse"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduktion", tabName = "intro", icon = icon("home")),
      menuItem("Over-/Underperformance", tabName = "performance", icon = icon("chart-line")),
      menuItem("xG Kort", tabName = "xg_map", icon = icon("map")),
      menuItem("Spilleranalyse", tabName = "player", icon = icon("user")),
      menuItem("Kamprapport", tabName = "match", icon = icon("futbol"))
    )
  ),
  dashboardBody(
    tabItems(
      # Introduktion
      tabItem(tabName = "intro",
              h2("Velkommen til Brøndby IF xG Analyse"),
              p("Denne dashboard bruger avanceret xG (forventede mål) modellering til at give indsigt i Brøndby IF's præstationer i den aktuelle sæson. Ved hjælp af xG_XGB-metriken analyserer vi, hvordan Brøndby præsterer i forhold til forventninger, hvor de skaber chancer, hvilke spillere der bidrager mest, og hvordan enkelte kampe udfolder sig."),
              h3("Hvad kan Brøndby IF bruge dette til?"),
              p("Analyserne hjælper Brøndby IF med at:"),
              tags$ul(
                tags$li("Identificere perioder med over- eller underperformance for at vurdere form og held."),
                tags$li("Evaluere skudkvalitet og positionering for at optimere angrebsstrategier."),
                tags$li("Vurdere spilleres bidrag til chance-skabelse og afslutningseffektivitet."),
                tags$li("Analysere kampresultater for at forstå, om resultater afspejler præstationer.")
              ),
              p("Brug sidemenuen til at udforske hver analyse.")
      ),
      # Over-/Underperformance
      tabItem(tabName = "performance",
              h2("Over-/Underperformance Over Tid"),
              p("Dette diagram viser kumulativ xG_XGB (forventede mål) og faktiske mål (SHOTISGOAL) over tid for Brøndby IF. Brug datovælgeren til at zoome ind på specifikke perioder. Perioder, hvor den blå linje (mål) er over den orange linje (xG), indikerer overperformance, mens det modsatte viser underperformance."),
              box(
                title = "Vælg tidsinterval",
                width = 12,
                dateRangeInput(
                  inputId = "date_range",
                  label = "Vælg datointerval:",
                  start = min(performance_data$DATE, na.rm = TRUE),
                  end = max(performance_data$DATE, na.rm = TRUE),
                  min = min(performance_data$DATE, na.rm = TRUE),
                  max = max(performance_data$DATE, na.rm = TRUE),
                  format = "dd-mm-yyyy",
                  language = "da"
                )
              ),
              box(
                title = "Kumulativ xG vs. Mål",
                width = 12,
                plotlyOutput("performance_plot", height = "500px")
              )
      ),
      # xG Kort
      tabItem(tabName = "xg_map",
              h2("xG Kort på Banen"),
              p("Dette kort viser Brøndby IF's skudpositioner på banen. Punkternes farve angiver xG_XGB-værdien (højere xG = varmere farver), og formen viser, om skuddet blev mål (cirkel) eller ej (kryds). Hold musen over punkter for at se detaljer om kampen og skuddet. Brug dette til at vurdere, hvor Brøndby tager skud fra, og om deres chancer er gode nok (afstand/vinkel)."),
              box(
                title = "xG Skudkort",
                width = 12,
                plotlyOutput("xg_map", height = "600px")
              )
      ),
      # Spilleranalyse
      tabItem(tabName = "player",
              h2("Spilleres xG Bidrag"),
              p("Denne side viser de top-10 spillere fra Brøndby IF efter samlet xG_XGB. Sammenligningen mellem forventede mål (xG_XGB) og faktiske mål viser, hvilke spillere der over- eller underperformer. Brug dette til at vurdere, hvem der skaber chancer, og hvem der er kliniske afsluttere."),
              box(
                title = "Top-10 Spillere efter xG",
                width = 12,
                plotlyOutput("player_plot", height = "500px")
              ),
              box(
                title = "Detaljeret Spilleroversigt",
                width = 12,
                DTOutput("player_table")
              )
      ),
      # Kamprapport
      tabItem(tabName = "match",
              h2("Kampspecifik Rapport"),
              p("Vælg en kamp for at se en detaljeret analyse, inklusive xG for Brøndby IF og modstanderen, skudstatistik og et xG-kort for Brøndby’s skud. Brug dette til at vurdere, om kampresultatet var fortjent baseret på chancerne."),
              box(
                title = "Vælg kamp",
                width = 12,
                selectInput("match_select", "Vælg kamp:", 
                            choices = unique(allshotevents2425_merged$MATCH_LABEL[allshotevents2425_merged$TEAM_WYID == brondby_team_id]))
              ),
              box(
                title = "Kampoversigt",
                width = 12,
                uiOutput("match_summary")
              ),
              box(
                title = "xG Kort for Brøndby IF",
                width = 12,
                plotlyOutput("match_xg_map", height = "600px")
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data for performance plot (Side 1)
  filtered_performance_data <- reactive({
    performance_data %>%
      filter(DATE >= input$date_range[1] & DATE <= input$date_range[2])
  })
  
  # Performance plot (Side 1)
  output$performance_plot <- renderPlotly({
    data <- filtered_performance_data()
    
    plot_ly(data) %>%
      add_trace(
        x = ~DATE,
        y = ~cum_xG,
        type = "scatter",
        mode = "lines+markers",
        name = "Kumulativ xG",
        line = list(color = "orange"),
        marker = list(color = "orange"),
        text = ~paste("Kamp: ", MATCH_LABEL, "<br>Dato: ", DATE, "<br>xG: ", round(total_xG, 2), "<br>Kumulativ xG: ", round(cum_xG, 2)),
        hoverinfo = "text"
      ) %>%
      add_trace(
        x = ~DATE,
        y = ~cum_goals,
        type = "scatter",
        mode = "lines+markers",
        name = "Kumulative Mål",
        line = list(color = "blue"),
        marker = list(color = "blue"),
        text = ~paste("Kamp: ", MATCH_LABEL, "<br>Dato: ", DATE, "<br>Mål: ", total_goals, "<br>Kumulative Mål: ", cum_goals),
        hoverinfo = "text"
      ) %>%
      layout(
        title = "Kumulativ xG vs. Faktiske Mål Over Tid",
        xaxis = list(title = "Dato", tickformat = "%d-%m-%Y"),
        yaxis = list(title = "Kumulativ Værdi"),
        hovermode = "closest",
        legend = list(x = 0.1, y = 0.9)
      )
  })
  
  # xG Map plot (Side 2)
  output$xg_map <- renderPlotly({
    p <- ggplot(xg_map_data, aes(x = LOCATIONX, y = LOCATIONY)) +
      annotate_pitch(dimensions = pitch_wyscout) +
      theme_pitch() +
      geom_point(aes(size = xG_XGB, fill = xG_XGB, shape = Goal_Label), alpha = 0.7) +
      scale_fill_gradient(low = "blue", high = "red", name = "xG Værdi") +
      scale_shape_manual(values = c("Mål" = 21, "Ikke mål" = 4), name = "Udfald") +
      scale_size_continuous(range = c(2, 8), name = "xG Værdi") +
      coord_fixed() +
      labs(
        title = "Brøndby IF Skudpositioner (xG Kort)",
        x = NULL, y = NULL
      ) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)
      )
    
    ggplotly(p, tooltip = c("x", "y", "fill", "shape", "text")) %>%
      layout(
        hovermode = "closest",
        annotations = list(
          list(
            x = 0.5, y = -0.1, text = "Hold musen over punkter for at se kamp og xG-detaljer",
            showarrow = FALSE, xref = "paper", yref = "paper"
          )
        )
      )
  })
  
  # Player plot (Side 3)
  output$player_plot <- renderPlotly({
    p <- ggplot(player_data, aes(x = reorder(SHORTNAME, total_xG), y = total_xG)) +
      geom_col(aes(fill = performance_label), alpha = 0.7) +
      geom_image(aes(image = IMAGEDATAURL.x), size = 0.05, nudge_y = max(player_data$total_xG) * 0.1) +
      scale_fill_manual(values = c("Overperformer" = "green", "Underperformer" = "red", "Som forventet" = "grey"), name = "Præstation") +
      labs(
        title = "Top-10 Spillere efter xG_XGB",
        x = "Spiller",
        y = "Samlet xG_XGB"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)
      )
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  # Player table (Side 3)
  output$player_table <- renderDT({
    datatable(
      player_data %>%
        mutate(
          Billede = sprintf('<img src="%s" height="50px"/>', IMAGEDATAURL.x)
        ) %>%
        select(
          Billede,
          Spiller = SHORTNAME,
          `Samlet xG` = total_xG,
          Mål = total_goals,
          `Mål - xG` = performance_diff,
          Præstation = performance_label
        ) %>%
        mutate(
          `Samlet xG` = round(`Samlet xG`, 2),
          `Mål - xG` = round(`Mål - xG`, 2)
        ),
      options = list(
        pageLength = 10,
        searching = FALSE,
        lengthChange = FALSE,
        columnDefs = list(list(targets = 0, searchable = FALSE, orderable = FALSE))
      ),
      rownames = FALSE,
      escape = FALSE
    )
  })
  
  # Match summary (Side 4)
  output$match_summary <- renderUI({
    selected_match <- input$match_select
    match_stats <- match_data %>% filter(MATCH_LABEL == selected_match)
    
    brondby_stats <- match_stats %>% filter(TEAM_WYID == brondby_team_id)
    opponent_stats <- match_stats %>% filter(TEAM_WYID != brondby_team_id)
    
    brondby_logo <- brondby_stats$IMAGEDATAURL.y[1]
    opponent_logo <- opponent_stats$IMAGEDATAURL.y[1]
    
    tagList(
      fluidRow(
        column(6,
               h3("Brøndby IF"),
               img(src = brondby_logo, height = "50px"),
               p(sprintf("xG: %.2f", brondby_stats$total_xG)),
               p(sprintf("Skud: %d", brondby_stats$total_shots)),
               p(sprintf("Skud på mål: %d", brondby_stats$shots_on_target)),
               p(sprintf("Mål: %d", brondby_stats$total_goals))
        ),
        column(6,
               h3("Modstander"),
               img(src = opponent_logo, height = "50px"),
               p(sprintf("xG: %.2f", opponent_stats$total_xG)),
               p(sprintf("Skud: %d", opponent_stats$total_shots)),
               p(sprintf("Skud på mål: %d", opponent_stats$shots_on_target)),
               p(sprintf("Mål: %d", opponent_stats$total_goals))
        )
      ),
      p("Sammenlign xG og skudstatistik for at vurdere, om resultatet var fortjent. Et højere xG indikerer bedre chancer.")
    )
  })
  
  # Match xG Map (Side 4)
  output$match_xg_map <- renderPlotly({
    selected_match <- input$match_select
    match_shots <- allshotevents2425_merged %>%
      filter(MATCH_LABEL == selected_match, TEAM_WYID == brondby_team_id) %>%
      mutate(
        SHOTISGOAL = as.numeric(as.character(SHOTISGOAL)),
        xG_XGB = as.numeric(as.character(xG_XGB)),
        Goal_Label = ifelse(SHOTISGOAL == 1, "Mål", "Ikke mål")
      )
    
    p <- ggplot(match_shots, aes(x = LOCATIONX, y = LOCATIONY)) +
      annotate_pitch(dimensions = pitch_wyscout) +
      theme_pitch() +
      geom_point(aes(size = xG_XGB, fill = xG_XGB, shape = Goal_Label), alpha = 0.7) +
      scale_fill_gradient(low = "blue", high = "red", name = "xG Værdi") +
      scale_shape_manual(values = c("Mål" = 21, "Ikke mål" = 4), name = "Udfald") +
      scale_size_continuous(range = c(2, 8), name = "xG Værdi") +
      coord_fixed() +
      labs(
        title = paste("Brøndby IF Skudpositioner i", selected_match),
        x = NULL, y = NULL
      ) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)
      )
    
    ggplotly(p, tooltip = c("x", "y", "fill", "shape", "text")) %>%
      layout(
        hovermode = "closest",
        annotations = list(
          list(
            x = 0.5, y = -0.1, text = "Hold musen over punkter for at se xG-detaljer",
            showarrow = FALSE, xref = "paper", yref = "paper"
          )
        )
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)