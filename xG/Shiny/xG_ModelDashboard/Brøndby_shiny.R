library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(ggsoccer)
library(ggimage)
library(DT)

# Indlæs data
allshotevents2425_merged <- readRDS("allshotevents2425_merged.rds")

# Definer Brøndby IF's TEAM_WYID
brondby_team_id <- 7453

# Preprocess data for Over-/Underperformance (Side 1)
performance_data <- allshotevents2425_merged %>%
  filter(TEAM_WYID == brondby_team_id) %>%
  mutate(
    DATE = as.Date(DATE, format = "%Y-%m-%d"),
    SHOTISGOAL = as.numeric(as.character(SHOTISGOAL)),
    xG_XGB = as.numeric(as.character(xG_XGB))
  ) %>%
  filter(!is.na(DATE)) %>%
  group_by(DATE, MATCH_LABEL) %>%
  summarise(
    total_xG = sum(xG_XGB, na.rm = TRUE),
    total_goals = sum(SHOTISGOAL, na.rm = TRUE),
    total_shots = n(),
    shots_on_target = sum(as.numeric(as.character(SHOTONTARGET)), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(DATE) %>%
  mutate(
    cum_xG = cumsum(total_xG),
    cum_goals = cumsum(total_goals),
    performance_diff = total_goals - total_xG,
    performance_label = case_when(
      performance_diff > 0 ~ "Overpræsterede",
      performance_diff < 0 ~ "Underpræsterede",
      TRUE ~ "Som forventet"
    )
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
    shots = n(),
    Avg_shot_xg = round(mean(xG_XGB, na.rm = TRUE), 3),
    sd_xg = round(sd(xG_XGB, na.rm = TRUE), 3),
    xG_Variation = round(ifelse(sd_xg == 0, 0, Avg_shot_xg / sd_xg), 3),
    .groups = "drop"
  ) %>%
  mutate(
    performance_diff = total_goals - total_xG,
    performance_label = case_when(
      performance_diff > 0 ~ "Overperformer",
      performance_diff < 0 ~ "Underperformer",
      TRUE ~ "Som forventet"
    ),
    avg_xG_per_shot = ifelse(shots > 0, total_xG / shots, 0),
    finishing_efficiency = ifelse(shots > 0, total_goals / shots, 0)
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

# Preprocess data for Season Statistics (Side 5)
season_stats_data <- allshotevents2425_merged %>%
  mutate(
    SHOTISGOAL = as.numeric(as.character(SHOTISGOAL)),
    xG_XGB = as.numeric(as.character(xG_XGB)),
    Team_Type = ifelse(TEAM_WYID == brondby_team_id, "Brøndby IF", "Modstander")
  )

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Brøndby IF xG Analyse"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Brøndby IF", icon = icon("home"), startExpanded = TRUE,
               menuSubItem("Oversigt", tabName = "intro_b"),
               menuSubItem("Over-/Underperformance", tabName = "performance_b", icon = icon("chart-line")),
               #menuSubItem("xG Kort", tabName = "xg_map", icon = icon("map")),
               menuSubItem("Spilleranalyse", tabName = "player", icon = icon("user")),
               menuSubItem("Kamprapport", tabName = "match", icon = icon("futbol"))#,
               #menuSubItem("Sæsonstatistik", tabName = "season_stats", icon = icon("table"))
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro_b",
              box(
                title = "Vigtig Information",
                status = "warning",
                solidHeader = TRUE,
                width = 12,
                p(strong("OBS OBS"), " Denne Shiny er ment for opgave 1.6 - Hvordan kan Brøndby IF bruge vores model i den igangværende sæson")
              ),
              h2("Velkommen til Brøndby IF xG Analyse"),
              p("Denne dashboard bruger avanceret xG (forventede mål) modellering til at give indsigt i Brøndby IF's præstationer i den aktuelle sæson. Ved hjælp af xG_XGB-metriken analyserer vi, hvordan Brøndby præsterer i forhold til forventninger, hvor de skaber chancer, hvilke spillere der bidrager mest, og hvordan enkelte kampe udfolder sig."),
              h3("Hvad kan Brøndby IF bruge dette til?"),
              p("Analyserne hjælper Brøndby IF med at:"),
              tags$ul(
                tags$li(icon("chart-line"), " Identificere perioder med over- eller underperformance for at vurdere form og held."),
                tags$li(icon("user"), " Vurdere spilleres bidrag til målskabelse og afslutningseffektivitet for at optimere individuelle præstationer."),
                tags$li(icon("futbol"), " Analysere kampresultater og skudkvalitet for at vurdere, om resultater afspejler præstationer.")
              ),
              p("Brug sidemenuen til at udforske hver analyse.")
      ),
      tabItem(tabName = "performance_b",
              h2("Over-/Underperformance Over Tid"),
              p("Dette diagram viser kumulativ xG_XGB (forventede mål) og faktiske mål (SHOTISGOAL) over tid for Brøndby IF. Brug datovælgeren til at zoome ind på specifikke perioder. Perioder, hvor den blå linje (mål) er over den orange linje (xG), indikerer overperformance, mens det modsatte viser underperformance."),
              box(
                title = "Vælg tidsinterval",
                width = 12,
                dateRangeInput(
                  inputId = "date_range",
                  label = "Vælg datointerval:",
                  start = if (nrow(performance_data) > 0) min(performance_data$DATE, na.rm = TRUE) else Sys.Date() - 30,
                  end = if (nrow(performance_data) > 0) max(performance_data$DATE, na.rm = TRUE) else Sys.Date(),
                  min = if (nrow(performance_data) > 0) min(performance_data$DATE, na.rm = TRUE) else Sys.Date() - 365,
                  max = if (nrow(performance_data) > 0) max(performance_data$DATE, na.rm = TRUE) else Sys.Date(),
                  format = "dd-mm-yyyy",
                  language = "da"
                )
              ),
              box(
                title = "Kumulativ xG vs. Mål",
                width = 12,
                plotlyOutput("performance_plot", height = "500px")
              ),
              box(
                title = "Statistik for Over- og Underpræsterede Kampe",
                width = 12,
                DTOutput("performance_stats_table")
              )
      ),
      tabItem(tabName = "xg_map",
              h2("xG Kort på Banen"),
              p("Dette kort viser Brøndby IF's skudpositioner på banen. Punkternes farve angiver xG_XGB-værdien (højere xG = varmere farver), og formen viser, om skuddet blev mål (cirkel) eller ej (kryds). Hold musen over punkter for at se detaljer om kampen og skuddet. Brug dette til at vurdere, hvor Brøndby tager skud fra, og om deres chancer er gode nok (afstand/vinkel)."),
              box(
                title = "xG Skudkort",
                width = 12,
                plotlyOutput("xg_map", height = "600px")
              )
      ),
      tabItem(tabName = "player",
              h2("Spilleres xG Bidrag"),
              p("Denne side viser de top-10 spillere fra Brøndby IF efter samlet xG_XGB. Sammenligningen mellem faktiske mål og forventede mål (xG_XGB) viser, hvilke spillere der over- eller underperformer. Punkterne i plottet viser xG-variationen (Gns. xG / SD xG), og stregen ved 0 markerer ingen variation."),
              box(
                title = "Hvad er xG-Variation?",
                width = 12,
                status = "info",
                solidHeader = TRUE,
                p("xG-Variation måler variationen i en spillers skudkvalitet og beregnes som gennemsnitlig xG per skud divideret med standardafvigelsen af xG-værdierne (Gns. xG / SD xG)."),
                tags$ul(
                  tags$li("En høj værdi (over 1) indikerer ensartede skud – spilleren tager skud med lignende xG-værdier, hvilket kan vise en konsistent skudpositionering."),
                  tags$li("En lav værdi (under 1) indikerer varierede skud – spilleren tager skud med forskellige xG-værdier, hvilket kan afspejle en mere alsidig spillestil.")
                ),
                p("Denne metrik hjælper med at forstå, om en spillers chancer er forudsigelige eller varierede, hvilket kan bruges til at tilpasse træning og taktik.")
              ),
              box(
                title = "Top-10 Spillere: Mål og xG Variation",
                width = 12,
                plotOutput("player_plot", height = "500px")
              ),
              box(
                title = "Detaljeret Spilleroversigt",
                width = 12,
                DTOutput("player_table")
              )
      ),
      tabItem(tabName = "match",
              h2("Kampspecifik Rapport"),
              p("Vælg en kamp for at se en detaljeret analyse, inklusive xG for Brøndby IF og modstanderen, skudstatistik og et xG-kort for begge holds skud. Brug dette til at vurdere, om kampresultatet var fortjent baseret på chancerne."),
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
                title = "xG Kort for Brøndby IF og Modstander",
                width = 12,
                plotlyOutput("match_xg_map", height = "600px")
              )
      ),
      tabItem(tabName = "season_stats",
              h2("Sæsonstatistik for Brøndby IF"),
              p("Denne side giver et overblik over Brøndby IF's præstationer i 2024/25-sæsonen mod forskellige modstanderformationer og hold. Brug tabellerne og graferne til at identificere, hvilke formationer og modstandere Brøndby klarer sig bedst og dårligst imod."),
              box(
                title = "Præstation mod Formationer",
                width = 6,
                DTOutput("formation_table"),
                plotlyOutput("formation_plot", height = "400px")
              ),
              box(
                title = "Præstation mod Modstandere",
                width = 6,
                DTOutput("opponent_table"),
                plotlyOutput("opponent_plot", height = "400px")
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Over-/Underperformance (Side 1)
  filtered_performance_data <- reactive({
    req(input$date_range)
    if (nrow(performance_data) == 0) return(data.frame())
    performance_data %>%
      filter(DATE >= as.Date(input$date_range[1]) & DATE <= as.Date(input$date_range[2]))
  })
  
  output$performance_plot <- renderPlotly({
    data <- filtered_performance_data()
    if (nrow(data) == 0) {
      return(plot_ly() %>%
               layout(
                 title = "Ingen data tilgængelige for det valgte interval",
                 xaxis = list(visible = FALSE),
                 yaxis = list(visible = FALSE)
               ))
    }
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
  
  output$performance_stats_table <- renderDT({
    data <- filtered_performance_data() %>%
      filter(performance_label != "Som forventet") %>%
      mutate(
        total_xG = round(total_xG, 2),
        performance_diff = round(performance_diff, 2)
      ) %>%
      select(
        Dato = DATE,
        Kamp = MATCH_LABEL,
        xG = total_xG,
        Mål = total_goals,
        `Mål - xG` = performance_diff,
        Skud = total_shots,
        `Skud på mål` = shots_on_target,
        Præstation = performance_label
      ) %>%
      arrange(desc(Dato))
    
    datatable(
      data,
      options = list(
        pageLength = 10,
        searching = TRUE,
        lengthChange = FALSE
      ),
      rownames = FALSE
    ) %>%
      formatStyle("Præstation", backgroundColor = styleEqual(
        c("Overpræsterede", "Underpræsterede"),
        c("#E6FFE6", "#FFE6E6")
      ))
  })
  
  # xG Map (Side 2)
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
  
  # Spilleranalyse (Side 3)
  output$player_plot <- renderPlot({
    ggplot(player_data, aes(x = reorder(SHORTNAME, total_goals))) +
      geom_bar(aes(y = total_goals, fill = performance_label), stat = "identity", alpha = 0.7) +
      geom_point(aes(y = xG_Variation), color = "red", size = 3) +
      geom_hline(yintercept = 1, color = "#3B4252", linetype = "dashed", size = 1) +
      coord_flip() +
      scale_fill_manual(values = c("Overperformer" = "#087e8b", "Underperformer" = "#ff5a5f", "Som forventet" = "grey"), name = "Præstation") +
      scale_y_continuous(
        name = "Antal mål",
        sec.axis = sec_axis(~ ., name = "xG Variation (Gns. xG / SD xG)")
      ) +
      labs(
        x = "Spiller",
        y = "Antal mål"
      ) +
      theme(
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "#4C566A", size = 0.2),
        panel.grid.minor = element_line(color = "#4C566A", size = 0.1),
        text = element_text(color = "#3B4252"),
        axis.text = element_text(color = "#3B4252"),
        axis.title.x = element_text(face = "bold", color = "#3B4252", margin = ggplot2::margin(t = 10)),
        axis.title.y = element_text(face = "bold", color = "#3B4252"),
        axis.text.x = element_text(color = "#3B4252"),
        axis.text.y = element_text(color = "#3B4252"),
        legend.position = "bottom",
        legend.background = element_rect(fill = "#eaebed", color = "#eaebed"),
        legend.key = element_rect(fill = "#eaebed", color = "#eaebed"),
        legend.title = element_blank(),
        legend.text = element_text(face = "bold", color = "#3B4252"),
        plot.title = element_text(hjust = 0.5, color = "#3B4252", face = "bold"),
        plot.margin = ggplot2::margin(t = 20, r = 20, b = 40, l = 20)
      ) +
      guides(fill = guide_legend(title = NULL), point = guide_legend(title = "xG Variation", override.aes = list(shape = 16)))
  })
  
  output$player_table <- renderDT({
    datatable(
      player_data %>%
        mutate(
          Billede = sprintf('<img src="%s" height="50px"/>', IMAGEDATAURL.x)
        ) %>%
        select(
          Billede,
          Spiller = SHORTNAME,
          `Antal skud` = shots,
          `Total mål` = total_goals,
          `Gennemsnit xG per skud` = avg_xG_per_shot,
          `xG-Variation` = xG_Variation,
          `Afslutningseffektivitet` = finishing_efficiency,
          Præstation = performance_label
        ) %>%
        mutate(
          `Gennemsnit xG per skud` = round(`Gennemsnit xG per skud`, 2),
          `xG-Variation` = round(`xG-Variation`, 2),
          `Afslutningseffektivitet` = round(`Afslutningseffektivitet`, 2)
        ),
      options = list(
        pageLength = 10,
        searching = FALSE,
        lengthChange = FALSE,
        columnDefs = list(list(targets = 0, searchable = FALSE, orderable = FALSE))
      ),
      rownames = FALSE,
      escape = FALSE
    ) %>%
      formatStyle(
        "Præstation",
        backgroundColor = styleEqual(
          c("Overperformer", "Underperformer", "Som forventet"),
          c("#E6FFE6", "#FFE6E6", "#F0F0F0")
        )
      )
  })
  
  # Kamprapport (Side 4)
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
  
  output$match_xg_map <- renderPlotly({
    selected_match <- input$match_select
    match_shots <- allshotevents2425_merged %>%
      filter(MATCH_LABEL == selected_match) %>%
      mutate(
        SHOTISGOAL = as.numeric(as.character(SHOTISGOAL)),
        xG_XGB = as.numeric(as.character(xG_XGB)),
        Goal_Label = ifelse(SHOTISGOAL == 1, "Mål", "Ikke mål"),
        Team_Type = ifelse(TEAM_WYID == brondby_team_id, "Brøndby IF", "Modstander"),
        Adjusted_X = ifelse(Team_Type == "Modstander", 100 - LOCATIONX, LOCATIONX),
        Adjusted_Y = ifelse(Team_Type == "Modstander", 100 - LOCATIONY, LOCATIONY),
        Hover_Text = paste(
          "xG: ", round(xG_XGB, 3), "<br>",
          "Spiller: ", SHORTNAME, "<br>",
          "Hold: ", ifelse(TEAM_WYID == brondby_team_id, "Brøndby IF", "Modstander"), "<br>",
          "Type: ", Team_Type, "<br>",
          "Afstand: ", round(shot_distance, 2), " m", "<br>",
          "Vinkel: ", round(shot_angle, 2), " grader"
        )
      )
    
    if (nrow(match_shots) == 0) {
      p <- ggplot() +
        annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "#f5f5f5") +
        coord_fixed(xlim = c(0, 100), ylim = c(0, 100)) +
        theme_pitch() +
        labs(
          title = paste("xG Kort for", selected_match),
          subtitle = "Ingen skud tilgængelige for denne kamp."
        ) +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5),
          plot.subtitle = element_text(face = "italic", hjust = 0.5)
        )
      return(ggplotly(p))
    }
    
    p <- ggplot(match_shots, aes(x = Adjusted_X, y = Adjusted_Y, text = Hover_Text, color = Team_Type)) +
      annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "#f5f5f5") +
      geom_point(aes(size = xG_XGB, fill = xG_XGB, shape = Goal_Label), alpha = 0.7) +
      scale_fill_gradient(low = "#0D1C8A", high = "#FDBA21", name = "xG") +
      scale_shape_manual(values = c("Mål" = 21, "Ikke mål" = 4), name = "Udfald") +
      scale_size_continuous(range = c(2, 8), name = "xG") +
      scale_color_manual(values = c("Brøndby IF" = "blue", "Modstander" = "red"), name = "Hold") +
      coord_fixed(xlim = c(0, 100), ylim = c(0, 100)) +
      theme_pitch() +
      labs(
        title = paste("xG Kort for", selected_match),
        subtitle = "Brøndby IF (venstre mod højre) vs. Modstander (højre mod venstre)"
      ) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(face = "italic", hjust = 0.5),
        legend.position = "bottom"
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hovermode = "closest",
        showlegend = TRUE,
        annotations = list(
          list(
            x = 0.5, y = -0.1, text = "Hold musen over punkter for at se xG-detaljer",
            showarrow = FALSE, xref = "paper", yref = "paper"
          )
        )
      )
  })
  
  # Sæsonstatistik (Side 5)
  formation_stats <- reactive({
    match_results <- season_stats_data %>%
      group_by(MATCH_WYID.x, Team_Type) %>%
      summarise(
        total_goals = sum(SHOTISGOAL, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      tidyr::pivot_wider(
        names_from = Team_Type,
        values_from = total_goals,
        values_fill = 0
      ) %>%
      mutate(
        Result = case_when(
          `Brøndby IF` > Modstander ~ "Sejr",
          `Brøndby IF` < Modstander ~ "Tab",
          TRUE ~ "Uafgjort"
        )
      )
    
    season_stats_data %>%
      filter(Team_Type == "Modstander") %>%
      group_by(FORMATION, MATCH_WYID.x) %>%
      summarise(
        Opp_xG = sum(xG_XGB, na.rm = TRUE),
        Opp_Goals = sum(SHOTISGOAL, na.rm = TRUE),
        Opp_Shots = n(),
        .groups = "drop"
      ) %>%
      left_join(
        season_stats_data %>%
          filter(Team_Type == "Brøndby IF") %>%
          group_by(MATCH_WYID.x) %>%
          summarise(
            Brondby_xG = sum(xG_XGB, na.rm = TRUE),
            Brondby_Goals = sum(SHOTISGOAL, na.rm = TRUE),
            Brondby_Shots = n(),
            .groups = "drop"
          ),
        by = "MATCH_WYID.x"
      ) %>%
      left_join(match_results %>% select(MATCH_WYID.x, Result), by = "MATCH_WYID.x") %>%
      group_by(FORMATION) %>%
      summarise(
        Kampe = n(),
        Brondby_xG = sum(Brondby_xG, na.rm = TRUE),
        Brondby_Goals = sum(Brondby_Goals, na.rm = TRUE),
        Brondby_Shots = sum(Brondby_Shots, na.rm = TRUE),
        Opp_xG = sum(Opp_xG, na.rm = TRUE),
        Opp_Goals = sum(Opp_Goals, na.rm = TRUE),
        Opp_Shots = sum(Opp_Shots, na.rm = TRUE),
        Sejre = sum(Result == "Sejr", na.rm = TRUE),
        Uafgjorte = sum(Result == "Uafgjort", na.rm = TRUE),
        Tab = sum(Result == "Tab", na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        Brondby_xG_per_kamp = round(Brondby_xG / Kampe, 2),
        Brondby_Goals_per_kamp = round(Brondby_Goals / Kampe, 2),
        Opp_xG_per_kamp = round(Opp_xG / Kampe, 2),
        Opp_Goals_per_kamp = round(Opp_Goals / Kampe, 2)
      ) %>%
      arrange(desc(Brondby_xG_per_kamp))
  })
  
  opponent_stats <- reactive({
    match_results <- season_stats_data %>%
      group_by(MATCH_WYID.x, Team_Type) %>%
      summarise(
        total_goals = sum(SHOTISGOAL, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      tidyr::pivot_wider(
        names_from = Team_Type,
        values_from = total_goals,
        values_fill = 0
      ) %>%
      mutate(
        Result = case_when(
          `Brøndby IF` > Modstander ~ "Sejr",
          `Brøndby IF` < Modstander ~ "Tab",
          TRUE ~ "Uafgjort"
        )
      )
    
    season_stats_data %>%
      filter(Team_Type == "Modstander") %>%
      group_by(OFFICIALNAME, IMAGEDATAURL.y, MATCH_WYID.x) %>%
      summarise(
        Opp_xG = sum(xG_XGB, na.rm = TRUE),
        Opp_Goals = sum(SHOTISGOAL, na.rm = TRUE),
        Opp_Shots = n(),
        .groups = "drop"
      ) %>%
      left_join(
        season_stats_data %>%
          filter(Team_Type == "Brøndby IF") %>%
          group_by(MATCH_WYID.x) %>%
          summarise(
            Brondby_xG = sum(xG_XGB, na.rm = TRUE),
            Brondby_Goals = sum(SHOTISGOAL, na.rm = TRUE),
            Brondby_Shots = n(),
            .groups = "drop"
          ),
        by = "MATCH_WYID.x"
      ) %>%
      left_join(match_results %>% select(MATCH_WYID.x, Result), by = "MATCH_WYID.x") %>%
      group_by(OFFICIALNAME, IMAGEDATAURL.y) %>%
      summarise(
        Kampe = n(),
        Brondby_xG = sum(Brondby_xG, na.rm = TRUE),
        Brondby_Goals = sum(Brondby_Goals, na.rm = TRUE),
        Brondby_Shots = sum(Brondby_Shots, na.rm = TRUE),
        Opp_xG = sum(Opp_xG, na.rm = TRUE),
        Opp_Goals = sum(Opp_Goals, na.rm = TRUE),
        Opp_Shots = sum(Opp_Shots, na.rm = TRUE),
        Sejre = sum(Result == "Sejr", na.rm = TRUE),
        Uafgjorte = sum(Result == "Uafgjort", na.rm = TRUE),
        Tab = sum(Result == "Tab", na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        Brondby_xG_per_kamp = round(Brondby_xG / Kampe, 2),
        Brondby_Goals_per_kamp = round(Brondby_Goals / Kampe, 2),
        Opp_xG_per_kamp = round(Opp_xG / Kampe, 2),
        Opp_Goals_per_kamp = round(Opp_Goals / Kampe, 2)
      ) %>%
      arrange(desc(Brondby_xG_per_kamp))
  })
  
  output$formation_table <- renderDT({
    datatable(
      formation_stats() %>%
        select(
          Formation = FORMATION,
          Kampe,
          `Brøndby xG (gns.)` = Brondby_xG_per_kamp,
          `Brøndby Mål (gns.)` = Brondby_Goals_per_kamp,
          `Brøndby Skud` = Brondby_Shots,
          `Modstander xG (gns.)` = Opp_xG_per_kamp,
          `Modstander Mål (gns.)` = Opp_Goals_per_kamp,
          `Modstander Skud` = Opp_Shots,
          Sejre,
          Uafgjorte,
          Tab
        ),
      options = list(
        pageLength = 10,
        searching = TRUE,
        lengthChange = FALSE
      ),
      rownames = FALSE
    ) %>%
      formatStyle("Brøndby xG (gns.)", backgroundColor = styleInterval(c(1, 2), c("#FFE6E6", "#FFF0E6", "#E6FFE6")))
  })
  
  output$formation_plot <- renderPlotly({
    data <- formation_stats() %>%
      tidyr::pivot_longer(
        cols = c(Brondby_xG_per_kamp, Brondby_Goals_per_kamp),
        names_to = "Metric",
        values_to = "Value"
      ) %>%
      mutate(Metric = recode(Metric, "Brondby_xG_per_kamp" = "xG (gns.)", "Brondby_Goals_per_kamp" = "Mål (gns.)"))
    
    p <- ggplot(data, aes(x = FORMATION, y = Value, fill = Metric)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c("xG (gns.)" = "#264653", "Mål (gns.)" = "#2a9d8f")) +
      labs(
        title = "Brøndby IF Præstation mod Formationer",
        x = "Formation",
        y = "Gennemsnit pr. Kamp"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      )
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  output$opponent_table <- renderDT({
    datatable(
      opponent_stats() %>%
        mutate(
          Hold = paste0('<img src="', IMAGEDATAURL.y, '" height="30px"/> ', OFFICIALNAME)
        ) %>%
        select(
          Hold,
          Kampe,
          `Brøndby xG (gns.)` = Brondby_xG_per_kamp,
          `Brøndby Mål (gns.)` = Brondby_Goals_per_kamp,
          `Brøndby Skud` = Brondby_Shots,
          `Modstander xG (gns.)` = Opp_xG_per_kamp,
          `Modstander Mål (gns.)` = Opp_Goals_per_kamp,
          `Modstander Skud` = Opp_Shots,
          Sejre,
          Uafgjorte,
          Tab
        ),
      options = list(
        pageLength = 10,
        searching = TRUE,
        lengthChange = FALSE,
        columnDefs = list(list(targets = 0, searchable = FALSE, orderable = FALSE))
      ),
      rownames = FALSE,
      escape = FALSE
    ) %>%
      formatStyle("Brøndby xG (gns.)", backgroundColor = styleInterval(c(1, 2), c("#FFE6E6", "#FFF0E6", "#E6FFE6")))
  })
  
  output$opponent_plot <- renderPlotly({
    data <- opponent_stats() %>%
      tidyr::pivot_longer(
        cols = c(Brondby_xG_per_kamp, Brondby_Goals_per_kamp),
        names_to = "Metric",
        values_to = "Value"
      ) %>%
      mutate(Metric = recode(Metric, "Brondby_xG_per_kamp" = "xG (gns.)", "Brondby_Goals_per_kamp" = "Mål (gns.)"))
    
    p <- ggplot(data, aes(x = OFFICIALNAME, y = Value, fill = Metric)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c("xG (gns.)" = "#264653", "Mål (gns.)" = "#2a9d8f")) +
      labs(
        title = "Brøndby IF Præstation mod Modstandere",
        x = "Modstander",
        y = "Gennemsnit pr. Kamp"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      )
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
}

# Run the app
shinyApp(ui = ui, server = server)