library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(ggplot2)
library(pROC)
library(dplyr)
library(ggsoccer)
library(plotly)
library(DT)
library(ggtext)

# -- Indlæs data og modeller --
allshotevents <- readRDS("allshotevents.rds")
test_data <- readRDS("test_data.rds")
test_data_yn <- readRDS("test_data_yn.rds")

xgb_pred <- readRDS("xgb_pred.rds")
rf_test <- readRDS("rf_test.rds")
tree_probs <- readRDS("tree_probs.rds")
glm_probs <- readRDS("glm_probs.rds")

# -- ROC-objekter --
rf_roc   <- roc(response = test_data_yn$SHOTISGOAL, predictor = rf_test)
tree_roc <- roc(response = test_data_yn$SHOTISGOAL, predictor = tree_probs)
xgb_roc  <- roc(response = test_data_yn$SHOTISGOAL, predictor = xgb_pred)
glm_roc  <- roc(response = test_data_yn$SHOTISGOAL, predictor = glm_probs)
wyscout_roc <- roc(response = test_data$SHOTISGOAL, predictor = test_data$SHOTXG)

# -- Dataframes til ROC-plot --
model_colors <- c(
  "GLM" = "#1B9E77", 
  "XGBoost" = "#D95F02", 
  "Random Forest" = "#7570B3", 
  "Decision Tree" = "#E7298A", 
  "WyScout" = "#66A61E"
)

roc_data_all <- bind_rows(
  data.frame(
    FPR = 1 - rf_roc$specificities, 
    TPR = rf_roc$sensitivities, 
    Model = "Random Forest",
    AUC = rep(as.numeric(auc(rf_roc)), length(rf_roc$specificities))
  ),
  data.frame(
    FPR = 1 - tree_roc$specificities, 
    TPR = tree_roc$sensitivities, 
    Model = "Decision Tree",
    AUC = rep(as.numeric(auc(tree_roc)), length(tree_roc$specificities))
  ),
  data.frame(
    FPR = 1 - xgb_roc$specificities, 
    TPR = xgb_roc$sensitivities, 
    Model = "XGBoost",
    AUC = rep(as.numeric(auc(xgb_roc)), length(xgb_roc$specificities))
  ),
  data.frame(
    FPR = 1 - glm_roc$specificities, 
    TPR = glm_roc$sensitivities, 
    Model = "GLM",
    AUC = rep(as.numeric(auc(glm_roc)), length(glm_roc$specificities))
  ),
  data.frame(
    FPR = 1 - wyscout_roc$specificities, 
    TPR = wyscout_roc$sensitivities, 
    Model = "WyScout",
    AUC = rep(as.numeric(auc(wyscout_roc)), length(wyscout_roc$specificities))
  )
) %>%
  mutate(AUC_label = sprintf("%.4f", AUC))

# Liste af ROC-objekter til sammenligning
roc_list <- list(
  "WyScout" = wyscout_roc,
  "Random Forest" = rf_roc,
  "Decision Tree" = tree_roc,
  "GLM" = glm_roc
)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "xG Model Evaluering"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("📘 Introduktion", tabName = "intro", icon = icon("info-circle")),
      menuItem("xG Fordeling", tabName = "xg_overview", icon = icon("bullseye")),
      menuItem("Model Performance", tabName = "performance", icon = icon("chart-line")),
      menuItem("Hold Analyse", tabName = "xg_team_view", icon = icon("shield-alt"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .box.box-primary { border-top-color: #D95F02; }
        .box.box-info { border-top-color: #66A61E; }
        .box.box-warning { border-top-color: #7570B3; }
        .skin-blue .main-header .logo { background-color: #D95F02; }
        .skin-blue .main-header .navbar { background-color: #D95F02; }
      "))
    ),
    tabItems(
      # Introduktion
      tabItem(tabName = "intro",
              h2("Evaluering af xG-modeller for Superligaen 2023/2024"),
              fluidRow(
                valueBoxOutput("xgb_auc_box", width = 4),
                column(width = 4,
                       valueBoxOutput("wyscout_auc_box", width = NULL),
                       selectInput("comparison_model", 
                                   "Sammenlign XGBoost med:", 
                                   choices = c("WyScout", "Random Forest", "Decision Tree", "GLM"),
                                   selected = "WyScout")
                ),
                valueBoxOutput("improvement_box", width = 4)
              ),
              p("Denne app præsenterer resultaterne fra vores xG-model baseret på XGBoost, evalueret mod WyScout og andre modeller (Random Forest, Decision Tree, GLM) for Superligaen 2023/24. XGBoost opnår en højere AUC end WyScout, hvilket indikerer bedre evne til at skelne mellem mål og ikke-mål. Visualiseringerne viser xG-fordelinger, modelpræstation og holdspecifikke analyser."),
              p("Brug menuen til at udforske:"),
              tags$ul(
                tags$li(tags$b("xG Fordeling"), ": Sammenlign xG-fordelinger på banen for XGBoost og WyScout.", style = "color: #D95F02;"),
                tags$li(tags$b("Model Performance"), ": Se ROC-kurver og AUC for alle modeller.", style = "color: #7570B3;"),
                tags$li(tags$b("Hold Analyse"), ": Dyk ned i xG for specifikke hold og kampe.", style = "color: #66A61E;")
              )
      ),
      # xG Fordeling
      tabItem(tabName = "xg_overview",
              fluidRow(
                box(title = "Vælg model og visning", width = 3, status = "primary", solidHeader = TRUE,
                    checkboxGroupInput("xg_models_selected", "Vælg modeller:",
                                       choices = c("XGBoost", "WyScout"),
                                       selected = c("XGBoost")),
                    checkboxInput("show_details", "Vis detaljer", value = FALSE)
                ),
                box(title = "xG Fordeling på Banen", width = 9, status = "info", solidHeader = TRUE,
                    plotOutput("xg_plot", height = "600px"))
              ),
              fluidRow(
                box(title = "Statistisk Oversigt", width = 6, status = "warning", solidHeader = TRUE,
                    tableOutput("xg_summary_table")),
                box(title = "Gennemsnitlig xG pr. Zone", width = 6, status = "warning", solidHeader = TRUE,
                    tableOutput("xg_zone_table"))
              )
      ),
      # Model Performance
      tabItem(tabName = "performance",
              fluidRow(
                box(title = "Vælg modeller", width = 3, status = "primary", solidHeader = TRUE,
                    checkboxGroupInput("selected_models", label = NULL,
                                       choices = unique(roc_data_all$Model),
                                       selected = c("XGBoost", "WyScout"))
                ),
                box(title = "ROC-kurve", width = 9, status = "warning", solidHeader = TRUE,
                    plotOutput("roc_plot", height = "500px"))
              ),
              fluidRow(
                box(title = "AUC Sammenligning", width = 12, status = "info", solidHeader = TRUE,
                    DTOutput("auc_table"))
              )
      ),
      # Hold Analyse
      tabItem(tabName = "xg_team_view",
              fluidRow(
                box(title = "Valg af hold og kamp", width = 3, status = "primary", solidHeader = TRUE,
                    selectInput("selected_team", "Vælg hold:", choices = sort(unique(allshotevents$TEAMNAME.x))),
                    uiOutput("match_selector"),
                    checkboxGroupInput("model_choice", "Vælg model:", choices = c("XGBoost", "WyScout"), selected = c("XGBoost"))
                ),
                box(title = "Skudpositioner", width = 9, status = "info", solidHeader = TRUE,
                    plotlyOutput("team_xg_plot", height = "600px"))
              ),
              fluidRow(
                box(title = "Statistisk Oversigt", width = 6, status = "warning", solidHeader = TRUE,
                    tableOutput("xg_summary_table_team")),
                box(title = "Gennemsnitlig xG pr. Zone", width = 6, status = "warning", solidHeader = TRUE,
                    tableOutput("xg_zone_table_team"))
              ),
              fluidRow(
                box(title = "Holdoversigt", width = 12, status = "warning", solidHeader = TRUE,
                    DTOutput("team_summary_table"))
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Introduktion: Value Boxes
  output$xgb_auc_box <- renderValueBox({
    valueBox(
      sprintf("%.4f", auc(xgb_roc)), "XGBoost AUC", icon = icon("chart-line"), color = "orange"
    )
  })
  
  output$wyscout_auc_box <- renderValueBox({
    selected_roc <- roc_list[[input$comparison_model]]
    valueBox(
      sprintf("%.4f", auc(selected_roc)), 
      paste(input$comparison_model, "AUC"), 
      icon = icon("chart-line"), 
      color = "green"
    )
  })
  
  output$improvement_box <- renderValueBox({
    selected_roc <- roc_list[[input$comparison_model]]
    valueBox(
      sprintf("%.2f%%", (auc(xgb_roc) - auc(selected_roc)) / auc(selected_roc) * 100),
      paste("XGBoost Forbedring over", input$comparison_model), 
      icon = icon("arrow-up"), 
      color = "blue"
    )
  })
  
  # xG Fordeling
  output$xg_plot <- renderPlot({
    req(length(input$xg_models_selected) > 0)
    
    # Forbered data baseret på valgte modeller
    df <- allshotevents %>%
      mutate(
        Goal_Label = ifelse(SHOTISGOAL == 1, "Mål", "Ikke mål"),
        Model = "XGBoost",
        xG = xG_XGB
      ) %>%
      bind_rows(
        allshotevents %>%
          mutate(
            Goal_Label = ifelse(SHOTISGOAL == 1, "Mål", "Ikke mål"),
            Model = "WyScout",
            xG = SHOTXG
          )
      ) %>%
      filter(Model %in% input$xg_models_selected)
    
    # Hvis "Vis detaljer" er markeret, vis det detaljerede plot
    if (input$show_details) {
      p <- ggplot(df, aes(x = LOCATIONX, y = LOCATIONY)) +
        annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "#f5f5f5") +
        geom_point(aes(size = xG, fill = xG, shape = Goal_Label), alpha = 0.7) +
        scale_fill_gradient(low = "#0D1C8A", high = "#FDBA21", name = "xG") +
        scale_shape_manual(values = c("Mål" = 21, "Ikke mål" = 4), name = "Udfald") +
        scale_size_continuous(range = c(2, 8), name = "xG") +
        facet_wrap(~Model, ncol = 1) +
        coord_fixed(xlim = c(0, 100), ylim = c(0, 100)) +
        theme_pitch() +
        labs(
          title = "xG Fordeling på Banen",
          subtitle = "Sammenligning af XGBoost og WyScout xG-værdier"
        ) +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
          plot.subtitle = element_text(hjust = 0.5),
          strip.text = element_text(face = "bold", size = 12)
        )
    } else {
      # Ellers vis det simple plot
      p <- ggplot(df, aes(x = LOCATIONX, y = LOCATIONY)) +
        annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "white") +
        geom_point(aes(size = xG, color = xG), alpha = 0.6) +
        scale_color_gradient(low = "#0D1C8A", high = "#FDBA21") +
        scale_size(range = c(1.5, 6)) +
        facet_wrap(~Model, ncol = 1) +
        coord_fixed(xlim = c(0, 100), ylim = c(0, 100)) +
        theme_pitch() +
        labs(
          title = "xGs størrelse udvikler sig som forventet ift. afstand til mål",
          subtitle = "Størrelse og farve repræsenterer xG-værdien",
          color = "xG"
        ) +
        guides(size = "none") +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5),
          plot.subtitle = element_text(face = "italic", hjust = 0.5),
          strip.text = element_text(face = "bold", size = 12)
        )
    }
    
    print(p)
  })
  
  output$xg_summary_table <- renderTable({
    allshotevents %>%
      mutate(SHOTISGOAL = factor(SHOTISGOAL, levels = c(0, 1), labels = c("Ikke mål", "Mål"))) %>%
      group_by(Gruppe = SHOTISGOAL) %>%
      summarise(
        `WyScout – Gennemsnit` = mean(SHOTXG, na.rm = TRUE),
        `WyScout – SD` = sd(SHOTXG, na.rm = TRUE),
        `XGBoost – Gennemsnit` = mean(xG_XGB, na.rm = TRUE),
        `XGBoost – SD` = sd(xG_XGB, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      bind_rows(
        allshotevents %>%
          summarise(
            Gruppe = "Alle skud",
            `WyScout – Gennemsnit` = mean(SHOTXG, na.rm = TRUE),
            `WyScout – SD` = sd(SHOTXG, na.rm = TRUE),
            `XGBoost – Gennemsnit` = mean(xG_XGB, na.rm = TRUE),
            `XGBoost – SD` = sd(xG_XGB, na.rm = TRUE)
          )
      )
  }, digits = 3)
  
  # Tabel: Gennemsnitlig xG pr. zone med standardafvigelse
  output$xg_zone_table <- renderTable({
    # Definér zoner baseret på opdaterede koordinater fra Wyscout-dokumentationen
    df_zones <- allshotevents %>%
      mutate(
        Zone = case_when(
          # Målområdet (det lille felt tæt på målet)
          LOCATIONX >= 94 & LOCATIONX <= 100 & LOCATIONY >= 37 & LOCATIONY <= 63 ~ "Målområdet",
          # Det store felt (straffesparksfeltet, ekskl. målområdet)
          LOCATIONX >= 84 & LOCATIONX < 94 & LOCATIONY >= 19 & LOCATIONY <= 81 ~ "Det store felt",
          # Uden for feltet
          TRUE ~ "Uden for feltet"
        )
      ) %>%
      group_by(Zone) %>%
      summarise(
        `XGBoost – Gennemsnit` = mean(xG_XGB, na.rm = TRUE),
        `XGBoost – SD` = sd(xG_XGB, na.rm = TRUE),
        `WyScout – Gennemsnit` = mean(SHOTXG, na.rm = TRUE),
        `WyScout – SD` = sd(SHOTXG, na.rm = TRUE),
        `Antal skud` = n(),
        .groups = "drop"
      ) %>%
      # Sorter zoner i en logisk rækkefølge
      mutate(Zone = factor(Zone, levels = c("Målområdet", "Det store felt", "Uden for feltet"))) %>%
      arrange(Zone)
    
    df_zones
  }, digits = 3)
  
  # Model Performance
  output$roc_plot <- renderPlot({
    df <- roc_data_all %>% filter(Model %in% input$selected_models)
    
    p <- ggplot(df, aes(x = FPR, y = TPR, color = Model, fill = Model)) +
      geom_ribbon(aes(ymin = 0, ymax = TPR), alpha = 0.2, color = NA) +
      geom_line(size = 1.5) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray60") +
      scale_color_manual(values = model_colors) +
      scale_fill_manual(values = model_colors) +
      geom_text(
        aes(x = 0.7, y = seq(0.4, 0.2, length.out = n_distinct(Model)), label = paste("AUC:", unique(AUC_label))),
        data = df %>% group_by(Model) %>% slice(1),
        show.legend = FALSE,
        size = 5
      ) +
      labs(
        title = "ROC-kurve for Modeller",
        subtitle = "Sammenligning af præstation på testdatasæt",
        x = "False Positive Rate",
        y = "True Positive Rate"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom"
      )
    
    print(p)
  })
  
  output$auc_table <- renderDT({
    datatable(
      roc_data_all %>%
        group_by(Model) %>%
        summarise(AUC = round(unique(AUC), 4), .groups = "drop") %>%
        arrange(desc(AUC)),
      options = list(pageLength = 5, searching = FALSE, lengthChange = FALSE),
      rownames = FALSE
    ) %>%
      formatStyle("AUC", backgroundColor = styleInterval(c(0.8, 0.85), c("#FFE6E6", "#FFF0E6", "#E6FFE6")))
  })
  
  # Hold Analyse: Hold- og kampvælger
  output$match_selector <- renderUI({
    req(input$selected_team)
    # Log valgt hold
    message("Valgt hold: ", input$selected_team)
    # Hent unikke kampe for det valgte hold
    matches <- allshotevents %>%
      filter(TEAMNAME.x == input$selected_team) %>%
      distinct(MATCH_WYID.x, MATCH_LABEL) %>%
      filter(!is.na(MATCH_LABEL)) %>%
      arrange(MATCH_WYID.x)
    
    # Log antallet af kampe
    message("Antal kampe for ", input$selected_team, ": ", nrow(matches))
    
    # Hvis der ikke er nogen gyldige kampe, returner en tom dropdown
    if (nrow(matches) == 0) {
      return(selectInput("selected_match", "Vælg kamp:", choices = c("Ingen kampe tilgængelige" = "")))
    }
    
    # Opret en named list: label som navn, ID som værdi
    match_choices <- setNames(matches$MATCH_WYID.x, matches$MATCH_LABEL)
    match_choices <- as.list(c("Alle kampe" = "Alle kampe", match_choices))
    
    selectInput("selected_match", "Vælg kamp:", choices = match_choices)
  })
  
  get_filtered_team_data <- reactive({
    # Log starten af funktionen
    message("Kører get_filtered_team_data...")
    message("Valgt hold: ", input$selected_team)
    message("Valgt kamp: ", input$selected_match)
    
    # Find alle MATCH_WYID.x, hvor det valgte hold deltog
    match_ids <- allshotevents %>%
      filter(TEAMNAME.x == input$selected_team) %>%
      distinct(MATCH_WYID.x) %>%
      pull(MATCH_WYID.x)
    
    # Log antallet af kampe
    message("Antal kampe fundet for ", input$selected_team, ": ", length(match_ids))
    
    # Hent alle skud fra disse kampe
    df <- allshotevents %>%
      filter(MATCH_WYID.x %in% match_ids)
    
    # Log antallet af rækker efter match-filtrering
    message("Antal rækker efter match-filtrering: ", nrow(df))
    
    # Hvis en specifik kamp er valgt, filtrer yderligere
    if (!is.null(input$selected_match) && input$selected_match != "Alle kampe" && input$selected_match != "") {
      df <- df %>% filter(MATCH_WYID.x == input$selected_match)
      # Log antallet af rækker efter kampfiltrering
      message("Antal rækker efter specifik kampfiltrering: ", nrow(df))
    }
    
    df
  })
  
  output$team_xg_plot <- renderPlotly({
    req(input$model_choice)
    df <- get_filtered_team_data()
    
    # Log antallet af rækker i df
    message("Antal rækker i df (team_xg_plot): ", nrow(df))
    
    # Hvis der ikke er nogen rækker, vis en tom bane med en besked
    if (nrow(df) == 0) {
      p <- ggplot() +
        annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "#f5f5f5") +
        coord_fixed(xlim = c(0, 100), ylim = c(0, 100)) +
        theme_pitch() +
        labs(
          title = sprintf("xG for %s", input$selected_team),
          subtitle = "Ingen skud tilgængelige for dette hold/kamp."
        ) +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5),
          plot.subtitle = element_text(face = "italic", hjust = 0.5)
        )
      return(ggplotly(p))
    }
    
    # Hent MATCH_LABEL for den valgte kamp (hvis relevant)
    selected_match_label <- if (!is.null(input$selected_match) && input$selected_match != "Alle kampe" && input$selected_match != "") {
      df %>%
        filter(MATCH_WYID.x == input$selected_match) %>%
        pull(MATCH_LABEL) %>%
        unique() %>%
        first()
    } else {
      "Hele sæsonen"
    }
    
    # Forbered data baseret på valgt model og spejl koordinater for modstanderskud
    df <- df %>%
      mutate(
        Goal_Label = ifelse(SHOTISGOAL == 1, "Mål", "Ikke mål"),
        Model = case_when(
          "XGBoost" %in% input$model_choice ~ "XGBoost",
          "WyScout" %in% input$model_choice ~ "WyScout",
          TRUE ~ "XGBoost" # Default til XGBoost, hvis ingen model er valgt
        ),
        xG = case_when(
          Model == "XGBoost" ~ xG_XGB,
          Model == "WyScout" ~ SHOTXG
        ),
        # Identificer om det er det valgte hold eller modstanderen baseret på TEAMNAME.y
        Team_Type = ifelse(TEAMNAME.y == input$selected_team, "Valgt Hold", "Modstander"),
        # Spejl koordinater for modstanderskud
        Adjusted_X = ifelse(Team_Type == "Modstander", 100 - LOCATIONX, LOCATIONX),
        Adjusted_Y = ifelse(Team_Type == "Modstander", 100 - LOCATIONY, LOCATIONY),
        # Tekst til hover-tooltip
        Hover_Text = paste(
          "xG_XGB: ", round(xG_XGB, 3), "<br>",
          "Spiller: ", SHORTNAME.y, "<br>",
          "Hold: ", TEAMNAME.y, "<br>",
          "Type: ", Team_Type
        )
      ) %>%
      filter(Model %in% input$model_choice)
    
    # Log antallet af rækker efter model-filtrering
    message("Antal rækker efter model-filtrering: ", nrow(df))
    
    # Log antallet af skud fra hvert hold
    message("Antal skud fra Valgt Hold: ", nrow(df %>% filter(Team_Type == "Valgt Hold")))
    message("Antal skud fra Modstander: ", nrow(df %>% filter(Team_Type == "Modstander")))
    
    # Grundlæggende plotopsætning med justerede koordinater
    p <- ggplot(df, aes(x = Adjusted_X, y = Adjusted_Y, text = Hover_Text, color = Team_Type)) +
      annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "#f5f5f5") +
      geom_point(aes(size = xG, fill = xG, shape = Goal_Label), alpha = 0.7) +
      scale_fill_gradient(low = "#0D1C8A", high = "#FDBA21", name = "xG") +
      scale_shape_manual(values = c("Mål" = 21, "Ikke mål" = 4), name = "Udfald") +
      scale_size_continuous(range = c(2, 8), name = "xG") +
      scale_color_manual(values = c("Valgt Hold" = "blue", "Modstander" = "red"), name = "Hold") +
      coord_fixed(xlim = c(0, 100), ylim = c(0, 100)) +
      theme_pitch() +
      labs(
        title = sprintf("xG for %s", input$selected_team),
        subtitle = ifelse(selected_match_label == "Hele sæsonen", 
                          "Hele sæsonen", 
                          paste("Kamp:", selected_match_label))
      ) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(face = "italic", hjust = 0.5)
      )
    
    # Konverter til interaktivt plotly-plot
    ggplotly(p, tooltip = "text") %>%
      layout(
        hovermode = "closest",
        showlegend = TRUE
      )
  })
  
  # Statistisk Oversigt for Hold Analyse
  output$xg_summary_table_team <- renderTable({
    df <- get_filtered_team_data()
    
    # Log antallet af rækker
    message("Antal rækker i xg_summary_table_team: ", nrow(df))
    
    # Hvis der ikke er nogen rækker, returner en tom tabel med en besked
    if (nrow(df) == 0) {
      return(data.frame(Gruppe = "Ingen data tilgængelige"))
    }
    
    df %>%
      mutate(SHOTISGOAL = factor(SHOTISGOAL, levels = c(0, 1), labels = c("Ikke mål", "Mål"))) %>%
      group_by(Gruppe = SHOTISGOAL) %>%
      summarise(
        `WyScout – Gennemsnit` = mean(SHOTXG, na.rm = TRUE),
        `WyScout – SD` = sd(SHOTXG, na.rm = TRUE),
        `XGBoost – Gennemsnit` = mean(xG_XGB, na.rm = TRUE),
        `XGBoost – SD` = sd(xG_XGB, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      bind_rows(
        df %>%
          summarise(
            Gruppe = "Alle skud",
            `WyScout – Gennemsnit` = mean(SHOTXG, na.rm = TRUE),
            `WyScout – SD` = sd(SHOTXG, na.rm = TRUE),
            `XGBoost – Gennemsnit` = mean(xG_XGB, na.rm = TRUE),
            `XGBoost – SD` = sd(xG_XGB, na.rm = TRUE)
          )
      )
  }, digits = 3)
  
  # Gennemsnitlig xG pr. Zone for Hold Analyse
  output$xg_zone_table_team <- renderTable({
    df <- get_filtered_team_data()
    
    # Log antallet af rækker
    message("Antal rækker i xg_zone_table_team: ", nrow(df))
    
    # Hvis der ikke er nogen rækker, returner en tom tabel med en besked
    if (nrow(df) == 0) {
      return(data.frame(Zone = "Ingen data tilgængelige"))
    }
    
    df_zones <- df %>%
      mutate(
        Zone = case_when(
          LOCATIONX >= 94 & LOCATIONX <= 100 & LOCATIONY >= 37 & LOCATIONY <= 63 ~ "Målområdet",
          LOCATIONX >= 84 & LOCATIONX < 94 & LOCATIONY >= 19 & LOCATIONY <= 81 ~ "Det store felt",
          TRUE ~ "Uden for feltet"
        )
      ) %>%
      group_by(Zone) %>%
      summarise(
        `XGBoost – Gennemsnit` = mean(xG_XGB, na.rm = TRUE),
        `XGBoost – SD` = sd(xG_XGB, na.rm = TRUE),
        `WyScout – Gennemsnit` = mean(SHOTXG, na.rm = TRUE),
        `WyScout – SD` = sd(SHOTXG, na.rm = TRUE),
        `Antal skud` = n(),
        .groups = "drop"
      ) %>%
      mutate(Zone = factor(Zone, levels = c("Målområdet", "Det store felt", "Uden for feltet"))) %>%
      arrange(Zone)
    
    df_zones
  }, digits = 3)
  
  output$team_summary_table <- renderDT({
    df <- get_filtered_team_data()
    
    # Log antallet af rækker
    message("Antal rækker i team_summary_table: ", nrow(df))
    
    # Hvis der ikke er nogen rækker, returner en tom tabel med en besked
    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "Ingen data tilgængelige")))
    }
    
    datatable(
      df %>%
        summarise(
          `Antal skud` = n(),
          `Mål` = sum(SHOTISGOAL == 1, na.rm = TRUE),
          `Mål (%)` = round(mean(SHOTISGOAL == 1, na.rm = TRUE) * 100, 1),
          `XGBoost xG (gns)` = mean(xG_XGB, na.rm = TRUE),
          `XGBoost xG (sum)` = sum(xG_XGB, na.rm = TRUE),
          `WyScout xG (gns)` = mean(SHOTXG, na.rm = TRUE),
          `WyScout xG (sum)` = sum(SHOTXG, na.rm = TRUE),
          `Over-/underpræstation (xGBoost)` = sum(SHOTISGOAL == 1, na.rm = TRUE) - sum(xG_XGB, na.rm = TRUE),
          `Over-/underpræstation (WyScout)` = sum(SHOTISGOAL == 1, na.rm = TRUE) - sum(SHOTXG, na.rm = TRUE)
        ) %>%
        mutate(across(where(is.numeric), ~round(.x, 2))),
      options = list(pageLength = 5, searching = FALSE, lengthChange = FALSE),
      rownames = FALSE
    ) %>%
      formatStyle("Over-/underpræstation (xGBoost)", 
                  backgroundColor = styleInterval(c(-0.5, 0.5), c("#FFE6E6", "#E6FFE6", "#E6FFE6")))
  })
}

shinyApp(ui, server)