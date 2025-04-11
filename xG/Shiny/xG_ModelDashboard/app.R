library(shiny)
library(shinydashboard)
library(ggplot2)
library(pROC)
library(dplyr)
library(ggsoccer)

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
rf_df <- data.frame(
  FPR = 1 - rf_roc$specificities,
  TPR = rf_roc$sensitivities,
  Model = "Random Forest"
) %>%
  mutate(AUC = as.numeric(auc(rf_roc)))

tree_df <- data.frame(
  FPR = 1 - tree_roc$specificities,
  TPR = tree_roc$sensitivities,
  Model = "Decision Tree"
) %>%
  mutate(AUC = as.numeric(auc(tree_roc)))

xgb_df <- data.frame(
  FPR = 1 - xgb_roc$specificities,
  TPR = xgb_roc$sensitivities,
  Model = "XGBoost"
) %>%
  mutate(AUC = as.numeric(auc(xgb_roc)))

glm_df <- data.frame(
  FPR = 1 - glm_roc$specificities,
  TPR = glm_roc$sensitivities,
  Model = "GLM"
) %>%
  mutate(AUC = as.numeric(auc(glm_roc)))

wyscout_df <- data.frame(
  FPR = 1 - wyscout_roc$specificities,
  TPR = wyscout_roc$sensitivities,
  Model = "WyScout"
) %>%
  mutate(AUC = as.numeric(auc(wyscout_roc)))

roc_data_all <- bind_rows(rf_df, tree_df, xgb_df, glm_df, wyscout_df)

# UI ------------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "xG Performance App"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("📘 Introduktion", tabName = "intro", icon = icon("info-circle")),
      menuItem("xG Fordeling", tabName = "xg_overview", icon = icon("bullseye")),
      menuItem("Model Performance", tabName = "performance", icon = icon("chart-line")),
      menuItem("xG pr. Hold", tabName = "xg_team_view", icon = icon("shield-alt")),
      menuItem("Under contruction", tabName = "", icon = icon("shield-alt"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro",
              h2("Velkommen til xG-dashboardet for Superligaen 2023/2024"),
              p("Denne app giver dig mulighed for at udforske expected goals (xG) fra Superligaen 2023/24 – både vores egen model og WyScout’s."),
              br(),
              p("Brug menuen i venstre side for at vælge en side. Under 'Model Performance' kan du sammenligne forskellige modeller og vurdere, hvor godt vores XGBoost-model præsterer sammenlignet med fx WyScout.")
      ),
      tabItem(tabName = "xg_overview",
              fluidRow(
                box(title = "Vælg model", width = 3, status = "primary", solidHeader = TRUE,
                    checkboxGroupInput("xg_models_selected", "Vælg modeller til visning:",
                                       choices = c("XGBoost", "WyScout"),
                                       selected = c("XGBoost", "WyScout"))
                ),
                box(title = "xG-visualisering", width = 9, solidHeader = TRUE, status = "info",
                    uiOutput("xg_plot_ui"))
              ),
              fluidRow(
                box(title = "Statistisk oversigt", width = 12, solidHeader = TRUE, status = "warning",
                    tableOutput("xg_summary_table"))
              ),
              fluidRow(
                box(title = "Konklusion", width = 12, solidHeader = TRUE, status = "success",
                    p("xGBoost-modellen præsterer bedre end WyScout ift. både AUC og måden hvorpå sandsynligheder fordeler sig på banen. 
                Den er særligt bedre til at skelne mellem gode og dårlige skudpositioner, men WyScout er stadig tæt på. 
                Variationen (SD) er lavere for mål end ikke-mål, hvilket viser at modellerne er mere sikre ved de skud, der rent faktisk bliver til mål."))
              )
      ),
      tabItem(tabName = "performance",
              fluidRow(
                box(title = "Vælg modeller til sammenligning:", status = "primary", solidHeader = TRUE, width = 3,
                    checkboxGroupInput("selected_models", label = NULL,
                                       choices = unique(roc_data_all$Model),
                                       selected = unique(roc_data_all$Model))
                ),
                box(title = "ROC-kurve for udvalgte modeller", status = "warning", solidHeader = TRUE, width = 9,
                    plotOutput("roc_plot"))
              )
      ),
      tabItem(tabName = "xg_team_view",
              fluidRow(
                box(title = "Valg af hold og kamp", width = 3, status = "primary", solidHeader = TRUE,
                    selectInput("selected_team", "Vælg hold:", choices = sort(unique(allshotevents$TEAMNAME.x))),
                    uiOutput("match_selector"),
                    checkboxGroupInput("model_choice", "Vælg model:", choices = c("XGBoost", "WyScout"), selected = c("XGBoost", "WyScout"))
                ),
                box(title = "Skudpositioner", width = 9, status = "info", solidHeader = TRUE,
                    uiOutput("team_xg_plot_ui"))
              ),
              fluidRow(
                box(title = "Skudplot", width = 6, solidHeader = TRUE, status = "info",
                    uiOutput("team_xg_plot_ui")
                ),
                box(title = "Tabeloversigt", width = 6, solidHeader = TRUE, status = "warning",
                    tableOutput("team_summary_table"))
              )
              
      )
    )
  )
)

# Server ------------------------------------------------------------------
# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  # === ROC-plot ===
  output$roc_plot <- renderPlot({
    df <- roc_data_all %>% filter(Model %in% input$selected_models)
    
    model_colors <- c(
      "GLM" = "#4682B4",
      "XGBoost" = "#FDBA21",
      "Random Forest" = "#00509d",
      "Decision Tree" = "#999999",
      "WyScout" = "#2C3E50"
    )
    
    auc_labels <- list(
      "GLM" = list(auc = auc(glm_roc), y = 0.45),
      "XGBoost" = list(auc = auc(xgb_roc), y = 0.38),
      "Random Forest" = list(auc = auc(rf_roc), y = 0.31),
      "Decision Tree" = list(auc = auc(tree_roc), y = 0.24),
      "WyScout" = list(auc = auc(wyscout_roc), y = 0.17)
    )
    
    p <- ggplot(df, aes(x = FPR, y = TPR, color = Model)) +
      geom_line(size = 1.4) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray60") +
      scale_color_manual(values = model_colors) +
      labs(
        title = "ROC-kurve for modeller",
        subtitle = "Evalueret på testdatasæt",
        x = "False Positive Rate",
        y = "True Positive Rate",
        color = "Model"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold")
      )
    
    for (model in input$selected_models) {
      if (model %in% names(auc_labels)) {
        p <- p + annotate("text", x = 0.65, y = auc_labels[[model]]$y,
                          label = paste0(model, " AUC: ", round(auc_labels[[model]]$auc, 4)),
                          color = model_colors[[model]],
                          fontface = "bold", size = 5)
      }
    }
    p
  })
  
  # === xG-overview (alle skud) ===
  output$xg_plot_ui <- renderUI({
    models <- input$xg_models_selected
    if (length(models) == 0) return(h4("Vælg mindst én model"))
    
    plots <- list()
    if ("XGBoost" %in% models) plots[[length(plots) + 1]] <- plotOutput("xg_plot_xgb")
    if ("WyScout" %in% models) plots[[length(plots) + 1]] <- plotOutput("xg_plot_wyscout")
    do.call(tagList, plots)
  })
  
  output$xg_plot_xgb <- renderPlot({
    ggplot(allshotevents, aes(x = LOCATIONX, y = LOCATIONY)) +
      annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "white") +
      geom_point(aes(size = xG_XGB, color = xG_XGB), alpha = 0.6) +
      scale_color_gradient(low = "#0D1C8A", high = "#FDBA21") +
      scale_size(range = c(1.5, 6)) +
      coord_fixed(xlim = c(0, 100), ylim = c(0, 100)) +
      theme_pitch() +
      labs(title = "xGBoost – xG-fordeling på banen", color = "xG") +
      guides(size = "none") +
      theme(plot.title = element_text(face = "bold", hjust = 0.5))
  })
  
  output$xg_plot_wyscout <- renderPlot({
    ggplot(allshotevents, aes(x = LOCATIONX, y = LOCATIONY)) +
      annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "white") +
      geom_point(aes(size = SHOTXG, color = SHOTXG), alpha = 0.6) +
      scale_color_gradient(low = "#2C3E50", high = "#FDBA21") +
      scale_size(range = c(1.5, 6)) +
      coord_fixed(xlim = c(0, 100), ylim = c(0, 100)) +
      theme_pitch() +
      labs(title = "WyScout – xG-fordeling på banen", color = "xG") +
      guides(size = "none") +
      theme(plot.title = element_text(face = "bold", hjust = 0.5))
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
  
  # === Team-kamp side ===
  output$match_selector <- renderUI({
    req(input$selected_team)
    kampe <- allshotevents %>%
      filter(TEAMNAME.x == input$selected_team) %>%
      arrange(MATCH_WYID.x) %>%
      pull(MATCH_WYID.x) %>%
      unique()
    selectInput("selected_match", "Vælg kamp (valgfrit):", choices = c("Alle kampe", kampe))
  })
  
  output$team_xg_plot_ui <- renderUI({
    req(input$selected_team, input$model_choice)
    plots <- list()
    if ("XGBoost" %in% input$model_choice) plots[[length(plots) + 1]] <- plotOutput("xg_plot_team_xgb")
    if ("WyScout" %in% input$model_choice) plots[[length(plots) + 1]] <- plotOutput("xg_plot_team_wyscout")
    do.call(tagList, plots)
  })
  
  get_filtered_team_data <- reactive({
    df <- allshotevents %>% filter(TEAMNAME.x == input$selected_team)
    if (!is.null(input$selected_match) && input$selected_match != "Alle kampe") {
      df <- df %>% filter(MATCH_WYID.x == input$selected_match)
    }
    df
  })
  
  output$xg_plot_team_xgb <- renderPlot({
    data <- get_filtered_team_data()
    ggplot(data, aes(x = LOCATIONX, y = LOCATIONY)) +
      annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "white") +
      geom_point(aes(size = xG_XGB, color = xG_XGB), alpha = 0.7) +
      scale_color_gradient(low = "#0D1C8A", high = "#FDBA21") +
      scale_size(range = c(1.5, 6)) +
      coord_fixed(xlim = c(0, 100), ylim = c(0, 100)) +
      theme_pitch() +
      labs(
        title = paste("xGBoost –", input$selected_team),
        subtitle = ifelse(input$selected_match == "Alle kampe", "Hele sæsonen", paste("Kamp:", input$selected_match)),
        color = "xG"
      ) +
      guides(size = "none") +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(face = "italic", hjust = 0.5)
      )
  })
  
  output$xg_plot_team_wyscout <- renderPlot({
    data <- get_filtered_team_data()
    ggplot(data, aes(x = LOCATIONX, y = LOCATIONY)) +
      annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "white") +
      geom_point(aes(size = SHOTXG, color = SHOTXG), alpha = 0.7) +
      scale_color_gradient(low = "#2C3E50", high = "#FDBA21") +
      scale_size(range = c(1.5, 6)) +
      coord_fixed(xlim = c(0, 100), ylim = c(0, 100)) +
      theme_pitch() +
      labs(
        title = paste("WyScout –", input$selected_team),
        subtitle = ifelse(input$selected_match == "Alle kampe", "Hele sæsonen", paste("Kamp:", input$selected_match)),
        color = "xG"
      ) +
      guides(size = "none") +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(face = "italic", hjust = 0.5)
      )
  })
  
  output$team_summary_table <- renderTable({
    df <- get_filtered_team_data()
    req(nrow(df) > 0)
    
    df %>%
      summarise(
        `Antal skud` = n(),
        `Mål` = sum(SHOTISGOAL == 1, na.rm = TRUE),
        `Mål (%)` = round(mean(SHOTISGOAL == 1, na.rm = TRUE) * 100, 1),
        `xGBoost xG (gns)` = mean(xG_XGB, na.rm = TRUE),
        `xGBoost xG (sum)` = sum(xG_XGB, na.rm = TRUE),
        `WyScout xG (gns)` = mean(SHOTXG, na.rm = TRUE),
        `WyScout xG (sum)` = sum(SHOTXG, na.rm = TRUE),
        `Over-/underpræstation (xGBoost)` = sum(SHOTISGOAL == 1, na.rm = TRUE) - sum(xG_XGB, na.rm = TRUE),
        `Over-/underpræstation (WyScout)` = sum(SHOTISGOAL == 1, na.rm = TRUE) - sum(SHOTXG, na.rm = TRUE)
      )
  }, digits = 2)
  
}


shinyApp(ui, server)