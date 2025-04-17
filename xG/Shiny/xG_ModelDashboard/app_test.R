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
library(caret)

# -- Indlæs data og modeller --
allshotevents <- readRDS("allshotevents.rds")
test_data <- readRDS("test_data.rds")
test_data_yn <- readRDS("test_data_yn.rds")

xgb_model <- readRDS("xgb_model_backup1.rds")
rf_model  <- readRDS("rf_model_final.rds")
tree_model <- readRDS("simple_tree.rds")
glm_model <- readRDS("glm_train.rds")

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
      menuItem("Kamp Analyse", tabName = "match_analysis", icon = icon("futbol")),
      menuItem("Hold Analyse", tabName = "team_analysis", icon = icon("shield-alt"))
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
        .logo-img { width: 20px; height: 20px; vertical-align: middle; margin-right: 5px; }
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
                tags$li(
                  tags$b("📘 xG Fordeling"), ": Sammenlign skudpositioner og xG-værdier på banen for ", 
                  tags$span(style = "color: #D95F02;", "XGBoost"), " og ", 
                  tags$span(style = "color: #66A61E;", "WyScout"), 
                  ". Giver overblik over modellets vurdering af farlige positioner."
                ),
                tags$li(
                  tags$b("📊 Model Performance"), ": Evaluer og sammenlign ", 
                  tags$span(style = "color: #D95F02;", "XGBoost"), ", ",
                  tags$span(style = "color: #7570B3;", "Random Forest"), ", ",
                  tags$span(style = "color: #E7298A;", "Decision Tree"), ", ",
                  tags$span(style = "color: #1B9E77;", "GLM"), " og ", 
                  tags$span(style = "color: #66A61E;", "WyScout"), 
                  " med AUC, Gini, confusion matrix og feature importance."
                ),
                tags$li(
                  tags$b("🎯 Kamp Analyse"), ": Dyk ned i enkelte kampe for et hold og se alle skud samt model-vurderet xG for både ",
                  tags$span(style = "color: blue;", "valgt hold"), " og ",
                  tags$span(style = "color: red;", "modstanderen"),
                  "."
                ),
                tags$li(
                  tags$b("🛡️ Hold Analyse"), ": Se samlet xG, mål og over-/underpræstation pr. hold for hele sæsonen ",
                  "baseret på ", tags$span(style = "color: #D95F02;", "XGBoost-modellen"), "."
                )
              ),
              box(
                title = "📋 Resumé af vores resultater",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                HTML("
  <p>I vores arbejde med Expected Goals har vi udviklet en klassifikationsmodel, der estimerer sandsynligheden for, at et skud bliver til mål – den såkaldte <i>xG-værdi</i>.</p>

  <p>Vi tog udgangspunkt i 4181 afslutninger fra Superligaen 2023/2024 og lavede et stratificeret split i trænings- og testdata for at sikre en ens fordeling af mål og ikke-mål, da kun 11,6 % af skuddene resulterede i mål.</p>

  <p>Som forklarende variable endte vi med kun at anvende tre: <b>afstand</b>, <b>vinkel</b> og <b>kropsdel</b>. Afstand og vinkel er beregnet ud fra afslutningskoordinaterne. En analyse af dataene viste tydelige mønstre: mål bliver i gennemsnit scoret tættere på og fra mere åbne vinkler end ikke-mål.</p>

  <p>Vi afprøvede fire modeller: <b>GLM, Decision Tree, Random Forest og XGBoost</b>. Modellerne blev evalueret på baggrund af AUC. Vores XGBoost-model opnåede den bedste performance med en AUC på <b>0.7774</b>, efterfulgt af WyScout med <b>0.7769</b>.</p>

  <p>Det antyder, at en relativt simpel model med få, men relevante variable og grundig databehandling <b>kan nærme sig</b> niveauet for en professionel benchmarkmodel – <i>i hvert fald når både trænings- og testdata stammer fra samme sæson og liga</i>.</p>
")
                
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
                box(title = "Sammenligning af modellerne", width = 12, status = "info", solidHeader = TRUE,
                    DTOutput("auc_table"))
              ),
              fluidRow(
                box(title = "Confusion Matrix (Threshold = 0.35)", width = 12, status = "info", solidHeader = TRUE,
                    verbatimTextOutput("confusion_matrix_table"))
              ),
              fluidRow(
                box(title = "Feature Importance", width = 12, status = "primary", solidHeader = TRUE,
                    plotOutput("var_imp_plot", height = "600px"))
              )
              
      ),
      # Kamp Analyse
      tabItem(tabName = "match_analysis",
              fluidRow(
                box(title = "Valg af hold og kamp", width = 3, status = "primary", solidHeader = TRUE,
                    selectInput("match_selected_team", "Vælg hold:", choices = sort(unique(allshotevents$TEAMNAME.x))),
                    uiOutput("match_selector"),
                    checkboxGroupInput("match_model_choice", "Vælg model:", choices = c("XGBoost"), selected = "XGBoost")
                ),
                box(title = "Skudpositioner", width = 9, status = "info", solidHeader = TRUE,
                    plotlyOutput("match_xg_plot", height = "600px"))
              ),
              fluidRow(
                box(title = "Kampoversigt", width = 12, status = "warning", solidHeader = TRUE,
                    DTOutput("match_summary_table"))
              )
      ),
      # Hold Analyse
      tabItem(tabName = "team_analysis",
              fluidRow(
                box(title = "Valg af hold", width = 3, status = "primary", solidHeader = TRUE,
                    checkboxGroupInput("team_selected_teams", "Vælg hold:",
                                       choices = sort(unique(allshotevents$TEAMNAME.x)),
                                       selected = sort(unique(allshotevents$TEAMNAME.x)))
                ),
                box(title = "Holdpræstation over Sæsonen", width = 9, status = "warning", solidHeader = TRUE,
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
  
  output$xg_zone_table <- renderTable({
    df_zones <- allshotevents %>%
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
        summarise(
          AUC = round(unique(AUC), 4),
          Gini = round((2 * unique(AUC)) - 1, 4),
          .groups = "drop"
        ) %>%
        arrange(desc(AUC)),
      options = list(pageLength = 5, searching = FALSE, lengthChange = FALSE),
      rownames = FALSE
    ) %>%
      formatStyle("AUC", backgroundColor = styleInterval(c(0.7, 0.77), c("#FFE6E6", "#FFF0E6", "#E6FFE6"))) %>%
      formatStyle("Gini", backgroundColor = styleInterval(c(0.5, 0.554), c("#FFE6E6", "#FFF0E6", "#E6FFE6")))
  })
  
  
  output$confusion_matrix_table <- renderPrint({
    # Tjek inputdata før beregning
    cat("=== Debugging Input Data ===\n")
    cat("Length of test_data_yn$SHOTISGOAL: ", length(test_data_yn$SHOTISGOAL), "\n")
    cat("Number of NAs in test_data_yn$SHOTISGOAL: ", sum(is.na(test_data_yn$SHOTISGOAL)), "\n")
    cat("First few test_data_yn$SHOTISGOAL: ", paste(head(test_data_yn$SHOTISGOAL, 5), collapse = ", "), "\n")
    cat("Length of test_data$SHOTISGOAL: ", length(test_data$SHOTISGOAL), "\n")
    cat("Number of NAs in test_data$SHOTISGOAL: ", sum(is.na(test_data$SHOTISGOAL)), "\n")
    cat("First few test_data$SHOTISGOAL: ", paste(head(test_data$SHOTISGOAL, 5), collapse = ", "), "\n")
    
    cat("Length of xgb_pred: ", length(xgb_pred), "\n")
    cat("Number of NAs in xgb_pred: ", sum(is.na(xgb_pred)), "\n")
    cat("First few xgb_pred: ", paste(head(xgb_pred, 5), collapse = ", "), "\n")
    
    cat("Length of rf_test: ", length(rf_test), "\n")
    cat("Number of NAs in rf_test: ", sum(is.na(rf_test)), "\n")
    cat("First few rf_test: ", paste(head(rf_test, 5), collapse = ", "), "\n")
    
    cat("Length of tree_probs: ", length(tree_probs), "\n")
    cat("Number of NAs in tree_probs: ", sum(is.na(tree_probs)), "\n")
    cat("First few tree_probs: ", paste(head(tree_probs, 5), collapse = ", "), "\n")
    
    cat("Length of glm_probs: ", length(glm_probs), "\n")
    cat("Number of NAs in glm_probs: ", sum(is.na(glm_probs)), "\n")
    cat("First few glm_probs: ", paste(head(glm_probs, 5), collapse = ", "), "\n")
    
    cat("Length of test_data$SHOTXG: ", length(test_data$SHOTXG), "\n")
    cat("Number of NAs in test_data$SHOTXG: ", sum(is.na(test_data$SHOTXG)), "\n")
    cat("First few test_data$SHOTXG: ", paste(head(test_data$SHOTXG, 5), collapse = ", "), "\n")
    cat("============================\n\n")
    
    threshold <- 0.35
    
    compute_confusion_matrix <- function(predictions, actual, model_name) {
      # Log forudsigelsernes detaljer for fejlfinding
      message("Model: ", model_name)
      message("Length of predictions: ", length(predictions))
      message("Length of actual: ", length(actual))
      message("Predictions range: ", paste(range(predictions, na.rm = TRUE), collapse = " to "))
      message("Number of NAs in predictions: ", sum(is.na(predictions)))
      message("First few predictions: ", paste(head(predictions, 5), collapse = ", "))
      message("Class of actual: ", class(actual))
      message("First few actual values: ", paste(head(actual, 5), collapse = ", "))
      
      # Konverter actual til numerisk
      if (is.factor(actual)) {
        # Tjek levels og map dem korrekt
        if (all(levels(actual) %in% c("no", "yes"))) {
          actual <- as.numeric(actual == "yes")  # "no" -> 0, "yes" -> 1
        } else if (all(levels(actual) %in% c("0", "1"))) {
          actual <- as.numeric(as.character(actual))  # "0" -> 0, "1" -> 1
        } else {
          return(list(error = paste("Unexpected factor levels in actual:", paste(levels(actual), collapse = ", "))))
        }
        message("Actual converted to numeric. First few values: ", paste(head(actual, 5), collapse = ", "))
      }
      
      # Tjek for NA-værdier og længde
      if (length(predictions) != length(actual)) {
        return(list(error = paste("Length mismatch: predictions (", length(predictions), ") does not match actual (", length(actual), ")")))
      }
      
      # Fjern NA-værdier fra både predictions og actual
      valid_idx <- !is.na(predictions) & !is.na(actual)
      if (sum(valid_idx) == 0) {
        return(list(error = "No valid data after removing NAs."))
      }
      predictions <- predictions[valid_idx]
      actual <- actual[valid_idx]
      message("Length after removing NAs - predictions: ", length(predictions), ", actual: ", length(actual))
      
      # Normaliser forudsigelser til intervallet [0, 1], hvis nødvendigt
      pred_range <- range(predictions, na.rm = TRUE)
      if (pred_range[2] > 1 || pred_range[1] < 0) {
        predictions <- (predictions - pred_range[1]) / (pred_range[2] - pred_range[1])
        message("Normalized predictions range: ", paste(range(predictions, na.rm = TRUE), collapse = " to "))
      }
      
      # Konverter til faktorer med faste niveauer
      predicted <- as.numeric(predictions >= threshold)
      predicted <- factor(predicted, levels = c(0, 1), labels = c("Not Goal", "Goal"))
      actual <- factor(actual, levels = c(0, 1), labels = c("Not Goal", "Goal"))
      
      # Beregn konfusionsmatrix med caret
      cm <- caret::confusionMatrix(predicted, actual, positive = "Goal")
      
      return(cm)
    }
    
    # Liste til at gemme konfusionsmatricer
    confusion_matrices <- list()
    
    if ("Random Forest" %in% input$selected_models) {
      confusion_matrices[["Random Forest"]] <- compute_confusion_matrix(rf_test, test_data_yn$SHOTISGOAL, "Random Forest")
    }
    if ("Decision Tree" %in% input$selected_models) {
      confusion_matrices[["Decision Tree"]] <- compute_confusion_matrix(tree_probs, test_data_yn$SHOTISGOAL, "Decision Tree")
    }
    if ("XGBoost" %in% input$selected_models) {
      confusion_matrices[["XGBoost"]] <- compute_confusion_matrix(xgb_pred, test_data_yn$SHOTISGOAL, "XGBoost")
    }
    if ("GLM" %in% input$selected_models) {
      confusion_matrices[["GLM"]] <- compute_confusion_matrix(glm_probs, test_data_yn$SHOTISGOAL, "GLM")
    }
    if ("WyScout" %in% input$selected_models) {
      confusion_matrices[["WyScout"]] <- compute_confusion_matrix(test_data$SHOTXG, test_data$SHOTISGOAL, "WyScout")
    }
    
    # Hvis ingen modeller er valgt, vis en besked
    if (length(confusion_matrices) == 0) {
      cat("Vælg mindst én model for at se konfusionsmatrixen.\n")
      return()
    }
    
    # Udfør print for hver model
    for (model_name in names(confusion_matrices)) {
      result <- confusion_matrices[[model_name]]
      cat("=== Confusion Matrix for", model_name, "===\n")
      if (is.list(result) && !is.null(result$error)) {
        cat("Error:", result$error, "\n\n")
      } else {
        print(result)
        cat("\n")
      }
    }
  })
  
  output$var_imp_plot <- renderPlot({
    selected <- input$selected_models
    models <- list(
      "XGBoost" = xgb_model,
      "Random Forest" = rf_model,
      "Decision Tree" = tree_model,
      "GLM" = glm_model
    )
    
    var_imp_data <- lapply(selected, function(model_name) {
      if (model_name %in% names(models)) {
        imp <- caret::varImp(models[[model_name]])$importance
        imp$Feature <- rownames(imp)
        imp$Model <- model_name
        imp
      }
    })
    
    var_imp_df <- bind_rows(var_imp_data)
    
    # Gør det til faktor og bevar rækkefølge
    var_imp_df$Feature <- factor(var_imp_df$Feature, levels = rev(unique(var_imp_df$Feature)))
    
    ggplot(var_imp_df, aes(x = Feature, y = Overall, fill = Model)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      coord_flip() +
      labs(
        title = "Feature Importance (sammenlignet på tværs af modeller)",
        x = "Feature",
        y = "Vigtighed"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.y = element_text(size = 12),
        legend.position = "bottom"
      )
  })
  
  
  get_filtered_match_data <- reactive({
    message("Kører get_filtered_match_data...")
    message("Valgt hold: ", input$match_selected_team)
    message("Valgt kamp: ", input$match_selected_match)
    
    match_ids <- allshotevents %>%
      filter(TEAMNAME.x == input$match_selected_team) %>%
      distinct(MATCH_WYID.x) %>%
      pull(MATCH_WYID.x)
    
    message("Antal kampe fundet for ", input$match_selected_team, ": ", length(match_ids))
    message("match_ids: ", paste(match_ids, collapse = ", "))
    
    df <- allshotevents %>%
      filter(MATCH_WYID.x %in% match_ids)
    
    message("Antal rækker efter match-filtrering: ", nrow(df))
    
    if (!is.null(input$match_selected_match) && input$match_selected_match != "Alle kampe" && input$match_selected_match != "") {
      selected_match_id <- as.numeric(input$match_selected_match)
      message("Type af MATCH_WYID.x: ", class(df$MATCH_WYID.x))
      message("Type af selected_match_id: ", class(selected_match_id))
      message("selected_match_id: ", selected_match_id)
      
      df <- df %>% filter(MATCH_WYID.x == selected_match_id)
      message("Antal rækker efter specifik kampfiltrering: ", nrow(df))
    }
    
    message("Antal rækker i df: ", nrow(df))
    message("Antal NA i IMAGEDATAURL.y: ", sum(is.na(df$IMAGEDATAURL.y)))
    message("Unikke værdier i IMAGEDATAURL.y: ", paste(unique(df$IMAGEDATAURL.y), collapse = ", "))
    
    df
  })
  
  output$match_xg_plot <- renderPlotly({
    req(input$match_model_choice)
    df <- get_filtered_match_data()
    
    message("Antal rækker i df (match_xg_plot): ", nrow(df))
    
    if (nrow(df) == 0) {
      p <- ggplot() +
        annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "#f5f5f5") +
        coord_fixed(xlim = c(0, 100), ylim = c(0, 100)) +
        theme_pitch() +
        labs(
          title = sprintf("xG for %s", input$match_selected_team),
          subtitle = "Ingen skud tilgængelige for dette hold/kamp."
        ) +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5),
          plot.subtitle = element_text(face = "italic", hjust = 0.5)
        )
      return(ggplotly(p))
    }
    
    selected_match_label <- if (!is.null(input$match_selected_match) && input$match_selected_match != "Alle kampe" && input$match_selected_match != "") {
      df %>%
        filter(MATCH_WYID.x == as.numeric(input$match_selected_match)) %>%
        pull(MATCH_LABEL) %>%
        unique() %>%
        first()
    } else {
      "Hele sæsonen"
    }
    
    df <- df %>%
      mutate(
        Goal_Label = ifelse(SHOTISGOAL == 1, "Mål", "Ikke mål"),
        Model = "XGBoost",
        xG = xG_XGB,
        Team_Type = ifelse(TEAMNAME.y == input$match_selected_team, "Valgt Hold", "Modstander"),
        Adjusted_X = ifelse(Team_Type == "Modstander", 100 - LOCATIONX, LOCATIONX),
        Adjusted_Y = ifelse(Team_Type == "Modstander", 100 - LOCATIONY, LOCATIONY),
        Hover_Text = paste(
          "xG: ", round(xG_XGB, 3), "<br>",
          "Spiller: ", SHORTNAME.y, "<br>",
          "Hold: ", TEAMNAME.y, "<br>",
          "Type: ", Team_Type
        )
      )
    
    message("Antal skud fra Valgt Hold: ", nrow(df %>% filter(Team_Type == "Valgt Hold")))
    message("Antal skud fra Modstander: ", nrow(df %>% filter(Team_Type == "Modstander")))
    
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
        title = sprintf("xG for %s", input$match_selected_team),
        subtitle = ifelse(selected_match_label == "Hele sæsonen", 
                          "Hele sæsonen", 
                          paste("Kamp:", selected_match_label))
      ) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(face = "italic", hjust = 0.5)
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hovermode = "closest",
        showlegend = TRUE
      )
  })
  
  output$match_summary_table <- renderDT({
    df <- get_filtered_match_data()
    
    message("Antal rækker i match_summary_table: ", nrow(df))
    
    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "Ingen data tilgængelige")))
    }
    
    df <- df %>%
      mutate(
        Team_Type = ifelse(TEAMNAME.y == input$match_selected_team, "Valgt Hold", "Modstander")
      )
    
    summary_df <- df %>%
      group_by(Team_Type, TEAMNAME.y) %>%
      summarise(
        `Antal skud` = n(),
        `Mål` = sum(SHOTISGOAL == 1, na.rm = TRUE),
        `Mål (%)` = round(mean(SHOTISGOAL == 1, na.rm = TRUE) * 100, 1),
        `xG (sum)` = sum(xG_XGB, na.rm = TRUE),
        `Over-/underpræstation` = sum(SHOTISGOAL == 1, na.rm = TRUE) - sum(xG_XGB, na.rm = TRUE),
        `Logo` = first(IMAGEDATAURL.y, default = "https://via.placeholder.com/20"),
        .groups = "drop"
      ) %>%
      mutate(
        `Hold` = TEAMNAME.y,
        across(where(is.numeric), ~round(.x, 2))
      )
    
    summary_df <- summary_df %>%
      mutate(
        Hold = paste0('<img src="', Logo, '" class="logo-img"></img> ', Hold)
      ) %>%
      select(
        `Hold`,
        `Antal skud`,
        `Mål`,
        `Mål (%)`,
        `xG (sum)`,
        `Over-/underpræstation`
      )
    
    datatable(
      summary_df,
      escape = FALSE,
      options = list(pageLength = 5, searching = FALSE, lengthChange = FALSE),
      rownames = FALSE
    ) %>%
      formatStyle("Over-/underpræstation", 
                  backgroundColor = styleInterval(c(-0.5, 0.5), c("#FFE6E6", "#E6FFE6", "#E6FFE6")))
  })
  
  get_filtered_team_data <- reactive({
    message("Kører get_filtered_team_data...")
    message("Valgte hold (Hold Analyse): ", paste(input$team_selected_teams, collapse = ", "))
    
    df <- allshotevents
    
    if (!is.null(input$team_selected_teams) && length(input$team_selected_teams) > 0) {
      df <- df %>%
        filter(TEAMNAME.x %in% input$team_selected_teams)
    }
    
    message("Antal rækker efter filtrering: ", nrow(df))
    message("Antal NA i IMAGEDATAURL.y: ", sum(is.na(df$IMAGEDATAURL.y)))
    message("Unikke værdier i IMAGEDATAURL.y: ", paste(unique(df$IMAGEDATAURL.y), collapse = ", "))
    
    df
  })
  
  output$team_summary_table <- renderDT({
    df <- get_filtered_team_data()
    
    message("Antal rækker i team_summary_table: ", nrow(df))
    
    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "Ingen data tilgængelige")))
    }
    
    summary_df <- df %>%
      group_by(TEAMNAME.x) %>%
      summarise(
        `Hold` = first(TEAMNAME.x),
        `Antal skud` = n(),
        `Mål` = sum(SHOTISGOAL == 1, na.rm = TRUE),
        `Mål (%)` = round(mean(SHOTISGOAL == 1, na.rm = TRUE) * 100, 1),
        `xG (sum)` = sum(xG_XGB, na.rm = TRUE),
        `Over-/underpræstation` = sum(SHOTISGOAL == 1, na.rm = TRUE) - sum(xG_XGB, na.rm = TRUE),
        `Logo` = first(IMAGEDATAURL.y, default = "https://via.placeholder.com/20"),
        .groups = "drop"
      ) %>%
      mutate(
        across(where(is.numeric), ~round(.x, 2))
      )
    
    summary_df <- summary_df %>%
      mutate(
        Hold = paste0('<img src="', Logo, '" class="logo-img"></img> ', Hold)
      ) %>%
      select(
        `Hold`,
        `Antal skud`,
        `Mål`,
        `Mål (%)`,
        `xG (sum)`,
        `Over-/underpræstation`
      ) %>%
      arrange(desc(`xG (sum)`))
    
    datatable(
      summary_df,
      escape = FALSE,
      options = list(paging = FALSE, searching = FALSE, lengthChange = FALSE),
      rownames = FALSE
    ) %>%
      formatStyle("Over-/underpræstation", 
                  backgroundColor = styleInterval(c(-0.5, 0.5), c("#FFE6E6", "#E6FFE6", "#E6FFE6")))
  })
}

# Start Shiny appen
shinyApp(ui, server)