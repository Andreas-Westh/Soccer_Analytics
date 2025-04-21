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
library(tibble) 
library(xgboost)
library(randomForest)

# -- Brøndby IF
library(lubridate)
library(ggimage)


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
  "GLM" = "#e9c46a", 
  "XGBoost" = "#264653", 
  "Random Forest" = "#2a9d8f", 
  "Decision Tree" = "#f4a261", 
  "WyScout" = "#e76f51"
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

# -- Brøndby IF --

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
  dashboardHeader(title = "xG Model Evaluering"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("📘 Introduktion", tabName = "intro", icon = icon("info-circle")),
      menuItem("xG Fordeling", tabName = "xg_overview", icon = icon("bullseye")),
      menuItem("Model Performance", tabName = "performance", icon = icon("chart-line")),
      menuItem("Hold Analyse", tabName = "team_analysis", icon = icon("shield-alt")),
      menuItem("Kamp Analyse", tabName = "match_analysis", icon = icon("futbol")),
      menuItem("Brøndby IF (Opgave 1.6)", icon = icon("home"), startExpanded = F,
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
    tags$head(
      tags$style(HTML("
        .box.box-primary { border-top-color: #264653; }
        .box.box-info { border-top-color: #e76f51; }
        .box.box-warning { border-top-color: #2a9d8f; }
        .skin-blue .main-header .logo { background-color: #264653; }
        .skin-blue .main-header .navbar { background-color: #264653; }
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
                  tags$span(style = "color: #264653;", "XGBoost"), " og ", 
                  tags$span(style = "color: #e76f51;", "WyScout"), 
                  ". Giver overblik over modellets vurdering af farlige positioner."
                ),
                tags$li(
                  tags$b("📊 Model Performance"), ": Evaluer og sammenlign ", 
                  tags$span(style = "color: #264653;", "XGBoost"), ", ",
                  tags$span(style = "color: #2a9d8f;", "Random Forest"), ", ",
                  tags$span(style = "color: #f4a261;", "Decision Tree"), ", ",
                  tags$span(style = "color: #e9c46a;", "GLM"), " og ", 
                  tags$span(style = "color: #e76f51;", "WyScout"), 
                  " med AUC, Gini, confusion matrix og feature importance."
                ),
                tags$li(
                  tags$b("🛡️ Hold Analyse"), ": Se samlet xG, mål og over-/underpræstation pr. hold for hele sæsonen ",
                  "baseret på ", tags$span(style = "color: #264653;", "XGBoost-modellen"), "."
                ),
                tags$li(
                  tags$b("🎯 Kamp Analyse"), ": Dyk ned i enkelte kampe for et hold og se alle skud samt model-vurderet xG for både ",
                  tags$span(style = "color: blue;", "valgt hold"), " og ",
                  tags$span(style = "color: red;", "modstanderen"),
                  "."
                ),
                tags$li(
                  tags$b("🏠 Brøndby IF"), ": Denne del er ment til opgave 1.6! Giver et indblik i Brøndby IF's performance i den igangværende sæson ",
                  "baseret på vores egen xG-model. Indeholder analyser på kamp-, hold- og spillerniveau."
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
                box(title = "Confusion Matrix (Threshold = 0.35)", width = 6, status = "info", solidHeader = TRUE,
                    verbatimTextOutput("confusion_matrix_table")),
                box(title = "Feature Importance", width = 6, status = "primary", solidHeader = TRUE,
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
              ),
              fluidRow(
                box(
                  title = "Hvad betyder Over-/underpræstation?",
                  class = "gray-box",  # Brugerdefineret klasse
                  solidHeader = TRUE,
                  width = 9,
                  offset = 3,
                  p("Over-/underpræstation viser forskellen mellem faktiske mål og forventede mål (xG). Positiv værdi = flere mål end forventet. Negativ værdi = færre mål end forventet.")
                )
              )
      ),
      # -- Brøndby IF --
      tabItem(tabName = "intro_b",
              box(
                title = "Vigtig Information",
                status = "warning",
                solidHeader = TRUE,
                width = 12,
                p(strong("OBS OBS"), " Denne Shiny er ment for opgave 1.6 - Hvordan kan Brøndby IF bruge vores model i den igangværende sæson")
              ),
              fluidRow(
                valueBoxOutput("xgb_auc_box_2425", width = 4),
                valueBoxOutput("wyscout_auc_box_2425", width = 4),
                valueBoxOutput("improvement_box_2425", width = 4)
              ),
              h2("Velkommen til Brøndby IF xG Analyse"),
              p("Denne del af dashboardet bruger vores egenudviklede xG-model baseret på XGBoost til at analysere Brøndby IF's præstationer i den igangværende Superliga-sæson 2024/25."),
              p("Ved at beregne forventede mål (xG) for hver afslutning – og sammenligne med de faktiske mål – kan vi vurdere, om holdet præsterer bedre eller dårligere end forventet."),
              p("Modellen er udviklet specifikt på Superliga-data og giver dermed et skræddersyet indblik i:"),
              tags$ul(
                tags$li(icon("chart-line"), " Holdets formkurve: over- og underperformance i forskellige perioder."),
                tags$li(icon("user"), " Spilleranalyse: Hvem skaber og har de bedste chancer, hvem udnytter dem bedst, og hvor på banen er deres skud."),
                tags$li(icon("futbol"), " Kampvurderinger: giver et realistisk billede af præstationen – uafhængigt af resultatet.")
              ),
              p("Ved at bruge denne indsigt kan Brøndby IF evaluere både kollektive og individuelle præstationer løbende – med data i centrum."),
              p("Navigér i menuen til venstre for at dykke ned i detaljer.")
              
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
              ),
              box(
                title = "Vælg spiller og kamp",
                width = 12,
                fluidRow(
                  column(6,
                         selectInput("player_select", "Vælg spiller:",
                                     choices = NULL) # Choices fyldes dynamisk i server-delen
                  ),
                  column(6,
                         uiOutput("player_match_selector")
                  )
                )
              ),
              box(
                title = "Spillerens Skud på Banen",
                width = 12,
                plotlyOutput("player_shot_map", height = "600px")
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
    
    # Tjek om der er data efter filtrering
    if (nrow(df) == 0 || length(input$selected_models) == 0) {
      p <- ggplot() +
        labs(title = "Ingen modeller valgt til ROC-kurve") +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5))
      print(p)
      return()
    }
    
    # Udvælg unikke AUC-værdier for hver model til geom_label
    auc_labels <- df %>%
      distinct(Model, AUC_label)
    
    p <- ggplot(df, aes(x = FPR, y = TPR, color = Model, fill = Model)) +
      geom_ribbon(aes(ymin = 0, ymax = TPR), alpha = 0.2, color = NA) +
      geom_line(size = 1.5) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray60") +
      scale_color_manual(values = model_colors) +
      scale_fill_manual(values = model_colors) +
      geom_label(
        aes(x = 0.7, y = seq(0.4, 0.2, length.out = n_distinct(Model)), 
            label = paste(Model, "AUC:", AUC_label)),
        data = auc_labels,
        show.legend = FALSE,
        size = 5,
        fill = "white",  # Hvid baggrund
        alpha = 0.8,     # Let gennemsigtighed i baggrunden
        label.padding = unit(0.3, "lines"),  # Juster padding omkring teksten
        label.size = 0.5,  # Tynd kant omkring label
        color = "black"    # Tekstfarve
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
    threshold <- 0.35
    
    compute_confusion_matrix <- function(predictions, actual, model_name) {
      # Log forudsigelsernes detaljer for fejlfinding
      #message("Model: ", model_name)
      #message("Length of predictions: ", length(predictions))
      #message("Length of actual: ", length(actual))
      #message("Predictions range: ", paste(range(predictions, na.rm = TRUE), collapse = " to "))
      #message("Number of NAs in predictions: ", sum(is.na(predictions)))
      #message("First few predictions: ", paste(head(predictions, 5), collapse = ", "))
      #message("Class of actual: ", class(actual))
      #message("First few actual values: ", paste(head(actual, 5), collapse = ", "))
      
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
        # Uddrag kun selve matricen
        cm_table <- result$table
        # Opret en formateret streng til visning
        cat("           Reference\n")
        cat("Prediction  Not Goal  Goal\n")
        cat("Not Goal    ", sprintf("%-9d", cm_table[1, 1]), sprintf("%-9d", cm_table[1, 2]), "\n")
        cat("Goal        ", sprintf("%-9d", cm_table[2, 1]), sprintf("%-9d", cm_table[2, 2]), "\n")
        cat("\n")
      }
    }
  })
  
  output$var_imp_plot <- renderPlot({
    # --- Gini Importance fra beslutningstræ (rpart) ---
    importance_tree <- tree_model$variable.importance
    
    # --- Gini Importance fra Random Forest (caret wrapper til ranger eller randomForest) ---
    importance_rf <- varImp(rf_model)$importance
    
    # --- Gini Importance fra XGBoost ---
    importance_xgb <- xgb.importance(model = xgb_model$finalModel)
    
    # -- Decision Tree (rpart) --
    importance_tree <- tree_model$variable.importance %>% 
      enframe(name = "Variable", value = "Importance") %>%
      mutate(Model = "Decision Tree")
    
    # -- Random Forest --
    importance_rf <- importance_rf %>%
      rownames_to_column("Variable") %>%
      mutate(Importance = rowMeans(across(c("no", "yes")))) %>%
      select(Variable, Importance) %>%
      mutate(Model = "Random Forest")
    
    # -- XGBoost (xgb.importance) --
    importance_xgb <- importance_xgb %>%
      mutate(Variable = Feature, Importance = Gain) %>%
      select(Variable, Importance) %>%
      mutate(Model = "XGBoost")
    
    # Normalisér alle modeller så deres importance summerer til 100
    normalize_importance <- function(df) {
      df %>%
        mutate(Importance = Importance / sum(Importance) * 100)
    }
    
    # Gør det for hver model
    importance_tree_norm <- normalize_importance(importance_tree)
    importance_rf_norm <- normalize_importance(importance_rf)
    importance_xgb_norm <- normalize_importance(importance_xgb)
    
    # Bind dem sammen
    importance_combined <- bind_rows(
      importance_tree_norm,
      importance_rf_norm,
      importance_xgb_norm
    )
    
    # Oversæt variabelnavne til mere læsevenlige navne
    importance_combined <- importance_combined %>%
      mutate(Variable = case_when(
        Variable == "shot_distance" ~ "Afstand til mål",
        Variable == "shot_angle" ~ "Vinkel mod mål",
        Variable == "SHOTBODYPART.head_or_other" ~ "Hoved eller andet",
        Variable == "SHOTBODYPART.left_foot" ~ "Venstre fod",
        Variable == "SHOTBODYPART.right_foot" ~ "Højre fod",
        TRUE ~ Variable
      ))
    
    # -- Plot --
    p <- ggplot(importance_combined, aes(x = reorder(Variable, Importance), y = Importance, fill = Model)) +
      geom_col(position = "dodge", color = "black") +
      coord_flip() +
      labs(
        title = "Afstand og vinkel, er altid de mest vigtigste",
        x = NULL,
        y = "Gini Importance"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "bottom"
      ) +
      scale_fill_manual(values = c(
        "Decision Tree" = "#f4a261",
        "Random Forest" = "#2a9d8f",
        "XGBoost" = "#264653"
      ))
    
    print(p)
  })
  
  # Kamp Analyse: Hold- og kampvælger
  output$match_selector <- renderUI({
    req(input$match_selected_team)
    message("Valgt hold (Kamp Analyse): ", input$match_selected_team)
    
    # Hent kampe for det valgte hold
    matches <- allshotevents %>%
      filter(TEAMNAME.x == input$match_selected_team) %>%  # Filtrer kun kampe for det valgte hold
      distinct(MATCH_WYID.x, MATCH_LABEL, .keep_all = TRUE) %>%  # Sørg for unikke kampe
      filter(!is.na(MATCH_LABEL), MATCH_LABEL != "") %>%  # Fjern NA og tomme strenge
      arrange(MATCH_WYID.x)  # Sorter efter MATCH_WYID.x (kan erstattes med dato, hvis tilgængelig)
    
    # Log for at inspicere data
    message("Antal kampe for ", input$match_selected_team, ": ", nrow(matches))
    message("MATCH_LABEL værdier: ", paste(matches$MATCH_LABEL, collapse = ", "))
    message("MATCH_WYID.x værdier: ", paste(matches$MATCH_WYID.x, collapse = ", "))
    
    # Tjek om der er kampe
    if (nrow(matches) == 0) {
      return(selectInput("match_selected_match", "Vælg kamp:", choices = c("Ingen kampe tilgængelige" = "")))
    }
    
    # Opret valgmuligheder
    match_choices <- setNames(as.character(matches$MATCH_WYID.x), matches$MATCH_LABEL)
    
    # Tjek for duplikerede MATCH_WYID.x
    if (any(duplicated(matches$MATCH_WYID.x))) {
      message("Advarsel: Duplikerede MATCH_WYID.x fundet: ", paste(matches$MATCH_WYID.x[duplicated(matches$MATCH_WYID.x)], collapse = ", "))
    }
    
    # Tjek for duplikerede MATCH_LABEL og gør dem unikke
    if (any(duplicated(matches$MATCH_LABEL))) {
      message("Advarsel: Duplikerede MATCH_LABEL fundet: ", paste(matches$MATCH_LABEL[duplicated(matches$MATCH_LABEL)], collapse = ", "))
      matches <- matches %>%
        mutate(MATCH_LABEL = paste0(MATCH_LABEL))
      match_choices <- setNames(as.character(matches$MATCH_WYID.x), matches$MATCH_LABEL)
    }
    
    # Tilføj "Alle kampe" som mulighed
    match_choices <- c("Alle kampe" = "Alle kampe", match_choices)
    
    selectInput("match_selected_match", "Vælg kamp:", choices = match_choices)
  })
  
  get_filtered_match_data <- reactive({
    message("Kører get_filtered_match_data...")
    message("Valgt hold: ", input$match_selected_team)
    message("Valgt kamp: ", input$match_selected_match)
    
    # Hent kamp-ID'er for det valgte hold
    match_ids <- allshotevents %>%
      filter(TEAMNAME.x == input$match_selected_team) %>%
      distinct(MATCH_WYID.x) %>%
      pull(MATCH_WYID.x)
    
    message("Antal kampe fundet for ", input$match_selected_team, ": ", length(match_ids))
    message("match_ids: ", paste(match_ids, collapse = ", "))
    
    # Filtrer data for de relevante kampe
    df <- allshotevents %>%
      filter(MATCH_WYID.x %in% match_ids)
    
    message("Antal rækker efter match-filtrering: ", nrow(df))
    
    # Hvis en specifik kamp er valgt, filtrer yderligere
    if (!is.null(input$match_selected_match) && input$match_selected_match != "Alle kampe" && input$match_selected_match != "") {
      selected_match_id <- as.character(input$match_selected_match)
      message("Type af MATCH_WYID.x: ", class(df$MATCH_WYID.x))
      message("Type af selected_match_id: ", class(selected_match_id))
      message("selected_match_id: ", selected_match_id)
      
      # Konverter MATCH_WYID.x til karakter for korrekt sammenligning
      df <- df %>%
        mutate(MATCH_WYID.x = as.character(MATCH_WYID.x)) %>%
        filter(MATCH_WYID.x == selected_match_id)
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
        filter(MATCH_WYID.x == as.character(input$match_selected_match)) %>%
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
          "Type: ", Team_Type, "<br>",
          "Afstand: ", round(shot_distance, 2), " m", "<br>",
          "Vinkel: ", round(shot_angle, 2), " grader"
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
        `Gns. Afstand` = mean(shot_distance, na.rm = TRUE),
        `Gns. Vinkel` = mean(shot_angle, na.rm = TRUE),
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
        `Over-/underpræstation`,
        `Gns. Afstand`,
        `Gns. Vinkel`
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
  
  
  
  
  
  
  
  
  
  
  
  # -- Brøndby IF --
  # Beregn AUC for 2024/25-datasættet
  auc_data_2425 <- reactive({
    # Filtrer data og fjern NA-værdier
    data <- allshotevents2425_merged %>%
      filter(!is.na(xG_XGB) & !is.na(SHOTXG) & !is.na(SHOTISGOAL)) %>%
      mutate(
        SHOTISGOAL = as.numeric(as.character(SHOTISGOAL)),
        xG_XGB = as.numeric(as.character(xG_XGB)),
        SHOTXG = as.numeric(as.character(SHOTXG))
      )
    
    # Beregn AUC for XGBoost
    roc_xgboost <- roc(data$SHOTISGOAL, data$xG_XGB, quiet = TRUE)
    auc_xgboost <- auc(roc_xgboost)
    
    # Beregn AUC for WyScout
    roc_wyscout <- roc(data$SHOTISGOAL, data$SHOTXG, quiet = TRUE)
    auc_wyscout <- auc(roc_wyscout)
    
    # Beregn procentforskel
    percent_diff <- ((auc_xgboost - auc_wyscout) / auc_wyscout) * 100
    
    # Returner resultater
    list(
      auc_xgboost = round(auc_xgboost, 4),
      auc_wyscout = round(auc_wyscout, 4),
      percent_diff = round(percent_diff, 2)
    )
  })
  
  
  # ValueBox outputs for intro_b (2024/25 data)
  output$xgb_auc_box_2425 <- renderValueBox({
    auc_vals <- auc_data_2425()
    valueBox(
      sprintf("%.4f", auc_vals$auc_xgboost), 
      "XGBoost AUC (2024/25)", 
      icon = icon("chart-line"), 
      color = "orange"
    )
  })
  
  output$wyscout_auc_box_2425 <- renderValueBox({
    auc_vals <- auc_data_2425()
    valueBox(
      sprintf("%.4f", auc_vals$auc_wyscout), 
      "WyScout AUC (2024/25)", 
      icon = icon("chart-line"), 
      color = "green"
    )
  })
  
  output$improvement_box_2425 <- renderValueBox({
    auc_vals <- auc_data_2425()
    valueBox(
      sprintf("%.2f%%", auc_vals$percent_diff), 
      "XGBoost Forbedring over WyScout", 
      icon = icon("arrow-up"), 
      color = "blue"
    )
  })
  
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
  
  observe({
    player_choices <- allshotevents2425_merged %>%
      filter(TEAM_WYID == brondby_team_id) %>%
      distinct(SHORTNAME) %>%
      arrange(SHORTNAME) %>%
      pull(SHORTNAME)
    
    updateSelectInput(session, "player_select",
                      choices = player_choices,
                      selected = player_choices[1])
  })
  
  # Dynamisk opdater kamp-vælgeren baseret på valgt spiller
  output$player_match_selector <- renderUI({
    req(input$player_select)
    
    # Hent kampe, hvor den valgte spiller har skudt
    matches <- allshotevents2425_merged %>%
      filter(TEAM_WYID == brondby_team_id, SHORTNAME == input$player_select) %>%
      distinct(MATCH_LABEL) %>%
      arrange(MATCH_LABEL) %>%
      pull(MATCH_LABEL)
    
    # Tilføj "Alle kampe" som mulighed
    match_choices <- c("Alle kampe" = "Alle kampe", matches)
    
    selectInput("player_match_select", "Vælg kamp:", choices = match_choices)
  })
  
  # Field plot for spillerens skud
 observe({
  player_choices <- allshotevents2425_merged %>%
    filter(TEAM_WYID == brondby_team_id) %>%
    distinct(SHORTNAME) %>%
    arrange(SHORTNAME) %>%
    pull(SHORTNAME)
  
  updateSelectInput(session, "player_select",
                    choices = player_choices,
                    selected = player_choices[1])
})

# Dynamisk opdater kamp-vælgeren baseret på valgt spiller
output$player_match_selector <- renderUI({
  req(input$player_select)
  
  # Hent kampe, hvor den valgte spiller har skudt
  matches <- allshotevents2425_merged %>%
    filter(TEAM_WYID == brondby_team_id, SHORTNAME == input$player_select) %>%
    distinct(MATCH_LABEL) %>%
    arrange(MATCH_LABEL) %>%
    pull(MATCH_LABEL)
  
  # Tilføj "Alle kampe" som mulighed
  match_choices <- c("Alle kampe" = "Alle kampe", matches)
  
  selectInput("player_match_select", "Vælg kamp:", choices = match_choices)
})

# Field plot for spillerens skud
output$player_shot_map <- renderPlotly({
  req(input$player_select)
  
  # Filtrer data for den valgte spiller
  player_shots <- allshotevents2425_merged %>%
    filter(TEAM_WYID == brondby_team_id, SHORTNAME == input$player_select) %>%
    mutate(
      SHOTISGOAL = as.numeric(as.character(SHOTISGOAL)),
      xG_XGB = as.numeric(as.character(xG_XGB)),
      Goal_Label = ifelse(SHOTISGOAL == 1, "Mål", "Ikke mål"),
      # Hent modstanderholdets navn
      Opponent = OFFICIALNAME,
      Hover_Text = paste(
        "xG: ", round(xG_XGB, 3), "<br>",
        "Spiller: ", SHORTNAME, "<br>",
        #"Modstander: ", Opponent, "<br>",
        "Afstand: ", round(shot_distance, 2), " m", "<br>",
        "Vinkel: ", round(shot_angle, 2), " grader"
      )
    )
  
  # Hvis en specifik kamp er valgt, filtrer yderligere
  if (!is.null(input$player_match_select) && input$player_match_select != "Alle kampe") {
    player_shots <- player_shots %>%
      filter(MATCH_LABEL == input$player_match_select)
  }
  
  if (nrow(player_shots) == 0) {
    p <- ggplot() +
      annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "#f5f5f5") +
      coord_fixed(xlim = c(0, 100), ylim = c(0, 100)) +
      theme_pitch() +
      labs(
        title = paste("Skud for", input$player_select),
        subtitle = "Ingen skud tilgængelige for denne spiller/kamp."
      ) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(face = "italic", hjust = 0.5)
      )
    return(ggplotly(p))
  }
  
  # Lav field plottet
  p <- ggplot(player_shots, aes(x = LOCATIONX, y = LOCATIONY, text = Hover_Text)) +
    annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "#f5f5f5") +
    geom_point(aes(size = xG_XGB, fill = xG_XGB, shape = Goal_Label), alpha = 0.7) +
    scale_fill_gradient(low = "#0D1C8A", high = "#FDBA21", name = "xG") +
    scale_shape_manual(values = c("Mål" = 21, "Ikke mål" = 4), name = "Udfald") +
    scale_size_continuous(range = c(2, 8), name = "xG") +
    coord_fixed(xlim = c(0, 100), ylim = c(0, 100)) +
    theme_pitch() +
    labs(
      title = paste("Skud for", input$player_select),
      subtitle = if (!is.null(input$player_match_select) && input$player_match_select != "Alle kampe") {
        paste("Kamp:", input$player_match_select)
      } else {
        "Alle kampe i sæsonen"
      }
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

# Start Shiny appen
shinyApp(ui, server)