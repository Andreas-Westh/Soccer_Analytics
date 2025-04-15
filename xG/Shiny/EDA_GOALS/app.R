library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggsoccer)
library(viridis)
library(purrr)
library(tidyverse)
library(ggimage)
library(gridExtra)

allshotevents <- readRDS("allshotevents_Shiny_EDA_1.rds")
train_data <- readRDS("train_data.rds")
test_data <- readRDS("test_data.rds")

# -- Farver til body_parts -----------------------------------------------------------
body_colors <- c(
  "head_or_other" = "#FDBA21",  # gul
  "left_foot" = "#0D1C8A",      # blå
  "right_foot" = "#FC7753"      # rødlig
)

# -- UI -----------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Måldata i Superligaen"),
  dashboardSidebar(
    sidebarMenu(
      id = "plot_choice",
      menuItem("📘 Introduktion", tabName = "intro", icon = icon("info-circle")),
      menuItem("Skudposition", tabName = "location_points", icon = icon("crosshairs")),
      menuItem("Skudvinkel", tabName = "shot_angle", icon = icon("angle-right")),
      menuItem("Afstand til mål", tabName = "shot_distance", icon = icon("ruler-horizontal")),
      menuItem("Kropsdel", tabName = "body_part", icon = icon("running")),
      menuItem("Team Ranking", tabName = "team_ranking", icon = icon("sort-amount-down-alt")),
      menuItem("Spiller-rating", tabName = "player_rating", icon = icon("chart-bar")),
      menuItem("Antal events i possession", tabName = "possession_events", icon = icon("list-ol")),
      menuItem("Index for possession", tabName = "possession_index", icon = icon("fingerprint")),
      menuItem("Varighed af possession", tabName = "possession_duration", icon = icon("clock")),
      br(),
      checkboxInput("split", "Vis opdelt på træning/test", value = FALSE),
      checkboxInput("absolute_counts", "Vis absolutte tal", value = FALSE)
    ),
    div(
      style = "background-color: rgba(200,200,200,0.4); padding: 10px; border-radius: 10px; margin: 10px; font-size: 13px;",
      "OBS: Hvis sider ikke vises korrekt, så klik på 'Skudposition' for at genindlæse visningen."
    )
  ),
 
  dashboardBody(
    tags$head(
      tags$link(rel = "shortcut icon", href = "favicon.ico")
    ),
    tabItems(
      # Intro-tab
      tabItem(tabName = "intro",
              h2("Velkommen til måldata-dashboardet"),
              p("Denne applikation giver dig mulighed for at udforske skuddata fra Superligaens 2023/2024-sæson med særligt fokus på, hvilke faktorer der adskiller mål fra ikke-mål."),
              p("Ved at undersøge fordelingen af skud ud fra forskellige variabler som afstand, vinkel og kropsdel – opdelt på om afslutningen førte til mål eller ej – kan du få indsigt i, hvad der øger sandsynligheden for en succesfuld afslutning."),
              tags$ul(
                tags$li("Brug menuen i venstre side til at vælge én variabel ad gangen."),
                tags$li("Tjek 'Vis absolutte tal' for at se absolutte antal i stedet for procent."),
                tags$li("Tjek 'Vis opdelt på træning/test' for at se forskelle i dine splits."),
                tags$li("Hver variabel vises som plot, en forklarende konklusion og en oversigtstabel.")
              ),
              br(),
              h3("Hvad viser de forskellige variabler?"),
              tags$ul(
                tags$li(strong("Skudposition:"), " Hvor på banen afslutningerne bliver taget fra."),
                tags$li(strong("Skudvinkel:"), " I hvilken vinkel spilleren skyder mod målet."),
                tags$li(strong("Afstand til mål:"), " Hvor langt der er fra spilleren til målet ved afslutning."),
                tags$li(strong("Kropsdel:"), " Hvilken kropsdel spilleren bruger til at afslutte."),
                tags$li(strong("Team Ranking:"), " Holdenes placering i ligaen sidste sæson."),
                tags$li(strong("Spiller-rating:"), " Den individuelle spiller-rating fra FIFA-data."),
                tags$li(strong("Antal events i possession:"), " Hvor mange 'aktioner'events' der er i et angreb før afslutning."),
                tags$li(strong("Index for possession:"), " Hvornår i kampen possessionen (angrebet) finder sted."),
                tags$li(strong("Varighed af possession:"), " Hvor mange sekunder possessionen varer før afslutning.")
              ),
              br(),
              p("God fornøjelse!")
      ),
      # Visualiseringer
      tabItem(tabName = "location_points",
              fluidRow(
                box(title = "Plot", width = 8, solidHeader = TRUE, status = "primary",
                    uiOutput("plots_ui")),
                box(title = "Konklusion", width = 4, solidHeader = TRUE, status = "info",
                    textOutput("conclusion_text"))
              ),
              fluidRow(
                conditionalPanel(
                  condition = "input.plot_choice == 'location_points'",
                  box(title = "Oversigt over skud fordelt på baneområder", width = 12, solidHeader = TRUE, status = "warning",
                      tableOutput("position_area_table"))
                )
              ),
              fluidRow(
                conditionalPanel(
                  condition = "input.plot_choice == 'shot_distance'",
                  column(
                    width = 6,
                    box(title = "Afstandslinjer på banen", width = NULL, solidHeader = TRUE, status = "warning",
                        plotOutput("distance_lines_plot", height = "400px"))
                  ),
                  column(
                    width = 6,
                    box(title = "Oversigt over skud fordelt på afstandsgrupper", width = NULL, solidHeader = TRUE, status = "warning",
                        tableOutput("shot_distance_table"))
                  )
                )
              ),
              fluidRow(
                conditionalPanel(
                  condition = "input.plot_choice == 'shot_angle'",
                  column(
                    width = 6,
                    box(title = "Visuel forklaring på skudvinkler", width = NULL, solidHeader = TRUE, status = "warning",
                        plotOutput("shot_angle_visual", height = "400px"))
                  ),
                  column(
                    width = 6,
                    box(title = "Oversigt over skud fordelt på vinkelgrupper", width = NULL, solidHeader = TRUE, status = "warning",
                        tableOutput("shot_angle_table"))
                  )
                )
              ),
              fluidRow(
                conditionalPanel(
                  condition = "input.plot_choice == 'body_part'",
                  column(
                    width = 6,
                    box(title = "Skudpositioner fordelt på kropsdel", width = NULL, solidHeader = TRUE, status = "warning",
                        plotOutput("body_location_plot", height = "400px"))
                  ),
                  column(
                    width = 6,
                    box(title = "Oversigt over skud fordelt på kropsdel", width = NULL, solidHeader = TRUE, status = "warning",
                        tableOutput("body_part_table"))
                  )
                )
              ),
              fluidRow(
                conditionalPanel(
                  condition = "input.plot_choice == 'team_ranking'",
                  box(title = "Oversigt over holdenes skudstatistik", width = 12, solidHeader = TRUE, status = "warning",
                      htmlOutput("team_ranking_summary_ui")
                      )
                )
              ),
              fluidRow(
                conditionalPanel(
                  condition = "input.plot_choice == 'player_rating'",
                  box(title = "Gennemsnit og variation i spiller-ratings", width = 12, solidHeader = TRUE, status = "info",
                      tableOutput("rating_table"))
                )
              ),
              fluidRow(
                conditionalPanel(
                  condition = "input.plot_choice == 'possession_events'",
                  box(title = "Opsummerende statistik for antal events i possession", 
                      width = 12, solidHeader = TRUE, status = "warning",
                      tableOutput("possession_events_table"))
                )
              ),
              fluidRow(
                conditionalPanel(
                  condition = "input.plot_choice == 'possession_index'",
                  box(
                    title = "Beskrivende statistik for possession index",
                    width = 12,
                    solidHeader = TRUE,
                    status = "warning",
                    tableOutput("possession_index_table")
                  )
                )
              ),
              fluidRow(
                conditionalPanel(
                  condition = "input.plot_choice == 'possession_duration'",
                  box(
                    title = "Opsummerende statistik opdelt efter Wyscout-varighedskategorier",
                    width = 12,
                    solidHeader = TRUE,
                    status = "warning",
                    tableOutput("duration_summary")
                  )
                )
              )
      )
    )
  )
)

# -- Konklusionstekster -------------------------------------------
get_conclusion <- function(var) {
  switch(var,
         "location_points" = "De fleste afslutninger i Superligaen bliver taget inden for det store felt, og det er tydeligt, at mål typisk scores tættere på målet. Målraten falder markant, jo længere væk skuddet tages – fx er næsten 1 ud af 3 skud fra målfeltet mål, mens det kun gælder ca. 1 ud af 22 skud uden for feltet. Skudposition spiller altså en væsentlig rolle for chancen for at score.",
         "shot_angle" = "Afslutningens vinkel mod målet har en tydelig sammenhæng med sandsynligheden for at score. Som det ses klart på plottet, stiger andelen af mål markant, jo større skudvinklen er. Skud inden for de smalleste vinkler (under 30°) resulterer næsten aldrig i mål, mens over en tredjedel af skud fra vinkler over 60° går i nettet. Det er intuitivt: en bred vinkel giver bedre udsyn og større del af målet at sigte efter, hvilket øger chancen for succes markant.",
         "shot_distance" = "Skuddets afstand til mål har en klar betydning for sandsynligheden for at score. Det ses tydeligt i fordelingen, hvor mål næsten udelukkende opstår ved korte afstande – især inden for 10 meter. Andelen af mål falder drastisk, jo længere væk skuddet tages, hvilket stemmer godt overens med fodboldfaglig intuition: det er ganske enkelt sværere at score langt udefra.

Dette mønster ses også i plottet over de absolutte tal, hvor mål og ikke-mål danner en tydelig forskudt fordeling. Mål er koncentreret omkring de kortere afstande (venstre side af fordelingen), mens ikke-mål er mere jævnt fordelt – og forskyder sig mod højre. Det er et stærkt billede på, hvordan skud fra længere afstande i langt højere grad ikke fører til mål – hvilket understreger den lave effektivitet ved langskud.",
         "body_part" = "En tydelig tendens i data er, at skud med hovedet (eller andre kropsdele end benene) i gennemsnit har en højere målrate end skud med både venstre og højre fod. Selvom hoved-/andet-skud kun udgør ca. 16 % af alle skud, er deres målrate den højeste (13,7 %), hvilket kan skyldes, at disse oftest afsluttes tættere på målet – som også understøttes af deres lavere gennemsnitsafstand.

Plottet understreger denne pointe visuelt: hoved-/andet-skud er koncentreret omkring målfeltet, mens fodafslutninger fordeler sig bredere, også længere ude på banen.

Det er altså ikke nødvendigvis kropsdelen i sig selv, der skaber forskellen – men snarere hvad kropsdelen fortæller os om konteksten for afslutningen. Et hovedstød indikerer typisk, at spilleren er tæt på mål og modtager en aflevering i luften, hvorimod skud med fødderne ofte kommer fra mere varierede positioner og situationer. På den måde fungerer kropsdel ikke blot som en teknisk oplysning, men som en stærk indikator for skudtypens karakter og kontekst.",
         "team_ranking" = "Selvom man kunne forvente, at holdenes placering i ligaen hænger tæt sammen med deres evne til at score mål, tyder dataene på, at forskellene i målrate på tværs af Superliga-holdene er relativt små.

Brøndby skiller sig ganske vist ud med en målrate på 17,1 %, men de fleste andre hold – både i toppen og bunden af tabellen – ligger i et snævert interval mellem 10 % og 13 %. For eksempel har både Nordsjælland (#1) og Lyngby (#12) meget lignende målrater, på henholdsvis 12,7 % og 11,1 %.

Det kunne altså tyde på, at en høj placering i ligaen - sidste sæson - ikke nødvendigvis afspejler en høj effektivitet foran målet.",
         "player_rating" = "Spillernes individuelle FIFA-ratings (både overall og potential) ser umiddelbart ikke ud til at have en entydig sammenhæng med målraten. Selvom vi kunne forvente, at bedre ratede spillere ville have lettere ved at score, viser målraten på tværs af rating-bins kun små udsving og intet klart mønster.

Eksempelvis scorer spillere med overall-rating mellem 50–60 faktisk en anelse oftere end dem i højere bins, og det samme mønster ses i potential-rating, hvor målraten topper i de lavere og højeste bins, men er lavere i midtergruppen.

Det kunne tyde på, at den individuelle spiller-rating ikke er en afgørende faktor i sig selv for, om et skud bliver til mål — i hvert fald ikke i denne Superliga-kontekst.",
         "possession_events" = "Andelen af mål falder generelt en smule, jo længere angrebet er – undtagen i den længste bin (30+ events), hvor mål-raten pludselig stiger til 14%. Det er dog værd at bemærke, at denne kategori kun indeholder 114 skud, hvilket gør det svært at sige noget sikkert.
Der kunne altså være en tendens til, at meget lange possessions har større sandsynlighed for mål, men det ville kræve et størrere datasæt/antal observationer at bekræfte.",
         "possession_index" = "Der synes ikke at være nogen klar sammenhæng mellem hvornår i kampen en possession starter og sandsynligheden for at den ender i mål. Målraten varierer kun lidt mellem bins, og selvom enkelte kategorier har lidt højere eller lavere værdier, er der ingen tydelig tendens i data. Det kunne derfor tyde på, at possessionens tidspunkt i kampen i sig selv ikke har stor betydning for udfaldet af afslutningen.",
         "possession_duration" = "Der er ikke store udsving i målraten på tværs af varighedskategorierne, men det ser ud til, at de korteste angreb (0–10 sekunder) en smule oftere fører til mål end længere angreb. Det kunne antyde, at “den der kommer først til mølle, får først malet” – altså at hurtige afslutninger efter fx omstillinger er en smule mere effektive. Forskellene er dog beskedne, og det er tvivlsomt, hvor stor betydning varighed isoleret set har for sandsynligheden for mål i forhold til andre, mere sigende variabler, som vi har set på før.",
         "Ukendt valg"
  )
}

# -- Plotfunktion -------------------------------------------------
make_plot <- function(data, var, absolute_counts) {
  switch(var,
         # Punktplot
         "location_points" = {
           ggplot(
             allshotevents %>% arrange(SHOTISGOAL),
             aes(x = LOCATIONX, y = LOCATIONY)
           ) +
             annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "white") +
             geom_point(
               aes(
                 color = factor(SHOTISGOAL),
                 alpha = ifelse(SHOTISGOAL == 1, 0.2, 0.8)
               ),
               size = 3
             ) +
             scale_color_manual(
               values = c("0" = "#0D1C8A", "1" = "#FDBA21"),
               labels = c("0" = "Ikke mål", "1" = "Mål"),
               name = "Resultat"
             ) +
             scale_alpha_identity() +
             coord_fixed(xlim = c(0, 100), ylim = c(0, 100)) +
             theme_pitch() +
             labs(
               title = "Skud i Superligen sæson 2023/2024",
               subtitle = "Klart størstedelen af alle skud der blev til mål, blev gjort tættere ved målet"
             ) +
             theme(
               plot.title = element_text(face = "bold", hjust = 0.5),
               plot.subtitle = element_text(face = "italic", hjust = 0.5),
               legend.position = "top"
             )
         },
         # Heatmap
         "location_heatmap" = {
           ggplot(data, aes(x = LOCATIONX, y = LOCATIONY)) +
             annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "white") +
             stat_density_2d(aes(fill = after_stat(density)), 
                             geom = "raster", contour = FALSE, alpha = 0.8) +
             scale_fill_viridis_c(option = "C") +
             theme_pitch() +
             labs(title = "Heatmap over skudpositioner")
         },
         # Shot angle
         "shot_angle" = {
           breaks <- c(seq(0, 90, by = 10), Inf)
           labels <- c(paste0("[", seq(0, 80, by = 10), ",", seq(10, 90, by = 10), ")"), "[90,+)")
           
           data <- data %>%
             mutate(angle_bin = cut(shot_angle, breaks = breaks, labels = labels, right = FALSE)) %>%
             filter(!is.na(angle_bin))
           
           binned_data <- data %>%
             group_by(angle_bin, SHOTISGOAL) %>%
             summarise(count = n(), .groups = "drop")
           
           if (absolute_counts) {
             ggplot(binned_data, aes(x = angle_bin, y = count, fill = factor(SHOTISGOAL))) +
               geom_col(position = position_dodge(width = 0.8), width = 0.7) +
               geom_text(aes(label = count),
                         position = position_dodge(width = 0.8),
                         vjust = -0.3, size = 3) +
               scale_fill_manual(values = c("0" = "#FDBA21", "1" = "#0D1C8A"),
                                 labels = c("Ikke mål", "Mål")) +
               labs(
                 x = "Skudvinkel (grader)",
                 y = "Antal skud",
                 fill = "Udfald",
                 title = "Skudvinkel – Antal skud pr. vinkelgruppe"
               ) +
               theme_minimal(base_size = 12) +
               theme(
                 plot.title = element_text(hjust = 0.5, face = "bold"),
                 axis.text.x = element_text(angle = 45, hjust = 1)
               )
           } else {
             binned_data <- binned_data %>%
               group_by(angle_bin) %>%
               mutate(total = sum(count),
                      percentage = 100 * count / total)
             
             ggplot(binned_data, aes(x = angle_bin, y = percentage, fill = factor(SHOTISGOAL))) +
               geom_col(position = position_dodge(width = 0.8), width = 0.7) +
               geom_text(aes(label = sprintf("%.1f%%", percentage)),
                         position = position_dodge(width = 0.8),
                         vjust = -0.3, size = 3) +
               scale_fill_manual(values = c("0" = "#FDBA21", "1" = "#0D1C8A"),
                                 labels = c("Ikke mål", "Mål")) +
               scale_y_continuous(limits = c(0, 100)) +
               labs(
                 x = "Skudvinkel (grader)",
                 y = "Procent (%)",
                 fill = "Udfald",
                 title = "Skudvinkel – Procentvis fordeling pr. vinkelgruppe"
               ) +
               theme_minimal(base_size = 12) +
               theme(
                 plot.title = element_text(hjust = 0.5, face = "bold"),
                 axis.text.x = element_text(angle = 45, hjust = 1)
               )
           }
         },
         # Shot distance
         "shot_distance" = {
           breaks <- c(seq(0, 45, by = 5), Inf)
           labels <- c(paste0("[", seq(0, 40, by = 5), ",", seq(5, 45, by = 5), ")"), "[45,+)")
           
           data <- data %>%
             mutate(dist_bin = cut(shot_distance, breaks = breaks, labels = labels, right = FALSE)) %>%
             filter(!is.na(dist_bin))
           
           binned_data <- data %>%
             group_by(dist_bin, SHOTISGOAL) %>%
             summarise(count = n(), .groups = "drop")
           
           if (absolute_counts) {
             ggplot(binned_data, aes(x = dist_bin, y = count, fill = factor(SHOTISGOAL))) +
               geom_col(position = position_dodge(width = 0.8), width = 0.7) +
               geom_text(aes(label = count),
                         position = position_dodge(width = 0.8),
                         vjust = -0.3, size = 3) +
               scale_fill_manual(values = c("0" = "#FDBA21", "1" = "#0D1C8A"),
                                 labels = c("Ikke mål", "Mål")) +
               labs(
                 x = "Skudafstand (meter)",
                 y = "Antal skud",
                 fill = "Udfald",
                 title = "Skudafstand – Antal skud pr. afstandsgruppe"
               ) +
               theme_minimal(base_size = 12) +
               theme(
                 plot.title = element_text(hjust = 0.5, face = "bold"),
                 axis.text.x = element_text(angle = 45, hjust = 1)
               )
           } else {
             binned_data <- binned_data %>%
               group_by(dist_bin) %>%
               mutate(total = sum(count),
                      percentage = 100 * count / total)
             
             ggplot(binned_data, aes(x = dist_bin, y = percentage, fill = factor(SHOTISGOAL))) +
               geom_col(position = position_dodge(width = 0.8), width = 0.7) +
               geom_text(aes(label = sprintf("%.1f%%", percentage)),
                         position = position_dodge(width = 0.8),
                         vjust = -0.3, size = 3) +
               scale_fill_manual(values = c("0" = "#FDBA21", "1" = "#0D1C8A"),
                                 labels = c("Ikke mål", "Mål")) +
               scale_y_continuous(limits = c(0, 100)) +
               labs(
                 x = "Skudafstand (meter)",
                 y = "Procent (%)",
                 fill = "Udfald",
                 title = "Skudafstand – Procentvis fordeling pr. afstandsgruppe"
               ) +
               theme_minimal(base_size = 12) +
               theme(
                 plot.title = element_text(hjust = 0.5, face = "bold"),
                 axis.text.x = element_text(angle = 45, hjust = 1)
               )
           }
         },
         # Body part
         "body_part" = {
           binned_data <- data %>%
             mutate(SHOTISGOAL = factor(SHOTISGOAL, levels = c(1, 0), labels = c("Mål", "Ikke mål"))) %>%
             count(SHOTBODYPART, SHOTISGOAL, name = "antal")
           
           if (absolute_counts) {
             ggplot(binned_data, aes(x = SHOTBODYPART, y = antal, fill = SHOTISGOAL)) +
               geom_col(position = position_dodge()) +
               geom_text(aes(label = antal),
                         position = position_dodge(width = 0.45),
                         vjust = -0.3, size = 3) +
               scale_fill_manual(values = c("Mål" = "#FDBA21", "Ikke mål" = "#0D1C8A")) +
               labs(x = "Kropsdel", y = "Antal skud", title = "Skud pr. kropsdel", fill = "Resultat") +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
           } else {
             binned_data <- binned_data %>%
               group_by(SHOTBODYPART) %>%
               mutate(procent = 100 * antal / sum(antal)) %>%
               ungroup()
             
             ggplot(binned_data, aes(x = SHOTBODYPART, y = procent, fill = SHOTISGOAL)) +
               geom_col(position = position_dodge()) +
               geom_text(aes(label = sprintf("%.1f%%", procent)),
                         position = position_dodge(width = 0.45),
                         vjust = -0.3, size = 3) +
               scale_fill_manual(values = c("Mål" = "#FDBA21", "Ikke mål" = "#0D1C8A")) +
               labs(x = "Kropsdel", y = "Andel (%)", title = "Skud pr. kropsdel (fordelt procentvis)", fill = "Resultat") +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
           }
         },
         # Team ranking
         "team_ranking" = {
           df_plot <- data %>%
             mutate(SHOTISGOAL = factor(SHOTISGOAL, levels = c(1, 0), labels = c("Mål", "Ikke mål"))) %>%
             count(TEAMNAME.x, Team_Ranking, IMAGEDATAURL, SHOTISGOAL, name = "antal") %>%
             mutate(label = paste0(TEAMNAME.x, " (#", Team_Ranking, ")")) %>%
             arrange(Team_Ranking)
           
           # Tjek for duplikationer (kan fjernes efter fejlfinding)
           if (nrow(df_plot %>% count(label, SHOTISGOAL) %>% filter(n > 1)) > 0) {
             warning("Duplikerede rækker fundet i df_plot for team_ranking")
           }
           
           målrate_df <- df_plot %>%
             group_by(label, Team_Ranking) %>%
             summarise(
               mål_antal = sum(antal[SHOTISGOAL == "Mål"]),
               total_antal = sum(antal),
               .groups = "drop"
             ) %>%
             mutate(label_text = paste0(round(100 * mål_antal / total_antal, 1), "%"))
           
           if (absolute_counts) {
             ggplot(df_plot, aes(x = reorder(label, -Team_Ranking), y = antal, fill = SHOTISGOAL)) +
               geom_col(position = position_dodge(width = 0.8), color = "black") +
               geom_text(
                 aes(x = reorder(label, -Team_Ranking), y = 1.5, label = antal),
                 inherit.aes = FALSE,
                 hjust = 0,
                 size = 4.5,
                 fontface = "bold",
                 color = "white"
               ) +
               geom_image(
                 data = df_plot %>% distinct(label, IMAGEDATAURL, Team_Ranking),
                 aes(x = reorder(label, -Team_Ranking), y = max(df_plot$antal) + 10, image = IMAGEDATAURL),
                 inherit.aes = FALSE,
                 size = 0.06,
                 asp = 1.2
               ) +
               coord_flip() +
               scale_fill_manual(values = c("Mål" = "#FDBA21", "Ikke mål" = "#0D1C8A")) +
               labs(x = NULL, y = "Antal skud", title = "Antal skud pr. hold", fill = "Resultat") +
               theme_minimal(base_size = 16) +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
           } else {
             df_plot <- df_plot %>%
               group_by(label) %>%
               mutate(procent = 100 * antal / sum(antal)) %>%
               ungroup()
             
             ggplot(df_plot, aes(x = reorder(label, -Team_Ranking), y = procent, fill = SHOTISGOAL)) +
               geom_col(position = position_dodge(width = 0.8), color = "black") +
               geom_text(
                 data = målrate_df,
                 aes(x = reorder(label, -Team_Ranking), y = 1.5, label = label_text),
                 inherit.aes = FALSE,
                 hjust = 0,
                 size = 4.5,
                 fontface = "bold",
                 color = "white"
               ) +
               geom_image(
                 data = df_plot %>% distinct(label, IMAGEDATAURL, Team_Ranking),
                 aes(x = reorder(label, -Team_Ranking), y = 95, image = IMAGEDATAURL),
                 inherit.aes = FALSE,
                 size = 0.06,
                 asp = 1.2
               ) +
               coord_flip(ylim = c(0, 110)) +
               scale_fill_manual(values = c("Mål" = "#FDBA21", "Ikke mål" = "#0D1C8A")) +
               labs(x = NULL, y = "Andel (%)", 
                    title = "Andel mål vs. ikke mål pr. hold", 
                    subtitle = "Det hvide tal, viser holdets målrate",
                    fill = "Resultat") +
               theme_minimal(base_size = 16) +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
           }
         },
         # Player rating
         "player_rating" = {
           data_ratings <- data %>%
             mutate(SHOTISGOAL = factor(SHOTISGOAL, levels = c(1, 0), labels = c("Mål", "Ikke mål")))
           
           bin_rating_data <- function(df, rating_col) {
             df %>%
               select(rating = {{ rating_col }}, SHOTISGOAL) %>%
               drop_na() %>%
               mutate(bin = cut(rating, breaks = seq(40, 100, by = 5), right = FALSE)) %>%
               group_by(bin, SHOTISGOAL) %>%
               summarise(count = n(), .groups = "drop")
           }
           
           df_overall <- bin_rating_data(data_ratings, overall)
           df_potential <- bin_rating_data(data_ratings, potential)
           
           mean_overall <- mean(data$overall, na.rm = TRUE)
           mean_potential <- mean(data$potential, na.rm = TRUE)
           
           if (absolute_counts) {
             plot_overall <- ggplot(df_overall, aes(x = bin, y = count, fill = SHOTISGOAL)) +
               geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "white") +
               geom_text(aes(label = count),
                         position = position_dodge(width = 0.8),
                         vjust = -0.3, size = 3) +
               scale_fill_manual(values = c("Mål" = "#FDBA21", "Ikke mål" = "#0D1C8A")) +
               labs(
                 title = "Skudfordeling – Overall-rating",
                 subtitle = "Antal skud pr. rating-bin",
                 x = "Overall-rating", y = "Antal skud", fill = "Resultat"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                     plot.subtitle = element_text(hjust = 0.5))
             
             plot_potential <- ggplot(df_potential, aes(x = bin, y = count, fill = SHOTISGOAL)) +
               geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "white") +
               geom_text(aes(label = count),
                         position = position_dodge(width = 0.8),
                         vjust = -0.3, size = 3) +
               scale_fill_manual(values = c("Mål" = "#FDBA21", "Ikke mål" = "#0D1C8A")) +
               labs(
                 title = "Skudfordeling – Potential-rating",
                 subtitle = "Antal skud pr. rating-bin",
                 x = "Potential-rating", y = "Antal skud", fill = "Resultat"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                     plot.subtitle = element_text(hjust = 0.5))
             
             gridExtra::grid.arrange(plot_overall, plot_potential, ncol = 2)
           } else {
             df_overall <- df_overall %>%
               group_by(bin) %>%
               mutate(procent = 100 * count / sum(count))
             df_potential <- df_potential %>%
               group_by(bin) %>%
               mutate(procent = 100 * count / sum(count))
             
             plot_overall <- ggplot(df_overall, aes(x = bin, y = procent, fill = SHOTISGOAL)) +
               geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "white") +
               geom_text(aes(label = sprintf("%.1f%%", procent)),
                         position = position_dodge(width = 0.8),
                         vjust = -0.3, size = 3) +
               scale_fill_manual(values = c("Mål" = "#FDBA21", "Ikke mål" = "#0D1C8A")) +
               labs(
                 title = "Skudfordeling – Overall-rating",
                 subtitle = "Andel mål/ikke mål pr. rating-bin",
                 x = "Overall-rating", y = "Andel (%)", fill = "Resultat"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                     plot.subtitle = element_text(hjust = 0.5))
             
             plot_potential <- ggplot(df_potential, aes(x = bin, y = procent, fill = SHOTISGOAL)) +
               geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "white") +
               geom_text(aes(label = sprintf("%.1f%%", procent)),
                         position = position_dodge(width = 0.8),
                         vjust = -0.3, size = 3) +
               scale_fill_manual(values = c("Mål" = "#FDBA21", "Ikke mål" = "#0D1C8A")) +
               labs(
                 title = "Skudfordeling – Potential-rating",
                 subtitle = "Andel mål/ikke mål pr. rating-bin",
                 x = "Potential-rating", y = "Andel (%)", fill = "Resultat"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                     plot.subtitle = element_text(hjust = 0.5))
             
             gridExtra::grid.arrange(plot_overall, plot_potential, ncol = 2)
           }
         },
         # Possession events
         "possession_events" = {
           breaks <- c(0, 5, 10, 15, 20, 30, 50, Inf)
           labels <- c("0–4", "5–9", "10–14", "15–19", "20–29", "30–49", "50+")
           
           data_binned <- data %>%
             filter(!is.na(POSSESSIONEVENTSNUMBER)) %>%
             mutate(SHOTISGOAL = factor(SHOTISGOAL, levels = c(1, 0), labels = c("Mål", "Ikke mål")),
                    events_bin = cut(POSSESSIONEVENTSNUMBER, breaks = breaks, labels = labels, right = FALSE)) %>%
             group_by(events_bin, SHOTISGOAL) %>%
             summarise(count = n(), .groups = "drop")
           
           if (absolute_counts) {
             ggplot(data_binned, aes(x = events_bin, y = count, fill = SHOTISGOAL)) +
               geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "white") +
               geom_text(aes(label = count),
                         position = position_dodge(width = 0.8),
                         vjust = -0.3, size = 3) +
               scale_fill_manual(values = c("Mål" = "#FDBA21", "Ikke mål" = "#0D1C8A")) +
               labs(
                 title = "Mål vs. ikke mål fordelt efter antal events i possession",
                 subtitle = "",
                 x = "Antal events i angreb (binned)",
                 y = "Antal skud",
                 fill = "Resultat"
               ) +
               theme_minimal(base_size = 13) +
               theme(
                 plot.title = element_text(hjust = 0.5, face = "bold"),
                 plot.subtitle = element_text(hjust = 0.5),
                 axis.text.x = element_text(face = "bold")
               )
           } else {
             data_binned <- data_binned %>%
               group_by(events_bin) %>%
               mutate(procent = 100 * count / sum(count))
             
             ggplot(data_binned, aes(x = events_bin, y = procent, fill = SHOTISGOAL)) +
               geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "white") +
               geom_text(aes(label = sprintf("%.1f%%", procent)),
                         position = position_dodge(width = 0.8),
                         vjust = -0.3, size = 3) +
               scale_fill_manual(values = c("Mål" = "#FDBA21", "Ikke mål" = "#0D1C8A")) +
               labs(
                 title = "Mål vs. ikke mål fordelt efter antal events i possession",
                 subtitle = "",
                 x = "Antal events i angreb (binned)",
                 y = "Andel (%)",
                 fill = "Resultat"
               ) +
               theme_minimal(base_size = 13) +
               theme(
                 plot.title = element_text(hjust = 0.5, face = "bold"),
                 plot.subtitle = element_text(hjust = 0.5),
                 axis.text.x = element_text(face = "bold")
               )
           }
         },
         # Possession index
         "possession_index" = {
           breaks <- c(0, 5, 10, 15, 20, 30, 40, Inf)
           labels <- c("0–4", "5–9", "10–14", "15–19", "20–29", "30–39", "40+")
           
           data_binned <- data %>%
             filter(!is.na(POSSESSIONEVENTINDEX)) %>%
             mutate(
               SHOTISGOAL = factor(SHOTISGOAL, levels = c(1, 0), labels = c("Mål", "Ikke mål")),
               index_bin = cut(POSSESSIONEVENTINDEX, breaks = breaks, labels = labels, right = FALSE)
             ) %>%
             group_by(index_bin, SHOTISGOAL) %>%
             summarise(count = n(), .groups = "drop")
           
           if (absolute_counts) {
             ggplot(data_binned, aes(x = index_bin, y = count, fill = SHOTISGOAL)) +
               geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "white") +
               geom_text(aes(label = count),
                         position = position_dodge(width = 0.8),
                         vjust = -0.3, size = 3) +
               scale_fill_manual(values = c("Mål" = "#FDBA21", "Ikke mål" = "#0D1C8A")) +
               labs(
                 title = "Hvornår i kampen bliver der afsluttet?",
                 subtitle = "Skud fordelt på hvor sent i kampen possession startede",
                 x = "Possession index i kampen (binned)",
                 y = "Antal skud",
                 fill = "Resultat"
               ) +
               theme_minimal(base_size = 13) +
               theme(
                 plot.title = element_text(hjust = 0.5, face = "bold"),
                 plot.subtitle = element_text(hjust = 0.5),
                 axis.text.x = element_text(face = "bold")
               )
           } else {
             data_binned <- data_binned %>%
               group_by(index_bin) %>%
               mutate(procent = 100 * count / sum(count))
             
             ggplot(data_binned, aes(x = index_bin, y = procent, fill = SHOTISGOAL)) +
               geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "white") +
               geom_text(aes(label = sprintf("%.1f%%", procent)),
                         position = position_dodge(width = 0.8),
                         vjust = -0.3, size = 3) +
               scale_fill_manual(values = c("Mål" = "#FDBA21", "Ikke mål" = "#0D1C8A")) +
               labs(
                 title = "Hvornår i kampen bliver der afsluttet?",
                 subtitle = "Skud fordelt på hvor sent i kampen possession startede",
                 x = "Possession index i kampen (binned)",
                 y = "Andel (%)",
                 fill = "Resultat"
               ) +
               theme_minimal(base_size = 13) +
               theme(
                 plot.title = element_text(hjust = 0.5, face = "bold"),
                 plot.subtitle = element_text(hjust = 0.5),
                 axis.text.x = element_text(face = "bold")
               )
           }
         },
         # Possession duration
         "possession_duration" = {
           data_binned <- data %>%
             filter(!is.na(POSSESSIONDURATION)) %>%
             mutate(
               SHOTISGOAL = factor(SHOTISGOAL, levels = c(1, 0), labels = c("Mål", "Ikke mål")),
               duration_bin = cut(
                 POSSESSIONDURATION,
                 breaks = c(0, 10, 20, 45, Inf),
                 labels = c("Short (0–10s)", "Medium (10–20s)", "Long (20–45s)", "Very long (45s+)"),
                 right = FALSE
               )
             ) %>%
             group_by(duration_bin, SHOTISGOAL) %>%
             summarise(count = n(), .groups = "drop")
           
           if (absolute_counts) {
             ggplot(data_binned, aes(x = duration_bin, y = count, fill = SHOTISGOAL)) +
               geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "white") +
               geom_text(aes(label = count),
                         position = position_dodge(width = 0.8),
                         vjust = -0.3, size = 3) +
               scale_fill_manual(values = c("Mål" = "#FDBA21", "Ikke mål" = "#0D1C8A")) +
               labs(
                 title = "Varighed af angrebet før afslutning",
                 subtitle = "Antal skud opdelt efter possessionens længde",
                 x = "Varighed (sekunder, Wyscout-kategorier)",
                 y = "Antal skud",
                 fill = "Resultat"
               ) +
               theme_minimal(base_size = 13) +
               theme(
                 plot.title = element_text(hjust = 0.5, face = "bold"),
                 plot.subtitle = element_text(hjust = 0.5),
                 axis.text.x = element_text(face = "bold")
               )
           } else {
             data_binned <- data_binned %>%
               group_by(duration_bin) %>%
               mutate(procent = 100 * count / sum(count))
             
             ggplot(data_binned, aes(x = duration_bin, y = procent, fill = SHOTISGOAL)) +
               geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "white") +
               geom_text(aes(label = sprintf("%.1f%%", procent)),
                         position = position_dodge(width = 0.8),
                         vjust = -0.3, size = 3) +
               scale_fill_manual(values = c("Mål" = "#FDBA21", "Ikke mål" = "#0D1C8A")) +
               labs(
                 title = "Varighed af angrebet før afslutning",
                 subtitle = "Fordeling af skud opdelt efter possessionens længde",
                 x = "Varighed (sekunder, Wyscout-kategorier)",
                 y = "Andel (%)",
                 fill = "Resultat"
               ) +
               theme_minimal(base_size = 13) +
               theme(
                 plot.title = element_text(hjust = 0.5, face = "bold"),
                 plot.subtitle = element_text(hjust = 0.5),
                 axis.text.x = element_text(face = "bold")
               )
           }
         }
  )
}

# -- Server -------------------------------------------------------
server <- function(input, output, session) {
  
  observe({
    updateTabItems(session, "plot_choice", selected = "intro")
  })
  
  output$plots_ui <- renderUI({
    if (input$split) {
      fluidRow(
        column(6, plotOutput("plot_train")),
        column(6, plotOutput("plot_test"))
      )
    } else {
      plotOutput("plot_combined")
    }
  })
  
  output$plot_combined <- renderPlot({
    make_plot(allshotevents, input$plot_choice, input$absolute_counts)
  })
  
  output$plot_train <- renderPlot({
    make_plot(train_data, input$plot_choice, input$absolute_counts) + ggtitle("Træningsdata")
  })
  
  output$plot_test <- renderPlot({
    make_plot(test_data, input$plot_choice, input$absolute_counts) + ggtitle("Testdata")
  })
  
  # Oversigtstabel for skudposition
  output$position_area_table <- renderTable({
    # Vælg data baseret på input$split
    data_list <- if (input$split) {
      list(train = train_data, test = test_data)
    } else {
      list(all = allshotevents)
    }
    
    # Funktion til at lave tabellen for et enkelt datasæt
    create_area_table <- function(df) {
      # Beregn antal skud pr. område og mål/ikke mål
      summary_data <- df %>%
        mutate(
          område = case_when(
            LOCATIONX > 94 & LOCATIONY >= 37 & LOCATIONY <= 63 ~ "Målfelt",
            LOCATIONX > 84 & LOCATIONY >= 19 & LOCATIONY <= 81 ~ "Store felt",
            TRUE ~ "Uden for feltet"
          ),
          SHOTISGOAL = factor(SHOTISGOAL, levels = c(0, 1), labels = c("Ikke mål", "Mål"))
        ) %>%
        group_by(område, SHOTISGOAL) %>%
        summarise(
          Antal_skud = n(),
          .groups = "drop"
        )
      
      # Lav tabel med totaler og mål-rate
      area_table <- summary_data %>%
        pivot_wider(
          names_from = SHOTISGOAL,
          values_from = Antal_skud,
          values_fill = 0,
          names_glue = "{SHOTISGOAL}_Antal_skud"
        ) %>%
        mutate(
          Total = `Ikke mål_Antal_skud` + Mål_Antal_skud,
          `Mål-rate (%)` = round(100 * Mål_Antal_skud / Total, 1)
        ) %>%
        select(
          område,
          Total,
          `Mål-rate (%)`
        ) %>%
        arrange(factor(område, levels = c("Målfelt", "Store felt", "Uden for feltet")))
      
      # Beregn total-række
      total_row <- summary_data %>%
        group_by(SHOTISGOAL) %>%
        summarise(
          Antal_skud = sum(Antal_skud),
          .groups = "drop"
        ) %>%
        pivot_wider(
          names_from = SHOTISGOAL,
          values_from = Antal_skud,
          values_fill = 0,
          names_glue = "{SHOTISGOAL}_Antal_skud"
        ) %>%
        mutate(
          område = "Total",
          Total = `Ikke mål_Antal_skud` + Mål_Antal_skud,
          `Mål-rate (%)` = round(100 * Mål_Antal_skud / Total, 1)
        ) %>%
        select(
          område,
          Total,
          `Mål-rate (%)`
        )
      
      # Beregn andel af samlede skud
      overall_total <- sum(area_table$Total)
      area_table <- area_table %>%
        mutate(
          `Andel af skud (%)` = round(100 * Total / overall_total, 1)
        )
      
      total_row <- total_row %>%
        mutate(
          `Andel af skud (%)` = 100.0
        )
      
      # Kombiner total-række med resten af tabellen
      bind_rows(total_row, area_table)
    }
    
    # Generer tabel baseret på om split er valgt
    if (input$split) {
      train_table <- create_area_table(data_list$train) %>% mutate(Dataset = "Træning")
      test_table <- create_area_table(data_list$test) %>% mutate(Dataset = "Test")
      bind_rows(train_table, test_table)
    } else {
      create_area_table(data_list$all) %>% mutate(Dataset = "Alle data")
    }
  })
  
  # Oversigtstabel for skudvinkel
  output$shot_angle_table <- renderTable({
    # Vælg data baseret på input$split
    data_list <- if (input$split) {
      list(train = train_data, test = test_data)
    } else {
      list(all = allshotevents)
    }
    
    # Funktion til at lave tabellen for et enkelt datasæt
    create_angle_table <- function(df) {
      # Definér nye vinkelintervaller
      breaks <- c(0, 30, 60, Inf)
      labels <- c("Lille (0-30°)", "Middel (30-60°)", "Stor (60°+)")
      
      # Beregn antal skud pr. vinkelgruppe, mål/ikke mål og standardafvigelse
      summary_data <- df %>%
        mutate(
          vinkelgruppe = cut(shot_angle, breaks = breaks, labels = labels, right = FALSE),
          SHOTISGOAL = factor(SHOTISGOAL, levels = c(0, 1), labels = c("Ikke mål", "Mål"))
        ) %>%
        filter(!is.na(vinkelgruppe)) %>%
        group_by(vinkelgruppe, SHOTISGOAL) %>%
        summarise(
          Antal_skud = n(),
          .groups = "drop"
        )
      
      # Beregn standardafvigelse pr. vinkelgruppe
      sd_data <- df %>%
        mutate(
          vinkelgruppe = cut(shot_angle, breaks = breaks, labels = labels, right = FALSE)
        ) %>%
        filter(!is.na(vinkelgruppe)) %>%
        group_by(vinkelgruppe) %>%
        summarise(
          SD_vinkel = round(sd(shot_angle, na.rm = TRUE), 1),
          .groups = "drop"
        )
      
      # Lav tabel med totaler og mål-rate
      angle_table <- summary_data %>%
        pivot_wider(
          names_from = SHOTISGOAL,
          values_from = Antal_skud,
          values_fill = 0,
          names_glue = "{SHOTISGOAL}_Antal_skud"
        ) %>%
        mutate(
          Total = `Ikke mål_Antal_skud` + Mål_Antal_skud,
          `Mål-rate (%)` = round(100 * Mål_Antal_skud / Total, 1)
        ) %>%
        left_join(sd_data, by = "vinkelgruppe") %>%
        select(
          Vinkelgruppe = vinkelgruppe,
          Total,
          `Mål-rate (%)`,
          `Standardafvigelse (grader)` = SD_vinkel
        )
      
      # Beregn total-række
      total_row <- summary_data %>%
        group_by(SHOTISGOAL) %>%
        summarise(
          Antal_skud = sum(Antal_skud),
          .groups = "drop"
        ) %>%
        pivot_wider(
          names_from = SHOTISGOAL,
          values_from = Antal_skud,
          values_fill = 0,
          names_glue = "{SHOTISGOAL}_Antal_skud"
        ) %>%
        mutate(
          Vinkelgruppe = "Total",
          Total = `Ikke mål_Antal_skud` + Mål_Antal_skud,
          `Mål-rate (%)` = round(100 * Mål_Antal_skud / Total, 1),
          `Standardafvigelse (grader)` = round(sd(df$shot_angle, na.rm = TRUE), 1)
        ) %>%
        select(
          Vinkelgruppe,
          Total,
          `Mål-rate (%)`,
          `Standardafvigelse (grader)`
        )
      
      # Beregn andel af samlede skud
      overall_total <- sum(angle_table$Total)
      angle_table <- angle_table %>%
        mutate(
          `Andel af skud (%)` = round(100 * Total / overall_total, 1)
        )
      
      total_row <- total_row %>%
        mutate(
          `Andel af skud (%)` = 100.0
        )
      
      # Kombiner total-række med resten af tabellen
      bind_rows(total_row, angle_table)
    }
    
    # Generer tabel baseret på om split er valgt
    if (input$split) {
      train_table <- create_angle_table(data_list$train) %>% mutate(Dataset = "Træning")
      test_table <- create_angle_table(data_list$test) %>% mutate(Dataset = "Test")
      bind_rows(train_table, test_table)
    } else {
      create_angle_table(data_list$all) %>% mutate(Dataset = "Alle data")
    }
  })
  
  
  
  output$distance_lines_plot <- renderPlot({
    goal_x <- 100
    goal_y <- 50
    radii <- c(7.5, 15, 20)
    colors <- c("#00296b", "#003f88", "#00509d")
    
    make_semicircle <- function(radius, center_x = goal_x, center_y = goal_y, n = 300) {
      angles <- seq(-pi/2, pi/2, length.out = n)
      data.frame(
        x = center_x - radius * cos(angles),
        y = center_y + radius * sin(angles)
      )
    }
    
    circles <- purrr::pmap_dfr(list(r = radii, col = colors), function(r, col) {
      make_semicircle(r) %>%
        mutate(group = r, color = col)
    })
    
    label_data <- circles %>%
      group_by(group, color) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(label = paste0(group, "m"))
    
    ggplot() +
      annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "white") +
      geom_point(
        data = allshotevents %>% arrange(SHOTISGOAL),
        aes(x = LOCATIONX, y = LOCATIONY, color = factor(SHOTISGOAL),
            alpha = ifelse(SHOTISGOAL == 1, 0.2, 0.8)),
        size = 3
      ) +
      geom_path(data = circles, aes(x = x, y = y, group = group, color = color), linewidth = 1) +
      geom_text(data = label_data, aes(x = x, y = y, label = label, color = color),
                vjust = -0.5, hjust = 0.8, fontface = "bold", size = 3.5) +
      scale_color_manual(
        values = c("0" = "#0D1C8A", "1" = "#FDBA21", colors),
        labels = c("0" = "Ikke mål", "1" = "Mål"),
        name = "Resultat"
      ) +
      scale_alpha_identity() +
      coord_flip(xlim = c(50, 100), ylim = c(0, 100)) +
      theme_pitch() +
      labs(
        title = "Skud og afstandscirkler i Superligaen 2023/2024",
        subtitle = "Klart størstedelen af alle skud der blev til mål, blev gjort tættere ved målet"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "italic"),
        legend.position = "top"
      )
  })
  
  output$conclusion_text <- renderText({
    get_conclusion(input$plot_choice)
  })
  
  output$shot_angle_visual <- renderPlot({
    goal_left <- c(x = 100, y = 44.285)
    goal_right <- c(x = 100, y = 55.715)
    
    vinkel_eksempler <- data.frame(
      vinkel_label = c("12°", "33°", "97°"),
      x = c(88, 86, 95),
      y = c(27, 59, 49)
    )
    
    ggplot() +
      annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "gray90") +
      geom_segment(data = vinkel_eksempler,
                   aes(x = x, y = y, xend = goal_left["x"], yend = goal_left["y"], color = vinkel_label),
                   arrow = arrow(length = unit(0.15, "cm"))) +
      geom_segment(data = vinkel_eksempler,
                   aes(x = x, y = y, xend = goal_right["x"], yend = goal_right["y"], color = vinkel_label),
                   arrow = arrow(length = unit(0.15, "cm"))) +
      geom_point(data = vinkel_eksempler, aes(x = x, y = y, color = vinkel_label), size = 4) +
      coord_flip(xlim = c(80, 105), ylim = c(0, 100)) +
      theme_pitch()
  })
  
  output$body_location_plot <- renderPlot({
    plot_data <- allshotevents %>%
      mutate(body_group = case_when(
        SHOTBODYPART == "head_or_other" ~ "Hoved/andet",
        SHOTBODYPART %in% c("left_foot", "right_foot") ~ "Ben"
      ))
    
    ggplot(plot_data, aes(x = LOCATIONX, y = LOCATIONY, fill = body_group)) +
      annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "white") +
      geom_bin2d(binwidth = c(2, 2), alpha = 0.6) +
      scale_fill_manual(
        values = c("Hoved/andet" = "#FDBA21", "Ben" = "#0D1C8A"),
        name = "Kropsdel"
      ) +
      coord_flip(xlim = c(50, 100), ylim = c(0, 100)) +
      theme_pitch() +
      labs(title = "Skudpositioner opdelt i ben og hoved/andet") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  output$shot_distance_table <- renderTable({
    # Vælg data baseret på input$split
    data_list <- if (input$split) {
      list(train = train_data, test = test_data)
    } else {
      list(all = allshotevents)
    }
    
    # Funktion til at lave tabellen for et enkelt datasæt
    create_distance_table <- function(df) {
      # Definér afstandsintervaller
      breaks <- c(0, 10, 20, Inf)
      labels <- c("Kort (0-10m)", "Middel (10-20m)", "Lang (20m+)")
      
      # Beregn antal skud pr. afstandsgruppe og mål/ikke mål
      summary_data <- df %>%
        mutate(
          afstandsgruppe = cut(shot_distance, breaks = breaks, labels = labels, right = FALSE),
          SHOTISGOAL = factor(SHOTISGOAL, levels = c(0, 1), labels = c("Ikke mål", "Mål"))
        ) %>%
        filter(!is.na(afstandsgruppe)) %>%
        group_by(afstandsgruppe, SHOTISGOAL) %>%
        summarise(
          Antal_skud = n(),
          .groups = "drop"
        )
      
      # Beregn standardafvigelse pr. afstandsgruppe
      sd_data <- df %>%
        mutate(
          afstandsgruppe = cut(shot_distance, breaks = breaks, labels = labels, right = FALSE)
        ) %>%
        filter(!is.na(afstandsgruppe)) %>%
        group_by(afstandsgruppe) %>%
        summarise(
          SD_afstand = round(sd(shot_distance, na.rm = TRUE), 1),
          .groups = "drop"
        )
      
      # Lav tabel med totaler og mål-rate
      distance_table <- summary_data %>%
        pivot_wider(
          names_from = SHOTISGOAL,
          values_from = Antal_skud,
          values_fill = 0,
          names_glue = "{SHOTISGOAL}_Antal_skud"
        ) %>%
        mutate(
          Total = `Ikke mål_Antal_skud` + Mål_Antal_skud,
          `Mål-rate (%)` = round(100 * Mål_Antal_skud / Total, 1)
        ) %>%
        left_join(sd_data, by = "afstandsgruppe") %>%
        select(
          Afstandsgruppe = afstandsgruppe,
          Total,
          `Mål-rate (%)`,
          `Standardafvigelse` = SD_afstand
        )
      
      # Beregn total-række
      total_row <- summary_data %>%
        group_by(SHOTISGOAL) %>%
        summarise(
          Antal_skud = sum(Antal_skud),
          .groups = "drop"
        ) %>%
        pivot_wider(
          names_from = SHOTISGOAL,
          values_from = Antal_skud,
          values_fill = 0,
          names_glue = "{SHOTISGOAL}_Antal_skud"
        ) %>%
        mutate(
          Afstandsgruppe = "Total",
          Total = `Ikke mål_Antal_skud` + Mål_Antal_skud,
          `Mål-rate (%)` = round(100 * Mål_Antal_skud / Total, 1),
          `Standardafvigelse` = round(sd(df$shot_distance, na.rm = TRUE), 1)
        ) %>%
        select(
          Afstandsgruppe,
          Total,
          `Mål-rate (%)`,
          `Standardafvigelse`
        )
      
      # Beregn andel af samlede skud
      overall_total <- sum(distance_table$Total)
      distance_table <- distance_table %>%
        mutate(
          `Andel af skud (%)` = round(100 * Total / overall_total, 1)
        )
      
      total_row <- total_row %>%
        mutate(
          `Andel af skud (%)` = 100.0
        )
      
      # Kombiner total-række med resten af tabellen
      bind_rows(total_row, distance_table)
    }
    
    # Generer tabel baseret på om split er valgt
    if (input$split) {
      train_table <- create_distance_table(data_list$train) %>% mutate(Dataset = "Træning")
      test_table <- create_distance_table(data_list$test) %>% mutate(Dataset = "Test")
      bind_rows(train_table, test_table)
    } else {
      create_distance_table(data_list$all) %>% mutate(Dataset = "Alle data")
    }
  })
  
  output$body_part_table <- renderTable({
    # Vælg data baseret på input$split
    data_list <- if (input$split) {
      list(train = train_data, test = test_data)
    } else {
      list(all = allshotevents)
    }
    
    # Funktion til at lave tabellen for et enkelt datasæt
    create_body_part_table <- function(df) {
      # Beregn antal skud pr. kropsdel og mål/ikke mål
      summary_data <- df %>%
        mutate(
          kropsdel = factor(SHOTBODYPART, levels = c("head_or_other", "left_foot", "right_foot"),
                            labels = c("Hoved/andet", "Venstre fod", "Højre fod")),
          SHOTISGOAL = factor(SHOTISGOAL, levels = c(0, 1), labels = c("Ikke mål", "Mål"))
        ) %>%
        filter(!is.na(kropsdel)) %>%
        group_by(kropsdel, SHOTISGOAL) %>%
        summarise(
          Antal_skud = n(),
          .groups = "drop"
        )
      
      # Beregn standardafvigelse for afstand pr. kropsdel
      sd_data <- df %>%
        mutate(
          kropsdel = factor(SHOTBODYPART, levels = c("head_or_other", "left_foot", "right_foot"),
                            labels = c("Hoved/andet", "Venstre fod", "Højre fod"))
        ) %>%
        filter(!is.na(kropsdel)) %>%
        group_by(kropsdel) %>%
        summarise(
          SD_afstand = round(sd(shot_distance, na.rm = TRUE), 1),
          .groups = "drop"
        )
      
      # Lav tabel med totaler og mål-rate
      body_table <- summary_data %>%
        pivot_wider(
          names_from = SHOTISGOAL,
          values_from = Antal_skud,
          values_fill = 0,
          names_glue = "{SHOTISGOAL}_Antal_skud"
        ) %>%
        mutate(
          Total = `Ikke mål_Antal_skud` + Mål_Antal_skud,
          `Mål-rate (%)` = round(100 * Mål_Antal_skud / Total, 1)
        ) %>%
        left_join(sd_data, by = "kropsdel") %>%
        select(
          Kropsdel = kropsdel,
          Total,
          `Mål-rate (%)`,
          `Standardafvigelse` = SD_afstand
        )
      
      # Beregn total-række
      total_row <- summary_data %>%
        group_by(SHOTISGOAL) %>%
        summarise(
          Antal_skud = sum(Antal_skud),
          .groups = "drop"
        ) %>%
        pivot_wider(
          names_from = SHOTISGOAL,
          values_from = Antal_skud,
          values_fill = 0,
          names_glue = "{SHOTISGOAL}_Antal_skud"
        ) %>%
        mutate(
          Kropsdel = "Total",
          Total = `Ikke mål_Antal_skud` + Mål_Antal_skud,
          `Mål-rate (%)` = round(100 * Mål_Antal_skud / Total, 1),
          `Standardafvigelse` = round(sd(df$shot_distance, na.rm = TRUE), 1)
        ) %>%
        select(
          Kropsdel,
          Total,
          `Mål-rate (%)`,
          `Standardafvigelse`
        )
      
      # Beregn andel af samlede skud
      overall_total <- sum(body_table$Total)
      body_table <- body_table %>%
        mutate(
          `Andel af skud (%)` = round(100 * Total / overall_total, 1)
        )
      
      total_row <- total_row %>%
        mutate(
          `Andel af skud (%)` = 100.0
        )
      
      # Kombiner total-række med resten af tabellen
      bind_rows(total_row, body_table)
    }
    
    # Generer tabel baseret på om split er valgt
    if (input$split) {
      train_table <- create_body_part_table(data_list$train) %>% mutate(Dataset = "Træning")
      test_table <- create_body_part_table(data_list$test) %>% mutate(Dataset = "Test")
      bind_rows(train_table, test_table)
    } else {
      create_body_part_table(data_list$all) %>% mutate(Dataset = "Alle data")
    }
  })
  
  output$team_ranking_summary_ui <- renderUI({
    data_list <- if (input$split) {
      list(train = train_data, test = test_data)
    } else {
      list(all = allshotevents)
    }
    
    create_html_table <- function(df, label) {
      kamp_data <- df %>%
        group_by(TEAMNAME.x, MATCH_WYID.x, IMAGEDATAURL, Team_Ranking) %>%
        summarise(
          skud = n(),
          mål = sum(SHOTISGOAL == 1),
          .groups = "drop"
        )
      
      team_summary <- kamp_data %>%
        group_by(TEAMNAME.x, IMAGEDATAURL, Team_Ranking) %>%
        summarise(
          antal_kampe = n(),
          total_skud = sum(skud),
          total_mål = sum(mål),
          gennemsnit_skud = total_skud / antal_kampe,
          målrate = ifelse(total_skud > 0, 100 * total_mål / total_skud, NA),
          sd_skud = ifelse(n() > 1, sd(skud), NA_real_),
          .groups = "drop"
        ) %>%
        mutate(dataset = label) %>%
        arrange(Team_Ranking)
      
      apply(team_summary, 1, function(row) {
        logo <- row[["IMAGEDATAURL"]]
        navn <- row[["TEAMNAME.x"]]
        kampe <- as.integer(row[["antal_kampe"]])
        gns <- sprintf("%.2f", as.numeric(row[["gennemsnit_skud"]]))
        målrate <- if (is.na(row[["målrate"]])) "-" else sprintf("%.1f%%", as.numeric(row[["målrate"]]))
        sd <- if (is.na(row[["sd_skud"]])) "-" else sprintf("%.2f", as.numeric(row[["sd_skud"]]))
        dataset <- row[["dataset"]]
        
        sprintf(
          "<tr>
        <td><img src='%s' height='30px' style='margin-right:10px;'> %s</td>
        <td>%d</td>
        <td>%s</td>
        <td>%s</td>
        <td>%s</td>
        <td>%s</td>
      </tr>",
          logo, navn, kampe, gns, målrate, sd, dataset
        )
      }) %>% paste(collapse = "\n")
    }
    
    rows_html <- if (input$split) {
      paste0(
        create_html_table(data_list$train, "Træning"),
        create_html_table(data_list$test, "Test")
      )
    } else {
      create_html_table(data_list$all, "Alle data")
    }
    
    HTML(sprintf("
  <table style='width:100%%; font-size:14px;'>
    <thead>
      <tr>
        <th>Hold</th>
        <th>Antal kampe</th>
        <th>Gns. skud pr. kamp</th>
        <th>Mål-rate</th>
        <th>Std. afvigelse</th>
        <th>Datasæt</th>
      </tr>
    </thead>
    <tbody>
      %s
    </tbody>
  </table>", rows_html))
  })
  
  

  
  output$rating_table <- renderTable({
    data_list <- if (input$split) {
      list(train = train_data, test = test_data)
    } else {
      list(all = allshotevents)
    }
    
    create_rating_table <- function(df, label) {
      df_long <- df %>%
        pivot_longer(cols = c(overall, potential), names_to = "Ratingtype", values_to = "rating") %>%
        filter(!is.na(rating))
      
      breaks <- seq(50, 100, by = 10)  # Op til 5 bins: 50–60, 60–70, ...
      
      binned <- df_long %>%
        mutate(
          Ratingtype = recode(Ratingtype,
                              "overall" = "Overall-rating",
                              "potential" = "Potential-rating"),
          bin = cut(rating, breaks = breaks, right = FALSE)
        ) %>%
        filter(!is.na(bin)) %>%
        group_by(Ratingtype, bin) %>%
        summarise(
          Mål = sum(SHOTISGOAL == 1, na.rm = TRUE),
          Total = n(),
          `Gns. rating` = round(mean(rating, na.rm = TRUE), 1),
          `Standardafvigelse` = round(sd(rating, na.rm = TRUE), 1),
          .groups = "drop"
        ) %>%
        mutate(
          `Mål-rate (%)` = round(100 * Mål / Total, 1),
          Datasæt = label
        ) %>%
        select(
          Ratingtype,
          `Rating bin` = bin,
          `Mål-rate (%)`,
          `Standardafvigelse`,
          `Gns. rating`,
          Datasæt
        )
      
      binned
    }
    
    if (input$split) {
      bind_rows(
        create_rating_table(data_list$train, "Træning"),
        create_rating_table(data_list$test, "Test")
      )
    } else {
      create_rating_table(data_list$all, "Alle data")
    }
  })
  
  
  
  
  
  output$possession_events_table <- renderTable({
    data_list <- if (input$split) {
      list(train = train_data, test = test_data)
    } else {
      list(all = allshotevents)
    }
    
    create_possession_table <- function(df, label) {
      df <- df %>%
        filter(!is.na(POSSESSIONEVENTSNUMBER)) %>%
        mutate(
          bin = cut(
            POSSESSIONEVENTSNUMBER,
            breaks = c(0, 15, 30, Inf),
            labels = c("0–15", "15–30", "30+"),
            right = FALSE
          )
        )
      
      bin_summary <- df %>%
        group_by(bin) %>%
        summarise(
          Antal = n(),
          Mål = sum(SHOTISGOAL == 1, na.rm = TRUE),
          `Gns. events` = round(mean(POSSESSIONEVENTSNUMBER, na.rm = TRUE), 2),
          `Standardafvigelse` = round(sd(POSSESSIONEVENTSNUMBER, na.rm = TRUE), 2),
          .groups = "drop"
        ) %>%
        mutate(
          `Mål-rate (%)` = round(100 * Mål / Antal, 1),
          Datasæt = label
        ) %>%
        select(
          `Possession-længde` = bin,
          Antal,
          `Mål-rate (%)`,
          `Standardafvigelse`,
          `Gns. events`,
          Datasæt
        )
      
      total_row <- df %>%
        summarise(
          Antal = n(),
          Mål = sum(SHOTISGOAL == 1, na.rm = TRUE),
          `Gns. events` = round(mean(POSSESSIONEVENTSNUMBER, na.rm = TRUE), 2),
          `Standardafvigelse` = round(sd(POSSESSIONEVENTSNUMBER, na.rm = TRUE), 2)
        ) %>%
        mutate(
          `Possession-længde` = "Total",
          `Mål-rate (%)` = round(100 * Mål / Antal, 1),
          Datasæt = label
        ) %>%
        select(
          `Possession-længde`,
          Antal,
          `Mål-rate (%)`,
          `Standardafvigelse`,
          `Gns. events`,
          Datasæt
        )
      
      bind_rows(total_row, bin_summary)
    }
    
    if (input$split) {
      bind_rows(
        create_possession_table(data_list$train, "Træning"),
        create_possession_table(data_list$test, "Test")
      )
    } else {
      create_possession_table(data_list$all, "Alle data")
    }
  })
  
  
  
  output$possession_index_table <- renderTable({
    data_list <- if (input$split) {
      list(train = train_data, test = test_data)
    } else {
      list(all = allshotevents)
    }
    
    create_index_table <- function(df, label) {
      df <- df %>%
        filter(!is.na(POSSESSIONEVENTINDEX)) %>%
        mutate(
          bin = cut(
            POSSESSIONEVENTINDEX,
            breaks = c(0, 5, 10, 15, Inf),
            labels = c("0–4", "5–9", "10–14", "15+"),
            right = FALSE
          )
        )
      
      bin_summary <- df %>%
        group_by(bin) %>%
        summarise(
          Antal = n(),
          Mål = sum(SHOTISGOAL == 1, na.rm = TRUE),
          `Gns. index` = round(mean(POSSESSIONEVENTINDEX, na.rm = TRUE), 1),
          `Standardafvigelse` = round(sd(POSSESSIONEVENTINDEX, na.rm = TRUE), 2),
          .groups = "drop"
        ) %>%
        mutate(
          `Mål-rate (%)` = round(100 * Mål / Antal, 1),
          Datasæt = label
        ) %>%
        select(
          `Possession Index` = bin,
          Antal,
          `Mål-rate (%)`,
          `Standardafvigelse`,
          `Gns. index`,
          Datasæt
        )
      
      total_row <- df %>%
        summarise(
          Antal = n(),
          Mål = sum(SHOTISGOAL == 1, na.rm = TRUE),
          `Gns. index` = round(mean(POSSESSIONEVENTINDEX, na.rm = TRUE), 1),
          `Standardafvigelse` = round(sd(POSSESSIONEVENTINDEX, na.rm = TRUE), 2)
        ) %>%
        mutate(
          `Possession Index` = "Total",
          `Mål-rate (%)` = round(100 * Mål / Antal, 1),
          Datasæt = label
        ) %>%
        select(
          `Possession Index`,
          Antal,
          `Mål-rate (%)`,
          `Standardafvigelse`,
          `Gns. index`,
          Datasæt
        )
      
      bind_rows(total_row, bin_summary)
    }
    
    if (input$split) {
      bind_rows(
        create_index_table(data_list$train, "Træning"),
        create_index_table(data_list$test, "Test")
      )
    } else {
      create_index_table(data_list$all, "Alle data")
    }
  })
  
  
  
  output$duration_summary <- renderTable({
    data_list <- if (input$split) {
      list(train = train_data, test = test_data)
    } else {
      list(all = allshotevents)
    }
    
    create_duration_table <- function(df, label) {
      df <- df %>%
        filter(!is.na(POSSESSIONDURATION), !is.na(SHOTISGOAL)) %>%
        mutate(
          duration_bin = cut(POSSESSIONDURATION,
                             breaks = c(0, 10, 20, 45, Inf),
                             labels = c("Short (0–10s)", "Medium (10–20s)", "Long (20–45s)", "Very long (45s+)"),
                             right = FALSE)
        )
      
      summary_data <- df %>%
        group_by(duration_bin) %>%
        summarise(
          Antal = n(),
          `Mål-rate (%)` = round(100 * mean(SHOTISGOAL == 1), 1),
          `Gennemsnitlig varighed (sek.)` = round(mean(POSSESSIONDURATION, na.rm = TRUE), 1),
          `Standardafvigelse (sek.)` = round(sd(POSSESSIONDURATION, na.rm = TRUE), 1),
          Datasæt = label,
          .groups = "drop"
        )
      
      total_row <- df %>%
        summarise(
          duration_bin = "Total",
          Antal = n(),
          `Mål-rate (%)` = round(100 * mean(SHOTISGOAL == 1), 1),
          `Gennemsnitlig varighed (sek.)` = round(mean(POSSESSIONDURATION, na.rm = TRUE), 1),
          `Standardafvigelse (sek.)` = round(sd(POSSESSIONDURATION, na.rm = TRUE), 1),
          Datasæt = label
        )
      
      bind_rows(total_row, summary_data)
    }
    
    if (input$split) {
      train_table <- create_duration_table(data_list$train, "Træning")
      test_table <- create_duration_table(data_list$test, "Test")
      bind_rows(train_table, test_table)
    } else {
      create_duration_table(data_list$all, "Alle data")
    }
  })
  
}

shinyApp(ui, server)