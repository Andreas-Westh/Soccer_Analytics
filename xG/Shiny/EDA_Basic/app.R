library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggsoccer)
library(viridis)
library(purrr)
library(tidyverse)
library(ggimage)




# -- Farver til body_parts -----------------------------------------------------------
body_colors <- c(
  "head_or_other" = "#FDBA21",  # gul
  "left_foot" = "#0D1C8A",      # blå
  "right_foot" = "#FC7753"      # rødlig
)


# Plot
ggplot(allshotevents, aes(x = LOCATIONX, y = LOCATIONY)) +
  annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "white") +
  geom_point(aes(color = SHOTBODYPART), alpha = 0.25, size = 3) +
  scale_color_manual(values = body_colors) +
  coord_flip(xlim = c(50, 100), ylim = c(0, 100)) +
  theme_pitch() +
  labs(title = "Skudpositioner opdelt efter kropsdel", color = "Kropsdel") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )



# -- UI -----------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Skuddata i Superligaen"),
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
      checkboxInput("avg_per_game", "Vis gennemsnit pr. kamp", value = FALSE)
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "shortcut icon", href = "favicon.ico")
    ),
    tabItems(
      # Intro-tab
      tabItem(tabName = "intro",
              h2("Velkommen til skuddata-dashboardet"),
              p("Denne applikation giver dig mulighed for at udforske skuddata fra Superligens 2023/2024 sæson."),
              tags$ul(
                tags$li("Brug menuen i venstre side til at vælge en variabel."),
                tags$li("Tjek 'Vis opdelt på træning/test' for at sammenligne splits."),
                tags$li("Hver variabel vises som et plot og med en tilhørende konklusion.")
              ),
              br(),
              h3("Hvad viser de forskellige variabler?"),
              tags$ul(
                tags$li(strong("Skudposition:"), " Hvor på banen afslutningerne bliver taget fra."),
                tags$li(strong("Skudvinkel:"), " I hvilken vinkel spilleren skyder mod målet."),
                tags$li(strong("Afstand til mål:"), " Hvor langt der er fra spilleren til målet ved afslutning."),
                tags$li(strong("Kropsdel:"), " Hvilken kropsdel spilleren bruger til at afslutte."),
                tags$li(strong("Team Ranking:"), " Holdenes placering i ligaen."),
                tags$li(strong("Spiller-rating:"), " Den individuelle spiller-rating fra FIFA-data."),
                tags$li(strong("Antal events i possession:"), " Hvor mange aktioner der er i et angreb før afslutning."),
                tags$li(strong("Index for possession:"), " Hvornår i kampen possessionen (angrebet) finder sted."),
                tags$li(strong("Varighed af possession:"), " Hvor mange sekunder possessionen varer før afslutning.")
              ),
              br(),
              p("God fornøjelse!")
      )
      ,
      
      # Din eksisterende visualisering
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
                  box(title = "Afstandslinjer på banen", width = 12, solidHeader = TRUE, status = "warning",
                      plotOutput("distance_lines_plot", height = "300px"))
                )
              ),
              fluidRow(
                conditionalPanel(
                  condition = "input.plot_choice == 'shot_angle'",
                  box(title = "Visuel forklaring på skudvinkler", width = 12, solidHeader = TRUE, status = "warning",
                      plotOutput("angle_visual_plot", height = "400px"))
                )
              ),
              fluidRow(
                conditionalPanel(
                  condition = "input.plot_choice == 'body_part'",
                  
                  # Først tabellen
                  box(title = "Afstande fordelt på kropsdel", width = 12, solidHeader = TRUE, status = "warning",
                      tableOutput("body_distance_table")),
                  
                  # Så plot
                  box(title = "Skudpositioner fordelt på kropsdel", width = 12, solidHeader = TRUE, status = "warning",
                      plotOutput("body_location_plot", height = "350px"))
                )
              ),
              fluidRow(
                conditionalPanel(
                  condition = "input.plot_choice == 'team_ranking'",
                  box(title = "Oversigt over holdenes skudstatistik", width = 12, solidHeader = TRUE, status = "warning",
                      htmlOutput("team_ranking_summary_ui"))
                )
              ),
              fluidRow(
                conditionalPanel(
                  condition = "input.plot_choice == 'player_rating'",
                  
                  # Tabel med statistik
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
                  
                  # -- Tabel-boks til Wyscout-opdeling --
                  box(
                    title = "Opsummerende statistik opdelt efter Wyscout-varighedskategorier",
                    width = 12,
                    solidHeader = TRUE,
                    status = "warning",
                    tableOutput("duration_summary")
                  )
                )
              )
              
              
              

      ))))


# -- Konklusionstekster -------------------------------------------
get_conclusion <- function(var) {
  switch(var,
         "location_points" = "Afslutninger i Superligaen bliver oftest taget uden for feltet. Hele 67 % af alle skud stammer fra områder uden for det store felt, mens 29 % sker inde i selve feltet, og kun 4 % tages fra det lille målfelt. Det viser, at mange hold enten vælger – eller bliver tvunget til – at afslutte fra distancen, frem for at spille sig helt tæt på mål. Fordelingen peger på en generel tendens i spillet, hvor afslutninger ofte kommer fra positioner, hvor sandsynligheden for scoring er lavere.",
         "shot_angle" = "Vinklen på skuddet er helt afgørende for, hvor farlig en afslutning er. Jo større vinkel spilleren har mod målet, jo lettere er det at placere bolden udenom målmanden. Gennemsnittet ligger omkring 34 grader, men med stor variation – hvilket passer godt med, at spillere skyder fra både åbne og meget skæve vinkler. De skarpeste vinkler opstår typisk ude ved siden af feltet, mens de åbne vinkler ofte ses tættere på mål og midt i feltet. Det underbygger, at skudvinkel er en vigtig variabel i en xG-model, da den siger noget om, hvor 'åben' chancen reelt er.",
         "shot_distance" = "Afstanden til målet er en af de mest oplagte og centrale forklarende variable, når man forsøger at beskrive kvaliteten af en afslutning. Vores data viser, at den gennemsnitlige afslutning bliver taget fra omkring 19 meter, men med stor variation – nogle afslutninger sker helt tæt under mål, mens andre bliver fyret af op mod 60 meter ude fra banen.

Histogrammet afslører, at skuddene især samler sig i intervallet 10–25 meter, mens skud udenfor boksen og helt tæt under mål er mere sjældne. De afstandscirkler vi har lagt ind på banen, viser tydeligt hvordan disse afstande fordeler sig visuelt – og understreger, at det typisk er i det centrale område foran målet, at vi ser flest afslutninger.

Afstand er dermed en naturlig og vigtig kandidat til enhver xG-model, netop fordi den på enkel vis indrammer noget af det, der gør en afslutning mere eller mindre fordelagtig.",
         "body_part" = "Langt de fleste skud tages med fødderne – især højre fod bliver brugt oftest. Skud med hovedet (eller andre dele af kroppen) sker typisk tættere på mål, hvilket giver god mening, da de ofte kommer fra dødbolde som hjørnespark eller frispark. Fodskud sker derimod oftere i åbent spil og fra længere afstande. Derfor giver det god mening at bruge SHOTBODYPART som en forklarende variabel i en xG-model, da den fanger forskelle i både situation og afstand.",
         "team_ranking" = "Holdets placering i ligaen kan virke som en oplagt indikator for deres offensive styrke – men vores data viser, at sammenhængen mellem rangering og antal skud ikke er særlig tydelig. Der er eksempler på både tophold og midterhold med mange afslutninger, ligesom flere lavere placerede hold har relativt få. Det tyder på, at ligaposition alene ikke forklarer, hvor ofte et hold afslutter.",
         "player_rating" = "FIFA-ratingen viser, at de spillere som står bag afslutningerne generelt ligger på et overall-niveau omkring 67, med en potential-rating omkring 73. Det tyder på, at flere spillere i datagrundlaget vurderes at have mulighed for udvikling, men ikke nødvendigvis har nået et højt niveau endnu. Fordelingen er forholdsvis smal, og der ses ikke umiddelbart nogle tydelige mønstre i forhold til bestemte ratingintervaller. Det er dog vigtigt at huske, at denne variabel kun dækker spillere, der allerede har afsluttet – og dermed udelader alle spillere, der ikke har taget skud. Det gør det sværere at vurdere rating som en stærk skillelinje i forhold til afslutningsadfærd.",
         "possession_events" = "Antallet af 'events' i possessionen før et skud svinger markant – nogle skud kommer efter lange, tålmodige opspil med over 40 handlinger, mens andre opstår efter bare 1–2 events. Det lave gennemsnit på 9,9 dækker derfor over stor variation. Det passer godt med, at nogle afslutninger opstår i strukturerede angreb, mens andre er resultatet af hurtige omstillinger eller tilfældige situationer. Alt i alt kan variablen eventuelt give et indblik i, hvor “kontrolleret” angrebet har været – og dermed i konteksten omkring skuddet.",
         "possession_index" = "Possession index fortæller os, hvornår i kampen afslutningen sker – altså om det fx er kampens 3., 10. eller 40. possession. Det giver et billede af kampens rytme og hvor hurtigt chancer opstår. Her ser vi, at afslutninger oftest kommer tidligt i kampens forløb, med et gennemsnitligt index på 8,9 og en median på 7. Det tyder på, at de fleste skud sker i de første 10 possession-forløb i kampen, hvilket giver mening, da det ofte er i denne periode, at hold stadig presser på og forsøger at tage kontrol over kampen.
Alt i alt en variabel, der hjælper os med at forstå den timing, som afslutninger indgår i – både i forhold til kampens udvikling og spillernes taktiske muligheder.",
         "possession_duration" = "Størstedelen af afslutningerne sker efter korte possessions – faktisk falder næsten halvdelen i kategorien ‘Short’ (0–10 sek.). Det peger på, at mange skud opstår hurtigt, fx gennem højt pres, omstillinger eller spontane muligheder.

Samtidig er det interessant, at der også forekommer skud efter meget lange possessions (45+ sek.), hvilket indikerer mere strukturerede og tålmodige angreb. Det afspejler to forskellige spilstile: hurtige, direkte angreb og kontrolleret opspil.

Den gennemsnitlige varighed før en afslutning er cirka 17 sekunder, men fordelingen er tydeligt højreskæv. Det gør possession-varighed til en spændende kontekstuel variabel, som siger noget om tempo og opbygning bag chancen – men det er usikkert, hvor meget forklaringskraft den i sig selv har.",
         "Ukendt valg"
  )
}

# -- Plotfunktion -------------------------------------------------
make_plot <- function(data, var, avg_on) {
  switch(var,
         
         # Punktplot
         "location_points" = {
           ggplot(data, aes(x = LOCATIONX, y = LOCATIONY)) +
             annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "white") +
             stat_density_2d(
               aes(fill = after_stat(density)),
               geom = "raster",
               contour = FALSE,
               alpha = 0.6,
               adjust = 0.8
             ) +
             scale_fill_viridis_c(option = "C") +
             theme_pitch() +
             labs(
               title = "Hvor på banen bliver der afsluttet fra?",
               subtitle = "Mørkere farver = højere koncentration af skud",
               fill = NULL
             ) +
             theme(
               plot.title = element_text(hjust = 0.5, face = "bold"),
               plot.subtitle = element_text(hjust = 0.5),
               legend.position = "right"
             )
         }
         ,
         
         
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
         
         # shot angle
         "shot_angle" = {
           if (avg_on) {
             data %>%
               group_by(MATCH_WYID.x) %>%
               summarise(mean_angle = mean(shot_angle, na.rm = TRUE)) %>%
               ggplot(aes(x = mean_angle)) +
               geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
               scale_x_continuous(breaks = seq(0, 180, 30)) +
               labs(
                 x = "Gns. skudvinkel pr. kamp (grader)",
                 y = "Antal kampe",
                 title = "Hvor skarpe vinkler skyder holdene fra pr. kamp?"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
             
           } else {
             mean_angle <- mean(data$shot_angle, na.rm = TRUE)
             
             ggplot(data, aes(x = shot_angle)) +
               geom_histogram(
                 bins = 30,
                 fill = "#0D1C8A",
                 color = "white",
                 alpha = 0.9
               ) +
               geom_vline(
                 xintercept = mean_angle,
                 color = "#FDBA21",
                 linewidth = 1.3
               ) +
               annotate("text",
                        x = mean_angle + 2,
                        y = Inf,
                        label = paste0("Gennemsnitlig vinkel: ", round(mean_angle, 1), "°"),
                        vjust = 2,
                        hjust = -0.1,
                        color = "#FDBA21",
                        fontface = "bold",
                        size = 3.5) +
               labs(
                 x = "Vinkel mod mål (grader)",
                 y = "Antal skud",
                 title = "Fordeling af skudvinkler"
               ) +
               theme_minimal(base_size = 13) +
               theme(
                 plot.title = element_text(hjust = 0.5, face = "bold")
               )
           }
         },
         
         # Shot distance
         "shot_distance" = {
           if (avg_on) {
             kamp_means <- data %>%
               group_by(MATCH_WYID.x) %>%
               summarise(mean_dist = mean(shot_distance, na.rm = TRUE))
             
             total_avg <- mean(kamp_means$mean_dist, na.rm = TRUE)
             
             kamp_means %>%
               mutate(distance_group = cut(mean_dist,
                                           breaks = c(0,10,20,30,40,Inf),
                                           labels = c("0–10m", "10–20m", "20–30m", "30–40m", "40m+"),
                                           right = FALSE)) %>%
               ggplot(aes(x = mean_dist, fill = distance_group)) +
               geom_histogram(binwidth = 2, color = "white") +
               geom_vline(xintercept = total_avg, color = "#FDBA21", linewidth = 1.2) +
               annotate("text", x = total_avg + 1.5, y = Inf,
                        label = paste0("Gns.: ", round(total_avg, 1), "m"),
                        vjust = 2, hjust = 0, fontface = "bold", size = 3.5, color = "#FDBA21") +
               scale_fill_manual(values = c("#00296b", "#003f88", "#00509d", "#fdc500", "#ffd500")) +
               scale_x_continuous(breaks = seq(0, 70, 10)) +
               labs(
                 x = "Gns. afstand til mål pr. kamp (meter)",
                 y = "Antal kampe",
                 fill = "Afstand",
                 title = "Hvor langt fra målet afslutter holdene pr. kamp?"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
             
           } else {
             total_avg <- mean(data$shot_distance, na.rm = TRUE)
             
             data %>%
               mutate(distance_group = cut(shot_distance,
                                           breaks = c(0,10,20,30,40,Inf),
                                           labels = c("0–10m", "10–20m", "20–30m", "30–40m", "40m+"),
                                           right = FALSE)) %>%
               ggplot(aes(x = shot_distance, fill = distance_group)) +
               geom_histogram(binwidth = 2, color = "white") +
               geom_vline(xintercept = total_avg, color = "#FDBA21", linewidth = 1.2) +
               annotate("text", x = total_avg + 1.5, y = Inf,
                        label = paste0("Gns.: ", round(total_avg, 1), "m"),
                        vjust = 2, hjust = 0, fontface = "bold", size = 3.5, color = "#FDBA21") +
               scale_fill_manual(values = c("#00296b", "#003f88", "#00509d", "#fdc500", "#ffd500")) +
               scale_x_continuous(breaks = seq(0, 70, 10)) +
               labs(
                 x = "Afstand til mål (meter)",
                 y = "Antal skud",
                 fill = "Afstand",
                 title = "Hvor langt fra målet afslutter spillerne?"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
           }
         },
         
         
         
         
         # Body part
         "body_part" = {
           if (avg_on) {
             data %>%
               group_by(MATCH_WYID.x, SHOTBODYPART) %>%
               summarise(count = n(), .groups = "drop") %>%
               group_by(SHOTBODYPART) %>%
               summarise(avg = mean(count)) %>%
               ggplot(aes(x = SHOTBODYPART, y = avg, fill = SHOTBODYPART)) +
               geom_col() +
               scale_fill_manual(values = body_colors) +
               labs(x = "Kropsdel", y = "Gennemsnit pr. kamp", title = "Skud pr. kropsdel (gennemsnit)") +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
           } else {
             ggplot(data, aes(x = SHOTBODYPART, fill = SHOTBODYPART)) +
               geom_bar() +
               scale_fill_manual(values = body_colors) +
               labs(x = "Kropsdel", y = "Antal skud", title = "Fordeling af skud pr. kropsdel") +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
           }
         },
         
         # Team ranking (med logo og labels)
         "team_ranking" = {
           if (avg_on) {
             data %>%
               group_by(TEAMNAME.x, Team_Ranking, IMAGEDATAURL, MATCH_WYID.x) %>%
               summarise(shots = n(), .groups = "drop") %>%
               group_by(TEAMNAME.x, Team_Ranking, IMAGEDATAURL) %>%
               summarise(avg_shots = mean(shots), .groups = "drop") %>%
               mutate(label = paste0(TEAMNAME.x, " (#", Team_Ranking, ")")) %>%
               arrange(Team_Ranking) %>%
               ggplot(aes(x = reorder(label, -Team_Ranking), y = avg_shots)) +
               geom_col(fill = "steelblue", color = "black") +
               geom_image(aes(image = IMAGEDATAURL), size = 0.06, asp = 1.2) +
               coord_flip() +
               labs(x = NULL, y = "Gennemsnit pr. kamp", title = "Skud pr. hold pr. kamp") +
               theme_minimal(base_size = 16)
           } else {
             data %>%
               group_by(TEAMNAME.x, Team_Ranking, IMAGEDATAURL) %>%
               summarise(total = n(), .groups = "drop") %>%
               mutate(label = paste0(TEAMNAME.x, " (#", Team_Ranking, ")")) %>%
               arrange(Team_Ranking) %>%
               ggplot(aes(x = reorder(label, -Team_Ranking), y = total)) +
               geom_col(fill = "lightgray", color = "black") +
               geom_image(aes(image = IMAGEDATAURL), size = 0.06, asp = 1.2) +
               coord_flip() +
               labs(x = NULL, y = "Antal skud", title = "Antal skud pr. hold") +
               theme_minimal(base_size = 16)
           }
         },
         
         "player_rating" = {
           if (avg_on) {
             data_long <- data %>%
               group_by(MATCH_WYID.x) %>%
               summarise(mean_overall = mean(overall, na.rm = TRUE),
                         mean_potential = mean(potential, na.rm = TRUE)) %>%
               pivot_longer(cols = everything(), names_to = "type", values_to = "rating")
           } else {
             data_long <- data %>%
               select(overall, potential) %>%
               pivot_longer(cols = everything(), names_to = "type", values_to = "rating")
           }
           
           mean_overall <- mean(data$overall, na.rm = TRUE)
           mean_potential <- mean(data$potential, na.rm = TRUE)
           
           ggplot(data_long, aes(x = rating, fill = type)) +
             geom_histogram(position = "dodge", alpha = 0.8, binwidth = 5, color = "white") +
             geom_vline(xintercept = mean_overall, color = "#0D1C8A", linewidth = 1, linetype = "dashed") +
             geom_vline(xintercept = mean_potential, color = "#FDBA21", linewidth = 1, linetype = "dashed") +
             annotate("text", x = mean_overall, y = Inf, label = paste0("Gns. overall: ", round(mean_overall, 1)),
                      hjust = 1.1, vjust = 4, color = "#0D1C8A", fontface = "bold") +
             annotate("text", x = mean_potential, y = Inf, label = paste0("Gns. potential: ", round(mean_potential, 1)),
                      hjust = -0, vjust = 1.8, color = "#FDBA21", fontface = "bold") +
             labs(
               title = if (avg_on) {
                 "Gns. spiller-rating pr. kamp"
               } else {
                 "Fordeling af skud baseret på spiller-rating"
               },
               subtitle = "Inkl. gennemsnitlig rating for Overall og Potential",
               x = "FIFA-rating",
               y = if (avg_on) "Gns. rating pr. kamp" else "Antal skud",
               fill = "Ratingtype"
             ) +
             scale_fill_manual(values = c("overall" = "#0D1C8A", "potential" = "#FDBA21"),
                               labels = c("Overall", "Potential")) +
             theme_minimal() +
             theme(plot.title = element_text(hjust = 0.5, face = "bold"))
         },
         
         
         
         # Possession events
         "possession_events" = {
           if (avg_on) {
             mean_value <- allshotevents %>%
               group_by(MATCH_WYID.x) %>%
               summarise(mean_events = mean(POSSESSIONEVENTSNUMBER, na.rm = TRUE)) %>%
               pull(mean_events) %>%
               mean(na.rm = TRUE)
             
             allshotevents %>%
               group_by(MATCH_WYID.x) %>%
               summarise(mean_value = mean(POSSESSIONEVENTSNUMBER, na.rm = TRUE)) %>%
               ggplot(aes(x = mean_value, fill = ..count..)) +
               geom_histogram(binwidth = 1, color = "white", alpha = 0.9) +
               scale_fill_gradient(low = "#0D1C8A", high = "#FDBA21") +
               geom_vline(xintercept = mean_value, color = "#8E05C2", linetype = "dashed", linewidth = 1.2) +
               annotate("text",
                        x = mean_value + 1,
                        y = Inf,
                        label = paste0("Gns: ", round(mean_value, 1)),
                        hjust = 0,
                        vjust = 2,
                        color = "#8E05C2",
                        fontface = "bold") +
               labs(
                 x = "Gns. antal hændelser pr. kamp",
                 y = "Antal kampe",
                 title = "Hvor mange hændelser leder op til skud pr. kamp?"
               ) +
               theme_minimal(base_size = 13) +
               theme(
                 plot.title = element_text(hjust = 0.5, face = "bold"),
                 legend.position = "none"
               ) +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
             
           } else {
             mean_value <- mean(allshotevents$POSSESSIONEVENTSNUMBER, na.rm = TRUE)
             
             ggplot(allshotevents, aes(x = POSSESSIONEVENTSNUMBER, fill = ..count..)) +
               geom_histogram(binwidth = 1, color = "white", alpha = 0.9) +
               scale_fill_gradient(low = "#0D1C8A", high = "#FDBA21") +
               geom_vline(xintercept = mean_value, color = "#8E05C2", linetype = "dashed", linewidth = 1.2) +
               annotate("text",
                        x = mean_value + 1,
                        y = Inf,
                        label = paste0("Gns: ", round(mean_value, 1)),
                        hjust = 0,
                        vjust = 2,
                        color = "#8E05C2",
                        fontface = "bold") +
               labs(
                 x = "Antal 'events' for holdet før skudet",
                 y = "Antal skud",
                 title = "Fordeling af possessionslængder før skud"
               ) +
               theme_minimal(base_size = 13) +
               theme(
                 plot.title = element_text(hjust = 0.5, face = "bold"),
                 legend.position = "none"
               ) +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
           }
         },
         

         # Possession index
         "possession_index" = {
           if (avg_on) {
             data %>%
               group_by(MATCH_WYID.x) %>%
               summarise(mean_index = mean(POSSESSIONEVENTINDEX, na.rm = TRUE)) %>%
               ggplot(aes(x = mean_index)) +
               geom_histogram(aes(fill = ..x..), binwidth = 5, color = "white") +
               scale_fill_gradient(low = "#0D1C8A", high = "#FDBA21", guide = "none") +
               geom_vline(aes(xintercept = mean(mean_index, na.rm = TRUE)),
                          color = "gray27", linetype = "dashed", linewidth = 1.2) +
               annotate("text", x = mean(data$POSSESSIONEVENTINDEX, na.rm = TRUE),
                        y = Inf,
                        label = paste0("Gns. index: ", round(mean(data$POSSESSIONEVENTINDEX, na.rm = TRUE), 1)),
                        vjust = 2, hjust = -0.1,
                        fontface = "bold", color = "gray27", size = 3.5) +
               labs(
                 x = "Gns. possession-index pr. kamp",
                 y = "Antal kampe",
                 title = "Hvornår i kampens possessions bliver der afsluttet (gennemsnit pr. kamp)?"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
             
           } else {
             ggplot(data, aes(x = POSSESSIONEVENTINDEX)) +
               geom_histogram(aes(fill = ..x..), binwidth = 5, color = "white") +
               scale_fill_gradient(low = "#0D1C8A", high = "#FDBA21", guide = "none") +
               geom_vline(aes(xintercept = mean(POSSESSIONEVENTINDEX, na.rm = TRUE)),
                          color = "gray27", linetype = "dashed", linewidth = 1.2) +
               annotate("text", x = mean(data$POSSESSIONEVENTINDEX, na.rm = TRUE),
                        y = Inf,
                        label = paste0("Gns. index: ", round(mean(data$POSSESSIONEVENTINDEX, na.rm = TRUE), 1)),
                        vjust = 2, hjust = -0.1,
                        fontface = "bold", color = "gray27", size = 3.5) +
               labs(
                 x = "Possession index i kampen",
                 y = "Antal skud",
                 title = "Hvornår i kampen bliver der afsluttet?"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"))
           }
         },
         
         
         
         # Possession duration
         "possession_duration" = {
           if (avg_on) {
             duration_data <- data %>%
               group_by(MATCH_WYID.x) %>%
               summarise(mean_duration = mean(POSSESSIONDURATION, na.rm = TRUE), .groups = "drop")
             
             avg_dur <- mean(duration_data$mean_duration, na.rm = TRUE)
             
             ggplot(duration_data, aes(x = mean_duration)) +
               geom_histogram(aes(fill = ..count..), binwidth = 2, color = "white", alpha = 0.9) +
               geom_vline(xintercept = avg_dur, linetype = "dashed", color = "#FDBA21", linewidth = 1.2) +
               annotate("text", x = avg_dur, y = Inf, label = paste0("Gns.: ", round(avg_dur, 1)), 
                        vjust = 2, hjust = -0.1, color = "#FDBA21", fontface = "bold") +
               scale_fill_gradient(low = "#0D1C8A", high = "#FDBA21", guide = "none") +
               labs(
                 title = "Fordeling af gennemsnitlig possession-varighed pr. kamp",
                 x = "Gns. varighed (sekunder)",
                 y = "Antal kampe"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(face = "bold", hjust = 0.5))
             
           } else {
             avg_dur <- mean(data$POSSESSIONDURATION, na.rm = TRUE)
             
             ggplot(data, aes(x = POSSESSIONDURATION)) +
               geom_histogram(aes(fill = ..count..), binwidth = 2, color = "white", alpha = 0.9) +
               geom_vline(xintercept = avg_dur, linetype = "dashed", color = "#FDBA21", linewidth = 1.2) +
               annotate("text", x = avg_dur, y = Inf, label = paste0("Gns.: ", round(avg_dur, 1)), 
                        vjust = 2, hjust = -0.1, color = "#FDBA21", fontface = "bold") +
               scale_fill_gradient(low = "#0D1C8A", high = "#FDBA21", guide = "none") +
               labs(
                 title = "Fordeling af possession-varighed for afslutninger",
                 x = "Varighed (sekunder)",
                 y = "Antal skud"
               ) +
               theme_minimal() +
               theme(plot.title = element_text(face = "bold", hjust = 0.5))
           }
         },
         
         
         
  )
}


# -- Server -------------------------------------------------------
server <- function(input, output, session) {
  allshotevents <- readRDS("allshotevents_Shiny_EDA_1.rds")
  train_data <- readRDS("train_data.rds")
  test_data <- readRDS("test_data.rds")
  
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
    make_plot(allshotevents, input$plot_choice, input$avg_per_game)
  })
  
  output$plot_train <- renderPlot({
    make_plot(train_data, input$plot_choice, input$avg_per_game) + ggtitle("Træningsdata")
  })
  
  output$plot_test <- renderPlot({
    make_plot(test_data, input$plot_choice, input$avg_per_game) + ggtitle("Testdata")
  })
  
  output$position_area_table <- renderTable({
    allshotevents %>%
      mutate(
        område = case_when(
          LOCATIONX > 94 & LOCATIONY >= 44.5 & LOCATIONY <= 55.5 ~ "Målfelt",
          LOCATIONX > 88 & LOCATIONY >= 35 & LOCATIONY <= 65 ~ "Store felt",
          TRUE ~ "Uden for feltet"
        )
      ) %>%
      count(område, name = "Antal skud") %>%
      mutate(
        `Andel (%)` = round(100 * `Antal skud` / sum(`Antal skud`), 1)
      ) %>%
      arrange(factor(område, levels = c("Målfelt", "Store felt", "Uden for feltet")))
  })
  

  
  # distance lines
  output$distance_lines_plot <- renderPlot({
    goal_x <- 100
    goal_y <- 50
    radii <- c(10, 20, 30, 40, 50)
    colors <- c("#00296b", "#003f88", "#00509d", "#fdc500", "#ffd500")  # De gamle farver
    
    make_semicircle <- function(radius, center_x = goal_x, center_y = goal_y, n = 300) {
      angles <- seq(-pi/2, pi/2, length.out = n)
      data.frame(
        x = center_x - radius * cos(angles),
        y = center_y + radius * sin(angles)
      )
    }
    
    # Kombiner farver og radius
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
      geom_path(data = circles, aes(x = x, y = y, group = group, color = color), linewidth = 1) +
      geom_text(data = label_data, aes(x = x, y = y, label = label, color = color),
                vjust = -0.5, hjust = 0.8, fontface = "bold", size = 3.5) +
      scale_color_identity() +  # <- NØGLEN TIL RIGTIGE FARVER
      coord_flip(xlim = c(50, 100), ylim = c(0, 100)) +
      theme_pitch() +
      labs(title = "Afstandscirkler fra mållinjen") +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none"
      )
  })
  
  
  
  
  
  output$conclusion_text <- renderText({
    get_conclusion(input$plot_choice)
  })
  
  output$angle_visual_plot <- renderPlot({
    # Målstolper
    goal_left <- c(x = 100, y = 44.285)
    goal_right <- c(x = 100, y = 55.715)
    
    vinkel_eksempler <- data.frame(
      label = c("Skarp vinkel (7°)", "Gennemsnitlig vinkel (34°)", "Åben vinkel (78°)"),
      x = c(69, 91, 93),
      y = c(91, 60, 50)
    )
    
    ggplot() +
      annotate_pitch(dimensions = pitch_wyscout, colour = "grey80", fill = "gray90") +
      geom_segment(data = vinkel_eksempler,
                   aes(x = x, y = y, xend = goal_left["x"], yend = goal_left["y"], color = label),
                   arrow = arrow(length = unit(0.15, "cm"))) +
      geom_segment(data = vinkel_eksempler,
                   aes(x = x, y = y, xend = goal_right["x"], yend = goal_right["y"], color = label),
                   arrow = arrow(length = unit(0.15, "cm"))) +
      geom_point(data = vinkel_eksempler, aes(x = x, y = y, color = label), size = 4) +
      geom_text(data = vinkel_eksempler, aes(x = x - 1.5, y = y, label = label, color = label),
                hjust = 1, fontface = "bold", show.legend = FALSE) +
      coord_flip(xlim = c(65, 105), ylim = c(0, 100)) +
      theme_pitch() +
      labs(title = "Visuel sammenligning af skudvinkler", color = NULL) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none"
      )
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
  
  output$body_distance_table <- renderTable({
    allshotevents %>%
      group_by(SHOTBODYPART) %>%
      summarise(
        Antal = n(),
        `Gns. afstand (m)` = round(mean(shot_distance, na.rm = TRUE), 1),
        `SD (afstand)` = round(sd(shot_distance, na.rm = TRUE), 1)
      )
  })
  
  output$team_ranking_summary_ui <- renderUI({
    df <- allshotevents %>%
      group_by(TEAMNAME.x, MATCH_WYID.x, IMAGEDATAURL) %>%
      summarise(shots = n(), .groups = "drop") %>%
      group_by(TEAMNAME.x, IMAGEDATAURL) %>%
      summarise(
        antal_kampe = n(),
        gennemsnit_skud = mean(shots),
        sd_skud = sd(shots),
        .groups = "drop"
      ) %>%
      arrange(desc(gennemsnit_skud))
    
    # lav HTML-rækker med logo
    html_rows <- apply(df, 1, function(row) {
      sprintf(
        '<tr>
        <td><img src="%s" height="30px" style="margin-right:10px;"> %s</td>
        <td>%.0f</td>
        <td>%.2f</td>
        <td>%.2f</td>
      </tr>',
        row["IMAGEDATAURL"],
        row["TEAMNAME.x"],
        as.numeric(row["antal_kampe"]),
        as.numeric(row["gennemsnit_skud"]),
        as.numeric(row["sd_skud"])
      )
    })
    
    # wrap i en HTML-tabel
    HTML(sprintf('
    <table style="width:100%%; font-size:14px;">
      <thead>
        <tr>
          <th>Hold</th>
          <th>Antal kampe</th>
          <th>Gns. skud</th>
          <th>Std. afvigelse</th>
        </tr>
      </thead>
      <tbody>
        %s
      </tbody>
    </table>', paste(html_rows, collapse = "")
    ))
  })
  
  output$rating_table <- renderTable({
    allshotevents %>%
      pivot_longer(cols = c(overall, potential), names_to = "Ratingtype", values_to = "rating") %>%
      group_by(Ratingtype) %>%
      summarise(
        `Gns. rating` = round(mean(rating, na.rm = TRUE), 1),
        `Standardafvigelse` = round(sd(rating, na.rm = TRUE), 1),
        `Antal skud` = n()
      )
  })
  
  output$possession_events_table <- renderTable({
    allshotevents %>%
      summarise(
        Antal_skud = n(),
        Gns_varighed = round(mean(POSSESSIONEVENTSNUMBER, na.rm = TRUE), 2),
        SD_varighed = round(sd(POSSESSIONEVENTSNUMBER, na.rm = TRUE), 2),
        Min = min(POSSESSIONEVENTSNUMBER, na.rm = TRUE),
        Max = max(POSSESSIONEVENTSNUMBER, na.rm = TRUE)
      )
  })
  
  output$possession_index_table <- renderTable({
    data <- if (input$split) train_data else allshotevents
    
    data %>%
      summarise(
        "Gennemsnit" = round(mean(POSSESSIONEVENTINDEX, na.rm = TRUE), 1),
        "Median" = round(median(POSSESSIONEVENTINDEX, na.rm = TRUE), 1),
        "Standardafvigelse" = round(sd(POSSESSIONEVENTINDEX, na.rm = TRUE), 1)
      )
  })
  
  output$duration_summary <- renderTable({
    allshotevents %>%
      mutate(duration_bin = cut(POSSESSIONDURATION,
                                breaks = c(0, 10, 20, 45, Inf),
                                labels = c("Short (0-10s)", "Medium (10-20s)", "Long (20-45s)", "Very long (45s+)"),
                                right = FALSE)) %>%
      group_by(duration_bin) %>%
      summarise(
        `Antal skud` = n(),
        `Gns. varighed` = round(mean(POSSESSIONDURATION, na.rm = TRUE), 1),
        `SD` = round(sd(POSSESSIONDURATION, na.rm = TRUE), 1),
        .groups = "drop"
      )
  })
  

}



shinyApp(ui, server)