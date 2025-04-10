library(shiny)
library(DT)

ui <- fluidPage(
  titlePanel("xP Visualisering – GIF og tabel"),
  
  fluidRow(
    column(
      width = 6,
      imageOutput("xp_animation"),
      br(),
      actionButton("reload_gif", "Se point-historie igen")
    ),
    column(
      width = 6,
      h3("Akkumuleret Expected Points"),
      dataTableOutput("xp_table")
    )
  )
)

server <- function(input, output, session) {
  gif_state <- reactiveVal("static")  # enten "static" eller "animated"
  
  # Tabel: xp_sum
  output$xp_table <- renderDataTable({
    xp_sum  # <- din data.frame skal være tilgængelig globalt
  })
  
  # GIF visning
  output$xp_gif <- renderImage({
    if (gif_state() == "animated") {
      list(src = "www/xp_animation.gif", contentType = 'image/gif', width = "100%")
    } else {
      list(src = "www/xp_static.png", contentType = 'image/png', width = "100%")
    }
  }, deleteFile = FALSE)
  
  # Når knappen trykkes, vis animation → og efter delay → vis static igen
  observeEvent(input$reload_gif, {
    gif_state("animated")
    
    delay <- 1000 * (maks_kampe * 0.4 + 1)  # tilpas evt. med +1 ekstra sekund
    later::later(function() {
      gif_state("static")
    }, delay / 1000)  # `later` bruger sekunder
  })
}

shinyApp(ui, server)
