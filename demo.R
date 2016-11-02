# A demo shiny app of layoutPlateMod
# run with shiny::runApp('demo.R')

library(shiny)

source('layoutPlateMod.R')

server <- function(input, output) {

  data <- reactive({
    expand.grid(Row = LETTERS[1:8], Col = 1:12, stringsAsFactors = F) %>%
      unite(Well, Row, Col, sep = '') %>%
      mutate(Value = runif(n()),
             Color_by = sample(c('Group A', 'Group B', 'Group C', NA_character_), n(), T),
             Label = sample(c(1:5, NA), n(), T)) %>%
      sample_frac(0.9) %>%
      arrange(Color_by)
  })

  selected <- callModule(loplate, 'test', data, 8, 12)

  output$out <- renderPrint({
    selected()
  })

}

ui <- fluidPage(

  loplateUI('test'),
  br(),
  verbatimTextOutput('out')

)

shinyApp(ui, server)
