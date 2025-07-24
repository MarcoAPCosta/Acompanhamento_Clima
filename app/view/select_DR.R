box::use(
  shiny[...],
  dplyr[...],
)

box::use(
  app/logic/global[opcoes]
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("asd"))
    
    )
  

}

#' @export
server <- function(id, dados, selecao_fora) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns

    output$asd <- renderUI({
      
      selectInput(
        ns("dr"),
        "Escolha o Departamento Regional:",
        choices = c("Brasil" = "BR", opcoes),
        selected = "Brasil"
      )
    })
    
    observeEvent(selecao_fora(), {
      updateSelectInput(session, "dr", selected = selecao_fora())
    })
    
    saida <- reactive({
      input$dr
    })
    
    return(saida)

  })
}
