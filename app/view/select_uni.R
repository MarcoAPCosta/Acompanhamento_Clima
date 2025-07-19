box::use(
  shiny[...],
  dplyr[...]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("unidade_ui"))
}

#' @export
server <- function(id, dados, dr_selecionado) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    unidades_filtradas <- reactive({
      req(dr_selecionado())
      print(dados())
      df <- dados() %>% filter(DR == dr_selecionado()) %>%
        pull(unidade) %>%
        unique() %>%
        sort()
    })
    
    output$unidade_ui <- renderUI({
      selectInput(
        ns("unidade"),
        "Escolha a Unidade:",
        choices = unidades_filtradas()
      )
    })
    
    observeEvent(dr_selecionado(), {
      updateSelectInput(session, "unidade", choices = unidades_filtradas())
    })
    
    saida <- reactive({
      input$unidade
    })
    return(saida)
  })
}
