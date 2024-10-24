box::use(
  shiny[moduleServer,
        NS, 
        tags,
        textOutput,
        renderText],
  glue[glue]
)

box::use(
  app/logic/global[hora_da_exportação]
)

#' @export
ui <- function(id, nome, tamanho = "xx-large") {
  ns <- NS(id)
  
  tags$div(
    class = "caixa-valores",
    style = "
      background: url(static/images/header.svg);
      background-position: right;
      background-repeat: no-repeat;
      background-size: 60%;
    ",
    tags$h2(
      id = "titulo",
      nome,
      style = glue("
          margin-top: 25px;
          margin-bottom: 0;
          padding-left: 25px;
          font-size:{tamanho}")
    ),
    tags$h5(textOutput(ns("hora")),
            style = "height: 1em;
            color:white;
            position: relative;
            align-content: center;
            padding-left: 25px;
            text-align: left;")
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$hora <- renderText({
      
        saida <- glue("Dados atualizado em {hora_da_exportação}")
        
        return(saida)
      
    })
    
  })
}
