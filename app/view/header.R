box::use(
  shiny[moduleServer,
        NS, 
        tags,
        textOutput,
        renderText],
  glue[glue],
  DBI[dbReadTable]
)



#' @export
ui <- function(id, nome, tamanho = "xx-large") {
  ns <- NS(id)
  
  tags$div(
    class = "caixa-valores",
    tags$h2(
      id = "titulo",
      nome,
      style = glue("
          margin-top: 25px;
          padding-left: 25px;
          background: url(static/images/header_clima.svg);
          background-position: right;
          background-repeat: no-repeat;
          background-size: 100%;
          font-size:{tamanho}")
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  
      
    })
    

}
