box::use(
  shiny[moduleServer, NS, reactiveTimer, reactive, icon],
)

box::use(
  f_importar = app/logic/f_import1[f_importar]
)

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    dados1 <- reactive({
      saida <- f_importar()
      
      return(saida)
      
    })
    
    
    return(dados1)
    
  })
}