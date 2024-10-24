box::use(
  shiny[moduleServer,
        NS,
        onStop,
        reactive],
  bslib[bs_theme,
        page_fillable,
        nav_panel,
        navset_tab]
)

box::use(
  app/view/relatorio,
  app/view/dados,
  app/view/header,
  app/view/dados1,
)
# 
box::use(
  app/logic/conexao_database
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_fillable(
    title = "Painel de Acompanhamento ANQP",
    header$ui(ns("titulo"), 
              "Painel de acompanhamento da ANQP - 2024",
              "xxx-large"),
    navset_tab(
      id = ns("rede"),
      nav_panel(
        title = "Rede Presencial",
        value = "presencial",
        relatorio$ui(ns("presencial"))
      ),
      nav_panel(
        title = "Rede EAD",
        value = "ead",
        relatorio$ui(ns("ead")))
    )
  )
  
  
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    rede_tab <- reactive({
      x <- ifelse(input$rede == "ead", 1, 0)
      return(x)
    })
    
    dados <- dados$server("asdas", rede_tab)
    
    dados1 <- dados1$server("asdasd", rede_tab)
    
    selecao_p <- relatorio$server("presencial", dados, dados1, selecao_e)
    
    selecao_e <- relatorio$server("ead", dados, dados1, selecao_p)
    
    header$server("titulo")
    
    
  })
}
