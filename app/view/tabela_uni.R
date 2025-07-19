box::use(
  shiny[moduleServer, NS, reactive, req, strong, span, em, HTML, br, tags],
  bslib[card_header, card_body,
        tooltip],
  dplyr[...],
  tidyr[starts_with],
  reactable[...],
)


box::use(
  app/logic/global[opcoes]
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  
  reactableOutput(ns("tbl_dr"),
                  width = "100%")
  
  
}

#' @export
server <- function(id, dados, dr_selecionado) {
  moduleServer(id, function(input, output, session) {
    
    output$tbl_dr <- renderReactable({
      # trad <- data.frame(Nomes = opcoes %>% names,
      #                    DR = unname(opcoes),
      #                    stringsAsFactors = FALSE) %>% 
      #   mutate(Nomes = factor(Nomes))
      
      dados_t <- dados() %>%
        filter(!is.na(valido)) %>% 
        mutate(.keep = "unused") %>% 
        group_by(DR, unidade) %>%
        filter(DR != "SG",
               DR == dr_selecionado()) %>%
        summarise(Validos = sum(valido == "1"),
                  Total = unique(Total),
                  Taxa = (Validos/Total)) %>%
        select(unidade, Validos, Total, Taxa)
      
      
      
      reactable(dados_t,
                pagination = FALSE,
                filterable = FALSE,
                highlight = TRUE,
                bordered = TRUE,
                striped = FALSE,
                height = 750,
                defaultColDef = colDef(format = colFormat(separators = TRUE,
                                                          locales = "pt-BR")),
                theme = reactableTheme(
                  color = "black",
                  highlightColor = "#8aa8ff",
                  headerStyle = list(
                    color = "white",
                    fontWeight = "bold",
                    backgroundColor = "#ffa32a",
                    fontSize = "18px"
                  )
                ),
                columns = list(
                  ead = colDef(
                    show = FALSE
                  ),
                  DR = colDef(
                    show = FALSE
                  ),
                  unidade = colDef(
                    name = "Unidade"
                  ),
                  Validos = colDef(
                    filterable = FALSE,
                    name = "Total de questionários válidos",
                    align = "center",
                    style = list(
                      fontSize = "16px"
                      
                    )
                  ),
                  Total = colDef(
                    show = FALSE
                  ),
                  Taxa = colDef(
                    name = "Taxa de resposta (%)",
                    filterable = FALSE,
                    format = colFormat(separators = TRUE,
                                       percent = TRUE,
                                       digits = 1),
                    align = "center",
                    style = list(
                      fontSize = "16px"
                    )
                  )
                )
                
      )
      
      
      
    })
    
  })
}

