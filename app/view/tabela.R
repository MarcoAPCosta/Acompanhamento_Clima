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
server <- function(id, dados, dados1, dr_selecionado) {
  moduleServer(id, function(input, output, session) {
    
    output$tbl_dr <- renderReactable({
      
      req(dr_selecionado())
      
      trad <- data.frame(Nomes = opcoes %>% names,
                         DR = unname(opcoes),
                         stringsAsFactors = FALSE)
      
      
      
      dados_t <- dados() %>%
        filter(!is.na(valido)) %>%
        select(-c(nome_unidade,cod_unidade, tp.aparelho, id, dia, tempo, dt.conclusao, Total))
      
      
      dados_p <- dados1() %>%
        filter(nome_unidade == "Total") %>%
        select(DR, nome_unidade, pop_a)
      
      
      dados_t1 <- left_join(dados_p, dados_t, by = c("DR")) %>%
        group_by(DR, nome_unidade) %>%
        filter(DR != "SG") %>%
        mutate(valido = ifelse(is.na(valido),
                               "0",
                               as.character(valido))) %>%
        summarise(Validos = sum(valido == "1"),
                  Total = unique(pop_a),
                  Taxa = (Validos/Total))
      
      dados_t2 <- dados_t1 %>%
        left_join(trad, by = c("DR")) %>%
        select(DR,Nomes, Validos, Total, Taxa)
      
      
      
      
      reactable(dados_t2,
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
                  headerStyle = list(
                    color = "white",
                    fontWeight = "bold",
                    backgroundColor = "#ec5650
",
                    fontSize = "18px"
                  )
                ) ,rowStyle = function(index) {
                  if (dados_t2[index, "DR"] == dr_selecionado()) {
                    list(background = "rgba(56, 118, 29, 0.5)")
                  }
                },
                columns = list(
                  ead = colDef(
                    show = FALSE
                  ),
                  nome_unidade = colDef(
                    show = FALSE
                  ),
                  DR = colDef(
                    show = FALSE
                  ),
                  Nomes = colDef(
                    name = "Departamento Regional",
                    maxWidth = 300,
                    minWidth = 168
                  ),
                  Validos = colDef(
                    filterable = FALSE,
                    name = "Total de questionários válidos",
                    align = "center",
                    maxWidth = 200,
                    minWidth = 111,
                    style = list(
                      fontSize = "16px"
                      
                    )
                  ),
                  Total = colDef(
                    name = "População Alvo",
                    align = "center",
                    maxWidth = 200,
                    minWidth = 111,
                  ),
                  Taxa = colDef(
                    name = "Taxa de resposta (%)",
                    filterable = FALSE,
                    format = colFormat(separators = TRUE,
                                       percent = TRUE,
                                       digits = 1),
                    maxWidth = 197,
                    minWidth = 110,
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

