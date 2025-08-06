box::use(
  shiny[...],
  dplyr[...],
  bslib[card,
        card_body,
        card_header,
        layout_columns,
        value_box,
        value_box_theme],
  bsicons[bs_icon],
  stats[median],
)

box::use(
  app/view/select_dr_uni,
  app/view/select_uni,
  app/view/grafico_taxa_uni,
  app/view/tp_aparelho_uni,
  app/view/mapa,
  app/view/tabela_uni,
)

box::use(
  app/logic/global[brasil, dados_p],
  app/logic/funcoes_auxiliares[formatar_numero]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  list(
    #ui1
    card(id = "Card1_Uni",
      card_header("Questionários válidos e Taxa de resposta",
                  style = "font-size: 24px;
                  text-align: center;
                  background-color: #ec6c8e;
                  color: white;
                  "),
      card_body(style = "background-color: #EDEDED;
                         color: black;",
                layout_columns(
                  col_widths = c(3, 3, 3, 3),
                  div(select_dr_uni$ui(ns("selecao")),
                      select_uni$ui(ns("selecao_uni"))),
                  
                  value_box(
                    title = "População Alvo da Unidade:",
                    value = textOutput(ns("pop_brasil")),
                    showcase = bs_icon("people-fill"),
                    theme = value_box_theme(fg = "#000",
                                            bg = "#fff")
                  ),
                  value_box(
                    title = "Questionários Válidos da Unidade:",
                    value = textOutput(ns("val_brasil")),
                    showcase = bs_icon("clipboard-check-fill"),
                    theme = value_box_theme(fg = "#000",
                                            bg = "#fff")
                  ),
                  value_box(
                    title = "Taxa de Resposta da Unidade:",
                    value = textOutput(ns("tx_brasil")),
                    showcase = bs_icon("percent"),
                    theme = value_box_theme(fg = "#000",
                                            bg = "#fff")
                  )
                )
      )
    ),
    #ui2
    card(
      card_header("Informações do acesso ao questionário",
                  style = "font-size: 24px;
                  text-align: center;
                  background-color: #ec6c8e;
                  color: white;
                  "),
      card_body(style = "background-color: #EDEDED;
                         color: black;",
                layout_columns(
                  col_widths = c(2, 6, 4),
                  layout_columns(
                    col_widths = c(12,12,12),
                    value_box(
                      title = "Total de Acessos:",
                      value = textOutput(ns("acessos"))
                    ),
                    value_box(
                      title = "Tempo médio de resposta:",
                      value = textOutput(ns("medio"))
                    ),
                    value_box(
                      title = "Tempo mediano de resposta:",
                      value = textOutput(ns("mediana"))
                    )
                  ),
                  card(
                    full_screen = TRUE,
                    
                    card_body(
                      grafico_taxa_uni$ui(ns("taxa"))
                    )
                  ),
                  
                  card(
                    full_screen = TRUE,
                    
                    card_body(
                      tp_aparelho_uni$ui(ns("tp"))
                    )
                    
                  )
                )
      )
    ),
    #ui3
    card(
      card_header(textOutput(ns("titulo_card_unidade")),
                  style = "font-size: 24px; 
                 text-align: center;
                 background-color: #ec6c8e;
                 color: white;
                 "),
      
      card_body(style = "background-color: #EDEDED;
                         color: black;",
                layout_columns(
                  col_widths = c(4, 4, 4),
                  value_box(
                    title = "População Alvo do DR:",
                    value = textOutput(ns("popalvo")),
                    showcase = bs_icon("people-fill"),
                    theme = value_box_theme(fg = "#000",
                                            bg = "#fff")
                  ),
                  value_box(
                    title = "População alvo com contato:",
                    value = textOutput(ns("poppesq")),
                    showcase = bs_icon("person-check-fill"),
                    theme = value_box_theme(fg = "#000",
                                            bg = "#fff")
                  ),
                  value_box(
                    title = "Taxa de Cobertura:",
                    value = textOutput(ns("taxacob")),
                    showcase = bs_icon("percent"),
                    theme = value_box_theme(fg = "#000",
                                            bg = "#fff")
                  )
                )
      )
    )
  )
  
}

#' @export
server <- function(id, dados, dados1,  selecao_fora) {
  moduleServer(id, function(input, output, session) {
    
    selecao <- select_dr_uni$server("selecao", dados, selecao_fora)
    
    unidade <- select_uni$server("selecao_uni", dados1, selecao)
    
    
    dados1_filtrado <- reactive({req(selecao())
      valor <- selecao()
        saida <- dados1() %>%
          filter(DR == selecao(),
                 nome_unidade == "Total")
      
      return(saida)
    })
    
    
    dados2_filtrado <- reactive({
      req(selecao(), unidade())
      valor <- selecao()
      if(valor == "BR"){
        saida <- dados()}else{
          saida <- dados() %>%
            filter(DR == selecao(),
                   nome_unidade == unidade())
        }
      return(saida)
    })
    
    
    
    grafico_taxa_uni$server("taxa", dados, selecao, unidade)
    
    tp_aparelho_uni$server("tp", dados, selecao, unidade)
    
    mapa$server("mapa", brasil,  dados)
    
    tabela_uni$server("tabela", dados, selecao)
    
    output$popalvo <- renderText({
      dados1_filtrado()$pop_a[1] %>% formatar_numero(ndigitos = 0)
    })
    
    output$poppesq <- renderText({
      dados1_filtrado()$pop_p[1] %>% formatar_numero(ndigitos = 0)
    })
    
    output$taxacob <- renderText({
      dados1_filtrado()$tx[1] %>% formatar_numero(percent = T, 
                                                  digitos = 1, 
                                                  ndigitos = 1)
    })
    
    output$acessos <- renderText({
      x <- dados2_filtrado() %>% count() %>% pull(n) %>% formatar_numero
    })
    
    output$medio <- renderText({
      
      
      x <- dados2_filtrado()
      
      if (nrow(x) == 0) {
        saida <- "0"
        
      } else if (nrow(filter(x, valido == "1")) == 0) {
        saida <- "-"
        
      } else {
        saida <- x %>% 
          filter(valido == "1") %>%
          summarise(media = round(mean(tempo, na.rm = TRUE), 2)) %>%
          pull(media) %>%
          formatar_numero(digitos = 1, ndigitos = 1) %>% 
          paste("minutos")
      }
      
      return(saida)
    })
    
    output$mediana <- renderText({
      
      x <- dados2_filtrado()
      
      if (nrow(x) == 0) {
        saida <- "0"
        
      } else if (nrow(filter(x, valido == "1")) == 0) {
        saida <- "-"
        
      } else {
        saida <- x %>% 
          filter(valido == "1") %>%
          summarise(mediana = round(median(tempo, na.rm = TRUE), 2)) %>%
          pull(mediana) %>%
          formatar_numero(digitos = 1, ndigitos = 1) %>% 
          paste("minutos")
      }
      
      return(saida)
      
    })
    
    validos_brasil <- reactive({
      req(selecao(), unidade())
      dados() %>%
        filter(!is.na(valido),
               valido == "1") %>% 
        filter(DR == selecao()) %>%
        filter(nome_unidade == unidade()) %>%
        nrow()
    })
    output$val_brasil <- renderText({
      validos_brasil() %>% 
        formatar_numero()
      
    })
    
    
    popbrasil <- reactive({
      req(selecao(), unidade())
      
      saida <- dados1() %>% 
        filter(DR == selecao()) %>%
        filter(nome_unidade != "Total") %>%
        filter(nome_unidade == unidade()) %>%
        summarise(pop_a = sum(pop_a, na.rm = T)) %>% 
        pull(pop_a)
      
      return(saida)
    })
    
    output$titulo_card_unidade <- renderText({
      req(selecao())
      
      
      
      paste0("População e Cadastro - ",selecao())
    })
    
    
    output$tx_brasil <- renderText({
      req(selecao(), unidade())
      
      valor <- validos_brasil()
      numerador <- popbrasil()
      saida <- valor/numerador
      
      saida <- formatar_numero(saida, percent = T,
                               digitos = 1, 
                               ndigitos = 1)
      
      return(saida)
    })
    
    output$pop_brasil <- renderText({
      saida <- popbrasil() %>% 
        formatar_numero(ndigitos = 0)
      
      return(saida)
    })
    
    return(selecao)
    
  })
  
    
}
