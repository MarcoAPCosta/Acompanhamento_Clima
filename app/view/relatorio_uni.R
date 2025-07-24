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
  app/view/select_DR,
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
    card(
      style = "margin-top: 20px",
      card_header("População e cadastro",
                  style = "font-size: 24px;
                  text-align: center;
                  background-color: #fb1366;
                  color: white;
                  "),
      card_body(style = "background-color: #EDEDED;
                         color: black;",
                layout_columns(
                  col_widths = c(3, 3, 3, 3),
                  div(select_DR$ui(ns("selecao")),
                      select_uni$ui(ns("selecao_uni"))),
                  
                  value_box(
                    title = "População Alvo:",
                    value = textOutput(ns("popalvo")),
                    showcase = bs_icon("people-fill"),
                    theme = value_box_theme(fg = "#000",
                                            bg = "#fff")
                  ),
                  value_box(
                    title = "População Alvo com contato:",
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
    ),
    #ui2
    card(
      card_header("Informações do acesso ao questionário",
                  style = "font-size: 24px;
                  text-align: center;
                  background-color: #6AA84F;
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
      card_header("Questionários válidos e Taxa de resposta",
                  style = "font-size: 24px; 
                 text-align: center;
                 background-color: #6AA84F;
                 color: white;
                 "),
      
      card_body(style = "background-color: #EDEDED;
                         color: black;",
                layout_columns(
                  col_widths = c(4, 4, 4),
                  value_box(
                    title = "População Alvo do DR:",
                    value = textOutput(ns("pop_brasil")),
                    showcase = bs_icon("people-fill"),
                    theme = value_box_theme(fg = "#000",
                                            bg = "#fff")
                  ),
                  value_box(
                    title = "Questionários válidos da Unidade:",
                    value = textOutput(ns("val_brasil")),
                    showcase = bs_icon("clipboard-check-fill"),
                    theme = value_box_theme(fg = "#000",
                                            bg = "#fff")
                  ),
                  value_box(
                    title = "Taxa de resposta da Unidade em relação ao DR:",
                    value = textOutput(ns("tx_brasil")),
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
server <- function(id, dados, dados1, selecao_fora) {
  moduleServer(id, function(input, output, session) {
    
    selecao <- select_DR$server("selecao", dados, selecao_fora)
    
    unidade <- select_uni$server("selecao_uni", dados, selecao)
    
    
    ead_valor <- reactive({
      print(unidade())
      dados() %>%
        pull(ead) %>% 
        unique() 
    })
    
    selecao1 <- reactive({req(selecao())
      dados_p |> 
        filter(DR == selecao()) |> 
        filter(ead == ead_valor())
    })
    
    dados1_filtrado <- reactive({req(selecao())
      valor <- selecao()
      if(valor == "BR"){
        saida <- dados1() %>% 
          summarise(across(c(pop_a, pop_p),
                           ~sum(.x, na.rm =T))) %>% 
          mutate(tx = pop_p/pop_a)
      }else{
        saida <- dados1() %>%
          filter(DR == selecao())
      }
      return(saida)
    })
    
    
    dados2_filtrado <- reactive({
      req(selecao(), unidade())
      valor <- selecao()
      if(valor == "BR"){
        saida <- dados()}else{
          saida <- dados() %>%
            filter(DR == selecao(),
                   unidade == unidade())
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
      
      if(nrow(x) > 0){
        saida <- x %>%
          summarise(media = round(mean(tempo), 2)) %>% 
          pull(media) %>%
          formatar_numero(
            digitos = 1, 
            ndigitos = 1) %>% 
          paste("mins")
      }
      
      if(nrow(x) == 0) saida <- "0"
      
      return(saida)
    })
    
    output$mediana <- renderText({
      
      x <- dados2_filtrado()
      if(nrow(x) > 0){
        saida <- x %>% 
          summarise(mediana = round(median(tempo), 2)) %>%
          pull(mediana) %>%
          formatar_numero(
            digitos = 1, 
            ndigitos = 1) %>% 
          paste("mins")
      }
      
      if(nrow(x) == 0) saida <- "0"
      
      return(saida)
    })
    
    validos_brasil <- reactive({
      req(selecao(), unidade())
      dados() %>%
        filter(!is.na(valido),
               valido == "1") %>% 
        filter(DR == selecao()) %>%
        filter(unidade == unidade()) %>%
        nrow()
    })
    output$val_brasil <- renderText({
      validos_brasil() %>% 
        formatar_numero()
      
    })
    
    
    popbrasil <- reactive({
      req(selecao())
      
      saida <- dados1() %>% 
        filter(DR == selecao()) %>%
        group_by(DR) %>%
        summarise(pop_a = sum(pop_a, na.rm = T)) %>% 
        pull(pop_a)
      
      return(saida)
    })
    
    
    output$tx_brasil <- renderText({
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
