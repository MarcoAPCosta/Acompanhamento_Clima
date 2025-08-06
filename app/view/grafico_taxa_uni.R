box::use(
  shiny[...],
  bslib[card_header,card_body],
  dplyr[`%>%`,
        filter],
  echarts4r[...],
  htmlwidgets[JS],
  glue[glue],
  stringr[str_detect]
  
)

box::use(
  grafico_tempo_t = app/logic/grafico_tempo_a[tabela_Geracao],
  titulo_grafico_t = app/logic/titulo_grafico_a[transformar_titulo],
  app/logic/funcoes_auxiliares[formatar_numero]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  echarts4rOutput(outputId = ns("chart_tempo_1"))
  
  
  
}

#' @export
server <- function(id, dados, dr_selecionado, unidade) {
  moduleServer(id, function(input, output, session) {
    
    output$chart_tempo_1 <- renderEcharts4r({
      req(dr_selecionado(), unidade())

      
      dados_aqui <- dados() %>%
        filter(DR == dr_selecionado())%>%
        filter(nome_unidade == unidade())
      
      titulo <- transformar_titulo(dados_aqui) %>% round() %>% 
        formatar_numero(ndigitos = 0)
      
      if(nrow(dados_aqui) > 0){
        
        tabela <- tabela_Geracao(dados_aqui)
        
        grafico <- tabela %>%
          e_chart(Data) %>%
          e_bar(Acessos) %>%
          e_legend(show = FALSE) %>%
          e_tooltip(valueFormatter =  JS('function(value) {
        var fmt = new Intl.NumberFormat("pt-BR",
        {style:"decimal",
        minimumFractionDigits:0,
        maximumFractionDigits:0});
        return fmt.format(value);
      }')) %>%
          e_theme_custom('{"color":["#87ac77"]}') %>%
          e_y_axis(formatter = JS('function(value) {
        var fmt = new Intl.NumberFormat("pt-BR",
        {style:"decimal",
        minimumFractionDigits:0,
        maximumFractionDigits:0});
        return fmt.format(value);
      }'),
                   axisLabel = list(fontSize = 14)) %>%
          e_title(text = "Total de acessos por dia, Pesquisa sobre Percepção de Clima, 2025",
                  textStyle = list(fontSize = 18,
                                   fontStyle = "normal")) %>% 
          e_show_loading(text = "Carregando",
                         color = "#8aa8ff",
                         text_color = "#000",
                         mask_color = "rgba(255, 255, 255, 1)")
      }
      
      if(nrow(dados_aqui) == 0){
        x <- data.frame(Sale = 1, modelo = "A", stringsAsFactors = F)
        
        grafico <- e_charts(x,
                            modelo) %>%
          e_bar(Sale,
                animation = T) %>%
          e_legend(show = FALSE) %>%
          e_color("transparent") %>%
          e_labels(position = "inside",
                   formatter = "Até o momento nenhum aluno da Unidade acessou a pesquisa",
                   fontSize = 25,
                   color = "black") %>%
          e_x_axis(show = FALSE) %>%
          e_y_axis(show = FALSE) %>% 
          e_show_loading(text = "Carregando",
                         color = "#8aa8ff",
                         text_color = "#000",
                         mask_color = "rgba(255, 255, 255, 1)")
      }
      
      return(grafico)
      
    })
    
    
  })
}