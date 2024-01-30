library(dplyr)
library(ggplot2)
library(openxlsx)

# Função para criar e salvar gráficos
generate_and_save_plots <- function(data, year) {
  dados_ano_especifico <- filter(data, ANO_CMPT == year)
  
  diagnosticos_frequentes_ano <- dados_ano_especifico %>%
    group_by(FAIXA_ETARIA, SEXO, DIAG_PRINC) %>%
    summarise(Frequencia = n(), .groups = "drop") %>%
    arrange(FAIXA_ETARIA, SEXO, desc(Frequencia))
  
  top_diagnosticos_ano <- diagnosticos_frequentes_ano %>%
    group_by(FAIXA_ETARIA, SEXO) %>%
    slice_max(order_by = Frequencia, n = 5, with_ties = FALSE)
  
  for (sexo in unique(data$SEXO)) {
    dados_sexo <- filter(top_diagnosticos_ano, SEXO == sexo)
    
    p <- ggplot(dados_sexo, aes(x = DIAG_PRINC, y = Frequencia, fill = DIAG_PRINC)) +
      geom_bar(stat = "identity", position = "dodge", na.rm = TRUE) +
      facet_wrap(~FAIXA_ETARIA, scales = "free_x", ncol = 3) +
      labs(title = paste("Top 5 Diagnósticos mais frequentes por Faixa Etária para", sexo, "em", year),
           x = "Diagnóstico Principal", y = "Frequência", fill = "Diagnóstico Principal") +  
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            legend.position = "bottom", 
            plot.title = element_text(hjust = 0.5),
            panel.background = element_rect(fill = "white", colour = "white")) # Alteração feita aqui
    
    ggsave(paste0("Diagnostico_", sexo, "_", year, ".png"), plot = p, width = 8, height = 11)
  }
}


# Ler dados
dados <- read.xlsx("dados_CID10.xlsx", sheet = 1)
dados$ANO_CMPT <- as.factor(dados$ANO_CMPT)
dados$MES_CMPT <- as.factor(dados$MES_CMPT)
dados$IDADE <- as.numeric(as.character(dados$IDADE))

# Criar uma nova coluna para a faixa etária com base na idade
dados$FAIXA_ETARIA <- cut(dados$IDADE, 
                          breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, Inf),
                          labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+"),
                          include.lowest = TRUE)

# Controlar a geração de gráficos
create_plots <- TRUE  # Mude para FALSE para não criar gráficos

if (create_plots) {
  for (year in 2013:2022) {
    generate_and_save_plots(dados, as.character(year))
  }
}

