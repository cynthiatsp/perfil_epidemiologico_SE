library(dplyr)
library(ggplot2)
library(openxlsx)

# Carregando os dados
dados <- read.xlsx("dados_CID10.xlsx", sheet = 1)

# Convertendo as colunas para o formato correto
dados$ANO_CMPT <- as.factor(dados$ANO_CMPT)
dados$MES_CMPT <- as.numeric(as.character(dados$MES_CMPT)) 
dados$IDADE <- as.numeric(as.character(dados$IDADE))

# Inicializando um dataframe para armazenar os dados mensais dos top 5 diagnósticos de cada ano
dados_mensais_top_diagnosticos <- data.frame()

# Loop para cada ano
anos <- unique(dados$ANO_CMPT)
for (ano in anos) {
  # Identificando os top 5 diagnósticos principais mais frequentes do ano
  top_diagnosticos_ano <- dados %>%
    filter(ANO_CMPT == ano) %>%
    group_by(DIAG_PRINC) %>%
    summarise(Frequencia = n(), .groups = "drop") %>%
    top_n(5, Frequencia) %>%
    pull(DIAG_PRINC)
  
  # Filtrando os dados para apenas os top 5 diagnósticos principais do ano
  dados_ano_top_diagnosticos <- dados %>%
    filter(ANO_CMPT == ano, DIAG_PRINC %in% top_diagnosticos_ano) %>%
    group_by(MES_CMPT, DIAG_PRINC) %>%
    summarise(Frequencia = n(), .groups = "drop") %>%
    mutate(ANO_CMPT = ano)  # Adicionando a coluna ANO_CMPT
  
  # Adicionando os dados do ano ao dataframe
  dados_mensais_top_diagnosticos <- rbind(dados_mensais_top_diagnosticos, dados_ano_top_diagnosticos)
}

# Função auxiliar para calcular o pico mais alto de cada linha para cada ano
get_peak_point <- function(df) {
  df %>%
    group_by(ANO_CMPT, DIAG_PRINC) %>%
    top_n(1, Frequencia) %>%
    ungroup()
}

# Calculando os picos mais altos
peak_points <- get_peak_point(dados_mensais_top_diagnosticos)

# Criando o gráfico de linha
p <- ggplot(dados_mensais_top_diagnosticos, aes(x = MES_CMPT, y = Frequencia, group = interaction(ANO_CMPT, DIAG_PRINC), color = DIAG_PRINC)) +
  geom_line() +
  geom_point(data = peak_points, aes(x = MES_CMPT, y = Frequencia, group = interaction(ANO_CMPT, DIAG_PRINC)), size = 3) +
  geom_text(data = peak_points, aes(x = MES_CMPT, y = Frequencia, label = DIAG_PRINC, group = interaction(ANO_CMPT, DIAG_PRINC), vjust = 2), size = 3, check_overlap = TRUE) +
  facet_wrap(~ANO_CMPT, scales = "free_y", ncol = 3) +
  labs(title = "Distribuição Mensal dos Top 5 Diagnósticos Principais de Cada Ano",
       x = "Mês", y = "Frequência", color = "Diagnóstico Principal") +
  expand_limits(y = c(0, max(dados_mensais_top_diagnosticos$Frequencia) )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5)) +
  theme(legend.position = "bottom")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = 1:12)

p

# Salvando o gráfico
ggsave("Distribuicao_Mensal_Top_Diagnosticos_Linha_Ano.png", plot = p, width = 8, height = 11)

