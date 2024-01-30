# Carregar pacotes necessários
library(dplyr)
library(ggplot2)
library(openxlsx)
library(sf)
library(gridExtra)


# Carregar os dados e ajustá-los
dados_populacao <- read.xlsx("Proj_pop_SE.xlsx", sheet = 1)
dados_internacoes <- read.xlsx("dados_CID10.xlsx", sheet = 1)

dados_internacoes$ANO_CMPT <- as.factor(dados_internacoes$ANO_CMPT)
dados_populacao$Populacao <- as.numeric(dados_populacao$Populacao)

# Renomear colunas em dados_populacao para corresponder a dados_internacoes
colnames(dados_populacao)[colnames(dados_populacao) == "Ano"] <- "ANO_CMPT"
colnames(dados_populacao)[colnames(dados_populacao) == "Faixa_etaria"] <- "FAIXA_ETARIA"
colnames(dados_populacao)[colnames(dados_populacao) == "Sexo"] <- "SEXO"

# Função para normalizar os nomes dos municípios
normalize_municipio <- function(municipio) {
  municipio <- tolower(municipio) # Converter para minúsculas
  municipio <- iconv(municipio, to = "ASCII//TRANSLIT") # Remover acentuações
  return(municipio)}

# Aplicar a função de normalização aos dados
dados_populacao$munResNome <- normalize_municipio(dados_populacao$Municipio)
dados_internacoes$munResNome <- normalize_municipio(dados_internacoes$munResNome)

#####
# Agrupar os dados de internações por município e ano, e contar o número de registros
internacoes_agrupadas <- dados_internacoes %>%
  group_by(munResNome, ANO_CMPT) %>%
  summarise(total_internacoes = n(), .groups = "drop")

# Agrupar os dados de população por município e ano
populacao_agrupada <- dados_populacao %>%
  group_by(munResNome, ANO_CMPT) %>%
  summarise(total_populacao = sum(Populacao))

# Juntar os dados de internações e população
dados_juntos <- left_join(internacoes_agrupadas, populacao_agrupada, 
                          by = c("munResNome", "ANO_CMPT"))

# Calcular a prevalência por mil habitantes
dados_juntos$prevalencia_por_mil <- (dados_juntos$total_internacoes / dados_juntos$total_populacao) * 1000

#####

# Selecionar os top 5 municípios por prevalência em cada ano
top5_por_ano <- dados_juntos %>%
  group_by(ANO_CMPT) %>%
  top_n(5, prevalencia_por_mil) %>%
  ungroup()

# Criar uma interação entre o ano e o nome do município para a ordenação
top5_por_ano <- top5_por_ano %>%
  arrange(ANO_CMPT, prevalencia_por_mil) %>%
  mutate(munResNome_ordenado = interaction(ANO_CMPT, munResNome, sep = " - "),
         munResNome_ordenado = factor(munResNome_ordenado, levels = unique(munResNome_ordenado)))

# Criar um gráfico com facetas para cada ano e barras coloridas por município
grafico_top5_prev <- ggplot(top5_por_ano, aes(x = munResNome_ordenado, y = prevalencia_por_mil, fill = munResNome)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(x = "Município", y = "Prevalência por Mil Habitantes") +
  coord_flip() +
  facet_wrap(~ANO_CMPT, scales = "free") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.x = element_text(size = 8),
        legend.position = "bottom")

grafico_top5_prev
ggsave("prev_municipio_ano.jpg", grafico_top5_prev, width = 8, height = 11, units = "in", dpi = 300)


#######
#Plotando um mapa
# Carregar os dados geográficos de Sergipe
sergipe_shape <- st_read("C:/Users/cynthia/Downloads/TCC/SE_Municipios_2022/SE_Municipios_2022.shp")

# Ajustes - normalizacao
colnames(sergipe_shape)[colnames(sergipe_shape) == "NM_MUN"] <- "munResNome"
sergipe_shape$munResNome <- normalize_municipio(sergipe_shape$munResNome)

# Combinar os dados geográficos com os dados de prevalência
mapa_dados <- merge(sergipe_shape, dados_juntos, by.x = "munResNome", by.y = "munResNome")

# Função para criar um mapa coroplético para um dado ano
criar_mapa_ano <- function(ano) {
  dados_ano <- mapa_dados %>% filter(ANO_CMPT == ano)
  
  ggplot(data = dados_ano) +
    geom_sf(aes(fill = prevalencia_por_mil), color = "white") +
    scale_fill_gradient(low = "blue", high = "red", name = "Prevalência") +
    labs(title = paste("Prevalência por mil habitantes, Sergipe -", ano),
         fill = "Prevalência") +
    theme_minimal() +
    theme(plot.title = element_text(size = 12),
          legend.title = element_blank(),
          axis.text.x = element_text(angle = 0, hjust = 1),
          strip.text.x = element_text(size = 8))
  }

# Criar e salvar mapas para cada ano
anos <- unique(mapa_dados$ANO_CMPT)
for (ano in anos) {
  mapa_ano <- criar_mapa_ano(ano)
  print(mapa_ano)
  ggsave(paste0("Mapa_prevalencia", ano, ".jpeg"), plot = mapa_ano, width = 10, height = 8, units = "in")
}


#####
# Também podemos fazer o seguinte: Top 3 Diagnósticos Principais em Top 5 Municípios de Maior Prevalência por Ano",

# Selecionar os top 5 municípios por prevalência em cada ano
top5_municipios_por_ano <- dados_juntos %>%
  group_by(ANO_CMPT) %>%
  top_n(5, prevalencia_por_mil) %>%
  select(ANO_CMPT, munResNome) %>%
  ungroup()

# Filtrar os dados de internações para incluir apenas os registros dos top 5 municípios em cada ano
dados_filtrados <- dados_internacoes %>%
  inner_join(top5_municipios_por_ano, by = c("munResNome", "ANO_CMPT"))

# Contar os DIAG_PRINC mais comuns em cada top 5 município por ano
top3_diag_por_municipio_ano <- dados_filtrados %>%
  group_by(ANO_CMPT, munResNome) %>%
  count(DIAG_PRINC, sort = TRUE) %>%
  top_n(3, n) %>%
  ungroup()

# Gerar o gráfico de barras lado a lado
grafico_diag_princ <- ggplot(top3_diag_por_municipio_ano, aes(x = munResNome, y = n, fill = DIAG_PRINC)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~ANO_CMPT, scales = "free_x") +
  labs(title = "Top 3 Diagnósticos Principais em Top 5 Municípios de Maior Prevalência por Ano",
       x = "Município",
       y = "Frequência",
       fill = "Diagnóstico Principal") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "bottom")
grafico_diag_princ
ggsave("top3_diagnosticos_top5mun_ano.jpg", grafico_diag_princ, width = 8, height = 11, units = "in", dpi = 300)


