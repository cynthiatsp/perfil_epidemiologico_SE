# Carregar pacotes necessários
library(dplyr)
library(ggplot2)
library(openxlsx)

# Carregar os dados e ajustá-los
dados_populacao <- read.xlsx("Proj_pop_SE.xlsx", sheet = 1)
dados_internacoes <- read.xlsx("dados_CID10.xlsx", sheet = 1)

dados_internacoes$ANO_CMPT <- as.factor(dados_internacoes$ANO_CMPT)
dados_populacao$Populacao <- as.numeric(dados_populacao$Populacao)
dados_internacoes$IDADE <- as.numeric(as.character(dados_internacoes$IDADE))

# Crie uma nova coluna para a faixa etária com base na idade
dados_internacoes$FAIXA_ETARIA <- cut(dados_internacoes$IDADE, 
                                      breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, Inf),
                                      labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+"),
                                      include.lowest = TRUE)

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

#### Vamos calcular outros indicadores epidemiologicos
# Filtrar internações que resultaram em morte
dados_mortes <- dados_internacoes %>%
  filter(MORTE == "Sim")

# Calcular a Taxa de Mortalidade ajustada para cada faixa etária
taxa_mortalidade <- dados_mortes %>%
  group_by(ANO_CMPT, FAIXA_ETARIA) %>%
  summarise(Total_Mortes = n()) %>%
  left_join(dados_populacao %>%
              group_by(ANO_CMPT, FAIXA_ETARIA) %>%
              summarise(Populacao_Faixa_Etaria = sum(Populacao)), by = c("ANO_CMPT", "FAIXA_ETARIA")) %>%
  mutate(Taxa_Mortalidade = (Total_Mortes / Populacao_Faixa_Etaria) * 10000)

grafico_taxa_mortalidade <- ggplot(taxa_mortalidade, aes(x = ANO_CMPT, y = Taxa_Mortalidade, color = FAIXA_ETARIA, group = FAIXA_ETARIA)) +
  geom_point() +
  geom_line() +
  labs(title = "Taxa de Mortalidade por Faixa Etária e Ano",
       x = "Ano",
       y = "Taxa de Mortalidade por 100.000 habitantes",
       color = "Faixa Etária") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))+ 
  expand_limits(y = c(0, 100))

grafico_taxa_mortalidade
ggsave("taxa_mortalidade_faixaetaria_ano.jpg", grafico_taxa_mortalidade, width = 8, height = 11, units = "in", dpi = 300)


# Calcular a Letalidade
letalidade <- dados_internacoes %>%
  group_by(ANO_CMPT, FAIXA_ETARIA) %>%
  summarise(Total_Casos = n(), Mortes = sum(MORTE == "Sim")) %>%
  mutate(Letalidade = (Mortes / Total_Casos) * 100)

grafico_letalidade <- ggplot(letalidade, aes(x = ANO_CMPT, y = Letalidade, fill = FAIXA_ETARIA)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Letalidade por Faixa Etária e Ano",
       x = "Ano",
       y = "Letalidade (%)") +
  theme_minimal()+
  theme(legend.position = "bottom")+ 
  theme(plot.title = element_text(hjust = 0.5))
grafico_letalidade
ggsave("letalidade_faixaetaria_ano.jpg", grafico_letalidade, width = 8, height = 11, units = "in", dpi = 300)

# Calcular a Proporção de Mortalidade
proporcao_mortalidade <- dados_mortes %>%
  group_by(ANO_CMPT, FAIXA_ETARIA) %>%
  summarise(Total_Mortes = n()) %>%
  left_join(dados_internacoes %>%
              group_by(ANO_CMPT, FAIXA_ETARIA) %>%
              summarise(Total_Internacoes = n()), by = c("ANO_CMPT", "FAIXA_ETARIA")) %>%
  mutate(Proporcao_Mortalidade = (Total_Mortes / Total_Internacoes) * 100)

proporcao_mortalidade$ANO_CMPT <- as.numeric(as.character(proporcao_mortalidade$ANO_CMPT))

grafico_proporcao <- ggplot(proporcao_mortalidade, aes(x = ANO_CMPT, y = Proporcao_Mortalidade, group = FAIXA_ETARIA, color = FAIXA_ETARIA)) +
  geom_point() + # Adiciona pontos
  geom_line() +  # Conecta os pontos com linhas
  labs(title = "Proporção de Mortalidade por Faixa Etária e Ano",
       x = "Ano",
       y = "Proporção de Mortalidade (%)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))
grafico_proporcao
ggsave("proporcao_mortalidade_ano.jpg", grafico_proporcao, width = 8, height = 11, units = "in", dpi = 300)
