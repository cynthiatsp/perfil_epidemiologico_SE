# Carregar pacotes necessários

library(ggplot2)
library(dplyr)
library(readxl)

# Carregar os dados
dados <- read_excel("dados_CID10.xlsx", sheet = 1)

# Converter as colunas para o formato correto
dados$ANO_CMPT <- as.factor(dados$ANO_CMPT)
dados$MES_CMPT <- as.factor(dados$MES_CMPT)
dados$IDADE <- as.numeric(as.character(dados$IDADE))

# Criar nova coluna para a faixa etária com base na idade
dados$FAIXA_ETARIA <- cut(dados$IDADE, 
                          breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, Inf),
                          labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+"),
                          include.lowest = TRUE)

### Gráfico 1:Soma de Diagnósticos Principais por Faixa Etária e Sexo em Cada Ano

# Contar o número de ocorrências de DIAG_PRINC por sexo, faixa etária e ano
dados_agrupados <- dados %>%
  group_by(ANO_CMPT, FAIXA_ETARIA, SEXO) %>%
  summarise(Contagem = n(), .groups = 'drop')

# Encontrar a maior soma para cada combinação de ano e sexo
maiores_somas <- dados_agrupados %>%
  group_by(ANO_CMPT, SEXO) %>%
  top_n(1, Contagem) %>%
  ungroup()

# Criar o gráfico
grafico_etario <- ggplot(dados_agrupados, aes(x = FAIXA_ETARIA, y = Contagem, color = SEXO, group = interaction(SEXO, ANO_CMPT))) +
  geom_line() +
  geom_point(data = maiores_somas, aes(x = FAIXA_ETARIA, y = Contagem), size = 3, shape = 19) +
  geom_text(data = maiores_somas, aes(x = FAIXA_ETARIA, y = Contagem, label = Contagem), vjust = -0.5, hjust = -0.5, size = 3) +
  facet_wrap(~ANO_CMPT, scales = "free_x") +
  ylim(0, 2000) +
  theme_minimal() +
  labs(title = "Internações pelo Cap.X do CID-10 por Faixa Etária e Sexo em Cada Ano",
       x = "Faixa Etária",
       y = "Internações",
       color = "Sexo") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  theme(legend.position = "bottom")+ 
  theme(plot.title = element_text(hjust = 0.5))
grafico_etario
ggsave("internacoes_faixaetaria_sexo_ano.jpg", grafico_etario, width = 8, height = 11, units = "in", dpi = 300)


### Gráfico 2:Soma de Diagnósticos Principais por Mês e Sexo em Cada Ano

# Agrupar dados por ANO_CMPT, MES_CMPT e SEXO, e somar DIAG_PRINC
dados_mensais <- dados %>%
  group_by(ANO_CMPT, MES_CMPT, SEXO) %>%
  summarise(SomaDiag = n(), .groups = 'drop')

# Renomear os meses
meses <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
dados_mensais$MES_CMPT <- factor(dados_mensais$MES_CMPT, levels = as.character(1:12), labels = meses)

# Criar o gráfico 
grafico_mensal <- ggplot(dados_mensais, aes(x = MES_CMPT, y = SomaDiag, color = SEXO, group = interaction(SEXO, ANO_CMPT))) +
  geom_line() +
  facet_wrap(~ANO_CMPT, scales = "free_y") +
  theme_minimal() +
  labs(title = "Internações pelo Cap.X do CID-10 por Mês e Sexo em Cada Ano",
       x = "Mês",
       y = "Soma de Diagnósticos Principais",
       color = "Sexo") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  theme(legend.position = "bottom")+ 
  theme(plot.title = element_text(hjust = 0.5))
grafico_mensal
ggsave("internacoes_sexo_mes_ano.jpg", grafico_mensal, width = 8, height = 11, units = "in", dpi = 300)


## Análise estatística:

# Verificar a normalidade dos dados para cada mês usando o teste de Shapiro-Wilk
resultados_shapiro <- by(dados_mensais$SomaDiag, dados_mensais$MES_CMPT, shapiro.test)
resultados_shapiro

# Dados não são normalmente distribuídos em todos os meses,dessa forma utilizaremos o teste de Kruskal-Wallis
kruskal_resultado <- kruskal.test(SomaDiag ~ MES_CMPT, data = dados_mensais)
kruskal_resultado

# Análise post hoc
# Instalar o pacote FSA se ainda não estiver instalado
if (!requireNamespace("FSA", quietly = TRUE)) {
  install.packages("FSA")
}

# Carregar o pacote FSA
library(FSA)

# Realizar o teste de Dunn
dunn_resultado <- dunnTest(SomaDiag ~ MES_CMPT, data = dados_mensais, method="bonferroni")
dunn_resultado

# As diferenças significativas encontradas sugerem que algo específico nesses meses (Agosto, Dezembro e Fevereiro) pode estar influenciando a quantidade de diagnósticos principais. 
# Essa influência pode ser devido a fatores sazonais (como doenças que ocorrem mais frequentemente em determinadas épocas do ano), eventos especiais que acontecem nesses meses, ou outros fatores ambientais ou sociais. É importante notar que essas comparações são feitas entre meses individuais, e não indicam que há uma tendência ou padrão contínuo ao longo de um período que abrange esses meses. Para entender melhor o que está causando essas diferenças, seria necessário investigar mais a fundo os tipos de diagnósticos que estão sendo feitos nesses meses e quaisquer fatores externos que possam estar influenciando esses números.\\\


### Grafico 3:Média de Diárias e Média do Valor Total por Faixa Etária e Sexo ao Ano

# Converter QT_DIARIAS e VAL_TOT para numérico
dados$QT_DIARIAS <- as.numeric(as.character(dados$QT_DIARIAS))
dados$VAL_TOT <- as.numeric(as.character(dados$VAL_TOT))

# Calculando médias de diárias e valor total
dados_agrupados <- dados %>%
  group_by(ANO_CMPT, FAIXA_ETARIA, SEXO) %>%
  summarise(MediaDiarias = mean(QT_DIARIAS, na.rm = TRUE),
            MediaValorTotal = mean(VAL_TOT, na.rm = TRUE),
            .groups = 'drop')


# Encontrar a relação entre as escalas dos dois eixos Y
fator_escala <- max(dados_agrupados$MediaDiarias, na.rm = TRUE) / max(dados_agrupados$MediaValorTotal, na.rm = TRUE)

# Calculando a média do valor total para cada faixa etária e ano
dados_valor_total <- dados_agrupados %>%
  group_by(ANO_CMPT, FAIXA_ETARIA) %>%
  summarise(MediaValorTotal = mean(MediaValorTotal, na.rm = TRUE), .groups = 'drop')

# Criar o gráfico
grafico_etario_valores <- ggplot(dados_agrupados, aes(x = FAIXA_ETARIA)) +
  geom_bar(aes(y = MediaDiarias, fill = SEXO), stat = "identity", position = position_dodge()) +
  geom_line(data = dados_valor_total, aes(y = MediaValorTotal * fator_escala, group = ANO_CMPT), color = "black", linewidth = 0.5) +
  scale_y_continuous(name = "Média de Diárias",
                     sec.axis = sec_axis(~ . / fator_escala, name = "Média do Valor Total (R$)")) +
  facet_wrap(~ANO_CMPT) +
  labs(title = "Média de Diárias e Média do Valor Total por Faixa Etária e Sexo ao Ano",
       x = "Faixa Etária", color = "Sexo", fill = "Sexo") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  theme(legend.position = "bottom")+ 
  theme(plot.title = element_text(hjust = 0.5))
grafico_etario_valores
ggsave("media_diarias_valores_faixaetaria.jpg", grafico_etario_valores, width = 8, height = 11, units = "in", dpi = 300)


### Grafico 4: Top 5 Municípios com Maiores Somas de Diagnósticos Principais por Ano e Sexo"
#Convertendo coluna
dados$SEXO <- as.factor(dados$SEXO)

# Agrupar por município, ano e sexo, e somar DIAG_PRINC
dados_municipio <- dados %>%
  group_by(ANO_CMPT, munResNome, SEXO) %>%
  summarise(SomaDiag = n(), .groups = 'drop') %>%
  arrange(desc(SomaDiag))

# Selecionar os top 5 municípios por ano e sexo
top_municipios <- dados_municipio %>%
  group_by(ANO_CMPT, SEXO) %>%
  top_n(5, SomaDiag) %>%
  ungroup()

# Criar o gráfico
grafico_municipios <- ggplot(top_municipios, aes(y = reorder(munResNome, SomaDiag), x = SomaDiag, fill = SEXO)) +
  geom_bar(stat = "identity", position = position_dodge(), orientation = "y") +
  facet_wrap(~ANO_CMPT, scales = "free_x") +
  labs(title = "Top 5 Municípios com Maiores Internações pelo Cap.X do CID-10 por Ano e Sexo",
       y = "Município",
       x = "Internações",
       fill = "Sexo") +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5))+
  theme(legend.position = "bottom")+ 
  theme(plot.title = element_text(hjust = 0.5))
grafico_municipios
ggsave("top5_internacoes_municipio_ano.jpg", grafico_municipios, width = 8, height = 11, units = "in", dpi = 300)


