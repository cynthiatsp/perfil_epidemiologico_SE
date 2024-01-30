##### Pacote para download de microdados do SUS e pré-processamento dos mesmos

# Instalar e carregar os pacotes
install.packages("devtools")
install.packages("writexl")
devtools::install_github("rfsaldanha/microdatasus", force = TRUE)
library(microdatasus)
library(writexl)

# Carregar a base de dados de Morbidade Hospitalar do Sistema de Informações Hospitalares (SIH) do SUS
# para oestado de SE no período 10 anos (janeiro de 2013 a dezembro de 2022)

dados <- fetch_datasus(year_start = 2013, month_start = 1, year_end = 2022, 
                       month_end = 12, uf = "SE", information_system = "SIH-RD")
dados <- process_sih(dados)
head(dados)

#Selecao das colunas de interesse
dados_selecao <- dados[c("UF_ZI", "ANO_CMPT", "MES_CMPT", "NASC", "SEXO", "IDADE", "RACA_COR",
                         "CEP", "munResNome", "munResUf",
                         "munResLat", "munResLon", "munResAlt",
                         "DT_INTER", "DT_SAIDA", "QT_DIARIAS",
                         "DIAG_PRINC", "DIAG_SECUN",
                         "VAL_SH", "VAL_SP", "VAL_TOT", "COBRANCA", "COMPLEX",
                         "CAR_INT", "CID_NOTIF", "MORTE", "CID_MORTE")]


unique(dados_selecao$DIAG_PRINC)

# Filtragem de elementos que começam com "J" em DIAG_PRINC
dados_filtrados <- subset(dados_selecao, startsWith(DIAG_PRINC, "J") & munResUf == "Sergipe")

#Salvando o banco de dados
write_xlsx(dados_filtrados, "dados_CID10.xlsx")
