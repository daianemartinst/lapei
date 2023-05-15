library(readr)
library(dplyr)
library(tidyr)
library(stringr)

setwd("C:/Users/Lapei_Cigets/OneDrive/Documents/LAPEI/a_bases secundárias")

# quantidade de empresas ativas em 2023

qtd_empresas <- read_delim("KPI - Total de Empresas NJ_Dados completos_data.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)

qtd_total <- 
  qtd_empresas %>% 
  group_by(cod_municipio_ibge) %>% 
  summarise(qtd_ativas = sum(qtd_empresas)) %>% 
  filter(cod_municipio_ibge != "NA")

qtd_total_nj <- 
  qtd_empresas %>% 
  group_by(cod_municipio_ibge, nom_nj_agrupada) %>% 
  summarise(total_ativas = sum(qtd_empresas)) %>%
  mutate(nom_nj_agrupada = str_c("qtd_ativas_",nom_nj_agrupada)) %>% 
  spread(nom_nj_agrupada,total_ativas, fill = 0) %>% 
  filter(cod_municipio_ibge != "NA")

qtd_empresas_total_nj <- qtd_total %>% 
                              left_join(qtd_total_nj, by = c("cod_municipio_ibge")) %>% 
                              mutate(cod_municipio_ibge = as.character(cod_municipio_ibge)) %>% 
                              mutate(cod_municipio_ibge = substr(cod_municipio_ibge, 1, 6))


# quantidade de empresas criadas em 2022


empresas_abertas <- read_delim("KPI - Total de Empresas Abertas_NJ_Dados completos_data.csv", 
           delim = ";", escape_double = FALSE, trim_ws = TRUE)

qtd_abertas <- 
  empresas_abertas %>% 
  group_by(cod_municipio_ibge) %>% 
  summarise(qtd_abertas = sum(qtd_empresas)) %>% 
  filter(cod_municipio_ibge != "NA")

qtd_abertas_nj <- 
  empresas_abertas %>% 
  group_by(cod_municipio_ibge,nom_nj_agrupada) %>% 
  summarise(total_abertas = sum(qtd_empresas))%>%
  mutate(nom_nj_agrupada = str_c("qtd_abertas_",nom_nj_agrupada)) %>% 
  spread(nom_nj_agrupada,total_abertas, fill = 0) %>% 
  filter(cod_municipio_ibge != "NA")


qtd_abertas_total_nj <- qtd_abertas %>% 
  left_join(qtd_abertas_nj, by = c("cod_municipio_ibge")) %>% 
  mutate(cod_municipio_ibge = as.character(cod_municipio_ibge)) %>% 
  mutate(cod_municipio_ibge = substr(cod_municipio_ibge, 1, 6))

# tempo_empresas
# pegamos 2022 e 2023 

tempo_abertura <- read_delim("_Tempo Médio de Abertura (Viabilidade + Registro)__data.csv", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)

tempo_medio <- 
  tempo_abertura %>% 
  group_by(cod_municipio_ibge) %>% 
  summarise(qtd_tempo_horas = mean(qtd_horas_abertura_soma)) %>% 
  filter(cod_municipio_ibge != "NA")

tempo_medio_nj <- 
  tempo_abertura %>% 
  group_by(cod_municipio_ibge,nom_nj_agrupada) %>% 
  summarise(media_tempo_horas = mean(qtd_horas_abertura_soma)) %>% 
  mutate(nom_nj_agrupada = str_c("qtd_horas_",nom_nj_agrupada)) %>% 
  mutate(media_tempo_horas = round(media_tempo_horas,2))%>%
  spread(nom_nj_agrupada,media_tempo_horas, fill = 0)

qtd_tempo_nj <- tempo_medio %>% 
  left_join(tempo_medio_nj, by = c("cod_municipio_ibge")) %>% 
  mutate(cod_municipio_ibge = as.character(cod_municipio_ibge)) %>% 
  mutate(cod_municipio_ibge = substr(cod_municipio_ibge, 1, 6))

# juntando as tres bases 


bases_receita <- qtd_empresas_total_nj %>% 
  left_join(qtd_abertas_total_nj, by = "cod_municipio_ibge") %>% 
  left_join(qtd_tempo_nj, by = "cod_municipio_ibge")


write.csv(bases_receita,"dados_receita.csv")
