library(readr)
library(dplyr)
library(tidyr)


# quantidade de empresas ativas em 2023

qtd_empresas <- read_delim("C:/Users/Lapei_Cigets/Downloads/KPI - Total de Empresas NJ_Dados completos_data.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)

qtd_total <- 
  qtd_empresas %>% 
  group_by(cod_municipio_ibge) %>% 
  summarise(total_ativas = sum(qtd_empresas)) 

qtd_total_tipo_porte <- 
  qtd_empresas %>% 
  group_by(cod_municipio_ibge, tip_porte) %>% 
  summarise(total_ativas = sum(qtd_empresas)) 

# quantidade de empresas criadas em 2022


empresas_abertas <- read_delim("C:/Users/Lapei_Cigets/Downloads/KPI - Total de Empresas Abertas_NJ_Dados completos_data.csv", 
           delim = ";", escape_double = FALSE, trim_ws = TRUE)

qtd_abertas <- 
  empresas_abertas %>% 
  group_by(cod_municipio_ibge) %>% 
  summarise(total_abertas = sum(qtd_empresas))

# tempo_empresas

tempo_abertura <- read_delim("C:/Users/Lapei_Cigets/Downloads/_Tempo Médio de Abertura (Viabilidade + Registro)__data.csv", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)

tempo_medio <- 
  tempo_abertura %>% 
  group_by(cod_municipio_ibge) %>% 
  summarise(media_tempo_horas = mean(qtd_horas_abertura_soma)) %>% 
  mutate(dias = media_tempo_horas/24)


# juntando as tres bases 

empresas <- qtd_total %>% 
  left_join(qtd_abertas, by = "cod_municipio_ibge") %>% 
  left_join(tempo_medio, by = "cod_municipio_ibge")

write.csv(empresas,"dados_receita.csv")
