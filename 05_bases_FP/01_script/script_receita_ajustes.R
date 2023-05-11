library(readr)
library(dplyr)
library(tidyr)


# quantidade de empresas ativas em 2023

qtd_empresas <- read_delim("C:/Users/lapei/Desktop/Faculdade/LAPEI/Extração gov empresas/Novas bases/KPI - Total de Empresas NJ_Dados completos_data.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)

qtd_total <- 
  qtd_empresas %>% 
  group_by(cod_municipio_ibge) %>% 
  summarise(total_ativas = sum(qtd_empresas)) 

qtd_total_nj <- 
  qtd_empresas %>% 
  group_by(cod_municipio_ibge, nom_nj_agrupada) %>% 
  summarise(total_ativas = sum(qtd_empresas))%>%
  spread(nom_nj_agrupada,total_ativas, fill = 0)

qtd_total_tipo_porte <- 
  qtd_empresas %>% 
  group_by(cod_municipio_ibge, tip_porte) %>% 
  summarise(total_ativas = sum(qtd_empresas)) 

qtd_total_tipo_porte_nj <- 
  qtd_empresas %>% 
  group_by(cod_municipio_ibge, tip_porte,nom_nj_agrupada) %>% 
  summarise(total_ativas = sum(qtd_empresas)) 

# quantidade de empresas criadas em 2022


empresas_abertas <- read_delim("C:/Users/lapei/Desktop/Faculdade/LAPEI/Extração gov empresas/Novas bases/KPI - Total de Empresas Abertas_NJ_Dados completos_data.csv", 
           delim = ";", escape_double = FALSE, trim_ws = TRUE)

qtd_abertas <- 
  empresas_abertas %>% 
  group_by(cod_municipio_ibge) %>% 
  summarise(total_abertas = sum(qtd_empresas))

qtd_abertas_nj <- 
  empresas_abertas %>% 
  group_by(cod_municipio_ibge,nom_nj_agrupada) %>% 
  summarise(total_abertas = sum(qtd_empresas))%>%
  spread(nom_nj_agrupada,total_abertas, fill = 0)

# tempo_empresas

tempo_abertura <- read_delim("C:/Users/lapei/Desktop/Faculdade/LAPEI/Extração gov empresas/Novas bases/_Tempo Médio de Abertura (Viabilidade + Registro)__data.csv", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)

tempo_medio <- 
  tempo_abertura %>% 
  group_by(cod_municipio_ibge) %>% 
  summarise(media_tempo_horas = mean(qtd_horas_abertura_soma)) %>% 
  mutate(dias = media_tempo_horas/24)

tempo_medio_nj <- 
  tempo_abertura %>% 
  group_by(cod_municipio_ibge,nom_nj_agrupada) %>% 
  summarise(media_tempo_horas = mean(qtd_horas_abertura_soma)) %>% 
  mutate(media_tempo_horas = round(media_tempo_horas,2))%>%
  spread(nom_nj_agrupada,media_tempo_horas, fill = 0)


# juntando as tres bases 

empresas <- qtd_total %>% 
  left_join(qtd_abertas, by = "cod_municipio_ibge") %>% 
  left_join(tempo_medio, by = "cod_municipio_ibge")

empresas_nj_j <- qtd_total_nj %>% 
  left_join(qtd_abertas_nj, by = "cod_municipio_ibge") %>% 
  left_join(tempo_medio_nj, by = "cod_municipio_ibge")


write.csv(empresas,"dados_receita.csv")
