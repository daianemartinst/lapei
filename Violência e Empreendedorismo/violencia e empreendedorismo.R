library(microdatasus); library(tidyverse); library(forcats); library(readxl)

setwd("~/LAPEI/violência e empreendedorismo")

# lendo base --------------------------------------------------------------

#Base SIM-DO 2018

SIM_DO2018 <- fetch_datasus(year_start = 2018, month_start = 1, year_end = 2018,month_end = 12, uf= "GO", 
                            information_system = "SIM-DO" ) %>% 
              janitor::clean_names()

cid_10 <- read_excel("C:/Users/adale/Desktop/Faculdade/2020.2/LAPEI/CID/cid_10.xlsx") %>% 
                            filter(IPEA != "NA") %>% 
                            janitor::clean_names()


### ler a base de projeções populacionais aqui 

populacao <- read_delim("C:/Users/adale/Desktop/Faculdade/2020.2/LAPEI/CID/populacao_residente.csv", 
                        ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                        trim_ws = TRUE)


populacao_empilhada <- populacao %>% 
                          gather(key = "sexo", value = "total", 3:4)

populacao_empilhada$ibge <- as.factor(populacao_empilhada$ibge)

# tratamento inicial ------------------------------------------------------
options(scipen = 999)

SIM_CONT2018 <- SIM_DO2018 %>%
  group_by(sexo,codmunocor,causabas)%>%
  count() %>% 
  filter(sexo != "0") %>% 
  left_join(cid_10, by = c("causabas" = "cod_subcat")) %>% 
  filter(cod_cat != "NA") %>% 
  select(-subcategoria_2) %>% 
  mutate(sexo = if_else(sexo == "1", "Masculino", "Feminino")) %>%
  left_join(populacao_empilhada, by = c("codmunocor"="ibge", "sexo")) %>% 
  mutate(uf_codigo = str_sub(codmunocor, end = 2)) %>% 
  filter(uf_codigo == "52")%>%
  mutate(taxa_incidencia = (n/total) * 1000) %>% 
  rename(populacao = total) %>% 
  select(-uf_codigo)
# taxa por 1000 habitantes


# Juntar bases de projeções populacional e SIM_CONT2018
# criar uma nova variável que corresponde à divisão entre o número de óbitos por 
# sexo e a quantidade de mulheres ou homens 

SIM_CONT2018 %>% 
  filter(codmunocor == "520870") %>% 
  ggplot(aes(fct_reorder(ipea,n), n, fill = sexo)) + geom_col() + coord_flip() + 
  theme_bw() 

##---------------------Fazendo os Graficos-----------------------------
## comando fazer duas barras = position = "dodge"

SIM_CONT2018 %>% 
  filter(codmunocor =="521250") %>%
  ggplot(aes(fct_reorder(ipea,taxa_incidencia), taxa_incidencia, fill = sexo)) + geom_col() + coord_flip() +
  theme_bw () + labs(title ="Luziânia")+ theme(plot.title = element_text(hjust = 0.5, size = 20))


##------------------filtrando os municipios--------------------------------------------

ibge_municipios_populosos <- c("520870","520140","520110", "521880","520025","521250")
sexo_feminino <- c ("Feminino")
sexo_masculino <- c ("Masculino")
#Aqui é só para filtrar 

analise_populosos <- SIM_CONT2018 %>%
  filter(codmunocor%in% ibge_municipios_populosos)
ggplot(analise_populosos, aes(fct_reorder(municipio,taxa_incidencia),taxa_incidencia, fill= ipea)) + geom_col(position = "dodge") + coord_flip() + theme_bw() + xlab(" Municípios") + ylab("Taxa de incidência para cada 1000 habitantes") +
  ggtitle("Taxas de óbito - Municípios com mais de 200 mil habitantes ", 
          "Dados do SIM (Datasus) - 2018")
 

analise_populosos_feminino <- SIM_CONT2018 %>%
  filter(codmunocor%in% ibge_municipios_populosos, sexo%in% sexo_feminino)
ggplot(analise_populosos_feminino, aes(fct_reorder(municipio,taxa_incidencia),taxa_incidencia, fill= ipea)) + geom_col(position = "dodge") + coord_flip() + theme_bw() + xlab(" Municípios") + ylab("Taxa de incidência para cada 1000 habitantes") +
  ggtitle("Taxas de óbito Feminino - Municípios com mais de 200 mil habitantes ", 
          "Dados do SIM (Datasus) - 2018")

analise_populosos_masculino <- SIM_CONT2018 %>%
  filter(codmunocor%in% ibge_municipios_populosos, sexo%in% sexo_masculino)
ggplot(analise_populosos_masculino, aes(fct_reorder(municipio,taxa_incidencia),taxa_incidencia, fill= ipea)) + geom_col(position = "dodge") + coord_flip() + theme_bw() + xlab(" Municípios") + ylab("Taxa de incidência para cada 1000 habitantes") +
  ggtitle("Taxas de óbito Masculino - Municípios com mais de 200 mil habitantes ", 
          "Dados do SIM (Datasus) - 2018")



# Fazendo gráficos - Daniel -----------------------------------------------
## Pensei em fazer algo assim 

SIM_CONT2018_feminino <- SIM_CONT2018 %>% 
                              filter(sexo == "Feminino")
SIM_CONT2018_feminino %>% 
  filter(ipea == "Intervenções legais e operações de guerra") %>%
  ungroup() %>% 
  arrange(desc(taxa_incidencia)) %>% 
  top_n(10, taxa_incidencia) %>% 
  ggplot(aes(x = fct_reorder(municipio, taxa_incidencia), y = taxa_incidencia)) + 

  geom_col(fill = "red") + coord_flip() + guides(fill=FALSE) +
  theme_bw() + xlab("Municípios") + ylab("Taxa de incidência para cada 1000 habitantes") +
  ggtitle("Municípios com maiores taxas de óbito por intervenções legais e operações de guerra- sexo feminino", 
          "Dados do SIM (Datasus) - 2018") 

SIM_CONT2018_masculino <- SIM_CONT2018 %>% 
  filter(sexo == "Masculino")


SIM_CONT2018_masculino %>% 
  filter(ipea == "Intervenções legais e operações de guerra") %>%
  ungroup() %>% 
  arrange(desc(taxa_incidencia)) %>% 
  top_n(10, taxa_incidencia) %>% 
  ggplot(aes(x = fct_reorder(municipio, taxa_incidencia), y = taxa_incidencia)) + 
  geom_col(fill="blue") + coord_flip() + guides(fill=FALSE) + 
  theme_bw() + xlab("Municípios") + ylab("Taxa de incidência para cada 1000 habitantes") +
  ggtitle("Municípios com maiores taxas de óbito por intervenções legais e operações de guerra- sexo masculino", 
          "Dados do SIM (Datasus) - 2018") 

  geom_col(fill="darkblue") + coord_flip() + guides(fill=FALSE) +
  theme_bw() + xlab("top 10 municípios") + ylab("Taxa de incidência para cada 1000 habitantes") +
  ggtitle("Top 10 municípios com maiores taxas de óbito por agressão - sexo feminino", 
          "Dados do SIM (Datasus) - 2018")

  write.csv(SIM_CONT2018, "Base_SIM_2018.csv")
  
  
##------------------Base Brasil Todo---------------------------------------

##tratamento
  
  populacao_empilhada_Brasil <- Municipios_Brasil %>% 
    gather(key = "sexo", value = "total", 3:4)
  
 
  Brasil_Obitos_sexo <-obitos_brasil%>%
  left_join(populacao_empilhada_Brasil, by = c("codmunocor"="Ibge", "sexo"))%>%
    rename(populacao = total)%>%
    mutate(taxa_incidencia = (n/populacao) * 1000)%>%
    mutate(uf_codigo = str_sub(codmunocor, end = 2))%>%
    left_join(Estados, by = c ("uf_codigo" = "Ibge"))

  
  Estados$Ibge <- as.character(Estados$Ibge)
  
  glimpse (Estados)
