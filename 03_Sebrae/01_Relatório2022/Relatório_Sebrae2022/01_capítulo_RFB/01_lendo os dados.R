library(tidyverse); 
library(vroom); 
library(genderBR); 
library(readxl); 
library(lubridate);
library(dygraphs)

setwd("~/LAPEI/Projeto SEBRAE/Atualizacao pesquisas/empreendedorismo RFB/estabelecimentos")


# lendo dados ----

# Estabelecimentos ---- 

path_cnae <- "02_bases_apoio/cnae.csv"


tab_cnae <- read_delim(path_cnae, 
                       ",", escape_double = FALSE, trim_ws = TRUE,
                       locale = locale(encoding = "ISO-8859-1")) %>% 
            select(cod_cnae, nm_cnae)

# Arquivo que contem as leituras inicias da base

arquivos_estab <- c("estab0.ESTABELE","estab1.ESTABELE","estab2.ESTABELE",
                    "estab3.ESTABELE","estab4.ESTABELE","estab5.ESTABELE",
                    "estab6.ESTABELE","estab7.ESTABELE","estab8.ESTABELE",
                    "estab9.ESTABELE")
                    
# Lendo dados de estabelecimento ---- 

leitura_estab <- function(arquivo){
  
  dados <- vroom(arquivo, col_names = TRUE) #%>% 
    # select(X1, X2, X3, X4, X5, X6, X7, 
    #        X11, X12, X20, X21) %>% 
    #filter(X6 == "02")
  
  return(dados)
  
}

estab0 <- leitura_estab("estab0.ESTABELE")

?vroom

for(i in arquivos_estab){
  
  base <- tibble()
  dados_lidos <- leitura_estab(i)
  base_estabelecimentos <- rbind(base, dados_lidos)
  
}

base_estabelecimentos %>% 
  rename(cnpj_basico = X1, cnpj_ordem = X2, 
         cnpj_dv = X3, matriz = X4, nome_fantasia = X5, 
         situacao = X6, data_situacao_atual = X7, 
         data_inicio_atividade = X11, cnae = X12, uf = X20, 
         municipio = X21)

# Empresas leitura -----

emp1 <- vroom("empresa1.EMPRECSV", col_names = FALSE) 
emp2 <- vroom("empresa2.EMPRECSV", col_names = FALSE) 
emp3 <- vroom("empresa3.EMPRECSV", col_names = FALSE) 
emp4 <- vroom("empresa4.EMPRECSV", col_names = FALSE) 
emp5 <- vroom("empresa5.EMPRECSV", col_names = FALSE) 
emp6 <- vroom("empresa6.EMPRECSV", col_names = FALSE) 
emp7 <- vroom("empresa7.EMPRECSV", col_names = FALSE) 
emp8 <- vroom("empresa8.EMPRECSV", col_names = FALSE) 
emp9 <- vroom("empresa9.EMPRECSV", col_names = FALSE) 
emp10 <- vroom("empresa10.EMPRECSV", col_names = FALSE) 

# Socios ----

socio1 <- vroom("socio1.SOCIOCSV", col_names = FALSE) 
socio2 <- vroom("socio2.SOCIOCSV", col_names = FALSE) 
socio3 <- vroom("socio3.SOCIOCSV", col_names = FALSE) 
socio4 <- vroom("socio4.SOCIOCSV", col_names = FALSE) 
socio5 <- vroom("socio5.SOCIOCSV", col_names = FALSE) 
socio6 <- vroom("socio6.SOCIOCSV", col_names = FALSE) 
socio7 <- vroom("socio7.SOCIOCSV", col_names = FALSE) 
socio8 <- vroom("socio8.SOCIOCSV", col_names = FALSE) 
socio9 <- vroom("socio9.SOCIOCSV", col_names = FALSE) 
socio10 <- vroom("socio10.SOCIOCSV", col_names = FALSE) 

socios <- rbind(socio1, socio2, socio3, socio4, socio5, 
                socio6, socio7, socio8, socio9, socio10) %>% 
  select(-X4, -X8, -X9, -X10, -X7, -X11) %>% 
  rename(cnpj_basico = X1, id_socio = X2, 
         nome_socio = X3, qualificacao_socio = X5, 
         entrada_sociedade = X6)

# write.csv(socios, "socios.csv")


# Tratamentos -------------------------------------------------------------


empresa <- rbind(emp1, emp2, emp3, emp4, emp5, emp6, emp7, emp8, emp9, emp10)

empresa <- empresa %>% 
  rename(razao_social = X2, natureza_juridica = X3, 
         qualifica_responsavel = X4, capital_social = X5,
         porte_empresa = X6, ente_federativo = X7)

ativas <- estabelecimentos %>% 
  left_join(empresa, by = c("cnpj_basico" = "X1"))  

# write.csv(ativas,"empresas_ativas_RS.csv")
