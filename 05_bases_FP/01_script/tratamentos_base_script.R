

#-----------------------------------------------------------------------Pacotes

library(dplyr)
library(readxl)
library(readr)
library(stringi)
library(tidyr)
library(here)
library(rio)

#--------------------------------------------------------Tratamento Base do TSE


#presidente

tse <- read_delim("C:/Users/lapei/Desktop/Faculdade/LAPEI/Extração gov empresas/Tratamento_bases/votacao_secao_2022_BR.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "latin1"))

tse_tratado <- tse%>%
               select(SG_UF,CD_MUNICIPIO,NM_MUNICIPIO,NR_TURNO, NM_VOTAVEL,QT_VOTOS)%>%
               filter(NR_TURNO == "2")

export(tse_tratado, "tse_reduzida.csv")


#----------------------------------------------------------Tratamento base ENEM


enem <- read_delim("C:/Users/lapei/Desktop/Faculdade/LAPEI/Extração gov empresas/Tratamento_bases/MICRODADOS_ENEM_2021.csv", 
                  delim = ";", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "latin1"))


#mais variaveis de prova

enem_tratado <- enem%>%
  select(CO_MUNICIPIO_ESC,CO_UF_ESC,SG_UF_ESC,CO_MUNICIPIO_PROVA,NO_MUNICIPIO_PROVA,CO_MUNICIPIO_PROVA,SG_UF_PROVA,
         NU_NOTA_CN,NU_NOTA_CH,NU_NOTA_LC,NU_NOTA_MT)


#variaveis apenas das notas

enem_tratado <- enem%>%
  select(CO_MUNICIPIO_ESC,CO_UF_ESC,SG_UF_ESC,NU_NOTA_CN,NU_NOTA_CH,NU_NOTA_LC,NU_NOTA_MT,NU_NOTA_REDACAO)
  
  
  export(enem_tratado, "enem_reduzida.xlsx")

#----------------------------------------------------------Tratamento base PIB

  
pib_municipios <- read_excel("C:/Users/lapei/Desktop/Faculdade/LAPEI/Extração gov empresas/PIB dos Municípios - base de dados 2010-2020.xls")
                  
           
pib_mun <- pib_municipios%>%
           rename(ano =`Ano`, 
                  pib_preco_corrente = `Produto Interno Bruto per capita
a precos correntes
`, 
                  atividade_maior_valor_ad_bruto =`Atividade com maior valor adicionado bruto`,
            codigo_uf = `Código da Unidade da Federação`,
            UF =  `Sigla da Unidade da Federação`, 
            estado = `Nome da Unidade da Federação`,
            codigo_municipio_ibge = `Código do Município`, 
            nome_municipio = `Nome do Município`)%>%
            select(codigo_uf,UF,estado,codigo_municipio_ibge,nome_municipio,ano,pib_preco_corrente,atividade_maior_valor_ad_bruto)%>%
            filter(ano == 2020)
                  
                  
export(pib_mun, "pib_municipio_reduzida.xlsx")


                  
#--------------------------------------------------------Tratamento base Geoses


nacional <- read_csv("C:/Users/lapei/Desktop/Faculdade/LAPEI/Extração gov empresas/Novas bases/nacional.csv")


geoses_reduzida <- nacional%>%
          select(v0002_codigo_do_municipio, GeoSES)


export(geoses_reduzida, "geoses_reduzida.xlsx")




#--------------------------------------------------------------------------fim








