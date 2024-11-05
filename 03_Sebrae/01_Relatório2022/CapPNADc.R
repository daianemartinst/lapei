if("PNADcIBGE" %in% rownames(installed.packages())==FALSE)
{
  install.packages("PNADcIBGE", dependencies=TRUE)
}


#install.packages("survey")

if("survey" %in% rownames(installed.packages())==FALSE)
{
  install.packages("survey")
}

#abrindo os pacotes
library(PNADcIBGE)
library(survey)
library(tidyverse)
library (readr)

?get_pnadc

#dados da PNADc de modo automatico

variaveis_selecionadas_PNADc <-c("UF", "V2007", "VD4007", "V1023", "V2009", 
                                 "V2010", "VD3004", "VD4017", "V4019", "V4016",
                                 "V4017", "V40171", "V4022", "V4040", "V4072")



dados_pnadc21_4tri <- get_pnadc(year=2022, quarter=4, vars=variaveis_selecionadas_PNADc)

class(dados_pnadc21)

#-----------------------------------

#Estimativas do QUARTO TRIMESTRE DE 2020 - Goias----

pnadc_goias21_4tri <- subset(dados_pnadc21_4tri, UF == "Goi?s")
pnadc_goias21_4tri

pnadc_mulherGO21_4tri <- subset(pnadc_goias21_4tri , V2007 == "Mulher")
pnadc_mulherGO21_4tri

pnadc_homemGO21_4tri <- subset(pnadc_goias21_4tri , V2007 == "Homem")
pnadc_homemGO21_4tri




##### 1 - Onde exercia normalmente esse trabalho#####

#-------Mulher-------

local_trabalho_empregado_mulher21_4tri <- svytotal(~V4022, subset (pnadc_mulherGO21_4tri, VD4007 == "Empregado (inclusive trabalhador dom?stico)"), na.rm = T)
local_trabalho_empregado_mulher21_4tri

local_trabalho_empregador_mulher21_4tri <- svytotal(~V4022, subset (pnadc_mulherGO21_4tri, VD4007 == "Empregador"), na.rm = T)
local_trabalho_empregador_mulher21_4tri

local_trabalho_cpropria_mulher21_4tri <- svytotal(~V4022, subset (pnadc_mulherGO21_4tri, VD4007 == "Conta pr?pria"), na.rm = T)
local_trabalho_cpropria_mulher21_4tri

#-------Homem--------

localtrabalho_empregado_homem21_4tri <- svytotal(~V4022, subset (pnadc_homemGO21_4tri, VD4007 == "Empregado (inclusive trabalhador dom?stico)"), na.rm = T)
localtrabalho_empregado_homem21_4tri

localtrabalho_empregador_homem21_4tri <- svytotal(~V4022, subset (pnadc_homemGO21_4tri, VD4007 == "Empregador"), na.rm = T)
localtrabalho_empregador_homem21_4tri

localtrabalho_cpropria_homem21_4tri <- svytotal(~V4022, subset (pnadc_homemGO21_4tri, VD4007 == "Conta pr?pria"), na.rm = T)
localtrabalho_cpropria_homem21_4tri


####### 2 - Esse neg?cio era registrado no CNPJ######

#-------Mulher-------

CNPJ_empregador_mulher21_4tri <- svytotal(~V4019, subset (pnadc_mulherGO21_4tri, VD4007 == "Empregador"), na.rm = T)
CNPJ_empregador_mulher21_4tri

CNPJ_cpropria_mulher21_4tri <- svytotal(~V4019, subset (pnadc_mulherGO21_4tri, VD4007 == "Conta pr?pria"), na.rm = T)
CNPJ_cpropria_mulher21_4tri

#-------Homem--------

CNPJ_empregador_homem21_4tri <- svytotal(~V4019, subset (pnadc_homemGO21_4tri, VD4007 == "Empregador"), na.rm = T)
CNPJ_empregador_homem21_4tri

CNPJ_cpropria_homem21_4tri <- svytotal(~V4019, subset (pnadc_homemGO21_4tri, VD4007 == "Conta pr?pria"), na.rm = T)
CNPJ_cpropria_homem21_4tri


###### 3 - C?digo da principal atividade desse neg?cio/empresa#######

#-------Mulher-------



#-------Homem--------


####### 4 - Quantos empregados trabalhavam nesse neg?cio##########

#-------Mulher-------

qtdEMPREGADOS_empregador_mulher21_4tri <- svytotal(~V4016, subset (pnadc_mulherGO21_4tri, VD4007 == "Empregador"), na.rm = T)
qtdEMPREGADOS_empregador_mulher21_4tri

qtdEMPREGADOS_cpropria_mulher21_4tri <- svytotal(~V4016, subset (pnadc_mulherGO21_4tri, VD4007 == "Conta pr?pria"), na.rm = T)
qtdEMPREGADOS_cpropria_mulher21_4tri

#-------Homem--------
qtdEMPREGADOS_empregador_homem21_4tri <- svytotal(~V4016, subset (pnadc_homemGO21_4tri, VD4007 == "Empregador"), na.rm = T)
qtdEMPREGADOS_empregador_homem21_4tri

qtdEMPREGADOS_cpropria_homem21_4tri <- svytotal(~V4016, subset (pnadc_homemGO21_4tri, VD4007 == "Conta pr?pria"), na.rm = T)
qtdEMPREGADOS_cpropria_homem21_4tri


##### 5 - tinha pelo menos um s?cio que trabalhava nesse neg?cio/empresa#####

#-------Mulher-------

socio_empregador_mulher21_4tri <- svytotal(~V4017, subset (pnadc_mulherGO21_4tri, VD4007 == "Empregador"), na.rm = T)
socio_empregador_mulher21_4tri

socio_cpropria_mulher21_4tri <- svytotal(~V4017, subset (pnadc_mulherGO21_4tri, VD4007 == "Conta pr?pria"), na.rm = T)
socio_cpropria_mulher21_4tri

#-------Homem--------
socio_empregador_homem21_4tri <- svytotal(~V4017, subset (pnadc_homemGO21_4tri, VD4007 == "Empregador"), na.rm = T)
socio_empregador_homem21_4tri

socio_cpropria_homem21_4tri <- svytotal(~V4017, subset (pnadc_homemGO21_4tri, VD4007 == "Conta pr?pria"), na.rm = T)
socio_cpropria_homem21_4tri

##### 6 - Quantos s?cios#####

#-------Mulher-------

qtdsocio_empregador_mulher21_4tri <- svytotal(~V40171, subset (pnadc_mulherGO21_4tri, VD4007 == "Empregador"), na.rm = T)
qtdsocio_empregador_mulher21_4tri

qtdsocio_cpropria_mulher21_4tri <- svytotal(~V40171, subset (pnadc_mulherGO21_4tri, VD4007 == "Conta pr?pria"), na.rm = T)
qtdsocio_cpropria_mulher21_4tri

#-------Homem--------
qtdsocio_empregador_homem21_4tri <- svytotal(~V40171, subset (pnadc_homemGO21_4tri, VD4007 == "Empregador"), na.rm = T)
qtdsocio_empregador_homem21_4tri

qtdsocio_cpropria_homem21_4tri <- svytotal(~V40171, subset (pnadc_homemGO21_4tri, VD4007 == "Conta pr?pria"), na.rm = T)
qtdsocio_cpropria_homem21_4tri


##### 7 - fazia quanto tempo que ... estava nesse trabalho#######

#-------Mulher-------

tempotrabalho_empregador_mulher21_4tri <- svytotal(~V4040, subset (pnadc_mulherGO21_4tri, VD4007 == "Empregador"), na.rm = T)
tempotrabalho_empregador_mulher21_4tri

tempotrabalho_cpropria_mulher21_4tri <- svytotal(~V4040, subset (pnadc_mulherGO21_4tri, VD4007 == "Conta pr?pria"), na.rm = T)
tempotrabalho_cpropria_mulher21_4tri

#-------Homem--------
tempotrabalho_empregador_homem21_4tri <- svytotal(~V4040, subset (pnadc_homemGO21_4tri, VD4007 == "Empregador"), na.rm = T)
tempotrabalho_empregador_homem21_4tri

tempotrabalho_cpropria_homem21_4tri <- svytotal(~V4040, subset (pnadc_homemGO21_4tri, VD4007 == "Conta pr?pria"), na.rm = T)
tempotrabalho_cpropria_homem21_4tri


#### 8 - Qual foi a principal provid?ncia que ... tomou para conseguir trabalho#####

#-------Mulher-------

PROVtrabalho_empregador_mulher21_4tri <- svytotal(~V4072, subset (pnadc_mulherGO21_4tri, VD4007 == "Empregador"), na.rm = T)
PROVtrabalho_empregador_mulher21_4tri

PROVtrabalho_cpropria_mulher21_4tri <- svytotal(~V4072, subset (pnadc_mulherGO21_4tri, VD4007 == "Conta pr?pria"), na.rm = T)
PROVtrabalho_cpropria_mulher21_4tri

#-------Homem--------
PROVtrabalho_empregador_homem21_4tri <- svytotal(~V4072, subset (pnadc_homemGO21_4tri, VD4007 == "Empregador"), na.rm = T)
PROVtrabalho_empregador_homem21_4tri

PROVtrabalho_cpropria_mulher21_4tri <- svytotal(~V4072, subset (pnadc_homemGO21_4tri, VD4007 == "Conta pr?pria"), na.rm = T)
PROVtrabalho_cpropria_mulher21_4tri


########
local_travalho <- (~ interaction (V4022,V2010), subset (pnadc_mulherGO21_4tri, VD4007 == "Conta pr?pria"), na.rm = T)



