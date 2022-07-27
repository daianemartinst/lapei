# Limpando arquivos armazenados na memoria
rm(list=ls(all=TRUE))

#install.packages("PNADcIBGE")
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

variaveis_selecionadas_PNADc <-c("VD2002", "VD2003", "V2007", "V2009", 
                                 "V2010", "VD3004", "VD4007", "VD4010", "UF", 
                                 "V4013", "V4016", "V4019", "V4020", "VD4016", 
                                 "VD4036", "VD4017", "V1023","VD4031","VD4032","V4040", "V4022")

dados_pnadc21 <- get_pnadc(year=2021, quarter=1, vars=variaveis_selecionadas_PNADc)

dados_pnadc21_2tri <- get_pnadc(year=2021, quarter=2, vars=variaveis_selecionadas_PNADc)

dados_pnadc21_3tri <- get_pnadc(year=2021, quarter=3, vars=variaveis_selecionadas_PNADc)

dados_pnadc21_4tri <- get_pnadc(year=2021, quarter=4, vars=variaveis_selecionadas_PNADc)

class(dados_pnadc21_4tri)


#Estimativas do QUARTO TRIMESTRE DE 2020 - Goias----

pnadc_goias21_4tri <- subset(dados_pnadc21_4tri, UF == "Goi·s")
pnadc_goias21_4tri

pnadc_goias21_4tri <- subset(dados_pnadc21_4tri, V2009 >=60)
pnadc_goias21_4tri

pnadc_mulherGO21_4tri <- subset(pnadc_goias21_4tri , V2007 == "Mulher")
pnadc_mulherGO21_4tri

pnadc_homemGO21_4tri <- subset(pnadc_goias21_4tri , V2007 == "Homem")
pnadc_homemGO21_4tri


###########################################################################

# Total de trabalhadores na Melhor idade em Goias (divisao por sexo)----

#-------- Mulher-----------


idade_M_21_4tri <- svytotal(~VD4007, subset(pnadc_mulherGO21_4tri, V2009 >=60 ), na.rm = T)
idade_M_21_4tri


#----------Homem---------

idade_H_21_4tri <- svytotal(~VD4007, subset(pnadc_homemGO21_4tri, V2009 >=60), na.rm = T)
idade_H_21_4tri

#-----------------------------------
# Total de trabalhadores por tipo de area em Goias (divisao por sexo)----

trab_tipoarea_Mulher21_melhor_id_4tri <- svytotal(~interaction (VD4007, V1023), subset(pnadc_mulherGO21_4tri, V2009 >=60), na.rm = T)
trab_tipoarea_Mulher21_melhor_id_4tri # (total de mulheres trabalhadoras por area em Goias)

trab_tipoarea_Homem21_melhor_id_4tri <- svytotal(~interaction (VD4007, V1023), subset(pnadc_homemGO21_4tri, V2009 >=60), na.rm = T)
trab_tipoarea_Homem21_melhor_id_4tri # (total de homens trabalhadores por area em Goias)

#---------------------------------------------------------

# M√©dia de renda de trabalhadores na Melhor idade em Goias (divisao por sexo)----

#---------Mulher----------

mediarenda_M_empregado21_melhor_id_4tri <- svymean(~VD4017, subset(pnadc_mulherGO21_4tri, VD4007 == "Empregado (inclusive trabalhador dom√©stico)" & V2009 >=60 ), na.rm = T)
mediarenda_M_empregado21_melhor_id_4tri

mediarenda_M_empregador21_melhor_id_4tri <- svymean(~VD4017, subset(pnadc_mulherGO21_4tri, VD4007 == "Empregador" & V2009 >=60 ), na.rm = T)
mediarenda_M_empregador21_melhor_id_4tri

mediarenda_M_cpropria21_melhor_id_4tri <- svymean(~VD4017, subset(pnadc_mulherGO21_4tri, VD4007 == "Conta pr√≥pria" & V2009 >=60 ), na.rm = T)
mediarenda_M_cpropria21_melhor_id_4tri

#--------Homem-----------

mediarenda_H_empregado21_melhor_id_4tri <- svymean(~VD4017, subset(pnadc_homemGO21_4tri, VD4007 == "Empregado (inclusive trabalhador dom√©stico)" & V2009 >=60), na.rm = T)
mediarenda_H_empregado21_melhor_id_4tri

mediarenda_H_Empregador21_melhor_id_4tri <- svymean(~VD4017, subset(pnadc_homemGO21_4tri, VD4007 == "Empregador" & V2009 >=60), na.rm = T)
mediarenda_H_Empregador21_melhor_id_4tri

mediarenda_H_cpropria21_melhor_id_4tri <- svymean(~VD4017, subset(pnadc_homemGO21_4tri, VD4007 == "Conta pr√≥pria" & V2009 >=60), na.rm = T)
mediarenda_H_cpropria21_melhor_id_4tri

#----------------------------------------------------------------

# Total de trabalhadores da melhor idade - Ra√ßa (divisao por sexo)----

totalraca_M_21_melhor_id_4tri <- svytotal(~ interaction(VD4007, V2010), subset(pnadc_mulherGO21_4tri, V2009 >=60 ), na.rm = T)
totalraca_M_21_melhor_id_4tri

totalraca_H_21_melhor_id_4tri <- svytotal(~ interaction(VD4007, V2010), subset(pnadc_homemGO21_4tri, V2009 >=60), na.rm = T)
totalraca_H_21_melhor_id_4tri

#---------------------------------

# Total de trabalhadores da melhor idade  - Escolaridade (divisao por sexo)----

escolaridade_M_21_melhor_id_4tri <- svytotal(~interaction(VD4007, VD3004), subset(pnadc_homemGO21_4tri, V2009 >=60), na.rm = T)
escolaridade_M_21_melhor_id_4tri

escolaridade_H_21_melhor_id_4tri <- svytotal(~interaction(VD4007, VD3004), subset(pnadc_homemGO21_4tri, V2009 >=60), na.rm = T)
escolaridade_H_21_melhor_id_4tri

#----------------------------------------


#######################


##### 1 - Onde exercia normalmente esse trabalho#####

#-------Mulher-------

local_trabalho_empregado_mulher21_4tri <- svytotal(~V4022, subset (pnadc_mulherGO21_4tri, VD4007 == "Empregado (inclusive trabalhador domÈstico)"), na.rm = T)
local_trabalho_empregado_mulher21_4tri

local_trabalho_empregador_mulher21_4tri <- svytotal(~V4022, subset (pnadc_mulherGO21_4tri, VD4007 == "Empregador"), na.rm = T)
local_trabalho_empregador_mulher21_4tri

local_trabalho_cpropria_mulher21_4tri <- svytotal(~V4022, subset (pnadc_mulherGO21_4tri, VD4007 == "Conta prÛpria"), na.rm = T)
local_trabalho_cpropria_mulher21_4tri

#-------Homem--------

localtrabalho_empregado_homem21_4tri <- svytotal(~V4022, subset (pnadc_homemGO21_4tri, VD4007 == "Empregado (inclusive trabalhador domÈstico)"), na.rm = T)
localtrabalho_empregado_homem21_4tri

localtrabalho_empregador_homem21_4tri <- svytotal(~V4022, subset (pnadc_homemGO21_4tri, VD4007 == "Empregador"), na.rm = T)
localtrabalho_empregador_homem21_4tri

localtrabalho_cpropria_homem21_4tri <- svytotal(~V4022, subset (pnadc_homemGO21_4tri, VD4007 == "Conta prÛpria"), na.rm = T)
localtrabalho_cpropria_homem21_4tri


####### 2 - Esse negÛcio era registrado no CNPJ######

#-------Mulher-------

CNPJ_empregador_mulher21_4tri <- svytotal(~V4019, subset (pnadc_mulherGO21_4tri, VD4007 == "Empregador"), na.rm = T)
CNPJ_empregador_mulher21_4tri

CNPJ_cpropria_mulher21_4tri <- svytotal(~V4019, subset (pnadc_mulherGO21_4tri, VD4007 == "Conta prÛpria"), na.rm = T)
CNPJ_cpropria_mulher21_4tri

#-------Homem--------

CNPJ_empregador_homem21_4tri <- svytotal(~V4019, subset (pnadc_homemGO21_4tri, VD4007 == "Empregador"), na.rm = T)
CNPJ_empregador_homem21_4tri

CNPJ_empregador_homem21_4tri <- svytotal(~V4019, subset (pnadc_homemGO21_4tri, VD4007 == "Conta prÛpria"), na.rm = T)
CNPJ_empregador_homem21_4tri


###### 3 - CÛdigo da principal atividade desse negÛcio/empresa#######

#-------Mulher-------



#-------Homem--------


####### 4 - Quantos empregados trabalhavam nesse negÛcio##########

#-------Mulher-------

qtdEMPREGADOS_empregador_mulher21_4tri <- svytotal(~V4016, subset (pnadc_mulherGO21_4tri, VD4007 == "Empregador"), na.rm = T)
qtdEMPREGADOS_empregador_mulher21_4tri

qtdEMPREGADOS_cpropria_mulher21_4tri <- svytotal(~V4016, subset (pnadc_mulherGO21_4tri, VD4007 == "Conta prÛpria"), na.rm = T)
qtdEMPREGADOS_cpropria_mulher21_4tri

#-------Homem--------
qtdEMPREGADOS_empregador_homem21_4tri <- svytotal(~V4016, subset (pnadc_homemGO21_4tri, VD4007 == "Empregador"), na.rm = T)
qtdEMPREGADOS_empregador_homem21_4tri

qtdEMPREGADOS_cpropria_homem21_4tri <- svytotal(~V4016, subset (pnadc_homemGO21_4tri, VD4007 == "Conta prÛpria"), na.rm = T)
qtdEMPREGADOS_cpropria_homem21_4tri


##### 5 - tinha pelo menos um sÛcio que trabalhava nesse negÛcio/empresa#####

#-------Mulher-------

socio_empregador_mulher21_4tri <- svytotal(~V4017, subset (pnadc_mulherGO21_4tri, VD4007 == "Empregador"), na.rm = T)
socio_empregador_mulher21_4tri

socio_cpropria_mulher21_4tri <- svytotal(~V4017, subset (pnadc_mulherGO21_4tri, VD4007 == "Conta prÛpria"), na.rm = T)
socio_cpropria_mulher21_4tri

#-------Homem--------
socio_empregador_homem21_4tri <- svytotal(~V4017, subset (pnadc_homemGO21_4tri, VD4007 == "Empregador"), na.rm = T)
socio_empregador_homem21_4tri

socio_cpropria_homem21_4tri <- svytotal(~V4017, subset (pnadc_homemGO21_4tri, VD4007 == "Conta prÛpria"), na.rm = T)
socio_cpropria_homem21_4tri

##### 6 - Quantos sÛcios#####

#-------Mulher-------

qtdsocio_empregador_mulher21_4tri <- svytotal(~V40171, subset (pnadc_mulherGO21_4tri, VD4007 == "Empregador"), na.rm = T)
qtdsocio_empregador_mulher21_4tri

qtdsocio_cpropria_mulher21_4tri <- svytotal(~V40171, subset (pnadc_mulherGO21_4tri, VD4007 == "Conta prÛpria"), na.rm = T)
qtdsocio_cpropria_mulher21_4tri

#-------Homem--------
qtdsocio_empregador_homem21_4tri <- svytotal(~V40171, subset (pnadc_homemGO21_4tri, VD4007 == "Empregador"), na.rm = T)
qtdsocio_empregador_homem21_4tri

qtdsocio_cpropria_homem21_4tri <- svytotal(~V40171, subset (pnadc_homemGO21_4tri, VD4007 == "Conta prÛpria"), na.rm = T)
qtdsocio_cpropria_homem21_4tri


##### 7 - fazia quanto tempo que ... estava nesse trabalho#######

#-------Mulher-------

tempotrabalho_empregador_mulher21_4tri <- svytotal(~V4040, subset (pnadc_mulherGO21_4tri, VD4007 == "Empregador"), na.rm = T)
tempotrabalho_empregador_mulher21_4tri

tempotrabalho_cpropria_mulher21_4tri <- svytotal(~V4040, subset (pnadc_mulherGO21_4tri, VD4007 == "Conta prÛpria"), na.rm = T)
tempotrabalho_cpropria_mulher21_4tri

#-------Homem--------
tempotrabalho_empregador_homem21_4tri <- svytotal(~V4040, subset (pnadc_homemGO21_4tri, VD4007 == "Empregador"), na.rm = T)
tempotrabalho_empregador_homem21_4tri

tempotrabalho_cpropria_homem21_4tri <- svytotal(~V4040, subset (pnadc_homemGO21_4tri, VD4007 == "Conta prÛpria"), na.rm = T)
tempotrabalho_cpropria_homem21_4tri


#### 8 - Qual foi a principal providÍncia que ... tomou para conseguir trabalho#####

#-------Mulher-------

PROVtrabalho_empregador_mulher21_4tri <- svytotal(~V4072, subset (pnadc_mulherGO21_4tri, VD4007 == "Empregador"), na.rm = T)
PROVtrabalho_empregador_mulher21_4tri

PROVtrabalho_cpropria_mulher21_4tri <- svytotal(~V4072, subset (pnadc_mulherGO21_4tri, VD4007 == "Conta prÛpria"), na.rm = T)
PROVtrabalho_cpropria_mulher21_4tri

#-------Homem--------
PROVtrabalho_empregador_homem21_4tri <- svytotal(~V4072, subset (pnadc_homemGO21_4tri, VD4007 == "Empregador"), na.rm = T)
PROVtrabalho_empregador_homem21_4tri

PROVtrabalho_cpropria_mulher21_4tri <- svytotal(~V4072, subset (pnadc_homemGO21_4tri, VD4007 == "Conta prÛpria"), na.rm = T)
PROVtrabalho_cpropria_mulher21_4tri
