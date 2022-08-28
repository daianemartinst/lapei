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
library(dplyr)

?get_pnadc

#dados da PNADc de modo automatico

variaveis_selecionadas_PNADc <-c("UF", "V2007", "VD4007", "V1023", 
                                 "V2009", "V2010", "VD3004", "VD4017", "VD4016", 
                                 "V4019", "V4016", "V4017", "V40171", "V4022", 
                                 "V4040", "V4072", "V1023","VD4031","VD4032")

dados_pnadc21 <- get_pnadc(year=2021, quarter=1, vars=variaveis_selecionadas_PNADc)

dados_pnadc21_2tri <- get_pnadc(year=2021, quarter=2, vars=variaveis_selecionadas_PNADc)

dados_pnadc21_3tri <- get_pnadc(year=2021, quarter=3, vars=variaveis_selecionadas_PNADc)

dados_pnadc21_4tri <- get_pnadc(year=2019, quarter=4, vars=variaveis_selecionadas_PNADc)

class(dados_pnadc21_4tri)


# Testedeflator -----------------------------------------------------------

dados_pnadc21_4tri[["variables"]]<- dados_pnadc21_4tri[["variables"]] %>% 
  mutate(deflator = 0.871141365089117) %>% 
  mutate(renda_corrigida = VD4017/deflator)



#-----------------------------------

#Estimativas do QUARTO TRIMESTRE DE 2020 - Goias----

pnadc_goias21_4tri <- subset(dados_pnadc21_4tri, UF == "Goiás")
pnadc_goias21_4tri

pnadc_mulherGO21_4tri <- subset(pnadc_goias21_4tri , V2007 == "Mulher")
pnadc_mulherGO21_4tri

pnadc_homemGO21_4tri <- subset(pnadc_goias21_4tri , V2007 == "Homem")
pnadc_homemGO21_4tri

# Total de trabalhadores por sexo em Goias----

totalsexo_empregado21_4tri <- svytotal(~V2007, subset(pnadc_goias21_4tri, VD4007 == "Empregado (inclusive trabalhador doméstico)"), na.rm = T)
totalsexo_empregado21_4tri # (total de empregado)

totalsexo_Empregador21_4tri <- svytotal(~V2007, subset(pnadc_goias21_4tri, VD4007 == "Empregador"), na.rm = T)
totalsexo_Empregador21_4tri # (total de empregador)

totalsexo_cpropria21_4tri <- svytotal(~V2007, subset(pnadc_goias21_4tri, VD4007 == "Conta própria"), na.rm = T)
totalsexo_cpropria21_4tri  # (total por conta propria)

total_trabalhador <- svytotal(~VD4007, pnadc_goias21_4tri, na.rm = T)
total_trabalhador

proporcao_totalsexo_empregado21_4tri <- svymean(~V2007, subset(pnadc_goias21_4tri, VD4007 == "Empregado (inclusive trabalhador doméstico)"), na.rm = T)
proporcao_totalsexo_empregado21_4tri

# Total de trabalhadores por tipo de area em Goias (divisao por sexo)----

trab_tipoarea_Mulher21_4tri <- svytotal(~interaction (VD4007, V1023), pnadc_mulherGO21_4tri, na.rm = T)
trab_tipoarea_Mulher21_4tri # (total de mulheres trabalhadoras por area em Goias)

trab_tipoarea_Homem21_4tri <- svytotal(~interaction (VD4007, V1023), pnadc_homemGO21_4tri, na.rm = T)
trab_tipoarea_Homem21_4tri # (total de homens trabalhadores por area em Goias)

# Media Renda dos trabalhadores em Goias (divisao por sexo)----

#---------Mulher----------
mediarenda_M_empregado21_4tri <- svymean(~renda_corrigida, subset(pnadc_mulherGO21_4tri, VD4007 == "Empregado (inclusive trabalhador doméstico)"), na.rm = T)
mediarenda_M_empregado21_4tri

mediarenda_M_Empregador21_4tri <- svymean(~renda_corrigida, subset(pnadc_mulherGO21_4tri, VD4007 == "Empregador"), na.rm = T)
mediarenda_M_Empregador21_4tri

mediarenda_M_cpropria21_4tri <- svymean(~renda_corrigida, subset(pnadc_mulherGO21_4tri, VD4007 == "Conta própria"), na.rm = T)
mediarenda_M_cpropria21_4tri

#--------Homem-----------

mediarenda_H_empregado21_4tri <- svymean(~VD4017, subset(pnadc_homemGO21_4tri, VD4007 == "Empregado (inclusive trabalhador doméstico)"), na.rm = T)
mediarenda_H_empregado21_4tri

mediarenda_H_Empregador21_4tri <- svymean(~VD4017, subset(pnadc_homemGO21_4tri, VD4007 == "Empregador"), na.rm = T)
mediarenda_H_Empregador21_4tri

mediarenda_H_cpropria21_4tri <- svymean(~VD4017, subset(pnadc_homemGO21_4tri, VD4007 == "Conta própria"), na.rm = T)
mediarenda_H_cpropria21_4tri


#--------------------------------------------------------------------
# Mediana Renda dos trabalhadores em Goias (divis?o por sexo)----

#---------Mulher----------

#medianarenda_M_empregado21_4tri <- svyquantile(~VD4017, subset(pnadc_mulherGO21_4tri, VD4007 == "Empregado (inclusive trabalhador doméstico)"), quantiles = .5, na.rm = T)
#medianarenda_M_empregado21_4tri

#medianarenda_M_Empregador21_4tri <- svyquantile(~VD4017, subset(pnadc_mulherGO21_4tri, VD4007 == "Empregador"), quantiles = .5, na.rm = T)
#medianarenda_M_Empregador21_4tri

#medianarenda_M_cpropria21_4tri <- svyquantile(~VD4017, subset(pnadc_mulherGO21_4tri, VD4007 == "Conta própria"), quantiles = .5, na.rm = T)
#medianarenda_M_cpropria21_4tri 

#--------Homem------------

#medianarenda_H_empregado21_4tri <- svyquantile(~VD4017, subset(pnadc_homemGO21_4tri, VD4007 == "Empregado (inclusive trabalhador doméstico)"), quantiles = .5, na.rm = T)
#medianarenda_H_empregado21_4tri

#medianarenda_H_Empregador21_4tri <- svyquantile(~VD4017, subset(pnadc_homemGO21_4tri, VD4007 == "Empregador"), quantiles = .5, na.rm = T)
#medianarenda_H_Empregador21_4tri

#medianarenda_H_cpropria21_4tri <- svyquantile(~VD4017, subset(pnadc_homemGO21_4tri, VD4007 == "Conta própria"), quantiles = .5, na.rm = T)
#medianarenda_H_cpropria21_4tri

#---------------------------------------------------------------

# Total de trabalhadores - Raça (divisao por sexo)----

totalraca_M_21_4tri <- svytotal(~ interaction(VD4007, V2010), pnadc_mulherGO21_4tri, na.rm = T)
totalraca_M_21_4tri

totalraca_H_21_4tri <- svytotal(~ interaction(VD4007, V2010), pnadc_homemGO21_4tri, na.rm = T)
totalraca_H_21_4tri

# Total de trabalhadores - Escolaridade (divisao por sexo)----

escolaridade_M_21_4tri <- svytotal(~interaction(VD4007, VD3004), pnadc_mulherGO21_4tri, na.rm = T)
escolaridade_M_21_4tri

escolaridade_H_21_4tri <- svytotal(~interaction(VD4007, VD3004), pnadc_homemGO21_4tri, na.rm = T)
escolaridade_H_21_4tri

# Media de idade dos trabalhadores em Goias (divisao por sexo)----

#-------- Mulher-----------

idade_M_empregado21_4tri <- svymean(~V2009, subset(pnadc_mulherGO21_4tri, VD4007 == "Empregado (inclusive trabalhador doméstico)"), na.rm = T)
idade_M_empregado21_4tri

idade_M_Empregador21_4tri <- svymean(~V2009, subset(pnadc_mulherGO21_4tri, VD4007 == "Empregador"), na.rm = T)
idade_M_Empregador21_4tri

idade_M_cpropria21_4tri <- svymean(~V2009, subset(pnadc_mulherGO21_4tri, VD4007 == "Conta própria"), na.rm = T)
idade_M_cpropria21_4tri

#----------Homem---------

idade_H_empregado21_4tri <- svymean(~V2009, subset(pnadc_homemGO21_4tri, VD4007 == "Empregado (inclusive trabalhador doméstico)"), na.rm = T)
idade_H_empregado21_4tri

idade_H_Empregador21_4tri <- svymean(~V2009, subset(pnadc_homemGO21_4tri, VD4007 == "Empregador"), na.rm = T)
idade_H_Empregador21_4tri

idade_H_cpropria21_4tri <- svymean(~V2009, subset(pnadc_homemGO21_4tri, VD4007 == "Conta própria"), na.rm = T)
idade_H_cpropria21_4tri


#---------------------------------------------------------------------------

# Mediana idade dos trabalhadores em Goias (divisao por sexo) ----

#--------Mulher-------

#medianaidade_M_empregado21_4tri <- svyquantile(~V2009, subset(pnadc_mulherGO20_4tri, VD4007 == "Empregado (inclusive trabalhador dom?stico)"), quantiles = .5, na.rm = T)
#medianaidade_M_empregado21_4tri

#medianaidade_M_Empregador21_4tri<- svyquantile(~V2009, subset(pnadc_mulherGO20_4tri, VD4007 == "Empregador"), quantiles = .5, na.rm = T)
#medianaidade_M_Empregador21_4tri

#medianaidade_M_cpropria21_4tri <- svyquantile(~V2009, subset(pnadc_mulherGO20_4tri, VD4007 == "Conta pr?pria"), quantiles = .5, na.rm = T)
#medianaidade_M_cpropria21_4tri

#-------Homem---------

#medianaidade_H_empregado21_4tri <- svyquantile(~V2009, subset(pnadc_homemGO21_4tri, VD4007 == "Empregado (inclusive trabalhador doméstico)"), quantiles = .5, na.rm = T)
#medianaidade_H_empregado21_4tri

#medianaidade_H_Empregador21_4tri <- svyquantile(~V2009, subset(pnadc_homemGO21_4tri, VD4007 == "Empregador"), quantiles = .5, na.rm = T)
#medianaidade_H_Empregador21_4tri

#medianaidade_H_cpropria21_4tri <- svyquantile(~V2009, subset(pnadc_homemGO21_4tri, VD4007 == "Conta própria"), quantiles = .5, na.rm = T)
#medianaidade_H_cpropria21_4tri

#------------------------------------------------------------------

# Total de horas trabalhadas - (divisao por sexo) ----

#-------Mulher-------

horasT_M_empregado21_4tri <- svytotal(~VD4036, subset (pnadc_mulherGO21_4tri, VD4007 == "Empregado (inclusive trabalhador doméstico)"), na.rm = T)
horasT_M_empregado21_4tri

horasT_M_Empregador21_4tri <- svytotal(~VD4036, subset (pnadc_mulherGO21_4tri, VD4007 == "Empregador"), na.rm = T)
horasT_M_Empregador21_4tri

horasT_M_cpropria21_4tri <- svytotal(~VD4036, subset (pnadc_mulherGO21_4tri, VD4007 == "Conta própria"), na.rm = T)
horasT_M_cpropria21_4tri

#-------Homem--------

horasT_H_empregado21_4tri <- svytotal(~VD4036, subset (pnadc_homemGO21_4tri, VD4007 == "Empregado (inclusive trabalhador doméstico)"), na.rm = T)
horasT_H_empregado21_4tri

horasT_H_Empregador21_4tri <- svytotal(~VD4036, subset (pnadc_homemGO21_4tri, VD4007 == "Empregador"), na.rm = T)
horasT_H_Empregador21_4tri

horasT_H_cpropria21_4tri <- svytotal(~VD4036, subset (pnadc_homemGO21_4tri, VD4007 == "Conta própria"), na.rm = T)
horasT_H_cpropria21_4tri 

# Total de horas efetivamente trabalhadas - (divisao por sexo)


#-------Mulher-------

horasEfetivamenteT_M_empregado21_4tri <- svytotal(~VD4032, subset (pnadc_mulherGO21_4tri, VD4007 == "Empregado (inclusive trabalhador doméstico)"), na.rm = T)
horasEfetivamenteT_M_empregado21_4tri

horasEfetivamenteT_M_Empregador21_4tri <- svytotal(~VD4032, subset (pnadc_mulherGO21_4tri, VD4007 == "Empregador"), na.rm = T)
horasEfetivamenteT_M_Empregador21_4tri

horasEfetivamenteT_M_cpropria21_4tri <- svytotal(~VD4032, subset (pnadc_mulherGO21_4tri, VD4007 == "Conta própria"), na.rm = T)
horasEfetivamenteT_M_cpropria21_4tri

#-------Homem--------

horasEfetivamenteT_H_empregado21_4tri <- svytotal(~VD4032, subset (pnadc_homemGO21_4tri, VD4007 == "Empregado (inclusive trabalhador doméstico)"), na.rm = T)
horasEfetivamenteT_H_empregado21_4tri

horasEfetivamenteT_H_Empregador21_4tri <- svytotal(~VD4032, subset (pnadc_homemGO21_4tri, VD4007 == "Empregador"), na.rm = T)
horasEfetivamenteT_H_Empregador21_4tri

horasEfetivamenteT_H_cpropria21_4tri <- svytotal(~VD4032, subset (pnadc_homemGO21_4tri, VD4007 == "Conta própria"), na.rm = T)
horasEfetivamenteT_H_cpropria21_4tri

# Media de horas habitualmente trabalhadas - (divisao por sexo)


#-------Mulher-------

horasHabitualmenteT_M_empregado21_4tri <- svymean(~VD4031, subset (pnadc_mulherGO21_4tri, VD4007 == "Empregado (inclusive trabalhador doméstico)"), na.rm = T)
horasHabitualmenteT_M_empregado21_4tri

horasHabitualmenteT_M_Empregador21_4tri <- svymean(~VD4031, subset (pnadc_mulherGO21_4tri, VD4007 == "Empregador"), na.rm = T)
horasHabitualmenteT_M_Empregador21_4tri

horasHabitualmenteT_M_cpropria21_4tri <- svymean(~VD4031, subset (pnadc_mulherGO21_4tri, VD4007 == "Conta própria"), na.rm = T)
horasHabitualmenteT_M_cpropria21_4tri

#-------Homem--------

horasHabitualmenteT_H_empregado21_4tri <- svymean(~VD4031, subset (pnadc_homemGO21_4tri, VD4007 == "Empregado (inclusive trabalhador doméstico)"), na.rm = T)
horasHabitualmenteT_H_empregado21_4tri

horasHabitualmenteT_H_Empregador21_4tri <- svymean(~VD4031, subset (pnadc_homemGO21_4tri, VD4007 == "Empregador"), na.rm = T)
horasHabitualmenteT_H_Empregador21_4tri

horasHabitualmenteT_H_cpropria21_4tri <- svymean(~VD4031, subset (pnadc_homemGO21_4tri, VD4007 == "Conta própria"), na.rm = T)
horasHabitualmenteT_H_cpropria21_4tri

#########################################


##### Onde exercia normalmente esse trabalho#####

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


####### Esse neg?cio era registrado no CNPJ######

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


###### C?digo da principal atividade desse neg?cio/empresa#######

#-------Mulher-------



#-------Homem--------


####### Quantos empregados trabalhavam nesse neg?cio##########

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


##### tinha pelo menos um s?cio que trabalhava nesse neg?cio/empresa#####

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

##### Quantos s?cios#####

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


#####  fazia quanto tempo que ... estava nesse trabalho#######

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


####  Qual foi a principal provid?ncia que ... tomou para conseguir trabalho#####

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
