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
                                 "VD4036", "VD4017", "V1023","VD4031","VD4032")

dados_pnadc21 <- get_pnadc(year=2021, quarter=1, vars=variaveis_selecionadas_PNADc)

dados_pnadc21_2tri <- get_pnadc(year=2021, quarter=2, vars=variaveis_selecionadas_PNADc)

dados_pnadc21_3tri <- get_pnadc(year=2021, quarter=3, vars=variaveis_selecionadas_PNADc)

dados_pnadc21_4tri <- get_pnadc(year=2021, quarter=4, vars=variaveis_selecionadas_PNADc)

class(dados_pnadc21)


#Estimativas do QUARTO TRIMESTRE DE 2020 - Goias----

pnadc_goias21_4tri <- subset(dados_pnadc21_4tri, UF == "Goiás")
pnadc_goias21_4tri

pnadc_mulherGO21_4tri <- subset(pnadc_goias21_4tri , V2007 == "Mulher")
pnadc_mulherGO21_4tri

pnadc_homemGO21_4tri <- subset(pnadc_goias21_4tri , V2007 == "Homem")
pnadc_homemGO21_4tri


###########################################################################

# Total de trabalhadores na Melhor idade em Goias (divisao por sexo)----

#-------- Mulher-----------


idade_M_empregado21_4tri <- svytotal(~VD4007, subset(pnadc_mulherGO21_4tri, V2009 >=60 ), na.rm = T)
idade_M_empregado21_4tri


#----------Homem---------

idade_H_empregado21_4tri <- svytotal(~VD4007, subset(pnadc_homemGO21_4tri, V2009 >=60 , na.rm = T)
idade_H_empregado21_4tri

#-----------------------------------
# Total de trabalhadores por tipo de area em Goias (divisao por sexo)----

trab_tipoarea_Mulher21_melhor_id_4tri <- svytotal(~interaction (VD4007, V1023), subset(pnadc_homemGO21_4tri, V2009 >=60), na.rm = T)
trab_tipoarea_Mulher21_melhor_id_4tri # (total de mulheres trabalhadoras por area em Goias)

trab_tipoarea_Homem21_melhor_id_4tri <- svytotal(~interaction (VD4007, V1023), subset(pnadc_homemGO21_4tri, V2009 >=60), na.rm = T)
trab_tipoarea_Homem21_melhor_id_4tri # (total de homens trabalhadores por area em Goias)

#---------------------------------------------------------

# Média de renda de trabalhadores na Melhor idade em Goias (divisao por sexo)----

#---------Mulher----------

mediarenda_M_empregado21_melhor_id_4tri <- svymean(~VD4017, subset(pnadc_mulherGO21_4tri, VD4007 == "Empregado (inclusive trabalhador doméstico)" & V2009 >=60 ), na.rm = T)
mediarenda_M_empregado21_melhor_id_4tri

mediarenda_M_empregador21_melhor_id_4tri <- svymean(~VD4017, subset(pnadc_mulherGO21_4tri, VD4007 == "Empregador" & V2009 >=60 ), na.rm = T)
mediarenda_M_empregador21_melhor_id_4tri

mediarenda_M_cpropria21_melhor_id_4tri <- svymean(~VD4017, subset(pnadc_mulherGO21_4tri, VD4007 == "Conta própria" & V2009 >=60 ), na.rm = T)
mediarenda_M_cpropria21_melhor_id_4tri

#--------Homem-----------

mediarenda_H_empregado21_melhor_id_4tri <- svymean(~VD4017, subset(pnadc_homemGO21_4tri, VD4007 == "Empregado (inclusive trabalhador doméstico)" & V2009 >=60), na.rm = T)
mediarenda_H_empregado21_melhor_id_4tri

mediarenda_H_Empregador21_melhor_id_4tri <- svymean(~VD4017, subset(pnadc_homemGO21_4tri, VD4007 == "Empregador" & V2009 >=60), na.rm = T)
mediarenda_H_Empregador21_melhor_id_4tri

mediarenda_H_cpropria21_melhor_id_4tri <- svymean(~VD4017, subset(pnadc_homemGO21_4tri, VD4007 == "Conta própria" & V2009 >=60), na.rm = T)
mediarenda_H_cpropria21_melhor_id_4tri

#----------------------------------------------------------------

# Total de trabalhadores da melhor idade - Raça (divisao por sexo)----

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

