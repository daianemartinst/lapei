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

dados_pnadc20 <- get_pnadc(year=2020, quarter=1, vars=variaveis_selecionadas_PNADc)

dados_pnadc20_2tri <- get_pnadc(year=2020, quarter=2, vars=variaveis_selecionadas_PNADc)

dados_pnadc20_3tri <- get_pnadc(year=2020, quarter=3, vars=variaveis_selecionadas_PNADc)

dados_pnadc20_4tri <- get_pnadc(year=2020, quarter=4, vars=variaveis_selecionadas_PNADc)

class(dados_pnadc20)

#-----------------------------------

#Estimativas do QUARTO TRIMESTRE DE 2020 - Goias----

pnadc_goias20_4tri <- subset(dados_pnadc20_4tri, UF == "Goiás")
pnadc_goias20_4tri

pnadc_mulherGO20_4tri <- subset(pnadc_goias20_4tri , V2007 == "Mulher")
pnadc_mulherGO20_4tri

pnadc_homemGO20_4tri <- subset(pnadc_goias20_4tri , V2007 == "Homem")
pnadc_homemGO20_4tri

# Total de trabalhadores por sexo em Goias----

totalsexo_empregado20_4tri <- svytotal(~V2007, subset(pnadc_goias20_4tri, VD4007 == "Empregado (inclusive trabalhador doméstico)"), na.rm = T)
totalsexo_empregado20_4tri # (total de empregado)

totalsexo_Empregador20_4tri <- svytotal(~V2007, subset(pnadc_goias20_4tri, VD4007 == "Empregador"), na.rm = T)
totalsexo_Empregador20_4tri # (total de empregador)

totalsexo_cpropria20_4tri <- svytotal(~V2007, subset(pnadc_goias20_4tri, VD4007 == "Conta própria"), na.rm = T)
totalsexo_cpropria20_4tri  # (total por conta propria)

total_trabalhador <- svytotal(~VD4007, pnadc_goias20_4tri, na.rm = T)

proporcao_totalsexo_empregado20_4tri <- svymean(~V2007, subset(pnadc_goias20_4tri, VD4007 == "Empregado (inclusive trabalhador dom?stico)"), na.rm = T)
proporcao_totalsexo_empregado20_4tri

# Total de trabalhadores por tipo de area em Goias (divis?o por sexo)----

trab_tipoarea_Mulher20_4tri <- svytotal(~interaction (VD4007, V1023), pnadc_mulherGO20_4tri, na.rm = T)
trab_tipoarea_Mulher20_4tri # (total de mulheres trabalhadoras por area em Goias)

trab_tipoarea_Homem20_4tri <- svytotal(~interaction (VD4007, V1023), pnadc_homemGO20_4tri, na.rm = T)
trab_tipoarea_Homem20_4tri # (total de homens trabalhadores por area em Goias)

# Media Renda dos trabalhadores em Goias (divis?o por sexo)----

#---------Mulher----------
mediarenda_M_empregado20_4tri <- svymean(~VD4017, subset(pnadc_mulherGO20_4tri, VD4007 == "Empregado (inclusive trabalhador dom?stico)"), na.rm = T)
mediarenda_M_empregado20_4tri

mediarenda_M_Empregador20_4tri <- svymean(~VD4017, subset(pnadc_mulherGO20_4tri, VD4007 == "Empregador"), na.rm = T)
mediarenda_M_Empregador20_4tri

mediarenda_M_cpropria20_4tri <- svymean(~VD4017, subset(pnadc_mulherGO20_4tri, VD4007 == "Conta pr?pria"), na.rm = T)
mediarenda_M_cpropria20_4tri

#--------Homem-----------

mediarenda_H_empregado20_4tri <- svymean(~VD4017, subset(pnadc_homemGO20_4tri, VD4007 == "Empregado (inclusive trabalhador dom?stico)"), na.rm = T)
mediarenda_H_empregado20_4tri

mediarenda_H_Empregador20_4tri <- svymean(~VD4017, subset(pnadc_homemGO20_4tri, VD4007 == "Empregador"), na.rm = T)
mediarenda_H_Empregador20_4tri

mediarenda_H_cpropria20_4tri <- svymean(~VD4017, subset(pnadc_homemGO20_4tri, VD4007 == "Conta pr?pria"), na.rm = T)
mediarenda_H_cpropria20_4tri

# Mediana Renda dos trabalhadores em Goias (divis?o por sexo)----

#---------Mulher----------

medianarenda_M_empregado20_4tri <- svyquantile(~VD4017, subset(pnadc_mulherGO20_4tri, VD4007 == "Empregado (inclusive trabalhador dom?stico)"), quantiles = .5, na.rm = T)
medianarenda_M_empregado20_4tri

medianarenda_M_Empregador20_4tri <- svyquantile(~VD4017, subset(pnadc_mulherGO20_4tri, VD4007 == "Empregador"), quantiles = .5, na.rm = T)
medianarenda_M_Empregador20_4tri

medianarenda_M_cpropria20_4tri <- svyquantile(~VD4017, subset(pnadc_mulherGO20_4tri, VD4007 == "Conta pr?pria"), quantiles = .5, na.rm = T)
medianarenda_M_cpropria20_4tri 

#--------Homem------------

medianarenda_H_empregado20_4tri <- svyquantile(~VD4017, subset(pnadc_homemGO20_4tri, VD4007 == "Empregado (inclusive trabalhador dom?stico)"), quantiles = .5, na.rm = T)
medianarenda_H_empregado20_4tri

medianarenda_H_Empregador20_4tri <- svyquantile(~VD4017, subset(pnadc_homemGO20_4tri, VD4007 == "Empregador"), quantiles = .5, na.rm = T)
medianarenda_H_Empregador20_4tri

medianarenda_H_cpropria20_4tri <- svyquantile(~VD4017, subset(pnadc_homemGO20_4tri, VD4007 == "Conta pr?pria"), quantiles = .5, na.rm = T)
medianarenda_H_cpropria20_4tri

# Total de trabalhadores - Ra?a (divis?o por sexo)----

totalraca_M_20_4tri <- svytotal(~ interaction(VD4007, V2010), pnadc_mulherGO20_4tri, na.rm = T)
totalraca_M_20_4tri

totalraca_H_20_4tri <- svytotal(~ interaction(VD4007, V2010), pnadc_homemGO20_4tri, na.rm = T)
totalraca_H_20_4tri

# Total de trabalhadores - Escolaridade (divis?o por sexo)----

escolaridade_M_20_4tri <- svytotal(~interaction(VD4007, VD3004), pnadc_mulherGO20_4tri, na.rm = T)
escolaridade_M_20_4tri

escolaridade_H_20_4tri <- svytotal(~interaction(VD4007, VD3004), pnadc_homemGO20_4tri, na.rm = T)
escolaridade_H_20_4tri

# Media de idade dos trabalhadores em Goias (divis?o por sexo)----

#-------- Mulher-----------

idade_M_empregado20_4tri <- svymean(~V2009, subset(pnadc_mulherGO20_4tri, VD4007 == "Empregado (inclusive trabalhador dom?stico)"), na.rm = T)
idade_M_empregado20_4tri

idade_M_Empregador20_4tri <- svymean(~V2009, subset(pnadc_mulherGO20_4tri, VD4007 == "Empregador"), na.rm = T)
idade_M_Empregador20_4tri

idade_M_cpropria20_4tri <- svymean(~V2009, subset(pnadc_mulherGO20_4tri, VD4007 == "Conta pr?pria"), na.rm = T)
idade_M_cpropria20_4tri

#----------Homem---------

idade_H_empregado20_4tri <- svymean(~V2009, subset(pnadc_homemGO20_4tri, VD4007 == "Empregado (inclusive trabalhador dom?stico)"), na.rm = T)
idade_H_empregado20_4tri

idade_H_Empregador20_4tri <- svymean(~V2009, subset(pnadc_homemGO20_4tri, VD4007 == "Empregador"), na.rm = T)
idade_H_Empregador20_4tri

idade_H_cpropria20_4tri <- svymean(~V2009, subset(pnadc_homemGO20_4tri, VD4007 == "Conta pr?pria"), na.rm = T)
idade_H_cpropria20_4tri

# Mediana idade dos trabalhadores em Goias (divis?o por sexo) ----

#--------Mulher-------

medianaidade_M_empregado20_4tri <- svyquantile(~V2009, subset(pnadc_mulherGO20_4tri, VD4007 == "Empregado (inclusive trabalhador dom?stico)"), quantiles = .5, na.rm = T)
medianaidade_M_empregado20_4tri

medianaidade_M_Empregador20_4tri<- svyquantile(~V2009, subset(pnadc_mulherGO20_4tri, VD4007 == "Empregador"), quantiles = .5, na.rm = T)
medianaidade_M_Empregador20_4tri

medianaidade_M_cpropria20_4tri <- svyquantile(~V2009, subset(pnadc_mulherGO20_4tri, VD4007 == "Conta pr?pria"), quantiles = .5, na.rm = T)
medianaidade_M_cpropria20_4tri

#-------Homem---------

medianaidade_H_empregado20_4tri <- svyquantile(~V2009, subset(pnadc_homemGO20_4tri, VD4007 == "Empregado (inclusive trabalhador dom?stico)"), quantiles = .5, na.rm = T)
medianaidade_H_empregado20_4tri

medianaidade_H_Empregador20_4tri <- svyquantile(~V2009, subset(pnadc_homemGO20_4tri, VD4007 == "Empregador"), quantiles = .5, na.rm = T)
medianaidade_H_Empregador20_4tri

medianaidade_H_cpropria20_4tri <- svyquantile(~V2009, subset(pnadc_homemGO20_4tri, VD4007 == "Conta pr?pria"), quantiles = .5, na.rm = T)
medianaidade_H_cpropria20_4tri

# Total de horas trabalhadas - (divis?o por sexo) ----

#-------Mulher-------

horasT_M_empregado20_4tri <- svytotal(~VD4036, subset (pnadc_mulherGO20_4tri, VD4007 == "Empregado (inclusive trabalhador dom?stico)"), na.rm = T)
horasT_M_empregado20_4tri

horasT_M_Empregador20_4tri <- svytotal(~VD4036, subset (pnadc_mulherGO20_4tri, VD4007 == "Empregador"), na.rm = T)
horasT_M_Empregador20_4tri

horasT_M_cpropria20_4tri <- svytotal(~VD4036, subset (pnadc_mulherGO20_4tri, VD4007 == "Conta pr?pria"), na.rm = T)
horasT_M_cpropria20_4tri

#-------Homem--------

horasT_H_empregado20_4tri <- svytotal(~VD4036, subset (pnadc_homemGO20_4tri, VD4007 == "Empregado (inclusive trabalhador dom?stico)"), na.rm = T)
horasT_H_empregado20_4tri

horasT_H_Empregador20_4tri <- svytotal(~VD4036, subset (pnadc_homemGO20_4tri, VD4007 == "Empregador"), na.rm = T)
horasT_H_Empregador20_4tri

horasT_H_cpropria20_4tri <- svytotal(~VD4036, subset (pnadc_homemGO20_4tri, VD4007 == "Conta pr?pria"), na.rm = T)
horasT_H_cpropria20_4tri 

# Total de horas efetivamente trabalhadas - (divisao por sexo)


#-------Mulher-------

horasEfetivamenteT_M_empregado20_4tri <- svytotal(~VD4032, subset (pnadc_mulherGO20_4tri, VD4007 == "Empregado (inclusive trabalhador doméstico)"), na.rm = T)
horasEfetivamenteT_M_empregado20_4tri

horasEfetivamenteT_M_Empregador20_4tri <- svytotal(~VD4032, subset (pnadc_mulherGO20_4tri, VD4007 == "Empregador"), na.rm = T)
horasEfetivamenteT_M_Empregador20_4tri

horasEfetivamenteT_M_cpropria20_4tri <- svytotal(~VD4032, subset (pnadc_mulherGO20_4tri, VD4007 == "Conta própria"), na.rm = T)
horasEfetivamenteT_M_cpropria20_4tri

#-------Homem--------

horasEfetivamenteT_H_empregado20_4tri <- svytotal(~VD4032, subset (pnadc_homemGO20_4tri, VD4007 == "Empregado (inclusive trabalhador doméstico)"), na.rm = T)
horasEfetivamenteT_H_empregado20_4tri

horasEfetivamenteT_H_Empregador20_4tri <- svytotal(~VD4032, subset (pnadc_homemGO20_4tri, VD4007 == "Empregador"), na.rm = T)
horasEfetivamenteT_H_Empregador20_4tri

horasEfetivamenteT_H_cpropria20_4tri <- svytotal(~VD4032, subset (pnadc_homemGO20_4tri, VD4007 == "Conta própria"), na.rm = T)
horasEfetivamenteT_H_cpropria20_4tri

# Media de horas habitualmente trabalhadas - (divisao por sexo)


#-------Mulher-------

horasHabitualmenteT_M_empregado20_4tri <- svymean(~VD4031, subset (pnadc_mulherGO20_4tri, VD4007 == "Empregado (inclusive trabalhador doméstico)"), na.rm = T)
horasHabitualmenteT_M_empregado20_4tri

horasHabitualmenteT_M_Empregador20_4tri <- svymean(~VD4031, subset (pnadc_mulherGO20_4tri, VD4007 == "Empregador"), na.rm = T)
horasHabitualmenteT_M_Empregador20_4tri

horasHabitualmenteT_M_cpropria20_4tri <- svymean(~VD4031, subset (pnadc_mulherGO20_4tri, VD4007 == "Conta própria"), na.rm = T)
horasHabitualmenteT_M_cpropria20_4tri

#-------Homem--------

horasHabitualmenteT_H_empregado20_4tri <- svymean(~VD4031, subset (pnadc_homemGO20_4tri, VD4007 == "Empregado (inclusive trabalhador doméstico)"), na.rm = T)
horasHabitualmenteT_H_empregado20_4tri

horasHabitualmenteT_H_Empregador20_4tri <- svymean(~VD4031, subset (pnadc_homemGO20_4tri, VD4007 == "Empregador"), na.rm = T)
horasHabitualmenteT_H_Empregador20_4tri

horasHabitualmenteT_H_cpropria20_4tri <- svymean(~VD4031, subset (pnadc_homemGO20_4tri, VD4007 == "Conta própria"), na.rm = T)
horasHabitualmenteT_H_cpropria20_4tri
