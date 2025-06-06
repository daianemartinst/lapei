setwd("C:/Users/LUAN/Desktop/UFG/LAPEI/R studio/PNS e PNADc")
library(tidyverse)
library(devtools)
library(DT)
library(DataEditR)
library(microdatasus)
library(ggplot2)
library(survey)
library(PNSIBGE)

# Definindo opcao de exibicao de numeros sem exponencial
aviso <- getOption("warn")
options(warn=-1)
options(scipen=999)
options(warn=aviso)
rm(aviso)

# Limpando arquivos armazenados na memoria
rm(list=ls(all=TRUE))

# Carregando informacoes do pacote PNSIBGE
if("PNSIBGE" %in% rownames(installed.packages())==FALSE)
{
  install.packages("PNSIBGE", dependencies=TRUE)
}

packageDescription("PNSIBGE")
help(package="PNSIBGE")

# Descrevendo as funcoes do pacote PNSIBGE
# get_pns
# read_pns
# pns_labeller
# pns_deflator
# pns_design
# pns_example

# Carregando microdados da PNS

#Primeira op��o:
dadosPNS_2019 <- get_pns(year=2019, labels=TRUE, deflator=TRUE, design=TRUE)

#Outras op��es:
dadosPNS_2019_Selecionado <- get_pns(year=2019, selected=TRUE, labels=TRUE, deflator=TRUE, design=TRUE)
dadosPNS_2019_Antropometria <- get_pns(year=2019, anthropometry=TRUE, labels=TRUE, deflator=TRUE, design=TRUE)
load("Microdados IBGE - Bases PNSIBGE.RData")
glimpse(dadosPNS_2019$variables)

#Selecionando as vari�veis de interesse:

variaveis_selecionadas <- c("V0001", "V0015", "B001", "C001", "C00301", "C004", "C006", "C008", "C009", "D00901", 
                            "E001", "E002", "E003", "E004", "E005", "E006011", "E008", "E01201", "E01401", "E014011", 
                            "E01402", "E01403", "E01501", "E01601", "E01602", "E017", "E01801", "E01802", "E01805",
                            "E019", "E022", "E023011", "E024011", "I00102", "I005", "I006", "I00401", "I00402", "I00403",
                            "I00404", "J001", "J00101", "J00402", "J00404", "J007", "J00801", "J009", "J01002", "J01101",
                            "J012", "J038", "J041", "J05301", "J057", "N010", "N011", "N012", "N013", "N014", "N015", "N016",
                            "N017", "N018", "Q092", "Q09201", "Q09301", "Q094", "Q09502", "Q09605", "Q09606", "Q10101", 
                            "Q10202", "Q105", "Q10701", "Q109", "Q11006", "Q11007", "Q11008", "Q11009", "Q11010", "Q111",
                            "Q11201", "Q11406", "Q11408", "Q115")

dadospns <- get_pns(year=2019, labels=TRUE, deflator=TRUE, design=TRUE, vars=variaveis_selecionadas)


### AN�LISE DOS DADOS

# AVALIADO O N�MERO DE EMPREENDEDORES COM DEPRESS�O por diagn�stico m�dico (vari�vel: Q092)

depressao <- dadospns$variables %>%
        select(E01401, Q092)

glimpse(depressao)

depressao_empre <- depressao %>%
  filter(E01401 == "Empregador" | E01401 == "Conta pr�pria")

glimpse(depressao_empre)

depressao_empre <- depressao_empre %>%
  group_by(E01401, Q092) %>%
  count()

depressao_empre <- depressao_empre %>%
  group_by(Q092) %>%
  count()

table(depressao_empre$Q092)
prop.table(table(depressao_empre$Q092))

# AVALIADO O N�MERO DE SUJEITOS (COM CARTEIRA ASSINADA) COM DEPRESS�O por diagn�stico m�dico (vari�vel: Q092)

depressao1 <- dadospns$variables %>%
  select(E01403, Q092)

glimpse(depressao1)

depressao_carteira <- depressao1 %>%
  filter(E01403 == "Sim")

glimpse(depressao_carteira)

depressao_carteira <- depressao_carteira %>%
  group_by(E01403, Q092) %>%
  count()

depressao_carteira <- depressao_carteira %>%
  group_by(Q092) %>%
  count()

table(depressao_carteira$Q092)
prop.table(table(depressao_carteira$Q092))

# SINAIS DE DEPRESS�O NOS EMPREENDEDORES

sinais_depressao <- dadospns$variables %>%
  select(E01401, N016)

sinais_depressao_empre <- sinais_depressao %>%
  filter(E01401 == "Empregador" | E01401 == "Conta pr�pria")

table(sinais_depressao_empre$N016)

#aQUELES COM CARTEIRA ASSINADA

sinais_depressao1 <- dadospns$variables %>%
  select(E01403, N016)

sinais_depressao_carteira <- sinais_depressao1 %>%
  filter(E01403 == "Sim")

table(sinais_depressao_carteira$N016)

# VERIFICANDO SE � AO ACASO OS N�MEROS ANTERIORES:

verificacao_empre <- dadospns$variables %>%
  select(E01401, Q092, N016)

verificacao_empre <- verificacao_empre %>%
  filter(E01401 == "Empregador" | E01401 == "Conta pr�pria")

verificacao_empre <- verificacao_empre %>%
  filter(Q092 == "Sim")

table(verificacao_empre$N016)

verificacao_carteira <- dadospns$variables %>%
  select(E01403, Q092, N016)

verificacao_carteira <- verificacao_carteira %>%
  filter(E01403 == "Sim")

verificacao_carteira <- verificacao_carteira %>%
  filter(Q092 == "Sim")

table(verificacao_carteira$N016)






### UTILIZANDO O PACOTE SURVEY ----------------------------------------------

## A n�vel nacional

empreendedor <-  subset(dadospns, E01401 == "Empregador"| E01401 == "Conta pr�pria")
carteira_assinada <- subset(dadospns, E01403 == "Sim")

# ESTIMANDO O N�MERO DE EMPREENDEDORES DO pa�s COM DEPRESS�O 

total_empreendedores <- svytotal(x=~E01401, dadospns, na.rm = T)
total_empreendedores

total_empre_dep <- svytotal(x=~Q092, empreendedor, na.rm = T)
total_empre_dep
confint(total_empre_dep)

proporcao_total_empre_dep <- svymean(x=~Q092, empreendedor, na.rm = T)
proporcao_total_empre_dep
confint(proporcao_total_empre_dep)

# ESTIMANDO O N�MERO DE sujeitos com carteira assinada DO pa�s COM DEPRESS�O

total_carteira <- svytotal(x=~E01403, dadospns, na.rm = T)
total_carteira

total_carteira_dep <- svytotal(x=~Q092, carteira_assinada, na.rm = T)
total_carteira_dep 

proporcao_total_carteira_dep <- svymean(x=~Q092, carteira_assinada, na.rm = T)
proporcao_total_carteira_dep
confint(proporcao_total_carteira_dep)

# Verificando a percep��o de sa�de do empreendedor  quanto a quest�es relacionadas � depress�o (N016)

ep1 <- svytotal(x=~N016, empreendedor, na.rm = T)
ep1

prop_ep1 <- svymean(x=~N016, empreendedor, na.rm = T)
prop_ep1
confint(prop_ep1)

# Agora, para os sujeitos de carteira assinada 

ct1 <- svytotal(x=~N016, carteira_assinada, na.rm = T)
ct1

prop_ct1 <- svymean(x=~N016, carteira_assinada, na.rm = T)
prop_ct1
confint(prop_ct1)

##Verificando a rela��o entre os indiv�duos diagnosticados com depresse�o em rala�ao a queles com uma percep��o forte � depress�o

empreendedor1 <- subset(empreendedor, Q092 == "Sim") #(empreendedores diagn�sticados com depress�o)

prop_ep12 <- svymean(x=~N016, empreendedor1, na.rm = T) #(propor��o de empreendedores diagnosticados com depress�o em rela��o a N016)
prop_ep12

prop_ep13 <- svymean(x=~Q092, subset(empreendedor, N016 == "Mais da metade dos dias"), na.rm = T) #(proor��o de empreendedores que tem depress�o e n�o tem depress�o em rela��o a uma resposta para N016)
prop_ep13
confint(prop_ep13)

prop_ep14 <- svymean(x=~Q092, subset(empreendedor, N016 == "Quase todos dias"), na.rm = T) #(an�logo ao anterior)
prop_ep14
confint(prop_ep14)

prop_ct15 <- svymean(x=~Q092, subset(carteira_assinada, N016 == "Mais da metade dos dias"), na.rm = T) #(proor��o de empreendedores que tem depress�o e n�o tem depress�o em rela��o a uma resposta para N016)
prop_ct15
confint(prop_ct15)

prop_ct16 <- svymean(x=~Q092, subset(carteira_assinada, N016 == "Quase todos dias"), na.rm = T) #(an�logo ao anterior)
prop_ct16
confint(prop_ct16)





## No estado do goi�s

pessoas_goias <- subset(dadospns, V0001 == "Goi�s")
empreendedor_goias <- subset(empreendedor, V0001 == "Goi�s")
carteira_assinada_goias <- subset(carteira_assinada, V0001 == "Goi�s")

# ESTIMANDO O N�MERO DE EMPREENDEDORES DO pa�s COM DEPRESS�O 

total_empreendedores_goias <- svytotal(x=~E01401, pessoas_goias, na.rm = T)
total_empreendedores_goias

total_empre_dep_goias <- svytotal(x=~Q092, empreendedor_goias, na.rm = T)
total_empre_dep_goias

proporcao_total_empre_dep_goias <- svymean(x=~Q092, empreendedor_goias, na.rm = T)
proporcao_total_empre_dep_goias
confint(proporcao_total_empre_dep_goias)

# ESTIMANDO O N�MERO DE sujeitos com carteira assinada DO GOI�S COM DEPRESS�O

total_carteira_goias <- svytotal(x=~E01403, pessoas_goias, na.rm = T)
total_carteira_goias

total_carteira_dep_goias <- svytotal(x=~Q092, carteira_assinada_goias, na.rm = T)
total_carteira_dep_goias 

proporcao_total_carteira_dep_goias <- svymean(x=~Q092, carteira_assinada_goias, na.rm = T)
proporcao_total_carteira_dep_goias
confint(proporcao_total_carteira_dep_goias)

# Verificando a percep��o de sa�de do empreendedor do goi�s  quanto a quest�es relacionadas � depress�o (N016)

ep1_goias <- svytotal(x=~N016, empreendedor_goias, na.rm = T)
ep1_goias

prop_ep1_goias <- svymean(x=~N016, empreendedor_goias, na.rm = T)
prop_ep1_goias
confint(prop_ep1_goias)

# Agora, para os sujeitos de carteira assinada 

ct1_goias <- svytotal(x=~N016, carteira_assinada_goias, na.rm = T)
ct1_goias

prop_ct1_goias <- svymean(x=~N016, carteira_assinada_goias, na.rm = T)
prop_ct1_goias
confint(prop_ct1_goias)

##Verificando a rela��o entre os indiv�duos diagnosticados com depresse�o em rala�ao a queles com uma percep��o forte � depress�o

prop_ep13_goias <- svymean(x=~Q092, subset(empreendedor_goias, N016 == "Mais da metade dos dias"), na.rm = T) #(proor��o de empreendedores que tem depress�o e n�o tem depress�o em rela��o a uma resposta para N016)
prop_ep13_goias
confint(prop_ep13_goias)

prop_ep14_goias <- svymean(x=~Q092, subset(empreendedor_goias, N016 == "Quase todos dias"), na.rm = T) #(an�logo ao anterior)
prop_ep14_goias
confint(prop_ep14_goias)

prop_ct15_goias <- svymean(x=~Q092, subset(carteira_assinada_goias, N016 == "Mais da metade dos dias"), na.rm = T) #(proor��o de empreendedores que tem depress�o e n�o tem depress�o em rela��o a uma resposta para N016)
prop_ct15_goias
confint(prop_ct15_goias)

prop_ct16_goias <- svymean(x=~Q092, subset(carteira_assinada_goias, N016 == "Quase todos dias"), na.rm = T) #(an�logo ao anterior)
prop_ct16_goias
confint(prop_ct16_goias)



## Outros diagn�sticos de doen�a mental 


#(GOI�S)
# para os empreendedores --------------------------------------------------

prop_ep17_goias <- svymean(x=~Q11006, empreendedor_goias, na.rm = T)
prop_ep17_goias

# para os sujeitos de carteira assinada -----------------------------------

prop_ep18_goias <- svymean(x=~Q11006, carteira_assinada_goias, na.rm = T)
prop_ep18_goias

#(Nacional)

prop_ep17_nacional <- svymean(x=~Q11006, empreendedor, na.rm = T)
prop_ep17_nacional

# para os sujeitos de carteira assinada -----------------------------------

prop_ep18_nacional <- svymean(x=~Q11006, carteira_assinada, na.rm = T)
prop_ep18_nacional


## Rascunho

glimpse(dadospns)

pessoas_goias <- subset(dadospns, V0001 == "Goi�s")

empre_goias <- subset(pessoas_goias, E01401== "Empregador"| E01401 == "Conta pr�pria")

empre_goias <- pns_design(data_pns = empre_goias)

total_empre <- svytotal(~E01401 == "Conta pr�pria", pessoas_goias , na.rm = T)
total_empre

total_empre1 <- svytotal(~E01401, subset (dadospns, E01401== "Empregador"| E01401 == "Conta pr�pria"), na.rm = T)
total_empre1

total_empre2 <- svytotal(x=~Q092, design=subset(pessoas_goias, E01401== "Empregador"| E01401 == "Conta pr�pria"), na.rm=TRUE)
total_empre2

total_empre3 <- svytotal(x=~Q092, design=subset(pessoas_goias, E01403 == "Sim" ), na.rm=TRUE)
total_empre3
