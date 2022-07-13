#install.packages("PNADcIBGE")
#install.packages("survey")
library(PNADcIBGE)
library(survey)

var<-c("VD2002", "VD2003", "V2007", "V2009", "V2010", "VD3004", "VD4007", "VD4010", "UF", 
       "V4013", "V4016", "V4019", "V4020", "VD4016", "VD4036", "V4121B", "VD4017", "V1023")


dados_pnadc19 <- read_pnadc("PNADC_2019_visita1.txt", "input_PNADC_2019_visita1.txt", vars = var)


dados_pnadc192 <- pnadc_design(dados_pnadc19)
dados_pnadc192


pnadc_goias192 <- subset(dados_pnadc192, UF == "52")
pnadc_goias192


pnadc_mulherGO192 <- subset(pnadc_goias192, V2007 == "2")
pnadc_mulherGO192


pnadc_homemGO192 <- subset(pnadc_goias192, V2007 == "1")
pnadc_homemGO192


totalsexoNE19 <- svytotal(~V2007, subset(pnadc_goias192, VD4007 == "1"), na.rm = T)
totalsexoNE19

totalsexoE19 <- svytotal(~V2007, subset(pnadc_goias192, VD4007 == "2"), na.rm = T)
totalsexoE19

totalsexoCP19 <- svytotal(~V2007, subset(pnadc_goias192, VD4007 == "3"), na.rm = T)
totalsexoCP19


trab_areaM19 <- svytotal(~interaction (VD4007, V1023), pnadc_mulherGO192, na.rm = T)
trab_areaM19

trab_areaH19 <- svytotal(~interaction (VD4007, V1023), pnadc_homemGO192, na.rm = T)
trab_areaH19


mediarendaMNE19 <- svymean(~VD4017, subset(pnadc_mulherGO192, VD4007 == "1"), na.rm = T)
mediarendaMNE19

mediarendaME19 <- svymean(~VD4017, subset(pnadc_mulherGO192, VD4007 == "2"), na.rm = T)
mediarendaME19

mediarendaMCP19 <- svymean(~VD4017, subset(pnadc_mulherGO192, VD4007 == "3"), na.rm = T)
mediarendaMCP19

mediarendaHNE19 <- svymean(~VD4017, subset(pnadc_homemGO192, VD4007 == "1"), na.rm = T)
mediarendaHNE19

mediarendaHE19 <- svymean(~VD4017, subset(pnadc_homemGO192, VD4007 == "2"), na.rm = T)
mediarendaHE19

mediarendaHCP19 <- svymean(~VD4017, subset(pnadc_homemGO192, VD4007 == "3"), na.rm = T)
mediarendaHCP19

medianarendaMNE19 <- svyquantile(~VD4017, subset(pnadc_mulherGO192, VD4007 == "1"), quantiles = .5, na.rm = T)
medianarendaMNE19

medianarendaME19 <- svyquantile(~VD4017, subset(pnadc_mulherGO192, VD4007 == "2"), quantiles = .5, na.rm = T)
medianarendaME19

medianarendaMCP19 <- svyquantile(~VD4017, subset(pnadc_mulherGO192, VD4007 == "3"), quantiles = .5, na.rm = T)
medianarendaMCP19

medianarendaHNE19 <- svyquantile(~VD4017, subset(pnadc_homemGO192, VD4007 == "1"), quantiles = .5, na.rm = T)
medianarendaHNE19

medianarendaE19 <- svyquantile(~VD4017, subset(pnadc_homemGO192, VD4007 == "2"), quantiles = .5, na.rm = T)
medianarendaE19

medianarendaHCP19 <- svyquantile(~VD4017, subset(pnadc_homemGO192, VD4007 == "3"), quantiles = .5, na.rm = T)
medianarendaHCP19


totalracaM19 <- svytotal(~ interaction(VD4007, V2010), pnadc_mulherGO192, na.rm = T)
totalracaM19

totalracaH19 <- svytotal(~ interaction(VD4007, V2010), pnadc_homemGO192, na.rm = T)
totalracaH19


escolaridadeM19 <- svytotal(~interaction(VD4007, VD3004), pnadc_mulherGO192, na.rm = T)
escolaridadeM19

escolaridadeH19 <- svytotal(~interaction(VD4007, VD3004), pnadc_homemGO192, na.rm = T)
escolaridadeH19


idadeMNE19 <- svymean(~V2009, subset(pnadc_mulherGO192, VD4007 == "1"), na.rm = T)
idadeMNE19

idadeME19 <- svymean(~V2009, subset(pnadc_mulherGO192, VD4007 == "2"), na.rm = T)
idadeME19

idadeMCP19 <- svymean(~V2009, subset(pnadc_mulherGO192, VD4007 == "3"), na.rm = T)
idadeMCP19

idadeHNE19 <- svymean(~V2009, subset(pnadc_homemGO192, VD4007 == "1"), na.rm = T)
idadeHNE19

idadeHE19 <- svymean(~V2009, subset(pnadc_homemGO192, VD4007 == "2"), na.rm = T)
idadeHE19

idadeHCP19 <- svymean(~V2009, subset(pnadc_homemGO192, VD4007 == "3"), na.rm = T)
idadeHCP19

medianaidadeMNE19 <- svyquantile(~V2009, subset(pnadc_mulherGO192, VD4007 == "1"), quantiles = .5, na.rm = T)
medianaidadeMNE19

medianaidadeME19 <- svyquantile(~V2009, subset(pnadc_mulherGO192, VD4007 == "2"), quantiles = .5, na.rm = T)
medianaidadeME19

medianaidadeMCP19 <- svyquantile(~V2009, subset(pnadc_mulherGO192, VD4007 == "3"), quantiles = .5, na.rm = T)
medianaidadeMCP19

medianaidadeHNE19 <- svyquantile(~V2009, subset(pnadc_homemGO192, VD4007 == "1"), quantiles = .5, na.rm = T)
medianaidadeHNE19

medianaidadeHE19 <- svyquantile(~V2009, subset(pnadc_homemGO192, VD4007 == "2"), quantiles = .5, na.rm = T)
medianaidadeHE19

medianaidadeHCP19 <- svyquantile(~V2009, subset(pnadc_homemGO192, VD4007 == "3"), quantiles = .5, na.rm = T)
medianaidadeHCP19


horasTMNE19 <- svytotal(~VD4036, subset (pnadc_mulherGO192, VD4007 == "1"), na.rm = T)
horasTMNE19

horasTME19 <- svytotal(~VD4036, subset (pnadc_mulherGO192, VD4007 == "2"), na.rm = T)
horasTME19

horasTMCP19 <- svytotal(~VD4036, subset (pnadc_mulherGO192, VD4007 == "3"), na.rm = T)
horasTMCP19

horasTHNE19 <- svytotal(~VD4036, subset (pnadc_homemGO192, VD4007 == "1"), na.rm = T)
horasTHNE19

horasTHE19 <- svytotal(~VD4036, subset (pnadc_homemGO192, VD4007 == "2"), na.rm = T)
horasTHE19

horasTHCP19 <- svytotal(~VD4036, subset (pnadc_homemGO192, VD4007 == "3"), na.rm = T)
horasTHCP19