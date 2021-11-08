library(srvyr); library(tidyverse); library(vroom); library(readxl); library(patchwork);
library(ggrepel)


dadospns <- get_pns(year=2019, labels=TRUE, deflator=TRUE, design=TRUE, vars=variaveis_selecionadas)

teste <- as_survey_design(dadospns)

tipotrabalho_sexo_uf <- teste %>%
  filter(E01401 == "Empregador" | E01401 == "Empregado do setor privado"|
         E01401 == "Conta própria") %>% 
  group_by(V001, E01401, C006) %>%
  summarise(proportion_sexo = survey_mean(),
            total = survey_total()) 

tipotrabalho_sexo_uf <- teste %>%
  filter(E01401 == "Empregador" | E01401 == "Empregado do setor privado"|
           E01401 == "Conta própria") %>% 
  group_by(V0001, E01401, C006) %>%
  summarise(proportion_sexo = survey_mean(),
            total = survey_total()) 

tipotrabalho_sexo2 <- teste %>%
  group_by(E01401, C006) %>%
  summarise(proportion_sexo = survey_mean(),
            total = survey_total()) %>% 
  filter(E01401 == "Empregador" | E01401 == "Empregado do setor privado"|
           E01401 == "Conta própria")


