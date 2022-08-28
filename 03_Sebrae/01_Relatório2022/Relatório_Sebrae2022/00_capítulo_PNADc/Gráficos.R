

library(tidyverse)
library(readxl)

options(scipen = 999)

path <- "Análises/análise_regioes.xlsx"

df <- readxl::read_excel(path)

df <- read_excel("~/GitHub/lapei/03_Sebrae/01_Relatório2022/Relatório_Sebrae2022/00_capítulo_PNADc/Análises/análise_regioes.xlsx")

df %>% 
  ggplot(aes(x = fct_reorder(Região, Total), y = Total, fill = `Perfil Ocupacional`)) + 
  geom_col() + coord_flip() + xlab("Região") + theme_minimal()


df %>% 
  ggplot(aes(x = fct_reorder(Região, Percentual), y = Percentual, fill = `Perfil Ocupacional`)) + 
  geom_col() + coord_flip() + xlab("Região") + theme_minimal() + 
  theme(text = element_text(size = 30))      
