library(tidyverse)
library(readxl)

# Lendo dados ----

path <- "00_dados/publicacoes_internacionais.xlsx"

pub_intern <- readxl::read_excel(path)

# Fazendo gráficos sobre artigos internacionais ----

# Levantamento quanti, quali, misto, teórico

pub_intern %>% 
  group_by(classificacao, revista) %>% 
  count() %>% 
  ggplot(aes(x = revista, y = n, fill = classificacao)) + 
  geom_col(position = "fill") + coord_flip() + theme_bw() + 
  xlab("Peri?dico") + ylab("Propor??o") + 
  guides(fill=guide_legend(title="Classificação")) + 
  theme(text = element_text(size=30))
