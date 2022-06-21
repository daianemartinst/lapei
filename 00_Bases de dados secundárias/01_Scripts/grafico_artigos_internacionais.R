library(tidyverse)
library(readxl)
library(roperators)

# Analises
# Lendo dados ----

path <- "00_dados/publicacoes_internacionais.xlsx"

pub_intern <- readxl::read_excel(path)

# Fazendo gráficos sobre artigos internacionais ----

# Levantamento quanti, quali, misto, teórico

# Mantendo apenas os artigos empíricos e retirando eventuais
# duplicatas

excluir <- c("Editorial", "Teórico", "Revisão")

pub_intern <- pub_intern %>% 
                  filter(classificacao %ni% excluir) %>% 
                  distinct(titulo, .keep_all = TRUE)

pub_intern %>% 
  group_by(classificacao, revista) %>% 
  count() %>% 
  ggplot(aes(x = revista, y = n, fill = classificacao)) + 
  geom_col(position = "dodge") + coord_flip() + theme_bw() + 
  xlab("Periódico") + ylab("Proporção") + ggtitle("Classificação de artigos", "n = 483") + 
  guides(fill=guide_legend(title="Classificação")) + 
  theme(text = element_text(size=30))


# Fazendo gráficos de primária, secundário ou misto para artigos quanti ----
# Considerando apenas os dados de 

quanti <- 
  pub_intern %>% filter(classificacao == "Quantitativo")

quanti %>% 
  group_by(revista, Levantamento) %>% 
  count() %>% 
  ggplot(aes(x = revista, y = n, fill = Levantamento)) + 
  geom_col(position = "dodge") + coord_flip() + theme_bw()






