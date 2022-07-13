library(tidyverse)
library(readxl)
library(roperators)

# Analises
# Lendo dados ----

path <- "00_dados/publicacoes_internacionais.xlsx"

pub_intern <- readxl::read_excel(path, sheet = "Planilha1")

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
  geom_col(position = "fill") + coord_flip() + theme_bw() + 
  xlab("Periódico") + ylab("Proporção") + ggtitle("Classificação de artigos", "n = 482") + 
  guides(fill=guide_legend(title="Classificação")) + 
  theme(text = element_text(size=30))

a <- pub_intern %>% 
  group_by(revista, classificacao) %>% 
  count()

writexl::write_xlsx(a, "contagem_tipo.xlsx")

# Fazendo gráficos de primária, secundário ou misto para artigos quanti ----
# Considerando apenas os dados de 

quanti <- 
  pub_intern %>% filter(classificacao == "Quantitativo")

quanti %>% 
  group_by(revista, Levantamento) %>% 
  count() %>% 
  filter(Levantamento != "Modelagem matemática") %>% 
  rename(fonte = Levantamento) %>% 
  ggplot(aes(x = revista, y = n, fill = fonte)) + 
  geom_col(position = "fill") + coord_flip() + theme_bw() + 
  theme(text = element_text(size=30)) +  
  ggtitle("Classificação de artigos", "n = 393 estudos quantitativos")


path_bases <- "00_dados/analises.xlsx"

bases <- readxl::read_excel(path, sheet = "Planilha1")

contagem_bases <- 
  bases %>% 
  select(base_arrumada) %>% 
  filter(base_arrumada != "Base privada de investidora") %>% 
  filter(base_arrumada != "Não identificado") %>% 
  group_by(base_arrumada) %>% 
  count() 


contagem_bases %>% 
  arrange(desc(n)) %>% 
  ungroup() %>% 
  top_n(12) %>% 
  ggplot(aes(x = fct_reorder(base_arrumada, n), y = n)) +
  geom_col() + coord_flip() + 
  theme_minimal() +
  theme(text = element_text(size=18)) +
  xlab("Base") + ggtitle("Bases mais usadas na amostra")


# Lendo dados do SINAN ----

path_sinan <- "00_dados/ACGRBR21.dbc"

sinan <- read.dbc::read.dbc(path_sinan)




