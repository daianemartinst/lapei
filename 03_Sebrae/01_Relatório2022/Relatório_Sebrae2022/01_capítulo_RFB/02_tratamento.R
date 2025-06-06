library(tidyverse) 
library(vroom)
library(genderBR) 
library(readxl) 
library(lubridate)
library(dygraphs)

# Definindo caminhos ----

path_cnae <- "02_bases_apoio/cnae.csv"
path_municipios <- "02_bases_apoio/municipios_serpro.csv"
path_nat_juridica <- "02_bases_apoio/natureza_juridica.csv"
path_pop_goias <- "02_bases_apoio/pop_goias_2020.csv"


# Lendo dados ----

tab_cnae <- read_delim(path_cnae, ",", escape_double = FALSE, trim_ws = TRUE, 
                       locale = locale(encoding = "windows-1252"))

municipios <- read_delim(path_municipios,";", escape_double = FALSE, trim_ws = TRUE, 
                         locale = locale(encoding = "windows-1252")) %>%
                         filter(UF == "GO")

natureza_juridica <- read_delim(path_nat_juridica,
                                ",", escape_double = FALSE, 
                                trim_ws = TRUE, 
                                locale = locale(encoding = "windows-1252")) 


pop_goias_2020 <- read_csv(pop_goias_2020.csv) %>% 
                        janitor::clean_names()

pop_ea <- pop_goias_2020 %>% 
  filter(idade != "De 0 até 4 anos" & 
         idade != "De 5 até 9 anos" &
         idade != "De 10 até 14 anos") %>% 
  group_by(cod_mun, sexo) %>% 
  summarise(total = sum(pop))

# Lendo dados que foram tratados pelo script 01 ----
# Colocar os paths das bases tratadas e depois adicionar a leitura
# dos dados usando a funcao vroom das seguintes bases:

# Sócios
# Empresas ativas

socios <- #vroom(colocar path aqui)

ativas_base <- #vroom(colocar path aqui)

ativas <- ativas_base %>% 
  rename(razao_social = X2, natureza_juridica = X3, 
         qualifica_responsavel = X4, capital_social = X5,
         porte_empresa = X6, ente_federativo = X7) %>% 
         left_join(natureza_juridica, by = c("natureza_juridica" = "cod_subclass_natureza_juridica")) %>% 
         filter(cod_natureza_juridica > 1 & cod_natureza_juridica < 5)


# tratando ME separadamente -----------------------------------------------

microempresas <- ativas %>% 
        filter(natureza_juridica == "2135") 

me <- microempresas %>% 
  mutate(genero = get_gender(razao_social), .after = razao_social) %>% 
  filter(genero != "NA") %>% 
  left_join(municipios, by = c("municipio"="Codigo_TOM_SERPRO")) %>% 
  mutate(data = parse_date_time(data_inicio_atividade, orders = "ymd"),
         ano = year(data)) 

me2 <- me %>% 
  distinct(razao_social, municipio, .keep_all = TRUE)

municipios_me <- me2 %>% 
  rename(nome_municipio = Municipio) %>% 
  group_by(municipio, nome_municipio , genero) %>% 
  count() %>% 
  mutate(natureza = "me")


nome_diferentes <- me %>%  
      filter(is.na(genero)) 

# writexl::write_xlsx(total_anos, "total_anos.xlsx")

# tratando n?o ME --------------------------------------------------------------

nao_me1 <- ativas %>% 
  filter(natureza_juridica != "2135") 

nao_me <- nao_me1 %>% 
        left_join(socios, by = c("cnpj_basico"="cnpj_basico")) 

nao_me_tratado <- nao_me %>% 
            mutate(genero = get_gender(nome_socio), .after = nome_socio) %>% 
            filter(data_inicio_atividade == entrada_sociedade) %>% 
            select(-`...1.x`) %>% 
            mutate(data = parse_date_time(data_inicio_atividade, orders = "ymd"),
            ano = year(data))


nao_me_tratado_s_na <- nao_me_tratado %>% 
                      filter(genero != "NA")

 x <- nao_me_tratado_s_na %>% 
         group_by(nome_socio) %>% 
         count() %>% 
         filter(n > 4)

nao_me_distinct <- nao_me_tratado_s_na %>% 
  distinct(nome_socio, municipio, faixa_etaria, .keep_all = TRUE)

# suzana <- nao_me1 %>% 
#   filter(nome_socio == "SUZANA MARIA CAMILO DE LIMA BITTENCOURT") %>% 
#   select(nome_socio, faixa_etaria, cnpj_basico, razao_social)
# 
# DT::datatable(suzana)

municipios_nao_me <- nao_me_distinct %>% 
                      left_join(municipios, by = c("municipio"="Codigo_TOM_SERPRO")) %>% 
                      rename(nome_municipio = Municipio) %>% 
                      group_by(municipio,nome_municipio,genero) %>% 
                      count() %>% 
                      mutate(natureza = "n?o me")

total_por_tipo <- rbind(municipios_me, municipios_nao_me)

total_sem_tipo <- total_por_tipo %>% 
          group_by(municipio, nome_municipio, genero) %>% 
          summarise(total_empreendedores = sum(n)) %>% 
          mutate(genero = case_when(genero == "Female" ~ "Feminino",
                                    genero == "Male" ~ "Masculino"))

# total_por_sexo_spread <- nao_me_tratado_s_na %>% 
#                       group_by(ano, cnpj_basico, genero) %>% 
#                       count() %>% 
#                       spread(genero,n)
# 
# total_por_sexo_spread[is.na(total_por_sexo_spread)] <- 0
# 
# nao_me_generos <- nao_me1 %>% 
#   filter(natureza_juridica != "2135") %>% 
#   left_join(total_por_sexo_spread, by = "cnpj_basico") %>% 
#   filter(ano != "NA")
# 
# nao_me_generos %>% 
#   gather(key = "genero", value = "total", Female, Male) %>% 
#   left_join(municipios, by = c("municipio"="Codigo_TOM_SERPRO")) %>% 
#   rename(nome_municipio = Municipio) %>% 
#   group_by(municipio, nome_municipio, genero) %>% 
#   summarise(total = sum(total)) %>% 
#   mutate(natureza = "nao me")
# 
# 
# 
# write.csv(nao_me, "ativas_nao_me.csv")


# Consolidando por popula??o ativa ----------------------------------------

pop_ea_spread <- pop_ea %>%  
                    spread(sexo, total)


total_taxa <- total_sem_tipo %>% 
  spread(genero, total_empreendedores) %>% 
  rename(emp_fem = Feminino, emp_masc = Masculino) %>% 
  left_join(municipios, by = c("municipio"="Codigo_TOM_SERPRO")) %>% 
  left_join(pop_ea_spread, by = c("cod_IBGE"="cod_mun")) %>% 
  select(-Municipio, -cod_IBGE, -UF) %>%  
  mutate(taxa_percentual_fem = emp_fem/Feminino, 
         taxa_percentual_masc = emp_masc/Masculino)


perc_municipio <- total_taxa %>% 
  filter(UF == "GO") %>% 
  select(nome_municipio, genero, total_empreendedores) %>% 
  spread(key = genero, value = total_empreendedores) %>% 
  mutate(perc_feminino = Feminino/(Feminino + Masculino),
         perc_masculino = Masculino/(Feminino + Masculino))


sum(perc_municipio$Feminino)

# writexl::write_xlsx(perc_municipio, "percentual_por_municipio.xlsx")
# writexl::write_xlsx(total_taxa, "taxa_por_municipio.xlsx")

# Analisando CNAE ---------------------------------------------------------

div_cnae <- tab_cnae %>% 
  group_by(cod_divisao,nm_divisao) %>% 
  count() %>% 
  select(-n)

me2$cnae <- as.character(me2$cnae)

cnae_me <- me2 %>% 
  group_by(cnae, genero) %>% 
  count() 

nao_me_distinct$cnae <- as.character(nao_me_distinct$cnae)

cnae_naome <- nao_me_distinct %>% 
  group_by(cnae, genero) %>% 
  count() 

cnae_total <- rbind(cnae_me, cnae_naome)

cnae_total2 <- cnae_total %>% 
  mutate(classe_cnae = str_sub(cnae, end = 2)) %>%  
  group_by(genero, classe_cnae) %>% 
  summarise(total = sum(n)) %>% 
  mutate(genero = case_when(genero == "Female" ~ "Feminino",
                            genero == "Male" ~ "Masculino")) %>% 
  spread(genero, total) %>% 
  mutate(perc_masculino = Masculino/(Feminino + Masculino),
         perc_feminino = Feminino/(Feminino + Masculino)) %>% 
  left_join(div_cnae, by = c("classe_cnae"="cod_divisao"))  


# writexl::write_xlsx(cnae_total2, "cnae_total.xlsx")

# contabilizando total por ano --------------------------------------------

me3 <- me2 %>% select(ano, genero, natureza_juridica)
nao_me2 <- nao_me_distinct %>% select(ano, genero, natureza_juridica)

total <- rbind(me3, nao_me2)

total_anos <- total %>% 
  group_by(ano, genero) %>% 
  count() %>% 
  spread(genero,n) %>% 
  mutate(total = Female + Male,
         prop_female = Female/total,
         prop_male = Male/total) 

total_anos %>% 
  filter(ano > 1980) %>% 
  ggplot() +
  geom_line(aes(x = ano, y = prop_female, col = "darkred")) +
  geom_line(aes(x = ano, y = prop_male, col = "darkblue")) + 
  theme_minimal()

# writexl::write_xlsx(total_anos, "evolucao_generos.xlsx")


total_natjur <- total %>% 
  group_by(genero, natureza_juridica) %>% 
  count() 

# writexl::write_xlsx(total_natjur, "totalnatureza.xlsx")
