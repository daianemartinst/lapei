library(PNSIBGE)
library(dplyr)


# Baixando os dados -------------------------------------------------------


pns_2019 <- get_pns(year = 2019)

base_pns <- pns_2019[["variables"]]

pns_tratada <- 
  base_pns %>% 
  filter(E01401 == "Conta própria") %>% 
  select(V0001, )



# analises ----------------------------------------------------------------


pns.svy <- get_pns(year=2019, selected=FALSE, anthropometry=FALSE,
                   labels=TRUE, deflator=TRUE, design=TRUE, savedir=tempdir())


survey::svymean(x=~J00101, design=pns.svy, na.rm=TRUE)

x <- pns.svy$variables
