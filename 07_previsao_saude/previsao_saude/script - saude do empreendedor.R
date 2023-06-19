library(PNSIBGE)
library(tidyverse)

setwd("~/LAPEI/Projeto saúde do empreendedor PNS")

variaveis <- c("E01401","J00402","J00101")

pns <- read_pns("PNS_2019.txt", input_txt = "input_PNS_2019.txt")

pns_2019 <- get_pns(year = 2019)

pns.svy <- get_pns(year=2019, selected=FALSE, anthropometry=FALSE,
                   labels=TRUE, deflator=TRUE, design=TRUE, savedir=tempdir())


survey::svymean(x=~J00101, design=pns.svy, na.rm=TRUE)

x <- pns.svy$variables
