library(PNSIBGE)
library(dplyr)


# Baixando os dados -------------------------------------------------------


pns_2019 <- get_pns(year = 2019)

base_pns <- pns_2019[["variables"]]

pns_tratada <- 
  base_pns %>% 
  filter(E01401 == "Conta própria") %>% 
  select(#identificação e controle
         V0001, V0015, V0026, V0031, 
         #Informações do Domicílio
         A001, A005010, A005012, A009010, A01403, 
         A01501, A016010, A02201,
         #Visitas domiciliares de Equipe de Saúde da Família e Agentes de Endemias 
         B001,
         #Características gerais dos moradores
         C001, C00301, C004, C006, C008,
         C009, C01001, C010010, C011,
         #Características de educação dos moradores
         D00202, D00901, 
         #Características de trabalho das pessoas de 14 anos ou mais de idade
         E005, #E010011, E010012, E010013, 
         E011, 
         E01201, E01401, E01501, E01602, E017,
         E01802, E019, E033,
         #Cobertura de Plano de Saúde
         I00101, I00102,
         #Utilização de serviços de saúde
         J00101, #J002, 
         J012, J01301, J038,
         #Informações para futuros contatos, características do trabalho e apoio social
         M00303, M00401, M00402, M005010, M005011,
         M00601, M007, M008, M01601, M01901,
         #Percepção do estado de saúde 
         N004, N005, N006, N00701, N010, 
         N011, N012, N013, N014, N015, 
         N016, N017, N018,
         #Acidentes
         O00101,
         #Estilos de vida
         P00104, P00404, P019, P02002, P02801, 
         P03301, P035, P050, P05405,
         #Doenças crônicas
         Q00201, Q03001, Q060, Q06306, Q06307,
         Q06308, Q06309, Q06310, Q068, Q074,
         Q079, Q084, Q088, Q092, Q11007,
         Q11008, Q11009, Q111, Q11605, Q11606,
         Q120, Q124,
         #Saúde bucal
         U005, U02303, U02403,
         #Doenças transmissíveis
         T003, T004,
         #Atividade sexual (moradores de 15 anos ou mais)
         Y002,
         #Atendimento médico
         H001,
         #Antropometria
         W001, W00103, W00203,
         #Variáveis de amostragem
         V00281)



# Outros ------------------------------------------------------------------
write.csv(pns_tratada, "pns_tratada1.csv")
