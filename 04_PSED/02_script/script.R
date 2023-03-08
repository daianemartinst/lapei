library(tidyverse)
library(haven)


# lendo os dados ----------------------------------------------------------

psed <- read_sav("01_dataframe/psedii_scrn_ABCDEF.sav")

psed_selecionado <- psed %>%
  select(BA15,CA15,DA15,#business effort
         BA42,CA42,DA42, #actively involved
         BA50,CA50,DA50,
         

# startup activities ------------------------------------------------------

         AD1,BD1,CD1,DD1, #business plan
         AD9,BD9,CD9,DD9, #marketing
         AD11,BD11,CD11,DD11, #own technology development
         AD13,BD13,CD13,DD13, #patent application, copyright or trademark
         AD16,BD16,CD16,DD16, # equipment, facilities, or property been purchased
         AD20,BD20,CD20,DD20, # talk with potential customers
         AD22,BD22,CD22,DD22, # collect information about competitors
         AD24,BD24,CD24,DD24, # define market opportunities 
         AD26,BD26,CD26,DD26, # financial projections
         BD30,CD30,DD30, #business modified or updated

# employee ----------------------------------------------------------------

         AE9,BE9,CE9,DE9, #employees working 35h or more
         AE10,BE10,CE10,DE10, #employees working less than 35h

# Highest level of education ----------------------------------------------
         AH6_1,
         AH6_2,BH6_2,CH6_2,DH6_2, 
         AH6_3,BH6_3,CH6_3,DH6_3, 
         AH6_4,BH6_4,CH6_4,DH6_4, 
         AH6_5,BH6_5,CH6_5,DH6_5, 
         BH6_6,CH6_6,DH6_6, 
         DH6_7,DH6_8,DH6_9,DH6_10, 

# How many years have you known? ------------------------------------------
         AH7_2,BH7_2,CH7_2,DH7_2, 
         AH7_3,BH7_3,CH7_3,DH7_3,  
         AH7_4,BH7_4,CH7_4,DH7_4,  
         AH7_5,BH7_5,CH7_5,DH7_5,  
         BH7_6,CH7_6,DH7_6,DH7_7,DH7_8,DH7_9,DH7_10,  

# Years of work experience ------------------------------------------------
         AH11_1,AH11_2, 
         BH11_2,CH11_2,DH11_2,AH11_3,BH11_3,CH11_3,DH11_3, 
         AH11_4,BH11_4,CH11_4,DH11_4,AH11_5,BH11_5,CH11_5,DH11_5, 
         BH11_6,CH11_6,DH11_6,DH11_7,DH11_8,DH11_9,DH11_10, 

# How many other businesses have helped to start as an owner or part-owner?--------
         AH12_1,AH12_2,BH12_2,CH12_2,DH12_2,AH12_3,BH12_3,CH12_3,DH12_3, 
         AH12_4,BH12_4,CH12_4,DH12_4,AH12_5,BH12_5,CH12_5,DH12_5, 
         BH12_6,CH12_6,DH12_6,DH12_7,DH12_8,DH12_9,DH12_10, 

# What is the dollar amount provided --------------------------------------
         AQ4_1,AQ4_2,AQ4_3,AQ4_4,AQ4_5, 
         BQ4_1,BQ4_2,BQ4_3,BQ4_4,BQ4_5,BQ4_6, 
         CQ4_1,CQ4_2,CQ4_3,CQ4_4,CQ4_5,CQ4_6, 
         DQ4_1,DQ4_2,DQ4_3,DQ4_4,DQ4_5,DQ4_6,DQ4_7,DQ4_8,DQ4_9,DQ4_10, 
         AQ5_1,AQ5_2,AQ5_3,AQ5_4,AQ5_5, 
         BQ5_1,BQ5_2,BQ5_3,BQ5_4,BQ5_5,BQ5_6, 
         CQ5_1,CQ5_2,CQ5_3,CQ5_4,CQ5_5,CQ5_6, 
         DQ5_1,DQ5_2,DQ5_3,DQ5_4,DQ5_5,DQ5_6,DQ5_7,DQ5_8,DQ5_9,DQ5_10, 

# Dollar amount of the debts ----------------------------------------------
         AR10,BR10,CR10,DR10, 
         AR15,BR15,CR15,DR15, 
         AR6,BR6,CR6,DR6, 
         AR8,BR8,CR8,DR8, 
         AR17,BR17,CR17,DR17,
         AR18,BR18,CR18,DR18,
         AR19,BR19,CR19,DR19,

# For this same twelve-month period... ------------------------------------
         BV11,CV11,DV11, #what (do you expect will be/was) the total of all expenses paid on behalf of the new business
         CV32,DV32, #What will be the total of all payments for wages, salaries, and benefits to full- and part-time employees and owners.
         CV34,DV34, #what (will be/was) the total of all payments for contract workers?
         CV36,DV36, #what (will be/was) the total spent on research and development of new products and services?
         CV38,DV38, #what (will be/was) the total spent on the purchase of new or used buildings or other structures
         CV40,DV40, #what (will be/was) the total spent on the purchase of land?
         CV42,DV42, #what (will be/was) the total spent on the purchase of new or used machinery or equipment?
         CV44,DV44, #(what do you expect/was) the total of all interest payments on all loans to be, not including repayment of principal?
         CV46,DV46, #what (will be/was) the total spent on rental or lease payments for buildings and other structures

# Others ------------------------------------------------------------------
         AR4,BR4,CR4,DR4, #additional money invested
         AS5,BS5,CS5,DS5, #Will spending on research and development be a major priority for this (new) business?
         AW1, #To achieve a higher position in society
         AW2, #To have greater flexibility for your personal and family life
         AW3, #To continue a family tradition
         AW4, #To be respected by your friends
         AW5, #To have considerable freedom to adapt your own approach to work
         AW6, #To give yourself, your spouse, and your children financial security
         AW7, #To follow the example of a person you admire
         AW8, #To build a business your children can inherit
         AW9, #To earn a larger personal income
         AW10, #To achieve something and get recognition for it
         AW11, #To develop an idea for a product
         AW12, #To have a chance to build great wealth or a very high income
         AW13, #To fulfill a personal vision
         AW14 #To have the power to greatly influence an organization
)


# Renomeando --------------------------------------------------------------

psed_renomeado <- 
  psed_selecionado %>% 
  rename(disangaged_wB = BA15, disangaged_wC = CA15, disangaged_wD = DA15, 
         
         involved_wB = BA42, involved_wC = CA42, involved_wD = DA42,
         
         new_firm_checkpoint_wB = BA50, new_firm_checkpoint_wC = CA50,new_firm_checkpoint_wD = DA50,
         business_plan_wA = AD1,business_plan_wB = BD1, business_plan_wC = CD1, business_plan_wD = DD1,
         marketing_checkpoint_wA = AD9,marketing_checkpoint_wB = BD9,marketing_checkpoint_wC = CD9,marketing_checkpoint_wD = DD9, 
         technology_checkpoint_wA = AD11, technology_checkpoint_wB = BD11, technology_checkpoint_wC = CD11, technology_checkpoint_wD = DD11,
         patent_checkpoint_wA = AD13, patent_checkpoint_wB = BD13, patent_checkpoint_wC = CD13, patent_checkpoint_wD = DD13, 
         opportunities_market_wA = AD24,opportunities_market_wB = BD24,opportunities_market_wC = CD24, opportunities_market_wD = DD24,
         facilities_wA = AD16, facilities_wB = BD16, facilities_wC = CD16, facilities_wD = DD16,
         talk_customer_wA = AD20, talk_customer_wB = BD20, talk_customer_wC = CD20, talk_customer_wD = DD20, 
         financial_proj_wA = AD26, financial_proj_wB = BD26, financial_proj_wC = CD26, financial_proj_wD = DD26,
         business_plan_modB = BD30, business_plan_modC = CD30, business_plan_modD = DD30,
         competitors_wA = AD22, competitors_wB = BD22, competitors_wC = CD22, competitors_wD = DD22,

# employee ----------------------------------------------------------------

       
       employees_more35hours_wA = AE9,employees_more35hours_wB = BE9,employees_more35hours_wC = CE9,employees_more35hours_wD = DE9,
       employees_less35hours_wA = AE10,employees_less35hours_wB = BE10,employees_less35hours_wC = CE10,employees_less35hours_wD = DE10,
       
       
# Highest level of education --------------------------------------
       education_wA = AH6_1,education_name2_wA = AH6_2,education_name2_wB = BH6_2,education_name2_wC = CH6_2,education_name2_wD = DH6_2,
       education_name3_wA = AH6_3, education_name3_wB = BH6_3, education_name3_wC = CH6_3, education_name3_wD = DH6_3,
       education_name4_wA = AH6_4,education_name4_wB = BH6_4,education_name4_wC = CH6_4, education_name4_wD = DH6_4,
       education_name5_wA = AH6_5,education_name5_wB = BH6_5,education_name5_wC = CH6_5,education_name5_wD = DH6_5,
       education_name6_wB = BH6_6,education_name6_wC = CH6_6,education_name6_wD = DH6_6,
       education_name7_wD = DH6_7,education_name8_wD = DH6_8,education_name9_wD = DH6_9,
       education_name10_wD = DH6_10,
    
# How many years have you known? ----------------------------------
       years_known_name2_wA = AH7_2,years_known_name2_wB = BH7_2,years_known_name2_wC = CH7_2,years_known_name2_wD = DH7_2,
       years_known_name3_wA = AH7_3,years_known_name3_wB = BH7_3,years_known_name3_wC = CH7_3,years_known_name3_wD = DH7_3,
       years_known_name4_wA = AH7_4,years_known_name4_wB = BH7_4,years_known_name4_wC = CH7_4,years_known_name4_wD = DH7_4,
       years_known_name5_wA = AH7_5,years_known_name5_wB = BH7_5,years_known_name5_wC = CH7_5,years_known_name5_wD = DH7_5,
       years_known_name6_wB = BH7_6, years_known_name6_wC = CH7_5, years_known_name6_wD = DH7_6,
       years_known_name7_wD = DH7_7,years_known_name8_wD = DH7_8,years_known_name9_wD = DH7_9,years_known_name10_wD = DH7_10,
       
# Years of work experience ----------------------------------------
       experience_wA =  AH11_1, 
       experience_nome2_wA =  AH11_2,experience_nome2_wB =  BH11_2,experience_nome2_wC =  CH11_2,experience_nome2_wD =  DH11_2,
       experience_nome3_wA =  AH11_3,experience_nome3_wB =  BH11_3,experience_nome3_wC =  CH11_3,experience_nome3_wD =  DH11_3,
       experience_nome4_wA =  AH11_4,experience_nome4_wB =  BH11_4,experience_nome4_wC =  CH11_4,experience_nome4_wD =  DH11_4,
       experience_nome5_wA =  AH11_5,experience_nome5_wB =  BH11_5,experience_nome5_wC =  CH11_5,experience_nome5_wD =  DH11_5,
       experience_nome6_wB =  BH11_6,experience_nome6_wC =  CH11_6,experience_nome6_wD =  DH11_6,
       experience_nome7_wD =  DH11_7,experience_nome8_wD =  DH11_8,experience_nome9_wD =  DH11_9,experience_nome10_wD =  DH11_10,
       
# How many other businesses have helped to start as an own --------
       businesses_have_helped_wA = AH12_1,  
       other_businesses_helped_nome2_wA = AH12_2, other_businesses_helped_nome2_wB = BH12_2, other_businesses_helped_nome2_wC = CH12_2, other_businesses_helped_nome2_wD = DH12_2,
       other_businesses_helped_nome3_wA = AH12_3, other_businesses_helped_nome3_wB = BH12_3, other_businesses_helped_nome3_wC = CH12_3, other_businesses_helped_nome3_wD = DH12_3,
       other_businesses_helped_nome4_wA = AH12_4, other_businesses_helped_nome4_wB = BH12_4, other_businesses_helped_nome4_wC = CH12_4, other_businesses_helped_nome4_wD = DH12_4,
       other_businesses_helped_nome5_wA = AH12_5, other_businesses_helped_nome5_wB = BH12_5, other_businesses_helped_nome5_wC = CH12_5, other_businesses_helped_nome5_wD = DH12_5,
       other_businesses_helped_nome6_wB = BH12_6, other_businesses_helped_nome6_wC = CH12_6, other_businesses_helped_nome6_wD = DH12_6,
       other_businesses_helped_nome7_wD = DH12_7,other_businesses_helped_nome8_wD = DH12_8,other_businesses_helped_nome9_wD = DH12_9,other_businesses_helped_nome10_wD = DH12_10,
       
# What is the dollar amount provided ------------------------------
       personal_investment1_wA = AQ4_1, personal_investment2_wA = AQ4_2,  personal_investment3_wA = AQ4_3,personal_investment4_wA = AQ4_4, personal_investment5_wA = AQ4_5,
       personal_investment1_wB = BQ4_1, personal_investment2_wB = BQ4_2,  personal_investment3_wB = BQ4_3,personal_investment4_wB = BQ4_4, personal_investment5_wB = BQ4_5,personal_investment6_wB = BQ4_6,
       personal_investment1_wC = CQ4_1, personal_investment2_wC = CQ4_2,  personal_investment3_wC = CQ4_3,personal_investment4_wC = CQ4_4, personal_investment5_wC = CQ4_5,personal_investment6_wC = CQ4_6,
       personal_investment1_wD = DQ4_1, personal_investment2_wD = DQ4_2,  personal_investment3_wD = DQ4_3,personal_investment4_wD = DQ4_4, personal_investment5_wD = DQ4_5,personal_investment6_wD = DQ4_6,personal_investment7_wD = DQ4_7,personal_investment8_wD = DQ4_8,personal_investment9_wD = DQ4_9,   personal_investment10_wD = DQ4_10,
       family_loans1_wA = AQ5_1,family_loans2_wA = AQ5_2,family_loans3_wA = AQ5_3,family_loans4_wA = AQ5_4,family_loans5_wA = AQ5_5,
       family_loans1_wB = BQ5_1,family_loans2_wB = BQ5_2,family_loans3_wB = BQ5_3,family_loans4_wB = BQ5_4,family_loans5_wB = BQ5_5,family_loans6_wB = BQ5_6,
       family_loans1_wC = CQ5_1,family_loans2_wC = CQ5_2,family_loans3_wC = CQ5_3,family_loans4_wC = CQ5_4,family_loans5_wC = CQ5_5,family_loans6_wC = CQ5_6,
       family_loans1_wD = DQ5_1,family_loans2_wD = DQ5_2,family_loans3_wD = DQ5_3,family_loans4_wD = DQ5_4,family_loans5_wD = DQ5_5,family_loans6_wD = DQ5_6,family_loans7_wD = DQ5_7,family_loans8_wD = DQ5_8,family_loans9_wD = DQ5_9,family_loans10_wD = DQ5_10,
       
# Dollar amount of the debts --------------------------------------
       personal_loans_debts_wA = AR10,personal_loans_debts_wB = BR10,personal_loans_debts_wC = CR10,personal_loans_debts_wD = DR10,
       credit_card_loans_debts_wA = AR15, credit_card_loans_debts_wB = BR15,credit_card_loans_debts_wC = CR15,credit_card_loans_debts_wD = DR15,
       assets_debts_wA = AR6,assets_debts_wB = BR6,assets_debts_wC = CR6,assets_debts_wD = DR6,
       bank_credit_debts_wA = AR8,bank_credit_debts_wB = BR8,bank_credit_debts_wC = CR8,bank_credit_debts_wD = DR8,
       loans_venture_capital_firms_debts_wA = AR17,loans_venture_capital_firms_debts_wB = BR17,loans_venture_capital_firms_debts_wC = CR17,loans_venture_capital_firms_debts_wD = DR17,
       government_agencies_debts_wA = AR18, government_agencies_debts_wB = BR18,government_agencies_debts_wC = CR18,government_agencies_debts_wD = DR18,
       bank_loans_debts_wA = AR19, bank_loans_debts_wB = BR19, bank_loans_debts_wC = CR19,bank_loans_debts_wD = DR19,
       
# For this same twelve-month period... ----------------------------
       total_expenses_paid_wB = BV11, total_expenses_paid_wC = CV11, total_expenses_paid_wD = DV11,
       salaries_payments_total_wC = CV32, salaries_payments_total_wD = DV32,
       contract_workers_payments_total_wC = CV34, contract_workers_payments_total_wD = DV34,
       total_spent_research_wC = CV36, total_spent_research_wD = DV36, 
       structures_spent_total_wC = CV38,structures_spent_total_wD = DV38, 
       total_spent_purchase_land_wC = CV40, total_spent_purchase_land_wD = DV40,
       total_spent_equipment_wC = CV42,total_spent_equipment_wD = DV42,
       total_payments_loans_wC = CV44,total_payments_loans_wD = DV44,
       total_spent_rental_payments_wC = CV46, total_spent_rental_payments_wD = DV46,
       
# others ----------------------------------------------------------
       invested_additional_money_wA = AR4, invested_additional_money_wB = BR4, invested_additional_money_wC = CR4, invested_additional_money_wD = DR4,
       research_spending_priority_wA = AS5, research_spending_priority_wB = BS5,research_spending_priority_wC = CS5,research_spending_priority_wD = DS5,
       achieve_higher_position_society_importance_wA = AW1, 
       flexibility_family_life_importance_wA = AW2,
       family_tradition_impotance_wA = AW3, 
       respected_friends_wA = AW4,
       freedom_importance_wA = AW5,
       yourself_spouse_children_importance_wA = AW6,
       example_person_admire_importance_wA = AW7,
       business_children_inherit_importance_wA = AW8,
       larger_personal_income_importance_wA = AW9,
       recognition_importance_wA = AW10, 
       develop_idea_product_importance_wA = AW11,
       wealth_importance_wA = AW12, 
       personal_vision_importance_wA = AW13,
       power_influence_organization_wA = AW14)




#skimr::skim(psed_selecionado)


# Tratamento de bases -----------------------------------------------------

# tratando algumas variáveis numericas antes 

# employee 

psed_renomeado$employees_less35hours_wA[is.na(psed_renomeado$employees_less35hours_wA)] <- 0
psed_renomeado$employees_less35hours_wB[is.na(psed_renomeado$employees_less35hours_wB)] <- 0
psed_renomeado$employees_less35hours_wC[is.na(psed_renomeado$employees_less35hours_wC)] <- 0
psed_renomeado$employees_less35hours_wD[is.na(psed_renomeado$employees_less35hours_wD)] <- 0


# Tratando base como um todo

psed_tratado <- 
  psed_renomeado %>% 
  # operando
  mutate(status = case_when(involved_wB == 5 | 
                            involved_wC == 5 | 
                            involved_wD == 5 ~ 'saiu', 
                            TRUE ~ 'operando')) %>% 
  # business plan
  mutate(business_plan = case_when(business_plan_wA == 1 |
                                   business_plan_wB == 1 |
                                   business_plan_wC == 1 |
                                   business_plan_wD == 1 ~ 'preparou BP',
                                   TRUE ~ 'não preparou BP')) %>% 
  # business plan modification 
  mutate(business_plan_mod = case_when(business_plan_modB == 1 |
                                       business_plan_modC == 1 |
                                       business_plan_modD == 1 ~ 'modificacao BP',
                                       TRUE ~ 'nao modificou BP')) %>% 
  # mkt efforts
  mutate(mkt_efforts = case_when(marketing_checkpoint_wA == 1 |
                                 marketing_checkpoint_wB == 1 |
                                 marketing_checkpoint_wC == 1 |
                                 marketing_checkpoint_wD == 1 ~ 'mkt efforts',
                                 TRUE ~ 'nao fez mkt efforts')) %>% 
  # technology efforts
  mutate(tech_efforts = case_when(technology_checkpoint_wA == 1 |
                                  technology_checkpoint_wB == 1 |
                                  technology_checkpoint_wC == 1 |
                                  technology_checkpoint_wD == 1 ~ 'tech efforts',
                                  TRUE ~ 'não tech efforts')) %>% 
  # patent
  mutate(patents = case_when(patent_checkpoint_wA == 1 |
                             patent_checkpoint_wB == 1 |
                             patent_checkpoint_wC == 1 | 
                             patent_checkpoint_wD == 1  ~ 'patent',
                             TRUE ~ 'nao patent')) %>% 
  
  # facilities 
  mutate(facilities = case_when(facilities_wA == 1 |
                                facilities_wB == 1 |
                                facilities_wC == 1 |
                                facilities_wD == 1 ~ 'facilities',
                                TRUE ~ 'nao facilities')) %>% 
  # talk customers 
  mutate(talk_customers = case_when(talk_customer_wA == 1 |
                                    talk_customer_wB == 1 |
                                    talk_customer_wC == 1 |
                                    talk_customer_wD == 1 ~ 'talk to customers',
                                    TRUE ~ 'did not talk to customers')) %>% 
  # financial projectios
  mutate(financial_projections = case_when(financial_proj_wA == 1 |
                                           financial_proj_wB == 1 |
                                           financial_proj_wC == 1 |
                                           financial_proj_wD == 1 ~ 'financial projections',
                                           TRUE ~ 'did not financial projections')) %>% 
  # competitors 
  mutate(info_competitors = case_when(competitors_wA == 1 |
                                      competitors_wB == 1 |
                                      competitors_wC == 1 |
                                      competitors_wD == 1 ~ 'collected competitors info',
                                      TRUE ~ 'did not collect competitors info')) %>% 
  
  # employees
  
  mutate(employee_less35 = (employees_less35hours_wA + employees_less35hours_wB + 
                           employees_less35hours_wC + employees_less35hours_wD)/4)

table(psed_tratado$mkt_efforts, psed_tratado$status)

