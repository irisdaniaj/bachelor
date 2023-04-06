getwd()
setwd("C:\tesi")
rm(list = ls())

#caricamento dati
library(readxl)
EIS_Data <- read_excel("EIS_Data.xlsx")
View(EIS_Data)
dim(EIS_Data)

#vedere le variabili 
library(funModeling)
df_status(EIS_Data) 

options(pillar.print_max = 10, pillar.print_min = 10)
#valori mancanti in "Level" 20,47%, variabile binaria
unique(EIS_Data[c("Level")]) #vedere quali sono i valori di Level--> 0-1
#???valori mancanti in Value 1.89% 
EIS_Data$Value
min(EIS_Data$Value)
max(EIS_Data$Value)
#cercare di capire per quali stati Value = 0 
library(dplyr)
dplyr::filter(EIS_Data, Value %in% c("0"))
#paesi come montenegro, bosnia, turchia hanno value = 0 per l'indicatore 1.1.1 che dice qual ? il numero di 
#dottorati STEM nella popolazione --> strano che siano zero


options(pillar.print_max = Inf) #per vedere tutti i valori del comando sotto
#sembrano esserci dei raggruppamenti in pi? rispetto a quelli segnati nella documentazione 
unique(EIS_Data[c("Indicator")])
unique(EIS_Data[c("Region")])
#smebra che gli indicatori elementari importanti siano: 
#0 Summary Innovation Index
#1.1 Human resources
#1.2 Attractive research system 
#1.3 Digitalisation
#2.1 Finance and support
#2.2 Firm investemnts
#2.3 Use of Information Technologies 
#3.1 Innovators 
#3.2 Linkages
#3.3 Intellectual assets
#4.1 Employment impacts
#4.2 Sales impact 
#4.3 Environmental sustainibility 
#5.1.1 GDP per capita(thousands)(SD)
#5.2.1 Enterprise births (SD)
#5.3.1 In-house product innovators with market novelties (SD) 
#5.4.1 Ease of starting a business (SD) 
#5.5.1 Circular material use rate (SD)

EIS_lavoro <- subset(EIS_Data, Indicator %in% c("0 Summary Innovation Index",
                                                "1.1 Human resources", 
                                                "1.2 Attractive research systems", 
                                                "1.3 Digitalisation", 
                                                "2.1 Finance and support",
                                                "2.2 Firm investments", 
                                                "2.3 Use of information technologies", 
                                                "3.1 Innovators", 
                                                "3.2 Linkages", 
                                                "3.3 Intellectual assets", 
                                                "4.1 Employment impacts", 
                                                "4.2 Sales impacts", 
                                                "4.3 Environmental sustainability", 
                                                "5.1.1 GDP per capita (thousands) (SD)", 
                                                "5.2.1 Enterprise births (SD)", 
                                                "5.3.1 In-house product innovators with market novelties (SD)",
                                                "5.4.1 Ease of starting a business (SD)", 
                                                "5.5.1 Circular material use rate (SD)",
                                                "1.1.1 New doctorate graduates",                                        
                                                "1.1.2 Population with tertiary education (Regional)",                  
                                                "1.1.3 Population involved in lifelong learning (Regional)",                                       
                                                "1.2.1 International scientific co-publications (Regional)",             
                                                "1.2.2 Scientific publications among the top 10% most cited (Regional)", 
                                                "1.2.3 Foreign doctorate students as a % of all doctorate students",                                         
                                                "1.3.1 Broadband penetration",                                          
                                                "1.3.2 Individuals with above basic overall digital skills (Regional)",                                         
                                                "2.1.1 R&D expenditure in the public sector (Regional)",              
                                                "2.1.2 Venture capital expenditures",                                 
                                                "2.1.3 Direct and indirect government support of business R&D",                                                
                                                "2.2.1 R&D expenditure in the business sector (Regional)",              
                                                "2.2.2 Non-R&D innovation expenditures (Regional)",                   
                                                "2.2.3 Innovation expenditures per person employed (Regional)",                                    
                                                "2.3.1 Enterprises providing ICT training",                             
                                                "2.3.2 Employed ICT specialists",                                                    
                                                "3.1.1 SMEs introducing product innovations (Regional)",                
                                                "3.1.2 SMEs introducing business process innovations (Regional)",                                                    
                                                "3.2.1 Innovative SMEs collaborating with others (Regional)",          
                                                "3.2.2 Public-private co-publications (Regional)",                      
                                                "3.2.3 Job-to-job mobility of HRST",                                           
                                                "3.3.1 PCT patent applications (Regional)",                             
                                                "3.3.2 Trademark applications (Regional)",                              
                                                "3.3.3 Design applications (Regional)",                                           
                                                "4.1.1 Employment in knowledge-intensive activities (Regional)",        
                                                "4.1.2 Employment in innovative enterprises (Regional)",                                                   
                                                "4.2.1 Exports of medium and high technology products",                
                                                "4.2.2 Knowledge-intensive services exports",                            
                                                "4.2.3 Sales of new-to-market and new-to-firm innovations (Regional)",                             
                                                "4.3.1 Resource productivity",                                           
                                                "4.3.2 Air emissions by fine particulates (Regional)",                              
                                                "5.1.2 Average annual GDP growth (SD)",                                  
                                                "5.1.3 Employment share Manufacturing (SD)",                            
                                                "5.1.4 Employment share High and Medium high-tech (SD)",                 
                                                "5.1.5 Employment share Services (SD)",                                  
                                                "5.1.6 Employment share Knowledge-intensive services (SD)",              
                                                "5.1.7 Turnover share SMEs (SD)",                                        
                                                "5.1.8 Turnover share large enterprises (SD)",                          
                                                "5.1.9 Foreign-controlled enterprises - share of value added (SD)",                                     
                                                "5.2.2 Total Entrepreneurial Activity (SD)",                             
                                                "5.2.3 FDI net inflows (SD)",                                            
                                                "5.2.4 Top R&D spending enterprises (SD)",                               
                                                "5.2.5 Buyer sophistication (SD)",        
                                                "5.3.2 In-house product innovators without market novelties (SD)",       
                                                "5.3.3 In-house business process innovators (SD)",                       
                                                "5.3.4 Innovators that do not develop innovations themselves (SD)",      
                                                "5.3.5 Innovation active non-innovators (SD)",                           
                                                "5.3.6 Non-innovators with potential to innovate (SD)",                  
                                                "5.3.7 Non-innovators without disposition to innovate (SD)",                             
                                                "5.4.2 Basic-school entrepreneurial education and training (SD)",        
                                                "5.4.3 Government procurement of advanced technology products (SD)",     
                                                "5.4.4 Rule of law (SD)",                             
                                                "5.5.2 Greenhouse gas emissions intensity of energy consumption  (SD)",  
                                                "5.5.3 Eco-Innovation Index (SD)",                                       
                                                "5.6.1 Population size (SD)",                                           
                                                "5.6.2 Average annual population growth (SD)",                           
                                                "5.6.3 Population density (SD)",                                         
                                                "4.3.3 Environment-related technologies",                                
                                                "5.5.2 Greenhouse gas emissions intensity of energy consumption (SD)"))
#c'? due volte 5.5.2 Greenhouse gas emissions intensity of energy consumption (SD)  nel dataset

#unit? statistiche i singoli paesi, poi raggruppati per zona e tengo solo gli indicatori elencati sopra
#creare 18 nuove colonne che corrispondono agli indicatori sopra
#controlliamo che countryname e ragion name abbiano gli stessi valori identici, cos? possiamo eliminarne una
options(pillar.print_max = 10, pillar.print_min = 10) #per limitare la vista dei risultati
unique(EIS_Data[c("CountryName")])
unique(EIS_Data[c("RegionName")])
#no sono diverse

#creaimo un nuovo dataset pulito su cui lavorare
EIS_lavoro <- EIS_Data

#teniamo solo gli indicatori che ci interessano 

options(pillar.print_max = 20)#per non vedere tutti i risultati

#non controlliamo 5.5.2 perch? in questo dataset non ce l'abbiamo

dim(EIS_Data)
dim(EIS_lavoro)


#cercare modo per creare nuova variabile con valori in specifiche variabili
rm(IS_lavoro)

EIS_lavoro <-transform(
  EIS_lavoro, Summary_innovation_index= ifelse(EIS_Data$Indicator =="0 Summary Innovation Index", EIS_Data$Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Human_resources= ifelse(EIS_Data$Indicator=="1.1 Human resources", EIS_Data$Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Attractive_research_system= ifelse(Indicator== "1.2 Attractive research systems", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Digitalisation= ifelse(Indicator== "1.3 Digitalisation", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Finance_and_support= ifelse(Indicator== "2.1 Finance and support", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Firm_investments= ifelse(Indicator== "2.2 Firm investments", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Use_of_Information_Technologies= ifelse(Indicator== "2.3 Use of information technologies", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Innovators= ifelse(Indicator== "3.1 Innovators", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Linkages= ifelse(Indicator== "3.2 Linkages", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Intellectual_assets= ifelse(Indicator== "3.3 Intellectual assets", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Employment_impacts= ifelse(Indicator== "4.1 Employment impacts", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Sales_impact= ifelse(Indicator== "4.2 Sales impacts", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Environmental_sustainibility= ifelse(Indicator== "4.3 Environmental sustainability", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, GDP_per_capita_thousands_SD= ifelse(Indicator== "5.1.1 GDP per capita (thousands) (SD)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Enterprise_births_SD= ifelse(Indicator== "5.2.1 Enterprise births (SD)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, In_house_product_innovators_with_market_novelties_SD= ifelse(Indicator== "5.3.1 In-house product innovators with market novelties (SD)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Ease_of_starting_a_business_SD= ifelse(Indicator== "5.4.1 Ease of starting a business (SD)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Circular_material_use_rate_SD= ifelse(Indicator== "5.5.1 Circular material use rate (SD)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, New_dcotorate_graduates = ifelse(Indicator== "1.1.1 New doctorate graduates", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Population_with_tertiary_education_regional = ifelse(Indicator== "1.1.2 Population with tertiary education (Regional)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Population_involved_in_lifelong_learning_regional = ifelse(Indicator== "1.1.3 Population involved in lifelong learning (Regional)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, International_scientific_copublications_regional = ifelse(Indicator== "1.2.1 International scientific co-publications (Regional)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Scientific_publications_among_the_top_10percent_most_cited_Regional = ifelse(Indicator== "1.2.2 Scientific publications among the top 10% most cited (Regional)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Foreign_doctorate_students_as_a_percentage_of_all_doctorate_students  = ifelse(Indicator== "1.2.3 Foreign doctorate students as a % of all doctorate students", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Broadband_penetration  = ifelse(Indicator== "1.3.1 Broadband penetration", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Individuals_with_above_basic_overall_digital_skills_Regional  = ifelse(Indicator== "1.3.2 Individuals with above basic overall digital skills (Regional)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, RD_expenditure_in_the_public_sector_Regional  = ifelse(Indicator== "2.1.1 R&D expenditure in the public sector (Regional)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Venture_capital_expenditures = ifelse(Indicator== "2.1.2 Venture capital expenditures", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Direct_and_indirect_government_support_of_business_RD = ifelse(Indicator== "2.1.3 Direct and indirect government support of business R&D", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, RD_expenditure_in_the_business_sector_Regional = ifelse(Indicator== "2.2.1 R&D expenditure in the business sector (Regional)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Non_RD_innovation_expenditures_Regional = ifelse(Indicator== "2.2.2 Non-R&D innovation expenditures (Regional)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Innovation_expenditures_per_person_employed_Regional = ifelse(Indicator== "2.2.3 Innovation expenditures per person employed (Regional)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Enterprises_providing_ICT_training  = ifelse(Indicator== "2.3.1 Enterprises providing ICT training", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Employed_ICT_specialists = ifelse(Indicator== "2.3.2 Employed ICT specialists", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, SMEs_introducing_product_innovations_Regional  = ifelse(Indicator== "3.1.1 SMEs introducing product innovations (Regional)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, SMEs_introducing_business_process_innovations_Regional  = ifelse(Indicator== "3.1.2 SMEs introducing business process innovations (Regional)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Innovative_SMEs_collaborating_with_others_Regional = ifelse(Indicator== "3.2.1 Innovative SMEs collaborating with others (Regional)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Public_private_copublications_Regional= ifelse(Indicator== "3.2.2 Public-private co-publications (Regional)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Job_to_job_mobility_of_HRST = ifelse(Indicator== "3.2.3 Job-to-job mobility of HRST", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, PCT_patent_applications_Regional = ifelse(Indicator== "3.3.1 PCT patent applications (Regional)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Trademark_applications_Regional = ifelse(Indicator== "3.3.2 Trademark applications (Regional)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Design_applications_Regional = ifelse(Indicator== "3.3.3 Design applications (Regional)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Employment_in_knowledge_intensive_activities_Regional  = ifelse(Indicator== "4.1.1 Employment in knowledge-intensive activities (Regional)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Employment_in_innovative_enterprises_Regional = ifelse(Indicator== "4.1.2 Employment in innovative enterprises (Regional)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Exports_of_medium_and_high_technology_products = ifelse(Indicator== "4.2.1 Exports of medium and high technology products", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Knowledge_intensive_services_exports = ifelse(Indicator== "4.2.2 Knowledge-intensive services exports", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Sales_of_new_to_market_and_new_to_firm_innovations_Regional = ifelse(Indicator== "4.2.3 Sales of new-to-market and new-to-firm innovations (Regional)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Resource_productivity = ifelse(Indicator== "4.3.1 Resource productivity", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Air_emissions_by_fine_particulates_Regional = ifelse(Indicator== "4.3.2 Air emissions by fine particulates (Regional)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Average_annual_GDP_growth_SD = ifelse(Indicator== "5.1.2 Average annual GDP growth (SD)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Employment_share_Manufacturing_SD = ifelse(Indicator== "5.1.3 Employment share Manufacturing (SD)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Employment_share_High_and_Medium_high_tech_SD = ifelse(Indicator== "5.1.4 Employment share High and Medium high-tech (SD)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Employment_share_Services_SD = ifelse(Indicator== "5.1.5 Employment share Services (SD)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Employment_share_Knowledge_intensive_services_SD = ifelse(Indicator== "5.1.6 Employment share Knowledge-intensive services (SD)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Turnover_share_SMEs_SD = ifelse(Indicator== "5.1.7 Turnover share SMEs (SD)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Turnover_share_large_enterprises_SD  = ifelse(Indicator== "5.1.8 Turnover share large enterprises (SD)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Foreign_controlled_enterprises_share_of_value_added_SD = ifelse(Indicator== "5.1.9 Foreign-controlled enterprises - share of value added (SD)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Total_Entrepreneurial_Activity_SD  = ifelse(Indicator== "5.2.2 Total Entrepreneurial Activity (SD)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, FDI_net_inflows_SD = ifelse(Indicator== "5.2.3 FDI net inflows (SD)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Top_RD_spending_enterprises_SD = ifelse(Indicator== "5.2.4 Top R&D spending enterprises (SD)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro,  Buyer_sophistication_SD  = ifelse(Indicator== "5.2.5 Buyer sophistication (SD)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, In_house_product_innovators_without_market_novelties_SD = ifelse(Indicator== "5.3.2 In-house product innovators without market novelties (SD)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, In_house_business_process_innovators_SD = ifelse(Indicator== "5.3.3 In-house business process innovators (SD)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Innovators_that_do_not_develop_innovations_themselves_SD  = ifelse(Indicator== "5.3.4 Innovators that do not develop innovations themselves (SD)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Innovation_active_non_innovators_SD = ifelse(Indicator== "5.3.5 Innovation active non-innovators (SD)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Non_innovators_with_potential_to_innovate_SD  = ifelse(Indicator== "5.3.6 Non-innovators with potential to innovate (SD)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Non_innovators_without_disposition_to_innovate_SD  = ifelse(Indicator== "5.3.7 Non-innovators without disposition to innovate (SD)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Basic_school_entrepreneurial_education_and_training_SD = ifelse(Indicator== "5.4.2 Basic-school entrepreneurial education and training (SD)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Government_procurement_of_advanced_technology_products_SD = ifelse(Indicator== "5.4.3 Government procurement of advanced technology products (SD)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Rule_of_law_SD = ifelse(Indicator== "5.4.4 Rule of law (SD)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Greenhouse_gas_emissions_intensity_of_energy_consumption_SD = ifelse(Indicator== "5.5.2 Greenhouse gas emissions intensity of energy consumption  (SD)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Eco_Innovation_Index_SD = ifelse(Indicator== "5.5.3 Eco-Innovation Index (SD)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Population_size_SD = ifelse(Indicator== "5.6.1 Population size (SD)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Average_annual_population_growth_SD  = ifelse(Indicator== "5.6.2 Average annual population growth (SD)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Population_density_SD = ifelse(Indicator== "5.6.3 Population density (SD)", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Environment_related_technologies = ifelse(Indicator== "4.3.3 Environment-related technologies", Value, NA))

EIS_lavoro <- transform(
  EIS_lavoro, Greenhouse_gas_emissions_intensity_of_energy_consumption_SD = ifelse(Indicator== "5.5.2 Greenhouse gas emissions intensity of energy consumption (SD)", Value, NA))

dim(EIS_lavoro)
EIS_lavoro_Indicator <-EIS_lavoro
#cancellare la variabile Indicator e aggiustare le altre 

#eliminare la variabile indicator 
EIS_lavoro$Indicator <- NULL
dim(EIS_lavoro)
library(funModeling)
df_status(EIS_lavoro) 

#sistemaer le altri variabili 

options(max.print= 500)
library(funModeling)
df_status(EIS_lavoro) 

#creo due dataset uno con region/region name che hannp 279 valori e altro con country/country name che hanno 40/39 valori
#vediamo per che pease differiscono regione e region name 


#la differenza ? un valore "It" in country che non c'? in Countryname. Non sapendo a cosa si riferisce uso countryname

#creo dataset in base a country name(tolgo country, e tolgo region)

library(dplyr)
prova_lavoro <-EIS_lavoro %>% 
  group_by(Year, CountryName) %>% 
  summarise_all(list(~first(na.omit(.))))

#enterprise birth bosnia ed erzegovina ? NA perch? non c'? nulla nel dataset originale e cos?
#per altri paesi. Le variabili che si vedono NA all'inizio ? perch? sono state rilevate solo per il 2021

unique(prova_lavoro[c("Level")]) 
#level continua ad avere solo un livello(0) --> perch?? 

#sistemare dataset prova_lavoro

#1 metto nulla Value
prova_lavoro$Value <- NULL

#faccio i raggruppamenti per macroregioni

options(pillar.print_max = 100, pillar.print_min = 100)
unique(prova_lavoro[c("RegionName")])

REGION <- ifelse(
  (prova_lavoro$RegionName == "Austria") | (prova_lavoro$RegionName == "Germany") 
  | (prova_lavoro$RegionName == "France") | (prova_lavoro$RegionName == "Belgium")
  | (prova_lavoro$RegionName == "Luxembourg") |(prova_lavoro$RegionName == "Netherlands")
  | (prova_lavoro$RegionName == "Switzerland"), "Europa Centrale", 
  ifelse((prova_lavoro$RegionName == "Italy") | (prova_lavoro$RegionName == "Spain")
         | (prova_lavoro$RegionName == "Portugal") | (prova_lavoro$RegionName == "Greece")
         | (prova_lavoro$RegionName == "Malta") , "Europa Meridionale",
         ifelse((prova_lavoro$RegionName == "United Kingdom") | (prova_lavoro$RegionName == "Sweden") | 
                      (prova_lavoro$RegionName == "Norway") |(prova_lavoro$RegionName == "Iceland") | 
                      (prova_lavoro$RegionName == "Ireland") | (prova_lavoro$RegionName == "Finland") | 
                      (prova_lavoro$RegionName == "Denmark") | (prova_lavoro$RegionName == "Estonia") | 
                      (prova_lavoro$RegionName == "Latvia") | (prova_lavoro$RegionName == "Lithuania"), "Europa Settentrionale",
                      ifelse((prova_lavoro$RegionName == "Bosnia and Herzegovina") | (prova_lavoro$RegionName == "Bulgaria") | 
                                  (prova_lavoro$RegionName == "Croatia") | (prova_lavoro$RegionName == "Czechia") | 
                                  (prova_lavoro$RegionName == "Hungary") | (prova_lavoro$RegionName == "Montenegro") |
                                  (prova_lavoro$RegionName == "North Macedonia") | (prova_lavoro$RegionName == "Poland") | 
                                  (prova_lavoro$RegionName == "Romania") | (prova_lavoro$RegionName == "Serbia") | 
                                  (prova_lavoro$RegionName == "Slovakia") | (prova_lavoro$RegionName == "Slovenia") | 
                                  (prova_lavoro$RegionName == "Ukraine"), "Europa dell'Est", 
                                  ifelse((prova_lavoro$RegionName == "Turkey") | (prova_lavoro$RegionName == "Israel") | 
                                        (prova_lavoro$RegionName == "Cyprus"), "Europa Orientale",
                                       ifelse((prova_lavoro$RegionName == "EU"), "EU", NA))))))

prova_lavoro <- data.frame(prova_lavoro, REGION)
prova_lavoro$REGION<- NULL

#level ha valori 0-1 per OGNI variabile quindi, per ognuna di queste dovrei creare una variabile corrispondnete con 0-1--> non ho ancora capito in base a cosa viene assegnato questo valore

#dimensione: istruzione e ricerca 

EIS_istruzione <- subset(EIS_Data, Indicator %in% c("1.1 Human resources", 
                                                "1.2 Attractive research systems", 
                                                "1.1.1 New doctorate graduates",                                        
                                                "1.1.2 Population with tertiary education (Regional)",                  
                                                "1.1.3 Population involved in lifelong learning (Regional)",                                       
                                                "1.2.1 International scientific co-publications (Regional)",             
                                                "1.2.2 Scientific publications among the top 10% most cited (Regional)", 
                                                "1.2.3 Foreign doctorate students as a % of all doctorate students", 
                                                "3.2.2 Public-private co-publications (Regional)"))


EIS_istruzione <- transform(
  EIS_istruzione, Human_resources= ifelse(Indicator=="1.1 Human resources", EIS_Data$Value, NA))

EIS_istruzione <- transform(
  EIS_istruzione, Attractive_research_system= ifelse(Indicator== "1.2 Attractive research systems", Value, NA))

EIS_istruzione <- transform(
  EIS_istruzione, New_dcotorate_graduates = ifelse(Indicator== "1.1.1 New doctorate graduates", Value, NA))

EIS_istruzione <- transform(
  EIS_istruzione, Population_with_tertiary_education_regional = ifelse(Indicator== "1.1.2 Population with tertiary education (Regional)", Value, NA))

EIS_istruzione <- transform(
  EIS_istruzione, Population_involved_in_lifelong_learning_regional = ifelse(Indicator== "1.1.3 Population involved in lifelong learning (Regional)", Value, NA))

EIS_istruzione <- transform(
  EIS_istruzione, International_scientific_copublications_regional = ifelse(Indicator== "1.2.1 International scientific co-publications (Regional)", Value, NA))

EIS_istruzione <- transform(
  EIS_istruzione, Scientific_publications_among_the_top_10percent_most_cited_Regional = ifelse(Indicator== "1.2.2 Scientific publications among the top 10% most cited (Regional)", Value, NA))

EIS_istruzione <- transform(
  EIS_istruzione, Foreign_doctorate_students_as_a_percentage_of_all_doctorate_students  = ifelse(Indicator== "1.2.3 Foreign doctorate students as a % of all doctorate students", Value, NA))

EIS_istruzione <- transform(
  EIS_istruzione, Public_private_copublications_Regional= ifelse(Indicator== "3.2.2 Public-private co-publications (Regional)", Value, NA))



library(dplyr)
prova_lavoro_istruzione <-EIS_istruzione %>% 
  group_by(Year, CountryName) %>% 
  summarise_all(list(~first(na.omit(.))))


#dimensione: innovazione tecnologica ed ambientale 

EIS_innovazione <- subset(EIS_Data, Indicator %in% c("1.3 Digitalisation", 
                                                "2.3 Use of information technologies", 
                                                "3.1 Innovators", 
                                                "3.2 Linkages",
                                                "1.3.1 Broadband penetration",
                                                "3.3 Intellectual assets", 
                                                "4.3 Environmental sustainability",
                                                "5.3.1 In-house product innovators with market novelties (SD)",                                    
                                                "1.3.2 Individuals with above basic overall digital skills (Regional)",                                  
                                                "2.3.1 Enterprises providing ICT training",                                                   
                                                "3.1.1 SMEs introducing product innovations (Regional)",                
                                                "3.1.2 SMEs introducing business process innovations (Regional)",                                                    
                                                "3.2.1 Innovative SMEs collaborating with others (Regional)", 
                                                "4.1.1 Employment in knowledge-intensive activities (Regional)",                                         
                                                "3.2.2 Public-private co-publications (Regional)",                      
                                                "3.2.3 Job-to-job mobility of HRST",                                           
                                                "3.3.1 PCT patent applications (Regional)",                             
                                                "3.3.2 Trademark applications (Regional)",                              
                                                "3.3.3 Design applications (Regional)",                                                  
                                                "4.2.1 Exports of medium and high technology products",                
                                                "4.2.2 Knowledge-intensive services exports",                            
                                                "4.2.3 Sales of new-to-market and new-to-firm innovations (Regional)",      
                                                "5.3.2 In-house product innovators without market novelties (SD)",       
                                                "5.3.3 In-house business process innovators (SD)",                       
                                                "5.3.4 Innovators that do not develop innovations themselves (SD)",      
                                                "5.3.5 Innovation active non-innovators (SD)",                           
                                                "5.3.6 Non-innovators with potential to innovate (SD)",                  
                                                "5.3.7 Non-innovators without disposition to innovate (SD)",          
                                                "5.4.3 Government procurement of advanced technology products (SD)",  
                                                "5.5.2 Greenhouse gas emissions intensity of energy consumption  (SD)",  
                                                "5.5.3 Eco-Innovation Index (SD)",                                        
                                                "4.3.3 Environment-related technologies",                                
                                                "5.5.2 Greenhouse gas emissions intensity of energy consumption (SD)",
                                                "4.3.2 Air emissions by fine particulates (Regional)", 
                                                "5.5.1 Circular material use rate (SD)",
                                                "2.2.3 Innovation expenditures per person employed (Regional)",
                                                "4.1.2 Employment in innovative enterprises (Regional)",
                                                "5.1.6 Employment share Knowledge-intensive services (SD)",
                                                "2.1.1 R&D expenditure in the public sector (Regional)"))


EIS_innovazione <- transform(
  EIS_innovazione, Digitalisation= ifelse(Indicator== "1.3 Digitalisation", Value, NA))

EIS_innovazione <- transform(
  EIS_innovazione, Use_of_Information_Technologies= ifelse(Indicator== "2.3 Use of information technologies", Value, NA))

EIS_innovazione <- transform(
  EIS_innovazione, Innovators= ifelse(Indicator== "3.1 Innovators", Value, NA))

EIS_innovazione <- transform(
  EIS_innovazione, Linkages= ifelse(Indicator== "3.2 Linkages", Value, NA))

EIS_innovazione <- transform(
  EIS_innovazione, Intellectual_assets= ifelse(Indicator== "3.3 Intellectual assets", Value, NA))

EIS_innovazione <- transform(
  EIS_innovazione, Environmental_sustainibility= ifelse(Indicator== "4.3 Environmental sustainability", Value, NA))

EIS_innovazione <- transform(
  EIS_innovazione, In_house_product_innovators_with_market_novelties_SD= ifelse(Indicator== "5.3.1 In-house product innovators with market novelties (SD)", Value, NA))

EIS_innovazione <- transform(
  EIS_innovazione, Broadband_penetration  = ifelse(Indicator== "1.3.1 Broadband penetration", Value, NA))

EIS_innovazione <- transform(
  EIS_innovazione, Individuals_with_above_basic_overall_digital_skills_Regional  = ifelse(Indicator== "1.3.2 Individuals with above basic overall digital skills (Regional)", Value, NA))

EIS_innovazione <- transform(
  EIS_innovazione, Enterprises_providing_ICT_training  = ifelse(Indicator== "2.3.1 Enterprises providing ICT training", Value, NA))

EIS_innovazione <- transform(
  EIS_innovazione, SMEs_introducing_product_innovations_Regional  = ifelse(Indicator== "3.1.1 SMEs introducing product innovations (Regional)", Value, NA))

EIS_innovazione<- transform(
  EIS_innovazione, SMEs_introducing_business_process_innovations_Regional  = ifelse(Indicator== "3.1.2 SMEs introducing business process innovations (Regional)", Value, NA))

EIS_innovazione <- transform(
  EIS_innovazione, Innovative_SMEs_collaborating_with_others_Regional = ifelse(Indicator== "3.2.1 Innovative SMEs collaborating with others (Regional)", Value, NA))

EIS_innovazione <- transform(
  EIS_innovazione, Employment_in_knowledge_intensive_activities_Regional = ifelse(Indicator== "4.1.1 Employment in knowledge-intensive activities (Regional)", Value, NA))

EIS_innovazione <- transform(
  EIS_innovazione, Public_private_copublications_Regional= ifelse(Indicator== "3.2.2 Public-private co-publications (Regional)", Value, NA))

EIS_innovazione <- transform(
  EIS_innovazione, Job_to_job_mobility_of_HRST = ifelse(Indicator== "3.2.3 Job-to-job mobility of HRST", Value, NA))

EIS_innovazione <- transform(
  EIS_innovazione, PCT_patent_applications_Regional = ifelse(Indicator== "3.3.1 PCT patent applications (Regional)", Value, NA))

EIS_innovazione <- transform(
  EIS_innovazione, Trademark_applications_Regional = ifelse(Indicator== "3.3.2 Trademark applications (Regional)", Value, NA))

EIS_innovazione <- transform(
  EIS_innovazione, Design_applications_Regional = ifelse(Indicator== "3.3.3 Design applications (Regional)", Value, NA))

EIS_innovazione <- transform(
  EIS_innovazione, Exports_of_medium_and_high_technology_products = ifelse(Indicator== "4.2.1 Exports of medium and high technology products", Value, NA))

EIS_innovazione <- transform(
  EIS_innovazione, Knowledge_intensive_services_exports = ifelse(Indicator== "4.2.2 Knowledge-intensive services exports", Value, NA))

EIS_innovazione <- transform(
  EIS_innovazione, Sales_of_new_to_market_and_new_to_firm_innovations_Regional = ifelse(Indicator== "4.2.3 Sales of new-to-market and new-to-firm innovations (Regional)", Value, NA))

EIS_innovazione <- transform(
  EIS_innovazione, Air_emissions_by_fine_particulates_Regional = ifelse(Indicator== "4.3.2 Air emissions by fine particulates (Regional)", Value, NA))

EIS_innovazione <- transform(
  EIS_innovazione, Employment_share_Knowledge_intensive_services_SD = ifelse(Indicator== "5.1.6 Employment share Knowledge-intensive services (SD)", Value, NA))

EIS_innovazione <- transform(
  EIS_innovazione, In_house_product_innovators_without_market_novelties_SD = ifelse(Indicator== "5.3.2 In-house product innovators without market novelties (SD)", Value, NA))

EIS_innovazione <- transform(
  EIS_innovazione, In_house_business_process_innovators_SD = ifelse(Indicator== "5.3.3 In-house business process innovators (SD)", Value, NA))

EIS_innovazione <- transform(
  EIS_innovazione, Innovators_that_do_not_develop_innovations_themselves_SD  = ifelse(Indicator== "5.3.4 Innovators that do not develop innovations themselves (SD)", Value, NA))

EIS_innovazione <- transform(
  EIS_innovazione, Innovation_active_non_innovators_SD = ifelse(Indicator== "5.3.5 Innovation active non-innovators (SD)", Value, NA))

EIS_innovazione <- transform(
  EIS_innovazione, Non_innovators_with_potential_to_innovate_SD  = ifelse(Indicator== "5.3.6 Non-innovators with potential to innovate (SD)", Value, NA))

EIS_innovazione <- transform(
  EIS_innovazione, Non_innovators_without_disposition_to_innovate_SD  = ifelse(Indicator== "5.3.7 Non-innovators without disposition to innovate (SD)", Value, NA))

EIS_innovazione <- transform(
  EIS_innovazione, Government_procurement_of_advanced_technology_products_SD = ifelse(Indicator== "5.4.3 Government procurement of advanced technology products (SD)", Value, NA))

EIS_innovazione <- transform(
  EIS_innovazione, Greenhouse_gas_emissions_intensity_of_energy_consumption_SD = ifelse(Indicator== "5.5.2 Greenhouse gas emissions intensity of energy consumption  (SD)", Value, NA))

EIS_innovazione <- transform(
  EIS_innovazione, Eco_Innovation_Index_SD = ifelse(Indicator== "5.5.3 Eco-Innovation Index (SD)", Value, NA))

EIS_innovazione <- transform(
  EIS_innovazione, Environment_related_technologies = ifelse(Indicator== "4.3.3 Environment-related technologies", Value, NA))

EIS_innovazione <- transform(
  EIS_innovazione, Greenhouse_gas_emissions_intensity_of_energy_consumption_SD = ifelse(Indicator== "5.5.2 Greenhouse gas emissions intensity of energy consumption (SD)", Value, NA))

EIS_innovazione <- transform(
  EIS_innovazione, Circular_material_use_rate_SD = ifelse(Indicator== "5.5.1 Circular material use rate (SD)", Value, NA))

EIS_innovazione<- transform(
  EIS_innovazione, Innovation_expenditures_per_person_employed_Regional = ifelse(Indicator== "2.2.3 Innovation expenditures per person employed (Regional)", Value, NA))

EIS_innovazione <- transform(
  EIS_innovazione, Employment_in_innovative_enterprises_Regional = ifelse(Indicator== "4.1.2 Employment in innovative enterprises (Regional)", Value, NA))

EIS_innovazione <- transform(
  EIS_innovazione, RD_expenditure_in_the_public_sector_Regional = ifelse(Indicator== "2.1.1 R&D expenditure in the public sector (Regional)", Value, NA))



library(dplyr)
prova_lavoro_innovazione <-EIS_innovazione%>% 
  group_by(Year, CountryName) %>% 
  summarise_all(list(~first(na.omit(.))))




#dimensione: economia + business 

  EIS_economia <- subset(EIS_Data, Indicator %in% c("2.1 Finance and support",
                                                    "2.2 Firm investments",  
                                                    "4.1 Employment impacts", 
                                                    "4.2 Sales impacts", 
                                                    "5.2.1 Enterprise births (SD)", 
                                                    "5.4.1 Ease of starting a business (SD)",                 
                                                    "2.1.2 Venture capital expenditures",                                 
                                                    "2.1.3 Direct and indirect government support of business R&D",                                                
                                                    "2.2.1 R&D expenditure in the business sector (Regional)",              
                                                    "2.2.2 Non-R&D innovation expenditures (Regional)",                                   
                                                    "2.3.2 Employed ICT specialists", 
                                                    "4.3.1 Resource productivity",                            
                                                    "5.1.2 Average annual GDP growth (SD)",                                  
                                                    "5.1.3 Employment share Manufacturing (SD)",                            
                                                    "5.1.4 Employment share High and Medium high-tech (SD)",                 
                                                    "5.1.5 Employment share Services (SD)",              
                                                    "5.1.7 Turnover share SMEs (SD)",                                        
                                                    "5.1.8 Turnover share large enterprises (SD)",                          
                                                    "5.1.9 Foreign-controlled enterprises - share of value added (SD)",                                     
                                                    "5.2.2 Total Entrepreneurial Activity (SD)",                             
                                                    "5.2.3 FDI net inflows (SD)",                                            
                                                    "5.2.4 Top R&D spending enterprises (SD)",
                                                    "5.4.2 Basic-school entrepreneurial education and training (SD)",
                                                    "5.2.5 Buyer sophistication (SD)",
                                                    "5.1.1 GDP per capita (thousands) (SD)"))



EIS_economia <- transform(
  EIS_economia, Finance_and_support= ifelse(Indicator== "2.1 Finance and support", Value, NA))

EIS_economia <- transform(
  EIS_economia, Firm_investments= ifelse(Indicator== "2.2 Firm investments", Value, NA))

EIS_economia <- transform(
  EIS_economia, Employment_impacts= ifelse(Indicator== "4.1 Employment impacts", Value, NA))

EIS_economia <- transform(
  EIS_economia, Sales_impact= ifelse(Indicator== "4.2 Sales impacts", Value, NA))

EIS_economia <- transform(
  EIS_economia, GDP_per_capita_thousands_SD= ifelse(Indicator== "5.1.1 GDP per capita (thousands) (SD)", Value, NA))

EIS_economia <- transform(
  EIS_economia, Enterprise_births_SD= ifelse(Indicator== "5.2.1 Enterprise births (SD)", Value, NA))

EIS_economia <- transform(
  EIS_economia, Ease_of_starting_a_business_SD= ifelse(Indicator== "5.4.1 Ease of starting a business (SD)", Value, NA))

EIS_economia <- transform(
  EIS_economia, Venture_capital_expenditures = ifelse(Indicator== "2.1.2 Venture capital expenditures", Value, NA))

EIS_economia <- transform(
  EIS_economia, Direct_and_indirect_government_support_of_business_RD = ifelse(Indicator== "2.1.3 Direct and indirect government support of business R&D", Value, NA))

EIS_economia <- transform(
  EIS_economia, RD_expenditure_in_the_business_sector_Regional = ifelse(Indicator== "2.2.1 R&D expenditure in the business sector (Regional)", Value, NA))

EIS_economia <- transform(
  EIS_economia, Non_RD_innovation_expenditures_Regional = ifelse(Indicator== "2.2.2 Non-R&D innovation expenditures (Regional)", Value, NA))

EIS_economia <- transform(
  EIS_economia, Employed_ICT_specialists = ifelse(Indicator== "2.3.2 Employed ICT specialists", Value, NA))

EIS_economia <- transform(
  EIS_economia, Resource_productivity = ifelse(Indicator== "4.3.1 Resource productivity", Value, NA))

EIS_economia <- transform(
  EIS_economia, Average_annual_GDP_growth_SD = ifelse(Indicator== "5.1.2 Average annual GDP growth (SD)", Value, NA))

EIS_economia <- transform(
  EIS_economia, Employment_share_Manufacturing_SD = ifelse(Indicator== "5.1.3 Employment share Manufacturing (SD)", Value, NA))

EIS_economia <- transform(
  EIS_economia, Employment_share_Services_SD = ifelse(Indicator== "5.1.5 Employment share Services (SD)", Value, NA))

EIS_economia <- transform(
  EIS_economia, Turnover_share_SMEs_SD = ifelse(Indicator== "5.1.7 Turnover share SMEs (SD)", Value, NA))

EIS_economia <- transform(
  EIS_economia, Turnover_share_large_enterprises_SD  = ifelse(Indicator== "5.1.8 Turnover share large enterprises (SD)", Value, NA))

EIS_economia <- transform(
  EIS_economia, Foreign_controlled_enterprises_share_of_value_added_SD = ifelse(Indicator== "5.1.9 Foreign-controlled enterprises - share of value added (SD)", Value, NA))

EIS_economia <- transform(
  EIS_economia, Total_Entrepreneurial_Activity_SD  = ifelse(Indicator== "5.2.2 Total Entrepreneurial Activity (SD)", Value, NA))

EIS_economia <- transform(
  EIS_economia, FDI_net_inflows_SD = ifelse(Indicator== "5.2.3 FDI net inflows (SD)", Value, NA))

EIS_economia <- transform(
  EIS_economia, Top_RD_spending_enterprises_SD = ifelse(Indicator== "5.2.4 Top R&D spending enterprises (SD)", Value, NA))

EIS_economia <- transform(
  EIS_economia,  Buyer_sophistication_SD  = ifelse(Indicator== "5.2.5 Buyer sophistication (SD)", Value, NA))

EIS_economia <- transform(
  EIS_economia, Basic_school_entrepreneurial_education_and_training_SD = ifelse(Indicator== "5.4.2 Basic-school entrepreneurial education and training (SD)", Value, NA))

EIS_economia <- transform(
  EIS_economia, Employment_share_High_and_Medium_high_tech_SD = ifelse(Indicator==  "5.1.4 Employment share High and Medium high-tech (SD)", Value, NA))



library(dplyr)
prova_lavoro_economia <-EIS_economia%>% 
  group_by(Year, CountryName) %>% 
  summarise_all(list(~first(na.omit(.))))


#dimension: demografia 

EIS_demografia <- subset(EIS_Data, Indicator %in% c("5.6.1 Population size (SD)",                                           
                                                  "5.6.2 Average annual population growth (SD)",                           
                                                  "5.6.3 Population density (SD)",
                                                  "5.4.4 Rule of law (SD)"))

EIS_demografia <- transform(
  EIS_demografia, Population_size_SD= ifelse(Indicator== "5.6.1 Population size (SD)", Value, NA))

EIS_demografia <- transform(
  EIS_demografia, Average_annual_population_growth_SD= ifelse(Indicator== "5.6.2 Average annual population growth (SD)", Value, NA))

EIS_demografia <- transform(
  EIS_demografia, Population_density_SD= ifelse(Indicator== "5.6.3 Population density (SD)", Value, NA))

EIS_demografia<- transform(
  EIS_demografia, Rule_of_law_SD= ifelse(Indicator== "5.4.4 Rule of law (SD)", Value, NA))


library(dplyr)
prova_lavoro_demografia <-EIS_demografia%>% 
  group_by(Year, CountryName) %>% 
  summarise_all(list(~first(na.omit(.))))

#tolgo le variabili che non mi interessano
EIS_demografia <-EIS_demografia[, -c(1, 5, 7, 8, 10)]
EIS_economia<- EIS_economia[, -c(1, 5, 7, 8, 10)]
EIS_innovazione <- EIS_innovazione[, -c(1, 5, 7, 8, 10)]
EIS_istruzione <- EIS_istruzione[, -c(1, 5, 7, 8, 10)]

#proviamo a metterle sullo stesso livello 
library(dplyr)
EIS_demografia1 <-EIS_demografia %>% 
  group_by(CountryName, Year) %>% 
  summarise(across(everything(), ~ purrr::reduce(.x, coalesce)))

library(dplyr)
EIS_innovazione1 <-EIS_innovazione %>% 
  group_by(CountryName, Year) %>% 
  summarise(across(everything(), ~ purrr::reduce(.x, coalesce)))

library(dplyr)
EIS_istruzione1 <-EIS_istruzione %>% 
  group_by(CountryName, Year) %>% 
  summarise(across(everything(), ~ purrr::reduce(.x, coalesce)))

library(dplyr)
EIS_economia1 <-EIS_economia %>% 
  group_by(CountryName, Year) %>% 
  summarise(across(everything(), ~ purrr::reduce(.x, coalesce)))


#al posto dei valori nulli metto 0 

EIS_demografia1[is.na(EIS_demografia1)] = 0
EIS_innovazione1[is.na(EIS_innovazione1)] = 0
EIS_economia1[is.na(EIS_economia1)] = 0
EIS_istruzione1[is.na(EIS_istruzione1)] = 0

#POSET demografia
#install.packages("parsec")
library(parsec) 
library(netrankr)
rownames(EIS_demografia1)<-c( "Austria",               
                    "Belgium",               
                    "Bosnia and Herzegovina",
                    "Bulgaria",              
                    "Croatia",               
                    "Cyprus",                
                    "Czechia",               
                    "Denmark",               
                    "Estonia",               
                    "EU",                    
                    "Finland",               
                    "France",                
                    "Germany",               
                    "Greece",                
                    "Hungary",                
                    "Iceland",               
                    "Ireland",               
                    "Israel",                
                    "Italy",                 
                    "Latvia",                
                    "Lithuania",             
                    "Luxembourg",            
                    "Malta",                 
                    "Montenegro",            
                    "Netherlands",           
                    "North Macedonia",      
                    "Norway",                
                    "Poland",                
                    "Portugal",              
                    "Romania",               
                    "Serbia",               
                    "Slovakia",              
                    "Slovenia",              
                    "Spain",                 
                    "Sweden",                 
                    "Turkey",                
                    "Ukraine", "United Kingdom")
X_dem <- rownames(EIS_demografia1) 
r_dem <- function(x_dem, y_dem) all(EIS_demografia1[x_dem,] <= EIS_demografia1[y_dem,]) 
r_dem<- Vectorize(r_dem) 
Z_dem <- outer(X_dem, X_dem, FUN = r_dem) 
dimnames(Z_dem) <- list(X_dem, X_dem) 
Z_dem <- validate.partialorder.incidence(Z_dem)
M_dem <- MRP(Z_dem, method = 'approx') 
rownames(M_dem)<-c( "Austria",               
                              "Belgium",               
                              "Bosnia and Herzegovina",
                              "Bulgaria",              
                              "Croatia",               
                              "Cyprus",                
                              "Czechia",               
                              "Denmark",               
                              "Estonia",               
                              "EU",                    
                              "Finland",               
                              "France",                
                              "Germany",               
                              "Greece",                
                              "Hungary",                
                              "Iceland",               
                              "Ireland",               
                              "Israel",                
                              "Italy",                 
                              "Latvia",                
                              "Lithuania",             
                              "Luxembourg",            
                              "Malta",                 
                              "Montenegro",            
                              "Netherlands",           
                              "North Macedonia",      
                              "Norway",                
                              "Poland",                
                              "Portugal",              
                              "Romania",               
                              "Serbia",               
                              "Slovakia",              
                              "Slovenia",              
                              "Spain",                 
                              "Sweden",                 
                              "Turkey",                
                              "Ukraine", "United Kingdom")# se non metto approx non funziona 
avr_height_dem <- colSums(M_dem) 
eigenvector_dem <- abs(svd(M_dem)$v[,1])

#hasse 
install.packages("hasseDiagram")
install.packages("Rgraphviz")
#install.packages("BiocManager")
#BiocManager::install("Rgraphviz")
require(Rgraphviz)
require(hasseDiagram)
hasse_dem <-hasse(M_dem, labels = c("Austria",               
                        "Belgium",               
                        "Bosnia and Herzegovina",
                        "Bulgaria",              
                        "Croatia",               
                        "Cyprus",                
                        "Czechia",               
                        "Denmark",               
                        "Estonia",               
                        "EU",                    
                        "Finland",               
                        "France",                
                        "Germany",               
                        "Greece",                
                        "Hungary",                
                        "Iceland",               
                        "Ireland",               
                        "Israel",                
                        "Italy",                 
                        "Latvia",                
                        "Lithuania",             
                        "Luxembourg",            
                        "Malta",                 
                        "Montenegro",            
                        "Netherlands",           
                        "North Macedonia",      
                        "Norway",                
                        "Poland",                
                        "Portugal",              
                        "Romania",               
                        "Serbia",               
                        "Slovakia",              
                        "Slovenia",              
                        "Spain",                 
                        "Sweden",                 
                        "Turkey",                
                        "Ukraine", "United Kingdom"))


#POSET innovazione
#install.packages("parsec")
library(parsec) 
library(netrankr)
rownames(EIS_innovazione1)<-c( "Austria- 2014",
                               "Austria- 2015",
                               "Austria- 2016",
                               "Austria- 2017",
                               "Austria- 2018",
                               "Austria- 2019",
                               "Austria- 2020",
                               "Austria- 2021",
                              "Belgium -2014",
                              "Belgium -2015",
                              "Belgium -2016",
                              "Belgium -2017",
                              "Belgium -2018",
                              "Belgium -2019",
                              "Belgium -2020",
                              "Belgium -2021",
                              "Bosnia and Herzegovina-2014",
                              "Bosnia and Herzegovina-2015",
                              "Bosnia and Herzegovina-2016",
                              "Bosnia and Herzegovina-2017",
                              "Bosnia and Herzegovina-2018",
                              "Bosnia and Herzegovina-2019",
                              "Bosnia and Herzegovina-2020",
                              "Bosnia and Herzegovina-2021",
                              "Bulgaria-2014", 
                              "Bulgaria-2015",  
                              "Bulgaria-2016",  
                              "Bulgaria-2017",  
                              "Bulgaria-2018",  
                              "Bulgaria-2019",  
                              "Bulgaria-2020",  
                              "Bulgaria-2021",  
                              "Croatia-2014",
                              "Croatia-2015",  
                              "Croatia-2016",  
                              "Croatia-2017",  
                              "Croatia-2018",  
                              "Croatia-2019",  
                              "Croatia-2020",  
                              "Croatia-2021",  
                              "Cyprus-2014",
                              "Cyprus-2015", 
                              "Cyprus-2016", 
                              "Cyprus-2017", 
                              "Cyprus-2018", 
                              "Cyprus-2019", 
                              "Cyprus-2020", 
                              "Cyprus-2021", 
                              "Czechia-2014",  
                              "Czechia-2015", 
                              "Czechia-2016", 
                              "Czechia-2017", 
                              "Czechia-2018", 
                              "Czechia-2019", 
                              "Czechia-2020", 
                              "Czechia-2021", 
                              "Denmark-2014",
                              "Denmark-2015",
                              "Denmark-2016",
                              "Denmark-2017",
                              "Denmark-2018",
                              "Denmark-2019",
                              "Denmark-2020",
                              "Denmark-2021",
                              "Estonia-2014",
                              "Estonia-2015",
                              "Estonia-2016",
                              "Estonia-2017",
                              "Estonia-2018",
                              "Estonia-2019",
                              "Estonia-2020",
                              "Estonia-2021",
                              "EU-2014",
                              "EU-2015", 
                              "EU-2016", 
                              "EU-2017", 
                              "EU-2018", 
                              "EU-2019", 
                              "EU-2020", 
                              "EU-2021", 
                              "Finland-2014", 
                              "Finland-2015",
                              "Finland-2016",
                              "Finland-2017",
                              "Finland-2018",
                              "Finland-2019",
                              "Finland-2020",
                              "Finland-2021",
                              "France-2014", 
                              "France-2015", 
                              "France-2016", 
                              "France-2017", 
                              "France-2018", 
                              "France-2019", 
                              "France-2020", 
                              "France-2021", 
                              "Germany-2014", 
                              "Germany-2015",  
                              "Germany-2016",  
                              "Germany-2017",  
                              "Germany-2018",  
                              "Germany-2019",  
                              "Germany-2020",  
                              "Germany-2021",  
                              "Greece-2014",
                              "Greece-2015",
                              "Greece-2016",
                              "Greece-2017",
                              "Greece-2018",
                              "Greece-2019",
                              "Greece-2020",
                              "Greece-2021",
                              "Hungary-2014", 
                              "Hungary-2015", 
                              "Hungary-2016", 
                              "Hungary-2017", 
                              "Hungary-2018", 
                              "Hungary-2019", 
                              "Hungary-2020", 
                              "Hungary-2021", 
                              "Iceland-2014",
                              "Iceland-2015",
                              "Iceland-2016",
                              "Iceland-2017",
                              "Iceland-2018",
                              "Iceland-2019",
                              "Iceland-2020",
                              "Iceland-2021",
                              "Ireland-2014", 
                              "Ireland-2015",
                              "Ireland-2016",
                              "Ireland-2017",
                              "Ireland-2018",
                              "Ireland-2019",
                              "Ireland-2020",
                              "Ireland-2021",
                              "Israel-2014",
                              "Israel-2015", 
                              "Israel-2016", 
                              "Israel-2017", 
                              "Israel-2018", 
                              "Israel-2019", 
                              "Israel-2020",
                              "Israel-2021", 
                              "Italy-2014",
                              "Italy-2015",
                              "Italy-2016",
                              "Italy-2017",
                              "Italy-2018",
                              "Italy-2019",
                              "Italy-2020",
                              "Italy-2021",
                              "Latvia-2014",
                              "Latvia-2015",
                              "Latvia-2016",
                              "Latvia-2017",
                              "Latvia-2018",
                              "Latvia-2019",
                              "Latvia-2020",
                              "Latvia-2021",
                              "Lithuania-2014",
                              "Lithuania-2015",
                              "Lithuania-2016",
                              "Lithuania-2017",
                              "Lithuania-2018",
                              "Lithuania-2019",
                              "Lithuania-2020",
                              "Lithuania-2021",
                              "Luxembourg-2014",
                              "Luxembourg-2015",
                              "Luxembourg-2016",
                              "Luxembourg-2017",
                              "Luxembourg-2018",
                              "Luxembourg-2019",
                              "Luxembourg-2020",
                              "Luxembourg-2021",
                              "Malta-2014",
                              "Malta-2015",
                              "Malta-2016",
                              "Malta-2017",
                              "Malta-2018",
                              "Malta-2019",
                              "Malta-2020",
                              "Malta-2021",
                              "Montenegro-2014",
                              "Montenegro-2015", 
                              "Montenegro-2016", 
                              "Montenegro-2017", 
                              "Montenegro-2018", 
                              "Montenegro-2019", 
                              "Montenegro-2020", 
                              "Montenegro-2021", 
                              "Netherlands-2014",
                              "Netherlands-2015",
                              "Netherlands-2016",
                              "Netherlands-2017",
                              "Netherlands-2018",
                              "Netherlands-2019",
                              "Netherlands-2020",
                              "Netherlands-2021",
                              "North Macedonia-2014",
                              "North Macedonia-2015",
                              "North Macedonia-2016",
                              "North Macedonia-2017",
                              "North Macedonia-2018",
                              "North Macedonia-2019",
                              "North Macedonia-2020",
                              "North Macedonia-2021",
                              "Norway-2014",
                              "Norway-2015",  
                              "Norway-2016",  
                              "Norway-2017",  
                              "Norway-2018",  
                              "Norway-2019",  
                              "Norway-2020",  
                              "Norway-2021",  
                              "Poland-2014",
                              "Poland-2015",  
                              "Poland-2016",  
                              "Poland-2017",  
                              "Poland-2018",  
                              "Poland-2019",  
                              "Poland-2020",  
                              "Poland-2021",  
                              "Portugal-2014",
                              "Portugal-2015", 
                              "Portugal-2016", 
                              "Portugal-2017", 
                              "Portugal-2018", 
                              "Portugal-2019", 
                              "Portugal-2020", 
                              "Portugal-2021", 
                              "Romania-2014",
                              "Romania-2015",
                              "Romania-2016",
                              "Romania-2017",
                              "Romania-2018",
                              "Romania-2019",
                              "Romania-2020",
                              "Romania-2021",
                              "Serbia-2014",
                              "Serbia-2015",
                              "Serbia-2016",
                              "Serbia-2017",
                              "Serbia-2018",
                              "Serbia-2019",
                              "Serbia-2020",
                              "Serbia-2021",
                              "Slovakia-2014", 
                              "Slovakia-2015",
                              "Slovakia-2016",
                              "Slovakia-2017",
                              "Slovakia-2018",
                              "Slovakia-2019",
                              "Slovakia-2020",
                              "Slovakia-2021",
                              "Slovenia-2014", 
                              "Slovenia-2015",
                              "Slovenia-2016",
                              "Slovenia-2017",
                              "Slovenia-2018",
                              "Slovenia-2019",
                              "Slovenia-2020",
                              "Slovenia-2021",
                              "Spain-2014", 
                              "Spain-2015",
                              "Spain-2016",
                              "Spain-2017",
                              "Spain-2018",
                              "Spain-2019",
                              "Spain-2020",
                              "Spain-2021",
                              "Sweden-2014",   
                              "Sweden-2015",
                              "Sweden-2016",
                              "Sweden-2017",
                              "Sweden-2018",
                              "Sweden-2019",
                              "Sweden-2020",
                              "Sweden-2021",
                              "Switzerland-2014",
                              "Switzerland-2015",
                              "Switzerland-2016",
                              "Switzerland-2017",
                              "Switzerland-2018",
                              "Switzerland-2019",
                              "Switzerland-2020",
                              "Switzerland-2021",
                              "Turkey-2014", 
                              "Turkey-2015",  
                              "Turkey-2016",  
                              "Turkey-2017",  
                              "Turkey-2018",  
                              "Turkey-2019",  
                              "Turkey-2020",  
                              "Turkey-2021",  
                              "Ukraine-2014",
                              "Ukraine-2015",
                              "Ukraine-2016",
                              "Ukraine-2017",
                              "Ukraine-2018",
                              "Ukraine-2019",
                              "Ukraine-2020",
                              "Ukraine-2021",
                              "United Kingdom-2014",
                              "United Kingdom-2015",
                              "United Kingdom-2016",
                              "United Kingdom-2017",
                              "United Kingdom-2018",
                              "United Kingdom-2019",
                              "United Kingdom-2020",
                              "United Kingdom-2021")
X_inn <- rownames(EIS_innovazione1) 
r_inn <- function(x_inn, y_inn) all(EIS_innovazione1[x_inn,] <= EIS_innovazione1[y_inn,]) 
r_inn<- Vectorize(r_inn) 
Z_inn <- outer(X_inn, X_inn, FUN = r_inn) 
dimnames(Z_inn) <- list(X_inn, X_inn) 
Z_inn <- validate.partialorder.incidence(Z_inn)
M_inn <- MRP(Z_inn, method = 'approx') # se non metto approx non funziona 
avr_height_inn <- colSums(M_inn) 
eigenvector_inn <- abs(svd(M_inn)$v[,1])


#POSET istruzione
#install.packages("parsec")
library(parsec) 
library(netrankr)
rownames(EIS_istruzione1)<-c( "Austria- 2014",
                               "Austria- 2015",
                               "Austria- 2016",
                               "Austria- 2017",
                               "Austria- 2018",
                               "Austria- 2019",
                               "Austria- 2020",
                               "Austria- 2021",
                               "Belgium -2014",
                               "Belgium -2015",
                               "Belgium -2016",
                               "Belgium -2017",
                               "Belgium -2018",
                               "Belgium -2019",
                               "Belgium -2020",
                               "Belgium -2021",
                               "Bosnia and Herzegovina-2014",
                               "Bosnia and Herzegovina-2015",
                               "Bosnia and Herzegovina-2016",
                               "Bosnia and Herzegovina-2017",
                               "Bosnia and Herzegovina-2018",
                               "Bosnia and Herzegovina-2019",
                               "Bosnia and Herzegovina-2020",
                               "Bosnia and Herzegovina-2021",
                               "Bulgaria-2014", 
                               "Bulgaria-2015",  
                               "Bulgaria-2016",  
                               "Bulgaria-2017",  
                               "Bulgaria-2018",  
                               "Bulgaria-2019",  
                               "Bulgaria-2020",  
                               "Bulgaria-2021",  
                               "Croatia-2014",
                               "Croatia-2015",  
                               "Croatia-2016",  
                               "Croatia-2017",  
                               "Croatia-2018",  
                               "Croatia-2019",  
                               "Croatia-2020",  
                               "Croatia-2021",  
                               "Cyprus-2014",
                               "Cyprus-2015", 
                               "Cyprus-2016", 
                               "Cyprus-2017", 
                               "Cyprus-2018", 
                               "Cyprus-2019", 
                               "Cyprus-2020", 
                               "Cyprus-2021", 
                               "Czechia-2014",  
                               "Czechia-2015", 
                               "Czechia-2016", 
                               "Czechia-2017", 
                               "Czechia-2018", 
                               "Czechia-2019", 
                               "Czechia-2020", 
                               "Czechia-2021", 
                               "Denmark-2014",
                               "Denmark-2015",
                               "Denmark-2016",
                               "Denmark-2017",
                               "Denmark-2018",
                               "Denmark-2019",
                               "Denmark-2020",
                               "Denmark-2021",
                               "Estonia-2014",
                               "Estonia-2015",
                               "Estonia-2016",
                               "Estonia-2017",
                               "Estonia-2018",
                               "Estonia-2019",
                               "Estonia-2020",
                               "Estonia-2021",
                               "EU-2014",
                               "EU-2015", 
                               "EU-2016", 
                               "EU-2017", 
                               "EU-2018", 
                               "EU-2019", 
                               "EU-2020", 
                               "EU-2021", 
                               "Finland-2014", 
                               "Finland-2015",
                               "Finland-2016",
                               "Finland-2017",
                               "Finland-2018",
                               "Finland-2019",
                               "Finland-2020",
                               "Finland-2021",
                               "France-2014", 
                               "France-2015", 
                               "France-2016", 
                               "France-2017", 
                               "France-2018", 
                               "France-2019", 
                               "France-2020", 
                               "France-2021", 
                               "Germany-2014", 
                               "Germany-2015",  
                               "Germany-2016",  
                               "Germany-2017",  
                               "Germany-2018",  
                               "Germany-2019",  
                               "Germany-2020",  
                               "Germany-2021",  
                               "Greece-2014",
                               "Greece-2015",
                               "Greece-2016",
                               "Greece-2017",
                               "Greece-2018",
                               "Greece-2019",
                               "Greece-2020",
                               "Greece-2021",
                               "Hungary-2014", 
                               "Hungary-2015", 
                               "Hungary-2016", 
                               "Hungary-2017", 
                               "Hungary-2018", 
                               "Hungary-2019", 
                               "Hungary-2020", 
                               "Hungary-2021", 
                               "Iceland-2014",
                               "Iceland-2015",
                               "Iceland-2016",
                               "Iceland-2017",
                               "Iceland-2018",
                               "Iceland-2019",
                               "Iceland-2020",
                               "Iceland-2021",
                               "Ireland-2014", 
                               "Ireland-2015",
                               "Ireland-2016",
                               "Ireland-2017",
                               "Ireland-2018",
                               "Ireland-2019",
                               "Ireland-2020",
                               "Ireland-2021",
                               "Israel-2014",
                               "Israel-2015", 
                               "Israel-2016", 
                               "Israel-2017", 
                               "Israel-2018", 
                               "Israel-2019", 
                               "Israel-2020",
                               "Israel-2021", 
                               "Italy-2014",
                               "Italy-2015",
                               "Italy-2016",
                               "Italy-2017",
                               "Italy-2018",
                               "Italy-2019",
                               "Italy-2020",
                               "Italy-2021",
                               "Latvia-2014",
                               "Latvia-2015",
                               "Latvia-2016",
                               "Latvia-2017",
                               "Latvia-2018",
                               "Latvia-2019",
                               "Latvia-2020",
                               "Latvia-2021",
                               "Lithuania-2014",
                               "Lithuania-2015",
                               "Lithuania-2016",
                               "Lithuania-2017",
                               "Lithuania-2018",
                               "Lithuania-2019",
                               "Lithuania-2020",
                               "Lithuania-2021",
                               "Luxembourg-2014",
                               "Luxembourg-2015",
                               "Luxembourg-2016",
                               "Luxembourg-2017",
                               "Luxembourg-2018",
                               "Luxembourg-2019",
                               "Luxembourg-2020",
                               "Luxembourg-2021",
                               "Malta-2014",
                               "Malta-2015",
                               "Malta-2016",
                               "Malta-2017",
                               "Malta-2018",
                               "Malta-2019",
                               "Malta-2020",
                               "Malta-2021",
                               "Montenegro-2014",
                               "Montenegro-2015", 
                               "Montenegro-2016", 
                               "Montenegro-2017", 
                               "Montenegro-2018", 
                               "Montenegro-2019", 
                               "Montenegro-2020", 
                               "Montenegro-2021", 
                               "Netherlands-2014",
                               "Netherlands-2015",
                               "Netherlands-2016",
                               "Netherlands-2017",
                               "Netherlands-2018",
                               "Netherlands-2019",
                               "Netherlands-2020",
                               "Netherlands-2021",
                               "North Macedonia-2014",
                               "North Macedonia-2015",
                               "North Macedonia-2016",
                               "North Macedonia-2017",
                               "North Macedonia-2018",
                               "North Macedonia-2019",
                               "North Macedonia-2020",
                               "North Macedonia-2021",
                               "Norway-2014",
                               "Norway-2015",  
                               "Norway-2016",  
                               "Norway-2017",  
                               "Norway-2018",  
                               "Norway-2019",  
                               "Norway-2020",  
                               "Norway-2021",  
                               "Poland-2014",
                               "Poland-2015",  
                               "Poland-2016",  
                               "Poland-2017",  
                               "Poland-2018",  
                               "Poland-2019",  
                               "Poland-2020",  
                               "Poland-2021",  
                               "Portugal-2014",
                               "Portugal-2015", 
                               "Portugal-2016", 
                               "Portugal-2017", 
                               "Portugal-2018", 
                               "Portugal-2019", 
                               "Portugal-2020", 
                               "Portugal-2021", 
                               "Romania-2014",
                               "Romania-2015",
                               "Romania-2016",
                               "Romania-2017",
                               "Romania-2018",
                               "Romania-2019",
                               "Romania-2020",
                               "Romania-2021",
                               "Serbia-2014",
                               "Serbia-2015",
                               "Serbia-2016",
                               "Serbia-2017",
                               "Serbia-2018",
                               "Serbia-2019",
                               "Serbia-2020",
                               "Serbia-2021",
                               "Slovakia-2014", 
                               "Slovakia-2015",
                               "Slovakia-2016",
                               "Slovakia-2017",
                               "Slovakia-2018",
                               "Slovakia-2019",
                               "Slovakia-2020",
                               "Slovakia-2021",
                               "Slovenia-2014", 
                               "Slovenia-2015",
                               "Slovenia-2016",
                               "Slovenia-2017",
                               "Slovenia-2018",
                               "Slovenia-2019",
                               "Slovenia-2020",
                               "Slovenia-2021",
                               "Spain-2014", 
                               "Spain-2015",
                               "Spain-2016",
                               "Spain-2017",
                               "Spain-2018",
                               "Spain-2019",
                               "Spain-2020",
                               "Spain-2021",
                               "Sweden-2014",   
                               "Sweden-2015",
                               "Sweden-2016",
                               "Sweden-2017",
                               "Sweden-2018",
                               "Sweden-2019",
                               "Sweden-2020",
                               "Sweden-2021",
                               "Switzerland-2014",
                               "Switzerland-2015",
                               "Switzerland-2016",
                               "Switzerland-2017",
                               "Switzerland-2018",
                               "Switzerland-2019",
                               "Switzerland-2020",
                               "Switzerland-2021",
                               "Turkey-2014", 
                               "Turkey-2015",  
                               "Turkey-2016",  
                               "Turkey-2017",  
                               "Turkey-2018",  
                               "Turkey-2019",  
                               "Turkey-2020",  
                               "Turkey-2021",  
                               "Ukraine-2014",
                               "Ukraine-2015",
                               "Ukraine-2016",
                               "Ukraine-2017",
                               "Ukraine-2018",
                               "Ukraine-2019",
                               "Ukraine-2020",
                               "Ukraine-2021",
                               "United Kingdom-2014",
                               "United Kingdom-2015",
                               "United Kingdom-2016",
                               "United Kingdom-2017",
                               "United Kingdom-2018",
                               "United Kingdom-2019",
                               "United Kingdom-2020",
                               "United Kingdom-2021")
X_ist <- rownames(EIS_istruzione1) 
r_ist <- function(x_ist, y_ist) all(EIS_istruzione1[x_ist,] <= EIS_istruzione1[y_ist,]) 
r_ist<- Vectorize(r_ist) 
Z_ist <- outer(X_ist, X_ist, FUN = r_ist) 
dimnames(Z_ist) <- list(X_ist, X_ist) 
Z_ist <- validate.partialorder.incidence(Z_ist)
M_ist <- MRP(Z_ist, method = 'approx') # se non metto approx non funziona 
avr_height_ist <- colSums(M_ist) 
eigenvector_ist <- abs(svd(M_ist)$v[,1])
dim(EIS_innovazione1)
dim(EIS_istruzione1)
dim(EIS_demografia1)

#POSET economia
#install.packages("parsec")
library(parsec) 
library(netrankr)
dim(EIS_economia1)
rownames(EIS_economia1)<-c( "Austria- 2014",
                               "Austria- 2015",
                               "Austria- 2016",
                               "Austria- 2017",
                               "Austria- 2018",
                               "Austria- 2019",
                               "Austria- 2020",
                               "Austria- 2021",
                               "Belgium -2014",
                               "Belgium -2015",
                               "Belgium -2016",
                               "Belgium -2017",
                               "Belgium -2018",
                               "Belgium -2019",
                               "Belgium -2020",
                               "Belgium -2021",
                               "Bosnia and Herzegovina-2014",
                               "Bosnia and Herzegovina-2015",
                               "Bosnia and Herzegovina-2016",
                               "Bosnia and Herzegovina-2017",
                               "Bosnia and Herzegovina-2018",
                               "Bosnia and Herzegovina-2019",
                               "Bosnia and Herzegovina-2020",
                               "Bosnia and Herzegovina-2021",
                               "Bulgaria-2014", 
                               "Bulgaria-2015",  
                               "Bulgaria-2016",  
                               "Bulgaria-2017",  
                               "Bulgaria-2018",  
                               "Bulgaria-2019",  
                               "Bulgaria-2020",  
                               "Bulgaria-2021",  
                               "Croatia-2014",
                               "Croatia-2015",  
                               "Croatia-2016",  
                               "Croatia-2017",  
                               "Croatia-2018",  
                               "Croatia-2019",  
                               "Croatia-2020",  
                               "Croatia-2021",  
                               "Cyprus-2014",
                               "Cyprus-2015", 
                               "Cyprus-2016", 
                               "Cyprus-2017", 
                               "Cyprus-2018", 
                               "Cyprus-2019", 
                               "Cyprus-2020", 
                               "Cyprus-2021", 
                               "Czechia-2014",  
                               "Czechia-2015", 
                               "Czechia-2016", 
                               "Czechia-2017", 
                               "Czechia-2018", 
                               "Czechia-2019", 
                               "Czechia-2020", 
                               "Czechia-2021", 
                               "Denmark-2014",
                               "Denmark-2015",
                               "Denmark-2016",
                               "Denmark-2017",
                               "Denmark-2018",
                               "Denmark-2019",
                               "Denmark-2020",
                               "Denmark-2021",
                               "Estonia-2014",
                               "Estonia-2015",
                               "Estonia-2016",
                               "Estonia-2017",
                               "Estonia-2018",
                               "Estonia-2019",
                               "Estonia-2020",
                               "Estonia-2021",
                               "EU-2014",
                               "EU-2015", 
                               "EU-2016", 
                               "EU-2017", 
                               "EU-2018", 
                               "EU-2019", 
                               "EU-2020", 
                               "EU-2021", 
                               "Finland-2014", 
                               "Finland-2015",
                               "Finland-2016",
                               "Finland-2017",
                               "Finland-2018",
                               "Finland-2019",
                               "Finland-2020",
                               "Finland-2021",
                               "France-2014", 
                               "France-2015", 
                               "France-2016", 
                               "France-2017", 
                               "France-2018", 
                               "France-2019", 
                               "France-2020", 
                               "France-2021", 
                               "Germany-2014", 
                               "Germany-2015",  
                               "Germany-2016",  
                               "Germany-2017",  
                               "Germany-2018",  
                               "Germany-2019",  
                               "Germany-2020",  
                               "Germany-2021",  
                               "Greece-2014",
                               "Greece-2015",
                               "Greece-2016",
                               "Greece-2017",
                               "Greece-2018",
                               "Greece-2019",
                               "Greece-2020",
                               "Greece-2021",
                               "Hungary-2014", 
                               "Hungary-2015", 
                               "Hungary-2016", 
                               "Hungary-2017", 
                               "Hungary-2018", 
                               "Hungary-2019", 
                               "Hungary-2020", 
                               "Hungary-2021", 
                               "Iceland-2014",
                               "Iceland-2015",
                               "Iceland-2016",
                               "Iceland-2017",
                               "Iceland-2018",
                               "Iceland-2019",
                               "Iceland-2020",
                               "Iceland-2021",
                               "Ireland-2014", 
                               "Ireland-2015",
                               "Ireland-2016",
                               "Ireland-2017",
                               "Ireland-2018",
                               "Ireland-2019",
                               "Ireland-2020",
                               "Ireland-2021",
                               "Israel-2014",
                               "Israel-2015", 
                               "Israel-2016", 
                               "Israel-2017", 
                               "Israel-2018", 
                               "Israel-2019", 
                               "Israel-2020",
                               "Israel-2021", 
                               "Italy-2014",
                               "Italy-2015",
                               "Italy-2016",
                               "Italy-2017",
                               "Italy-2018",
                               "Italy-2019",
                               "Italy-2020",
                               "Italy-2021",
                               "Latvia-2014",
                               "Latvia-2015",
                               "Latvia-2016",
                               "Latvia-2017",
                               "Latvia-2018",
                               "Latvia-2019",
                               "Latvia-2020",
                               "Latvia-2021",
                               "Lithuania-2014",
                               "Lithuania-2015",
                               "Lithuania-2016",
                               "Lithuania-2017",
                               "Lithuania-2018",
                               "Lithuania-2019",
                               "Lithuania-2020",
                               "Lithuania-2021",
                               "Luxembourg-2014",
                               "Luxembourg-2015",
                               "Luxembourg-2016",
                               "Luxembourg-2017",
                               "Luxembourg-2018",
                               "Luxembourg-2019",
                               "Luxembourg-2020",
                               "Luxembourg-2021",
                               "Malta-2014",
                               "Malta-2015",
                               "Malta-2016",
                               "Malta-2017",
                               "Malta-2018",
                               "Malta-2019",
                               "Malta-2020",
                               "Malta-2021",
                               "Montenegro-2014",
                               "Montenegro-2015", 
                               "Montenegro-2016", 
                               "Montenegro-2017", 
                               "Montenegro-2018", 
                               "Montenegro-2019", 
                               "Montenegro-2020", 
                               "Montenegro-2021", 
                               "Netherlands-2014",
                               "Netherlands-2015",
                               "Netherlands-2016",
                               "Netherlands-2017",
                               "Netherlands-2018",
                               "Netherlands-2019",
                               "Netherlands-2020",
                               "Netherlands-2021",
                               "North Macedonia-2014",
                               "North Macedonia-2015",
                               "North Macedonia-2016",
                               "North Macedonia-2017",
                               "North Macedonia-2018",
                               "North Macedonia-2019",
                               "North Macedonia-2020",
                               "North Macedonia-2021",
                               "Norway-2014",
                               "Norway-2015",  
                               "Norway-2016",  
                               "Norway-2017",  
                               "Norway-2018",  
                               "Norway-2019",  
                               "Norway-2020",  
                               "Norway-2021",  
                               "Poland-2014",
                               "Poland-2015",  
                               "Poland-2016",  
                               "Poland-2017",  
                               "Poland-2018",  
                               "Poland-2019",  
                               "Poland-2020",  
                               "Poland-2021",  
                               "Portugal-2014",
                               "Portugal-2015", 
                               "Portugal-2016", 
                               "Portugal-2017", 
                               "Portugal-2018", 
                               "Portugal-2019", 
                               "Portugal-2020", 
                               "Portugal-2021", 
                               "Romania-2014",
                               "Romania-2015",
                               "Romania-2016",
                               "Romania-2017",
                               "Romania-2018",
                               "Romania-2019",
                               "Romania-2020",
                               "Romania-2021",
                               "Serbia-2014",
                               "Serbia-2015",
                               "Serbia-2016",
                               "Serbia-2017",
                               "Serbia-2018",
                               "Serbia-2019",
                               "Serbia-2020",
                               "Serbia-2021",
                               "Slovakia-2014", 
                               "Slovakia-2015",
                               "Slovakia-2016",
                               "Slovakia-2017",
                               "Slovakia-2018",
                               "Slovakia-2019",
                               "Slovakia-2020",
                               "Slovakia-2021",
                               "Slovenia-2014", 
                               "Slovenia-2015",
                               "Slovenia-2016",
                               "Slovenia-2017",
                               "Slovenia-2018",
                               "Slovenia-2019",
                               "Slovenia-2020",
                               "Slovenia-2021",
                               "Spain-2014", 
                               "Spain-2015",
                               "Spain-2016",
                               "Spain-2017",
                               "Spain-2018",
                               "Spain-2019",
                               "Spain-2020",
                               "Spain-2021",
                               "Sweden-2014",   
                               "Sweden-2015",
                               "Sweden-2016",
                               "Sweden-2017",
                               "Sweden-2018",
                               "Sweden-2019",
                               "Sweden-2020",
                               "Sweden-2021",
                               "Switzerland-2014",
                               "Switzerland-2015",
                               "Switzerland-2016",
                               "Switzerland-2017",
                               "Switzerland-2018",
                               "Switzerland-2019",
                               "Switzerland-2020",
                               "Switzerland-2021",
                               "Turkey-2014", 
                               "Turkey-2015",  
                               "Turkey-2016",  
                               "Turkey-2017",  
                               "Turkey-2018",  
                               "Turkey-2019",  
                               "Turkey-2020",  
                               "Turkey-2021",  
                               "Ukraine-2014",
                               "Ukraine-2015",
                               "Ukraine-2016",
                               "Ukraine-2017",
                               "Ukraine-2018",
                               "Ukraine-2019",
                               "Ukraine-2020",
                               "Ukraine-2021",
                               "United Kingdom-2014",
                               "United Kingdom-2015",
                               "United Kingdom-2016",
                               "United Kingdom-2017",
                               "United Kingdom-2018",
                               "United Kingdom-2019",
                               "United Kingdom-2020",
                               "United Kingdom-2021")
X_eco <- rownames(EIS_economia1) 
r_eco <- function(x_eco, y_eco) all(EIS_economia1[x_eco,] <= EIS_economia1[y_eco,]) 
r_eco<- Vectorize(r_eco) 
Z_eco<- outer(X_eco, X_eco, FUN = r_eco) 
dimnames(Z_eco) <- list(X_eco, X_eco) 
Z_eco <- validate.partialorder.incidence(Z_eco)
M_eco <- MRP(Z_eco, method = 'approx') # se non metto approx non funziona 
avr_height_eco <- colSums(M_eco) 
eigenvector_eco <- abs(svd(M_eco)$v[,1])
dim(EIS_economia1)

rownames(M_eco)<-c( "Austria- 2014",
                            "Austria- 2015",
                            "Austria- 2016",
                            "Austria- 2017",
                            "Austria- 2018",
                            "Austria- 2019",
                            "Austria- 2020",
                            "Austria- 2021",
                            "Belgium -2014",
                            "Belgium -2015",
                            "Belgium -2016",
                            "Belgium -2017",
                            "Belgium -2018",
                            "Belgium -2019",
                            "Belgium -2020",
                            "Belgium -2021",
                            "Bosnia and Herzegovina-2014",
                            "Bosnia and Herzegovina-2015",
                            "Bosnia and Herzegovina-2016",
                            "Bosnia and Herzegovina-2017",
                            "Bosnia and Herzegovina-2018",
                            "Bosnia and Herzegovina-2019",
                            "Bosnia and Herzegovina-2020",
                            "Bosnia and Herzegovina-2021",
                            "Bulgaria-2014", 
                            "Bulgaria-2015",  
                            "Bulgaria-2016",  
                            "Bulgaria-2017",  
                            "Bulgaria-2018",  
                            "Bulgaria-2019",  
                            "Bulgaria-2020",  
                            "Bulgaria-2021",  
                            "Croatia-2014",
                            "Croatia-2015",  
                            "Croatia-2016",  
                            "Croatia-2017",  
                            "Croatia-2018",  
                            "Croatia-2019",  
                            "Croatia-2020",  
                            "Croatia-2021",  
                            "Cyprus-2014",
                            "Cyprus-2015", 
                            "Cyprus-2016", 
                            "Cyprus-2017", 
                            "Cyprus-2018", 
                            "Cyprus-2019", 
                            "Cyprus-2020", 
                            "Cyprus-2021", 
                            "Czechia-2014",  
                            "Czechia-2015", 
                            "Czechia-2016", 
                            "Czechia-2017", 
                            "Czechia-2018", 
                            "Czechia-2019", 
                            "Czechia-2020", 
                            "Czechia-2021", 
                            "Denmark-2014",
                            "Denmark-2015",
                            "Denmark-2016",
                            "Denmark-2017",
                            "Denmark-2018",
                            "Denmark-2019",
                            "Denmark-2020",
                            "Denmark-2021",
                            "Estonia-2014",
                            "Estonia-2015",
                            "Estonia-2016",
                            "Estonia-2017",
                            "Estonia-2018",
                            "Estonia-2019",
                            "Estonia-2020",
                            "Estonia-2021",
                            "EU-2014",
                            "EU-2015", 
                            "EU-2016", 
                            "EU-2017", 
                            "EU-2018", 
                            "EU-2019", 
                            "EU-2020", 
                            "EU-2021", 
                            "Finland-2014", 
                            "Finland-2015",
                            "Finland-2016",
                            "Finland-2017",
                            "Finland-2018",
                            "Finland-2019",
                            "Finland-2020",
                            "Finland-2021",
                            "France-2014", 
                            "France-2015", 
                            "France-2016", 
                            "France-2017", 
                            "France-2018", 
                            "France-2019", 
                            "France-2020", 
                            "France-2021", 
                            "Germany-2014", 
                            "Germany-2015",  
                            "Germany-2016",  
                            "Germany-2017",  
                            "Germany-2018",  
                            "Germany-2019",  
                            "Germany-2020",  
                            "Germany-2021",  
                            "Greece-2014",
                            "Greece-2015",
                            "Greece-2016",
                            "Greece-2017",
                            "Greece-2018",
                            "Greece-2019",
                            "Greece-2020",
                            "Greece-2021",
                            "Hungary-2014", 
                            "Hungary-2015", 
                            "Hungary-2016", 
                            "Hungary-2017", 
                            "Hungary-2018", 
                            "Hungary-2019", 
                            "Hungary-2020", 
                            "Hungary-2021", 
                            "Iceland-2014",
                            "Iceland-2015",
                            "Iceland-2016",
                            "Iceland-2017",
                            "Iceland-2018",
                            "Iceland-2019",
                            "Iceland-2020",
                            "Iceland-2021",
                            "Ireland-2014", 
                            "Ireland-2015",
                            "Ireland-2016",
                            "Ireland-2017",
                            "Ireland-2018",
                            "Ireland-2019",
                            "Ireland-2020",
                            "Ireland-2021",
                            "Israel-2014",
                            "Israel-2015", 
                            "Israel-2016", 
                            "Israel-2017", 
                            "Israel-2018", 
                            "Israel-2019", 
                            "Israel-2020",
                            "Israel-2021", 
                            "Italy-2014",
                            "Italy-2015",
                            "Italy-2016",
                            "Italy-2017",
                            "Italy-2018",
                            "Italy-2019",
                            "Italy-2020",
                            "Italy-2021",
                            "Latvia-2014",
                            "Latvia-2015",
                            "Latvia-2016",
                            "Latvia-2017",
                            "Latvia-2018",
                            "Latvia-2019",
                            "Latvia-2020",
                            "Latvia-2021",
                            "Lithuania-2014",
                            "Lithuania-2015",
                            "Lithuania-2016",
                            "Lithuania-2017",
                            "Lithuania-2018",
                            "Lithuania-2019",
                            "Lithuania-2020",
                            "Lithuania-2021",
                            "Luxembourg-2014",
                            "Luxembourg-2015",
                            "Luxembourg-2016",
                            "Luxembourg-2017",
                            "Luxembourg-2018",
                            "Luxembourg-2019",
                            "Luxembourg-2020",
                            "Luxembourg-2021",
                            "Malta-2014",
                            "Malta-2015",
                            "Malta-2016",
                            "Malta-2017",
                            "Malta-2018",
                            "Malta-2019",
                            "Malta-2020",
                            "Malta-2021",
                            "Montenegro-2014",
                            "Montenegro-2015", 
                            "Montenegro-2016", 
                            "Montenegro-2017", 
                            "Montenegro-2018", 
                            "Montenegro-2019", 
                            "Montenegro-2020", 
                            "Montenegro-2021", 
                            "Netherlands-2014",
                            "Netherlands-2015",
                            "Netherlands-2016",
                            "Netherlands-2017",
                            "Netherlands-2018",
                            "Netherlands-2019",
                            "Netherlands-2020",
                            "Netherlands-2021",
                            "North Macedonia-2014",
                            "North Macedonia-2015",
                            "North Macedonia-2016",
                            "North Macedonia-2017",
                            "North Macedonia-2018",
                            "North Macedonia-2019",
                            "North Macedonia-2020",
                            "North Macedonia-2021",
                            "Norway-2014",
                            "Norway-2015",  
                            "Norway-2016",  
                            "Norway-2017",  
                            "Norway-2018",  
                            "Norway-2019",  
                            "Norway-2020",  
                            "Norway-2021",  
                            "Poland-2014",
                            "Poland-2015",  
                            "Poland-2016",  
                            "Poland-2017",  
                            "Poland-2018",  
                            "Poland-2019",  
                            "Poland-2020",  
                            "Poland-2021",  
                            "Portugal-2014",
                            "Portugal-2015", 
                            "Portugal-2016", 
                            "Portugal-2017", 
                            "Portugal-2018", 
                            "Portugal-2019", 
                            "Portugal-2020", 
                            "Portugal-2021", 
                            "Romania-2014",
                            "Romania-2015",
                            "Romania-2016",
                            "Romania-2017",
                            "Romania-2018",
                            "Romania-2019",
                            "Romania-2020",
                            "Romania-2021",
                            "Serbia-2014",
                            "Serbia-2015",
                            "Serbia-2016",
                            "Serbia-2017",
                            "Serbia-2018",
                            "Serbia-2019",
                            "Serbia-2020",
                            "Serbia-2021",
                            "Slovakia-2014", 
                            "Slovakia-2015",
                            "Slovakia-2016",
                            "Slovakia-2017",
                            "Slovakia-2018",
                            "Slovakia-2019",
                            "Slovakia-2020",
                            "Slovakia-2021",
                            "Slovenia-2014", 
                            "Slovenia-2015",
                            "Slovenia-2016",
                            "Slovenia-2017",
                            "Slovenia-2018",
                            "Slovenia-2019",
                            "Slovenia-2020",
                            "Slovenia-2021",
                            "Spain-2014", 
                            "Spain-2015",
                            "Spain-2016",
                            "Spain-2017",
                            "Spain-2018",
                            "Spain-2019",
                            "Spain-2020",
                            "Spain-2021",
                            "Sweden-2014",   
                            "Sweden-2015",
                            "Sweden-2016",
                            "Sweden-2017",
                            "Sweden-2018",
                            "Sweden-2019",
                            "Sweden-2020",
                            "Sweden-2021",
                            "Switzerland-2014",
                            "Switzerland-2015",
                            "Switzerland-2016",
                            "Switzerland-2017",
                            "Switzerland-2018",
                            "Switzerland-2019",
                            "Switzerland-2020",
                            "Switzerland-2021",
                            "Turkey-2014", 
                            "Turkey-2015",  
                            "Turkey-2016",  
                            "Turkey-2017",  
                            "Turkey-2018",  
                            "Turkey-2019",  
                            "Turkey-2020",  
                            "Turkey-2021",  
                            "Ukraine-2014",
                            "Ukraine-2015",
                            "Ukraine-2016",
                            "Ukraine-2017",
                            "Ukraine-2018",
                            "Ukraine-2019",
                            "Ukraine-2020",
                            "Ukraine-2021",
                            "United Kingdom-2014",
                            "United Kingdom-2015",
                            "United Kingdom-2016",
                            "United Kingdom-2017",
                            "United Kingdom-2018",
                            "United Kingdom-2019",
                            "United Kingdom-2020",
                            "United Kingdom-2021")

#SVD 
SVD_eco <-svd(M_eco)
SVD_inn <-svd(M_inn)
SVD_ist <-svd(M_ist)
SVD_dem <-svd(M_dem)

#RANKING
ranking_eco <- sort(avr_height_eco, decreasing = TRUE)
ranking_ist <- sort(avr_height_ist, decreasing = TRUE)
ranking_dem <- sort(avr_height_dem, decreasing = TRUE)
ranking_inn <- sort(avr_height_inn, decreasing = TRUE)

#GRAFICI
dim(ranking_eco)
plot(ranking_eco)
text(ranking_eco, labels= c( "United Kingdom-2020" ,"United Kingdom-2019","United Kingdom-2017" ,"United Kingdom-2018","United Kingdom-2021" ,"Norway-2020" ,"Portugal-2014","Ukraine-2014","Ukraine-2015","Ukraine-2016","Turkey-2014",
                             "France-2015","Croatia-2018", "Turkey-2016","Croatia-2016","Croatia-2017","Poland-2014","Poland-2015","Poland-2016","Romania-2020","North Macedonia-2015","Cyprus-2019","Czechia-2019","Lithuania-2014","Greece-2016",
                             "Italy-2014" ,"France-2014","Bosnia and Herzegovina-2020","Cyprus-2015","Montenegro-2018","Romania-2019","Belgium -2015","Luxembourg-2015","Netherlands-2018","Ukraine-2019","Norway-2019","Serbia-2015","Finland-2017",
                             "Finland-2018","Latvia-2014","Latvia-2015","Germany-2020", 
                             "Germany-2019","Montenegro-2017","Montenegro-2020","Norway-2014", "Turkey-2015","Cyprus-2016","Switzerland-2014","Croatia-2014","Montenegro-2016","Romania-2018","Croatia-2015","Norway-2015","United Kingdom-2015",
                            "Austria- 2014","Austria- 2015","Austria- 2016","Austria- 2017","Austria- 2018","Austria- 2019","Austria- 2020","Austria- 2021","Belgium -2014","Belgium -2017","Belgium -2018","Belgium -2019", 
                             "Belgium -2020","Belgium -2021" ,"Bosnia and Herzegovina-2021","Bulgaria-2021","Croatia-2019","Croatia-2020","Croatia-2021","Cyprus-2020","Cyprus-2021","Czechia-2014","Czechia-2015","Czechia-2016","Czechia-2017",
                            "Czechia-2018","Denmark-2014", "Denmark-2015","Denmark-2016","Denmark-2017","Denmark-2018","Denmark-2019","Denmark-2020", "Denmark-2021", "EU-2021","Finland-2015","Finland-2016","Finland-2021","France-2021",
                             "Germany-2021","Greece-2018","Greece-2021","Hungary-2014","Hungary-2015","Hungary-2021","Iceland-2021","Ireland-2014","Ireland-2015","Ireland-2016","Israel-2015","Israel-2016","Israel-2021","Italy-2021",
                            "Latvia-2016","Latvia-2018","Latvia-2019","Latvia-2020", "Latvia-2021","Lithuania-2015","Lithuania-2016","Luxembourg-2014","Luxembourg-2017","Luxembourg-2018","Luxembourg-2021","Malta-2016", "Malta-2017","Malta-2018",
                            " Malta-2021" ,"Montenegro-2021","Netherlands-2014","Netherlands-2015","Netherlands-2016", "Netherlands-2017","Netherlands-2019","Netherlands-2021","North Macedonia-2014","North Macedonia-2018", 
                            " North Macedonia-2019" ,"North Macedonia-2020", "North Macedonia-2021","Norway-2021","Poland-2017","Poland-2018","Poland-2019","Poland-2021","Portugal-2021" ,"Romania-2021","Serbia-2016" ,"Serbia-2017","Serbia-2018",
                            " Slovakia-2014","Slovakia-2015","Slovakia-2016","Slovakia-2017","Slovakia-2018","Slovakia-2019","Slovakia-2020","Slovakia-2021","Slovenia-2014","Slovenia-2015", "Slovenia-2016" ,"Slovenia-2017","Slovenia-2018",
                             "Slovenia-2019" ,"Slovenia-2020","Slovenia-2021","Spain-2021","Sweden-2021","Turkey-2019","Turkey-2021","Ukraine-2017","Ukraine-2018","Ukraine-2021","North Macedonia-2016","Cyprus-2014","Germany-2018","France-2016", 
                             "Sweden-2018","Iceland-2014","Portugal-2015","Norway-2018","Sweden-2020","Sweden-2019","EU-2014","Italy-2016","Italy-2018", "Bosnia and Herzegovina-2019","Montenegro-2015","United Kingdom-2016" ,"Norway-2016","Norway-2017",
                            " Cyprus-2018","Montenegro-2014","Romania-2017","Bulgaria-2020","Iceland-2015","Germany-2017","Sweden-2017","Turkey-2017", "Bosnia and Herzegovina-2018","EU-2017","Switzerland-2015","Czechia-2020","Cyprus-2017",
                             "Malta-2014","Malta-2015","Lithuania-2017","Lithuania-2018", "Lithuania-2019","Israel-2017","Israel-2018","Israel-2019","Israel-2020","Serbia-2020","Serbia-2021","Ireland-2017","Portugal-2016","Portugal-2017" ,
                             "Belgium -2016","Luxembourg-2016","Netherlands-2020","Ukraine-2020","EU-2019","Spain-2014","France-2019","Portugal-2019","EU-2018","Portugal-2018","Romania-2016","Estonia-2019","Estonia-2020","Estonia-2014","EU-2020",           
                             "Switzerland-2020","Switzerland-2021","Iceland-2020","Switzerland-2019", 
                            " Germany-2014", "Ireland-2018","Luxembourg-2019","Luxembourg-2020","Serbia-2019","Spain-2019","Spain-2020","Estonia-2021","North Macedonia-2017","Spain-2015","France-2018" ,"Germany-2015","Poland-2020","Iceland-2019",
                             "Italy-2020","Bulgaria-2017","Turkey-2018","Greece-2020","Spain-2018","France-2020" ,"Bulgaria-2018","Sweden-2014","Spain-2016","Spain-2017","United Kingdom-2014","Turkey-2020","Iceland-2018","Ireland-2020",
                             "Germany-2016","Malta-2019" ,"Italy-2015","Hungary-2020","Iceland-2017","Malta-2020","Iceland-2016","Portugal-2020","Czechia-2021","France-2017", "Bosnia and Herzegovina-2017","Lithuania-2020","Lithuania-2021",
                            " Ireland-2019","Romania-2015","Sweden-2016","Hungary-2019" ,"Ireland-2021","Italy-2019","EU-2015","Sweden-2015","Hungary-2018","Finland-2020","Estonia-2016","Estonia-2017","EU-2016","Hungary-2016","Italy-2017" ,
                             "Hungary-2017","Estonia-2018","Finland-2019" ,"Bosnia and Herzegovina-2016", 
                             "Romania-2014","Bulgaria-2019", "Bosnia and Herzegovina-2015", "Bosnia and Herzegovina-2014","Bulgaria-2016","Bulgaria-2015","Bulgaria-2014" ))
plot(ranking_dem)
plot(ranking_inn)
plot(ranking_ist)
dt <- data.table::as.data.table(ranking_eco)
d_dem <-data.table::as.data.table(ranking_dem)
d_ist <-data.table::as.data.table(ranking_ist)
d_inn <-data.table::as.data.table(ranking_inn)
#salvare in excel per tabella 
library("writexl")
write_xlsx(dt,"C:\\tesi\\ranking_eco.xlsx")
library("writexl")
write_xlsx(d_dem,"C:\\tesi\\ranking_dem.xlsx")
library("writexl")
write_xlsx(d_ist,"C:\\tesi\\ranking_ist.xlsx")
library("writexl")
write_xlsx(d_inn,"C:\\tesi\\ranking_inn.xlsx")

#RANKING SVD 
ranking_eco_SVD <- sort(SVD_eco[[1]], decreasing = TRUE)
ranking_ist_SVD <- sort(SVD_ist[[1]], decreasing = TRUE)
ranking_dem_SVD <- sort(SVD_dem[[1]], decreasing = TRUE)
ranking_inn_SVD <- sort(SVD_inn[[1]], decreasing = TRUE)

typeof(ranking_eco)
#provare a plottare il ranking 

require(tidyverse)
ggplot(ranking_eco, aes(x=d3,  y=value, group=d3))+geom_bar(stat='identity',     
                                                     aes(fill=d3))+facet_wrap(~variable)

#LIM demografia 

LIM_dem <- matrix(NA_real_, NCOL(M_dem), NCOL(M_dem))
for (i in seq_len(NCOL(LIM_dem)))
  for (j in seq_len(NCOL(LIM_dem)))
    LIM_dem[i,j] <- min(min(M_dem[i,j], M_dem[j, i])) 

LIMdem =  LIM_dem - diag(diag(LIM_dem))

rownames(LIMdem)<-c( "Austria",               
                    "Belgium",               
                    "Bosnia and Herzegovina",
                    "Bulgaria",              
                    "Croatia",               
                    "Cyprus",                
                    "Czechia",               
                    "Denmark",               
                    "Estonia",               
                    "EU",                    
                    "Finland",               
                    "France",                
                    "Germany",               
                    "Greece",                
                    "Hungary",                
                    "Iceland",               
                    "Ireland",               
                    "Israel",                
                    "Italy",                 
                    "Latvia",                
                    "Lithuania",             
                    "Luxembourg",            
                    "Malta",                 
                    "Montenegro",            
                    "Netherlands",           
                    "North Macedonia",      
                    "Norway",                
                    "Poland",                
                    "Portugal",              
                    "Romania",               
                    "Serbia",               
                    "Slovakia",              
                    "Slovenia",              
                    "Spain",                 
                    "Sweden",                 
                    "Turkey",                
                    "Ukraine", "United Kingdom")

colnames(LIMdem)<-c( "Austria",               
                    "Belgium",               
                    "Bosnia and Herzegovina",
                    "Bulgaria",              
                    "Croatia",               
                    "Cyprus",                
                    "Czechia",               
                    "Denmark",               
                    "Estonia",               
                    "EU",                    
                    "Finland",               
                    "France",                
                    "Germany",               
                    "Greece",                
                    "Hungary",                
                    "Iceland",               
                    "Ireland",               
                    "Israel",                
                    "Italy",                 
                    "Latvia",                
                    "Lithuania",             
                    "Luxembourg",            
                    "Malta",                 
                    "Montenegro",            
                    "Netherlands",           
                    "North Macedonia",      
                    "Norway",                
                    "Poland",                
                    "Portugal",              
                    "Romania",               
                    "Serbia",               
                    "Slovakia",              
                    "Slovenia",              
                    "Spain",                 
                    "Sweden",                 
                    "Turkey",                
                    "Ukraine", "United Kingdom")
#LIM inn

LIM_inn <- matrix(NA_real_, NCOL(M_inn), NCOL(M_inn))
for (i in seq_len(NCOL(LIM_inn)))
  for (j in seq_len(NCOL(LIM_inn)))
    LIM_inn[i,j] <- min(min(M_inn[i,j], M_inn[j, i])) 
LIMinn =  LIM_inn - diag(diag(LIM_inn))

rownames(LIMinn)<-c( "Austria- 2014",
                               "Austria- 2015",
                               "Austria- 2016",
                               "Austria- 2017",
                               "Austria- 2018",
                               "Austria- 2019",
                               "Austria- 2020",
                               "Austria- 2021",
                               "Belgium -2014",
                               "Belgium -2015",
                               "Belgium -2016",
                               "Belgium -2017",
                               "Belgium -2018",
                               "Belgium -2019",
                               "Belgium -2020",
                               "Belgium -2021",
                               "Bosnia and Herzegovina-2014",
                               "Bosnia and Herzegovina-2015",
                               "Bosnia and Herzegovina-2016",
                               "Bosnia and Herzegovina-2017",
                               "Bosnia and Herzegovina-2018",
                               "Bosnia and Herzegovina-2019",
                               "Bosnia and Herzegovina-2020",
                               "Bosnia and Herzegovina-2021",
                               "Bulgaria-2014", 
                               "Bulgaria-2015",  
                               "Bulgaria-2016",  
                               "Bulgaria-2017",  
                               "Bulgaria-2018",  
                               "Bulgaria-2019",  
                               "Bulgaria-2020",  
                               "Bulgaria-2021",  
                               "Croatia-2014",
                               "Croatia-2015",  
                               "Croatia-2016",  
                               "Croatia-2017",  
                               "Croatia-2018",  
                               "Croatia-2019",  
                               "Croatia-2020",  
                               "Croatia-2021",  
                               "Cyprus-2014",
                               "Cyprus-2015", 
                               "Cyprus-2016", 
                               "Cyprus-2017", 
                               "Cyprus-2018", 
                               "Cyprus-2019", 
                               "Cyprus-2020", 
                               "Cyprus-2021", 
                               "Czechia-2014",  
                               "Czechia-2015", 
                               "Czechia-2016", 
                               "Czechia-2017", 
                               "Czechia-2018", 
                               "Czechia-2019", 
                               "Czechia-2020", 
                               "Czechia-2021", 
                               "Denmark-2014",
                               "Denmark-2015",
                               "Denmark-2016",
                               "Denmark-2017",
                               "Denmark-2018",
                               "Denmark-2019",
                               "Denmark-2020",
                               "Denmark-2021",
                               "Estonia-2014",
                               "Estonia-2015",
                               "Estonia-2016",
                               "Estonia-2017",
                               "Estonia-2018",
                               "Estonia-2019",
                               "Estonia-2020",
                               "Estonia-2021",
                               "EU-2014",
                               "EU-2015", 
                               "EU-2016", 
                               "EU-2017", 
                               "EU-2018", 
                               "EU-2019", 
                               "EU-2020", 
                               "EU-2021", 
                               "Finland-2014", 
                               "Finland-2015",
                               "Finland-2016",
                               "Finland-2017",
                               "Finland-2018",
                               "Finland-2019",
                               "Finland-2020",
                               "Finland-2021",
                               "France-2014", 
                               "France-2015", 
                               "France-2016", 
                               "France-2017", 
                               "France-2018", 
                               "France-2019", 
                               "France-2020", 
                               "France-2021", 
                               "Germany-2014", 
                               "Germany-2015",  
                               "Germany-2016",  
                               "Germany-2017",  
                               "Germany-2018",  
                               "Germany-2019",  
                               "Germany-2020",  
                               "Germany-2021",  
                               "Greece-2014",
                               "Greece-2015",
                               "Greece-2016",
                               "Greece-2017",
                               "Greece-2018",
                               "Greece-2019",
                               "Greece-2020",
                               "Greece-2021",
                               "Hungary-2014", 
                               "Hungary-2015", 
                               "Hungary-2016", 
                               "Hungary-2017", 
                               "Hungary-2018", 
                               "Hungary-2019", 
                               "Hungary-2020", 
                               "Hungary-2021", 
                               "Iceland-2014",
                               "Iceland-2015",
                               "Iceland-2016",
                               "Iceland-2017",
                               "Iceland-2018",
                               "Iceland-2019",
                               "Iceland-2020",
                               "Iceland-2021",
                               "Ireland-2014", 
                               "Ireland-2015",
                               "Ireland-2016",
                               "Ireland-2017",
                               "Ireland-2018",
                               "Ireland-2019",
                               "Ireland-2020",
                               "Ireland-2021",
                               "Israel-2014",
                               "Israel-2015", 
                               "Israel-2016", 
                               "Israel-2017", 
                               "Israel-2018", 
                               "Israel-2019", 
                               "Israel-2020",
                               "Israel-2021", 
                               "Italy-2014",
                               "Italy-2015",
                               "Italy-2016",
                               "Italy-2017",
                               "Italy-2018",
                               "Italy-2019",
                               "Italy-2020",
                               "Italy-2021",
                               "Latvia-2014",
                               "Latvia-2015",
                               "Latvia-2016",
                               "Latvia-2017",
                               "Latvia-2018",
                               "Latvia-2019",
                               "Latvia-2020",
                               "Latvia-2021",
                               "Lithuania-2014",
                               "Lithuania-2015",
                               "Lithuania-2016",
                               "Lithuania-2017",
                               "Lithuania-2018",
                               "Lithuania-2019",
                               "Lithuania-2020",
                               "Lithuania-2021",
                               "Luxembourg-2014",
                               "Luxembourg-2015",
                               "Luxembourg-2016",
                               "Luxembourg-2017",
                               "Luxembourg-2018",
                               "Luxembourg-2019",
                               "Luxembourg-2020",
                               "Luxembourg-2021",
                               "Malta-2014",
                               "Malta-2015",
                               "Malta-2016",
                               "Malta-2017",
                               "Malta-2018",
                               "Malta-2019",
                               "Malta-2020",
                               "Malta-2021",
                               "Montenegro-2014",
                               "Montenegro-2015", 
                               "Montenegro-2016", 
                               "Montenegro-2017", 
                               "Montenegro-2018", 
                               "Montenegro-2019", 
                               "Montenegro-2020", 
                               "Montenegro-2021", 
                               "Netherlands-2014",
                               "Netherlands-2015",
                               "Netherlands-2016",
                               "Netherlands-2017",
                               "Netherlands-2018",
                               "Netherlands-2019",
                               "Netherlands-2020",
                               "Netherlands-2021",
                               "North Macedonia-2014",
                               "North Macedonia-2015",
                               "North Macedonia-2016",
                               "North Macedonia-2017",
                               "North Macedonia-2018",
                               "North Macedonia-2019",
                               "North Macedonia-2020",
                               "North Macedonia-2021",
                               "Norway-2014",
                               "Norway-2015",  
                               "Norway-2016",  
                               "Norway-2017",  
                               "Norway-2018",  
                               "Norway-2019",  
                               "Norway-2020",  
                               "Norway-2021",  
                               "Poland-2014",
                               "Poland-2015",  
                               "Poland-2016",  
                               "Poland-2017",  
                               "Poland-2018",  
                               "Poland-2019",  
                               "Poland-2020",  
                               "Poland-2021",  
                               "Portugal-2014",
                               "Portugal-2015", 
                               "Portugal-2016", 
                               "Portugal-2017", 
                               "Portugal-2018", 
                               "Portugal-2019", 
                               "Portugal-2020", 
                               "Portugal-2021", 
                               "Romania-2014",
                               "Romania-2015",
                               "Romania-2016",
                               "Romania-2017",
                               "Romania-2018",
                               "Romania-2019",
                               "Romania-2020",
                               "Romania-2021",
                               "Serbia-2014",
                               "Serbia-2015",
                               "Serbia-2016",
                               "Serbia-2017",
                               "Serbia-2018",
                               "Serbia-2019",
                               "Serbia-2020",
                               "Serbia-2021",
                               "Slovakia-2014", 
                               "Slovakia-2015",
                               "Slovakia-2016",
                               "Slovakia-2017",
                               "Slovakia-2018",
                               "Slovakia-2019",
                               "Slovakia-2020",
                               "Slovakia-2021",
                               "Slovenia-2014", 
                               "Slovenia-2015",
                               "Slovenia-2016",
                               "Slovenia-2017",
                               "Slovenia-2018",
                               "Slovenia-2019",
                               "Slovenia-2020",
                               "Slovenia-2021",
                               "Spain-2014", 
                               "Spain-2015",
                               "Spain-2016",
                               "Spain-2017",
                               "Spain-2018",
                               "Spain-2019",
                               "Spain-2020",
                               "Spain-2021",
                               "Sweden-2014",   
                               "Sweden-2015",
                               "Sweden-2016",
                               "Sweden-2017",
                               "Sweden-2018",
                               "Sweden-2019",
                               "Sweden-2020",
                               "Sweden-2021",
                               "Switzerland-2014",
                               "Switzerland-2015",
                               "Switzerland-2016",
                               "Switzerland-2017",
                               "Switzerland-2018",
                               "Switzerland-2019",
                               "Switzerland-2020",
                               "Switzerland-2021",
                               "Turkey-2014", 
                               "Turkey-2015",  
                               "Turkey-2016",  
                               "Turkey-2017",  
                               "Turkey-2018",  
                               "Turkey-2019",  
                               "Turkey-2020",  
                               "Turkey-2021",  
                               "Ukraine-2014",
                               "Ukraine-2015",
                               "Ukraine-2016",
                               "Ukraine-2017",
                               "Ukraine-2018",
                               "Ukraine-2019",
                               "Ukraine-2020",
                               "Ukraine-2021",
                               "United Kingdom-2014",
                               "United Kingdom-2015",
                               "United Kingdom-2016",
                               "United Kingdom-2017",
                               "United Kingdom-2018",
                               "United Kingdom-2019",
                               "United Kingdom-2020",
                               "United Kingdom-2021")

colnames(LIMinn)<-c( "Austria- 2014",
                               "Austria- 2015",
                               "Austria- 2016",
                               "Austria- 2017",
                               "Austria- 2018",
                               "Austria- 2019",
                               "Austria- 2020",
                               "Austria- 2021",
                               "Belgium -2014",
                               "Belgium -2015",
                               "Belgium -2016",
                               "Belgium -2017",
                               "Belgium -2018",
                               "Belgium -2019",
                               "Belgium -2020",
                               "Belgium -2021",
                               "Bosnia and Herzegovina-2014",
                               "Bosnia and Herzegovina-2015",
                               "Bosnia and Herzegovina-2016",
                               "Bosnia and Herzegovina-2017",
                               "Bosnia and Herzegovina-2018",
                               "Bosnia and Herzegovina-2019",
                               "Bosnia and Herzegovina-2020",
                               "Bosnia and Herzegovina-2021",
                               "Bulgaria-2014", 
                               "Bulgaria-2015",  
                               "Bulgaria-2016",  
                               "Bulgaria-2017",  
                               "Bulgaria-2018",  
                               "Bulgaria-2019",  
                               "Bulgaria-2020",  
                               "Bulgaria-2021",  
                               "Croatia-2014",
                               "Croatia-2015",  
                               "Croatia-2016",  
                               "Croatia-2017",  
                               "Croatia-2018",  
                               "Croatia-2019",  
                               "Croatia-2020",  
                               "Croatia-2021",  
                               "Cyprus-2014",
                               "Cyprus-2015", 
                               "Cyprus-2016", 
                               "Cyprus-2017", 
                               "Cyprus-2018", 
                               "Cyprus-2019", 
                               "Cyprus-2020", 
                               "Cyprus-2021", 
                               "Czechia-2014",  
                               "Czechia-2015", 
                               "Czechia-2016", 
                               "Czechia-2017", 
                               "Czechia-2018", 
                               "Czechia-2019", 
                               "Czechia-2020", 
                               "Czechia-2021", 
                               "Denmark-2014",
                               "Denmark-2015",
                               "Denmark-2016",
                               "Denmark-2017",
                               "Denmark-2018",
                               "Denmark-2019",
                               "Denmark-2020",
                               "Denmark-2021",
                               "Estonia-2014",
                               "Estonia-2015",
                               "Estonia-2016",
                               "Estonia-2017",
                               "Estonia-2018",
                               "Estonia-2019",
                               "Estonia-2020",
                               "Estonia-2021",
                               "EU-2014",
                               "EU-2015", 
                               "EU-2016", 
                               "EU-2017", 
                               "EU-2018", 
                               "EU-2019", 
                               "EU-2020", 
                               "EU-2021", 
                               "Finland-2014", 
                               "Finland-2015",
                               "Finland-2016",
                               "Finland-2017",
                               "Finland-2018",
                               "Finland-2019",
                               "Finland-2020",
                               "Finland-2021",
                               "France-2014", 
                               "France-2015", 
                               "France-2016", 
                               "France-2017", 
                               "France-2018", 
                               "France-2019", 
                               "France-2020", 
                               "France-2021", 
                               "Germany-2014", 
                               "Germany-2015",  
                               "Germany-2016",  
                               "Germany-2017",  
                               "Germany-2018",  
                               "Germany-2019",  
                               "Germany-2020",  
                               "Germany-2021",  
                               "Greece-2014",
                               "Greece-2015",
                               "Greece-2016",
                               "Greece-2017",
                               "Greece-2018",
                               "Greece-2019",
                               "Greece-2020",
                               "Greece-2021",
                               "Hungary-2014", 
                               "Hungary-2015", 
                               "Hungary-2016", 
                               "Hungary-2017", 
                               "Hungary-2018", 
                               "Hungary-2019", 
                               "Hungary-2020", 
                               "Hungary-2021", 
                               "Iceland-2014",
                               "Iceland-2015",
                               "Iceland-2016",
                               "Iceland-2017",
                               "Iceland-2018",
                               "Iceland-2019",
                               "Iceland-2020",
                               "Iceland-2021",
                               "Ireland-2014", 
                               "Ireland-2015",
                               "Ireland-2016",
                               "Ireland-2017",
                               "Ireland-2018",
                               "Ireland-2019",
                               "Ireland-2020",
                               "Ireland-2021",
                               "Israel-2014",
                               "Israel-2015", 
                               "Israel-2016", 
                               "Israel-2017", 
                               "Israel-2018", 
                               "Israel-2019", 
                               "Israel-2020",
                               "Israel-2021", 
                               "Italy-2014",
                               "Italy-2015",
                               "Italy-2016",
                               "Italy-2017",
                               "Italy-2018",
                               "Italy-2019",
                               "Italy-2020",
                               "Italy-2021",
                               "Latvia-2014",
                               "Latvia-2015",
                               "Latvia-2016",
                               "Latvia-2017",
                               "Latvia-2018",
                               "Latvia-2019",
                               "Latvia-2020",
                               "Latvia-2021",
                               "Lithuania-2014",
                               "Lithuania-2015",
                               "Lithuania-2016",
                               "Lithuania-2017",
                               "Lithuania-2018",
                               "Lithuania-2019",
                               "Lithuania-2020",
                               "Lithuania-2021",
                               "Luxembourg-2014",
                               "Luxembourg-2015",
                               "Luxembourg-2016",
                               "Luxembourg-2017",
                               "Luxembourg-2018",
                               "Luxembourg-2019",
                               "Luxembourg-2020",
                               "Luxembourg-2021",
                               "Malta-2014",
                               "Malta-2015",
                               "Malta-2016",
                               "Malta-2017",
                               "Malta-2018",
                               "Malta-2019",
                               "Malta-2020",
                               "Malta-2021",
                               "Montenegro-2014",
                               "Montenegro-2015", 
                               "Montenegro-2016", 
                               "Montenegro-2017", 
                               "Montenegro-2018", 
                               "Montenegro-2019", 
                               "Montenegro-2020", 
                               "Montenegro-2021", 
                               "Netherlands-2014",
                               "Netherlands-2015",
                               "Netherlands-2016",
                               "Netherlands-2017",
                               "Netherlands-2018",
                               "Netherlands-2019",
                               "Netherlands-2020",
                               "Netherlands-2021",
                               "North Macedonia-2014",
                               "North Macedonia-2015",
                               "North Macedonia-2016",
                               "North Macedonia-2017",
                               "North Macedonia-2018",
                               "North Macedonia-2019",
                               "North Macedonia-2020",
                               "North Macedonia-2021",
                               "Norway-2014",
                               "Norway-2015",  
                               "Norway-2016",  
                               "Norway-2017",  
                               "Norway-2018",  
                               "Norway-2019",  
                               "Norway-2020",  
                               "Norway-2021",  
                               "Poland-2014",
                               "Poland-2015",  
                               "Poland-2016",  
                               "Poland-2017",  
                               "Poland-2018",  
                               "Poland-2019",  
                               "Poland-2020",  
                               "Poland-2021",  
                               "Portugal-2014",
                               "Portugal-2015", 
                               "Portugal-2016", 
                               "Portugal-2017", 
                               "Portugal-2018", 
                               "Portugal-2019", 
                               "Portugal-2020", 
                               "Portugal-2021", 
                               "Romania-2014",
                               "Romania-2015",
                               "Romania-2016",
                               "Romania-2017",
                               "Romania-2018",
                               "Romania-2019",
                               "Romania-2020",
                               "Romania-2021",
                               "Serbia-2014",
                               "Serbia-2015",
                               "Serbia-2016",
                               "Serbia-2017",
                               "Serbia-2018",
                               "Serbia-2019",
                               "Serbia-2020",
                               "Serbia-2021",
                               "Slovakia-2014", 
                               "Slovakia-2015",
                               "Slovakia-2016",
                               "Slovakia-2017",
                               "Slovakia-2018",
                               "Slovakia-2019",
                               "Slovakia-2020",
                               "Slovakia-2021",
                               "Slovenia-2014", 
                               "Slovenia-2015",
                               "Slovenia-2016",
                               "Slovenia-2017",
                               "Slovenia-2018",
                               "Slovenia-2019",
                               "Slovenia-2020",
                               "Slovenia-2021",
                               "Spain-2014", 
                               "Spain-2015",
                               "Spain-2016",
                               "Spain-2017",
                               "Spain-2018",
                               "Spain-2019",
                               "Spain-2020",
                               "Spain-2021",
                               "Sweden-2014",   
                               "Sweden-2015",
                               "Sweden-2016",
                               "Sweden-2017",
                               "Sweden-2018",
                               "Sweden-2019",
                               "Sweden-2020",
                               "Sweden-2021",
                               "Switzerland-2014",
                               "Switzerland-2015",
                               "Switzerland-2016",
                               "Switzerland-2017",
                               "Switzerland-2018",
                               "Switzerland-2019",
                               "Switzerland-2020",
                               "Switzerland-2021",
                               "Turkey-2014", 
                               "Turkey-2015",  
                               "Turkey-2016",  
                               "Turkey-2017",  
                               "Turkey-2018",  
                               "Turkey-2019",  
                               "Turkey-2020",  
                               "Turkey-2021",  
                               "Ukraine-2014",
                               "Ukraine-2015",
                               "Ukraine-2016",
                               "Ukraine-2017",
                               "Ukraine-2018",
                               "Ukraine-2019",
                               "Ukraine-2020",
                               "Ukraine-2021",
                               "United Kingdom-2014",
                               "United Kingdom-2015",
                               "United Kingdom-2016",
                               "United Kingdom-2017",
                               "United Kingdom-2018",
                               "United Kingdom-2019",
                               "United Kingdom-2020",
                               "United Kingdom-2021")
#LIM ist 

LIM_ist <- matrix(NA_real_, NCOL(M_ist), NCOL(M_ist))
for (i in seq_len(NCOL(LIM_ist)))
  for (j in seq_len(NCOL(LIM_ist)))
    LIM_ist[i,j] <- min(min(M_ist[i,j], M_ist[j, i]))

LIMist =  LIM_ist - diag(diag(LIM_ist))

rownames(LIMist)<-c( "Austria- 2014",
                              "Austria- 2015",
                              "Austria- 2016",
                              "Austria- 2017",
                              "Austria- 2018",
                              "Austria- 2019",
                              "Austria- 2020",
                              "Austria- 2021",
                              "Belgium -2014",
                              "Belgium -2015",
                              "Belgium -2016",
                              "Belgium -2017",
                              "Belgium -2018",
                              "Belgium -2019",
                              "Belgium -2020",
                              "Belgium -2021",
                              "Bosnia and Herzegovina-2014",
                              "Bosnia and Herzegovina-2015",
                              "Bosnia and Herzegovina-2016",
                              "Bosnia and Herzegovina-2017",
                              "Bosnia and Herzegovina-2018",
                              "Bosnia and Herzegovina-2019",
                              "Bosnia and Herzegovina-2020",
                              "Bosnia and Herzegovina-2021",
                              "Bulgaria-2014", 
                              "Bulgaria-2015",  
                              "Bulgaria-2016",  
                              "Bulgaria-2017",  
                              "Bulgaria-2018",  
                              "Bulgaria-2019",  
                              "Bulgaria-2020",  
                              "Bulgaria-2021",  
                              "Croatia-2014",
                              "Croatia-2015",  
                              "Croatia-2016",  
                              "Croatia-2017",  
                              "Croatia-2018",  
                              "Croatia-2019",  
                              "Croatia-2020",  
                              "Croatia-2021",  
                              "Cyprus-2014",
                              "Cyprus-2015", 
                              "Cyprus-2016", 
                              "Cyprus-2017", 
                              "Cyprus-2018", 
                              "Cyprus-2019", 
                              "Cyprus-2020", 
                              "Cyprus-2021", 
                              "Czechia-2014",  
                              "Czechia-2015", 
                              "Czechia-2016", 
                              "Czechia-2017", 
                              "Czechia-2018", 
                              "Czechia-2019", 
                              "Czechia-2020", 
                              "Czechia-2021", 
                              "Denmark-2014",
                              "Denmark-2015",
                              "Denmark-2016",
                              "Denmark-2017",
                              "Denmark-2018",
                              "Denmark-2019",
                              "Denmark-2020",
                              "Denmark-2021",
                              "Estonia-2014",
                              "Estonia-2015",
                              "Estonia-2016",
                              "Estonia-2017",
                              "Estonia-2018",
                              "Estonia-2019",
                              "Estonia-2020",
                              "Estonia-2021",
                              "EU-2014",
                              "EU-2015", 
                              "EU-2016", 
                              "EU-2017", 
                              "EU-2018", 
                              "EU-2019", 
                              "EU-2020", 
                              "EU-2021", 
                              "Finland-2014", 
                              "Finland-2015",
                              "Finland-2016",
                              "Finland-2017",
                              "Finland-2018",
                              "Finland-2019",
                              "Finland-2020",
                              "Finland-2021",
                              "France-2014", 
                              "France-2015", 
                              "France-2016", 
                              "France-2017", 
                              "France-2018", 
                              "France-2019", 
                              "France-2020", 
                              "France-2021", 
                              "Germany-2014", 
                              "Germany-2015",  
                              "Germany-2016",  
                              "Germany-2017",  
                              "Germany-2018",  
                              "Germany-2019",  
                              "Germany-2020",  
                              "Germany-2021",  
                              "Greece-2014",
                              "Greece-2015",
                              "Greece-2016",
                              "Greece-2017",
                              "Greece-2018",
                              "Greece-2019",
                              "Greece-2020",
                              "Greece-2021",
                              "Hungary-2014", 
                              "Hungary-2015", 
                              "Hungary-2016", 
                              "Hungary-2017", 
                              "Hungary-2018", 
                              "Hungary-2019", 
                              "Hungary-2020", 
                              "Hungary-2021", 
                              "Iceland-2014",
                              "Iceland-2015",
                              "Iceland-2016",
                              "Iceland-2017",
                              "Iceland-2018",
                              "Iceland-2019",
                              "Iceland-2020",
                              "Iceland-2021",
                              "Ireland-2014", 
                              "Ireland-2015",
                              "Ireland-2016",
                              "Ireland-2017",
                              "Ireland-2018",
                              "Ireland-2019",
                              "Ireland-2020",
                              "Ireland-2021",
                              "Israel-2014",
                              "Israel-2015", 
                              "Israel-2016", 
                              "Israel-2017", 
                              "Israel-2018", 
                              "Israel-2019", 
                              "Israel-2020",
                              "Israel-2021", 
                              "Italy-2014",
                              "Italy-2015",
                              "Italy-2016",
                              "Italy-2017",
                              "Italy-2018",
                              "Italy-2019",
                              "Italy-2020",
                              "Italy-2021",
                              "Latvia-2014",
                              "Latvia-2015",
                              "Latvia-2016",
                              "Latvia-2017",
                              "Latvia-2018",
                              "Latvia-2019",
                              "Latvia-2020",
                              "Latvia-2021",
                              "Lithuania-2014",
                              "Lithuania-2015",
                              "Lithuania-2016",
                              "Lithuania-2017",
                              "Lithuania-2018",
                              "Lithuania-2019",
                              "Lithuania-2020",
                              "Lithuania-2021",
                              "Luxembourg-2014",
                              "Luxembourg-2015",
                              "Luxembourg-2016",
                              "Luxembourg-2017",
                              "Luxembourg-2018",
                              "Luxembourg-2019",
                              "Luxembourg-2020",
                              "Luxembourg-2021",
                              "Malta-2014",
                              "Malta-2015",
                              "Malta-2016",
                              "Malta-2017",
                              "Malta-2018",
                              "Malta-2019",
                              "Malta-2020",
                              "Malta-2021",
                              "Montenegro-2014",
                              "Montenegro-2015", 
                              "Montenegro-2016", 
                              "Montenegro-2017", 
                              "Montenegro-2018", 
                              "Montenegro-2019", 
                              "Montenegro-2020", 
                              "Montenegro-2021", 
                              "Netherlands-2014",
                              "Netherlands-2015",
                              "Netherlands-2016",
                              "Netherlands-2017",
                              "Netherlands-2018",
                              "Netherlands-2019",
                              "Netherlands-2020",
                              "Netherlands-2021",
                              "North Macedonia-2014",
                              "North Macedonia-2015",
                              "North Macedonia-2016",
                              "North Macedonia-2017",
                              "North Macedonia-2018",
                              "North Macedonia-2019",
                              "North Macedonia-2020",
                              "North Macedonia-2021",
                              "Norway-2014",
                              "Norway-2015",  
                              "Norway-2016",  
                              "Norway-2017",  
                              "Norway-2018",  
                              "Norway-2019",  
                              "Norway-2020",  
                              "Norway-2021",  
                              "Poland-2014",
                              "Poland-2015",  
                              "Poland-2016",  
                              "Poland-2017",  
                              "Poland-2018",  
                              "Poland-2019",  
                              "Poland-2020",  
                              "Poland-2021",  
                              "Portugal-2014",
                              "Portugal-2015", 
                              "Portugal-2016", 
                              "Portugal-2017", 
                              "Portugal-2018", 
                              "Portugal-2019", 
                              "Portugal-2020", 
                              "Portugal-2021", 
                              "Romania-2014",
                              "Romania-2015",
                              "Romania-2016",
                              "Romania-2017",
                              "Romania-2018",
                              "Romania-2019",
                              "Romania-2020",
                              "Romania-2021",
                              "Serbia-2014",
                              "Serbia-2015",
                              "Serbia-2016",
                              "Serbia-2017",
                              "Serbia-2018",
                              "Serbia-2019",
                              "Serbia-2020",
                              "Serbia-2021",
                              "Slovakia-2014", 
                              "Slovakia-2015",
                              "Slovakia-2016",
                              "Slovakia-2017",
                              "Slovakia-2018",
                              "Slovakia-2019",
                              "Slovakia-2020",
                              "Slovakia-2021",
                              "Slovenia-2014", 
                              "Slovenia-2015",
                              "Slovenia-2016",
                              "Slovenia-2017",
                              "Slovenia-2018",
                              "Slovenia-2019",
                              "Slovenia-2020",
                              "Slovenia-2021",
                              "Spain-2014", 
                              "Spain-2015",
                              "Spain-2016",
                              "Spain-2017",
                              "Spain-2018",
                              "Spain-2019",
                              "Spain-2020",
                              "Spain-2021",
                              "Sweden-2014",   
                              "Sweden-2015",
                              "Sweden-2016",
                              "Sweden-2017",
                              "Sweden-2018",
                              "Sweden-2019",
                              "Sweden-2020",
                              "Sweden-2021",
                              "Switzerland-2014",
                              "Switzerland-2015",
                              "Switzerland-2016",
                              "Switzerland-2017",
                              "Switzerland-2018",
                              "Switzerland-2019",
                              "Switzerland-2020",
                              "Switzerland-2021",
                              "Turkey-2014", 
                              "Turkey-2015",  
                              "Turkey-2016",  
                              "Turkey-2017",  
                              "Turkey-2018",  
                              "Turkey-2019",  
                              "Turkey-2020",  
                              "Turkey-2021",  
                              "Ukraine-2014",
                              "Ukraine-2015",
                              "Ukraine-2016",
                              "Ukraine-2017",
                              "Ukraine-2018",
                              "Ukraine-2019",
                              "Ukraine-2020",
                              "Ukraine-2021",
                              "United Kingdom-2014",
                              "United Kingdom-2015",
                              "United Kingdom-2016",
                              "United Kingdom-2017",
                              "United Kingdom-2018",
                              "United Kingdom-2019",
                              "United Kingdom-2020",
                              "United Kingdom-2021")

colnames(LIMist)<-c( "Austria- 2014",
                              "Austria- 2015",
                              "Austria- 2016",
                              "Austria- 2017",
                              "Austria- 2018",
                              "Austria- 2019",
                              "Austria- 2020",
                              "Austria- 2021",
                              "Belgium -2014",
                              "Belgium -2015",
                              "Belgium -2016",
                              "Belgium -2017",
                              "Belgium -2018",
                              "Belgium -2019",
                              "Belgium -2020",
                              "Belgium -2021",
                              "Bosnia and Herzegovina-2014",
                              "Bosnia and Herzegovina-2015",
                              "Bosnia and Herzegovina-2016",
                              "Bosnia and Herzegovina-2017",
                              "Bosnia and Herzegovina-2018",
                              "Bosnia and Herzegovina-2019",
                              "Bosnia and Herzegovina-2020",
                              "Bosnia and Herzegovina-2021",
                              "Bulgaria-2014", 
                              "Bulgaria-2015",  
                              "Bulgaria-2016",  
                              "Bulgaria-2017",  
                              "Bulgaria-2018",  
                              "Bulgaria-2019",  
                              "Bulgaria-2020",  
                              "Bulgaria-2021",  
                              "Croatia-2014",
                              "Croatia-2015",  
                              "Croatia-2016",  
                              "Croatia-2017",  
                              "Croatia-2018",  
                              "Croatia-2019",  
                              "Croatia-2020",  
                              "Croatia-2021",  
                              "Cyprus-2014",
                              "Cyprus-2015", 
                              "Cyprus-2016", 
                              "Cyprus-2017", 
                              "Cyprus-2018", 
                              "Cyprus-2019", 
                              "Cyprus-2020", 
                              "Cyprus-2021", 
                              "Czechia-2014",  
                              "Czechia-2015", 
                              "Czechia-2016", 
                              "Czechia-2017", 
                              "Czechia-2018", 
                              "Czechia-2019", 
                              "Czechia-2020", 
                              "Czechia-2021", 
                              "Denmark-2014",
                              "Denmark-2015",
                              "Denmark-2016",
                              "Denmark-2017",
                              "Denmark-2018",
                              "Denmark-2019",
                              "Denmark-2020",
                              "Denmark-2021",
                              "Estonia-2014",
                              "Estonia-2015",
                              "Estonia-2016",
                              "Estonia-2017",
                              "Estonia-2018",
                              "Estonia-2019",
                              "Estonia-2020",
                              "Estonia-2021",
                              "EU-2014",
                              "EU-2015", 
                              "EU-2016", 
                              "EU-2017", 
                              "EU-2018", 
                              "EU-2019", 
                              "EU-2020", 
                              "EU-2021", 
                              "Finland-2014", 
                              "Finland-2015",
                              "Finland-2016",
                              "Finland-2017",
                              "Finland-2018",
                              "Finland-2019",
                              "Finland-2020",
                              "Finland-2021",
                              "France-2014", 
                              "France-2015", 
                              "France-2016", 
                              "France-2017", 
                              "France-2018", 
                              "France-2019", 
                              "France-2020", 
                              "France-2021", 
                              "Germany-2014", 
                              "Germany-2015",  
                              "Germany-2016",  
                              "Germany-2017",  
                              "Germany-2018",  
                              "Germany-2019",  
                              "Germany-2020",  
                              "Germany-2021",  
                              "Greece-2014",
                              "Greece-2015",
                              "Greece-2016",
                              "Greece-2017",
                              "Greece-2018",
                              "Greece-2019",
                              "Greece-2020",
                              "Greece-2021",
                              "Hungary-2014", 
                              "Hungary-2015", 
                              "Hungary-2016", 
                              "Hungary-2017", 
                              "Hungary-2018", 
                              "Hungary-2019", 
                              "Hungary-2020", 
                              "Hungary-2021", 
                              "Iceland-2014",
                              "Iceland-2015",
                              "Iceland-2016",
                              "Iceland-2017",
                              "Iceland-2018",
                              "Iceland-2019",
                              "Iceland-2020",
                              "Iceland-2021",
                              "Ireland-2014", 
                              "Ireland-2015",
                              "Ireland-2016",
                              "Ireland-2017",
                              "Ireland-2018",
                              "Ireland-2019",
                              "Ireland-2020",
                              "Ireland-2021",
                              "Israel-2014",
                              "Israel-2015", 
                              "Israel-2016", 
                              "Israel-2017", 
                              "Israel-2018", 
                              "Israel-2019", 
                              "Israel-2020",
                              "Israel-2021", 
                              "Italy-2014",
                              "Italy-2015",
                              "Italy-2016",
                              "Italy-2017",
                              "Italy-2018",
                              "Italy-2019",
                              "Italy-2020",
                              "Italy-2021",
                              "Latvia-2014",
                              "Latvia-2015",
                              "Latvia-2016",
                              "Latvia-2017",
                              "Latvia-2018",
                              "Latvia-2019",
                              "Latvia-2020",
                              "Latvia-2021",
                              "Lithuania-2014",
                              "Lithuania-2015",
                              "Lithuania-2016",
                              "Lithuania-2017",
                              "Lithuania-2018",
                              "Lithuania-2019",
                              "Lithuania-2020",
                              "Lithuania-2021",
                              "Luxembourg-2014",
                              "Luxembourg-2015",
                              "Luxembourg-2016",
                              "Luxembourg-2017",
                              "Luxembourg-2018",
                              "Luxembourg-2019",
                              "Luxembourg-2020",
                              "Luxembourg-2021",
                              "Malta-2014",
                              "Malta-2015",
                              "Malta-2016",
                              "Malta-2017",
                              "Malta-2018",
                              "Malta-2019",
                              "Malta-2020",
                              "Malta-2021",
                              "Montenegro-2014",
                              "Montenegro-2015", 
                              "Montenegro-2016", 
                              "Montenegro-2017", 
                              "Montenegro-2018", 
                              "Montenegro-2019", 
                              "Montenegro-2020", 
                              "Montenegro-2021", 
                              "Netherlands-2014",
                              "Netherlands-2015",
                              "Netherlands-2016",
                              "Netherlands-2017",
                              "Netherlands-2018",
                              "Netherlands-2019",
                              "Netherlands-2020",
                              "Netherlands-2021",
                              "North Macedonia-2014",
                              "North Macedonia-2015",
                              "North Macedonia-2016",
                              "North Macedonia-2017",
                              "North Macedonia-2018",
                              "North Macedonia-2019",
                              "North Macedonia-2020",
                              "North Macedonia-2021",
                              "Norway-2014",
                              "Norway-2015",  
                              "Norway-2016",  
                              "Norway-2017",  
                              "Norway-2018",  
                              "Norway-2019",  
                              "Norway-2020",  
                              "Norway-2021",  
                              "Poland-2014",
                              "Poland-2015",  
                              "Poland-2016",  
                              "Poland-2017",  
                              "Poland-2018",  
                              "Poland-2019",  
                              "Poland-2020",  
                              "Poland-2021",  
                              "Portugal-2014",
                              "Portugal-2015", 
                              "Portugal-2016", 
                              "Portugal-2017", 
                              "Portugal-2018", 
                              "Portugal-2019", 
                              "Portugal-2020", 
                              "Portugal-2021", 
                              "Romania-2014",
                              "Romania-2015",
                              "Romania-2016",
                              "Romania-2017",
                              "Romania-2018",
                              "Romania-2019",
                              "Romania-2020",
                              "Romania-2021",
                              "Serbia-2014",
                              "Serbia-2015",
                              "Serbia-2016",
                              "Serbia-2017",
                              "Serbia-2018",
                              "Serbia-2019",
                              "Serbia-2020",
                              "Serbia-2021",
                              "Slovakia-2014", 
                              "Slovakia-2015",
                              "Slovakia-2016",
                              "Slovakia-2017",
                              "Slovakia-2018",
                              "Slovakia-2019",
                              "Slovakia-2020",
                              "Slovakia-2021",
                              "Slovenia-2014", 
                              "Slovenia-2015",
                              "Slovenia-2016",
                              "Slovenia-2017",
                              "Slovenia-2018",
                              "Slovenia-2019",
                              "Slovenia-2020",
                              "Slovenia-2021",
                              "Spain-2014", 
                              "Spain-2015",
                              "Spain-2016",
                              "Spain-2017",
                              "Spain-2018",
                              "Spain-2019",
                              "Spain-2020",
                              "Spain-2021",
                              "Sweden-2014",   
                              "Sweden-2015",
                              "Sweden-2016",
                              "Sweden-2017",
                              "Sweden-2018",
                              "Sweden-2019",
                              "Sweden-2020",
                              "Sweden-2021",
                              "Switzerland-2014",
                              "Switzerland-2015",
                              "Switzerland-2016",
                              "Switzerland-2017",
                              "Switzerland-2018",
                              "Switzerland-2019",
                              "Switzerland-2020",
                              "Switzerland-2021",
                              "Turkey-2014", 
                              "Turkey-2015",  
                              "Turkey-2016",  
                              "Turkey-2017",  
                              "Turkey-2018",  
                              "Turkey-2019",  
                              "Turkey-2020",  
                              "Turkey-2021",  
                              "Ukraine-2014",
                              "Ukraine-2015",
                              "Ukraine-2016",
                              "Ukraine-2017",
                              "Ukraine-2018",
                              "Ukraine-2019",
                              "Ukraine-2020",
                              "Ukraine-2021",
                              "United Kingdom-2014",
                              "United Kingdom-2015",
                              "United Kingdom-2016",
                              "United Kingdom-2017",
                              "United Kingdom-2018",
                              "United Kingdom-2019",
                              "United Kingdom-2020",
                              "United Kingdom-2021")

#LIM eco 


LIM_eco <- matrix(NA_real_, NCOL(M_eco), NCOL(M_eco))
for (i in seq_len(NCOL(LIM_eco)))
  for (j in seq_len(NCOL(LIM_eco)))
    LIM_eco[i,j] <- min(min(M_eco[i,j], M_eco[j, i])) #no zeri sulla diagonale 

LIMeco =  LIM_eco - diag(diag(LIM_eco))

rownames(LIMeco)<-c( "Austria- 2014",
                     "Austria- 2015",
                     "Austria- 2016",
                     "Austria- 2017",
                     "Austria- 2018",
                     "Austria- 2019",
                     "Austria- 2020",
                     "Austria- 2021",
                     "Belgium -2014",
                     "Belgium -2015",
                     "Belgium -2016",
                     "Belgium -2017",
                     "Belgium -2018",
                     "Belgium -2019",
                     "Belgium -2020",
                     "Belgium -2021",
                     "Bosnia and Herzegovina-2014",
                     "Bosnia and Herzegovina-2015",
                     "Bosnia and Herzegovina-2016",
                     "Bosnia and Herzegovina-2017",
                     "Bosnia and Herzegovina-2018",
                     "Bosnia and Herzegovina-2019",
                     "Bosnia and Herzegovina-2020",
                     "Bosnia and Herzegovina-2021",
                     "Bulgaria-2014", 
                     "Bulgaria-2015",  
                     "Bulgaria-2016",  
                     "Bulgaria-2017",  
                     "Bulgaria-2018",  
                     "Bulgaria-2019",  
                     "Bulgaria-2020",  
                     "Bulgaria-2021",  
                     "Croatia-2014",
                     "Croatia-2015",  
                     "Croatia-2016",  
                     "Croatia-2017",  
                     "Croatia-2018",  
                     "Croatia-2019",  
                     "Croatia-2020",  
                     "Croatia-2021",  
                     "Cyprus-2014",
                     "Cyprus-2015", 
                     "Cyprus-2016", 
                     "Cyprus-2017", 
                     "Cyprus-2018", 
                     "Cyprus-2019", 
                     "Cyprus-2020", 
                     "Cyprus-2021", 
                     "Czechia-2014",  
                     "Czechia-2015", 
                     "Czechia-2016", 
                     "Czechia-2017", 
                     "Czechia-2018", 
                     "Czechia-2019", 
                     "Czechia-2020", 
                     "Czechia-2021", 
                     "Denmark-2014",
                     "Denmark-2015",
                     "Denmark-2016",
                     "Denmark-2017",
                     "Denmark-2018",
                     "Denmark-2019",
                     "Denmark-2020",
                     "Denmark-2021",
                     "Estonia-2014",
                     "Estonia-2015",
                     "Estonia-2016",
                     "Estonia-2017",
                     "Estonia-2018",
                     "Estonia-2019",
                     "Estonia-2020",
                     "Estonia-2021",
                     "EU-2014",
                     "EU-2015", 
                     "EU-2016", 
                     "EU-2017", 
                     "EU-2018", 
                     "EU-2019", 
                     "EU-2020", 
                     "EU-2021", 
                     "Finland-2014", 
                     "Finland-2015",
                     "Finland-2016",
                     "Finland-2017",
                     "Finland-2018",
                     "Finland-2019",
                     "Finland-2020",
                     "Finland-2021",
                     "France-2014", 
                     "France-2015", 
                     "France-2016", 
                     "France-2017", 
                     "France-2018", 
                     "France-2019", 
                     "France-2020", 
                     "France-2021", 
                     "Germany-2014", 
                     "Germany-2015",  
                     "Germany-2016",  
                     "Germany-2017",  
                     "Germany-2018",  
                     "Germany-2019",  
                     "Germany-2020",  
                     "Germany-2021",  
                     "Greece-2014",
                     "Greece-2015",
                     "Greece-2016",
                     "Greece-2017",
                     "Greece-2018",
                     "Greece-2019",
                     "Greece-2020",
                     "Greece-2021",
                     "Hungary-2014", 
                     "Hungary-2015", 
                     "Hungary-2016", 
                     "Hungary-2017", 
                     "Hungary-2018", 
                     "Hungary-2019", 
                     "Hungary-2020", 
                     "Hungary-2021", 
                     "Iceland-2014",
                     "Iceland-2015",
                     "Iceland-2016",
                     "Iceland-2017",
                     "Iceland-2018",
                     "Iceland-2019",
                     "Iceland-2020",
                     "Iceland-2021",
                     "Ireland-2014", 
                     "Ireland-2015",
                     "Ireland-2016",
                     "Ireland-2017",
                     "Ireland-2018",
                     "Ireland-2019",
                     "Ireland-2020",
                     "Ireland-2021",
                     "Israel-2014",
                     "Israel-2015", 
                     "Israel-2016", 
                     "Israel-2017", 
                     "Israel-2018", 
                     "Israel-2019", 
                     "Israel-2020",
                     "Israel-2021", 
                     "Italy-2014",
                     "Italy-2015",
                     "Italy-2016",
                     "Italy-2017",
                     "Italy-2018",
                     "Italy-2019",
                     "Italy-2020",
                     "Italy-2021",
                     "Latvia-2014",
                     "Latvia-2015",
                     "Latvia-2016",
                     "Latvia-2017",
                     "Latvia-2018",
                     "Latvia-2019",
                     "Latvia-2020",
                     "Latvia-2021",
                     "Lithuania-2014",
                     "Lithuania-2015",
                     "Lithuania-2016",
                     "Lithuania-2017",
                     "Lithuania-2018",
                     "Lithuania-2019",
                     "Lithuania-2020",
                     "Lithuania-2021",
                     "Luxembourg-2014",
                     "Luxembourg-2015",
                     "Luxembourg-2016",
                     "Luxembourg-2017",
                     "Luxembourg-2018",
                     "Luxembourg-2019",
                     "Luxembourg-2020",
                     "Luxembourg-2021",
                     "Malta-2014",
                     "Malta-2015",
                     "Malta-2016",
                     "Malta-2017",
                     "Malta-2018",
                     "Malta-2019",
                     "Malta-2020",
                     "Malta-2021",
                     "Montenegro-2014",
                     "Montenegro-2015", 
                     "Montenegro-2016", 
                     "Montenegro-2017", 
                     "Montenegro-2018", 
                     "Montenegro-2019", 
                     "Montenegro-2020", 
                     "Montenegro-2021", 
                     "Netherlands-2014",
                     "Netherlands-2015",
                     "Netherlands-2016",
                     "Netherlands-2017",
                     "Netherlands-2018",
                     "Netherlands-2019",
                     "Netherlands-2020",
                     "Netherlands-2021",
                     "North Macedonia-2014",
                     "North Macedonia-2015",
                     "North Macedonia-2016",
                     "North Macedonia-2017",
                     "North Macedonia-2018",
                     "North Macedonia-2019",
                     "North Macedonia-2020",
                     "North Macedonia-2021",
                     "Norway-2014",
                     "Norway-2015",  
                     "Norway-2016",  
                     "Norway-2017",  
                     "Norway-2018",  
                     "Norway-2019",  
                     "Norway-2020",  
                     "Norway-2021",  
                     "Poland-2014",
                     "Poland-2015",  
                     "Poland-2016",  
                     "Poland-2017",  
                     "Poland-2018",  
                     "Poland-2019",  
                     "Poland-2020",  
                     "Poland-2021",  
                     "Portugal-2014",
                     "Portugal-2015", 
                     "Portugal-2016", 
                     "Portugal-2017", 
                     "Portugal-2018", 
                     "Portugal-2019", 
                     "Portugal-2020", 
                     "Portugal-2021", 
                     "Romania-2014",
                     "Romania-2015",
                     "Romania-2016",
                     "Romania-2017",
                     "Romania-2018",
                     "Romania-2019",
                     "Romania-2020",
                     "Romania-2021",
                     "Serbia-2014",
                     "Serbia-2015",
                     "Serbia-2016",
                     "Serbia-2017",
                     "Serbia-2018",
                     "Serbia-2019",
                     "Serbia-2020",
                     "Serbia-2021",
                     "Slovakia-2014", 
                     "Slovakia-2015",
                     "Slovakia-2016",
                     "Slovakia-2017",
                     "Slovakia-2018",
                     "Slovakia-2019",
                     "Slovakia-2020",
                     "Slovakia-2021",
                     "Slovenia-2014", 
                     "Slovenia-2015",
                     "Slovenia-2016",
                     "Slovenia-2017",
                     "Slovenia-2018",
                     "Slovenia-2019",
                     "Slovenia-2020",
                     "Slovenia-2021",
                     "Spain-2014", 
                     "Spain-2015",
                     "Spain-2016",
                     "Spain-2017",
                     "Spain-2018",
                     "Spain-2019",
                     "Spain-2020",
                     "Spain-2021",
                     "Sweden-2014",   
                     "Sweden-2015",
                     "Sweden-2016",
                     "Sweden-2017",
                     "Sweden-2018",
                     "Sweden-2019",
                     "Sweden-2020",
                     "Sweden-2021",
                     "Switzerland-2014",
                     "Switzerland-2015",
                     "Switzerland-2016",
                     "Switzerland-2017",
                     "Switzerland-2018",
                     "Switzerland-2019",
                     "Switzerland-2020",
                     "Switzerland-2021",
                     "Turkey-2014", 
                     "Turkey-2015",  
                     "Turkey-2016",  
                     "Turkey-2017",  
                     "Turkey-2018",  
                     "Turkey-2019",  
                     "Turkey-2020",  
                     "Turkey-2021",  
                     "Ukraine-2014",
                     "Ukraine-2015",
                     "Ukraine-2016",
                     "Ukraine-2017",
                     "Ukraine-2018",
                     "Ukraine-2019",
                     "Ukraine-2020",
                     "Ukraine-2021",
                     "United Kingdom-2014",
                     "United Kingdom-2015",
                     "United Kingdom-2016",
                     "United Kingdom-2017",
                     "United Kingdom-2018",
                     "United Kingdom-2019",
                     "United Kingdom-2020",
                     "United Kingdom-2021")

colnames(LIMeco)<-c( "Austria- 2014",
                     "Austria- 2015",
                     "Austria- 2016",
                     "Austria- 2017",
                     "Austria- 2018",
                     "Austria- 2019",
                     "Austria- 2020",
                     "Austria- 2021",
                     "Belgium -2014",
                     "Belgium -2015",
                     "Belgium -2016",
                     "Belgium -2017",
                     "Belgium -2018",
                     "Belgium -2019",
                     "Belgium -2020",
                     "Belgium -2021",
                     "Bosnia and Herzegovina-2014",
                     "Bosnia and Herzegovina-2015",
                     "Bosnia and Herzegovina-2016",
                     "Bosnia and Herzegovina-2017",
                     "Bosnia and Herzegovina-2018",
                     "Bosnia and Herzegovina-2019",
                     "Bosnia and Herzegovina-2020",
                     "Bosnia and Herzegovina-2021",
                     "Bulgaria-2014", 
                     "Bulgaria-2015",  
                     "Bulgaria-2016",  
                     "Bulgaria-2017",  
                     "Bulgaria-2018",  
                     "Bulgaria-2019",  
                     "Bulgaria-2020",  
                     "Bulgaria-2021",  
                     "Croatia-2014",
                     "Croatia-2015",  
                     "Croatia-2016",  
                     "Croatia-2017",  
                     "Croatia-2018",  
                     "Croatia-2019",  
                     "Croatia-2020",  
                     "Croatia-2021",  
                     "Cyprus-2014",
                     "Cyprus-2015", 
                     "Cyprus-2016", 
                     "Cyprus-2017", 
                     "Cyprus-2018", 
                     "Cyprus-2019", 
                     "Cyprus-2020", 
                     "Cyprus-2021", 
                     "Czechia-2014",  
                     "Czechia-2015", 
                     "Czechia-2016", 
                     "Czechia-2017", 
                     "Czechia-2018", 
                     "Czechia-2019", 
                     "Czechia-2020", 
                     "Czechia-2021", 
                     "Denmark-2014",
                     "Denmark-2015",
                     "Denmark-2016",
                     "Denmark-2017",
                     "Denmark-2018",
                     "Denmark-2019",
                     "Denmark-2020",
                     "Denmark-2021",
                     "Estonia-2014",
                     "Estonia-2015",
                     "Estonia-2016",
                     "Estonia-2017",
                     "Estonia-2018",
                     "Estonia-2019",
                     "Estonia-2020",
                     "Estonia-2021",
                     "EU-2014",
                     "EU-2015", 
                     "EU-2016", 
                     "EU-2017", 
                     "EU-2018", 
                     "EU-2019", 
                     "EU-2020", 
                     "EU-2021", 
                     "Finland-2014", 
                     "Finland-2015",
                     "Finland-2016",
                     "Finland-2017",
                     "Finland-2018",
                     "Finland-2019",
                     "Finland-2020",
                     "Finland-2021",
                     "France-2014", 
                     "France-2015", 
                     "France-2016", 
                     "France-2017", 
                     "France-2018", 
                     "France-2019", 
                     "France-2020", 
                     "France-2021", 
                     "Germany-2014", 
                     "Germany-2015",  
                     "Germany-2016",  
                     "Germany-2017",  
                     "Germany-2018",  
                     "Germany-2019",  
                     "Germany-2020",  
                     "Germany-2021",  
                     "Greece-2014",
                     "Greece-2015",
                     "Greece-2016",
                     "Greece-2017",
                     "Greece-2018",
                     "Greece-2019",
                     "Greece-2020",
                     "Greece-2021",
                     "Hungary-2014", 
                     "Hungary-2015", 
                     "Hungary-2016", 
                     "Hungary-2017", 
                     "Hungary-2018", 
                     "Hungary-2019", 
                     "Hungary-2020", 
                     "Hungary-2021", 
                     "Iceland-2014",
                     "Iceland-2015",
                     "Iceland-2016",
                     "Iceland-2017",
                     "Iceland-2018",
                     "Iceland-2019",
                     "Iceland-2020",
                     "Iceland-2021",
                     "Ireland-2014", 
                     "Ireland-2015",
                     "Ireland-2016",
                     "Ireland-2017",
                     "Ireland-2018",
                     "Ireland-2019",
                     "Ireland-2020",
                     "Ireland-2021",
                     "Israel-2014",
                     "Israel-2015", 
                     "Israel-2016", 
                     "Israel-2017", 
                     "Israel-2018", 
                     "Israel-2019", 
                     "Israel-2020",
                     "Israel-2021", 
                     "Italy-2014",
                     "Italy-2015",
                     "Italy-2016",
                     "Italy-2017",
                     "Italy-2018",
                     "Italy-2019",
                     "Italy-2020",
                     "Italy-2021",
                     "Latvia-2014",
                     "Latvia-2015",
                     "Latvia-2016",
                     "Latvia-2017",
                     "Latvia-2018",
                     "Latvia-2019",
                     "Latvia-2020",
                     "Latvia-2021",
                     "Lithuania-2014",
                     "Lithuania-2015",
                     "Lithuania-2016",
                     "Lithuania-2017",
                     "Lithuania-2018",
                     "Lithuania-2019",
                     "Lithuania-2020",
                     "Lithuania-2021",
                     "Luxembourg-2014",
                     "Luxembourg-2015",
                     "Luxembourg-2016",
                     "Luxembourg-2017",
                     "Luxembourg-2018",
                     "Luxembourg-2019",
                     "Luxembourg-2020",
                     "Luxembourg-2021",
                     "Malta-2014",
                     "Malta-2015",
                     "Malta-2016",
                     "Malta-2017",
                     "Malta-2018",
                     "Malta-2019",
                     "Malta-2020",
                     "Malta-2021",
                     "Montenegro-2014",
                     "Montenegro-2015", 
                     "Montenegro-2016", 
                     "Montenegro-2017", 
                     "Montenegro-2018", 
                     "Montenegro-2019", 
                     "Montenegro-2020", 
                     "Montenegro-2021", 
                     "Netherlands-2014",
                     "Netherlands-2015",
                     "Netherlands-2016",
                     "Netherlands-2017",
                     "Netherlands-2018",
                     "Netherlands-2019",
                     "Netherlands-2020",
                     "Netherlands-2021",
                     "North Macedonia-2014",
                     "North Macedonia-2015",
                     "North Macedonia-2016",
                     "North Macedonia-2017",
                     "North Macedonia-2018",
                     "North Macedonia-2019",
                     "North Macedonia-2020",
                     "North Macedonia-2021",
                     "Norway-2014",
                     "Norway-2015",  
                     "Norway-2016",  
                     "Norway-2017",  
                     "Norway-2018",  
                     "Norway-2019",  
                     "Norway-2020",  
                     "Norway-2021",  
                     "Poland-2014",
                     "Poland-2015",  
                     "Poland-2016",  
                     "Poland-2017",  
                     "Poland-2018",  
                     "Poland-2019",  
                     "Poland-2020",  
                     "Poland-2021",  
                     "Portugal-2014",
                     "Portugal-2015", 
                     "Portugal-2016", 
                     "Portugal-2017", 
                     "Portugal-2018", 
                     "Portugal-2019", 
                     "Portugal-2020", 
                     "Portugal-2021", 
                     "Romania-2014",
                     "Romania-2015",
                     "Romania-2016",
                     "Romania-2017",
                     "Romania-2018",
                     "Romania-2019",
                     "Romania-2020",
                     "Romania-2021",
                     "Serbia-2014",
                     "Serbia-2015",
                     "Serbia-2016",
                     "Serbia-2017",
                     "Serbia-2018",
                     "Serbia-2019",
                     "Serbia-2020",
                     "Serbia-2021",
                     "Slovakia-2014", 
                     "Slovakia-2015",
                     "Slovakia-2016",
                     "Slovakia-2017",
                     "Slovakia-2018",
                     "Slovakia-2019",
                     "Slovakia-2020",
                     "Slovakia-2021",
                     "Slovenia-2014", 
                     "Slovenia-2015",
                     "Slovenia-2016",
                     "Slovenia-2017",
                     "Slovenia-2018",
                     "Slovenia-2019",
                     "Slovenia-2020",
                     "Slovenia-2021",
                     "Spain-2014", 
                     "Spain-2015",
                     "Spain-2016",
                     "Spain-2017",
                     "Spain-2018",
                     "Spain-2019",
                     "Spain-2020",
                     "Spain-2021",
                     "Sweden-2014",   
                     "Sweden-2015",
                     "Sweden-2016",
                     "Sweden-2017",
                     "Sweden-2018",
                     "Sweden-2019",
                     "Sweden-2020",
                     "Sweden-2021",
                     "Switzerland-2014",
                     "Switzerland-2015",
                     "Switzerland-2016",
                     "Switzerland-2017",
                     "Switzerland-2018",
                     "Switzerland-2019",
                     "Switzerland-2020",
                     "Switzerland-2021",
                     "Turkey-2014", 
                     "Turkey-2015",  
                     "Turkey-2016",  
                     "Turkey-2017",  
                     "Turkey-2018",  
                     "Turkey-2019",  
                     "Turkey-2020",  
                     "Turkey-2021",  
                     "Ukraine-2014",
                     "Ukraine-2015",
                     "Ukraine-2016",
                     "Ukraine-2017",
                     "Ukraine-2018",
                     "Ukraine-2019",
                     "Ukraine-2020",
                     "Ukraine-2021",
                     "United Kingdom-2014",
                     "United Kingdom-2015",
                     "United Kingdom-2016",
                     "United Kingdom-2017",
                     "United Kingdom-2018",
                     "United Kingdom-2019",
                     "United Kingdom-2020",
                     "United Kingdom-2021")
#SVD LIM
SVD_eco_LIM <-svd(LIMeco)
SVD_inn_LIM <-svd(LIMinn)
SVD_ist_LIM <-svd(LIMist)
SVD_dem_LIM <-svd(LIMdem)


#RANKING SVD LIM
ranking_eco_SVD_LIM <- sort(SVD_eco_LIM[[1]], decreasing = TRUE)
ranking_ist_SVD_LIM <- sort(SVD_ist_LIM[[1]], decreasing = TRUE)
ranking_dem_SVD_LIM <- sort(SVD_dem_LIM[[1]], decreasing = TRUE)
ranking_inn_SVD_LIM <- sort(SVD_inn_LIM[[1]], decreasing = TRUE)

#AVERAGE HEIGHT 
avr_height_eco_LIM <- colSums(LIMeco)
avr_height_inn_LIM <- colSums(LIMinn)
avr_height_dem_LIM <- colSums(LIMdem)
avr_height_ist_LIM <- colSums(LIMist)

#RANKING #standardizzati? 
ranking_eco_LIM <- sort(avr_height_eco_LIM, decreasing = TRUE)
ranking_ist_LIM <- sort(avr_height_ist_LIM, decreasing = TRUE)
ranking_dem_LIM <- sort(avr_height_dem_LIM, decreasing = TRUE)
ranking_inn_LIM <- sort(avr_height_inn_LIM, decreasing = TRUE)


plot(ranking_eco_SVD_LIM, avr_height_eco_LIM)
plot(ranking_dem_SVD_LIM, avr_height_dem_LIM)
plot(ranking_ist_SVD_LIM, avr_height_ist_LIM)
plot(ranking_inn_SVD_LIM, avr_height_inn_LIM)

#METODO PROF PAPER 2020

avr_height_eco_LIM <- colSums(LIMeco)
avr_height_inn_LIM <- colSums(LIMinn)
avr_height_dem_LIM <- colSums(LIMdem)
avr_height_ist_LIM <- colSums(LIMist)


#trovare eigenvalue per poi trovare eigenvector


library(rARPACK)
eigs(LIMeco, 1)           
eigen_vector_eco <-eigs_sym(LIMeco, 1)


library(rARPACK)
eigs(LIMinn, 1)           
eigen_vector_inn <-eigs_sym(LIMinn, 1)


library(rARPACK)
eigs(LIMist, 1)           
eigen_vector_ist <-eigs_sym(LIMist, 1)



library(rARPACK)
eigs(LIMdem, 1)           
eigen_vector_dem <-eigs_sym(LIMdem, 1) #perchè sono negativi 


#proviamo a plottare 



#prima normalizziamo gli avg_height 

#economia
min_eco <- min(avr_height_eco_LIM)
max_eco <- max(avr_height_eco_LIM)
avg_norm_eco = (avr_height_eco_LIM - min_eco ) / ( max_eco - min_eco)
plot(eigen_vector_eco$vectors, avg_norm_eco)

#istruzione

min_ist <- min(avr_height_ist_LIM)
max_ist <- max(avr_height_ist_LIM)
avg_norm_ist = (avr_height_ist_LIM - min_ist ) / ( max_ist - min_ist)
plot(eigen_vector_ist$vectors, avg_norm_ist)


#innovazione 


min_inn <- min(avr_height_inn_LIM)
max_inn <- max(avr_height_inn_LIM)
avg_norm_inn = (avr_height_inn_LIM - min_inn ) / ( max_inn - min_inn)
plot(eigen_vector_inn$vectors, avg_norm_inn)



#demografia 


min_dem <- min(avr_height_dem_LIM)
max_dem <- max(avr_height_dem_LIM)
avg_norm_dem = (avr_height_dem_LIM - min_dem ) / ( max_dem - min_dem)
plot(eigen_vector_dem$vectors, avg_norm_dem)


#ora proviamo a normalizzare la SVD su LIM-> incomparability(?) 

min_eco_svd <- min(ranking_eco_SVD_LIM)
max_eco_svd <- max(ranking_eco_SVD_LIM)
ranking_eco_SVD_LIM_norm = (ranking_eco_SVD_LIM- min_eco_svd ) / ( max_eco_svd - min_eco_svd)
plot(ranking_eco_SVD_LIM_norm, eigen_vector_eco$vectors)

plot(ranking_eco_SVD_LIM_norm, avg_norm_eco)

#normalizziamo la av dei ranking normali 

#economia
min_eco_rank <- min(avr_height_eco)
max_eco_rank <- max(avr_height_eco)
avr_height_eco_norm = (avr_height_eco- min_eco_rank ) / ( max_eco_rank - min_eco_rank)
plot( avg_norm_eco, avr_height_eco_norm)
ranking_eco <- avr_height_eco_norm
incomparibilità <-avg_norm_eco 
plot(incomparibilità, ranking)
library(lapply)
ranking_eco_ord <- sort(ranking_eco, decreasing=TRUE)

#istruzione

min_ist_rank <- min(avr_height_ist)
max_ist_rank <- max(avr_height_ist)
avr_height_ist_norm = (avr_height_ist - min_ist_rank ) / ( max_ist_rank - min_ist_rank)
plot( avg_norm_ist, avr_height_ist_norm)
ranking_ist <- avr_height_ist_norm
incomparibilità <-avg_norm_ist
plot(incomparibilità, ranking)
ranking_ist_ord <- sort(ranking_ist, decreasing=TRUE)

#innovazione 

min_inn_rank <- min(avr_height_inn)
max_inn_rank <- max(avr_height_inn)
avr_height_inn_norm = (avr_height_inn - min_inn_rank ) / ( max_inn_rank - min_inn_rank)
plot( avg_norm_inn, avr_height_inn_norm)
ranking_inn <- avr_height_inn_norm
incomparibilità <-avg_norm_inn
plot(incomparibilità, ranking)
ranking_inn_ord <- sort(ranking_inn, decreasing=TRUE)

#demografia

min_dem_rank <- min(avr_height_dem)
max_dem_rank <- max(avr_height_dem)
avr_height_dem_norm = (avr_height_dem - min_dem_rank ) / ( max_dem_rank - min_dem_rank)
plot( avg_norm_dem, avr_height_dem_norm)
ranking_dem <- avr_height_dem_norm
incomparibilità <-avg_norm_dem
plot(incomparibilità, ranking_dem)
ranking_dem_ord <- sort(ranking_dem, decreasing=TRUE)

plot(ranking_dem_ord)
plot(ranking_inn_ord)
plot(ranking_ist_ord)
plot(ranking_eco_ord)
