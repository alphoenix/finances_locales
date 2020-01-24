library(readODS)
library(tidyverse)
library(stringr)
library(DescTools)

### PEREQUATIONS ###

balances_2017<-read_delim("Balance_Commune_2017_Dec2018.Csv",";")

read_delim("Balance_GfpEptML_2017_Dec2018.csv",";") %>% 
  filter((CBUDG == "1" & COMPTE == "732221" & NOMEN == "M57"))  %>%
  mutate(OBNETCRE = as.numeric(gsub(",",".",OBNETCRE)),OOBCRE = as.numeric(gsub(",",".",OOBCRE))) %>%
  mutate(fsip = OBNETCRE-OOBCRE)

dsu<-read_delim("Dotations Commune_c12_2017.csv","\t", escape_double = FALSE, col_names = FALSE,trim_ws = TRUE, skip = 1) %>%
  setNames(c("commune","dsu")) %>%
  separate(commune,sep = " - ",into = "code",extra = "drop") %>%
  mutate(dsu = as.numeric(gsub(" ","",dsu)))

dsr_bc<-read_delim("Dotations Commune_c13_2017.csv","\t", escape_double = FALSE, col_names = FALSE,trim_ws = TRUE, skip = 1) %>%
  setNames(c("commune","dsr_bc")) %>%
  separate(commune,sep = " - ",into = "code",extra = "drop") %>%
  mutate(dsr_bc = as.numeric(gsub(" ","",dsr_bc)))

dsr_p<-read_delim("Dotations Commune_c14_2017.csv","\t", escape_double = FALSE, col_names = FALSE,trim_ws = TRUE, skip = 1) %>%
  setNames(c("commune","dsr_p")) %>%
  separate(commune,sep = " - ",into = "code",extra = "drop") %>%
  mutate(dsr_p = as.numeric(gsub(" ","",dsr_p)))

dsr_c<-read_delim("Dotations Commune_c18_2017.csv","\t", escape_double = FALSE, col_names = FALSE,trim_ws = TRUE, skip = 1) %>%
  setNames(c("commune","dsr_c")) %>%
  separate(commune,sep = " - ",into = "code",extra = "drop") %>%
  mutate(dsr_c = as.numeric(gsub(" ","",dsr_c)))

dnp<-read_delim("Dotations Commune_c15_2017.csv","\t", escape_double = FALSE, col_names = FALSE,trim_ws = TRUE, skip = 1) %>%
  setNames(c("commune","dnp")) %>%
  separate(commune,sep = " - ",into = "code",extra = "drop") %>%
  mutate(dnp = as.numeric(gsub(" ","",dnp)))

fsrif<-read_delim("Dotations Commune_c24_2017.csv","\t", escape_double = FALSE, col_names = FALSE,trim_ws = TRUE, skip = 1) %>%
  setNames(c("commune","fsrif")) %>%
  separate(commune,sep = " - ",into = "code",extra = "drop") %>%
  mutate(fsrif = as.numeric(gsub(" ","",fsrif)))

fpic_credit<-balances_2017 %>%
  filter(CBUDG == "1" & COMPTE == "73223" & (NOMEN == "M14" | NOMEN == "M14A"))  %>%
  mutate(OBNETCRE = as.numeric(gsub(",",".",OBNETCRE)),OOBCRE = as.numeric(gsub(",",".",OOBCRE))) %>%
  mutate(credit_fpic = OBNETCRE-OOBCRE) %>%
  mutate(code_insee = ifelse(NDEPT %in% c("971","972","973","974","976"),paste("97",INSEE,sep = ""),paste(substr(NDEPT,2,3),INSEE,sep = ""))) %>%
  select(code_insee,credit_fpic) 

fpic_credit %>%
  filter(credit_fpic > 0) %>%
  summarise(somme = sum(credit,na.rm=TRUE))

fpic_versement<-balances_2017 %>%
  filter(CBUDG == "1" & COMPTE == "739223" & (NOMEN == "M14" | NOMEN == "M14A"))  %>%
  mutate(OBNETDEB = as.numeric(gsub(",",".",OBNETDEB)),OOBDEB = as.numeric(gsub(",",".",OOBDEB))) %>%
  mutate(debit_fpic = OBNETDEB-OOBDEB) %>%
  mutate(code_insee = ifelse(NDEPT %in% c("971","972","973","974","976"),paste("97",INSEE,sep = ""),paste(substr(NDEPT,2,3),INSEE,sep = ""))) %>%
  select(code_insee,debit_fpic)

fpic_versement %>%
  filter(debit_fpic > 0) %>%
  summarise(somme = sum(debit,na.rm=TRUE))

recap <- readxl::read_excel("criteres_repartition.xlsx") %>%
  mutate(`code collectivite` = str_pad(as.character(`code collectivite`), 5, pad = "0")) %>%
  full_join(fpic_credit,by = c("code collectivite" = "code_insee")) %>%
  full_join(fpic_versement,by = c("code collectivite" = "code_insee")) %>%
  full_join(dsu,by=c("code collectivite" = "code")) %>%
  full_join(dsr_bc,by=c("code collectivite" = "code")) %>%
  full_join(dsr_p,by=c("code collectivite" = "code")) %>%
  full_join(dsr_c,by=c("code collectivite" = "code")) %>%
  full_join(dnp,by=c("code collectivite" = "code")) %>%
  full_join(fsrif,by=c("code collectivite" = "code")) %>%
  mutate(dsr = dsr_p + dsr_bc + dsr_c) %>%
  mutate(solde_fpic = credit_fpic - debit_fpic) %>%
  mutate(dnp = replace(dnp, is.na(dnp), 0),
         fsrif = replace(fsrif, is.na(fsrif), 0),
         dsr = replace(dsr, is.na(dsr), 0),
         dsu = replace(dsu, is.na(dsu), 0),
         solde_fpic = replace(solde_fpic, is.na(solde_fpic), 0)) %>%
  mutate(perequations = dnp + fsrif + dsr + dsu + solde_fpic) %>%
  left_join(.,readxl::read_excel("~/INSEE/table-appartenance-geo-communes-19.xls",skip=5),by = c("code collectivite"="CODGEO")) %>%
  left_join(.,read_delim("ofgl-base-communes.csv",";", escape_double = FALSE, trim_ws = TRUE),by=c("code collectivite" = "Code INSEE")) %>%
  mutate(recette_avantp = MONTANT - perequations) %>%
  filter(POPULATION > 0) %>%
  mutate(avantp_hab = recette_avantp / POPULATION,
         apresp_hab = MONTANT / POPULATION) %>%
  select(`code collectivite`,LIBGEO,dsu,dsr,dnp,solde_fpic,fsrif,perequations,MONTANT,avantp_hab,apresp_hab)

recap %>% write_csv("dotations_par_commune.csv")
   
## DISPERSION ##

Gini((recap %>% filter(avantp_hab > 0))$avantp_hab,na.rm = TRUE)
Gini((recap %>% filter(apresp_hab > 0))$apresp_hab,na.rm = TRUE)



### BUDGETS PAR FONCTION ###

com_fonction_2018<-read_delim("BalanceSPL_Fonction_2018_Dec2019.csv",";", escape_double = FALSE, trim_ws = TRUE) %>%
  filter(CATEG == "Commune") %>%
  mutate(ANNEE = "2018")

