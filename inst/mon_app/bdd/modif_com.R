library(tidyverse)


BalanceCommune_2016 <- read_csv2("C:/Users/cgirard-adc/Desktop/Base comptables/balance_communes/Balance_Commune_2016.csv")

base_com  <- BalanceCommune_2016
remove(BalanceCommune_2016)

##Preparation de la base ====

base_com <- rename(base_com,obDEB=OBNETDEB,ordreDEB=OOBDEB,obCRE=OBNETCRE,ordreCRE=OOBCRE)
base_com$reelDEB=base_com$obDEB-base_com$ordreDEB
base_com$reelCRE=base_com$obCRE-base_com$ordreCRE

base_com$compte1 <- substr(base_com$COMPTE,1,1)
base_com$compte2 <- substr(base_com$COMPTE,1,2)
base_com$compte3 <- substr(base_com$COMPTE,1,3)
base_com$compte4 <- substr(base_com$COMPTE,1,4)
base_com$compte5 <- substr(base_com$COMPTE,1,5)
base_com$compte6 <- substr(base_com$COMPTE,1,6)

## sÃ©lection des donnÃ©es====

base_com_BP  <- base_com[base_com$CBUDG == 1,]
base_com_BA  <- base_com[base_com$BUDGET == "BA" & !base_com$NOMEN == "M22",]
list  <-base_com_BP %>% group_by(SIREN,LBUDG) %>% summarise(n=n()) %>% select(SIREN,LBUDG)

resultat  <- list %>% select(SIREN,LBUDG)

dep_invest  <- list %>% select(SIREN)

rec_invest  <- list %>% select(SIREN)

resultat  <- base_com %>% select(SIREN) %>% distinct()
write.csv2(resultat,"test.csv")
#dep invest
dep_invest  <- base_com_BP %>% filter((compte2 %in% c(13,20,21,23,26,27) | compte3 %in% c(102,454,456,458,481)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(dep_invest_1=sum(reelDEB)/1000) %>% full_join(dep_invest,by="SIREN") %>% replace(is.na(.),0)
dep_invest <- base_com_BP %>% filter(compte3 %in% c(237,238))  %>% group_by(SIREN) %>% summarise(dep_invest_2=sum(reelCRE)/1000) %>%  full_join(dep_invest,by="SIREN") %>% replace(is.na(.),0)
resultat <- dep_invest %>% mutate(dep_invest=dep_invest_1-dep_invest_2) %>% select(SIREN,dep_invest) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
rec_invest  <- base_com_BP %>% filter((compte2 %in% c(13,20,21,26,27) | compte3 %in% c(102,231,232,454,456,458)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(rec_invest_1=sum(reelCRE)/1000) %>% full_join(rec_invest,by="SIREN") %>% replace(is.na(.),0)
rec_invest  <- base_com_BP %>% filter(compte3 %in% c(103,775))  %>% group_by(SIREN) %>% summarise(rec_invest_2=sum(reelCRE-reelDEB)/1000) %>%  full_join(rec_invest,by="SIREN") %>% replace(is.na(.),0)
resultat <- rec_invest %>% mutate(rec_invest=rec_invest_1+rec_invest_2) %>% select(SIREN,rec_invest) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)

rec_invest  <- list %>% select(SIREN)
dep_invest  <- list %>% select(SIREN)

dep_invest  <- base_com_BA %>% filter((compte2 %in% c(13,20,21,23,26,27) | compte3 %in% c(102,454,456,458,481)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(dep_invest_1=sum(reelDEB)/1000) %>% full_join(dep_invest,by="SIREN") %>% replace(is.na(.),0)
dep_invest <- base_com_BA %>% filter(compte3 %in% c(237,238))  %>% group_by(SIREN) %>% summarise(dep_invest_2=sum(reelCRE)/1000) %>%  full_join(dep_invest,by="SIREN") %>% replace(is.na(.),0)
resultat <- dep_invest %>% mutate(dep_invest_ba=dep_invest_1-dep_invest_2) %>% select(SIREN,dep_invest_ba) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)

rec_invest  <- base_com_BA %>% filter((compte2 %in% c(13,20,21,26,27) | compte3 %in% c(102,231,232,454,456,458)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(rec_invest_1=sum(reelCRE)/1000) %>% full_join(rec_invest,by="SIREN") %>% replace(is.na(.),0)
rec_invest  <- base_com_BA %>% filter(compte3 %in% c(103,775))  %>% group_by(SIREN) %>% summarise(rec_invest_2=sum(reelCRE-reelDEB)/1000) %>%  full_join(rec_invest,by="SIREN") %>% replace(is.na(.),0)
resultat <- rec_invest %>% mutate(rec_invest_ba=rec_invest_1+rec_invest_2) %>% select(SIREN,rec_invest_ba) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)


remove(base_com,base_com_BA,base_com_BP)
gc()



#population
table <- read_csv2("Z:/BDD et outils/Bases de donnees/bd communes/com_nouvelles/table_14_15_16.csv")

table_16 <- table %>%
  select(SIREN_16,code_insee16) %>%
  distinct() %>%
  rename(SIREN = SIREN_16)


resultat <- resultat %>% left_join(table_16,by="SIREN")

resultat_16 <- resultat


gc()


#2eme etape

resultat_14$annee = 2014
resultat_15$annee = 2015
resultat_16$annee = 2016


resultat <- rbind(resultat_14,resultat_15,resultat_16)

resultat <- resultat %>% group_by(code_insee16,annee) %>% summarise(dep_invest = sum(dep_invest,na.rm = T),dep_invest_ba = sum(dep_invest_ba,na.rm = T),rec_invest = sum(rec_invest,na.rm = T),rec_invest_ba = sum(rec_invest_ba,na.rm = T))

warnings()

base_invest <- appliInvest::base_invest_com

base_invest <- base_invest %>% select(-dep_invest,-dep_invest_ba,-rec_invest,-rec_invest_ba)

resultat <- resultat %>% rename(code_insee16 = code_insee)

base_invest %>% filter(annee == 2014) %>% summarise(test = sum(dep_invest))

base_invest <- left_join(base_invest,resultat,by=c("code_insee16","annee"))

write.csv2(base_invest,"C:/Users/cgirard-adc/Desktop/Programme R/appliInvest/inst/mon_app/bdd/base_invest_com.csv",row.names = F)

base_invest_com <- base_invest

devtools::use_data(base_invest_com,pkg = ".",overwrite = T)
