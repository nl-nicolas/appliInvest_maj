library(tidyverse)

BalanceCommune_2016 <- read_csv2("C:/Users/cgirard-adc/Desktop/Base comptables/balances_gfp_synd_ept/Balance_GFP_EPT_2016.csv",col_types =cols(NDEPT = col_character()))


base_com  <- BalanceCommune_2016
remove(BalanceCommune_2016)

##Preparation de la base

base_com <- rename(base_com,obDEB=OBNETDEB,ordreDEB=OOBDEB,obCRE=OBNETCRE,ordreCRE=OOBCRE)
base_com$reelDEB=base_com$obDEB-base_com$ordreDEB
base_com$reelCRE=base_com$obCRE-base_com$ordreCRE

base_com$compte1 <- substr(base_com$COMPTE,1,1)
base_com$compte2 <- substr(base_com$COMPTE,1,2)
base_com$compte3 <- substr(base_com$COMPTE,1,3)
base_com$compte4 <- substr(base_com$COMPTE,1,4)
base_com$compte5 <- substr(base_com$COMPTE,1,5)
base_com$compte6 <- substr(base_com$COMPTE,1,6)


base_com  <- base_com[!base_com$NOMEN == "M22",]

base_com <- base_com[base_com$EXER == 2016,]
base_com_BP  <- base_com %>% filter(CTYPE %in% c(401,402,403,404,405,705,425))
base_com_M57 <- base_com_BP[base_com_BP$NOMEN == "M57",]
base_com_M14 <- base_com_BP[base_com_BP$NOMEN %in% c("M14","M14A"),]
base_com_BA <- base_com %>% filter(CTYPE %in% c(495,440))
list  <-base_com_BP %>% group_by(SIREN,LBUDG) %>% summarise(n=n()) %>% select(SIREN,LBUDG)
list_m57  <-base_com_M57 %>% group_by(SIREN,LBUDG) %>% summarise(n=n()) %>% select(SIREN,LBUDG)
list_m14  <-base_com_M14 %>% group_by(SIREN,LBUDG) %>% summarise(n=n()) %>% select(SIREN,LBUDG)

resultat  <- list %>% select(SIREN,LBUDG)
resultat_m57  <- list_m57 %>% select(SIREN,LBUDG)
resultat_m14  <- list_m14 %>% select(SIREN,LBUDG)

dep_invest_57  <- list_m57 %>% select(SIREN)
dep_invest_14  <- list_m14 %>% select(SIREN)

rec_invest_57  <- list_m57 %>% select(SIREN)
rec_invest_14  <- list_m14 %>% select(SIREN)

gc()
test  <- base_com_M14 %>% filter((compte2 %in% c(13,20,21,23,26,27) | compte3 %in% c(102,454,456,458,481))) %>% select(compte1,compte2,compte3,compte4,compte5)
write.csv2(test,"test.csv")

# Depenses d'investissement
dep_invest_14  <- base_com_M14 %>% filter((compte2 %in% c(13,20,21,23,26,27) | compte3 %in% c(102,454,456,458,481)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(dep_invest_1=sum(reelDEB)/1000) %>% full_join(dep_invest_14,by="SIREN") %>% replace(is.na(.),0)
dep_invest_14 <- base_com_M14 %>% filter(compte3 %in% c(237,238))  %>% group_by(SIREN) %>% summarise(dep_invest_2=sum(reelCRE)/1000) %>%  full_join(dep_invest_14,by="SIREN") %>% replace(is.na(.),0)
resultat_m14 <- dep_invest_14 %>% mutate(dep_invest=dep_invest_1-dep_invest_2) %>% select(SIREN,dep_invest) %>% full_join(resultat_m14,by="SIREN") %>% replace(is.na(.),0)
dep_invest_57  <- base_com_M57 %>% filter((compte2 %in% c(13,20,21,23,26,27) | compte3 %in% c(102,454,455,458,481)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(dep_invest_1=sum(reelDEB)/1000) %>% full_join(dep_invest_57,by="SIREN") %>% replace(is.na(.),0)
dep_invest_57 <- base_com_M57 %>% filter(compte3 %in% c(237,238))  %>% group_by(SIREN) %>% summarise(dep_invest_2=sum(reelCRE)/1000) %>%  full_join(dep_invest_57,by="SIREN") %>% replace(is.na(.),0)
resultat_m57 <- dep_invest_57 %>% mutate(dep_invest=dep_invest_1-dep_invest_2) %>% select(SIREN,dep_invest) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)
# Recette d'investissements
rec_invest_14 <- base_com_M14 %>% filter((compte2 %in% c(13,20,21,26,27) | compte3 %in% c(102,231,232,454,456,458)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(rec_invest_1=sum(reelCRE)/1000) %>% full_join(rec_invest_14,by="SIREN") %>% replace(is.na(.),0)
rec_invest_14 <- base_com_M14 %>% filter(compte3 %in% c(103,775))  %>% group_by(SIREN) %>% summarise(rec_invest_2=sum(reelCRE-reelDEB)/1000) %>%  full_join(rec_invest_14,by="SIREN") %>% replace(is.na(.),0)
rec_invest_14 <- rec_invest_14 %>% mutate(rec_invest=rec_invest_1+rec_invest_2)
resultat_m14 <- rec_invest_14 %>% select(SIREN,rec_invest) %>% full_join(resultat_m14,by="SIREN") %>% replace(is.na(.),0)
rec_invest_57 <- base_com_M57 %>% filter((compte2 %in% c(13,20,21,26,27) | compte3 %in% c(102,231,232,454,455,458)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(rec_invest_1=sum(reelCRE)/1000) %>% full_join(rec_invest_57,by="SIREN") %>% replace(is.na(.),0)
rec_invest_57 <- base_com_M57 %>% filter(compte3 %in% c(103,775))  %>% group_by(SIREN) %>% summarise(rec_invest_2=sum(reelCRE-reelDEB)/1000) %>%  full_join(rec_invest_57,by="SIREN") %>% replace(is.na(.),0)
rec_invest_57 <- rec_invest_57 %>% mutate(rec_invest=rec_invest_1+rec_invest_2)
resultat_m57 <- rec_invest_57 %>% select(SIREN,rec_invest) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)

resultat <- rbind(resultat_m14,resultat_m57)

rec_invest <- base_com_BA %>% select(SIREN) %>% distinct()
dep_invest  <- base_com_BA %>% select(SIREN) %>% distinct()

# depense d'investissement
dep_invest  <- base_com_BA %>% filter((compte2 %in% c(13,20,21,23,26,27) | compte3 %in% c(102,454,456,458,481)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(dep_invest_1=sum(reelDEB)/1000) %>% full_join(dep_invest,by="SIREN") %>% replace(is.na(.),0)
dep_invest <- base_com_BA %>% filter(compte3 %in% c(237,238))  %>% group_by(SIREN) %>% summarise(dep_invest_2=sum(reelCRE)/1000) %>%  full_join(dep_invest,by="SIREN") %>% replace(is.na(.),0)
resultat <- dep_invest %>% mutate(dep_invest_ba=dep_invest_1-dep_invest_2) %>% select(SIREN,dep_invest_ba) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
# recette d'investissement
rec_invest  <- base_com_BA %>% filter((compte2 %in% c(13,20,21,26,27) | compte3 %in% c(102,231,232,454,456,458)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(rec_invest_1=sum(reelCRE)/1000) %>% full_join(rec_invest,by="SIREN") %>% replace(is.na(.),0)
rec_invest  <- base_com_BA %>% filter(compte3 %in% c(103,775))  %>% group_by(SIREN) %>% summarise(rec_invest_2=sum(reelCRE-reelDEB)/1000) %>%  full_join(rec_invest,by="SIREN") %>% replace(is.na(.),0)
resultat <- rec_invest %>% mutate(rec_invest_ba=rec_invest_1+rec_invest_2) %>% select(SIREN,rec_invest_ba) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)

remove(base_com,base_com_BA,base_com_BP,base_com_M14,base_com_M57)
gc()

base_invest <- appliInvest::base_invest_gfp

base_invest <- base_invest %>% select(-dep_invest,-dep_invest_ba,-rec_invest,-rec_invest_ba)


resultat <- resultat %>% rename(siren_epci = SIREN)
resultat <- resultat %>% select(-LBUDG)

base_invest <- left_join(base_invest,resultat,by=c("siren_epci"))
base_invest %>% filter(nj_epci2016 != "EPT") %>% summarise(test = sum(dep_invest,na.rm = T))

write.csv2(base_invest,"C:/Users/cgirard-adc/Desktop/Programme R/appliInvest/inst/mon_app/bdd/base_invest_gfp.csv",row.names = F)

base_invest_gfp <- base_invest

devtools::use_data(base_invest_gfp,pkg = ".",overwrite = T)
