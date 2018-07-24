library(readr)
library(dplyr)
library(tidyr)


## Les base_invests 2016 ====

##Preparation de la base
Balancedep_2016 <- read.csv2("C:/Users/cgirard-adc/Desktop/Base comptables/balance_dep/Balance_DEPT_2016.csv")

base_com  <- Balancedep_2016
gc()

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
base_com_BP  <- base_com[base_com$CBUDG == 1,]
base_com_BA  <- base_com[base_com$CBUDG == 3,]

base_com_M52  <- base_com_BP[base_com_BP$NOMEN == "M52",]
base_com_M57  <- base_com_BP[base_com_BP$NOMEN == "M57",]

resultat  <- base_com_BP %>% select(SIREN,LBUDG,NDEPT) %>% distinct()
resultat_m57  <- base_com_M57 %>% select(SIREN,LBUDG) %>% distinct()
resultat_m52  <- base_com_M52 %>% select(SIREN,LBUDG) %>% distinct()

dep_invest_57  <- base_com_M57 %>% select(SIREN) %>% distinct()
dep_invest_52  <- base_com_M52 %>% select(SIREN) %>% distinct()

rec_invest  <- base_com_BP %>% select(SIREN) %>% distinct()


# Charges de fonctionnement
resultat  <- base_com_BP %>% filter(compte1==6 & !compte2 %in% c(68) & !compte3 %in% c(675,676)) %>% group_by(SIREN) %>% summarise(dep_fonc=sum(reelDEB-reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte2 %in% c(68)) %>% group_by(SIREN) %>% summarise(dot_prov=sum(reelDEB-reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)

# Produits de fonctionnement
resultat  <- base_com_BP %>% filter(compte1==7 & !compte2 %in% c(78) & !compte3 %in% c(775,776,777))  %>% group_by(SIREN) %>% summarise(rec_fonc=sum(reelCRE-reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte2 %in% c(78))  %>% group_by(SIREN) %>% summarise(repri_prov=sum(reelCRE-reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)

# Depenses d'investissement
dep_invest_52  <- base_com_M52 %>% filter((compte2 %in% c(13,20,21,23,26,27) | compte3 %in% c(102,454,455,458,481)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(dep_invest_1=sum(reelDEB)/1000) %>% full_join(dep_invest_52,by="SIREN") %>% replace(is.na(.),0)
dep_invest_52 <- base_com_M52 %>% filter(compte3 %in% c(236,237,238))  %>% group_by(SIREN) %>% summarise(dep_invest_2=sum(reelCRE)/1000) %>%  full_join(dep_invest_52,by="SIREN") %>% replace(is.na(.),0)
resultat_m52 <- dep_invest_52 %>% mutate(dep_invest=dep_invest_1-dep_invest_2) %>% select(SIREN,dep_invest) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)
dep_invest_57  <- base_com_M57 %>% filter((compte2 %in% c(13,20,21,23,26,27) | compte3 %in% c(102,454,455,458,481)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(dep_invest_1=sum(reelDEB)/1000) %>% full_join(dep_invest_57,by="SIREN") %>% replace(is.na(.),0)
dep_invest_57 <- base_com_M57 %>% filter(compte3 %in% c(236,237,238))  %>% group_by(SIREN) %>% summarise(dep_invest_2=sum(reelCRE)/1000) %>%  full_join(dep_invest_57,by="SIREN") %>% replace(is.na(.),0)
resultat_m57 <- dep_invest_57 %>% mutate(dep_invest=dep_invest_1-dep_invest_2) %>% select(SIREN,dep_invest) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)

## compte 21
resultat <- base_com_BP %>% filter(compte2 == 21) %>% group_by(SIREN) %>% summarise(compte21 = sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
#compte 23
resultat <- base_com_BP %>% filter(compte2 == 23) %>% group_by(SIREN) %>% summarise(compte23 = sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
##  Depenses d'équipements
dep_invest_52  <- base_com_M52 %>% filter(compte2 %in% c(20,21,23) & !compte3 %in% c(204))  %>% group_by(SIREN) %>% summarise(dep_invest_3=sum(reelDEB)/1000) %>%  full_join(dep_invest_52,by="SIREN")  %>% replace(is.na(.),0)
resultat_m52  <- dep_invest_52 %>% mutate(dep_equip=dep_invest_3-dep_invest_2) %>% select(SIREN,dep_equip) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)
dep_invest_57  <- base_com_M57 %>% filter(compte2 %in% c(20,21,23) & !compte3 %in% c(204))  %>% group_by(SIREN) %>% summarise(dep_invest_3=sum(reelDEB)/1000) %>%  full_join(dep_invest_57,by="SIREN")  %>% replace(is.na(.),0)
resultat_m57  <- dep_invest_57 %>% mutate(dep_equip=dep_invest_3-dep_invest_2) %>% select(SIREN,dep_equip) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)
resultat_m52  <- base_com_M52 %>% filter(compte2 %in% c(20,21,23) & !compte3 %in% c(204))  %>% group_by(SIREN) %>% summarise(dep_equip_brut=sum(reelDEB)/1000) %>%  full_join(resultat_m52,by="SIREN")  %>% replace(is.na(.),0)
resultat_m57  <- base_com_M57 %>% filter(compte2 %in% c(20,21,23) & !compte3 %in% c(204))  %>% group_by(SIREN) %>% summarise(dep_equip_brut=sum(reelDEB)/1000) %>%  full_join(resultat_m57,by="SIREN")  %>% replace(is.na(.),0)


#terrain
resultat  <- base_com_BP %>% filter(compte3 %in% c(211,212) | compte4 %in% c(2171,2172,2312)) %>% group_by(SIREN) %>% summarise(terrains=sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
#construction
resultat  <- base_com_BP %>% filter(compte3 %in% c(213,214) | compte4 %in% c(2173,2174,2313,2314)) %>% group_by(SIREN) %>% summarise(constructions=sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
# reseau
resultat  <- base_com_BP %>% filter(compte3 %in% c(215) | compte4 %in% c(2175,2315)) %>% group_by(SIREN) %>% summarise(reseaux=sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
#bien meuble
resultat  <- base_com_BP %>% filter(compte3 %in% c(218) | compte4 %in% c(2178,2318)) %>% group_by(SIREN) %>% summarise(bien_meuble=sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)

##  Subventions
resultat_m52  <- base_com_M52 %>% filter(compte3 %in% c(204)) %>% group_by(SIREN) %>% summarise(subvention_204=sum(reelDEB)/1000) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)
resultat_m52  <- base_com_M52 %>% filter(compte5 %in% c(20411)) %>% group_by(SIREN) %>% summarise(sub_204_etat=sum(reelDEB)/1000) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)
resultat_m52  <- base_com_M52 %>% filter(compte5 %in% c(20414)) %>% group_by(SIREN) %>% summarise(sub_204_bc=sum(reelDEB)/1000) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)
resultat_m52  <- base_com_M52 %>% filter(compte4 %in% c(2041) & !compte5 %in% c(20411,20414)) %>% group_by(SIREN) %>% summarise(sub_204_autres=sum(reelDEB)/1000) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)
resultat_m52  <- base_com_M52 %>% filter(compte4 %in% c(2042)) %>% group_by(SIREN) %>% summarise(sub_204_prive=sum(reelDEB)/1000) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)

resultat_m57  <- base_com_M57 %>% filter(compte3 %in% c(204)) %>% group_by(SIREN) %>% summarise(subvention_204=sum(reelDEB)/1000) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)
resultat_m57  <- base_com_M57 %>% filter(compte5 %in% c(20411)) %>% group_by(SIREN) %>% summarise(sub_204_etat=sum(reelDEB)/1000) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)
resultat_m57  <- base_com_M57 %>% filter(compte5 %in% c(20414) | compte6 %in% c(204151)) %>% group_by(SIREN) %>% summarise(sub_204_bc=sum(reelDEB)/1000) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)
resultat_m57  <- base_com_M57 %>% filter(compte4 %in% c(2041) & !compte5 %in% c(20411,20414) & !compte6 %in% c(204151)) %>% group_by(SIREN) %>% summarise(sub_204_autres=sum(reelDEB)/1000) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)
resultat_m57  <- base_com_M57 %>% filter(compte4 %in% c(2042)) %>% group_by(SIREN) %>% summarise(sub_204_prive=sum(reelDEB)/1000) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)

#Travaux comptes de tiers
resultat <- base_com_BP %>% filter(compte3 %in% c(454,455,458)) %>% group_by(SIREN) %>% summarise(compte_tier_d = sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)

#compte 26
resultat <- base_com_BP %>% filter(compte2 %in% c(26) & !compte3 %in% c(269)) %>% group_by(SIREN) %>% summarise(compte_26_d = sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
#compte 27
resultat <- base_com_BP %>% filter(compte2 %in% c(27) & !compte3 %in% c(279) & !compte4 %in% c(2768)) %>% group_by(SIREN) %>% summarise(compte_27_d = sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)

# Recette d'investissements
rec_invest <- base_com_BP %>% filter((compte2 %in% c(13,20,21,26,27) | compte3 %in% c(102,231,232,454,455,458)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(rec_invest_1=sum(reelCRE)/1000) %>% full_join(rec_invest,by="SIREN") %>% replace(is.na(.),0)
rec_invest <- base_com_BP %>% filter(compte3 %in% c(103,775))  %>% group_by(SIREN) %>% summarise(rec_invest_2=sum(reelCRE-reelDEB)/1000) %>%  full_join(rec_invest,by="SIREN") %>% replace(is.na(.),0)
rec_invest <- rec_invest %>% mutate(rec_invest=rec_invest_1+rec_invest_2)
resultat <- rec_invest %>% select(SIREN,rec_invest) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
#compte 13
resultat <- base_com_BP %>% filter(compte2 %in% c(13) & !compte3 == 139 | compte3 %in% c(102) & !compte5 %in% c(10222,10229) & !compte4 == 1027) %>% group_by(SIREN) %>% summarise(compte13_10 = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
## Sub recu dotations
resultat  <- base_com_BP %>% filter(compte4 %in% c(1311,1321,1381)) %>% group_by(SIREN) %>% summarise(sub_recu_etat=sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte4 %in% c(1312,1322,1382)) %>% group_by(SIREN) %>% summarise(sub_recu_reg=sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte4 %in% c(1313,1323,1383)) %>% group_by(SIREN) %>% summarise(sub_recu_dep=sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte4 %in% c(1314,1324,1384)) %>% group_by(SIREN) %>% summarise(sub_recu_com=sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte4 %in% c(1315,1325,1385)) %>% group_by(SIREN) %>% summarise(sub_recu_gfp=sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte4 %in% c(1317,1327,1387)) %>% group_by(SIREN) %>% summarise(sub_recu_bcfs=sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
# taxe amenage
resultat_m52  <- base_com_M52 %>% filter(compte4 %in% c(1334,1344)) %>% group_by(SIREN) %>% summarise(taxe_amenag=sum(reelCRE)/1000) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)
resultat_m57  <- base_com_M57 %>% filter(compte5 %in% c(10221,10226,13363,13463)) %>% group_by(SIREN) %>% summarise(taxe_amenag=sum(reelCRE)/1000) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)
## amade police
resultat_m52  <- base_com_M52 %>% filter(compte4 %in% c(1335,1345)) %>% group_by(SIREN) %>% summarise(amende=sum(reelCRE)/1000) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)
resultat_m57  <- base_com_M57 %>% filter(compte4 %in% c(1335,1345) | compte5 %in% c(13362,13462)) %>% group_by(SIREN) %>% summarise(amende=sum(reelCRE)/1000) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)
#FCTVA
resultat <- base_com_BP %>% filter(compte5 %in% c(10222)) %>% group_by(SIREN) %>% summarise(FCTVA = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
#plan de relance FCTVA
resultat <- base_com_BP %>% filter(compte3 %in% c(103)) %>% group_by(SIREN) %>% summarise(planFCTVA_r = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
resultat <- base_com_BP %>% filter(compte3 %in% c(103)) %>% group_by(SIREN) %>% summarise(planFCTVA_d = sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)

# Autres daotations hors FCTVA
resultat <- base_com_BP %>% filter(compte3 %in% c(102) & !compte5 %in% c(10222,10229) & !compte4 == 1027) %>% group_by(SIREN) %>% summarise(autres_dot = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
# Produits de cessions
resultat <- base_com_BP %>% filter(compte3 %in% c(775)) %>% group_by(SIREN) %>% summarise(cessions = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
#compte de tier
resultat <- base_com_BP %>% filter(compte3 %in% c(454,455,458)) %>% group_by(SIREN) %>% summarise(compte_tier_r = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
#compte 26
resultat <- base_com_BP %>% filter(compte2 %in% c(26) & !compte3 %in% c(269)) %>% group_by(SIREN) %>% summarise(compte_26_r = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
#compte 27
resultat <- base_com_BP %>% filter(compte2 %in% c(27) & !compte3 %in% c(279) & !compte4 %in% c(2768)) %>% group_by(SIREN) %>% summarise(compte_27_r = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)

resultat_i <- rbind(resultat_m52,resultat_m57)
resultat <- full_join(resultat,resultat_i,by="SIREN")
resultat_all <- resultat

# GAD
GAD  <- base_com_BP %>%
  filter(compte3 %in% c(166) | compte5 %in% c(16449)) %>%
  select(SIREN,reelCRE,reelDEB)
GAD$min  <- GAD %>% select(reelCRE,reelDEB) %>% apply(MARGIN = 1,FUN = min)
resultat_all  <- GAD %>% group_by(SIREN) %>% summarise(gad=(sum(min))/1000) %>% select(SIREN,gad) %>% full_join(resultat_all,by="SIREN" )  %>% replace(is.na(.),0)
## Emprunt
resultat_all  <- base_com_BP %>% filter(compte2 %in% c(16) & !compte3 %in% c(169) & !compte4 %in% c(1645,1688)) %>% group_by(SIREN) %>% summarise(emprunt=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all <- resultat_all %>% mutate(emprunt=emprunt-gad)
## Remboursement
resultat_all  <- base_com_BP %>% filter(compte2 %in% c(16) & !compte3 %in% c(169) & !compte4 %in% c(1645,1688)) %>% group_by(SIREN) %>% summarise(remboursement=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all <- resultat_all %>% mutate(remboursement=remboursement-gad)
# Dette
resultat_all  <- base_com_BP %>% filter(compte2 %in% c(16) & !compte3 %in% c(169) & !compte4 %in% c(1688)) %>% group_by(SIREN) %>% summarise(dette=sum(SC)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# Depot au tresor
resultat_all  <- base_com_BP %>% filter(compte3 %in% c(515)) %>% group_by(SIREN) %>% summarise(depot_tre=sum(SD)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# travaux en r?gie
resultat_all  <- base_com_BP %>% filter(compte2 %in% c(72)) %>% group_by(SIREN) %>% summarise(tra_regie=sum(ordreCRE-ordreDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
#charge a repar
resultat_all  <- base_com_BP %>% filter(compte3 %in% c(481)) %>% group_by(SIREN) %>% summarise(ch_repart=sum(ordreDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)


# Les variables pour les BA 2016 ====


# Charges de fonctionnement
resultat_all  <- base_com_BA %>% filter(compte1==6 & !compte2 %in% c(68) & !compte3 %in% c(675,676)) %>% group_by(SIREN) %>% summarise(dep_fonc_ba=sum(reelDEB-reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte2 %in% c(68)) %>% group_by(SIREN) %>% summarise(dot_prov_ba=sum(reelDEB-reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)

# Produits de fonctionnement
resultat_all  <- base_com_BA %>% filter(compte1==7 & !compte2 %in% c(78) & !compte3 %in% c(775,776,777))  %>% group_by(SIREN) %>% summarise(rec_fonc_ba=sum(reelCRE-reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte2 %in% c(78))  %>% group_by(SIREN) %>% summarise(repri_prov_ba=sum(reelCRE-reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)

# dep et rec d'invest
dep_invest  <- base_com_BA %>% select(SIREN) %>% distinct()
rec_invest <- base_com_BA %>% select(SIREN) %>% distinct()
# depense d'investissement
dep_invest  <- base_com_BA %>% filter((compte2 %in% c(13,20,21,23,26,27) | compte3 %in% c(102,454,455,458,481)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(dep_invest_1=sum(reelDEB)/1000) %>% full_join(dep_invest,by="SIREN") %>% replace(is.na(.),0)
dep_invest <- base_com_BA %>% filter(compte3 %in% c(237,238))  %>% group_by(SIREN) %>% summarise(dep_invest_2=sum(reelCRE)/1000) %>%  full_join(dep_invest,by="SIREN") %>% replace(is.na(.),0)
resultat_all <- dep_invest %>% mutate(dep_invest_ba=dep_invest_1-dep_invest_2) %>% select(SIREN,dep_invest_ba) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# les depenses d'?quip
dep_invest  <- base_com_BA %>% filter(compte2 %in% c(20,21,23) & !compte3 %in% c(204))  %>% group_by(SIREN) %>% summarise(dep_invest_3=sum(reelDEB)/1000) %>%  full_join(dep_invest,by="SIREN")  %>% replace(is.na(.),0)
resultat_all  <- dep_invest %>% mutate(dep_equip_ba=dep_invest_3-dep_invest_2,dep_equip_brut_ba=dep_invest_3) %>% select(SIREN,dep_equip_ba,dep_equip_brut_ba) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
#terrain
resultat_all  <- base_com_BA %>% filter(compte3 %in% c(211,212) | compte4 %in% c(2171,2172,2312)) %>% group_by(SIREN) %>% summarise(terrains_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
#construction
resultat_all  <- base_com_BA %>% filter(compte3 %in% c(213,214) | compte4 %in% c(2173,2174,2313,2314)) %>% group_by(SIREN) %>% summarise(constructions_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# reseau
resultat_all  <- base_com_BA %>% filter(compte3 %in% c(215) | compte4 %in% c(2175,2315)) %>% group_by(SIREN) %>% summarise(reseaux_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
#bien meuble
resultat_all  <- base_com_BA %>% filter(compte3 %in% c(218) | compte4 %in% c(2178,2318)) %>% group_by(SIREN) %>% summarise(bien_meuble_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)

# sub d equip
resultat_all  <- base_com_BA %>% filter(compte3 %in% c(204)) %>% group_by(SIREN) %>% summarise(subvention_204_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte5 %in% c(20412)) %>% group_by(SIREN) %>% summarise(sub_204_reg_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte5 %in% c(20413)) %>% group_by(SIREN) %>% summarise(sub_204_dep_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte5 %in% c(20414)) %>% group_by(SIREN) %>% summarise(sub_204_com_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte6 %in% c(204151)) %>% group_by(SIREN) %>% summarise(sub_204_gfp_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# compte de tier d
resultat_all <- base_com_BA %>% filter(compte3 %in% c(454,456,458)) %>% group_by(SIREN) %>% summarise(compte_tier_d_ba = sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)
# compte 26
resultat_all <- base_com_BA %>% filter(compte2 %in% c(26) & !compte3 %in% c(269)) %>% group_by(SIREN) %>% summarise(compte_26_ba_d = sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)
#0 compte 27
resultat_all <- base_com_BA %>% filter(compte2 %in% c(27) & !compte3 %in% c(279) & !compte4 %in% c(2768)) %>% group_by(SIREN) %>% summarise(compte_27_ba_d = sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)



# recette d'investissement
rec_invest  <- base_com_BA %>% filter((compte2 %in% c(13,20,21,26,27) | compte3 %in% c(102,231,232,454,455,458)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(rec_invest_1=sum(reelCRE)/1000) %>% full_join(rec_invest,by="SIREN") %>% replace(is.na(.),0)
rec_invest  <- base_com_BA %>% filter(compte3 %in% c(103,775))  %>% group_by(SIREN) %>% summarise(rec_invest_2=sum(reelCRE-reelDEB)/1000) %>%  full_join(rec_invest,by="SIREN") %>% replace(is.na(.),0)
resultat_all <- rec_invest %>% mutate(rec_invest_ba=rec_invest_1+rec_invest_2) %>% select(SIREN,rec_invest_ba) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# sub recu des ba
resultat_all <- base_com_BA %>% filter(compte2 %in% c(13) & !compte3 == 139 | compte3 %in% c(102) & !compte5 %in% c(10222,10229) & !compte4 == 1027) %>% group_by(SIREN) %>% summarise(compte13_10_ba = sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)
## Sub recu dotations
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1311,1321,1381)) %>% group_by(SIREN) %>% summarise(sub_recu_etat_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1312,1322,1382)) %>% group_by(SIREN) %>% summarise(sub_recu_reg_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1313,1323,1383)) %>% group_by(SIREN) %>% summarise(sub_recu_dep_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1314,1324,1384)) %>% group_by(SIREN) %>% summarise(sub_recu_com_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1315,1325,1385)) %>% group_by(SIREN) %>% summarise(sub_recu_gfp_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1317,1327,1387)) %>% group_by(SIREN) %>% summarise(sub_recu_bcfs_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
##DETR
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1331,1341)) %>% group_by(SIREN) %>% summarise(detr_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1331,1341)) %>% group_by(SIREN) %>% summarise(detr_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)

# taxe amenage
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1333,1343) | compte5 %in% c(10223,10226)) %>% group_by(SIREN) %>% summarise(taxe_amenag_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)

#amende
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1332,1342)) %>% group_by(SIREN) %>% summarise(amende_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
#FCTVA
resultat_all <- base_com_BA %>% filter(compte5 %in% c(10222)) %>% group_by(SIREN) %>% summarise(FCTVA_ba = sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)
#plan de relance FCTVA
resultat_all <- base_com_BA %>% filter(compte3 %in% c(103)) %>% group_by(SIREN) %>% summarise(planFCTVA_r_ba = sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)
resultat_all <- base_com_BA %>% filter(compte3 %in% c(103)) %>% group_by(SIREN) %>% summarise(planFCTVA_d_ba = sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)

#prod cession
resultat_all <- base_com_BA %>% filter(compte3 %in% c(775)) %>% group_by(SIREN) %>% summarise(cessions_ba = sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)

#compre de tier r
resultat_all <- base_com_BA %>% filter(compte3 %in% c(454,456,458)) %>% group_by(SIREN) %>% summarise(compte_tier_r_ba = sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)
#comtpe 26
resultat_all <- base_com_BA %>% filter(compte2 %in% c(26) & !compte3 %in% c(269)) %>% group_by(SIREN) %>% summarise(compte_26_ba_r = sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)
#0 compte 27
resultat_all <- base_com_BA %>% filter(compte2 %in% c(27) & !compte3 %in% c(279) & !compte4 %in% c(2768)) %>% group_by(SIREN) %>% summarise(compte_27_ba_r = sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)



# GAD
GAD  <- base_com_BA %>%
  filter(compte3 %in% c(166) | compte5 %in% c(16449)) %>%
  select(SIREN,reelCRE,reelDEB)
GAD$min  <- GAD %>% select(reelCRE,reelDEB) %>% apply(MARGIN = 1,FUN = min)
resultat_all  <- GAD %>% group_by(SIREN) %>% summarise(gad_ba=(sum(min))/1000) %>% select(SIREN,gad_ba) %>% full_join(resultat_all,by="SIREN" )  %>% replace(is.na(.),0)
## Emprunt
resultat_all  <- base_com_BA %>% filter(compte2 %in% c(16) & !compte3 %in% c(169) & !compte4 %in% c(1645,1688)) %>% group_by(SIREN) %>% summarise(emprunt_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all <- resultat_all %>% mutate(emprunt_ba=emprunt_ba-gad_ba)
## Remboursement
resultat_all  <- base_com_BA %>% filter(compte2 %in% c(16) & !compte3 %in% c(169) & !compte4 %in% c(1645,1688)) %>% group_by(SIREN) %>% summarise(remboursement_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all <- resultat_all %>% mutate(remboursement_ba=remboursement_ba-gad_ba)
# Dette
resultat_all  <- base_com_BA %>% filter(compte2 %in% c(16) & !compte3 %in% c(169) & !compte4 %in% c(1688)) %>% group_by(SIREN) %>% summarise(dette_ba=sum(SC)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# Depot au tresor
resultat_all  <- base_com_BA %>% filter(compte3 %in% c(515)) %>% group_by(SIREN) %>% summarise(depot_tre_ba=sum(SD)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# travaux en r?gie
resultat_all  <- base_com_BA %>% filter(compte2 %in% c(72)) %>% group_by(SIREN) %>% summarise(tra_regie_ba=sum(ordreCRE-ordreDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
#charge a repar
resultat_all  <- base_com_BA %>% filter(compte3 %in% c(481)) %>% group_by(SIREN) %>% summarise(ch_repart_ba=sum(ordreDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)


#Flux crois?e
fc <- resultat_all %>% select(SIREN)
fc <- base_com_BA %>% filter(NOMEN %in% c("M52","M57") & compte4 %in% c(1313,1323,1383))  %>% group_by(SIREN) %>% summarise(fc_sub_m5257=sum(reelCRE)/1000) %>%  full_join(fc,by="SIREN") %>% replace(is.na(.),0)
fc <- base_com_BA %>% filter(!NOMEN %in% c("M52","M57") & compte4 %in% c(1313))  %>% group_by(SIREN) %>% summarise(fc_sub_m4=sum(reelCRE)/1000) %>%  full_join(fc,by="SIREN") %>% replace(is.na(.),0)


fc  <- base_com_BP %>% filter(compte4 == 6521 | compte5 == 67441 | compte6 %in% c(657363,657364)) %>% group_by(SIREN) %>% summarise(df_bp15 =sum(reelDEB)/1000) %>% full_join(fc,by="SIREN") %>% replace(is.na(.),0)
fc  <- base_com_BA %>% filter(!NOMEN %in% c("M14","M14A") & (compte4 %in% c(6215,6287) | compte3 == 672 )) %>% group_by(SIREN) %>% summarise(df_ba_m415 =sum(reelDEB)/1000) %>% full_join(fc,by="SIREN") %>% replace(is.na(.),0)
fc  <- base_com_BA %>% filter(NOMEN %in% c("M14") & (compte4 %in% c(6215,6522) | compte5 == 62871 | compte6 == 661133)) %>% group_by(SIREN) %>% summarise(df_ba_m1415 =sum(reelDEB)/1000) %>% full_join(fc,by="SIREN") %>% replace(is.na(.),0)
fc  <- base_com_BA %>% filter(NOMEN %in% c("M14A") & (compte4 %in% c(6287,6522))) %>% group_by(SIREN) %>% summarise(df_ba_m14A15 =sum(reelDEB)/1000)  %>% full_join(fc,by="SIREN") %>% replace(is.na(.),0)
fc  <- base_com_BA %>% filter(NOMEN %in% c("M14") & (compte5 %in% c(70871))) %>% group_by(SIREN) %>% summarise(df_ba_m14r15 =sum(reelCRE)/1000) %>%  full_join(fc,by="SIREN") %>% replace(is.na(.),0)

resultat_all  <- fc %>% mutate(fc_f = df_bp15 + df_ba_m415 +df_ba_m14A15  + df_ba_m1415 + df_ba_m14r15, fc_i = fc_sub_m5257 + fc_sub_m4 ) %>% select(SIREN,fc_f,fc_i) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)

resultat_16 <- resultat_all

remove(resultat_m52,resultat_i,resultat_m57,base_com_M57,base_com_M52,base_com_BP,base_com,resultat_all,dep_invest,dep_invest_57,dep_invest_52,rec_invest)
gc()


## Les base_invests 2015 ====

##Preparation de la base
Balancedep_2016 <- read.csv2("C:/Users/cgirard-adc/Desktop/Base comptables/balance_dep/BalanceDEPT_2015.csv")

base_com  <- Balancedep_2016
gc()

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
base_com_BP  <- base_com[base_com$BUDGET == "BP",]
base_com_BA  <- base_com[base_com$BUDGET == "BA",]

base_com_M52  <- base_com_BP[base_com_BP$NOMEN == "M52",]
base_com_M57  <- base_com_BP[base_com_BP$NOMEN == "M57",]

resultat  <- base_com_BP %>% select(SIREN,LBUDG,NDEPT) %>% distinct()
resultat_m57  <- base_com_M57 %>% select(SIREN,LBUDG) %>% distinct()
resultat_m52  <- base_com_M52 %>% select(SIREN,LBUDG) %>% distinct()

dep_invest_57  <- base_com_M57 %>% select(SIREN) %>% distinct()
dep_invest_52  <- base_com_M52 %>% select(SIREN) %>% distinct()

rec_invest  <- base_com_BP %>% select(SIREN) %>% distinct()


# Charges de fonctionnement
resultat  <- base_com_BP %>% filter(compte1==6 & !compte2 %in% c(68) & !compte3 %in% c(675,676)) %>% group_by(SIREN) %>% summarise(dep_fonc=sum(reelDEB-reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte2 %in% c(68)) %>% group_by(SIREN) %>% summarise(dot_prov=sum(reelDEB-reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)

# Produits de fonctionnement
resultat  <- base_com_BP %>% filter(compte1==7 & !compte2 %in% c(78) & !compte3 %in% c(775,776,777))  %>% group_by(SIREN) %>% summarise(rec_fonc=sum(reelCRE-reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte2 %in% c(78))  %>% group_by(SIREN) %>% summarise(repri_prov=sum(reelCRE-reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)

# Depenses d'investissement
dep_invest_52  <- base_com_M52 %>% filter((compte2 %in% c(13,20,21,23,26,27) | compte3 %in% c(102,454,455,458,481)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(dep_invest_1=sum(reelDEB)/1000) %>% full_join(dep_invest_52,by="SIREN") %>% replace(is.na(.),0)
dep_invest_52 <- base_com_M52 %>% filter(compte3 %in% c(236,237,238))  %>% group_by(SIREN) %>% summarise(dep_invest_2=sum(reelCRE)/1000) %>%  full_join(dep_invest_52,by="SIREN") %>% replace(is.na(.),0)
resultat_m52 <- dep_invest_52 %>% mutate(dep_invest=dep_invest_1-dep_invest_2) %>% select(SIREN,dep_invest) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)
dep_invest_57  <- base_com_M57 %>% filter((compte2 %in% c(13,20,21,23,26,27) | compte3 %in% c(102,454,455,458,481)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(dep_invest_1=sum(reelDEB)/1000) %>% full_join(dep_invest_57,by="SIREN") %>% replace(is.na(.),0)
dep_invest_57 <- base_com_M57 %>% filter(compte3 %in% c(237,238))  %>% group_by(SIREN) %>% summarise(dep_invest_2=sum(reelCRE)/1000) %>%  full_join(dep_invest_57,by="SIREN") %>% replace(is.na(.),0)
resultat_m57 <- dep_invest_57 %>% mutate(dep_invest=dep_invest_1-dep_invest_2) %>% select(SIREN,dep_invest) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)

## compte 21
resultat <- base_com_BP %>% filter(compte2 == 21) %>% group_by(SIREN) %>% summarise(compte21 = sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
#compte 23
resultat <- base_com_BP %>% filter(compte2 == 23) %>% group_by(SIREN) %>% summarise(compte23 = sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
##  Depenses d'équipements
dep_invest_52  <- base_com_M52 %>% filter(compte2 %in% c(20,21,23) & !compte3 %in% c(204))  %>% group_by(SIREN) %>% summarise(dep_invest_3=sum(reelDEB)/1000) %>%  full_join(dep_invest_52,by="SIREN")  %>% replace(is.na(.),0)
resultat_m52  <- dep_invest_52 %>% mutate(dep_equip=dep_invest_3-dep_invest_2) %>% select(SIREN,dep_equip) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)
dep_invest_57  <- base_com_M57 %>% filter(compte2 %in% c(20,21,23) & !compte3 %in% c(204))  %>% group_by(SIREN) %>% summarise(dep_invest_3=sum(reelDEB)/1000) %>%  full_join(dep_invest_57,by="SIREN")  %>% replace(is.na(.),0)
resultat_m57  <- dep_invest_57 %>% mutate(dep_equip=dep_invest_3-dep_invest_2) %>% select(SIREN,dep_equip) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)
resultat_m52  <- base_com_M52 %>% filter(compte2 %in% c(20,21,23) & !compte3 %in% c(204))  %>% group_by(SIREN) %>% summarise(dep_equip_brut=sum(reelDEB)/1000) %>%  full_join(resultat_m52,by="SIREN")  %>% replace(is.na(.),0)
resultat_m57  <- base_com_M57 %>% filter(compte2 %in% c(20,21,23) & !compte3 %in% c(204))  %>% group_by(SIREN) %>% summarise(dep_equip_brut=sum(reelDEB)/1000) %>%  full_join(resultat_m57,by="SIREN")  %>% replace(is.na(.),0)


#terrain
resultat  <- base_com_BP %>% filter(compte3 %in% c(211,212) | compte4 %in% c(2171,2172,2312)) %>% group_by(SIREN) %>% summarise(terrains=sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
#construction
resultat  <- base_com_BP %>% filter(compte3 %in% c(213,214) | compte4 %in% c(2173,2174,2313,2314)) %>% group_by(SIREN) %>% summarise(constructions=sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
# reseau
resultat  <- base_com_BP %>% filter(compte3 %in% c(215) | compte4 %in% c(2175,2315)) %>% group_by(SIREN) %>% summarise(reseaux=sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
#bien meuble
resultat  <- base_com_BP %>% filter(compte3 %in% c(218) | compte4 %in% c(2178,2318)) %>% group_by(SIREN) %>% summarise(bien_meuble=sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)

##  Subventions
resultat_m52  <- base_com_M52 %>% filter(compte3 %in% c(204)) %>% group_by(SIREN) %>% summarise(subvention_204=sum(reelDEB)/1000) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)
resultat_m52  <- base_com_M52 %>% filter(compte5 %in% c(20411)) %>% group_by(SIREN) %>% summarise(sub_204_etat=sum(reelDEB)/1000) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)
resultat_m52  <- base_com_M52 %>% filter(compte5 %in% c(20414)) %>% group_by(SIREN) %>% summarise(sub_204_bc=sum(reelDEB)/1000) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)
resultat_m52  <- base_com_M52 %>% filter(compte4 %in% c(2041) & !compte5 %in% c(20411,20414)) %>% group_by(SIREN) %>% summarise(sub_204_autres=sum(reelDEB)/1000) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)
resultat_m52  <- base_com_M52 %>% filter(compte4 %in% c(2042)) %>% group_by(SIREN) %>% summarise(sub_204_prive=sum(reelDEB)/1000) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)

resultat_m57  <- base_com_M57 %>% filter(compte3 %in% c(204)) %>% group_by(SIREN) %>% summarise(subvention_204=sum(reelDEB)/1000) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)
resultat_m57  <- base_com_M57 %>% filter(compte5 %in% c(20411)) %>% group_by(SIREN) %>% summarise(sub_204_etat=sum(reelDEB)/1000) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)
resultat_m57  <- base_com_M57 %>% filter(compte5 %in% c(20414) | compte6 %in% c(204151)) %>% group_by(SIREN) %>% summarise(sub_204_bc=sum(reelDEB)/1000) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)
resultat_m57  <- base_com_M57 %>% filter(compte4 %in% c(2041) & !compte5 %in% c(20411,20414) & !compte6 %in% c(204151)) %>% group_by(SIREN) %>% summarise(sub_204_autres=sum(reelDEB)/1000) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)
resultat_m57  <- base_com_M57 %>% filter(compte4 %in% c(2042)) %>% group_by(SIREN) %>% summarise(sub_204_prive=sum(reelDEB)/1000) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)
#Travaux comptes de tiers
resultat <- base_com_BP %>% filter(compte3 %in% c(454,455,458)) %>% group_by(SIREN) %>% summarise(compte_tier_d = sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)

#compte 26
resultat <- base_com_BP %>% filter(compte2 %in% c(26) & !compte3 %in% c(269)) %>% group_by(SIREN) %>% summarise(compte_26_d = sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
#compte 27
resultat <- base_com_BP %>% filter(compte2 %in% c(27) & !compte3 %in% c(279) & !compte4 %in% c(2768)) %>% group_by(SIREN) %>% summarise(compte_27_d = sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)

# Recette d'investissements
rec_invest <- base_com_BP %>% filter((compte2 %in% c(13,20,21,26,27) | compte3 %in% c(102,231,232,454,455,458)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(rec_invest_1=sum(reelCRE)/1000) %>% full_join(rec_invest,by="SIREN") %>% replace(is.na(.),0)
rec_invest <- base_com_BP %>% filter(compte3 %in% c(103,775))  %>% group_by(SIREN) %>% summarise(rec_invest_2=sum(reelCRE-reelDEB)/1000) %>%  full_join(rec_invest,by="SIREN") %>% replace(is.na(.),0)
rec_invest <- rec_invest %>% mutate(rec_invest=rec_invest_1+rec_invest_2)
resultat <- rec_invest %>% select(SIREN,rec_invest) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
#compte 13
resultat <- base_com_BP %>% filter(compte2 %in% c(13) & !compte3 == 139 | compte3 %in% c(102) & !compte5 %in% c(10222,10229) & !compte4 == 1027) %>% group_by(SIREN) %>% summarise(compte13_10 = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
## Sub recu dotations
resultat  <- base_com_BP %>% filter(compte4 %in% c(1311,1321,1381)) %>% group_by(SIREN) %>% summarise(sub_recu_etat=sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte4 %in% c(1312,1322,1382)) %>% group_by(SIREN) %>% summarise(sub_recu_reg=sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte4 %in% c(1313,1323,1383)) %>% group_by(SIREN) %>% summarise(sub_recu_dep=sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte4 %in% c(1314,1324,1384)) %>% group_by(SIREN) %>% summarise(sub_recu_com=sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte4 %in% c(1315,1325,1385)) %>% group_by(SIREN) %>% summarise(sub_recu_gfp=sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte4 %in% c(1317,1327,1387)) %>% group_by(SIREN) %>% summarise(sub_recu_bcfs=sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
# taxe amenage
resultat_m52  <- base_com_M52 %>% filter(compte4 %in% c(1334,1344)) %>% group_by(SIREN) %>% summarise(taxe_amenag=sum(reelCRE)/1000) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)
resultat_m57  <- base_com_M57 %>% filter(compte5 %in% c(10221,10226,13363,13463)) %>% group_by(SIREN) %>% summarise(taxe_amenag=sum(reelCRE)/1000) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)
## amade police
resultat_m52  <- base_com_M52 %>% filter(compte4 %in% c(1335,1345)) %>% group_by(SIREN) %>% summarise(amende=sum(reelCRE)/1000) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)
resultat_m57  <- base_com_M57 %>% filter(compte4 %in% c(1335,1345) | compte5 %in% c(13362,13462)) %>% group_by(SIREN) %>% summarise(amende=sum(reelCRE)/1000) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)
#FCTVA
resultat <- base_com_BP %>% filter(compte5 %in% c(10222)) %>% group_by(SIREN) %>% summarise(FCTVA = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
#plan de relance FCTVA
resultat <- base_com_BP %>% filter(compte3 %in% c(103)) %>% group_by(SIREN) %>% summarise(planFCTVA_r = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
resultat <- base_com_BP %>% filter(compte3 %in% c(103)) %>% group_by(SIREN) %>% summarise(planFCTVA_d = sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)

# Autres daotations hors FCTVA
resultat <- base_com_BP %>% filter(compte3 %in% c(102) & !compte5 %in% c(10222,10229) & !compte4 == 1027) %>% group_by(SIREN) %>% summarise(autres_dot = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
# Produits de cessions
resultat <- base_com_BP %>% filter(compte3 %in% c(775)) %>% group_by(SIREN) %>% summarise(cessions = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
#compte de tier
resultat <- base_com_BP %>% filter(compte3 %in% c(454,455,458)) %>% group_by(SIREN) %>% summarise(compte_tier_r = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
#compte 26
resultat <- base_com_BP %>% filter(compte2 %in% c(26) & !compte3 %in% c(269)) %>% group_by(SIREN) %>% summarise(compte_26_r = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
#compte 27
resultat <- base_com_BP %>% filter(compte2 %in% c(27) & !compte3 %in% c(279) & !compte4 %in% c(2768)) %>% group_by(SIREN) %>% summarise(compte_27_r = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)

resultat_i <- rbind(resultat_m52,resultat_m57)
resultat <- full_join(resultat,resultat_i,by="SIREN")
resultat_all <- resultat

# GAD
GAD  <- base_com_BP %>%
  filter(compte3 %in% c(166) | compte5 %in% c(16449)) %>%
  select(SIREN,reelCRE,reelDEB)
GAD$min  <- GAD %>% select(reelCRE,reelDEB) %>% apply(MARGIN = 1,FUN = min)
resultat_all  <- GAD %>% group_by(SIREN) %>% summarise(gad=(sum(min))/1000) %>% select(SIREN,gad) %>% full_join(resultat_all,by="SIREN" )  %>% replace(is.na(.),0)
## Emprunt
resultat_all  <- base_com_BP %>% filter(compte2 %in% c(16) & !compte3 %in% c(169) & !compte4 %in% c(1645,1688)) %>% group_by(SIREN) %>% summarise(emprunt=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all <- resultat_all %>% mutate(emprunt=emprunt-gad)
## Remboursement
resultat_all  <- base_com_BP %>% filter(compte2 %in% c(16) & !compte3 %in% c(169) & !compte4 %in% c(1645,1688)) %>% group_by(SIREN) %>% summarise(remboursement=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all <- resultat_all %>% mutate(remboursement=remboursement-gad)
# Dette
resultat_all  <- base_com_BP %>% filter(compte2 %in% c(16) & !compte3 %in% c(169) & !compte4 %in% c(1688)) %>% group_by(SIREN) %>% summarise(dette=sum(SC)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# Depot au tresor
resultat_all  <- base_com_BP %>% filter(compte3 %in% c(515)) %>% group_by(SIREN) %>% summarise(depot_tre=sum(SD)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# travaux en r?gie
resultat_all  <- base_com_BP %>% filter(compte2 %in% c(72)) %>% group_by(SIREN) %>% summarise(tra_regie=sum(ordreCRE-ordreDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
#charge a repar
resultat_all  <- base_com_BP %>% filter(compte3 %in% c(481)) %>% group_by(SIREN) %>% summarise(ch_repart=sum(ordreDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)


# Les variables pour les BA 2015 ====


# Charges de fonctionnement
resultat_all  <- base_com_BA %>% filter(compte1==6 & !compte2 %in% c(68) & !compte3 %in% c(675,676)) %>% group_by(SIREN) %>% summarise(dep_fonc_ba=sum(reelDEB-reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte2 %in% c(68)) %>% group_by(SIREN) %>% summarise(dot_prov_ba=sum(reelDEB-reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)

# Produits de fonctionnement
resultat_all  <- base_com_BA %>% filter(compte1==7 & !compte2 %in% c(78) & !compte3 %in% c(775,776,777))  %>% group_by(SIREN) %>% summarise(rec_fonc_ba=sum(reelCRE-reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte2 %in% c(78))  %>% group_by(SIREN) %>% summarise(repri_prov_ba=sum(reelCRE-reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)

# dep et rec d'invest
dep_invest  <- base_com_BA %>% select(SIREN) %>% distinct()
rec_invest <- base_com_BA %>% select(SIREN) %>% distinct()
# depense d'investissement
dep_invest  <- base_com_BA %>% filter((compte2 %in% c(13,20,21,23,26,27) | compte3 %in% c(102,454,455,458,481)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(dep_invest_1=sum(reelDEB)/1000) %>% full_join(dep_invest,by="SIREN") %>% replace(is.na(.),0)
dep_invest <- base_com_BA %>% filter(compte3 %in% c(237,238))  %>% group_by(SIREN) %>% summarise(dep_invest_2=sum(reelCRE)/1000) %>%  full_join(dep_invest,by="SIREN") %>% replace(is.na(.),0)
resultat_all <- dep_invest %>% mutate(dep_invest_ba=dep_invest_1-dep_invest_2) %>% select(SIREN,dep_invest_ba) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# les depenses d'?quip
dep_invest  <- base_com_BA %>% filter(compte2 %in% c(20,21,23) & !compte3 %in% c(204))  %>% group_by(SIREN) %>% summarise(dep_invest_3=sum(reelDEB)/1000) %>%  full_join(dep_invest,by="SIREN")  %>% replace(is.na(.),0)
resultat_all  <- dep_invest %>% mutate(dep_equip_ba=dep_invest_3-dep_invest_2,dep_equip_brut_ba=dep_invest_3) %>% select(SIREN,dep_equip_ba,dep_equip_brut_ba) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
#terrain
resultat_all  <- base_com_BA %>% filter(compte3 %in% c(211,212) | compte4 %in% c(2171,2172,2312)) %>% group_by(SIREN) %>% summarise(terrains_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
#construction
resultat_all  <- base_com_BA %>% filter(compte3 %in% c(213,214) | compte4 %in% c(2173,2174,2313,2314)) %>% group_by(SIREN) %>% summarise(constructions_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# reseau
resultat_all  <- base_com_BA %>% filter(compte3 %in% c(215) | compte4 %in% c(2175,2315)) %>% group_by(SIREN) %>% summarise(reseaux_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
#bien meuble
resultat_all  <- base_com_BA %>% filter(compte3 %in% c(218) | compte4 %in% c(2178,2318)) %>% group_by(SIREN) %>% summarise(bien_meuble_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)

# sub d equip
resultat_all  <- base_com_BA %>% filter(compte3 %in% c(204)) %>% group_by(SIREN) %>% summarise(subvention_204_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte5 %in% c(20412)) %>% group_by(SIREN) %>% summarise(sub_204_reg_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte5 %in% c(20413)) %>% group_by(SIREN) %>% summarise(sub_204_dep_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte5 %in% c(20414)) %>% group_by(SIREN) %>% summarise(sub_204_com_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte6 %in% c(204151)) %>% group_by(SIREN) %>% summarise(sub_204_gfp_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# compte de tier d
resultat_all <- base_com_BA %>% filter(compte3 %in% c(454,456,458)) %>% group_by(SIREN) %>% summarise(compte_tier_d_ba = sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)
# compte 26
resultat_all <- base_com_BA %>% filter(compte2 %in% c(26) & !compte3 %in% c(269)) %>% group_by(SIREN) %>% summarise(compte_26_ba_d = sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)
#0 compte 27
resultat_all <- base_com_BA %>% filter(compte2 %in% c(27) & !compte3 %in% c(279) & !compte4 %in% c(2768)) %>% group_by(SIREN) %>% summarise(compte_27_ba_d = sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)



# recette d'investissement
rec_invest  <- base_com_BA %>% filter((compte2 %in% c(13,20,21,26,27) | compte3 %in% c(102,231,232,454,455,458)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(rec_invest_1=sum(reelCRE)/1000) %>% full_join(rec_invest,by="SIREN") %>% replace(is.na(.),0)
rec_invest  <- base_com_BA %>% filter(compte3 %in% c(103,775))  %>% group_by(SIREN) %>% summarise(rec_invest_2=sum(reelCRE-reelDEB)/1000) %>%  full_join(rec_invest,by="SIREN") %>% replace(is.na(.),0)
resultat_all <- rec_invest %>% mutate(rec_invest_ba=rec_invest_1+rec_invest_2) %>% select(SIREN,rec_invest_ba) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# sub recu des ba
resultat_all <- base_com_BA %>% filter(compte2 %in% c(13) & !compte3 == 139 | compte3 %in% c(102) & !compte5 %in% c(10222,10229) & !compte4 == 1027) %>% group_by(SIREN) %>% summarise(compte13_10_ba = sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)
## Sub recu dotations
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1311,1321,1381)) %>% group_by(SIREN) %>% summarise(sub_recu_etat_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1312,1322,1382)) %>% group_by(SIREN) %>% summarise(sub_recu_reg_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1313,1323,1383)) %>% group_by(SIREN) %>% summarise(sub_recu_dep_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1314,1324,1384)) %>% group_by(SIREN) %>% summarise(sub_recu_com_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1315,1325,1385)) %>% group_by(SIREN) %>% summarise(sub_recu_gfp_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1317,1327,1387)) %>% group_by(SIREN) %>% summarise(sub_recu_bcfs_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
##DETR
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1331,1341)) %>% group_by(SIREN) %>% summarise(detr_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1331,1341)) %>% group_by(SIREN) %>% summarise(detr_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)

# taxe amenage
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1333,1343) | compte5 %in% c(10223,10226)) %>% group_by(SIREN) %>% summarise(taxe_amenag_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)

#amende
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1332,1342)) %>% group_by(SIREN) %>% summarise(amende_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
#FCTVA
resultat_all <- base_com_BA %>% filter(compte5 %in% c(10222)) %>% group_by(SIREN) %>% summarise(FCTVA_ba = sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)
#plan de relance FCTVA
resultat_all <- base_com_BA %>% filter(compte3 %in% c(103)) %>% group_by(SIREN) %>% summarise(planFCTVA_r_ba = sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)
resultat_all <- base_com_BA %>% filter(compte3 %in% c(103)) %>% group_by(SIREN) %>% summarise(planFCTVA_d_ba = sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)

#prod cession
resultat_all <- base_com_BA %>% filter(compte3 %in% c(775)) %>% group_by(SIREN) %>% summarise(cessions_ba = sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)

#compre de tier r
resultat_all <- base_com_BA %>% filter(compte3 %in% c(454,456,458)) %>% group_by(SIREN) %>% summarise(compte_tier_r_ba = sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)
#comtpe 26
resultat_all <- base_com_BA %>% filter(compte2 %in% c(26) & !compte3 %in% c(269)) %>% group_by(SIREN) %>% summarise(compte_26_ba_r = sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)
#0 compte 27
resultat_all <- base_com_BA %>% filter(compte2 %in% c(27) & !compte3 %in% c(279) & !compte4 %in% c(2768)) %>% group_by(SIREN) %>% summarise(compte_27_ba_r = sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)



# GAD
GAD  <- base_com_BA %>%
  filter(compte3 %in% c(166) | compte5 %in% c(16449)) %>%
  select(SIREN,reelCRE,reelDEB)
GAD$min  <- GAD %>% select(reelCRE,reelDEB) %>% apply(MARGIN = 1,FUN = min)
resultat_all  <- GAD %>% group_by(SIREN) %>% summarise(gad_ba=(sum(min))/1000) %>% select(SIREN,gad_ba) %>% full_join(resultat_all,by="SIREN" )  %>% replace(is.na(.),0)
## Emprunt
resultat_all  <- base_com_BA %>% filter(compte2 %in% c(16) & !compte3 %in% c(169) & !compte4 %in% c(1645,1688)) %>% group_by(SIREN) %>% summarise(emprunt_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all <- resultat_all %>% mutate(emprunt_ba=emprunt_ba-gad_ba)
## Remboursement
resultat_all  <- base_com_BA %>% filter(compte2 %in% c(16) & !compte3 %in% c(169) & !compte4 %in% c(1645,1688)) %>% group_by(SIREN) %>% summarise(remboursement_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all <- resultat_all %>% mutate(remboursement_ba=remboursement_ba-gad_ba)
# Dette
resultat_all  <- base_com_BA %>% filter(compte2 %in% c(16) & !compte3 %in% c(169) & !compte4 %in% c(1688)) %>% group_by(SIREN) %>% summarise(dette_ba=sum(SC)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# Depot au tresor
resultat_all  <- base_com_BA %>% filter(compte3 %in% c(515)) %>% group_by(SIREN) %>% summarise(depot_tre_ba=sum(SD)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# travaux en r?gie
resultat_all  <- base_com_BA %>% filter(compte2 %in% c(72)) %>% group_by(SIREN) %>% summarise(tra_regie_ba=sum(ordreCRE-ordreDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
#charge a repar
resultat_all  <- base_com_BA %>% filter(compte3 %in% c(481)) %>% group_by(SIREN) %>% summarise(ch_repart_ba=sum(ordreDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)


#Flux crois?e
fc <- resultat_all %>% select(SIREN)
fc <- base_com_BA %>% filter(NOMEN %in% c("M52","M57") & compte4 %in% c(1313,1323,1383))  %>% group_by(SIREN) %>% summarise(fc_sub_m5257=sum(reelCRE)/1000) %>%  full_join(fc,by="SIREN") %>% replace(is.na(.),0)
fc <- base_com_BA %>% filter(!NOMEN %in% c("M52","M57") & compte4 %in% c(1313))  %>% group_by(SIREN) %>% summarise(fc_sub_m4=sum(reelCRE)/1000) %>%  full_join(fc,by="SIREN") %>% replace(is.na(.),0)


fc  <- base_com_BP %>% filter(compte4 == 6521 | compte5 == 67441 | compte6 %in% c(657363,657364)) %>% group_by(SIREN) %>% summarise(df_bp15 =sum(reelDEB)/1000) %>% full_join(fc,by="SIREN") %>% replace(is.na(.),0)
fc  <- base_com_BA %>% filter(!NOMEN %in% c("M14","M14A") & (compte4 %in% c(6215,6287) | compte3 == 672 )) %>% group_by(SIREN) %>% summarise(df_ba_m415 =sum(reelDEB)/1000) %>% full_join(fc,by="SIREN") %>% replace(is.na(.),0)
fc  <- base_com_BA %>% filter(NOMEN %in% c("M14") & (compte4 %in% c(6215,6522) | compte5 == 62871 | compte6 == 661133)) %>% group_by(SIREN) %>% summarise(df_ba_m1415 =sum(reelDEB)/1000) %>% full_join(fc,by="SIREN") %>% replace(is.na(.),0)
fc  <- base_com_BA %>% filter(NOMEN %in% c("M14A") & (compte4 %in% c(6287,6522))) %>% group_by(SIREN) %>% summarise(df_ba_m14A15 =sum(reelDEB)/1000)  %>% full_join(fc,by="SIREN") %>% replace(is.na(.),0)
fc  <- base_com_BA %>% filter(NOMEN %in% c("M14") & (compte5 %in% c(70871))) %>% group_by(SIREN) %>% summarise(df_ba_m14r15 =sum(reelCRE)/1000) %>%  full_join(fc,by="SIREN") %>% replace(is.na(.),0)

resultat_all  <- fc %>% mutate(fc_f = df_bp15 + df_ba_m415 +df_ba_m14A15  + df_ba_m1415 + df_ba_m14r15, fc_i = fc_sub_m5257 + fc_sub_m4 ) %>% select(SIREN,fc_f,fc_i) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)

resultat_15 <- resultat_all

remove(resultat_m52,resultat_i,resultat_m57,base_com_M57,base_com_M52,base_com_BP,base_com,resultat_all,dep_invest,dep_invest_57,dep_invest_52,rec_invest)
gc()


## Les base_invests 2014 ====

##Preparation de la base
Balancedep_2016 <- read.csv2("C:/Users/cgirard-adc/Desktop/Base comptables/balance_dep/BalanceDEPT_2014.csv")

base_com  <- Balancedep_2016
gc()

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
base_com_BP  <- base_com[base_com$BUDGET == "BP",]
base_com_BA  <- base_com[base_com$BUDGET == "BA",]

base_com_M52  <- base_com_BP[base_com_BP$NOMEN == "M52",]
base_com_M57  <- base_com_BP[base_com_BP$NOMEN == "M57",]

resultat  <- base_com_BP %>% select(SIREN,LBUDG,NDEPT) %>% distinct()
resultat_m57  <- base_com_M57 %>% select(SIREN,LBUDG) %>% distinct()
resultat_m52  <- base_com_M52 %>% select(SIREN,LBUDG) %>% distinct()

dep_invest_57  <- base_com_M57 %>% select(SIREN) %>% distinct()
dep_invest_52  <- base_com_M52 %>% select(SIREN) %>% distinct()

rec_invest  <- base_com_BP %>% select(SIREN) %>% distinct()


# Charges de fonctionnement
resultat  <- base_com_BP %>% filter(compte1==6 & !compte2 %in% c(68) & !compte3 %in% c(675,676)) %>% group_by(SIREN) %>% summarise(dep_fonc=sum(reelDEB-reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte2 %in% c(68)) %>% group_by(SIREN) %>% summarise(dot_prov=sum(reelDEB-reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)

# Produits de fonctionnement
resultat  <- base_com_BP %>% filter(compte1==7 & !compte2 %in% c(78) & !compte3 %in% c(775,776,777))  %>% group_by(SIREN) %>% summarise(rec_fonc=sum(reelCRE-reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte2 %in% c(78))  %>% group_by(SIREN) %>% summarise(repri_prov=sum(reelCRE-reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)

# Depenses d'investissement
dep_invest_52  <- base_com_M52 %>% filter((compte2 %in% c(13,20,21,23,26,27) | compte3 %in% c(102,454,455,458,481)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(dep_invest_1=sum(reelDEB)/1000) %>% full_join(dep_invest_52,by="SIREN") %>% replace(is.na(.),0)
dep_invest_52 <- base_com_M52 %>% filter(compte3 %in% c(236,237,238))  %>% group_by(SIREN) %>% summarise(dep_invest_2=sum(reelCRE)/1000) %>%  full_join(dep_invest_52,by="SIREN") %>% replace(is.na(.),0)
resultat_m52 <- dep_invest_52 %>% mutate(dep_invest=dep_invest_1-dep_invest_2) %>% select(SIREN,dep_invest) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)
dep_invest_57  <- base_com_M57 %>% filter((compte2 %in% c(13,20,21,23,26,27) | compte3 %in% c(102,454,455,458,481)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(dep_invest_1=sum(reelDEB)/1000) %>% full_join(dep_invest_57,by="SIREN") %>% replace(is.na(.),0)
dep_invest_57 <- base_com_M57 %>% filter(compte3 %in% c(237,238))  %>% group_by(SIREN) %>% summarise(dep_invest_2=sum(reelCRE)/1000) %>%  full_join(dep_invest_57,by="SIREN") %>% replace(is.na(.),0)
resultat_m57 <- dep_invest_57 %>% mutate(dep_invest=dep_invest_1-dep_invest_2) %>% select(SIREN,dep_invest) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)

## compte 21
resultat <- base_com_BP %>% filter(compte2 == 21) %>% group_by(SIREN) %>% summarise(compte21 = sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
#compte 23
resultat <- base_com_BP %>% filter(compte2 == 23) %>% group_by(SIREN) %>% summarise(compte23 = sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
##  Depenses d'équipements
dep_invest_52  <- base_com_M52 %>% filter(compte2 %in% c(20,21,23) & !compte3 %in% c(204))  %>% group_by(SIREN) %>% summarise(dep_invest_3=sum(reelDEB)/1000) %>%  full_join(dep_invest_52,by="SIREN")  %>% replace(is.na(.),0)
resultat_m52  <- dep_invest_52 %>% mutate(dep_equip=dep_invest_3-dep_invest_2) %>% select(SIREN,dep_equip) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)
dep_invest_57  <- base_com_M57 %>% filter(compte2 %in% c(20,21,23) & !compte3 %in% c(204))  %>% group_by(SIREN) %>% summarise(dep_invest_3=sum(reelDEB)/1000) %>%  full_join(dep_invest_57,by="SIREN")  %>% replace(is.na(.),0)
resultat_m57  <- dep_invest_57 %>% mutate(dep_equip=dep_invest_3-dep_invest_2) %>% select(SIREN,dep_equip) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)
resultat_m52  <- base_com_M52 %>% filter(compte2 %in% c(20,21,23) & !compte3 %in% c(204))  %>% group_by(SIREN) %>% summarise(dep_equip_brut=sum(reelDEB)/1000) %>%  full_join(resultat_m52,by="SIREN")  %>% replace(is.na(.),0)
resultat_m57  <- base_com_M57 %>% filter(compte2 %in% c(20,21,23) & !compte3 %in% c(204))  %>% group_by(SIREN) %>% summarise(dep_equip_brut=sum(reelDEB)/1000) %>%  full_join(resultat_m57,by="SIREN")  %>% replace(is.na(.),0)


#terrain
resultat  <- base_com_BP %>% filter(compte3 %in% c(211,212) | compte4 %in% c(2171,2172,2312)) %>% group_by(SIREN) %>% summarise(terrains=sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
#construction
resultat  <- base_com_BP %>% filter(compte3 %in% c(213,214) | compte4 %in% c(2173,2174,2313,2314)) %>% group_by(SIREN) %>% summarise(constructions=sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
# reseau
resultat  <- base_com_BP %>% filter(compte3 %in% c(215) | compte4 %in% c(2175,2315)) %>% group_by(SIREN) %>% summarise(reseaux=sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
#bien meuble
resultat  <- base_com_BP %>% filter(compte3 %in% c(218) | compte4 %in% c(2178,2318)) %>% group_by(SIREN) %>% summarise(bien_meuble=sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)

##  Subventions
resultat_m52  <- base_com_M52 %>% filter(compte3 %in% c(204)) %>% group_by(SIREN) %>% summarise(subvention_204=sum(reelDEB)/1000) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)
resultat_m52  <- base_com_M52 %>% filter(compte5 %in% c(20411)) %>% group_by(SIREN) %>% summarise(sub_204_etat=sum(reelDEB)/1000) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)
resultat_m52  <- base_com_M52 %>% filter(compte5 %in% c(20414)) %>% group_by(SIREN) %>% summarise(sub_204_bc=sum(reelDEB)/1000) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)
resultat_m52  <- base_com_M52 %>% filter(compte4 %in% c(2041) & !compte5 %in% c(20411,20414)) %>% group_by(SIREN) %>% summarise(sub_204_autres=sum(reelDEB)/1000) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)
resultat_m52  <- base_com_M52 %>% filter(compte4 %in% c(2042)) %>% group_by(SIREN) %>% summarise(sub_204_prive=sum(reelDEB)/1000) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)

resultat_m57  <- base_com_M57 %>% filter(compte3 %in% c(204)) %>% group_by(SIREN) %>% summarise(subvention_204=sum(reelDEB)/1000) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)
resultat_m57  <- base_com_M57 %>% filter(compte5 %in% c(20411)) %>% group_by(SIREN) %>% summarise(sub_204_etat=sum(reelDEB)/1000) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)
resultat_m57  <- base_com_M57 %>% filter(compte5 %in% c(20414) | compte6 %in% c(204151)) %>% group_by(SIREN) %>% summarise(sub_204_bc=sum(reelDEB)/1000) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)
resultat_m57  <- base_com_M57 %>% filter(compte4 %in% c(2041) & !compte5 %in% c(20411,20414) & !compte6 %in% c(204151)) %>% group_by(SIREN) %>% summarise(sub_204_autres=sum(reelDEB)/1000) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)
resultat_m57  <- base_com_M57 %>% filter(compte4 %in% c(2042)) %>% group_by(SIREN) %>% summarise(sub_204_prive=sum(reelDEB)/1000) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)
#Travaux comptes de tiers
resultat <- base_com_BP %>% filter(compte3 %in% c(454,455,458)) %>% group_by(SIREN) %>% summarise(compte_tier_d = sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)

#compte 26
resultat <- base_com_BP %>% filter(compte2 %in% c(26) & !compte3 %in% c(269)) %>% group_by(SIREN) %>% summarise(compte_26_d = sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
#compte 27
resultat <- base_com_BP %>% filter(compte2 %in% c(27) & !compte3 %in% c(279) & !compte4 %in% c(2768)) %>% group_by(SIREN) %>% summarise(compte_27_d = sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)

# Recette d'investissements
rec_invest <- base_com_BP %>% filter((compte2 %in% c(13,20,21,26,27) | compte3 %in% c(102,231,232,454,455,458)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(rec_invest_1=sum(reelCRE)/1000) %>% full_join(rec_invest,by="SIREN") %>% replace(is.na(.),0)
rec_invest <- base_com_BP %>% filter(compte3 %in% c(103,775))  %>% group_by(SIREN) %>% summarise(rec_invest_2=sum(reelCRE-reelDEB)/1000) %>%  full_join(rec_invest,by="SIREN") %>% replace(is.na(.),0)
rec_invest <- rec_invest %>% mutate(rec_invest=rec_invest_1+rec_invest_2)
resultat <- rec_invest %>% select(SIREN,rec_invest) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
#compte 13
resultat <- base_com_BP %>% filter(compte2 %in% c(13) & !compte3 == 139 | compte3 %in% c(102) & !compte5 %in% c(10222,10229) & !compte4 == 1027) %>% group_by(SIREN) %>% summarise(compte13_10 = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
## Sub recu dotations
resultat  <- base_com_BP %>% filter(compte4 %in% c(1311,1321,1381)) %>% group_by(SIREN) %>% summarise(sub_recu_etat=sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte4 %in% c(1312,1322,1382)) %>% group_by(SIREN) %>% summarise(sub_recu_reg=sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte4 %in% c(1313,1323,1383)) %>% group_by(SIREN) %>% summarise(sub_recu_dep=sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte4 %in% c(1314,1324,1384)) %>% group_by(SIREN) %>% summarise(sub_recu_com=sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte4 %in% c(1315,1325,1385)) %>% group_by(SIREN) %>% summarise(sub_recu_gfp=sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte4 %in% c(1317,1327,1387)) %>% group_by(SIREN) %>% summarise(sub_recu_bcfs=sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
# taxe amenage
resultat_m52  <- base_com_M52 %>% filter(compte4 %in% c(1334,1344)) %>% group_by(SIREN) %>% summarise(taxe_amenag=sum(reelCRE)/1000) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)
resultat_m57  <- base_com_M57 %>% filter(compte5 %in% c(10221,10226,13363,13463)) %>% group_by(SIREN) %>% summarise(taxe_amenag=sum(reelCRE)/1000) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)
## amade police
resultat_m52  <- base_com_M52 %>% filter(compte4 %in% c(1335,1345)) %>% group_by(SIREN) %>% summarise(amende=sum(reelCRE)/1000) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)
resultat_m57  <- base_com_M57 %>% filter(compte4 %in% c(1335,1345) | compte5 %in% c(13362,13462)) %>% group_by(SIREN) %>% summarise(amende=sum(reelCRE)/1000) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)
#FCTVA
resultat <- base_com_BP %>% filter(compte5 %in% c(10222)) %>% group_by(SIREN) %>% summarise(FCTVA = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
#plan de relance FCTVA
resultat <- base_com_BP %>% filter(compte3 %in% c(103)) %>% group_by(SIREN) %>% summarise(planFCTVA_r = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
resultat <- base_com_BP %>% filter(compte3 %in% c(103)) %>% group_by(SIREN) %>% summarise(planFCTVA_d = sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)

# Autres daotations hors FCTVA
resultat <- base_com_BP %>% filter(compte3 %in% c(102) & !compte5 %in% c(10222,10229) & !compte4 == 1027) %>% group_by(SIREN) %>% summarise(autres_dot = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
# Produits de cessions
resultat <- base_com_BP %>% filter(compte3 %in% c(775)) %>% group_by(SIREN) %>% summarise(cessions = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
#compte de tier
resultat <- base_com_BP %>% filter(compte3 %in% c(454,455,458)) %>% group_by(SIREN) %>% summarise(compte_tier_r = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
#compte 26
resultat <- base_com_BP %>% filter(compte2 %in% c(26) & !compte3 %in% c(269)) %>% group_by(SIREN) %>% summarise(compte_26_r = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
#compte 27
resultat <- base_com_BP %>% filter(compte2 %in% c(27) & !compte3 %in% c(279) & !compte4 %in% c(2768)) %>% group_by(SIREN) %>% summarise(compte_27_r = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)

resultat_i <- rbind(resultat_m52,resultat_m57)
resultat <- full_join(resultat,resultat_i,by="SIREN")
resultat_all <- resultat

# GAD
GAD  <- base_com_BP %>%
  filter(compte3 %in% c(166) | compte5 %in% c(16449)) %>%
  select(SIREN,reelCRE,reelDEB)
GAD$min  <- GAD %>% select(reelCRE,reelDEB) %>% apply(MARGIN = 1,FUN = min)
resultat_all  <- GAD %>% group_by(SIREN) %>% summarise(gad=(sum(min))/1000) %>% select(SIREN,gad) %>% full_join(resultat_all,by="SIREN" )  %>% replace(is.na(.),0)
## Emprunt
resultat_all  <- base_com_BP %>% filter(compte2 %in% c(16) & !compte3 %in% c(169) & !compte4 %in% c(1645,1688)) %>% group_by(SIREN) %>% summarise(emprunt=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all <- resultat_all %>% mutate(emprunt=emprunt-gad)
## Remboursement
resultat_all  <- base_com_BP %>% filter(compte2 %in% c(16) & !compte3 %in% c(169) & !compte4 %in% c(1645,1688)) %>% group_by(SIREN) %>% summarise(remboursement=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all <- resultat_all %>% mutate(remboursement=remboursement-gad)
# Dette
resultat_all  <- base_com_BP %>% filter(compte2 %in% c(16) & !compte3 %in% c(169) & !compte4 %in% c(1688)) %>% group_by(SIREN) %>% summarise(dette=sum(SC)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# Depot au tresor
resultat_all  <- base_com_BP %>% filter(compte3 %in% c(515)) %>% group_by(SIREN) %>% summarise(depot_tre=sum(SD)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# travaux en r?gie
resultat_all  <- base_com_BP %>% filter(compte2 %in% c(72)) %>% group_by(SIREN) %>% summarise(tra_regie=sum(ordreCRE-ordreDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
#charge a repar
resultat_all  <- base_com_BP %>% filter(compte3 %in% c(481)) %>% group_by(SIREN) %>% summarise(ch_repart=sum(ordreDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)


# Les variables pour les BA 2014 ====


# Charges de fonctionnement
resultat_all  <- base_com_BA %>% filter(compte1==6 & !compte2 %in% c(68) & !compte3 %in% c(675,676)) %>% group_by(SIREN) %>% summarise(dep_fonc_ba=sum(reelDEB-reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte2 %in% c(68)) %>% group_by(SIREN) %>% summarise(dot_prov_ba=sum(reelDEB-reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)

# Produits de fonctionnement
resultat_all  <- base_com_BA %>% filter(compte1==7 & !compte2 %in% c(78) & !compte3 %in% c(775,776,777))  %>% group_by(SIREN) %>% summarise(rec_fonc_ba=sum(reelCRE-reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte2 %in% c(78))  %>% group_by(SIREN) %>% summarise(repri_prov_ba=sum(reelCRE-reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)

# dep et rec d'invest
dep_invest  <- base_com_BA %>% select(SIREN) %>% distinct()
rec_invest <- base_com_BA %>% select(SIREN) %>% distinct()
# depense d'investissement
dep_invest  <- base_com_BA %>% filter((compte2 %in% c(13,20,21,23,26,27) | compte3 %in% c(102,454,455,458,481)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(dep_invest_1=sum(reelDEB)/1000) %>% full_join(dep_invest,by="SIREN") %>% replace(is.na(.),0)
dep_invest <- base_com_BA %>% filter(compte3 %in% c(237,238))  %>% group_by(SIREN) %>% summarise(dep_invest_2=sum(reelCRE)/1000) %>%  full_join(dep_invest,by="SIREN") %>% replace(is.na(.),0)
resultat_all <- dep_invest %>% mutate(dep_invest_ba=dep_invest_1-dep_invest_2) %>% select(SIREN,dep_invest_ba) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# les depenses d'?quip
dep_invest  <- base_com_BA %>% filter(compte2 %in% c(20,21,23) & !compte3 %in% c(204))  %>% group_by(SIREN) %>% summarise(dep_invest_3=sum(reelDEB)/1000) %>%  full_join(dep_invest,by="SIREN")  %>% replace(is.na(.),0)
resultat_all  <- dep_invest %>% mutate(dep_equip_ba=dep_invest_3-dep_invest_2,dep_equip_brut_ba=dep_invest_3) %>% select(SIREN,dep_equip_ba,dep_equip_brut_ba) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
#terrain
resultat_all  <- base_com_BA %>% filter(compte3 %in% c(211,212) | compte4 %in% c(2171,2172,2312)) %>% group_by(SIREN) %>% summarise(terrains_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
#construction
resultat_all  <- base_com_BA %>% filter(compte3 %in% c(213,214) | compte4 %in% c(2173,2174,2313,2314)) %>% group_by(SIREN) %>% summarise(constructions_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# reseau
resultat_all  <- base_com_BA %>% filter(compte3 %in% c(215) | compte4 %in% c(2175,2315)) %>% group_by(SIREN) %>% summarise(reseaux_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
#bien meuble
resultat_all  <- base_com_BA %>% filter(compte3 %in% c(218) | compte4 %in% c(2178,2318)) %>% group_by(SIREN) %>% summarise(bien_meuble_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)

# sub d equip
resultat_all  <- base_com_BA %>% filter(compte3 %in% c(204)) %>% group_by(SIREN) %>% summarise(subvention_204_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte5 %in% c(20412)) %>% group_by(SIREN) %>% summarise(sub_204_reg_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte5 %in% c(20413)) %>% group_by(SIREN) %>% summarise(sub_204_dep_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte5 %in% c(20414)) %>% group_by(SIREN) %>% summarise(sub_204_com_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte6 %in% c(204151)) %>% group_by(SIREN) %>% summarise(sub_204_gfp_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# compte de tier d
resultat_all <- base_com_BA %>% filter(compte3 %in% c(454,456,458)) %>% group_by(SIREN) %>% summarise(compte_tier_d_ba = sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)
# compte 26
resultat_all <- base_com_BA %>% filter(compte2 %in% c(26) & !compte3 %in% c(269)) %>% group_by(SIREN) %>% summarise(compte_26_ba_d = sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)
#0 compte 27
resultat_all <- base_com_BA %>% filter(compte2 %in% c(27) & !compte3 %in% c(279) & !compte4 %in% c(2768)) %>% group_by(SIREN) %>% summarise(compte_27_ba_d = sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)



# recette d'investissement
rec_invest  <- base_com_BA %>% filter((compte2 %in% c(13,20,21,26,27) | compte3 %in% c(102,231,232,454,455,458)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(rec_invest_1=sum(reelCRE)/1000) %>% full_join(rec_invest,by="SIREN") %>% replace(is.na(.),0)
rec_invest  <- base_com_BA %>% filter(compte3 %in% c(103,775))  %>% group_by(SIREN) %>% summarise(rec_invest_2=sum(reelCRE-reelDEB)/1000) %>%  full_join(rec_invest,by="SIREN") %>% replace(is.na(.),0)
resultat_all <- rec_invest %>% mutate(rec_invest_ba=rec_invest_1+rec_invest_2) %>% select(SIREN,rec_invest_ba) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# sub recu des ba
resultat_all <- base_com_BA %>% filter(compte2 %in% c(13) & !compte3 == 139 | compte3 %in% c(102) & !compte5 %in% c(10222,10229) & !compte4 == 1027) %>% group_by(SIREN) %>% summarise(compte13_10_ba = sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)
## Sub recu dotations
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1311,1321,1381)) %>% group_by(SIREN) %>% summarise(sub_recu_etat_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1312,1322,1382)) %>% group_by(SIREN) %>% summarise(sub_recu_reg_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1313,1323,1383)) %>% group_by(SIREN) %>% summarise(sub_recu_dep_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1314,1324,1384)) %>% group_by(SIREN) %>% summarise(sub_recu_com_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1315,1325,1385)) %>% group_by(SIREN) %>% summarise(sub_recu_gfp_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1317,1327,1387)) %>% group_by(SIREN) %>% summarise(sub_recu_bcfs_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
##DETR
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1331,1341)) %>% group_by(SIREN) %>% summarise(detr_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1331,1341)) %>% group_by(SIREN) %>% summarise(detr_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)

# taxe amenage
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1333,1343) | compte5 %in% c(10223,10226)) %>% group_by(SIREN) %>% summarise(taxe_amenag_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)

#amende
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1332,1342)) %>% group_by(SIREN) %>% summarise(amende_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
#FCTVA
resultat_all <- base_com_BA %>% filter(compte5 %in% c(10222)) %>% group_by(SIREN) %>% summarise(FCTVA_ba = sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)
#plan de relance FCTVA
resultat_all <- base_com_BA %>% filter(compte3 %in% c(103)) %>% group_by(SIREN) %>% summarise(planFCTVA_r_ba = sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)
resultat_all <- base_com_BA %>% filter(compte3 %in% c(103)) %>% group_by(SIREN) %>% summarise(planFCTVA_d_ba = sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)

#prod cession
resultat_all <- base_com_BA %>% filter(compte3 %in% c(775)) %>% group_by(SIREN) %>% summarise(cessions_ba = sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)

#compre de tier r
resultat_all <- base_com_BA %>% filter(compte3 %in% c(454,456,458)) %>% group_by(SIREN) %>% summarise(compte_tier_r_ba = sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)
#comtpe 26
resultat_all <- base_com_BA %>% filter(compte2 %in% c(26) & !compte3 %in% c(269)) %>% group_by(SIREN) %>% summarise(compte_26_ba_r = sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)
#0 compte 27
resultat_all <- base_com_BA %>% filter(compte2 %in% c(27) & !compte3 %in% c(279) & !compte4 %in% c(2768)) %>% group_by(SIREN) %>% summarise(compte_27_ba_r = sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)



# GAD
GAD  <- base_com_BA %>%
  filter(compte3 %in% c(166) | compte5 %in% c(16449)) %>%
  select(SIREN,reelCRE,reelDEB)
GAD$min  <- GAD %>% select(reelCRE,reelDEB) %>% apply(MARGIN = 1,FUN = min)
resultat_all  <- GAD %>% group_by(SIREN) %>% summarise(gad_ba=(sum(min))/1000) %>% select(SIREN,gad_ba) %>% full_join(resultat_all,by="SIREN" )  %>% replace(is.na(.),0)
## Emprunt
resultat_all  <- base_com_BA %>% filter(compte2 %in% c(16) & !compte3 %in% c(169) & !compte4 %in% c(1645,1688)) %>% group_by(SIREN) %>% summarise(emprunt_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all <- resultat_all %>% mutate(emprunt_ba=emprunt_ba-gad_ba)
## Remboursement
resultat_all  <- base_com_BA %>% filter(compte2 %in% c(16) & !compte3 %in% c(169) & !compte4 %in% c(1645,1688)) %>% group_by(SIREN) %>% summarise(remboursement_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all <- resultat_all %>% mutate(remboursement_ba=remboursement_ba-gad_ba)
# Dette
resultat_all  <- base_com_BA %>% filter(compte2 %in% c(16) & !compte3 %in% c(169) & !compte4 %in% c(1688)) %>% group_by(SIREN) %>% summarise(dette_ba=sum(SC)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# Depot au tresor
resultat_all  <- base_com_BA %>% filter(compte3 %in% c(515)) %>% group_by(SIREN) %>% summarise(depot_tre_ba=sum(SD)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# travaux en r?gie
resultat_all  <- base_com_BA %>% filter(compte2 %in% c(72)) %>% group_by(SIREN) %>% summarise(tra_regie_ba=sum(ordreCRE-ordreDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
#charge a repar
resultat_all  <- base_com_BA %>% filter(compte3 %in% c(481)) %>% group_by(SIREN) %>% summarise(ch_repart_ba=sum(ordreDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)


#Flux crois?e
fc <- resultat_all %>% select(SIREN)
fc <- base_com_BA %>% filter(NOMEN %in% c("M52","M57") & compte4 %in% c(1313,1323,1383))  %>% group_by(SIREN) %>% summarise(fc_sub_m5257=sum(reelCRE)/1000) %>%  full_join(fc,by="SIREN") %>% replace(is.na(.),0)
fc <- base_com_BA %>% filter(!NOMEN %in% c("M52","M57") & compte4 %in% c(1313))  %>% group_by(SIREN) %>% summarise(fc_sub_m4=sum(reelCRE)/1000) %>%  full_join(fc,by="SIREN") %>% replace(is.na(.),0)


fc  <- base_com_BP %>% filter(compte4 == 6521 | compte5 == 67441 | compte6 %in% c(657363,657364)) %>% group_by(SIREN) %>% summarise(df_bp15 =sum(reelDEB)/1000) %>% full_join(fc,by="SIREN") %>% replace(is.na(.),0)
fc  <- base_com_BA %>% filter(!NOMEN %in% c("M14","M14A") & (compte4 %in% c(6215,6287) | compte3 == 672 )) %>% group_by(SIREN) %>% summarise(df_ba_m415 =sum(reelDEB)/1000) %>% full_join(fc,by="SIREN") %>% replace(is.na(.),0)
fc  <- base_com_BA %>% filter(NOMEN %in% c("M14") & (compte4 %in% c(6215,6522) | compte5 == 62871 | compte6 == 661133)) %>% group_by(SIREN) %>% summarise(df_ba_m1415 =sum(reelDEB)/1000) %>% full_join(fc,by="SIREN") %>% replace(is.na(.),0)
fc  <- base_com_BA %>% filter(NOMEN %in% c("M14A") & (compte4 %in% c(6287,6522))) %>% group_by(SIREN) %>% summarise(df_ba_m14A15 =sum(reelDEB)/1000)  %>% full_join(fc,by="SIREN") %>% replace(is.na(.),0)
fc  <- base_com_BA %>% filter(NOMEN %in% c("M14") & (compte5 %in% c(70871))) %>% group_by(SIREN) %>% summarise(df_ba_m14r15 =sum(reelCRE)/1000) %>%  full_join(fc,by="SIREN") %>% replace(is.na(.),0)

resultat_all  <- fc %>% mutate(fc_f = df_bp15 + df_ba_m415 +df_ba_m14A15  + df_ba_m1415 + df_ba_m14r15, fc_i = fc_sub_m5257 + fc_sub_m4 ) %>% select(SIREN,fc_f,fc_i) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)

resultat_14 <- resultat_all

remove(resultat_m52,resultat_i,resultat_m57,base_com_M57,base_com_M52,base_com_BP,base_com,resultat_all,dep_invest,dep_invest_57,dep_invest_52,rec_invest)
gc()


## Les base_invests 2013 ====

##Preparation de la base
Balancedep_2016 <- read.csv2("C:/Users/cgirard-adc/Desktop/Base comptables/balance_dep/BalanceDEPT_2013.csv")

base_com  <- Balancedep_2016
gc()

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
base_com_BP  <- base_com[base_com$BUDGET == "BP",]
base_com_BA  <- base_com[base_com$BUDGET == "BA",]

base_com_M52  <- base_com_BP[base_com_BP$NOMEN == "M52",]
base_com_M57  <- base_com_BP[base_com_BP$NOMEN == "M57",]

resultat  <- base_com_BP %>% select(SIREN,LBUDG,NDEPT) %>% distinct()
resultat_m57  <- base_com_M57 %>% select(SIREN,LBUDG) %>% distinct()
resultat_m52  <- base_com_M52 %>% select(SIREN,LBUDG) %>% distinct()

dep_invest_57  <- base_com_M57 %>% select(SIREN) %>% distinct()
dep_invest_52  <- base_com_M52 %>% select(SIREN) %>% distinct()

rec_invest  <- base_com_BP %>% select(SIREN) %>% distinct()


# Charges de fonctionnement
resultat  <- base_com_BP %>% filter(compte1==6 & !compte2 %in% c(68) & !compte3 %in% c(675,676)) %>% group_by(SIREN) %>% summarise(dep_fonc=sum(reelDEB-reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte2 %in% c(68)) %>% group_by(SIREN) %>% summarise(dot_prov=sum(reelDEB-reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)

# Produits de fonctionnement
resultat  <- base_com_BP %>% filter(compte1==7 & !compte2 %in% c(78) & !compte3 %in% c(775,776,777))  %>% group_by(SIREN) %>% summarise(rec_fonc=sum(reelCRE-reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte2 %in% c(78))  %>% group_by(SIREN) %>% summarise(repri_prov=sum(reelCRE-reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)

# Depenses d'investissement
dep_invest_52  <- base_com_M52 %>% filter((compte2 %in% c(13,20,21,23,26,27) | compte3 %in% c(102,454,455,458,481)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(dep_invest_1=sum(reelDEB)/1000) %>% full_join(dep_invest_52,by="SIREN") %>% replace(is.na(.),0)
dep_invest_52 <- base_com_M52 %>% filter(compte3 %in% c(236,237,238))  %>% group_by(SIREN) %>% summarise(dep_invest_2=sum(reelCRE)/1000) %>%  full_join(dep_invest_52,by="SIREN") %>% replace(is.na(.),0)
resultat_m52 <- dep_invest_52 %>% mutate(dep_invest=dep_invest_1-dep_invest_2) %>% select(SIREN,dep_invest) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)
dep_invest_57  <- base_com_M57 %>% filter((compte2 %in% c(13,20,21,23,26,27) | compte3 %in% c(102,454,455,458,481)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(dep_invest_1=sum(reelDEB)/1000) %>% full_join(dep_invest_57,by="SIREN") %>% replace(is.na(.),0)
dep_invest_57 <- base_com_M57 %>% filter(compte3 %in% c(237,238))  %>% group_by(SIREN) %>% summarise(dep_invest_2=sum(reelCRE)/1000) %>%  full_join(dep_invest_57,by="SIREN") %>% replace(is.na(.),0)
resultat_m57 <- dep_invest_57 %>% mutate(dep_invest=dep_invest_1-dep_invest_2) %>% select(SIREN,dep_invest) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)

## compte 21
resultat <- base_com_BP %>% filter(compte2 == 21) %>% group_by(SIREN) %>% summarise(compte21 = sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
#compte 23
resultat <- base_com_BP %>% filter(compte2 == 23) %>% group_by(SIREN) %>% summarise(compte23 = sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
##  Depenses d'équipements
dep_invest_52  <- base_com_M52 %>% filter(compte2 %in% c(20,21,23) & !compte3 %in% c(204))  %>% group_by(SIREN) %>% summarise(dep_invest_3=sum(reelDEB)/1000) %>%  full_join(dep_invest_52,by="SIREN")  %>% replace(is.na(.),0)
resultat_m52  <- dep_invest_52 %>% mutate(dep_equip=dep_invest_3-dep_invest_2) %>% select(SIREN,dep_equip) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)
dep_invest_57  <- base_com_M57 %>% filter(compte2 %in% c(20,21,23) & !compte3 %in% c(204))  %>% group_by(SIREN) %>% summarise(dep_invest_3=sum(reelDEB)/1000) %>%  full_join(dep_invest_57,by="SIREN")  %>% replace(is.na(.),0)
resultat_m57  <- dep_invest_57 %>% mutate(dep_equip=dep_invest_3-dep_invest_2) %>% select(SIREN,dep_equip) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)
resultat_m52  <- base_com_M52 %>% filter(compte2 %in% c(20,21,23) & !compte3 %in% c(204))  %>% group_by(SIREN) %>% summarise(dep_equip_brut=sum(reelDEB)/1000) %>%  full_join(resultat_m52,by="SIREN")  %>% replace(is.na(.),0)
resultat_m57  <- base_com_M57 %>% filter(compte2 %in% c(20,21,23) & !compte3 %in% c(204))  %>% group_by(SIREN) %>% summarise(dep_equip_brut=sum(reelDEB)/1000) %>%  full_join(resultat_m57,by="SIREN")  %>% replace(is.na(.),0)


#terrain
resultat  <- base_com_BP %>% filter(compte3 %in% c(211,212) | compte4 %in% c(2171,2172,2312)) %>% group_by(SIREN) %>% summarise(terrains=sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
#construction
resultat  <- base_com_BP %>% filter(compte3 %in% c(213,214) | compte4 %in% c(2173,2174,2313,2314)) %>% group_by(SIREN) %>% summarise(constructions=sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
# reseau
resultat  <- base_com_BP %>% filter(compte3 %in% c(215) | compte4 %in% c(2175,2315)) %>% group_by(SIREN) %>% summarise(reseaux=sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
#bien meuble
resultat  <- base_com_BP %>% filter(compte3 %in% c(218) | compte4 %in% c(2178,2318)) %>% group_by(SIREN) %>% summarise(bien_meuble=sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)

##  Subventions
resultat_m52  <- base_com_M52 %>% filter(compte3 %in% c(204)) %>% group_by(SIREN) %>% summarise(subvention_204=sum(reelDEB)/1000) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)
resultat_m52  <- base_com_M52 %>% filter(compte5 %in% c(20411)) %>% group_by(SIREN) %>% summarise(sub_204_etat=sum(reelDEB)/1000) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)
resultat_m52  <- base_com_M52 %>% filter(compte5 %in% c(20414)) %>% group_by(SIREN) %>% summarise(sub_204_bc=sum(reelDEB)/1000) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)
resultat_m52  <- base_com_M52 %>% filter(compte4 %in% c(2041) & !compte5 %in% c(20411,20414)) %>% group_by(SIREN) %>% summarise(sub_204_autres=sum(reelDEB)/1000) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)
resultat_m52  <- base_com_M52 %>% filter(compte4 %in% c(2042)) %>% group_by(SIREN) %>% summarise(sub_204_prive=sum(reelDEB)/1000) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)

resultat_m57  <- base_com_M57 %>% filter(compte3 %in% c(204)) %>% group_by(SIREN) %>% summarise(subvention_204=sum(reelDEB)/1000) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)
resultat_m57  <- base_com_M57 %>% filter(compte5 %in% c(20411)) %>% group_by(SIREN) %>% summarise(sub_204_etat=sum(reelDEB)/1000) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)
resultat_m57  <- base_com_M57 %>% filter(compte5 %in% c(20414) | compte6 %in% c(204151)) %>% group_by(SIREN) %>% summarise(sub_204_bc=sum(reelDEB)/1000) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)
resultat_m57  <- base_com_M57 %>% filter(compte4 %in% c(2041) & !compte5 %in% c(20411,20414) & !compte6 %in% c(204151)) %>% group_by(SIREN) %>% summarise(sub_204_autres=sum(reelDEB)/1000) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)
resultat_m57  <- base_com_M57 %>% filter(compte4 %in% c(2042)) %>% group_by(SIREN) %>% summarise(sub_204_prive=sum(reelDEB)/1000) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)
#Travaux comptes de tiers
resultat <- base_com_BP %>% filter(compte3 %in% c(454,455,458)) %>% group_by(SIREN) %>% summarise(compte_tier_d = sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)

#compte 26
resultat <- base_com_BP %>% filter(compte2 %in% c(26) & !compte3 %in% c(269)) %>% group_by(SIREN) %>% summarise(compte_26_d = sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
#compte 27
resultat <- base_com_BP %>% filter(compte2 %in% c(27) & !compte3 %in% c(279) & !compte4 %in% c(2768)) %>% group_by(SIREN) %>% summarise(compte_27_d = sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)

# Recette d'investissements
rec_invest <- base_com_BP %>% filter((compte2 %in% c(13,20,21,26,27) | compte3 %in% c(102,231,232,454,455,458)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(rec_invest_1=sum(reelCRE)/1000) %>% full_join(rec_invest,by="SIREN") %>% replace(is.na(.),0)
rec_invest <- base_com_BP %>% filter(compte3 %in% c(103,775))  %>% group_by(SIREN) %>% summarise(rec_invest_2=sum(reelCRE-reelDEB)/1000) %>%  full_join(rec_invest,by="SIREN") %>% replace(is.na(.),0)
rec_invest <- rec_invest %>% mutate(rec_invest=rec_invest_1+rec_invest_2)
resultat <- rec_invest %>% select(SIREN,rec_invest) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
#compte 13
resultat <- base_com_BP %>% filter(compte2 %in% c(13) & !compte3 == 139 | compte3 %in% c(102) & !compte5 %in% c(10222,10229) & !compte4 == 1027) %>% group_by(SIREN) %>% summarise(compte13_10 = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
## Sub recu dotations
resultat  <- base_com_BP %>% filter(compte4 %in% c(1311,1321,1381)) %>% group_by(SIREN) %>% summarise(sub_recu_etat=sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte4 %in% c(1312,1322,1382)) %>% group_by(SIREN) %>% summarise(sub_recu_reg=sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte4 %in% c(1313,1323,1383)) %>% group_by(SIREN) %>% summarise(sub_recu_dep=sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte4 %in% c(1314,1324,1384)) %>% group_by(SIREN) %>% summarise(sub_recu_com=sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte4 %in% c(1315,1325,1385)) %>% group_by(SIREN) %>% summarise(sub_recu_gfp=sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte4 %in% c(1317,1327,1387)) %>% group_by(SIREN) %>% summarise(sub_recu_bcfs=sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
# taxe amenage
resultat_m52  <- base_com_M52 %>% filter(compte4 %in% c(1334,1344)) %>% group_by(SIREN) %>% summarise(taxe_amenag=sum(reelCRE)/1000) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)
resultat_m57  <- base_com_M57 %>% filter(compte5 %in% c(10221,10226,13363,13463)) %>% group_by(SIREN) %>% summarise(taxe_amenag=sum(reelCRE)/1000) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)
## amade police
resultat_m52  <- base_com_M52 %>% filter(compte4 %in% c(1335,1345)) %>% group_by(SIREN) %>% summarise(amende=sum(reelCRE)/1000) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)
resultat_m57  <- base_com_M57 %>% filter(compte4 %in% c(1335,1345) | compte5 %in% c(13362,13462)) %>% group_by(SIREN) %>% summarise(amende=sum(reelCRE)/1000) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)
#FCTVA
resultat <- base_com_BP %>% filter(compte5 %in% c(10222)) %>% group_by(SIREN) %>% summarise(FCTVA = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
#plan de relance FCTVA
resultat <- base_com_BP %>% filter(compte3 %in% c(103)) %>% group_by(SIREN) %>% summarise(planFCTVA_r = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
resultat <- base_com_BP %>% filter(compte3 %in% c(103)) %>% group_by(SIREN) %>% summarise(planFCTVA_d = sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)

# Autres daotations hors FCTVA
resultat <- base_com_BP %>% filter(compte3 %in% c(102) & !compte5 %in% c(10222,10229) & !compte4 == 1027) %>% group_by(SIREN) %>% summarise(autres_dot = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
# Produits de cessions
resultat <- base_com_BP %>% filter(compte3 %in% c(775)) %>% group_by(SIREN) %>% summarise(cessions = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
#compte de tier
resultat <- base_com_BP %>% filter(compte3 %in% c(454,455,458)) %>% group_by(SIREN) %>% summarise(compte_tier_r = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
#compte 26
resultat <- base_com_BP %>% filter(compte2 %in% c(26) & !compte3 %in% c(269)) %>% group_by(SIREN) %>% summarise(compte_26_r = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
#compte 27
resultat <- base_com_BP %>% filter(compte2 %in% c(27) & !compte3 %in% c(279) & !compte4 %in% c(2768)) %>% group_by(SIREN) %>% summarise(compte_27_r = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)

resultat_i <- rbind(resultat_m52,resultat_m57)
resultat <- full_join(resultat,resultat_i,by="SIREN")
resultat_all <- resultat

# GAD
GAD  <- base_com_BP %>%
  filter(compte3 %in% c(166) | compte5 %in% c(16449)) %>%
  select(SIREN,reelCRE,reelDEB)
GAD$min  <- GAD %>% select(reelCRE,reelDEB) %>% apply(MARGIN = 1,FUN = min)
resultat_all  <- GAD %>% group_by(SIREN) %>% summarise(gad=(sum(min))/1000) %>% select(SIREN,gad) %>% full_join(resultat_all,by="SIREN" )  %>% replace(is.na(.),0)
## Emprunt
resultat_all  <- base_com_BP %>% filter(compte2 %in% c(16) & !compte3 %in% c(169) & !compte4 %in% c(1645,1688)) %>% group_by(SIREN) %>% summarise(emprunt=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all <- resultat_all %>% mutate(emprunt=emprunt-gad)
## Remboursement
resultat_all  <- base_com_BP %>% filter(compte2 %in% c(16) & !compte3 %in% c(169) & !compte4 %in% c(1645,1688)) %>% group_by(SIREN) %>% summarise(remboursement=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all <- resultat_all %>% mutate(remboursement=remboursement-gad)
# Dette
resultat_all  <- base_com_BP %>% filter(compte2 %in% c(16) & !compte3 %in% c(169) & !compte4 %in% c(1688)) %>% group_by(SIREN) %>% summarise(dette=sum(SC)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# Depot au tresor
resultat_all  <- base_com_BP %>% filter(compte3 %in% c(515)) %>% group_by(SIREN) %>% summarise(depot_tre=sum(SD)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# travaux en r?gie
resultat_all  <- base_com_BP %>% filter(compte2 %in% c(72)) %>% group_by(SIREN) %>% summarise(tra_regie=sum(ordreCRE-ordreDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
#charge a repar
resultat_all  <- base_com_BP %>% filter(compte3 %in% c(481)) %>% group_by(SIREN) %>% summarise(ch_repart=sum(ordreDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)


# Les variables pour les BA 2013 ====


# Charges de fonctionnement
resultat_all  <- base_com_BA %>% filter(compte1==6 & !compte2 %in% c(68) & !compte3 %in% c(675,676)) %>% group_by(SIREN) %>% summarise(dep_fonc_ba=sum(reelDEB-reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte2 %in% c(68)) %>% group_by(SIREN) %>% summarise(dot_prov_ba=sum(reelDEB-reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)

# Produits de fonctionnement
resultat_all  <- base_com_BA %>% filter(compte1==7 & !compte2 %in% c(78) & !compte3 %in% c(775,776,777))  %>% group_by(SIREN) %>% summarise(rec_fonc_ba=sum(reelCRE-reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte2 %in% c(78))  %>% group_by(SIREN) %>% summarise(repri_prov_ba=sum(reelCRE-reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)

# dep et rec d'invest
dep_invest  <- base_com_BA %>% select(SIREN) %>% distinct()
rec_invest <- base_com_BA %>% select(SIREN) %>% distinct()
# depense d'investissement
dep_invest  <- base_com_BA %>% filter((compte2 %in% c(13,20,21,23,26,27) | compte3 %in% c(102,454,455,458,481)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(dep_invest_1=sum(reelDEB)/1000) %>% full_join(dep_invest,by="SIREN") %>% replace(is.na(.),0)
dep_invest <- base_com_BA %>% filter(compte3 %in% c(237,238))  %>% group_by(SIREN) %>% summarise(dep_invest_2=sum(reelCRE)/1000) %>%  full_join(dep_invest,by="SIREN") %>% replace(is.na(.),0)
resultat_all <- dep_invest %>% mutate(dep_invest_ba=dep_invest_1-dep_invest_2) %>% select(SIREN,dep_invest_ba) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# les depenses d'?quip
dep_invest  <- base_com_BA %>% filter(compte2 %in% c(20,21,23) & !compte3 %in% c(204))  %>% group_by(SIREN) %>% summarise(dep_invest_3=sum(reelDEB)/1000) %>%  full_join(dep_invest,by="SIREN")  %>% replace(is.na(.),0)
resultat_all  <- dep_invest %>% mutate(dep_equip_ba=dep_invest_3-dep_invest_2,dep_equip_brut_ba=dep_invest_3) %>% select(SIREN,dep_equip_ba,dep_equip_brut_ba) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
#terrain
resultat_all  <- base_com_BA %>% filter(compte3 %in% c(211,212) | compte4 %in% c(2171,2172,2312)) %>% group_by(SIREN) %>% summarise(terrains_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
#construction
resultat_all  <- base_com_BA %>% filter(compte3 %in% c(213,214) | compte4 %in% c(2173,2174,2313,2314)) %>% group_by(SIREN) %>% summarise(constructions_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# reseau
resultat_all  <- base_com_BA %>% filter(compte3 %in% c(215) | compte4 %in% c(2175,2315)) %>% group_by(SIREN) %>% summarise(reseaux_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
#bien meuble
resultat_all  <- base_com_BA %>% filter(compte3 %in% c(218) | compte4 %in% c(2178,2318)) %>% group_by(SIREN) %>% summarise(bien_meuble_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)

# sub d equip
resultat_all  <- base_com_BA %>% filter(compte3 %in% c(204)) %>% group_by(SIREN) %>% summarise(subvention_204_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte5 %in% c(20412)) %>% group_by(SIREN) %>% summarise(sub_204_reg_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte5 %in% c(20413)) %>% group_by(SIREN) %>% summarise(sub_204_dep_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte5 %in% c(20414)) %>% group_by(SIREN) %>% summarise(sub_204_com_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte6 %in% c(204151)) %>% group_by(SIREN) %>% summarise(sub_204_gfp_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# compte de tier d
resultat_all <- base_com_BA %>% filter(compte3 %in% c(454,456,458)) %>% group_by(SIREN) %>% summarise(compte_tier_d_ba = sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)
# compte 26
resultat_all <- base_com_BA %>% filter(compte2 %in% c(26) & !compte3 %in% c(269)) %>% group_by(SIREN) %>% summarise(compte_26_ba_d = sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)
#0 compte 27
resultat_all <- base_com_BA %>% filter(compte2 %in% c(27) & !compte3 %in% c(279) & !compte4 %in% c(2768)) %>% group_by(SIREN) %>% summarise(compte_27_ba_d = sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)



# recette d'investissement
rec_invest  <- base_com_BA %>% filter((compte2 %in% c(13,20,21,26,27) | compte3 %in% c(102,231,232,454,455,458)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(rec_invest_1=sum(reelCRE)/1000) %>% full_join(rec_invest,by="SIREN") %>% replace(is.na(.),0)
rec_invest  <- base_com_BA %>% filter(compte3 %in% c(103,775))  %>% group_by(SIREN) %>% summarise(rec_invest_2=sum(reelCRE-reelDEB)/1000) %>%  full_join(rec_invest,by="SIREN") %>% replace(is.na(.),0)
resultat_all <- rec_invest %>% mutate(rec_invest_ba=rec_invest_1+rec_invest_2) %>% select(SIREN,rec_invest_ba) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# sub recu des ba
resultat_all <- base_com_BA %>% filter(compte2 %in% c(13) & !compte3 == 139 | compte3 %in% c(102) & !compte5 %in% c(10222,10229) & !compte4 == 1027) %>% group_by(SIREN) %>% summarise(compte13_10_ba = sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)
## Sub recu dotations
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1311,1321,1381)) %>% group_by(SIREN) %>% summarise(sub_recu_etat_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1312,1322,1382)) %>% group_by(SIREN) %>% summarise(sub_recu_reg_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1313,1323,1383)) %>% group_by(SIREN) %>% summarise(sub_recu_dep_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1314,1324,1384)) %>% group_by(SIREN) %>% summarise(sub_recu_com_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1315,1325,1385)) %>% group_by(SIREN) %>% summarise(sub_recu_gfp_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1317,1327,1387)) %>% group_by(SIREN) %>% summarise(sub_recu_bcfs_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
##DETR
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1331,1341)) %>% group_by(SIREN) %>% summarise(detr_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1331,1341)) %>% group_by(SIREN) %>% summarise(detr_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)

# taxe amenage
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1333,1343) | compte5 %in% c(10223,10226)) %>% group_by(SIREN) %>% summarise(taxe_amenag_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)

#amende
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1332,1342)) %>% group_by(SIREN) %>% summarise(amende_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
#FCTVA
resultat_all <- base_com_BA %>% filter(compte5 %in% c(10222)) %>% group_by(SIREN) %>% summarise(FCTVA_ba = sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)
#plan de relance FCTVA
resultat_all <- base_com_BA %>% filter(compte3 %in% c(103)) %>% group_by(SIREN) %>% summarise(planFCTVA_r_ba = sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)
resultat_all <- base_com_BA %>% filter(compte3 %in% c(103)) %>% group_by(SIREN) %>% summarise(planFCTVA_d_ba = sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)

#prod cession
resultat_all <- base_com_BA %>% filter(compte3 %in% c(775)) %>% group_by(SIREN) %>% summarise(cessions_ba = sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)

#compre de tier r
resultat_all <- base_com_BA %>% filter(compte3 %in% c(454,456,458)) %>% group_by(SIREN) %>% summarise(compte_tier_r_ba = sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)
#comtpe 26
resultat_all <- base_com_BA %>% filter(compte2 %in% c(26) & !compte3 %in% c(269)) %>% group_by(SIREN) %>% summarise(compte_26_ba_r = sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)
#0 compte 27
resultat_all <- base_com_BA %>% filter(compte2 %in% c(27) & !compte3 %in% c(279) & !compte4 %in% c(2768)) %>% group_by(SIREN) %>% summarise(compte_27_ba_r = sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)



# GAD
GAD  <- base_com_BA %>%
  filter(compte3 %in% c(166) | compte5 %in% c(16449)) %>%
  select(SIREN,reelCRE,reelDEB)
GAD$min  <- GAD %>% select(reelCRE,reelDEB) %>% apply(MARGIN = 1,FUN = min)
resultat_all  <- GAD %>% group_by(SIREN) %>% summarise(gad_ba=(sum(min))/1000) %>% select(SIREN,gad_ba) %>% full_join(resultat_all,by="SIREN" )  %>% replace(is.na(.),0)
## Emprunt
resultat_all  <- base_com_BA %>% filter(compte2 %in% c(16) & !compte3 %in% c(169) & !compte4 %in% c(1645,1688)) %>% group_by(SIREN) %>% summarise(emprunt_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all <- resultat_all %>% mutate(emprunt_ba=emprunt_ba-gad_ba)
## Remboursement
resultat_all  <- base_com_BA %>% filter(compte2 %in% c(16) & !compte3 %in% c(169) & !compte4 %in% c(1645,1688)) %>% group_by(SIREN) %>% summarise(remboursement_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all <- resultat_all %>% mutate(remboursement_ba=remboursement_ba-gad_ba)
# Dette
resultat_all  <- base_com_BA %>% filter(compte2 %in% c(16) & !compte3 %in% c(169) & !compte4 %in% c(1688)) %>% group_by(SIREN) %>% summarise(dette_ba=sum(SC)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# Depot au tresor
resultat_all  <- base_com_BA %>% filter(compte3 %in% c(515)) %>% group_by(SIREN) %>% summarise(depot_tre_ba=sum(SD)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# travaux en r?gie
resultat_all  <- base_com_BA %>% filter(compte2 %in% c(72)) %>% group_by(SIREN) %>% summarise(tra_regie_ba=sum(ordreCRE-ordreDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
#charge a repar
resultat_all  <- base_com_BA %>% filter(compte3 %in% c(481)) %>% group_by(SIREN) %>% summarise(ch_repart_ba=sum(ordreDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)


#Flux crois?e
fc <- resultat_all %>% select(SIREN)
fc <- base_com_BA %>% filter(NOMEN %in% c("M52","M57") & compte4 %in% c(1313,1323,1383))  %>% group_by(SIREN) %>% summarise(fc_sub_m5257=sum(reelCRE)/1000) %>%  full_join(fc,by="SIREN") %>% replace(is.na(.),0)
fc <- base_com_BA %>% filter(!NOMEN %in% c("M52","M57") & compte4 %in% c(1313))  %>% group_by(SIREN) %>% summarise(fc_sub_m4=sum(reelCRE)/1000) %>%  full_join(fc,by="SIREN") %>% replace(is.na(.),0)


fc  <- base_com_BP %>% filter(compte4 == 6521 | compte5 == 67441 | compte6 %in% c(657363,657364)) %>% group_by(SIREN) %>% summarise(df_bp15 =sum(reelDEB)/1000) %>% full_join(fc,by="SIREN") %>% replace(is.na(.),0)
fc  <- base_com_BA %>% filter(!NOMEN %in% c("M14","M14A") & (compte4 %in% c(6215,6287) | compte3 == 672 )) %>% group_by(SIREN) %>% summarise(df_ba_m415 =sum(reelDEB)/1000) %>% full_join(fc,by="SIREN") %>% replace(is.na(.),0)
fc  <- base_com_BA %>% filter(NOMEN %in% c("M14") & (compte4 %in% c(6215,6522) | compte5 == 62871 | compte6 == 661133)) %>% group_by(SIREN) %>% summarise(df_ba_m1415 =sum(reelDEB)/1000) %>% full_join(fc,by="SIREN") %>% replace(is.na(.),0)
fc  <- base_com_BA %>% filter(NOMEN %in% c("M14A") & (compte4 %in% c(6287,6522))) %>% group_by(SIREN) %>% summarise(df_ba_m14A15 =sum(reelDEB)/1000)  %>% full_join(fc,by="SIREN") %>% replace(is.na(.),0)
fc  <- base_com_BA %>% filter(NOMEN %in% c("M14") & (compte5 %in% c(70871))) %>% group_by(SIREN) %>% summarise(df_ba_m14r15 =sum(reelCRE)/1000) %>%  full_join(fc,by="SIREN") %>% replace(is.na(.),0)

resultat_all  <- fc %>% mutate(fc_f = df_bp15 + df_ba_m415 +df_ba_m14A15  + df_ba_m1415 + df_ba_m14r15, fc_i = fc_sub_m5257 + fc_sub_m4 ) %>% select(SIREN,fc_f,fc_i) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)

resultat_13 <- resultat_all

remove(resultat_m52,resultat_i,resultat_m57,base_com_M57,base_com_M52,base_com_BP,base_com,resultat_all,dep_invest,dep_invest_57,dep_invest_52,rec_invest)
gc()

## Les base_invests 2012 ====

##Preparation de la base
Balancedep_2016 <- read.csv2("C:/Users/cgirard-adc/Desktop/Base comptables/balance_dep/BalanceDEPT_2012.csv")

base_com  <- Balancedep_2016
gc()

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
base_com_BP  <- base_com[base_com$BUDGET == "BP",]
base_com_BA  <- base_com[base_com$BUDGET == "BA",]

base_com_M52  <- base_com_BP[base_com_BP$NOMEN == "M52",]
base_com_M57  <- base_com_BP[base_com_BP$NOMEN == "M57",]

resultat  <- base_com_BP %>% select(SIREN,LBUDG,NDEPT) %>% distinct()
resultat_m57  <- base_com_M57 %>% select(SIREN,LBUDG) %>% distinct()
resultat_m52  <- base_com_M52 %>% select(SIREN,LBUDG) %>% distinct()

dep_invest_57  <- base_com_M57 %>% select(SIREN) %>% distinct()
dep_invest_52  <- base_com_M52 %>% select(SIREN) %>% distinct()

rec_invest  <- base_com_BP %>% select(SIREN) %>% distinct()


# Charges de fonctionnement
resultat  <- base_com_BP %>% filter(compte1==6 & !compte2 %in% c(68) & !compte3 %in% c(675,676)) %>% group_by(SIREN) %>% summarise(dep_fonc=sum(reelDEB-reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte2 %in% c(68)) %>% group_by(SIREN) %>% summarise(dot_prov=sum(reelDEB-reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)

# Produits de fonctionnement
resultat  <- base_com_BP %>% filter(compte1==7 & !compte2 %in% c(78) & !compte3 %in% c(775,776,777))  %>% group_by(SIREN) %>% summarise(rec_fonc=sum(reelCRE-reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte2 %in% c(78))  %>% group_by(SIREN) %>% summarise(repri_prov=sum(reelCRE-reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)

# Depenses d'investissement
dep_invest_52  <- base_com_M52 %>% filter((compte2 %in% c(13,20,21,23,26,27) | compte3 %in% c(102,454,455,458,481)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(dep_invest_1=sum(reelDEB)/1000) %>% full_join(dep_invest_52,by="SIREN") %>% replace(is.na(.),0)
dep_invest_52 <- base_com_M52 %>% filter(compte3 %in% c(236,237,238))  %>% group_by(SIREN) %>% summarise(dep_invest_2=sum(reelCRE)/1000) %>%  full_join(dep_invest_52,by="SIREN") %>% replace(is.na(.),0)
resultat_m52 <- dep_invest_52 %>% mutate(dep_invest=dep_invest_1-dep_invest_2) %>% select(SIREN,dep_invest) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)
dep_invest_57  <- base_com_M57 %>% filter((compte2 %in% c(13,20,21,23,26,27) | compte3 %in% c(102,454,455,458,481)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(dep_invest_1=sum(reelDEB)/1000) %>% full_join(dep_invest_57,by="SIREN") %>% replace(is.na(.),0)
dep_invest_57 <- base_com_M57 %>% filter(compte3 %in% c(237,238))  %>% group_by(SIREN) %>% summarise(dep_invest_2=sum(reelCRE)/1000) %>%  full_join(dep_invest_57,by="SIREN") %>% replace(is.na(.),0)
resultat_m57 <- dep_invest_57 %>% mutate(dep_invest=dep_invest_1-dep_invest_2) %>% select(SIREN,dep_invest) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)

## compte 21
resultat <- base_com_BP %>% filter(compte2 == 21) %>% group_by(SIREN) %>% summarise(compte21 = sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
#compte 23
resultat <- base_com_BP %>% filter(compte2 == 23) %>% group_by(SIREN) %>% summarise(compte23 = sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
##  Depenses d'équipements
dep_invest_52  <- base_com_M52 %>% filter(compte2 %in% c(20,21,23) & !compte3 %in% c(204))  %>% group_by(SIREN) %>% summarise(dep_invest_3=sum(reelDEB)/1000) %>%  full_join(dep_invest_52,by="SIREN")  %>% replace(is.na(.),0)
resultat_m52  <- dep_invest_52 %>% mutate(dep_equip=dep_invest_3-dep_invest_2) %>% select(SIREN,dep_equip) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)
dep_invest_57  <- base_com_M57 %>% filter(compte2 %in% c(20,21,23) & !compte3 %in% c(204))  %>% group_by(SIREN) %>% summarise(dep_invest_3=sum(reelDEB)/1000) %>%  full_join(dep_invest_57,by="SIREN")  %>% replace(is.na(.),0)
resultat_m57  <- dep_invest_57 %>% mutate(dep_equip=dep_invest_3-dep_invest_2) %>% select(SIREN,dep_equip) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)
resultat_m52  <- base_com_M52 %>% filter(compte2 %in% c(20,21,23) & !compte3 %in% c(204))  %>% group_by(SIREN) %>% summarise(dep_equip_brut=sum(reelDEB)/1000) %>%  full_join(resultat_m52,by="SIREN")  %>% replace(is.na(.),0)
resultat_m57  <- base_com_M57 %>% filter(compte2 %in% c(20,21,23) & !compte3 %in% c(204))  %>% group_by(SIREN) %>% summarise(dep_equip_brut=sum(reelDEB)/1000) %>%  full_join(resultat_m57,by="SIREN")  %>% replace(is.na(.),0)


#terrain
resultat  <- base_com_BP %>% filter(compte3 %in% c(211,212) | compte4 %in% c(2171,2172,2312)) %>% group_by(SIREN) %>% summarise(terrains=sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
#construction
resultat  <- base_com_BP %>% filter(compte3 %in% c(213,214) | compte4 %in% c(2173,2174,2313,2314)) %>% group_by(SIREN) %>% summarise(constructions=sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
# reseau
resultat  <- base_com_BP %>% filter(compte3 %in% c(215) | compte4 %in% c(2175,2315)) %>% group_by(SIREN) %>% summarise(reseaux=sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
#bien meuble
resultat  <- base_com_BP %>% filter(compte3 %in% c(218) | compte4 %in% c(2178,2318)) %>% group_by(SIREN) %>% summarise(bien_meuble=sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)

##  Subventions
resultat_m52  <- base_com_M52 %>% filter(compte3 %in% c(204)) %>% group_by(SIREN) %>% summarise(subvention_204=sum(reelDEB)/1000) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)
resultat_m52  <- base_com_M52 %>% filter(compte5 %in% c(20411)) %>% group_by(SIREN) %>% summarise(sub_204_etat=sum(reelDEB)/1000) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)
resultat_m52  <- base_com_M52 %>% filter(compte5 %in% c(20414)) %>% group_by(SIREN) %>% summarise(sub_204_bc=sum(reelDEB)/1000) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)
resultat_m52  <- base_com_M52 %>% filter(compte4 %in% c(2041) & !compte5 %in% c(20411,20414)) %>% group_by(SIREN) %>% summarise(sub_204_autres=sum(reelDEB)/1000) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)
resultat_m52  <- base_com_M52 %>% filter(compte4 %in% c(2042)) %>% group_by(SIREN) %>% summarise(sub_204_prive=sum(reelDEB)/1000) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)

resultat_m57  <- base_com_M57 %>% filter(compte3 %in% c(204)) %>% group_by(SIREN) %>% summarise(subvention_204=sum(reelDEB)/1000) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)
resultat_m57  <- base_com_M57 %>% filter(compte5 %in% c(20411)) %>% group_by(SIREN) %>% summarise(sub_204_etat=sum(reelDEB)/1000) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)
resultat_m57  <- base_com_M57 %>% filter(compte5 %in% c(20414) | compte6 %in% c(204151)) %>% group_by(SIREN) %>% summarise(sub_204_bc=sum(reelDEB)/1000) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)
resultat_m57  <- base_com_M57 %>% filter(compte4 %in% c(2041) & !compte5 %in% c(20411,20414) & !compte6 %in% c(204151)) %>% group_by(SIREN) %>% summarise(sub_204_autres=sum(reelDEB)/1000) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)
resultat_m57  <- base_com_M57 %>% filter(compte4 %in% c(2042)) %>% group_by(SIREN) %>% summarise(sub_204_prive=sum(reelDEB)/1000) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)
#Travaux comptes de tiers
resultat <- base_com_BP %>% filter(compte3 %in% c(454,455,458)) %>% group_by(SIREN) %>% summarise(compte_tier_d = sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)

#compte 26
resultat <- base_com_BP %>% filter(compte2 %in% c(26) & !compte3 %in% c(269)) %>% group_by(SIREN) %>% summarise(compte_26_d = sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
#compte 27
resultat <- base_com_BP %>% filter(compte2 %in% c(27) & !compte3 %in% c(279) & !compte4 %in% c(2768)) %>% group_by(SIREN) %>% summarise(compte_27_d = sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)

# Recette d'investissements
rec_invest <- base_com_BP %>% filter((compte2 %in% c(13,20,21,26,27) | compte3 %in% c(102,231,232,454,455,458)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(rec_invest_1=sum(reelCRE)/1000) %>% full_join(rec_invest,by="SIREN") %>% replace(is.na(.),0)
rec_invest <- base_com_BP %>% filter(compte3 %in% c(103,775))  %>% group_by(SIREN) %>% summarise(rec_invest_2=sum(reelCRE-reelDEB)/1000) %>%  full_join(rec_invest,by="SIREN") %>% replace(is.na(.),0)
rec_invest <- rec_invest %>% mutate(rec_invest=rec_invest_1+rec_invest_2)
resultat <- rec_invest %>% select(SIREN,rec_invest) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
#compte 13
resultat <- base_com_BP %>% filter(compte2 %in% c(13) & !compte3 == 139 | compte3 %in% c(102) & !compte5 %in% c(10222,10229) & !compte4 == 1027) %>% group_by(SIREN) %>% summarise(compte13_10 = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
## Sub recu dotations
resultat  <- base_com_BP %>% filter(compte4 %in% c(1311,1321,1381)) %>% group_by(SIREN) %>% summarise(sub_recu_etat=sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte4 %in% c(1312,1322,1382)) %>% group_by(SIREN) %>% summarise(sub_recu_reg=sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte4 %in% c(1313,1323,1383)) %>% group_by(SIREN) %>% summarise(sub_recu_dep=sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte4 %in% c(1314,1324,1384)) %>% group_by(SIREN) %>% summarise(sub_recu_com=sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte4 %in% c(1315,1325,1385)) %>% group_by(SIREN) %>% summarise(sub_recu_gfp=sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte4 %in% c(1317,1327,1387)) %>% group_by(SIREN) %>% summarise(sub_recu_bcfs=sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
# taxe amenage
resultat_m52  <- base_com_M52 %>% filter(compte4 %in% c(1334,1344)) %>% group_by(SIREN) %>% summarise(taxe_amenag=sum(reelCRE)/1000) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)
resultat_m57  <- base_com_M57 %>% filter(compte5 %in% c(10221,10226,13363,13463)) %>% group_by(SIREN) %>% summarise(taxe_amenag=sum(reelCRE)/1000) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)
## amade police
resultat_m52  <- base_com_M52 %>% filter(compte4 %in% c(1335,1345)) %>% group_by(SIREN) %>% summarise(amende=sum(reelCRE)/1000) %>% full_join(resultat_m52,by="SIREN") %>% replace(is.na(.),0)
resultat_m57  <- base_com_M57 %>% filter(compte4 %in% c(1335,1345) | compte5 %in% c(13362,13462)) %>% group_by(SIREN) %>% summarise(amende=sum(reelCRE)/1000) %>% full_join(resultat_m57,by="SIREN") %>% replace(is.na(.),0)
#FCTVA
resultat <- base_com_BP %>% filter(compte5 %in% c(10222)) %>% group_by(SIREN) %>% summarise(FCTVA = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
#plan de relance FCTVA
resultat <- base_com_BP %>% filter(compte3 %in% c(103)) %>% group_by(SIREN) %>% summarise(planFCTVA_r = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
resultat <- base_com_BP %>% filter(compte3 %in% c(103)) %>% group_by(SIREN) %>% summarise(planFCTVA_d = sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)

# Autres daotations hors FCTVA
resultat <- base_com_BP %>% filter(compte3 %in% c(102) & !compte5 %in% c(10222,10229) & !compte4 == 1027) %>% group_by(SIREN) %>% summarise(autres_dot = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
# Produits de cessions
resultat <- base_com_BP %>% filter(compte3 %in% c(775)) %>% group_by(SIREN) %>% summarise(cessions = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
#compte de tier
resultat <- base_com_BP %>% filter(compte3 %in% c(454,455,458)) %>% group_by(SIREN) %>% summarise(compte_tier_r = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
#compte 26
resultat <- base_com_BP %>% filter(compte2 %in% c(26) & !compte3 %in% c(269)) %>% group_by(SIREN) %>% summarise(compte_26_r = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
#compte 27
resultat <- base_com_BP %>% filter(compte2 %in% c(27) & !compte3 %in% c(279) & !compte4 %in% c(2768)) %>% group_by(SIREN) %>% summarise(compte_27_r = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)

resultat_i <- rbind(resultat_m52,resultat_m57)
resultat <- full_join(resultat,resultat_i,by="SIREN")
resultat_all <- resultat

# GAD
GAD  <- base_com_BP %>%
  filter(compte3 %in% c(166) | compte5 %in% c(16449)) %>%
  select(SIREN,reelCRE,reelDEB)
GAD$min  <- GAD %>% select(reelCRE,reelDEB) %>% apply(MARGIN = 1,FUN = min)
resultat_all  <- GAD %>% group_by(SIREN) %>% summarise(gad=(sum(min))/1000) %>% select(SIREN,gad) %>% full_join(resultat_all,by="SIREN" )  %>% replace(is.na(.),0)
## Emprunt
resultat_all  <- base_com_BP %>% filter(compte2 %in% c(16) & !compte3 %in% c(169) & !compte4 %in% c(1645,1688)) %>% group_by(SIREN) %>% summarise(emprunt=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all <- resultat_all %>% mutate(emprunt=emprunt-gad)
## Remboursement
resultat_all  <- base_com_BP %>% filter(compte2 %in% c(16) & !compte3 %in% c(169) & !compte4 %in% c(1645,1688)) %>% group_by(SIREN) %>% summarise(remboursement=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all <- resultat_all %>% mutate(remboursement=remboursement-gad)
# Dette
resultat_all  <- base_com_BP %>% filter(compte2 %in% c(16) & !compte3 %in% c(169) & !compte4 %in% c(1688)) %>% group_by(SIREN) %>% summarise(dette=sum(SC)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# Depot au tresor
resultat_all  <- base_com_BP %>% filter(compte3 %in% c(515)) %>% group_by(SIREN) %>% summarise(depot_tre=sum(SD)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# travaux en r?gie
resultat_all  <- base_com_BP %>% filter(compte2 %in% c(72)) %>% group_by(SIREN) %>% summarise(tra_regie=sum(ordreCRE-ordreDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
#charge a repar
resultat_all  <- base_com_BP %>% filter(compte3 %in% c(481)) %>% group_by(SIREN) %>% summarise(ch_repart=sum(ordreDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)


# Les variables pour les BA 2012 ====


# Charges de fonctionnement
resultat_all  <- base_com_BA %>% filter(compte1==6 & !compte2 %in% c(68) & !compte3 %in% c(675,676)) %>% group_by(SIREN) %>% summarise(dep_fonc_ba=sum(reelDEB-reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte2 %in% c(68)) %>% group_by(SIREN) %>% summarise(dot_prov_ba=sum(reelDEB-reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)

# Produits de fonctionnement
resultat_all  <- base_com_BA %>% filter(compte1==7 & !compte2 %in% c(78) & !compte3 %in% c(775,776,777))  %>% group_by(SIREN) %>% summarise(rec_fonc_ba=sum(reelCRE-reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte2 %in% c(78))  %>% group_by(SIREN) %>% summarise(repri_prov_ba=sum(reelCRE-reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)

# dep et rec d'invest
dep_invest  <- base_com_BA %>% select(SIREN) %>% distinct()
rec_invest <- base_com_BA %>% select(SIREN) %>% distinct()
# depense d'investissement
dep_invest  <- base_com_BA %>% filter((compte2 %in% c(13,20,21,23,26,27) | compte3 %in% c(102,454,455,458,481)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(dep_invest_1=sum(reelDEB)/1000) %>% full_join(dep_invest,by="SIREN") %>% replace(is.na(.),0)
dep_invest <- base_com_BA %>% filter(compte3 %in% c(237,238))  %>% group_by(SIREN) %>% summarise(dep_invest_2=sum(reelCRE)/1000) %>%  full_join(dep_invest,by="SIREN") %>% replace(is.na(.),0)
resultat_all <- dep_invest %>% mutate(dep_invest_ba=dep_invest_1-dep_invest_2) %>% select(SIREN,dep_invest_ba) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# les depenses d'?quip
dep_invest  <- base_com_BA %>% filter(compte2 %in% c(20,21,23) & !compte3 %in% c(204))  %>% group_by(SIREN) %>% summarise(dep_invest_3=sum(reelDEB)/1000) %>%  full_join(dep_invest,by="SIREN")  %>% replace(is.na(.),0)
resultat_all  <- dep_invest %>% mutate(dep_equip_ba=dep_invest_3-dep_invest_2,dep_equip_brut_ba=dep_invest_3) %>% select(SIREN,dep_equip_ba,dep_equip_brut_ba) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
#terrain
resultat_all  <- base_com_BA %>% filter(compte3 %in% c(211,212) | compte4 %in% c(2171,2172,2312)) %>% group_by(SIREN) %>% summarise(terrains_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
#construction
resultat_all  <- base_com_BA %>% filter(compte3 %in% c(213,214) | compte4 %in% c(2173,2174,2313,2314)) %>% group_by(SIREN) %>% summarise(constructions_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# reseau
resultat_all  <- base_com_BA %>% filter(compte3 %in% c(215) | compte4 %in% c(2175,2315)) %>% group_by(SIREN) %>% summarise(reseaux_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
#bien meuble
resultat_all  <- base_com_BA %>% filter(compte3 %in% c(218) | compte4 %in% c(2178,2318)) %>% group_by(SIREN) %>% summarise(bien_meuble_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)

# sub d equip
resultat_all  <- base_com_BA %>% filter(compte3 %in% c(204)) %>% group_by(SIREN) %>% summarise(subvention_204_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte5 %in% c(20412)) %>% group_by(SIREN) %>% summarise(sub_204_reg_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte5 %in% c(20413)) %>% group_by(SIREN) %>% summarise(sub_204_dep_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte5 %in% c(20414)) %>% group_by(SIREN) %>% summarise(sub_204_com_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte6 %in% c(204151)) %>% group_by(SIREN) %>% summarise(sub_204_gfp_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# compte de tier d
resultat_all <- base_com_BA %>% filter(compte3 %in% c(454,456,458)) %>% group_by(SIREN) %>% summarise(compte_tier_d_ba = sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)
# compte 26
resultat_all <- base_com_BA %>% filter(compte2 %in% c(26) & !compte3 %in% c(269)) %>% group_by(SIREN) %>% summarise(compte_26_ba_d = sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)
#0 compte 27
resultat_all <- base_com_BA %>% filter(compte2 %in% c(27) & !compte3 %in% c(279) & !compte4 %in% c(2768)) %>% group_by(SIREN) %>% summarise(compte_27_ba_d = sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)



# recette d'investissement
rec_invest  <- base_com_BA %>% filter((compte2 %in% c(13,20,21,26,27) | compte3 %in% c(102,231,232,454,455,458)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(rec_invest_1=sum(reelCRE)/1000) %>% full_join(rec_invest,by="SIREN") %>% replace(is.na(.),0)
rec_invest  <- base_com_BA %>% filter(compte3 %in% c(103,775))  %>% group_by(SIREN) %>% summarise(rec_invest_2=sum(reelCRE-reelDEB)/1000) %>%  full_join(rec_invest,by="SIREN") %>% replace(is.na(.),0)
resultat_all <- rec_invest %>% mutate(rec_invest_ba=rec_invest_1+rec_invest_2) %>% select(SIREN,rec_invest_ba) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# sub recu des ba
resultat_all <- base_com_BA %>% filter(compte2 %in% c(13) & !compte3 == 139 | compte3 %in% c(102) & !compte5 %in% c(10222,10229) & !compte4 == 1027) %>% group_by(SIREN) %>% summarise(compte13_10_ba = sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)
## Sub recu dotations
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1311,1321,1381)) %>% group_by(SIREN) %>% summarise(sub_recu_etat_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1312,1322,1382)) %>% group_by(SIREN) %>% summarise(sub_recu_reg_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1313,1323,1383)) %>% group_by(SIREN) %>% summarise(sub_recu_dep_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1314,1324,1384)) %>% group_by(SIREN) %>% summarise(sub_recu_com_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1315,1325,1385)) %>% group_by(SIREN) %>% summarise(sub_recu_gfp_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1317,1327,1387)) %>% group_by(SIREN) %>% summarise(sub_recu_bcfs_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
##DETR
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1331,1341)) %>% group_by(SIREN) %>% summarise(detr_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1331,1341)) %>% group_by(SIREN) %>% summarise(detr_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)

# taxe amenage
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1333,1343) | compte5 %in% c(10223,10226)) %>% group_by(SIREN) %>% summarise(taxe_amenag_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)

#amende
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1332,1342)) %>% group_by(SIREN) %>% summarise(amende_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
#FCTVA
resultat_all <- base_com_BA %>% filter(compte5 %in% c(10222)) %>% group_by(SIREN) %>% summarise(FCTVA_ba = sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)
#plan de relance FCTVA
resultat_all <- base_com_BA %>% filter(compte3 %in% c(103)) %>% group_by(SIREN) %>% summarise(planFCTVA_r_ba = sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)
resultat_all <- base_com_BA %>% filter(compte3 %in% c(103)) %>% group_by(SIREN) %>% summarise(planFCTVA_d_ba = sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)

#prod cession
resultat_all <- base_com_BA %>% filter(compte3 %in% c(775)) %>% group_by(SIREN) %>% summarise(cessions_ba = sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)

#compre de tier r
resultat_all <- base_com_BA %>% filter(compte3 %in% c(454,456,458)) %>% group_by(SIREN) %>% summarise(compte_tier_r_ba = sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)
#comtpe 26
resultat_all <- base_com_BA %>% filter(compte2 %in% c(26) & !compte3 %in% c(269)) %>% group_by(SIREN) %>% summarise(compte_26_ba_r = sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)
#0 compte 27
resultat_all <- base_com_BA %>% filter(compte2 %in% c(27) & !compte3 %in% c(279) & !compte4 %in% c(2768)) %>% group_by(SIREN) %>% summarise(compte_27_ba_r = sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)



# GAD
GAD  <- base_com_BA %>%
  filter(compte3 %in% c(166) | compte5 %in% c(16449)) %>%
  select(SIREN,reelCRE,reelDEB)
GAD$min  <- GAD %>% select(reelCRE,reelDEB) %>% apply(MARGIN = 1,FUN = min)
resultat_all  <- GAD %>% group_by(SIREN) %>% summarise(gad_ba=(sum(min))/1000) %>% select(SIREN,gad_ba) %>% full_join(resultat_all,by="SIREN" )  %>% replace(is.na(.),0)
## Emprunt
resultat_all  <- base_com_BA %>% filter(compte2 %in% c(16) & !compte3 %in% c(169) & !compte4 %in% c(1645,1688)) %>% group_by(SIREN) %>% summarise(emprunt_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all <- resultat_all %>% mutate(emprunt_ba=emprunt_ba-gad_ba)
## Remboursement
resultat_all  <- base_com_BA %>% filter(compte2 %in% c(16) & !compte3 %in% c(169) & !compte4 %in% c(1645,1688)) %>% group_by(SIREN) %>% summarise(remboursement_ba=sum(reelDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all <- resultat_all %>% mutate(remboursement_ba=remboursement_ba-gad_ba)
# Dette
resultat_all  <- base_com_BA %>% filter(compte2 %in% c(16) & !compte3 %in% c(169) & !compte4 %in% c(1688)) %>% group_by(SIREN) %>% summarise(dette_ba=sum(SC)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# Depot au tresor
resultat_all  <- base_com_BA %>% filter(compte3 %in% c(515)) %>% group_by(SIREN) %>% summarise(depot_tre_ba=sum(SD)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# travaux en r?gie
resultat_all  <- base_com_BA %>% filter(compte2 %in% c(72)) %>% group_by(SIREN) %>% summarise(tra_regie_ba=sum(ordreCRE-ordreDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
#charge a repar
resultat_all  <- base_com_BA %>% filter(compte3 %in% c(481)) %>% group_by(SIREN) %>% summarise(ch_repart_ba=sum(ordreDEB)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)


#Flux crois?e
fc <- resultat_all %>% select(SIREN)
fc <- base_com_BA %>% filter(NOMEN %in% c("M52","M57") & compte4 %in% c(1313,1323,1383))  %>% group_by(SIREN) %>% summarise(fc_sub_m5257=sum(reelCRE)/1000) %>%  full_join(fc,by="SIREN") %>% replace(is.na(.),0)
fc <- base_com_BA %>% filter(!NOMEN %in% c("M52","M57") & compte4 %in% c(1313))  %>% group_by(SIREN) %>% summarise(fc_sub_m4=sum(reelCRE)/1000) %>%  full_join(fc,by="SIREN") %>% replace(is.na(.),0)


fc  <- base_com_BP %>% filter(compte4 == 6521 | compte5 == 67441 | compte6 %in% c(657363,657364)) %>% group_by(SIREN) %>% summarise(df_bp15 =sum(reelDEB)/1000) %>% full_join(fc,by="SIREN") %>% replace(is.na(.),0)
fc  <- base_com_BA %>% filter(!NOMEN %in% c("M14","M14A") & (compte4 %in% c(6215,6287) | compte3 == 672 )) %>% group_by(SIREN) %>% summarise(df_ba_m415 =sum(reelDEB)/1000) %>% full_join(fc,by="SIREN") %>% replace(is.na(.),0)
fc  <- base_com_BA %>% filter(NOMEN %in% c("M14") & (compte4 %in% c(6215,6522) | compte5 == 62871 | compte6 == 661133)) %>% group_by(SIREN) %>% summarise(df_ba_m1415 =sum(reelDEB)/1000) %>% full_join(fc,by="SIREN") %>% replace(is.na(.),0)
fc  <- base_com_BA %>% filter(NOMEN %in% c("M14A") & (compte4 %in% c(6287,6522))) %>% group_by(SIREN) %>% summarise(df_ba_m14A15 =sum(reelDEB)/1000)  %>% full_join(fc,by="SIREN") %>% replace(is.na(.),0)
fc  <- base_com_BA %>% filter(NOMEN %in% c("M14") & (compte5 %in% c(70871))) %>% group_by(SIREN) %>% summarise(df_ba_m14r15 =sum(reelCRE)/1000) %>%  full_join(fc,by="SIREN") %>% replace(is.na(.),0)

resultat_all  <- fc %>% mutate(fc_f = df_bp15 + df_ba_m415 +df_ba_m14A15  + df_ba_m1415 + df_ba_m14r15, fc_i = fc_sub_m5257 + fc_sub_m4 ) %>% select(SIREN,fc_f,fc_i) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)

resultat_12 <- resultat_all

remove(resultat_m52,resultat_i,resultat_m57,base_com_M57,base_com_M52,base_com_BP,base_com,resultat_all,dep_invest,dep_invest_57,dep_invest_52,rec_invest)
gc()

#### dernier étape ====

resultat_12$annee=2012
resultat_13$annee=2013
resultat_14$annee=2014
resultat_15$annee=2015
resultat_16$annee=2016

resultat <- rbind(resultat_12,resultat_13,resultat_14,resultat_15,resultat_16)


write.csv2(base_invest_dep,"inst/mon_app/bdd/base_invest_dep.csv")

pop <- read.csv2("Z:/BDD et outils/Bases de donnees/general/population/pop_dep_10_17.csv")

test <- appliInvest::base_invest_dep %>% select(SIREN,annee,strate16)

resultat <- resultat %>% select(-NDEPT) %>% left_join(test,by=c("SIREN","annee"))

resultat <- resultat %>% filter(!is.na(NDEPT))

pop <- pop %>% rename(population = pop_t)

resultat <- resultat %>%
  left_join(pop,by=c("NDEPT"="code_dep","annee"))

resultat <- resultat %>% select(-nom_dep.y,- nb_com,-pop_mun) %>% rename(nom_dep = nom_dep.x)

base_invest_dep <- resultat %>% left_join(test,by=c("SIREN","annee"))

devtools::use_data(base_invest_dep,pkg = ".",overwrite = T)


#### traitement spe met lyon

#apres avoir calculer les données pour la mtl

resultat_15$annee=2015
resultat_16$annee=2016

resultat <- rbind.data.frame(resultat_15,resultat_16)

resultat <- resultat %>%
  filter(SIREN == 200046977)

test <- appliInvest::base_invest_dep

setdiff(names(test),names(resultat))

resultat_bc <- resultat %>% select(sub_204_bc)
resultat <- resultat %>% select(-sub_204_bc)

resultat$NDEPT <- NA
resultat$nom_dep <- "METRO LYON"
resultat$population = 1370678

resultat$strate16 <- "1000000 et plus"

resultat <- cbind(resultat,resultat_bc)

base_invest_dep <- rbind(test,resultat)

devtools::use_data(base_invest_dep,pkg = ".",overwrite = T)

dfm <- read.csv2("C:/Users/cgirard-adc/Desktop/Classeur1.csv")

base_invest_dep <-  appliInvest::base_invest_dep

base_invest_dep <- base_invest_dep %>%
  left_join(dfm,by="nom_dep")

base_invest_dep$DFM <- ifelse(base_invest_dep$nom_dep == "Alpes-Maritimes" & base_invest_dep$annee == 2012,0,base_invest_dep$DFM)

base_invest_dep <- base_invest_dep %>%
  select(-X)

