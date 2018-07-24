# Préparation de la base investissemennt ====

library(readr)
library(dplyr)
library(tidyr)


## Les base_invests 2016 ====

##Preparation de la base
BalanceCommune_2016 <- read_csv2("C:/Users/cgirard-adc/Desktop/Base comptables/balance_communes/Balance_Commune_2016.csv")

base_com <- BalanceCommune_2016

base_com <- rename(base_com,obDEB=OBNETDEB,ordreDEB=OOBDEB,obCRE=OBNETCRE,ordreCRE=OOBCRE)
base_com$reelDEB=base_com$obDEB-base_com$ordreDEB
base_com$reelCRE=base_com$obCRE-base_com$ordreCRE

base_com$compte1 <- substr(base_com$COMPTE,1,1)
base_com$compte2 <- substr(base_com$COMPTE,1,2)
base_com$compte3 <- substr(base_com$COMPTE,1,3)
base_com$compte4 <- substr(base_com$COMPTE,1,4)
base_com$compte5 <- substr(base_com$COMPTE,1,5)
base_com$compte6 <- substr(base_com$COMPTE,1,6)

base_com_BP  <- base_com[base_com$CBUDG == 1,]
base_com_BA  <- base_com[base_com$CBUDG == 3 & !base_com$NOMEN == "M22",]
list_mgp  <- c(217500016,219100278,219103264,219104320,219104791,219105897,219106879,219200029,219200045,219200078,219200094,219200128,219200144,219200193,219200201,219200227,219200235,219200243,219200250,219200268,219200326,219200334,219200359,219200367,219200409,219200441,219200466,219200474,219200482,219200490,219200508,219200516,219200607,219200623,219200631,219200649,219200714,219200722,219200730,219200755,219200763,219200771,219200789,219300019,219300050,219300068,219300076,219300084,219300100,219300134,219300142,219300159,219300274,219300290,219300308,219300316,219300324,219300332,219300399,219300456,219300464,219300472,219300480,219300498,219300506,219300514,219300530,219300555,219300571,219300597,219300613,219300621,219300639,219300647,219300662,219300704,219300712,219300720,219300738,219300746,219300779,219300787,219300795,219400017,219400025,219400033,219400041,219400116,219400157,219400165,219400173,219400181,219400199,219400215,219400223,219400280,219400330,219400348,219400371,219400389,219400413,219400421,219400439,219400447,219400462,219400470,219400488,219400520,219400538,219400546,219400553,219400561,219400587,219400595,219400603,219400652,219400678,219400686,219400694,219400702,219400710,219400736,219400744,219400751,219400769,219400777,219400785,219400793,219400801,219400819,219500188)
base_com_HMGP  <- base_com_BP %>% filter(!SIREN %in% list_mgp)
base_com_MGP  <- base_com_BP %>% filter(SIREN %in% list_mgp)
list  <-base_com_BP %>% group_by(SIREN,LBUDG) %>% summarise(n=n()) %>% select(SIREN,LBUDG)


resultat_HMGP  <- list %>% filter(!SIREN %in% list_mgp)

resultat_MGP  <- list %>% filter(SIREN %in% list_mgp)

dep_invest_HMGP  <- list %>% filter(!SIREN %in% list_mgp) %>% select(SIREN)
dep_invest_MGP  <- list %>% filter(SIREN %in% list_mgp) %>% select(SIREN)

rec_invest_HMGP  <- list %>% filter(!SIREN %in% list_mgp) %>% select(SIREN)
rec_invest_MGP  <- list %>% filter(SIREN %in% list_mgp) %>% select(SIREN)

resultat_ba_m14  <- base_com_BA %>% filter(NOMEN %in% c("M14","M14A")) %>% select(SIREN) %>%  distinct()
resultat_ba_m4  <- base_com_BA %>% filter(!NOMEN %in% c("M14","M14A")) %>% select(SIREN) %>% distinct()

remove(BalanceCommune_2016,base_com)

gc()
gc()

# Les variables pour les BP 2016 ====

# Charges de fonctionnement
resultat_HMGP  <- base_com_HMGP %>% filter(compte1==6 & !compte2 %in% c(68) & !compte3 %in% c(675,676)) %>% group_by(SIREN) %>% summarise(dep_fonc=sum(reelDEB-reelCRE)/1000) %>% full_join(resultat_HMGP,by="SIREN") %>% replace(is.na(.),0)
resultat_MGP  <- base_com_MGP %>% filter(compte1==6 & !compte2 %in% c(68) & !compte3 %in% c(675,676) & !compte5 %in% c(65541)) %>% group_by(SIREN) %>% summarise(dep_fonc=sum(reelDEB-reelCRE)/1000) %>% full_join(resultat_MGP,by="SIREN") %>% replace(is.na(.),0)
resultat_HMGP  <- base_com_HMGP %>% filter(compte2 %in% c(68)) %>% group_by(SIREN) %>% summarise(dot_prov=sum(reelDEB-reelCRE)/1000) %>% full_join(resultat_HMGP,by="SIREN") %>% replace(is.na(.),0)
resultat_MGP  <- base_com_MGP %>% filter(compte2 %in% c(68)) %>% group_by(SIREN) %>% summarise(dot_prov=sum(reelDEB-reelCRE)/1000) %>% full_join(resultat_MGP,by="SIREN") %>% replace(is.na(.),0)

# Produits de fonctionnement
resultat_HMGP  <- base_com_HMGP %>% filter(compte1==7 & !compte2 %in% c(78) & !compte3 %in% c(775,776,777))  %>% group_by(SIREN) %>% summarise(rec_fonc=sum(reelCRE-reelDEB)/1000) %>% full_join(resultat_HMGP,by="SIREN") %>% replace(is.na(.),0)
rec_fonc  <- base_com_MGP %>% filter(compte1==7 & !compte2 %in% c(78) & !compte3 %in% c(775,776,777))  %>% group_by(SIREN) %>% summarise(rec_fonc=sum(reelCRE-reelDEB)/1000)  %>% replace(is.na(.),0)
rec_fonc  <- base_com_MGP %>% filter(compte5 %in% c(65541)) %>% group_by(SIREN) %>% summarise(compte65541=sum(reelDEB)/1000) %>% full_join(rec_fonc,by="SIREN") %>% replace(is.na(.),0)
resultat_MGP  <- rec_fonc %>% mutate(rec_fonc=rec_fonc-compte65541) %>% select(SIREN,rec_fonc) %>% full_join(resultat_MGP,by="SIREN") %>% replace(is.na(.),0)
resultat_HMGP  <- base_com_HMGP %>% filter(compte2 %in% c(78))  %>% group_by(SIREN) %>% summarise(repri_prov=sum(reelCRE-reelDEB)/1000) %>% full_join(resultat_HMGP,by="SIREN") %>% replace(is.na(.),0)
resultat_MGP  <- base_com_MGP %>% filter(compte2 %in% c(78))  %>% group_by(SIREN) %>% summarise(repri_prov=sum(reelCRE-reelDEB)/1000) %>% full_join(resultat_MGP,by="SIREN") %>% replace(is.na(.),0)

# Depenses d'investissement
dep_invest_HMGP  <- base_com_HMGP %>% filter((compte2 %in% c(13,20,21,23,26,27) | compte3 %in% c(102,454,456,458,481)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(dep_invest_1=sum(reelDEB)/1000) %>% full_join(dep_invest_HMGP,by="SIREN") %>% replace(is.na(.),0)
dep_invest_HMGP  <- base_com_HMGP %>% filter(compte3 %in% c(237,238))  %>% group_by(SIREN) %>% summarise(dep_invest_2=sum(reelCRE)/1000) %>%  full_join(dep_invest_HMGP,by="SIREN") %>% replace(is.na(.),0)
resultat_HMGP <- dep_invest_HMGP %>% mutate(dep_invest=dep_invest_1-dep_invest_2) %>% select(SIREN,dep_invest) %>% full_join(resultat_HMGP,by="SIREN") %>% replace(is.na(.),0)
dep_invest_MGP  <- base_com_MGP %>% filter((compte2 %in% c(13,20,21,23,26,27) | compte3 %in% c(102,454,456,458,481)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(dep_invest_1=sum(reelDEB)/1000) %>% full_join(dep_invest_MGP,by="SIREN") %>% replace(is.na(.),0)
dep_invest_MGP  <- base_com_MGP %>% filter(compte3 %in% c(237,238))  %>% group_by(SIREN) %>% summarise(dep_invest_2=sum(reelCRE)/1000) %>%  full_join(dep_invest_MGP,by="SIREN")  %>% replace(is.na(.),0)
resultat_MGP <- dep_invest_MGP %>% mutate(dep_invest=dep_invest_1-dep_invest_2) %>% select(SIREN,dep_invest) %>% full_join(resultat_MGP,by="SIREN") %>% replace(is.na(.),0)
## compte 21
resultat_HMGP <- base_com_HMGP %>% filter(compte2 == 21) %>% group_by(SIREN) %>% summarise(compte21 = sum(reelDEB)/1000) %>% full_join(resultat_HMGP,by="SIREN")  %>% replace(is.na(.),0)
resultat_MGP <- base_com_MGP %>% filter(compte2 == 21) %>% group_by(SIREN) %>% summarise(compte21 = sum(reelDEB)/1000) %>% full_join(resultat_MGP,by="SIREN")  %>% replace(is.na(.),0)
#compte 23
resultat_HMGP <- base_com_HMGP %>% filter(compte2 == 23) %>% group_by(SIREN) %>% summarise(compte23 = sum(reelDEB)/1000) %>% full_join(resultat_HMGP,by="SIREN")  %>% replace(is.na(.),0)
resultat_MGP <- base_com_MGP %>% filter(compte2 == 23) %>% group_by(SIREN) %>% summarise(compte23 = sum(reelDEB)/1000) %>% full_join(resultat_MGP,by="SIREN")  %>% replace(is.na(.),0)
##  Depenses d'équipements
dep_invest_HMGP  <- base_com_HMGP %>% filter(compte2 %in% c(20,21,23) & !compte3 %in% c(204))  %>% group_by(SIREN) %>% summarise(dep_invest_3=sum(reelDEB)/1000) %>%  full_join(dep_invest_HMGP,by="SIREN")  %>% replace(is.na(.),0)
resultat_HMGP  <- dep_invest_HMGP %>% mutate(dep_equip=dep_invest_3-dep_invest_2) %>% select(SIREN,dep_equip) %>% full_join(resultat_HMGP,by="SIREN") %>% replace(is.na(.),0)
dep_invest_MGP  <- base_com_MGP %>% filter(compte2 %in% c(20,21,23) & !compte3 %in% c(204))  %>% group_by(SIREN) %>% summarise(dep_invest_3=sum(reelDEB)/1000) %>%  full_join(dep_invest_MGP,by="SIREN") %>% replace(is.na(.),0)
resultat_MGP  <- dep_invest_MGP %>% mutate(dep_equip=dep_invest_3-dep_invest_2) %>% select(SIREN,dep_equip) %>% full_join(resultat_MGP,by="SIREN") %>% replace(is.na(.),0)
#description des dep equip
#terrain
resultat_HMGP  <- base_com_HMGP %>% filter(compte3 %in% c(211,212) | compte4 %in% c(2171,2172,2312)) %>% group_by(SIREN) %>% summarise(terrains=sum(reelDEB)/1000) %>% full_join(resultat_HMGP,by="SIREN") %>% replace(is.na(.),0)
resultat_MGP  <- base_com_MGP %>% filter(compte3 %in% c(211,212) | compte4 %in% c(2171,2172,2312)) %>% group_by(SIREN) %>% summarise(terrains=sum(reelDEB)/1000) %>% full_join(resultat_MGP,by="SIREN") %>% replace(is.na(.),0)
#construction
resultat_HMGP  <- base_com_HMGP %>% filter(compte3 %in% c(213,214) | compte4 %in% c(2173,2174,2313,2314)) %>% group_by(SIREN) %>% summarise(constructions=sum(reelDEB)/1000) %>% full_join(resultat_HMGP,by="SIREN") %>% replace(is.na(.),0)
resultat_MGP  <- base_com_MGP %>% filter(compte3 %in% c(213,214) | compte4 %in% c(2173,2174,2313,2314)) %>% group_by(SIREN) %>% summarise(constructions=sum(reelDEB)/1000) %>% full_join(resultat_MGP,by="SIREN") %>% replace(is.na(.),0)
# reseau
resultat_HMGP  <- base_com_HMGP %>% filter(compte3 %in% c(215) | compte4 %in% c(2175,2315)) %>% group_by(SIREN) %>% summarise(reseaux=sum(reelDEB)/1000) %>% full_join(resultat_HMGP,by="SIREN") %>% replace(is.na(.),0)
resultat_MGP  <- base_com_MGP %>% filter(compte3 %in% c(215) | compte4 %in% c(2175,2315)) %>% group_by(SIREN) %>% summarise(reseaux=sum(reelDEB)/1000) %>% full_join(resultat_MGP,by="SIREN") %>% replace(is.na(.),0)
#bien meuble
resultat_HMGP  <- base_com_HMGP %>% filter(compte3 %in% c(218) | compte4 %in% c(2178,2318)) %>% group_by(SIREN) %>% summarise(bien_meuble=sum(reelDEB)/1000) %>% full_join(resultat_HMGP,by="SIREN") %>% replace(is.na(.),0)
resultat_MGP  <- base_com_MGP %>% filter(compte3 %in% c(218) | compte4 %in% c(2178,2318)) %>% group_by(SIREN) %>% summarise(bien_meuble=sum(reelDEB)/1000) %>% full_join(resultat_MGP,by="SIREN") %>% replace(is.na(.),0)

##  Subventions
resultat_HMGP  <- base_com_HMGP %>% filter(compte3 %in% c(204)) %>% group_by(SIREN) %>% summarise(subvention_204=sum(reelDEB)/1000) %>% full_join(resultat_HMGP,by="SIREN") %>% replace(is.na(.),0)
resultat_HMGP  <- base_com_HMGP %>% filter(compte5 %in% c(20412)) %>% group_by(SIREN) %>% summarise(sub_204_reg=sum(reelDEB)/1000) %>% full_join(resultat_HMGP,by="SIREN") %>% replace(is.na(.),0)
resultat_HMGP  <- base_com_HMGP %>% filter(compte5 %in% c(20413)) %>% group_by(SIREN) %>% summarise(sub_204_dep=sum(reelDEB)/1000) %>% full_join(resultat_HMGP,by="SIREN") %>% replace(is.na(.),0)
resultat_HMGP  <- base_com_HMGP %>% filter(compte5 %in% c(20414)) %>% group_by(SIREN) %>% summarise(sub_204_com=sum(reelDEB)/1000) %>% full_join(resultat_HMGP,by="SIREN") %>% replace(is.na(.),0)
resultat_HMGP  <- base_com_HMGP %>% filter(compte6 %in% c(204151)) %>% group_by(SIREN) %>% summarise(sub_204_gfp=sum(reelDEB)/1000) %>% full_join(resultat_HMGP,by="SIREN") %>% replace(is.na(.),0)
resultat_MGP  <- base_com_MGP %>% filter(compte3 %in% c(204)) %>% group_by(SIREN) %>% summarise(subvention_204=sum(reelDEB)/1000) %>% full_join(resultat_MGP,by="SIREN") %>% replace(is.na(.),0)
resultat_MGP  <- base_com_MGP %>% filter(compte5 %in% c(20412)) %>% group_by(SIREN) %>% summarise(sub_204_reg=sum(reelDEB)/1000) %>% full_join(resultat_MGP,by="SIREN") %>% replace(is.na(.),0)
resultat_MGP  <- base_com_MGP %>% filter(compte5 %in% c(20413)) %>% group_by(SIREN) %>% summarise(sub_204_dep=sum(reelDEB)/1000) %>% full_join(resultat_MGP,by="SIREN") %>% replace(is.na(.),0)
resultat_MGP  <- base_com_MGP %>% filter(compte5 %in% c(20414)) %>% group_by(SIREN) %>% summarise(sub_204_com=sum(reelDEB)/1000) %>% full_join(resultat_MGP,by="SIREN") %>% replace(is.na(.),0)
resultat_MGP  <- base_com_MGP %>% filter(compte6 %in% c(204151)) %>% group_by(SIREN) %>% summarise(sub_204_gfp=sum(reelDEB)/1000) %>% full_join(resultat_MGP,by="SIREN") %>% replace(is.na(.),0)
#Travaux comptes de tiers
resultat_HMGP <- base_com_HMGP %>% filter(compte3 %in% c(454,456,458)) %>% group_by(SIREN) %>% summarise(compte_tier_d = sum(reelDEB)/1000) %>% full_join(resultat_HMGP,by="SIREN")  %>% replace(is.na(.),0)
resultat_MGP <- base_com_MGP %>% filter(compte3 %in% c(454,456,458)) %>% group_by(SIREN) %>% summarise(compte_tier_d = sum(reelDEB)/1000) %>% full_join(resultat_MGP,by="SIREN")  %>% replace(is.na(.),0)
#compte 26
resultat_HMGP <- base_com_HMGP %>% filter(compte2 %in% c(26) & !compte3 %in% c(269)) %>% group_by(SIREN) %>% summarise(compte_26_d = sum(reelDEB)/1000) %>% full_join(resultat_HMGP,by="SIREN")  %>% replace(is.na(.),0)
resultat_MGP <- base_com_MGP %>% filter(compte2 %in% c(26) & !compte3 %in% c(269)) %>% group_by(SIREN) %>% summarise(compte_26_d = sum(reelDEB)/1000) %>% full_join(resultat_MGP,by="SIREN")  %>% replace(is.na(.),0)
#compte 27
resultat_HMGP <- base_com_HMGP %>% filter(compte2 %in% c(27) & !compte3 %in% c(279) & !compte4 %in% c(2768)) %>% group_by(SIREN) %>% summarise(compte_27_d = sum(reelDEB)/1000) %>% full_join(resultat_HMGP,by="SIREN")  %>% replace(is.na(.),0)
resultat_MGP <- base_com_MGP %>% filter(compte2 %in% c(27) & !compte3 %in% c(279) & !compte4 %in% c(2768)) %>% group_by(SIREN) %>% summarise(compte_27_d = sum(reelDEB)/1000) %>% full_join(resultat_MGP,by="SIREN")  %>% replace(is.na(.),0)


# Recette d'investissements
rec_invest_HMGP  <- base_com_HMGP %>% filter((compte2 %in% c(13,20,21,26,27) | compte3 %in% c(102,231,232,454,456,458)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(rec_invest_1=sum(reelCRE)/1000) %>% full_join(rec_invest_HMGP,by="SIREN") %>% replace(is.na(.),0)
rec_invest_HMGP  <- base_com_HMGP %>% filter(compte3 %in% c(103,775))  %>% group_by(SIREN) %>% summarise(rec_invest_2=sum(reelCRE-reelDEB)/1000) %>%  full_join(rec_invest_HMGP,by="SIREN") %>% replace(is.na(.),0)
rec_invest_HMGP  <- rec_invest_HMGP %>% mutate(rec_invest=rec_invest_1+rec_invest_2)
resultat_HMGP <- rec_invest_HMGP %>% select(SIREN,rec_invest) %>% full_join(resultat_HMGP,by="SIREN") %>% replace(is.na(.),0)
rec_invest_MGP  <- base_com_MGP %>% filter((compte2 %in% c(13,20,21,26,27) | compte3 %in% c(102,231,232,454,456,458)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(rec_invest_1=sum(reelCRE)/1000) %>% full_join(rec_invest_MGP,by="SIREN") %>% replace(is.na(.),0)
rec_invest_MGP  <- base_com_MGP %>% filter(compte3 %in% c(103,775))  %>% group_by(SIREN) %>% summarise(rec_invest_2=sum(reelCRE-reelDEB)/1000) %>%  full_join(rec_invest_MGP,by="SIREN") %>% replace(is.na(.),0)
rec_invest_MGP  <- rec_invest_MGP %>% mutate(rec_invest=rec_invest_1+rec_invest_2)
resultat_MGP <- rec_invest_MGP %>% select(SIREN,rec_invest) %>% full_join(resultat_MGP,by="SIREN") %>% replace(is.na(.),0)
#subvention recu
resultat_HMGP <- base_com_HMGP %>% filter(compte2 %in% c(13) & !compte3 == 139 | compte3 %in% c(102) & !compte5 %in% c(10222,10229) & !compte4 == 1027) %>% group_by(SIREN) %>% summarise(compte13_10 = sum(reelCRE)/1000) %>% full_join(resultat_HMGP,by="SIREN")  %>% replace(is.na(.),0)
resultat_MGP <- base_com_MGP %>% filter(compte2 %in% c(13) & !compte3 == 139 | compte3 %in% c(102) & !compte5 %in% c(10222,10229) & !compte4 == 1027) %>% group_by(SIREN) %>% summarise(compte13_10 = sum(reelCRE)/1000) %>% full_join(resultat_MGP,by="SIREN")  %>% replace(is.na(.),0)
## Sub recu dotations
resultat_HMGP  <- base_com_HMGP %>% filter(compte4 %in% c(1311,1321,1381)) %>% group_by(SIREN) %>% summarise(sub_recu_etat=sum(reelCRE)/1000) %>% full_join(resultat_HMGP,by="SIREN") %>% replace(is.na(.),0)
resultat_HMGP  <- base_com_HMGP %>% filter(compte4 %in% c(1312,1322,1382)) %>% group_by(SIREN) %>% summarise(sub_recu_reg=sum(reelCRE)/1000) %>% full_join(resultat_HMGP,by="SIREN") %>% replace(is.na(.),0)
resultat_HMGP  <- base_com_HMGP %>% filter(compte4 %in% c(1313,1323,1383)) %>% group_by(SIREN) %>% summarise(sub_recu_dep=sum(reelCRE)/1000) %>% full_join(resultat_HMGP,by="SIREN") %>% replace(is.na(.),0)
resultat_HMGP  <- base_com_HMGP %>% filter(compte4 %in% c(1314,1324,1384)) %>% group_by(SIREN) %>% summarise(sub_recu_com=sum(reelCRE)/1000) %>% full_join(resultat_HMGP,by="SIREN") %>% replace(is.na(.),0)
resultat_HMGP  <- base_com_HMGP %>% filter(compte4 %in% c(1315,1325,1385)) %>% group_by(SIREN) %>% summarise(sub_recu_gfp=sum(reelCRE)/1000) %>% full_join(resultat_HMGP,by="SIREN") %>% replace(is.na(.),0)
resultat_HMGP  <- base_com_HMGP %>% filter(compte4 %in% c(1317,1327,1387)) %>% group_by(SIREN) %>% summarise(sub_recu_bcfs=sum(reelCRE)/1000) %>% full_join(resultat_HMGP,by="SIREN") %>% replace(is.na(.),0)
resultat_MGP  <- base_com_MGP %>% filter(compte4 %in% c(1311,1321,1381)) %>% group_by(SIREN) %>% summarise(sub_recu_etat=sum(reelCRE)/1000) %>% full_join(resultat_MGP,by="SIREN") %>% replace(is.na(.),0)
resultat_MGP  <- base_com_MGP %>% filter(compte4 %in% c(1312,1322,1382)) %>% group_by(SIREN) %>% summarise(sub_recu_reg=sum(reelCRE)/1000) %>% full_join(resultat_MGP,by="SIREN") %>% replace(is.na(.),0)
resultat_MGP  <- base_com_MGP %>% filter(compte4 %in% c(1313,1323,1383)) %>% group_by(SIREN) %>% summarise(sub_recu_dep=sum(reelCRE)/1000) %>% full_join(resultat_MGP,by="SIREN") %>% replace(is.na(.),0)
resultat_MGP  <- base_com_MGP %>% filter(compte4 %in% c(1314,1324,1384)) %>% group_by(SIREN) %>% summarise(sub_recu_com=sum(reelCRE)/1000) %>% full_join(resultat_MGP,by="SIREN") %>% replace(is.na(.),0)
resultat_MGP  <- base_com_MGP %>% filter(compte4 %in% c(1315,1325,1385)) %>% group_by(SIREN) %>% summarise(sub_recu_gfp=sum(reelCRE)/1000) %>% full_join(resultat_MGP,by="SIREN") %>% replace(is.na(.),0)
resultat_MGP  <- base_com_MGP %>% filter(compte4 %in% c(1317,1327,1387)) %>% group_by(SIREN) %>% summarise(sub_recu_bcfs=sum(reelCRE)/1000) %>% full_join(resultat_MGP,by="SIREN") %>% replace(is.na(.),0)
## DETR
resultat_HMGP  <- base_com_HMGP %>% filter(compte4 %in% c(1331,1341)) %>% group_by(SIREN) %>% summarise(detr=sum(reelCRE)/1000) %>% full_join(resultat_HMGP,by="SIREN") %>% replace(is.na(.),0)
resultat_MGP  <- base_com_MGP %>% filter(compte4 %in% c(1331,1341)) %>% group_by(SIREN) %>% summarise(detr=sum(reelCRE)/1000) %>% full_join(resultat_MGP,by="SIREN") %>% replace(is.na(.),0)
#taxe amenangement
resultat_HMGP  <- base_com_HMGP %>% filter(compte4 %in% c(1333,1343) | compte5 %in% c(10223,10226)) %>% group_by(SIREN) %>% summarise(taxe_amenag=sum(reelCRE)/1000) %>% full_join(resultat_HMGP,by="SIREN") %>% replace(is.na(.),0)
resultat_MGP  <- base_com_MGP %>% filter(compte4 %in% c(1333,1343) | compte5 %in% c(10223,10226)) %>% group_by(SIREN) %>% summarise(taxe_amenag=sum(reelCRE)/1000) %>% full_join(resultat_MGP,by="SIREN") %>% replace(is.na(.),0)

## amade police
resultat_HMGP  <- base_com_HMGP %>% filter(compte4 %in% c(1332,1342)) %>% group_by(SIREN) %>% summarise(amende=sum(reelCRE)/1000) %>% full_join(resultat_HMGP,by="SIREN") %>% replace(is.na(.),0)
resultat_MGP  <- base_com_MGP %>% filter(compte4 %in% c(1332,1342)) %>% group_by(SIREN) %>% summarise(amende=sum(reelCRE)/1000) %>% full_join(resultat_MGP,by="SIREN") %>% replace(is.na(.),0)

#FCTVA
resultat_HMGP <- base_com_HMGP %>% filter(compte5 %in% c(10222)) %>% group_by(SIREN) %>% summarise(FCTVA = sum(reelCRE)/1000) %>% full_join(resultat_HMGP,by="SIREN")  %>% replace(is.na(.),0)
resultat_MGP <- base_com_MGP %>% filter(compte5 %in% c(10222)) %>% group_by(SIREN) %>% summarise(FCTVA = sum(reelCRE)/1000) %>% full_join(resultat_MGP,by="SIREN")  %>% replace(is.na(.),0)
#plan de relance FCTVA
resultat_HMGP <- base_com_HMGP %>% filter(compte3 %in% c(103)) %>% group_by(SIREN) %>% summarise(planFCTVA_r = sum(reelCRE)/1000) %>% full_join(resultat_HMGP,by="SIREN")  %>% replace(is.na(.),0)
resultat_MGP <- base_com_MGP %>% filter(compte3 %in% c(103)) %>% group_by(SIREN) %>% summarise(planFCTVA_r = sum(reelCRE)/1000) %>% full_join(resultat_MGP,by="SIREN")  %>% replace(is.na(.),0)
resultat_HMGP <- base_com_HMGP %>% filter(compte3 %in% c(103)) %>% group_by(SIREN) %>% summarise(planFCTVA_d = sum(reelDEB)/1000) %>% full_join(resultat_HMGP,by="SIREN")  %>% replace(is.na(.),0)
resultat_MGP <- base_com_MGP %>% filter(compte3 %in% c(103)) %>% group_by(SIREN) %>% summarise(planFCTVA_d = sum(reelDEB)/1000) %>% full_join(resultat_MGP,by="SIREN")  %>% replace(is.na(.),0)

# Autres daotations hors FCTVA
resultat_HMGP <- base_com_HMGP %>% filter(compte3 %in% c(102) & !compte5 %in% c(10222,10229) & !compte4 == 1027) %>% group_by(SIREN) %>% summarise(autres_dot = sum(reelCRE)/1000) %>% full_join(resultat_HMGP,by="SIREN")  %>% replace(is.na(.),0)
resultat_MGP <- base_com_MGP %>% filter(compte3 %in% c(102) & !compte5 %in% c(10222,10229) & !compte4 == 1027) %>% group_by(SIREN) %>% summarise(autres_dot = sum(reelCRE)/1000) %>% full_join(resultat_MGP,by="SIREN")  %>% replace(is.na(.),0)
# Produits de cessions
resultat_HMGP <- base_com_HMGP %>% filter(compte3 %in% c(775)) %>% group_by(SIREN) %>% summarise(cessions = sum(reelCRE)/1000) %>% full_join(resultat_HMGP,by="SIREN")  %>% replace(is.na(.),0)
resultat_MGP <- base_com_MGP %>% filter(compte3 %in% c(775)) %>% group_by(SIREN) %>% summarise(cessions = sum(reelCRE)/1000) %>% full_join(resultat_MGP,by="SIREN")  %>% replace(is.na(.),0)
#compte de tier
resultat_HMGP <- base_com_HMGP %>% filter(compte3 %in% c(454,456,458)) %>% group_by(SIREN) %>% summarise(compte_tier_r = sum(reelCRE)/1000) %>% full_join(resultat_HMGP,by="SIREN")  %>% replace(is.na(.),0)
resultat_MGP <- base_com_MGP %>% filter(compte3 %in% c(454,456,458)) %>% group_by(SIREN) %>% summarise(compte_tier_r = sum(reelCRE)/1000) %>% full_join(resultat_MGP,by="SIREN")  %>% replace(is.na(.),0)
#compte 26
resultat_HMGP <- base_com_HMGP %>% filter(compte2 %in% c(26) & !compte3 %in% c(269)) %>% group_by(SIREN) %>% summarise(compte_26_r = sum(reelCRE)/1000) %>% full_join(resultat_HMGP,by="SIREN")  %>% replace(is.na(.),0)
resultat_MGP <- base_com_MGP %>% filter(compte2 %in% c(26) & !compte3 %in% c(269)) %>% group_by(SIREN) %>% summarise(compte_26_r = sum(reelCRE)/1000) %>% full_join(resultat_MGP,by="SIREN")  %>% replace(is.na(.),0)
#compte 27
resultat_HMGP <- base_com_HMGP %>% filter(compte2 %in% c(27) & !compte3 %in% c(279) & !compte4 %in% c(2768)) %>% group_by(SIREN) %>% summarise(compte_27_r = sum(reelCRE)/1000) %>% full_join(resultat_HMGP,by="SIREN")  %>% replace(is.na(.),0)
resultat_MGP <- base_com_MGP %>% filter(compte2 %in% c(27) & !compte3 %in% c(279) & !compte4 %in% c(2768)) %>% group_by(SIREN) %>% summarise(compte_27_r = sum(reelCRE)/1000) %>% full_join(resultat_MGP,by="SIREN")  %>% replace(is.na(.),0)


resultat_all <- rbind(resultat_HMGP,resultat_MGP)

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
#dep equip brute
resultat_all  <- base_com_BP %>% filter(compte2 %in% c(20,21,23) & !compte3 %in% c(204))  %>% group_by(SIREN) %>% summarise(dep_equip_brut=sum(reelDEB)/1000) %>%  full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)


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
dep_invest  <- base_com_BA %>% filter((compte2 %in% c(13,20,21,23,26,27) | compte3 %in% c(102,454,456,458,481)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(dep_invest_1=sum(reelDEB)/1000) %>% full_join(dep_invest,by="SIREN") %>% replace(is.na(.),0)
dep_invest <- base_com_BA %>% filter(compte3 %in% c(237,238))  %>% group_by(SIREN) %>% summarise(dep_invest_2=sum(reelCRE)/1000) %>%  full_join(dep_invest,by="SIREN") %>% replace(is.na(.),0)
resultat_all <- dep_invest %>% mutate(dep_invest_ba=dep_invest_1-dep_invest_2) %>% select(SIREN,dep_invest_ba) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# les depenses d'?quip
dep_invest  <- base_com_BA %>% filter(compte2 %in% c(20,21,23) & !compte3 %in% c(204))  %>% group_by(SIREN) %>% summarise(dep_invest_3=sum(reelDEB)/1000) %>%  full_join(dep_invest,by="SIREN")  %>% replace(is.na(.),0)
resultat_all  <- dep_invest %>% mutate(dep_equip_ba=dep_invest_3-dep_invest_2) %>% select(SIREN,dep_equip_ba) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
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
rec_invest  <- base_com_BA %>% filter((compte2 %in% c(13,20,21,26,27) | compte3 %in% c(102,231,232,454,456,458)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(rec_invest_1=sum(reelCRE)/1000) %>% full_join(rec_invest,by="SIREN") %>% replace(is.na(.),0)
rec_invest  <- base_com_BA %>% filter(compte3 %in% c(103,775))  %>% group_by(SIREN) %>% summarise(rec_invest_2=sum(reelCRE-reelDEB)/1000) %>%  full_join(rec_invest,by="SIREN") %>% replace(is.na(.),0)
resultat_all <- rec_invest %>% mutate(rec_invest_ba=rec_invest_1+rec_invest_2) %>% select(SIREN,rec_invest_ba) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
#sub recu de ba
resultat_all <- base_com_BA %>% filter(compte2 %in% c(13) & !compte3 == 139 | compte3 %in% c(102) & !compte5 %in% c(10222,10229) & !compte4 == 1027) %>% group_by(SIREN) %>% summarise(compte13_10_ba = sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)
## Sub recu dotations
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1311,1321,1381)) %>% group_by(SIREN) %>% summarise(sub_recu_etat_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1312,1322,1382)) %>% group_by(SIREN) %>% summarise(sub_recu_reg_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1313,1323,1383)) %>% group_by(SIREN) %>% summarise(sub_recu_dep_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1314,1324,1384)) %>% group_by(SIREN) %>% summarise(sub_recu_com_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1315,1325,1385)) %>% group_by(SIREN) %>% summarise(sub_recu_gfp_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1317,1327,1387)) %>% group_by(SIREN) %>% summarise(sub_recu_bcfs_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
##DETR
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1331,1341) & NOMEN %in% c("M14","M14A")) %>% group_by(SIREN) %>% summarise(detr_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# taxe amenage
resultat_ba_m14  <- base_com_BA %>% filter(NOMEN %in% c("M14","M14A") & (compte4 %in% c(1333,1343) | compte5 %in% c(10223,10226))) %>% group_by(SIREN) %>% summarise(taxe_amenag_ba=sum(reelCRE)/1000) %>% full_join(resultat_ba_m14,by="SIREN") %>% replace(is.na(.),0)
resultat_ba_m4  <- base_com_BA %>% filter(!NOMEN %in% c("M14","M14A","M49","M49A") & compte4 %in% c(1333)) %>% group_by(SIREN) %>% summarise(taxe_amenag_ba=sum(reelCRE)/1000) %>% full_join(resultat_ba_m4,by="SIREN") %>% replace(is.na(.),0)
resultat_ba_m49 <- base_com_BA %>% filter(NOMEN %in% c("M49","M49A") & compte4 %in% c(1333) | compte5 %in% c(10226)) %>% group_by(SIREN) %>% summarise(taxe_amenag_ba=sum(reelCRE)/1000) %>% replace(is.na(.),0)

#amende
resultat_ba_m14  <- base_com_BA %>% filter(NOMEN %in% c("M14","M14A") & compte4 %in% c(1332,1342)) %>% group_by(SIREN) %>% summarise(amende_ba=sum(reelCRE)/1000) %>% full_join(resultat_ba_m14,by="SIREN") %>% replace(is.na(.),0)
resultat_ba_m4  <- base_com_BA %>% filter(!NOMEN %in% c("M14","M14A","M49","M49A") & compte4 %in% c(1332)) %>% group_by(SIREN) %>% summarise(amende_ba=sum(reelCRE)/1000) %>% full_join(resultat_ba_m4,by="SIREN") %>% replace(is.na(.),0)
resultat_ba_m49 <- resultat_ba_m49 %>% group_by(SIREN) %>% mutate(amende_ba=0) %>% select(SIREN,amende_ba) %>% full_join(resultat_ba_m49,by="SIREN") %>% replace(is.na(.),0)


resultat_i <- rbind.data.frame(resultat_ba_m14,resultat_ba_m4,resultat_ba_m49)

resultat_i <- resultat_i %>% group_by(SIREN) %>% summarise_all(sum)

resultat_all <- dplyr::left_join(resultat_all,resultat_i,by="SIREN") %>% replace(is.na(.),0)

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
# dep_equip_brut
resultat_all  <- base_com_BA %>% filter(compte2 %in% c(20,21,23) & !compte3 %in% c(204))  %>% group_by(SIREN) %>% summarise(dep_equip_brut_ba=sum(reelDEB)/1000) %>%  full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)


#Flux crois?e
fc <- resultat_all %>% select(SIREN)
fc <- base_com_BP %>% filter(compte6 %in% c(204163,204164))  %>% group_by(SIREN) %>% summarise(fc_sub=sum(reelDEB)/1000) %>%  full_join(fc,by="SIREN") %>% replace(is.na(.),0)
fc  <- base_com_BP %>% filter(compte4 == 6521 | compte5 == 67441 | compte6 %in% c(657363,657364)) %>% group_by(SIREN) %>% summarise(df_bp15 =sum(reelDEB)/1000) %>% full_join(fc,by="SIREN") %>% replace(is.na(.),0)
fc  <- base_com_BA %>% filter(!NOMEN %in% c("M14","M14A") & (compte4 %in% c(6215,6287) | compte3 == 672 )) %>% group_by(SIREN) %>% summarise(df_ba_m415 =sum(reelDEB)/1000) %>% full_join(fc,by="SIREN") %>% replace(is.na(.),0)
fc  <- base_com_BA %>% filter(NOMEN %in% c("M14") & (compte4 %in% c(6215,6522) | compte5 == 62871 | compte6 == 661133)) %>% group_by(SIREN) %>% summarise(df_ba_m1415 =sum(reelDEB)/1000) %>% full_join(fc,by="SIREN") %>% replace(is.na(.),0)
fc  <- base_com_BA %>% filter(NOMEN %in% c("M14A") & (compte4 %in% c(6287,6522))) %>% group_by(SIREN) %>% summarise(df_ba_m14A15 =sum(reelDEB)/1000)  %>% full_join(fc,by="SIREN") %>% replace(is.na(.),0)
fc  <- base_com_BA %>% filter(NOMEN %in% c("M14") & (compte5 %in% c(70871))) %>% group_by(SIREN) %>% summarise(df_ba_m14r15 =sum(reelCRE)/1000) %>%  full_join(fc,by="SIREN") %>% replace(is.na(.),0)

resultat_all  <- fc %>% mutate(fc_f = df_bp15 + df_ba_m415 +df_ba_m14A15  + df_ba_m1415 + df_ba_m14r15, fc_i = fc_sub ) %>% select(SIREN,fc_f,fc_i) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)




#base de donnee en 2016
resultat_16  <- resultat_all
remove(resultat_MGP,resultat_i,resultat_HMGP,rec_invest_MGP,rec_invest_HMGP,dep_invest_MGP,dep_invest_HMGP,rec_fonc,base_com_MGP,base_com_HMGP,base_com_BA,base_com_BP,fc)

gc()
gc()

## Les communes 2015 ====

BalanceCommune_2016 <- read_csv2("C:/Users/cgirard-adc/Desktop/Base comptables/balance_communes/BalanceCommune_2015.csv")

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


base_com_BP  <- base_com[base_com$BUDGET == "BP",]
base_com_BA  <- base_com[base_com$BUDGET == "BA" & !base_com$NOMEN == "M22",]
list  <-base_com_BP %>% group_by(SIREN,LBUDG) %>% summarise(n=n()) %>% select(SIREN,LBUDG)

resultat  <- list %>% select(SIREN,LBUDG)

dep_invest  <- list %>% select(SIREN)

rec_invest  <- list %>% select(SIREN)
resultat_ba_m14  <- base_com_BA %>% filter(NOMEN %in% c("M14","M14A")) %>% select(SIREN) %>%  distinct()
resultat_ba_m4  <- base_com_BA %>% filter(!NOMEN %in% c("M14","M14A")) %>% select(SIREN) %>% distinct()

remove(base_com)
gc()


# Charges de fonctionnement
resultat  <- base_com_BP %>% filter(compte1==6 & !compte2 %in% c(68) & !compte3 %in% c(675,676)) %>% group_by(SIREN) %>% summarise(dep_fonc=sum(reelDEB-reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte2 %in% c(68)) %>% group_by(SIREN) %>% summarise(dot_prov=sum(reelDEB-reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)

# Produits de fonctionnement
resultat  <- base_com_BP %>% filter(compte1==7 & !compte2 %in% c(78) & !compte3 %in% c(775,776,777))  %>% group_by(SIREN) %>% summarise(rec_fonc=sum(reelCRE-reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte2 %in% c(78))  %>% group_by(SIREN) %>% summarise(repri_prov=sum(reelCRE-reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)

# Depenses d'investissement
dep_invest  <- base_com_BP %>% filter((compte2 %in% c(13,20,21,23,26,27) | compte3 %in% c(102,454,456,458,481)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(dep_invest_1=sum(reelDEB)/1000) %>% full_join(dep_invest,by="SIREN") %>% replace(is.na(.),0)
dep_invest <- base_com_BP %>% filter(compte3 %in% c(237,238))  %>% group_by(SIREN) %>% summarise(dep_invest_2=sum(reelCRE)/1000) %>%  full_join(dep_invest,by="SIREN") %>% replace(is.na(.),0)
resultat <- dep_invest %>% mutate(dep_invest=dep_invest_1-dep_invest_2) %>% select(SIREN,dep_invest) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
## compte 21
resultat <- base_com_BP %>% filter(compte2 == 21) %>% group_by(SIREN) %>% summarise(compte21 = sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
#compte 23
resultat <- base_com_BP %>% filter(compte2 == 23) %>% group_by(SIREN) %>% summarise(compte23 = sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
##  Depenses d'équipements
dep_invest  <- base_com_BP %>% filter(compte2 %in% c(20,21,23) & !compte3 %in% c(204))  %>% group_by(SIREN) %>% summarise(dep_invest_3=sum(reelDEB)/1000) %>%  full_join(dep_invest,by="SIREN")  %>% replace(is.na(.),0)
resultat  <- dep_invest %>% mutate(dep_equip=dep_invest_3-dep_invest_2) %>% select(SIREN,dep_equip) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
#terrain
resultat  <- base_com_BP %>% filter(compte3 %in% c(211,212) | compte4 %in% c(2171,2172,2312)) %>% group_by(SIREN) %>% summarise(terrains=sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
#construction
resultat  <- base_com_BP %>% filter(compte3 %in% c(213,214) | compte4 %in% c(2173,2174,2313,2314)) %>% group_by(SIREN) %>% summarise(constructions=sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
# reseau
resultat  <- base_com_BP %>% filter(compte3 %in% c(215) | compte4 %in% c(2175,2315)) %>% group_by(SIREN) %>% summarise(reseaux=sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
#bien meuble
resultat  <- base_com_BP %>% filter(compte3 %in% c(218) | compte4 %in% c(2178,2318)) %>% group_by(SIREN) %>% summarise(bien_meuble=sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)

##  Subventions
resultat  <- base_com_BP %>% filter(compte3 %in% c(204)) %>% group_by(SIREN) %>% summarise(subvention_204=sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte5 %in% c(20412)) %>% group_by(SIREN) %>% summarise(sub_204_reg=sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte5 %in% c(20413)) %>% group_by(SIREN) %>% summarise(sub_204_dep=sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte5 %in% c(20414)) %>% group_by(SIREN) %>% summarise(sub_204_com=sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte6 %in% c(204151)) %>% group_by(SIREN) %>% summarise(sub_204_gfp=sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
#Travaux comptes de tiers
resultat <- base_com_BP %>% filter(compte3 %in% c(454,456,458)) %>% group_by(SIREN) %>% summarise(compte_tier_d = sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
#compte 26
resultat <- base_com_BP %>% filter(compte2 %in% c(26) & !compte3 %in% c(269)) %>% group_by(SIREN) %>% summarise(compte_26_d = sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
#compte 27
resultat <- base_com_BP %>% filter(compte2 %in% c(27) & !compte3 %in% c(279) & !compte4 %in% c(2768)) %>% group_by(SIREN) %>% summarise(compte_27_d = sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)

# Recette d'investissements
rec_invest <- base_com_BP %>% filter((compte2 %in% c(13,20,21,26,27) | compte3 %in% c(102,231,232,454,456,458)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(rec_invest_1=sum(reelCRE)/1000) %>% full_join(rec_invest,by="SIREN") %>% replace(is.na(.),0)
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
## DETR
resultat  <- base_com_BP %>% filter(compte4 %in% c(1331,1341)) %>% group_by(SIREN) %>% summarise(detr=sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
#♠ taxe amenage
resultat  <- base_com_BP %>% filter(compte4 %in% c(1333,1343) | compte5 %in% c(10223,10226)) %>% group_by(SIREN) %>% summarise(taxe_amenag=sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)

## amade police
resultat  <- base_com_BP %>% filter(compte4 %in% c(1332,1342)) %>% group_by(SIREN) %>% summarise(amende=sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
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
resultat <- base_com_BP %>% filter(compte3 %in% c(454,456,458)) %>% group_by(SIREN) %>% summarise(compte_tier_r = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
#compte 26
resultat <- base_com_BP %>% filter(compte2 %in% c(26) & !compte3 %in% c(269)) %>% group_by(SIREN) %>% summarise(compte_26_r = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
#compte 27
resultat <- base_com_BP %>% filter(compte2 %in% c(27) & !compte3 %in% c(279) & !compte4 %in% c(2768)) %>% group_by(SIREN) %>% summarise(compte_27_r = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)

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
# dep_equip_brut
resultat_all  <- base_com_BP %>% filter(compte2 %in% c(20,21,23) & !compte3 %in% c(204))  %>% group_by(SIREN) %>% summarise(dep_equip_brut=sum(reelDEB)/1000) %>%  full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)


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
dep_invest  <- base_com_BA %>% filter((compte2 %in% c(13,20,21,23,26,27) | compte3 %in% c(102,454,456,458,481)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(dep_invest_1=sum(reelDEB)/1000) %>% full_join(dep_invest,by="SIREN") %>% replace(is.na(.),0)
dep_invest <- base_com_BA %>% filter(compte3 %in% c(237,238))  %>% group_by(SIREN) %>% summarise(dep_invest_2=sum(reelCRE)/1000) %>%  full_join(dep_invest,by="SIREN") %>% replace(is.na(.),0)
resultat_all <- dep_invest %>% mutate(dep_invest_ba=dep_invest_1-dep_invest_2) %>% select(SIREN,dep_invest_ba) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# les depenses d'?quip
dep_invest  <- base_com_BA %>% filter(compte2 %in% c(20,21,23) & !compte3 %in% c(204))  %>% group_by(SIREN) %>% summarise(dep_invest_3=sum(reelDEB)/1000) %>%  full_join(dep_invest,by="SIREN")  %>% replace(is.na(.),0)
resultat_all  <- dep_invest %>% mutate(dep_equip_ba=dep_invest_3-dep_invest_2) %>% select(SIREN,dep_equip_ba) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
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
rec_invest  <- base_com_BA %>% filter((compte2 %in% c(13,20,21,26,27) | compte3 %in% c(102,231,232,454,456,458)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(rec_invest_1=sum(reelCRE)/1000) %>% full_join(rec_invest,by="SIREN") %>% replace(is.na(.),0)
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
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1331,1341) & NOMEN %in% c("M14","M14A")) %>% group_by(SIREN) %>% summarise(detr_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# taxe amenage
# taxe amenage
resultat_ba_m14  <- base_com_BA %>% filter(NOMEN %in% c("M14","M14A") & (compte4 %in% c(1333,1343) | compte5 %in% c(10223,10226))) %>% group_by(SIREN) %>% summarise(taxe_amenag_ba=sum(reelCRE)/1000) %>% full_join(resultat_ba_m14,by="SIREN") %>% replace(is.na(.),0)
resultat_ba_m4  <- base_com_BA %>% filter(!NOMEN %in% c("M14","M14A","M49","M49A") & compte4 %in% c(1333)) %>% group_by(SIREN) %>% summarise(taxe_amenag_ba=sum(reelCRE)/1000) %>% full_join(resultat_ba_m4,by="SIREN") %>% replace(is.na(.),0)
resultat_ba_m49 <- base_com_BA %>% filter(NOMEN %in% c("M49","M49A") & compte4 %in% c(1333) | compte5 %in% c(10226)) %>% group_by(SIREN) %>% summarise(taxe_amenag_ba=sum(reelCRE)/1000) %>% replace(is.na(.),0)

#amende
resultat_ba_m14  <- base_com_BA %>% filter(NOMEN %in% c("M14","M14A") & compte4 %in% c(1332,1342)) %>% group_by(SIREN) %>% summarise(amende_ba=sum(reelCRE)/1000) %>% full_join(resultat_ba_m14,by="SIREN") %>% replace(is.na(.),0)
resultat_ba_m4  <- base_com_BA %>% filter(!NOMEN %in% c("M14","M14A","M49","M49A") & compte4 %in% c(1332)) %>% group_by(SIREN) %>% summarise(amende_ba=sum(reelCRE)/1000) %>% full_join(resultat_ba_m4,by="SIREN") %>% replace(is.na(.),0)
resultat_ba_m49 <- resultat_ba_m49 %>% group_by(SIREN) %>% mutate(amende_ba=0) %>% select(SIREN,amende_ba) %>% full_join(resultat_ba_m49,by="SIREN") %>% replace(is.na(.),0)

resultat_i <- rbind.data.frame(resultat_ba_m14,resultat_ba_m4,resultat_ba_m49)

resultat_i <- resultat_i %>% group_by(SIREN) %>% summarise_all(sum)

resultat_all <- dplyr::left_join(resultat_all,resultat_i,by="SIREN") %>% replace(is.na(.),0)


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
# dep_equip_brut
resultat_all  <- base_com_BA %>% filter(compte2 %in% c(20,21,23) & !compte3 %in% c(204))  %>% group_by(SIREN) %>% summarise(dep_equip_brut_ba=sum(reelDEB)/1000) %>%  full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)


#Flux crois?e
fc <- resultat_all %>% select(SIREN)
fc <- base_com_BP %>% filter(compte6 %in% c(204163,204164))  %>% group_by(SIREN) %>% summarise(fc_sub=sum(reelDEB)/1000) %>%  full_join(fc,by="SIREN") %>% replace(is.na(.),0)
fc  <- base_com_BP %>% filter(compte4 == 6521 | compte5 == 67441 | compte6 %in% c(657363,657364)) %>% group_by(SIREN) %>% summarise(df_bp15 =sum(reelDEB)/1000) %>% full_join(fc,by="SIREN") %>% replace(is.na(.),0)
fc  <- base_com_BA %>% filter(!NOMEN %in% c("M14","M14A") & (compte4 %in% c(6215,6287) | compte3 == 672 )) %>% group_by(SIREN) %>% summarise(df_ba_m415 =sum(reelDEB)/1000) %>% full_join(fc,by="SIREN") %>% replace(is.na(.),0)
fc  <- base_com_BA %>% filter(NOMEN %in% c("M14") & (compte4 %in% c(6215,6522) | compte5 == 62871 | compte6 == 661133)) %>% group_by(SIREN) %>% summarise(df_ba_m1415 =sum(reelDEB)/1000) %>% full_join(fc,by="SIREN") %>% replace(is.na(.),0)
fc  <- base_com_BA %>% filter(NOMEN %in% c("M14A") & (compte4 %in% c(6287,6522))) %>% group_by(SIREN) %>% summarise(df_ba_m14A15 =sum(reelDEB)/1000)  %>% full_join(fc,by="SIREN") %>% replace(is.na(.),0)
fc  <- base_com_BA %>% filter(NOMEN %in% c("M14") & (compte5 %in% c(70871))) %>% group_by(SIREN) %>% summarise(df_ba_m14r15 =sum(reelCRE)/1000) %>%  full_join(fc,by="SIREN") %>% replace(is.na(.),0)

resultat_all  <- fc %>% mutate(fc_f = df_bp15 + df_ba_m415 +df_ba_m14A15  + df_ba_m1415 + df_ba_m14r15, fc_i = fc_sub ) %>% select(SIREN,fc_f,fc_i) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)


#base de donnee en 2015
resultat_15  <- resultat_all

remove(resultat,resultat_i,resultat_all,rec_invest,dep_invest,base_com_BP,base_com_BA,fc)

gc()



## Les communes 2014 ====

BalanceCommune_2016 <- read_csv2("C:/Users/cgirard-adc/Desktop/Base comptables/balance_communes/BalanceCommune_2014.csv")

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


base_com_BP  <- base_com[base_com$BUDGET == "BP",]
base_com_BA  <- base_com[base_com$BUDGET == "BA" & !base_com$NOMEN == "M22",]
list  <-base_com_BP %>% group_by(SIREN,LBUDG) %>% summarise(n=n()) %>% select(SIREN,LBUDG)

resultat  <- list %>% select(SIREN,LBUDG)

dep_invest  <- list %>% select(SIREN)

rec_invest  <- list %>% select(SIREN)
resultat_ba_m14  <- base_com_BA %>% filter(NOMEN %in% c("M14","M14A")) %>% select(SIREN) %>%  distinct()
resultat_ba_m4  <- base_com_BA %>% filter(!NOMEN %in% c("M14","M14A")) %>% select(SIREN) %>% distinct()

remove(base_com)
gc()


# Charges de fonctionnement
resultat  <- base_com_BP %>% filter(compte1==6 & !compte2 %in% c(68) & !compte3 %in% c(675,676)) %>% group_by(SIREN) %>% summarise(dep_fonc=sum(reelDEB-reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte2 %in% c(68)) %>% group_by(SIREN) %>% summarise(dot_prov=sum(reelDEB-reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)

# Produits de fonctionnement
resultat  <- base_com_BP %>% filter(compte1==7 & !compte2 %in% c(78) & !compte3 %in% c(775,776,777))  %>% group_by(SIREN) %>% summarise(rec_fonc=sum(reelCRE-reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte2 %in% c(78))  %>% group_by(SIREN) %>% summarise(repri_prov=sum(reelCRE-reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)

# Depenses d'investissement
dep_invest  <- base_com_BP %>% filter((compte2 %in% c(13,20,21,23,26,27) | compte3 %in% c(102,454,456,458,481)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(dep_invest_1=sum(reelDEB)/1000) %>% full_join(dep_invest,by="SIREN") %>% replace(is.na(.),0)
dep_invest <- base_com_BP %>% filter(compte3 %in% c(237,238))  %>% group_by(SIREN) %>% summarise(dep_invest_2=sum(reelCRE)/1000) %>%  full_join(dep_invest,by="SIREN") %>% replace(is.na(.),0)
resultat <- dep_invest %>% mutate(dep_invest=dep_invest_1-dep_invest_2) %>% select(SIREN,dep_invest) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
## compte 21
resultat <- base_com_BP %>% filter(compte2 == 21) %>% group_by(SIREN) %>% summarise(compte21 = sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
#compte 23
resultat <- base_com_BP %>% filter(compte2 == 23) %>% group_by(SIREN) %>% summarise(compte23 = sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
##  Depenses d'équipements
dep_invest  <- base_com_BP %>% filter(compte2 %in% c(20,21,23) & !compte3 %in% c(204))  %>% group_by(SIREN) %>% summarise(dep_invest_3=sum(reelDEB)/1000) %>%  full_join(dep_invest,by="SIREN")  %>% replace(is.na(.),0)
resultat  <- dep_invest %>% mutate(dep_equip=dep_invest_3-dep_invest_2) %>% select(SIREN,dep_equip) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
#terrain
resultat  <- base_com_BP %>% filter(compte3 %in% c(211,212) | compte4 %in% c(2171,2172,2312)) %>% group_by(SIREN) %>% summarise(terrains=sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
#construction
resultat  <- base_com_BP %>% filter(compte3 %in% c(213,214) | compte4 %in% c(2173,2174,2313,2314)) %>% group_by(SIREN) %>% summarise(constructions=sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
# reseau
resultat  <- base_com_BP %>% filter(compte3 %in% c(215) | compte4 %in% c(2175,2315)) %>% group_by(SIREN) %>% summarise(reseaux=sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
#bien meuble
resultat  <- base_com_BP %>% filter(compte3 %in% c(218) | compte4 %in% c(2178,2318)) %>% group_by(SIREN) %>% summarise(bien_meuble=sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)

##  Subventions
resultat  <- base_com_BP %>% filter(compte3 %in% c(204)) %>% group_by(SIREN) %>% summarise(subvention_204=sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte5 %in% c(20412)) %>% group_by(SIREN) %>% summarise(sub_204_reg=sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte5 %in% c(20413)) %>% group_by(SIREN) %>% summarise(sub_204_dep=sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte5 %in% c(20414)) %>% group_by(SIREN) %>% summarise(sub_204_com=sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
resultat  <- base_com_BP %>% filter(compte6 %in% c(204151)) %>% group_by(SIREN) %>% summarise(sub_204_gfp=sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
#Travaux comptes de tiers
resultat <- base_com_BP %>% filter(compte3 %in% c(454,456,458)) %>% group_by(SIREN) %>% summarise(compte_tier_d = sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
#compte 26
resultat <- base_com_BP %>% filter(compte2 %in% c(26) & !compte3 %in% c(269)) %>% group_by(SIREN) %>% summarise(compte_26_d = sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
#compte 27
resultat <- base_com_BP %>% filter(compte2 %in% c(27) & !compte3 %in% c(279) & !compte4 %in% c(2768)) %>% group_by(SIREN) %>% summarise(compte_27_d = sum(reelDEB)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)

# Recette d'investissements
rec_invest <- base_com_BP %>% filter((compte2 %in% c(13,20,21,26,27) | compte3 %in% c(102,231,232,454,456,458)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(rec_invest_1=sum(reelCRE)/1000) %>% full_join(rec_invest,by="SIREN") %>% replace(is.na(.),0)
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
## DETR
resultat  <- base_com_BP %>% filter(compte4 %in% c(1331,1341)) %>% group_by(SIREN) %>% summarise(detr=sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
#♠ taxe amenage
resultat  <- base_com_BP %>% filter(compte4 %in% c(1333,1343) | compte5 %in% c(10223,10226)) %>% group_by(SIREN) %>% summarise(taxe_amenag=sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)

## amade police
resultat  <- base_com_BP %>% filter(compte4 %in% c(1332,1342)) %>% group_by(SIREN) %>% summarise(amende=sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN") %>% replace(is.na(.),0)
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
resultat <- base_com_BP %>% filter(compte3 %in% c(454,456,458)) %>% group_by(SIREN) %>% summarise(compte_tier_r = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
#compte 26
resultat <- base_com_BP %>% filter(compte2 %in% c(26) & !compte3 %in% c(269)) %>% group_by(SIREN) %>% summarise(compte_26_r = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)
#compte 27
resultat <- base_com_BP %>% filter(compte2 %in% c(27) & !compte3 %in% c(279) & !compte4 %in% c(2768)) %>% group_by(SIREN) %>% summarise(compte_27_r = sum(reelCRE)/1000) %>% full_join(resultat,by="SIREN")  %>% replace(is.na(.),0)

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
# dep_equip_brut
resultat_all  <- base_com_BP %>% filter(compte2 %in% c(20,21,23) & !compte3 %in% c(204))  %>% group_by(SIREN) %>% summarise(dep_equip_brut=sum(reelDEB)/1000) %>%  full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)


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
dep_invest  <- base_com_BA %>% filter((compte2 %in% c(13,20,21,23,26,27) | compte3 %in% c(102,454,456,458,481)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(dep_invest_1=sum(reelDEB)/1000) %>% full_join(dep_invest,by="SIREN") %>% replace(is.na(.),0)
dep_invest <- base_com_BA %>% filter(compte3 %in% c(237,238))  %>% group_by(SIREN) %>% summarise(dep_invest_2=sum(reelCRE)/1000) %>%  full_join(dep_invest,by="SIREN") %>% replace(is.na(.),0)
resultat_all <- dep_invest %>% mutate(dep_invest_ba=dep_invest_1-dep_invest_2) %>% select(SIREN,dep_invest_ba) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# les depenses d'?quip
dep_invest  <- base_com_BA %>% filter(compte2 %in% c(20,21,23) & !compte3 %in% c(204))  %>% group_by(SIREN) %>% summarise(dep_invest_3=sum(reelDEB)/1000) %>%  full_join(dep_invest,by="SIREN")  %>% replace(is.na(.),0)
resultat_all  <- dep_invest %>% mutate(dep_equip_ba=dep_invest_3-dep_invest_2) %>% select(SIREN,dep_equip_ba) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
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
rec_invest  <- base_com_BA %>% filter((compte2 %in% c(13,20,21,26,27) | compte3 %in% c(102,231,232,454,456,458)) & !compte3 %in% c(139,269,279) & !compte4 %in% c(1027,2768) & !compte5 %in% c(10229))  %>% group_by(SIREN) %>% summarise(rec_invest_1=sum(reelCRE)/1000) %>% full_join(rec_invest,by="SIREN") %>% replace(is.na(.),0)
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
resultat_all  <- base_com_BA %>% filter(compte4 %in% c(1331,1341) & NOMEN %in% c("M14","M14A")) %>% group_by(SIREN) %>% summarise(detr_ba=sum(reelCRE)/1000) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)
# taxe amenage
resultat_ba_m14  <- base_com_BA %>% filter(NOMEN %in% c("M14","M14A") & (compte4 %in% c(1333,1343) | compte5 %in% c(10223,10226))) %>% group_by(SIREN) %>% summarise(taxe_amenag_ba=sum(reelCRE)/1000) %>% full_join(resultat_ba_m14,by="SIREN") %>% replace(is.na(.),0)
resultat_ba_m4  <- base_com_BA %>% filter(!NOMEN %in% c("M14","M14A","M49","M49A") & compte4 %in% c(1333)) %>% group_by(SIREN) %>% summarise(taxe_amenag_ba=sum(reelCRE)/1000) %>% full_join(resultat_ba_m4,by="SIREN") %>% replace(is.na(.),0)
resultat_ba_m49 <- base_com_BA %>% filter(NOMEN %in% c("M49","M49A") & compte4 %in% c(1333) | compte5 %in% c(10226)) %>% group_by(SIREN) %>% summarise(taxe_amenag_ba=sum(reelCRE)/1000) %>% replace(is.na(.),0)

#amende
resultat_ba_m14  <- base_com_BA %>% filter(NOMEN %in% c("M14","M14A") & compte4 %in% c(1332,1342)) %>% group_by(SIREN) %>% summarise(amende_ba=sum(reelCRE)/1000) %>% full_join(resultat_ba_m14,by="SIREN") %>% replace(is.na(.),0)
resultat_ba_m4  <- base_com_BA %>% filter(!NOMEN %in% c("M14","M14A","M49","M49A") & compte4 %in% c(1332)) %>% group_by(SIREN) %>% summarise(amende_ba=sum(reelCRE)/1000) %>% full_join(resultat_ba_m4,by="SIREN") %>% replace(is.na(.),0)
resultat_ba_m49 <- resultat_ba_m49 %>% group_by(SIREN) %>% mutate(amende_ba=0) %>% select(SIREN,amende_ba) %>% full_join(resultat_ba_m49,by="SIREN") %>% replace(is.na(.),0)

resultat_i <- rbind.data.frame(resultat_ba_m14,resultat_ba_m4,resultat_ba_m49)

resultat_i <- resultat_i %>% group_by(SIREN) %>% summarise_all(sum)

resultat_all <- dplyr::left_join(resultat_all,resultat_i,by="SIREN") %>% replace(is.na(.),0)



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
# dep_equip_brut
resultat_all  <- base_com_BA %>% filter(compte2 %in% c(20,21,23) & !compte3 %in% c(204))  %>% group_by(SIREN) %>% summarise(dep_equip_brut_ba=sum(reelDEB)/1000) %>%  full_join(resultat_all,by="SIREN")  %>% replace(is.na(.),0)


#Flux crois?e
fc <- resultat_all %>% select(SIREN)
fc <- base_com_BP %>% filter(compte6 %in% c(204163,204164))  %>% group_by(SIREN) %>% summarise(fc_sub=sum(reelDEB)/1000) %>%  full_join(fc,by="SIREN") %>% replace(is.na(.),0)
fc  <- base_com_BP %>% filter(compte4 == 6521 | compte5 == 67441 | compte6 %in% c(657363,657364)) %>% group_by(SIREN) %>% summarise(df_bp15 =sum(reelDEB)/1000) %>% full_join(fc,by="SIREN") %>% replace(is.na(.),0)
fc  <- base_com_BA %>% filter(!NOMEN %in% c("M14","M14A") & (compte4 %in% c(6215,6287) | compte3 == 672 )) %>% group_by(SIREN) %>% summarise(df_ba_m415 =sum(reelDEB)/1000) %>% full_join(fc,by="SIREN") %>% replace(is.na(.),0)
fc  <- base_com_BA %>% filter(NOMEN %in% c("M14") & (compte4 %in% c(6215,6522) | compte5 == 62871 | compte6 == 661133)) %>% group_by(SIREN) %>% summarise(df_ba_m1415 =sum(reelDEB)/1000) %>% full_join(fc,by="SIREN") %>% replace(is.na(.),0)
fc  <- base_com_BA %>% filter(NOMEN %in% c("M14A") & (compte4 %in% c(6287,6522))) %>% group_by(SIREN) %>% summarise(df_ba_m14A15 =sum(reelDEB)/1000)  %>% full_join(fc,by="SIREN") %>% replace(is.na(.),0)
fc  <- base_com_BA %>% filter(NOMEN %in% c("M14") & (compte5 %in% c(70871))) %>% group_by(SIREN) %>% summarise(df_ba_m14r15 =sum(reelCRE)/1000) %>%  full_join(fc,by="SIREN") %>% replace(is.na(.),0)

resultat_all  <- fc %>% mutate(fc_f = df_bp15 + df_ba_m415 +df_ba_m14A15  + df_ba_m1415 + df_ba_m14r15, fc_i = fc_sub ) %>% select(SIREN,fc_f,fc_i) %>% full_join(resultat_all,by="SIREN") %>% replace(is.na(.),0)


#base de donnee en 2014
resultat_14  <- resultat_all

remove(resultat,resultat_i,resultat_all,rec_invest,dep_invest,base_com_BP,base_com_BA,fc)

gc()

resultat_14$annee = 2014
resultat_15$annee = 2015
resultat_16$annee = 2016

resultat <- rbind(resultat_14,resultat_15,resultat_16)

table <- read_csv2("Z:/BDD et outils/Bases de donnees/bd communes/com_nouvelles/table_14_15_16.csv")

table14 <- table %>% select(code_insee14,SIREN_14,code_insee16) %>% distinct()

resultat14 <- resultat %>% filter(annee == 2014)
resultat14 <- resultat14 %>% rename(SIREN_14 = SIREN)
resultat14 <- left_join(resultat14,table14,by="SIREN_14")

table15 <- table %>% select(code_insee15,SIREN_15,code_insee16) %>% distinct()
resultat15 <- resultat %>% filter(annee == 2015)
resultat15 <- resultat15 %>% rename(SIREN_15 = SIREN)
resultat15 <- left_join(resultat15,table15,by="SIREN_15")

resultat16 <- resultat %>% filter(annee == 2016)
table16 <- table %>% select(SIREN_16,code_insee16) %>% distinct()
resultat16 <- resultat16 %>% rename(SIREN_16 = SIREN)
resultat16 <- left_join(resultat16,table16,by="SIREN_16")

resultat16$code_insee = resultat16$code_insee16


resultat16 <- resultat16 %>% rename(code_insee = code_insee16,code_insee16=code_insee)
resultat14 <- resultat14 %>% rename(code_insee = code_insee14)
resultat15 <- resultat15 %>% rename(code_insee = code_insee15)


resultat16 <- resultat16 %>% rename(SIREN = SIREN_16)
resultat14 <- resultat14 %>% rename(SIREN = SIREN_14)
resultat15 <- resultat15 %>% rename(SIREN = SIREN_15)
resultat <- rbind(resultat14,resultat15,resultat16)

resultat <- resultat %>% distinct()

write.csv2(resultat,"resultat_com.csv")

resultat <- read.csv2("resultat_com.csv")

pop <- read_tsv("Z:/BDD et outils/Bases de donnees/general/population/pop_com_10_17_codeinsee.txt")

pop <-  pop %>%
  select(code_insee,pop_14,pop_15,pop_16)

pop <-  pop %>%
  dplyr::rename("2014" = pop_14, "2015" = pop_15,"2016"=pop_16)

pop <- pop %>%
  gather(key = "annee",value = "population",2:4)

pop <- pop %>%
  distinct()
pop <- pop %>%
  filter(!is.na(population))
pop$annee <- as.integer(pop$annee)

resultat <- resultat %>% left_join(pop,by=c("code_insee","annee"))

resultat <- resultat %>%
  filter(!is.na(population))
test <- read.csv2("inst/mon_app/bdd/montagne_touristique.csv")

resultat <- resultat[,-1]

resultat <- rbind(resultat,test)

resultat <- resultat %>% distinct()

resultat_t <- resultat %>%
  group_by(code_insee16,annee) %>%
  select_if(is.numeric) %>%
  summarise_all(all_vars(sum))

base <- base %>%
  select(code_insee16,annee,DEP,SIREN,LBUDG,code_dep,nom_dep,nom_reg,strate16,nom_com)

resultat_t <- resultat_t %>%
  left_join(base,by=c("code_insee16","annee"))

resultat_t <- resultat_t %>% distinct()

test <- resultat_t %>%
  filter(code_insee16 %in% c(25576))

write.csv2(base_invest_com,"inst/mon_app/bdd/base_invest_com.csv",row.names = FALSE)

base_invest_com <- appliInvest::base_invest_com

base_invest_com <- as.data.frame(base_invest_com)

base_invest_com <- base_invest_com %>% left_join(test,by=c("code_insee16","annee"))

devtools::use_data(base_invest_com,pkg = ".",overwrite = T)

base_invest_com <- base_invest_com %>% distinct()

resultat <- resultat %>%
  select(code_insee16,annee,amende_ba,taxe_amenag_ba) %>% group_by(code_insee16,annee) %>% summarise(amende_ba = sum(amende_ba),taxe_amenag_ba=sum(taxe_amenag_ba))


base_invest_com <-  base_invest_com %>%
  select(-amende_ba,-taxe_amenag_ba)

base_invest_com <- base_invest_com %>%
  left_join(resultat,by=c("code_insee16","annee"))
