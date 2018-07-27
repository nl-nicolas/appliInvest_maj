##### Bonus si problème avec les factor!!!!


base_invest <-  appliInvest::base_invest_com
base_invest$code_insee16 <- as.character(base_invest$code_insee16)
base_invest$rec_invest <- as.numeric(base_invest$rec_invest)
base_invest$LBUDG <- as.character(base_invest$LBUDG)

base_invest_gfp$nom_dep <- as.character(base_invest_gfp$nom_dep)
base_invest_gfp$strate16 <- as.character(base_invest_gfp$strate16)





#### PArtie de sdep =====
base_invest_dep <- appliInvest::base_invest_dep
base_invest_dep$nom_dep <- as.character(base_invest_dep$nom_dep)
base_invest_dep$population <-as.integer(str_replace_all(base_invest_dep$population," ",""))

base_invest_dep2 <- base_invest_dep %>%
  rename(strate16 = strate_2017) %>% 
  select(SIREN,annee,nom_dep,strate16,population,"LBUDG",
         dep_invest,dep_invest_ba,
         rec_invest,rec_invest_ba,
         dep_equip,dep_equip_ba,
         dep_equip_brut,dep_equip_brut_ba,
         remboursement,remboursement_ba,
         subvention_204,subvention_204_ba,
         sub_204_etat,sub_204_bc,sub_204_autres,sub_204_prive,
         emprunt,emprunt_ba,
         dep_fonc,dep_fonc_ba,
         rec_fonc,rec_fonc_ba,
         dette,dette_ba,
         fc_i,fc_f,
         terrains,terrains_ba,
         constructions,constructions_ba,
         reseaux,reseaux_ba,
         bien_meuble,bien_meuble_ba,
         taxe_amenag,taxe_amenag_ba,
         compte13_10,compte13_10_ba,
         amende,amende_ba,
         FCTVA,FCTVA_ba,
         planFCTVA_r,planFCTVA_d,planFCTVA_r_ba,planFCTVA_d_ba,
         cessions,cessions_ba,
         urb_rur_2017) %>%
  mutate(epn=rec_fonc - dep_fonc - remboursement,
         epn_ba = rec_fonc_ba -dep_fonc_ba -remboursement_ba,
         rec_invest_hors_planFCTVA = rec_invest + planFCTVA_d - planFCTVA_r,rec_invest_hors_planFCTVA_ba = rec_invest_ba + planFCTVA_d_ba - planFCTVA_r_ba,plan_relance_FCTVA_net = planFCTVA_r - planFCTVA_d,plan_relance_FCTVA_net_ba = planFCTVA_r_ba - planFCTVA_d_ba,
         remb_avance = dep_equip - dep_equip_brut,remb_avance_ba = dep_equip_ba - dep_equip_brut_ba,
         sub_dot_ssTAAm = (compte13_10 - taxe_amenag - amende),
         sub_dot_ssTAAm_ba = (compte13_10_ba - taxe_amenag_ba - amende_ba),
         autres_dep_equip = dep_equip - terrains - constructions - reseaux - bien_meuble,
         autres_dep_equip_ba = dep_equip_ba - terrains_ba - constructions_ba - reseaux_ba - bien_meuble_ba,
         autres_rec_invest = rec_invest - taxe_amenag - sub_dot_ssTAAm - amende - (FCTVA - planFCTVA_d  + planFCTVA_r) - cessions,
         autres_rec_invest_ba = rec_invest_ba - taxe_amenag_ba - sub_dot_ssTAAm_ba - amende_ba  - FCTVA_ba - cessions_ba,
         autres_rec_invest_hors_planrelance = rec_invest - taxe_amenag - sub_dot_ssTAAm - amende - FCTVA - cessions - planFCTVA_r + planFCTVA_d,
         autres_rec_invest_hors_planrelance_ba = rec_invest_ba - taxe_amenag_ba - sub_dot_ssTAAm_ba - amende_ba  - FCTVA_ba - cessions_ba - planFCTVA_r_ba + planFCTVA_d_ba,
         autre_dep_invest = dep_invest - dep_equip - subvention_204,
         autre_dep_invest_ba = dep_invest_ba - dep_equip_ba - subvention_204_ba,
         autre_sub_204 = subvention_204 - sub_204_etat -sub_204_bc - sub_204_autres - sub_204_prive,
         var_fon_roul = rec_invest + rec_fonc + emprunt - dep_fonc - dep_invest - remboursement,
         var_fon_roul_ba = rec_invest_ba + rec_fonc_ba + emprunt_ba - dep_fonc_ba - dep_invest_ba - remboursement_ba)
##### Partie des GFP ======
base_invest_gfp <- base_invest_gfp %>%
  rename(LBUDG = "LBUDG.x",dep_equip_brut = dep_equip_brute,dep_equip_brut_ba = dep_equip_brute_ba,nj_epci2016 = nature_juridique, fisc_epci2016 = fiscalite,siren_epci = SIREN,dep_equip_com_ba = dep_equip_ba_comm) %>% 
  filter(!is.na(population)) %>%
  select(siren_epci,annee,nom_dep,strate16,"LBUDG",
         population,"nj_epci2016","fisc_epci2016",
         dep_invest,dep_invest_ba,
         rec_invest,rec_invest_ba,
         dep_equip,dep_equip_ba,
         dep_equip_brut,dep_equip_brut_ba,
         remboursement,remboursement_ba,
         emprunt,emprunt_ba,
         dep_fonc,dep_fonc_ba,
         rec_fonc,rec_fonc_ba,
         subvention_204,subvention_204_ba,
         dette,dette_ba,
         fc_i,fc_f,
         terrains,terrains_ba,
         constructions,constructions_ba,
         reseaux,reseaux_ba,
         bien_meuble,bien_meuble_ba,
         taxe_amenag,taxe_amenag_ba,
         compte13_10,compte13_10_ba,
         amende,amende_ba,
         FCTVA,FCTVA_ba,
         planFCTVA_r,planFCTVA_d,planFCTVA_r_ba,planFCTVA_d_ba,
         cessions,cessions_ba,
         dep_equip_com,dep_equip_com_ba) %>%
  mutate(epn=rec_fonc - dep_fonc - remboursement,
         rec_invest_hors_planFCTVA = rec_invest + planFCTVA_d - planFCTVA_r,rec_invest_hors_planFCTVA_ba = rec_invest_ba + planFCTVA_d_ba - planFCTVA_r_ba,plan_relance_FCTVA_net = planFCTVA_r - planFCTVA_d,plan_relance_FCTVA_net_ba = planFCTVA_r_ba - planFCTVA_d_ba,
         epn_ba = rec_fonc_ba -dep_fonc_ba -remboursement_ba,
         sub_dot_ssTAAm = (compte13_10 - taxe_amenag - amende),
         sub_dot_ssTAAm_ba = (compte13_10_ba - taxe_amenag_ba - amende_ba),
         autres_dep_equip = dep_equip_brut - terrains - constructions - reseaux - bien_meuble,
         autres_dep_equip_ba = dep_equip_brut_ba - terrains_ba - constructions_ba - reseaux_ba - bien_meuble_ba,
         autres_rec_invest = rec_invest - taxe_amenag - sub_dot_ssTAAm - amende - FCTVA - cessions,
         autres_rec_invest_ba = rec_invest_ba - taxe_amenag_ba - sub_dot_ssTAAm_ba - amende_ba  - FCTVA_ba - cessions_ba,
         autres_rec_invest_hors_planrelance = rec_invest - taxe_amenag - sub_dot_ssTAAm - amende - FCTVA - cessions - planFCTVA_r + planFCTVA_d,
         autres_rec_invest_hors_planrelance_ba = rec_invest_ba - taxe_amenag_ba - sub_dot_ssTAAm_ba - amende_ba  - FCTVA_ba - cessions_ba - planFCTVA_r_ba + planFCTVA_d_ba)

base_invest_gfp <- base_invest_gfp %>%
  mutate(remb_avance = dep_equip - dep_equip_brut,remb_avance_ba = dep_equip_ba - dep_equip_brut_ba)



###### Partie des commmunes====

base_invest <- base_invest_com %>%
  filter(population != 0) %>%
  rename(code_insee16 = code_insee17,annee = "annee.x",strate16 = strate,nom_complet = "LBUDG.x") %>% 
  select(code_insee16,annee,nom_dep,strate16,LBUDG,nom_com,
         population,montagne,touristique, 
         dep_invest,dep_invest_ba,
         rec_invest,rec_invest_ba,
         dep_equip,dep_equip_ba,
         dep_equip_brut,dep_equip_brut_ba,
         remboursement,remboursement_ba,
         emprunt,emprunt_ba,
         dep_fonc,dep_fonc_ba,
         rec_fonc,rec_fonc_ba,
         dette,dette_ba,
         fc_i,fc_f,
         terrains,terrains_ba,
         constructions,constructions_ba,
         reseaux,reseaux_ba,
         bien_meuble,bien_meuble_ba,
         taxe_amenag,taxe_amenag_ba,
         compte13_10,compte13_10_ba,
         amende,amende_ba,
         FCTVA,FCTVA_ba,
         planFCTVA_r,planFCTVA_d,planFCTVA_r_ba,planFCTVA_d_ba,
         cessions,cessions_ba,subvention_204,subvention_204_ba,siren_epci,nom_complet,
         indice_pop) %>%
  mutate(epn=rec_fonc - dep_fonc - remboursement,
         epn_ba = rec_fonc_ba -dep_fonc_ba -remboursement_ba,
         rec_invest_hors_planFCTVA = rec_invest + planFCTVA_d - planFCTVA_r,rec_invest_hors_planFCTVA_ba = rec_invest_ba + planFCTVA_d_ba - planFCTVA_r_ba,plan_relance_FCTVA_net = planFCTVA_r - planFCTVA_d,plan_relance_FCTVA_net_ba = planFCTVA_r_ba - planFCTVA_d_ba,
         remb_avance = dep_equip - dep_equip_brut,remb_avance_ba = dep_equip_ba - dep_equip_brut_ba,
         sub_dot_ssTAAm = (compte13_10 - taxe_amenag - amende),
         sub_dot_ssTAAm_ba = (compte13_10_ba - taxe_amenag_ba - amende_ba),
         autres_dep_equip = dep_equip_brut - terrains - constructions - reseaux - bien_meuble,
         autres_dep_equip_ba = dep_equip_brut_ba - terrains_ba - constructions_ba - reseaux_ba - bien_meuble_ba,
         autres_rec_invest = rec_invest - taxe_amenag - sub_dot_ssTAAm - amende - FCTVA  - cessions,
         autres_rec_invest_ba = rec_invest_ba - taxe_amenag_ba - sub_dot_ssTAAm_ba - amende_ba  - FCTVA_ba - cessions_ba,
         autres_rec_invest_hors_planrelance = rec_invest - taxe_amenag - sub_dot_ssTAAm - amende - FCTVA - cessions - planFCTVA_r + planFCTVA_d,
         autres_rec_invest_hors_planrelance_ba = rec_invest_ba - taxe_amenag_ba - sub_dot_ssTAAm_ba - amende_ba  - FCTVA_ba - cessions_ba - planFCTVA_r_ba + planFCTVA_d_ba,
         var_fon_roul = rec_invest + rec_fonc + emprunt - dep_fonc - dep_invest - remboursement,
         var_fon_roul_ba = rec_invest_ba + rec_fonc_ba + emprunt_ba - dep_fonc_ba - dep_invest_ba - remboursement_ba)




