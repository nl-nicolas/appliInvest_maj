#' Information sur dep_equip
#'
#' Permet d'obtenir un tableau pour avoir des inforamtions de dispersions sur les dep_equip
#'
#' @param echantillon bdd de l'echantillon
#' @param gfp_ou_com choisir 1 pour com 2 pour gfp
#'
#' @import dplyr
#' @import tidyr
#' @importFrom  magrittr %>% 
#' @importFrom stats quantile
#' @importFrom stats median
#'
#' @export

data_stat_echan <- function(echantillon,gfp_ou_com){

if(gfp_ou_com == 2){
  data_2016_moy <- echantillon %>%
    filter(annee == annee_encours) %>%
    summarise("Budgets principaux (BP)" = round(sum(dep_equip)/sum(population)*1000,0),"Budgets annexes (BA)"= round(sum(dep_equip_ba)/sum(population)*1000,0),"BP et BA"= round(sum(dep_equip + dep_equip_ba)/sum(population)*1000,0)) %>%
    mutate(variable = "Moyenne") %>%
    gather("nom","chiffre",1:3)
  resultat_moy <- rbind(data_2016_moy)


  data_2016 <- echantillon %>%
    filter(annee == annee_encours) %>%
    mutate(dep_equip1 = dep_equip/population*1000,dep_equip_ba1 = dep_equip_ba/population*1000,dep_equip_bp_ba = (dep_equip + dep_equip_ba)/population*1000)

  data_2016_med <- data_2016 %>%
    summarise("Budgets principaux (BP)" = round(median(dep_equip1),0),"Budgets annexes (BA)" = round(median(dep_equip_ba1),0),"BP et BA" = round(median(dep_equip_bp_ba),0)) %>%
    mutate(variable = "Médiane") %>%
    gather("nom","chiffre",1:3)
  resultat_med <- rbind(data_2016_med)


  data_2016_q25 <- data_2016 %>%
    summarise("Budgets principaux (BP)" = round(quantile(dep_equip1,0.25),0),"Budgets annexes (BA)" = round(quantile(dep_equip_ba1,0.25),0),"BP et BA" = round(quantile(dep_equip_bp_ba,0.25),0))%>%
    mutate(variable = "1er Quartile") %>%
    gather("nom","chiffre",1:3)
  resultat_q25 <- rbind(data_2016_q25)


  data_2016_q75 <- data_2016 %>%
    summarise("Budgets principaux (BP)" = round(quantile(dep_equip1,0.75),0),"Budgets annexes (BA)" = round(quantile(dep_equip_ba1,0.75),0),"BP et BA" = round(quantile(dep_equip_bp_ba,0.75),0))%>%
    mutate(variable = "3ème Quartile") %>%
    gather("nom","chiffre",1:3)
  resultat_q75 <- rbind(data_2016_q75)


  resultat <- rbind(resultat_moy,resultat_med,resultat_q25,resultat_q75)

  resultat <- resultat %>% spread(key = nom , value = chiffre) %>% select(variable,"BP et BA","Budgets principaux (BP)","Budgets annexes (BA)")

}else if(gfp_ou_com == 1){
  data_2016_moy <- echantillon %>%
    filter(annee == annee_encours) %>%
    summarise("Budgets principaux (BP)" = round(sum(dep_equip)/sum(population)*1000,0),"Budgets annexes (BA)"= round(sum(dep_equip_ba)/sum(population)*1000,0),"BP et BA"= round(sum(dep_equip + dep_equip_ba)/sum(population)*1000,0)) %>%
    mutate(Annee = 2017, variable = "Moyenne") %>%
    gather("nom","chiffre",1:3)
  data_14_16_moy <- echantillon %>%
    filter( !is.na(population)) %>%
    summarise("Budgets principaux (BP)" = round(sum(dep_equip)/sum(population)*1000,0),"Budgets annexes (BA)"= round(sum(dep_equip_ba)/sum(population)*1000,0),"BP et BA"= round(sum(dep_equip + dep_equip_ba)/sum(population)*1000,0))%>%
    mutate(Annee = "2014 à 2017", variable = "Moyenne") %>%
    gather("nom","chiffre",1:3)
  resultat_moy <- rbind(data_2016_moy,data_14_16_moy)


  data_2016 <- echantillon %>%
    filter(annee == annee_encours) %>%
    mutate(dep_equip1 = dep_equip/population*1000,dep_equip_ba1 = dep_equip_ba/population*1000,dep_equip_bp_ba = (dep_equip + dep_equip_ba)/population*1000)
  data_14_16 <- echantillon %>%
    group_by(nom_com) %>% 
    summarise(dep_equip1 = sum(dep_equip)/sum(population)*1000,dep_equip_ba1 = sum(dep_equip_ba)/sum(population)*1000,dep_equip_bp_ba = (sum(dep_equip + dep_equip_ba))/sum(population)*1000)

  data_2016_med <- data_2016 %>%
    summarise("Budgets principaux (BP)" = round(median(dep_equip1),0),"Budgets annexes (BA)" = round(median(dep_equip_ba1),0),"BP et BA" = round(median(dep_equip_bp_ba),0))%>%
    mutate(Annee = 2017, variable = "Médiane") %>%
    gather("nom","chiffre",1:3)
  data_14_16_med <- data_14_16 %>%
    summarise("Budgets principaux (BP)" = round(median(dep_equip1),0),"Budgets annexes (BA)" = round(median(dep_equip_ba1),0),"BP et BA" = round(median(dep_equip_bp_ba),0)) %>%
    mutate(Annee = "2014 à 2017", variable = "Médiane") %>%
    gather("nom","chiffre",1:3)
  resultat_med <- rbind(data_2016_med,data_14_16_med)


  data_2016_q25 <- data_2016 %>%
    summarise("Budgets principaux (BP)" = round(quantile(dep_equip1,0.25),0),"Budgets annexes (BA)" = round(quantile(dep_equip_ba1,0.25),0),"BP et BA" = round(quantile(dep_equip_bp_ba,0.25),0)) %>%
    mutate(Annee = 2017, variable = "1er Quartile") %>%
    gather("nom","chiffre",1:3)
  data_14_16_q25 <- data_14_16 %>%
    summarise("Budgets principaux (BP)" = round(quantile(dep_equip1,0.25),0),"Budgets annexes (BA)" = round(quantile(dep_equip_ba1,0.25),0),"BP et BA" = round(quantile(dep_equip_bp_ba,0.25),0)) %>%
    mutate(Annee = "2014 à 2017", variable = "1er Quartile") %>%
    gather("nom","chiffre",1:3)
  resultat_q25 <- rbind(data_2016_q25,data_14_16_q25)


  data_2016_q75 <- data_2016 %>%
    summarise("Budgets principaux (BP)" = round(quantile(dep_equip1,0.75),0),"Budgets annexes (BA)" = round(quantile(dep_equip_ba1,0.75),0),"BP et BA" = round(quantile(dep_equip_bp_ba,0.75),0)) %>%
    mutate(Annee = 2017, variable = "3ème Quartile") %>%
    gather("nom","chiffre",1:3)
  data_14_16_q75 <- data_14_16 %>%
    summarise("Budgets principaux (BP)" = round(quantile(dep_equip1,0.75),0),"Budgets annexes (BA)" = round(quantile(dep_equip_ba1,0.75),0),"BP et BA" = round(quantile(dep_equip_bp_ba,0.75),0)) %>%
    mutate(Annee = "2014 à 2017", variable = "3ème Quartile") %>%
    gather("nom","chiffre",1:3)
  resultat_q75 <- rbind(data_2016_q75,data_14_16_q75)


  resultat <- rbind(resultat_moy,resultat_med,resultat_q25,resultat_q75)
  resultat <- resultat %>% spread(key = nom , value = chiffre) %>% select(Annee,variable,"BP et BA","Budgets principaux (BP)","Budgets annexes (BA)")
}


  return(resultat)
}
