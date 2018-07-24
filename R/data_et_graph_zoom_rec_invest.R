#' Data zoom sur Rec Invest
#'
#' Permet d'obtenir la base pour le graph sur les rec_invest
#'
#' @param echantillon bdd de l'echantillon
#' @param entite bdd de l'entite selectionnee
#' @param gr_de_ref bdd du groupe de ref de l'ofgl
#' @param nom_gfp_com "Commune" ou "Groupement" etc...
#' @param bp_ba selectionne le bon calcul entre BP Ba ou BP + BA
#'
#' @import dplyr
#' @import tidyr
#' @importFrom  magrittr %>% 
#'
#' @export

data_zoom_rec_invest <- function(echantillon,entite,gr_de_ref,bp_ba,nom_gfp_com){
  if(bp_ba == 1){
    base <- echantillon %>%
      filter(annee == annee_encours) %>%
      select(rec_invest,FCTVA,cessions,sub_dot_ssTAAm,amende,taxe_amenag,autres_rec_invest,planFCTVA_r,planFCTVA_d) %>%
      summarise_all(sum) %>%
      mutate(autres_rec_invest = autres_rec_invest - planFCTVA_r + planFCTVA_d , rec_invest = rec_invest - planFCTVA_r + planFCTVA_d) %>%
      mutate(FCTVA = FCTVA/rec_invest,cessions = cessions/rec_invest,sub_dot_ssTAAm = sub_dot_ssTAAm/rec_invest,amende = amende/rec_invest,taxe_amenag = taxe_amenag/rec_invest,autres_rec_invest = autres_rec_invest/rec_invest) %>%
      gather("variables","montant",2:7) %>%
      mutate(rec_invest = "Echantillon")
    base2 <- entite %>%
      filter(annee == annee_encours) %>%
      select(rec_invest,FCTVA,cessions,sub_dot_ssTAAm,amende,taxe_amenag,autres_rec_invest,planFCTVA_r,planFCTVA_d) %>%
      mutate(autres_rec_invest = autres_rec_invest - planFCTVA_r + planFCTVA_d , rec_invest = rec_invest - planFCTVA_r + planFCTVA_d) %>%
      mutate(FCTVA = FCTVA/rec_invest,cessions = cessions/rec_invest,sub_dot_ssTAAm = sub_dot_ssTAAm/rec_invest,amende = amende/rec_invest,taxe_amenag = taxe_amenag/rec_invest,autres_rec_invest = autres_rec_invest/rec_invest) %>%
      gather("variables","montant",2:7) %>%
      mutate(rec_invest = nom_gfp_com)
    base3 <- gr_de_ref %>%
      filter(annee == annee_encours) %>%
      select(rec_invest,FCTVA,cessions,sub_dot_ssTAAm,amende,taxe_amenag,autres_rec_invest,planFCTVA_r,planFCTVA_d) %>%
      summarise_all(sum) %>%
      mutate(autres_rec_invest = autres_rec_invest - planFCTVA_r + planFCTVA_d , rec_invest = rec_invest - planFCTVA_r + planFCTVA_d) %>%
      mutate(FCTVA = FCTVA/rec_invest,cessions = cessions/rec_invest,sub_dot_ssTAAm = sub_dot_ssTAAm/rec_invest,amende = amende/rec_invest,taxe_amenag = taxe_amenag/rec_invest,autres_rec_invest = autres_rec_invest/rec_invest) %>%
      gather("variables","montant",2:7) %>%
      mutate(rec_invest = "Gr de réf")
    base_f <- rbind(base,base2,base3)}
  else if(bp_ba == 2){
    base <- echantillon %>%
      filter(annee == annee_encours) %>%
      select(rec_invest_ba,FCTVA_ba,cessions_ba,sub_dot_ssTAAm_ba,amende_ba,taxe_amenag_ba,autres_rec_invest_ba,planFCTVA_r_ba,planFCTVA_d_ba) %>%
      summarise_all(sum) %>%
      mutate(autres_rec_invest_ba = autres_rec_invest_ba - planFCTVA_r_ba + planFCTVA_d_ba , rec_invest_ba = rec_invest_ba - planFCTVA_r_ba + planFCTVA_d_ba) %>%
      mutate(FCTVA = FCTVA_ba/rec_invest_ba,cessions = cessions_ba/rec_invest_ba,sub_dot_ssTAAm = sub_dot_ssTAAm_ba/rec_invest_ba,amende = amende_ba/rec_invest_ba,taxe_amenag = taxe_amenag_ba/rec_invest_ba,autres_rec_invest = autres_rec_invest_ba/rec_invest_ba) %>%
      select(rec_invest_ba,FCTVA,cessions,sub_dot_ssTAAm,amende,taxe_amenag,autres_rec_invest) %>%
      gather("variables","montant",2:7) %>%
      mutate(rec_invest = "Echantillon")
    base2 <- entite %>%
      filter(annee == annee_encours) %>%
      select(rec_invest_ba,FCTVA_ba,cessions_ba,sub_dot_ssTAAm_ba,amende_ba,taxe_amenag_ba,autres_rec_invest_ba,planFCTVA_r_ba,planFCTVA_d_ba) %>%
      mutate(autres_rec_invest_ba = autres_rec_invest_ba - planFCTVA_r_ba + planFCTVA_d_ba , rec_invest_ba = rec_invest_ba - planFCTVA_r_ba + planFCTVA_d_ba) %>%
      mutate(FCTVA = FCTVA_ba/rec_invest_ba,cessions = cessions_ba/rec_invest_ba,sub_dot_ssTAAm = sub_dot_ssTAAm_ba/rec_invest_ba,amende = amende_ba/rec_invest_ba,taxe_amenag = taxe_amenag_ba/rec_invest_ba,autres_rec_invest = autres_rec_invest_ba/rec_invest_ba) %>%
      select(rec_invest_ba,FCTVA,cessions,sub_dot_ssTAAm,amende,taxe_amenag,autres_rec_invest) %>%
      gather("variables","montant",2:7) %>%
      mutate(rec_invest = nom_gfp_com)
    base3 <- gr_de_ref %>%
      filter(annee == annee_encours) %>%
      select(rec_invest_ba,FCTVA_ba,cessions_ba,sub_dot_ssTAAm_ba,amende_ba,taxe_amenag_ba,autres_rec_invest_ba,planFCTVA_r_ba,planFCTVA_d_ba) %>%
      summarise_all(sum) %>%
      mutate(autres_rec_invest_ba = autres_rec_invest_ba - planFCTVA_r_ba + planFCTVA_d_ba , rec_invest_ba = rec_invest_ba - planFCTVA_r_ba + planFCTVA_d_ba) %>%
      mutate(FCTVA = FCTVA_ba/rec_invest_ba,cessions = cessions_ba/rec_invest_ba,sub_dot_ssTAAm = sub_dot_ssTAAm_ba/rec_invest_ba,amende = amende_ba/rec_invest_ba,taxe_amenag = taxe_amenag_ba/rec_invest_ba,autres_rec_invest = autres_rec_invest_ba/rec_invest_ba) %>%
      select(rec_invest_ba,FCTVA,cessions,sub_dot_ssTAAm,amende,taxe_amenag,autres_rec_invest) %>%
      gather("variables","montant",2:7) %>%
      mutate(rec_invest = "Gr de réf")
    base_f <- rbind(base,base2,base3)
  } else {
    base <- echantillon %>%
      filter(annee == annee_encours) %>%
      select(rec_invest,FCTVA,cessions,sub_dot_ssTAAm,amende,taxe_amenag,autres_rec_invest,rec_invest_ba,FCTVA_ba,cessions_ba,sub_dot_ssTAAm_ba,amende_ba,taxe_amenag_ba,autres_rec_invest_ba,planFCTVA_r,planFCTVA_d,planFCTVA_r_ba,planFCTVA_d_ba,fc_i) %>%
      summarise_all(sum) %>%
      mutate(autres_rec_invest = autres_rec_invest - planFCTVA_r + planFCTVA_d , autres_rec_invest_ba = autres_rec_invest_ba - planFCTVA_r_ba + planFCTVA_d_ba , rec_invest = rec_invest - planFCTVA_r + planFCTVA_d, rec_invest_ba = rec_invest_ba - planFCTVA_r_ba + planFCTVA_d_ba) %>%
      mutate(rec_invest = rec_invest + rec_invest_ba - fc_i, FCTVA = FCTVA + FCTVA_ba, cessions = cessions + cessions_ba,sub_dot_ssTAAm = sub_dot_ssTAAm + sub_dot_ssTAAm_ba - fc_i,amende = amende_ba + amende,taxe_amenag = taxe_amenag + taxe_amenag_ba,autres_rec_invest = autres_rec_invest + autres_rec_invest_ba) %>%
      mutate(FCTVA = FCTVA/rec_invest,cessions = cessions/rec_invest,sub_dot_ssTAAm = sub_dot_ssTAAm/rec_invest,amende = amende/rec_invest,taxe_amenag = taxe_amenag/rec_invest,autres_rec_invest = autres_rec_invest/rec_invest) %>%
      select(rec_invest,FCTVA,cessions,sub_dot_ssTAAm,amende,taxe_amenag,autres_rec_invest) %>%
      gather("variables","montant",2:7) %>%
      mutate(rec_invest = "Echantillon")
    base2 <- entite %>%
      filter(annee == annee_encours) %>%
      select(rec_invest,FCTVA,cessions,sub_dot_ssTAAm,amende,taxe_amenag,autres_rec_invest,rec_invest_ba,FCTVA_ba,cessions_ba,sub_dot_ssTAAm_ba,amende_ba,taxe_amenag_ba,autres_rec_invest_ba,planFCTVA_r,planFCTVA_d,planFCTVA_r_ba,planFCTVA_d_ba,fc_i) %>%
      mutate(autres_rec_invest = autres_rec_invest - planFCTVA_r + planFCTVA_d , autres_rec_invest_ba = autres_rec_invest_ba - planFCTVA_r_ba + planFCTVA_d_ba , rec_invest = rec_invest - planFCTVA_r + planFCTVA_d, rec_invest_ba = rec_invest_ba - planFCTVA_r_ba + planFCTVA_d_ba) %>%
      mutate(rec_invest = rec_invest + rec_invest_ba- fc_i, FCTVA = FCTVA + FCTVA_ba, cessions = cessions + cessions_ba,sub_dot_ssTAAm = sub_dot_ssTAAm + sub_dot_ssTAAm_ba - fc_i,amende = amende_ba + amende,taxe_amenag = taxe_amenag + taxe_amenag_ba,autres_rec_invest = autres_rec_invest + autres_rec_invest_ba) %>%
      mutate(FCTVA = FCTVA/rec_invest,cessions = cessions/rec_invest,sub_dot_ssTAAm = sub_dot_ssTAAm/rec_invest,amende = amende/rec_invest,taxe_amenag = taxe_amenag/rec_invest,autres_rec_invest = autres_rec_invest/rec_invest) %>%
      select(rec_invest,FCTVA,cessions,sub_dot_ssTAAm,amende,taxe_amenag,autres_rec_invest) %>%
      gather("variables","montant",2:7) %>%
      mutate(rec_invest = nom_gfp_com)
    base3 <- gr_de_ref %>%
      filter(annee == annee_encours) %>%
      select(rec_invest,FCTVA,cessions,sub_dot_ssTAAm,amende,taxe_amenag,autres_rec_invest,rec_invest_ba,FCTVA_ba,cessions_ba,sub_dot_ssTAAm_ba,amende_ba,taxe_amenag_ba,autres_rec_invest_ba,planFCTVA_r,planFCTVA_d,planFCTVA_r_ba,planFCTVA_d_ba,fc_i) %>%
      summarise_all(sum) %>%
      mutate(autres_rec_invest = autres_rec_invest - planFCTVA_r + planFCTVA_d , autres_rec_invest_ba = autres_rec_invest_ba - planFCTVA_r_ba + planFCTVA_d_ba , rec_invest = rec_invest - planFCTVA_r + planFCTVA_d, rec_invest_ba = rec_invest_ba - planFCTVA_r_ba + planFCTVA_d_ba) %>%
      mutate(rec_invest = rec_invest + rec_invest_ba - fc_i, FCTVA = FCTVA + FCTVA_ba, cessions = cessions + cessions_ba,sub_dot_ssTAAm = sub_dot_ssTAAm + sub_dot_ssTAAm_ba - fc_i,amende = amende_ba + amende,taxe_amenag = taxe_amenag + taxe_amenag_ba,autres_rec_invest = autres_rec_invest + autres_rec_invest_ba) %>%
      mutate(FCTVA = FCTVA/rec_invest,cessions = cessions/rec_invest,sub_dot_ssTAAm = sub_dot_ssTAAm/rec_invest,amende = amende/rec_invest,taxe_amenag = taxe_amenag/rec_invest,autres_rec_invest = autres_rec_invest/rec_invest) %>%
      select(rec_invest,FCTVA,cessions,sub_dot_ssTAAm,amende,taxe_amenag,autres_rec_invest) %>%
      gather("variables","montant",2:7) %>%
      mutate(rec_invest = "Gr de réf")
    base_f <- rbind(base,base2,base3)
  }
  return(base_f)
}

#' Graph sur le zoom sur les rec_invest
#'
#' Permet d'obtenir le graph sur les details des rec_invest
#'
#' @param data_graph bdd qui permet d'obtenir le graph
#' @param entite bdd de l'entite, permet de savoir si une entite a été selectionné
#' @param nom_gfp_com Permet de choisir entre groupement ou autre pour le graphique
#' @param bp_ba pour le subtitle
#' @importFrom  magrittr %>% 
#' @import ggplot2
#' @importFrom scales percent
#' @importFrom forcats fct_relevel
#'
#' @export

graph_zoom_rec_invest <- function(data_graph,entite,nom_gfp_com,bp_ba){
  graph <- data_graph %>%
    ggplot(aes(x=rec_invest,y=montant,fill=fct_relevel(as.factor(variables),c("autres_rec_invest","cessions","taxe_amenag","amende","FCTVA","sub_dot_ssTAAm")))) +
    geom_col() +
    geom_hline(yintercept = 1) +
    geom_hline(yintercept = 0) +
    geom_text(aes(label=paste(round(montant*100,0),"%")),position = position_stack(vjust = 0.5)) +
    labs(title ="Structure des recettes d'invetissement hors emprunt en 2017, en %",x= "",y = "") +
    scale_y_continuous(labels = percent) +
    theme(panel.background = element_rect(fill = "white"),axis.text.x = element_text(size=13),legend.title = element_blank(),legend.text = element_text(size= 11)) +
    scale_fill_brewer(guide = guide_legend(),
                      labels = c("cessions"="Produit des cessions","sub_dot_ssTAAm"="Subventions et autres dotations","amende"="Amendes","taxe_amenag"="Taxe d'aménagement","autres_rec_invest" = "Autres recettes"),
                      limits= c("autres_rec_invest","cessions","taxe_amenag","amende","FCTVA","sub_dot_ssTAAm"),
                      palette="YlGnBu")

  if(nrow(entite) == 0){
    graph  <-   graph +
      scale_x_discrete(limits = c("Echantillon","Gr de réf"), label = c("Echantillon","Groupe de réf")) +
      labs(caption = "Traitement OFGL - Source : balances comptables DGFIP \nRecettes hors opérations liées aux prêts relais FCTVA (plan de relance)")
  }else{
    graph  <-   graph +
      scale_x_discrete(limits = c("Echantillon",nom_gfp_com,"Gr de réf"), label = c("Echantillon",nom_gfp_com,"Groupe de réf")) +
      labs(caption = paste("Traitement OFGL - Source : balances comptables DGFIP \nRecettes hors opérations liées aux prêts relais FCTVA (plan de relance) \nCollectivité sélectionnée : ",entite$LBUDG))
  }
  if(bp_ba == 1 ){
    graph <- graph + labs(subtitle ="Budgets principaux")
  }else if(bp_ba == 2 ){
    graph <- graph + labs(subtitle ="Budgets annexes")
  }else if(bp_ba == 3 ){
    graph <- graph + labs(subtitle ="Budgets principaux et annexes")
  }

  return(graph)
}
