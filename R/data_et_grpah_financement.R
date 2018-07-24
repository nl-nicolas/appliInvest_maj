#' Etape data grph financement
#'
#' Permet d'obtenir la bddd pour le grap de financement
#'
#' @param echantillon bdd de l'echantillon
#' @param entite bdd de l'entite selectionnee gfp ou com ou dep ou reg
#' @param gr_de_ref bdd du groupe de reference de l'ofgl
#' @param nom_gfp_com var qui permet d'indiquer si l'entite est gfp ou commune
#' @param annee_encours permet de selectionne l'annee
#' @param bp_ba choisir le bon input de bp_ba
#' @import dplyr
#' @import tidyr
#' @importFrom  magrittr %>% 
#'
#' @export

data_graph_finan <- function(echantillon,entite,gr_de_ref,nom_gfp_com,annee_encours,bp_ba){
  if(bp_ba == 1){
  echan <- echantillon %>%
    filter(annee %in% annee_encours) %>%
    select(dep_invest,rec_fonc,dep_fonc,remboursement,rec_invest,emprunt,epn) %>%
    summarise_all(sum)%>%
    select(dep_invest,epn,rec_invest,emprunt)%>%
    mutate(epn = epn/dep_invest,rec_invest = rec_invest/dep_invest , emprunt = emprunt/dep_invest) %>%
    gather("variables","montant",2:4) %>%
    mutate(dep_invest = "Echantillon") %>%
    rename(finance = "dep_invest")

  indiv <- entite %>%
    filter(annee %in% annee_encours) %>%
    select(dep_invest,rec_fonc,dep_fonc,remboursement,rec_invest,emprunt,epn) %>%
    summarise_all(sum) %>%
    select(dep_invest,epn,rec_invest,emprunt) %>%
    mutate(epn = epn/dep_invest,rec_invest = rec_invest/dep_invest , emprunt = emprunt/dep_invest) %>%
    gather("variables","montant",2:4) %>%
    mutate(dep_invest = nom_gfp_com) %>%
    rename(finance = "dep_invest")

  gr_ref <- gr_de_ref %>%
    filter(annee %in% annee_encours) %>%
    select(dep_invest,rec_fonc,dep_fonc,remboursement,rec_invest,emprunt,epn) %>%
    summarise_all(sum)%>%
    select(dep_invest,epn,rec_invest,emprunt)%>%
    mutate(epn = epn/dep_invest,rec_invest = rec_invest/dep_invest , emprunt = emprunt/dep_invest) %>%
    gather("variables","montant",2:4) %>%
    mutate(dep_invest = "Gr de réf") %>%
    rename(finance = "dep_invest")

  base <- rbind(echan,indiv,gr_ref)}

else{
  if(bp_ba == 2){
    echan <- echantillon %>%
      filter(annee %in% annee_encours) %>%
      select(dep_invest_ba,rec_fonc_ba,dep_fonc_ba,remboursement_ba,rec_invest_ba,emprunt_ba,epn_ba) %>%
      summarise_all(sum) %>%
      select(dep_invest_ba,epn_ba,rec_invest_ba,emprunt_ba) %>%
      mutate(dep_invest = dep_invest_ba,epn = epn_ba/dep_invest_ba, rec_invest = rec_invest_ba/dep_invest_ba , emprunt = emprunt_ba/dep_invest_ba) %>%
      select(dep_invest,epn,rec_invest,emprunt) %>%
      gather("variables","montant",2:4) %>%
      mutate(dep_invest = "Echantillon") %>%
      rename(finance = "dep_invest")

    indiv <- entite %>%
      filter(annee %in% annee_encours) %>%
      select(dep_invest_ba,rec_fonc_ba,dep_fonc_ba,remboursement_ba,rec_invest_ba,emprunt_ba,epn_ba) %>%
      summarise_all(sum)%>%
      select(dep_invest_ba,epn_ba,rec_invest_ba,emprunt_ba) %>%
      mutate(dep_invest = dep_invest_ba,epn = epn_ba/dep_invest_ba, rec_invest = rec_invest_ba/dep_invest_ba , emprunt = emprunt_ba/dep_invest_ba) %>%
      select(dep_invest,epn,rec_invest,emprunt) %>%
      gather("variables","montant",2:4) %>%
      mutate(dep_invest = nom_gfp_com) %>%
      rename(finance = "dep_invest")

    gr_ref <- gr_de_ref %>%
      filter(annee %in% annee_encours) %>%
      select(dep_invest_ba,rec_fonc_ba,dep_fonc_ba,remboursement_ba,rec_invest_ba,emprunt_ba,epn_ba) %>%
      summarise_all(sum) %>%
      select(dep_invest_ba,epn_ba,rec_invest_ba,emprunt_ba) %>%
      mutate(dep_invest = dep_invest_ba,epn = epn_ba/dep_invest_ba, rec_invest = rec_invest_ba/dep_invest_ba , emprunt = emprunt_ba/dep_invest_ba) %>%
      select(dep_invest,epn,rec_invest,emprunt) %>%
      gather("variables","montant",2:4) %>%
      mutate(dep_invest = "Gr de réf") %>%
      rename(finance = "dep_invest")

    base <- rbind(echan,indiv,gr_ref)}

  else{
    if(bp_ba == 3){
      echan <- echantillon %>%
        filter(annee %in% annee_encours) %>%
        select(dep_invest,rec_fonc,dep_fonc,remboursement,rec_invest,emprunt,dep_invest_ba,rec_fonc_ba,dep_fonc_ba,remboursement_ba,rec_invest_ba,emprunt_ba,fc_i,fc_f) %>%
        summarise_all(sum) %>%
        mutate(epn = rec_fonc - dep_fonc - remboursement, epn_ba = rec_fonc_ba - dep_fonc_ba - remboursement_ba) %>%
        mutate(dep_invest = dep_invest + dep_invest_ba - fc_i, rec_invest = rec_invest + rec_invest_ba - fc_i, epn = epn + epn_ba, emprunt = emprunt + emprunt_ba) %>%
        select(dep_invest,epn,rec_invest,emprunt) %>%
        mutate(epn = epn/dep_invest,rec_invest = rec_invest/dep_invest , emprunt = emprunt/dep_invest) %>%
        gather("variables","montant",2:4) %>%
        mutate(dep_invest = "Echantillon") %>%
        rename(finance = "dep_invest")

      indiv <- entite %>%
        filter(annee %in% annee_encours) %>%
        select(dep_invest,rec_fonc,dep_fonc,remboursement,rec_invest,emprunt,dep_invest_ba,rec_fonc_ba,dep_fonc_ba,remboursement_ba,rec_invest_ba,emprunt_ba,fc_i,fc_f) %>%
        summarise_all(sum) %>%
        mutate(epn = rec_fonc - dep_fonc - remboursement,epn_ba = rec_fonc_ba - dep_fonc_ba - remboursement_ba) %>%
        mutate(dep_invest = dep_invest + dep_invest_ba - fc_i, rec_invest = rec_invest + rec_invest_ba - fc_i, epn = epn + epn_ba, emprunt = emprunt + emprunt_ba) %>%
        select(dep_invest,epn,rec_invest,emprunt) %>%
        mutate(epn = epn/dep_invest,rec_invest = rec_invest/dep_invest , emprunt = emprunt/dep_invest) %>%
        gather("variables","montant",2:4) %>%
        mutate(dep_invest = nom_gfp_com) %>%
        rename(finance = "dep_invest")

      gr_ref <- gr_de_ref %>%
        filter(annee %in% annee_encours) %>%
        select(dep_invest,rec_fonc,dep_fonc,remboursement,rec_invest,emprunt,dep_invest_ba,rec_fonc_ba,dep_fonc_ba,remboursement_ba,rec_invest_ba,emprunt_ba,fc_i,fc_f) %>%
        summarise_all(sum) %>%
        mutate(epn = rec_fonc - dep_fonc - remboursement, epn_ba = rec_fonc_ba - dep_fonc_ba - remboursement_ba) %>%
        mutate(dep_invest = dep_invest + dep_invest_ba - fc_i, rec_invest = rec_invest + rec_invest_ba - fc_i, epn = epn + epn_ba, emprunt = emprunt + emprunt_ba) %>%
        select(dep_invest,epn,rec_invest,emprunt) %>%
        mutate(epn = epn/dep_invest,rec_invest = rec_invest/dep_invest , emprunt = emprunt/dep_invest) %>%
        gather("variables","montant",2:4) %>%
        mutate(dep_invest = "Gr de réf") %>%
        rename(finance = "dep_invest")

      base <- rbind(echan,indiv,gr_ref)
    }
  }
}
  return(base)
}


#' Graphique sur les moyens de financement
#'
#' Permet de faire le graphque sur les moyens de financement
#'
#' @param data_graph bdd qui permet de faire le graphique data_grph_finan
#' @param entite bdd de l'entite selectionnee
#' @param nom_gfp_com var qui permet d'indiquer si l'entite est gfp ou commune
#' @param bp_ba permet de choisir entre bp ba
#'
#' @importFrom  magrittr %>% 
#' @import ggplot2
#' @importFrom scales percent
#' @importFrom forcats fct_relevel
#'
#' @export

graph_financement <- function(data_graph,entite,nom_gfp_com,bp_ba){

  graph <-  data_graph  %>%
    ggplot(aes(x=finance ,y=montant,fill = fct_relevel(as.factor(variables),c("emprunt","rec_invest","epn")))) +
    geom_col(position = "stack") +
    geom_text(aes(label = paste(round(data_graph$montant*100,0),"%")),position = position_stack(vjust = 0.5)) +
    geom_hline(yintercept = 1) +
    geom_hline(yintercept = 0) +
    scale_y_continuous(labels = percent) +
    theme(panel.background = element_rect(fill = "white"),axis.text.x = element_text(size=13),legend.title = element_blank(),legend.text = element_text(size= 11)) +
    scale_fill_brewer(guide = guide_legend(),labels = c("emprunt"="Emprunts","rec_invest"="Recettes Invest","epn"="Epargne nette"), palette="Blues",direction = -1)

if(nrow(entite) == 0){
  graph  <-   graph +
    scale_x_discrete(limits = c("Echantillon","Gr de réf"), label = c("Echantillon","Groupe de réf")) +
    labs(caption="Traitement OFGL - Source : balances comptables DGFIP")
}else{
  graph  <-   graph +
  scale_x_discrete(limits = c("Echantillon",nom_gfp_com,"Gr de réf"), label = c("Echantillon",nom_gfp_com,"Groupe de réf")) +
  labs(caption= paste0("Traitement OFGL - Source : balances comptables DGFIP \n","Collectivité sélectionnée :",entite$LBUDG))
}
  if(bp_ba == 1){
    graph  <-   graph + labs(title = "En % des dépenses d'investissement hors remboursement de la dette en 2017",subtitle ="Budgets principaux",x= "",y = "")
  }else if(bp_ba == 2){
    graph  <-   graph + labs(title ="En % des dépenses d'investissement hors remboursement de la dette en 2017 \n",subtitle ="Budgets annexes",x= "",y = "")
  }else if(bp_ba == 3){
    graph  <-   graph +  labs(title = "En % des dépenses d'investissement hors remboursement de la dette en 2017 \n",subtitle ="Budgets principaux et annexes",x= "",y = "")
  }
return(graph)
}
