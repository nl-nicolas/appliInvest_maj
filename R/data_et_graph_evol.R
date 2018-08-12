#' Etape data pour le graph sur les evolutions
#'
#' Permet d'obetenit la base pour le graphique sur les evolutions
#'
#' @param echantillon bdd de l'echantillon
#' @param entite bdd de l'entite selectionne
#' @param gr_de_ref bdd du gr de ref
#' @param bp_ba la bonne valeur de l'input bp ba ou de la value
#' @param nom_gfp_com choix entre "Commune", "Groupement" etc...
#' @param ma_var selectionne la bonne variable
#'
#' @import dplyr
#' @import tidyr
#' @importFrom  magrittr %>% 
#'
#' @export

data_grap_evol <- function(echantillon,entite,gr_de_ref,bp_ba,nom_gfp_com,ma_var){
  if(bp_ba == 1 | bp_ba == 2){
    echan <- echantillon %>%
      select(annee,ma_var,population) %>%
      group_by(annee) %>%
      summarise_all(sum)
    names(echan) <- c("annee","var_select_evol","population")
    echan <- echan %>%
      mutate(var_select_evol = var_select_evol/population*1000) %>%
      mutate(nom="Echantillon")

    com <- entite %>%
      select(annee,ma_var,population)
    names(com) <- c("annee","var_select_evol","population")
    com <- com %>%
      mutate(var_select_evol = var_select_evol/population*1000) %>%
      mutate(nom=nom_gfp_com)

    gr_ref <- gr_de_ref %>%
      select(annee,ma_var,population) %>%
      group_by(annee) %>%
      summarise_all(sum)
    names(gr_ref) <- c("annee","var_select_evol","population")
    gr_ref <- gr_ref %>%
      mutate(var_select_evol = var_select_evol/population*1000) %>%
      mutate(nom="Gr de réf")

    base <- rbind(echan,com,gr_ref)
  }else{
    echan <- echantillon %>%
      select(annee,dep_equip,dep_equip_ba,dep_invest,rec_fonc,dep_fonc,remboursement,rec_invest,emprunt,dep_invest_ba,rec_fonc_ba,dep_fonc_ba,remboursement_ba,rec_invest_ba,emprunt_ba,var_fon_roul,var_fon_roul_ba,dette,dette_ba,fc_i,population) %>%
      mutate(dep_equip=dep_equip + dep_equip_ba - fc_i, dep_invest = dep_invest + dep_invest_ba - fc_i, rec_invest = rec_invest + rec_invest_ba - fc_i,epn = rec_fonc + rec_fonc_ba - dep_fonc - dep_fonc_ba - remboursement - remboursement_ba, emprunt = emprunt + emprunt_ba, var_fon_roul = var_fon_roul + var_fon_roul_ba, dette = dette + dette_ba) %>%
      select(annee,ma_var,population)
    echan <- echan %>%
      group_by(annee) %>%
      summarise_all(sum)
    names(echan) <- c("annee","var_select_evol","population")
    echan <- echan %>%
      mutate(var_select_evol = var_select_evol/population*1000) %>%
      mutate(nom="Echantillon")

    com <- entite %>%
      select(annee,dep_equip,dep_equip_ba,dep_invest,rec_fonc,dep_fonc,remboursement,rec_invest,emprunt,dep_invest_ba,rec_fonc_ba,dep_fonc_ba,remboursement_ba,rec_invest_ba,emprunt_ba,var_fon_roul,var_fon_roul_ba,dette,dette_ba,fc_i,population) %>%
      mutate(dep_equip=dep_equip + dep_equip_ba - fc_i, dep_invest = dep_invest + dep_invest_ba - fc_i, rec_invest = rec_invest + rec_invest_ba - fc_i,epn = rec_fonc + rec_fonc_ba - dep_fonc - dep_fonc_ba - remboursement - remboursement_ba, emprunt = emprunt + emprunt_ba, var_fon_roul = var_fon_roul + var_fon_roul_ba, dette = dette + dette_ba) %>%
      select(annee,ma_var,population)
    names(com) <- c("annee","var_select_evol","population")
    com <- com %>%
      mutate(var_select_evol = var_select_evol/population*1000) %>%
      mutate(nom=nom_gfp_com)

    gr_ref <- gr_de_ref %>%
      select(annee,dep_equip,dep_equip_ba,dep_invest,rec_fonc,dep_fonc,remboursement,rec_invest,emprunt,dep_invest_ba,rec_fonc_ba,dep_fonc_ba,remboursement_ba,rec_invest_ba,emprunt_ba,var_fon_roul,var_fon_roul_ba,dette,dette_ba,fc_i,population) %>%
      mutate(dep_equip=dep_equip + dep_equip_ba, dep_invest = dep_invest + dep_invest_ba - fc_i, rec_invest = rec_invest + rec_invest_ba - fc_i,epn = rec_fonc + rec_fonc_ba - dep_fonc - dep_fonc_ba - remboursement - remboursement_ba, emprunt = emprunt + emprunt_ba, var_fon_roul = var_fon_roul + var_fon_roul_ba, dette = dette + dette_ba) %>%
      select(annee,ma_var,population)
    gr_ref <- gr_ref %>%
      group_by(annee) %>%
      summarise_all(sum)
    names(gr_ref) <- c("annee","var_select_evol","population")
    gr_ref <- gr_ref %>%
      mutate(var_select_evol = var_select_evol/population*1000) %>%
      mutate(nom="Gr de réf")

    base <- rbind(echan,com,gr_ref)
  }
  return(base)
}

#' Graph des evolutions des variables
#'
#' Permet d'obtenir le graph des evolutions
#'
#' @param data_evol la bdd pour le grph souvent data_graph_evol
#' @param annee_dispo les annee disponible sous la forme c(2012,2013)...
#' @param bp_ba permet de choisir les titre en fonction d'un input
#' @param var_select modifie le titre du graph en fonction d'un input
#' @param entite bdd de l'entite selectionne
#'
#' @importFrom  magrittr %>% 
#' @import ggplot2
#'
#' @export
graph_evol <- function(data_evol,annee_dispo,bp_ba,var_select,entite){
  graph <- ggplot(data = data_evol,aes(x=annee,y=var_select_evol,color=nom)) +
    geom_line(size=1.25) +
    xlab("") +
    ylab("") +
    scale_x_continuous(breaks = annee_dispo) +
    scale_color_brewer(palette="Dark2",label = c("Gr de réf" = "Groupe de réf")) +
    theme(panel.background = element_rect(fill = "#f7fbff", colour = "grey"), panel.grid.major = element_line(color  = "grey",size = 0.25),panel.grid.minor = element_line(color  = "grey",size = 0.25),axis.text.x = element_text(size=15),legend.title = element_blank(),legend.text = element_text(size=10))

  if(min(data_evol$var_select_evol)<0){
    graph <- graph + scale_y_continuous(limits = c(min(data_evol$var_select_evol),max(data_evol$var_select_evol)))
  }else{
    graph <- graph + scale_y_continuous(limits = c(0,max(data_evol$var_select_evol)))
  }
  if(nrow(entite) == 0){
    graph <- graph + labs(caption ="Traitement OFGL - Source : balances comptables DGFIP")
    }else if(nrow(entite) != 0){
    graph <- graph + labs(caption =paste("Traitement OFGL - Source : balances comptables DGFIP \n","Collectivité sélectionnée :",entite$LBUDG))
  }
  if(bp_ba == 1){
    if(var_select == "dep_equip"){
    graph  <-   graph + ggtitle(label = "Dépenses d'équipement en €/hab., budgets principaux")
    }else if(var_select == "rec_invest"){
      graph  <-   graph + ggtitle(label =  "Recettes d'investissement hors emp. en €/hab., budgets principaux")
    }else if(var_select == "epn"){
      graph  <-   graph + ggtitle(label = "Epargne nette en €/hab., budgets principaux",subtitle = "Epargne nette = Recettes de fonctionnement - dépenses de fonctionnement - remboursement de la dette")
    }else if(var_select == "dep_invest"){
      graph  <-   graph + ggtitle( label =  "Dépenses d'investissement hors remb. en €/hab., budgets principaux")
    }else if(var_select == "emprunt"){
      graph  <-   graph + ggtitle(label = "Emprunts en €/hab., budgets principaux",subtitle = "Emprunts hors gestion active de la dette")
    }else if(var_select == "var_fon_roul"){
      graph  <-   graph + ggtitle(label = "Variation du fonds de roulement en €/hab., budgets principaux",subtitle = "Variation du fonds de roulement = Recettes totales - dépenses totales")
    }else if(var_select == "subvention_204"){
      graph  <-   graph + ggtitle(label = "Subventions d'investissement versées en €/hab., budgets principaux")
      }else if(var_select == "sub_204_etat"){
    graph  <-   graph + ggtitle(label = "Subventions d'investissement versées à l'état \n en €/hab., budgets principaux")
    }else if(var_select == "sub_204_bc"){
      graph  <-   graph + ggtitle(label = "Subventions d'investissement versées au bloc communal \n en €/hab., budgets principaux")
    }else if(var_select == "sub_204_transport"){
      graph  <-   graph + ggtitle(label = "Subventions d'investissement versées aux organismes de transport \n en €/hab., budgets principaux")
    }else if(var_select == "sub_204_autres"){
      graph  <-   graph + ggtitle(label = "Subventions d'investissement versées aux autres organismes  \n en €/hab., budgets principaux")
    }else if(var_select == "sub_204_prive"){
      graph  <-   graph + ggtitle(label = "Subventions d'investissement versées aux organismes privés \n en €/hab., budgets principaux")}
    }else if(bp_ba == 2){
    if(var_select == "dep_equip_ba"){
      graph  <-   graph + ggtitle(label = "Dépenses d'équipement en €/hab., budgets annexes")
    }else if(var_select == "rec_invest_ba"){
      graph  <-   graph + ggtitle(label =  "Recettes d'investissement hors emp. en €/hab., budgets annexes")
    }else if(var_select == "epn_ba"){
      graph  <-   graph + ggtitle(label = "Epargne nette en €/hab., budgets annexes",subtitle = "Epargne nette = Recettes de fonctionnement - dépenses de fonctionnement - remboursement de la dette")
    }else if(var_select == "dep_invest_ba"){
      graph  <-   graph + ggtitle( label =  "Dépenses d'investissement hors remb. en €/hab., budgets annexes")
    }else if(var_select == "emprunt_ba"){
      graph  <-   graph + ggtitle(label = "Emprunts en €/hab., budgets annexes",subtitle = "Emprunts hors gestion active de la dette")
    }else if(var_select == "var_fon_roul_ba"){
      graph  <-   graph + ggtitle(label = "Variation du fonds de roulement en €/hab., budgets annexes",subtitle = "Variation du fonds de roulement = Recettes totales - dépenses totales")
    }
  }else if(bp_ba == 3){
    if(var_select == "dep_equip"){
      graph  <-   graph + ggtitle(label = "Dépenses d'équipement en €/hab., budgets principaux et budgets annexes")
    }else if(var_select == "rec_invest"){
      graph  <-   graph + ggtitle(label =  "Recettes d'investissement hors emp. en €/hab., budgets principaux et budgets annexes")
    }else if(var_select == "epn"){
      graph  <-   graph + ggtitle(label = "Epargne nette en €/hab., budgets principaux et budgets annexes",subtitle = "Epargne nette = Recettes de fonctionnement - dépenses de fonctionnement - remboursement de la dette")
    }else if(var_select == "dep_invest"){
      graph  <-   graph + ggtitle( label =  "Dépenses d'investissement hors remb. en €/hab., budgets principaux et budgets annexes")
    }else if(var_select == "emprunt"){
      graph  <-   graph + ggtitle(label = "Emprunts en €/hab., budgets principaux et budgets annexes",subtitle = "Emprunts hors gestion active de la dette")
    }else if(var_select == "var_fon_roul"){
      graph  <-   graph + ggtitle(label = "Variation du fonds de roulement en €/hab., budgets principaux et budgets annexes",subtitle = "Variation du fonds de roulement = Recettes totales - dépenses totales")
    }
  }

  return(graph)
}
