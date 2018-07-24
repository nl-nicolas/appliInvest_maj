#' Le bloc communal pour les GFP
#'
#' Permet d'obtenir une bdd qui va permettre de faire un graphique entre les gfp et les communes membres
#'
#' @param echantillon bdd de l'échantillon sélectionné
#' @param entite bdd de l'entite selectionnée
#' @param gr_ref bdd du groupe de reference
#' @import dplyr
#' @import tidyr
#' @importFrom  magrittr %>% 
#'
#' @export

data_bc_dep_equip <- function(echantillon,entite,gr_ref){

  echan <-   echantillon %>%
    select(annee,population,dep_equip,dep_equip_ba,dep_equip_com,dep_equip_com_ba) %>%
    filter(!is.na(population)) %>% filter(!is.na(dep_equip_com_ba)) %>% filter(!is.na(dep_equip_com)) %>%
    group_by(annee) %>%
    summarise_all(sum)
  echan_16 <- echan %>%
    filter(annee == annee_encours) %>%
    mutate(dep_equip_gfp = (dep_equip + dep_equip_ba)/(dep_equip + dep_equip_ba +dep_equip_com + dep_equip_com_ba),dep_equip_com_gfp = (dep_equip_com + dep_equip_com_ba)/(dep_equip + dep_equip_ba +dep_equip_com + dep_equip_com_ba)) %>%
    select(dep_equip_gfp,dep_equip_com_gfp) %>%
    mutate(nom="echan") %>%
    gather("variables","montant",1:2)

  # Donnees pour l'entite

  base_entite <-   entite %>%
    select(annee,population,dep_equip,dep_equip_ba,dep_equip_com,dep_equip_com_ba)
  base_entite_16 <- base_entite %>%
    filter(annee == annee_encours) %>%
    mutate(dep_equip_gfp = (dep_equip + dep_equip_ba)/(dep_equip + dep_equip_ba +dep_equip_com + dep_equip_com_ba),dep_equip_com_gfp = (dep_equip_com + dep_equip_com_ba)/(dep_equip + dep_equip_ba +dep_equip_com + dep_equip_com_ba)) %>%
    select(dep_equip_gfp,dep_equip_com_gfp) %>%
    mutate(nom= "entite") %>%
    gather("variables","montant",1:2)

  # Donnees pour le gr de ref

  base_gr_ref <-   gr_ref %>%
    select(annee,population,dep_equip,dep_equip_ba,dep_equip_com,dep_equip_com_ba) %>%
    filter_all(all_vars(!is.na(.))) %>%
    group_by(annee) %>%
    summarise_all(sum)
  base_gr_ref_16 <- base_gr_ref %>%
    filter(annee == annee_encours) %>%
    mutate(dep_equip_gfp = (dep_equip + dep_equip_ba)/(dep_equip + dep_equip_ba +dep_equip_com + dep_equip_com_ba),dep_equip_com_gfp = (dep_equip_com + dep_equip_com_ba)/(dep_equip + dep_equip_ba +dep_equip_com + dep_equip_com_ba)) %>%
    select(dep_equip_gfp,dep_equip_com_gfp) %>%
    mutate(nom="gr_ref") %>%
    gather("variables","montant",1:2)

  base <- rbind(echan_16,base_entite_16,base_gr_ref_16)

}

#' Graphique poids des gfp
#'
#' Permet de connaitre le poids des gfp dans le bloc communal
#'
#'@param data bdd pour obtenir le graphique souvent data_bc_dep_equip
#'@param entite bdd de l'entité permet de savoir si une entité a été isolé
#'@param nom_gfp_com choisir entre commune et groupement
#'
#' @importFrom  magrittr %>% 
#' @import ggplot2
#'
#'@export

graph_bc_dep_equip <- function(data,entite,nom_gfp_com){

  graph <- data %>%
    ggplot() +
      geom_col(aes(y=montant,x=nom,fill = variables),position = "fill") +
      geom_text(aes(y=montant,x=nom,label = paste(round(montant*100,0),"%")),position = position_fill(vjust = 0.5)) +
      coord_flip() +
      ggtitle("Répartition des dépenses d'équipement 2017 au sein du bloc communal \n(groupements et leurs communes membres), en %") +
      ylab("") +
      xlab("") +
      theme(panel.background = element_rect(fill = "white"),axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.text.y = element_text(size = 15),legend.title = element_blank(),legend.text = element_text(size = 9)) +
      scale_fill_brewer(palette="Blues",labels= c("dep_equip_gfp" = "Dépenses des GFP", "dep_equip_com_gfp" = "Dépenses des communes"),limits = c("dep_equip_gfp","dep_equip_com_gfp"))
      scale_x_continuous(labels = percent)

   if(nrow(entite) == 0){
     graph  <-   graph + scale_x_discrete(labels = c("gr_ref"="Groupe de réf",
                                                     "echan" ="Echantillon"),
                                          limits = c("gr_ref","echan")) +
       labs(caption = "Traitement OFGL - Source : balances comptables DGFIP")


   }else if(nrow(entite) != 0 ){
     graph  <-   graph + scale_x_discrete(labels = c("gr_ref"="Groupe de réf",
                                                     "echan" = "Echantillon",
                                                     "entite" = nom_gfp_com),
                                          limits = c("gr_ref","entite","echan")) +
       labs(caption = paste("Traitement OFGL - Source : balances comptables DGFIP \nCollectivité sélectionnée : ",entite$LBUDG))

   }
    return(graph)
  }
