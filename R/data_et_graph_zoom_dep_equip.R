#' Data pour bdd graph detail dep_equip
#'
#' Permet d'obtenir la bdd qui va bien avec la fonction graph_zoom_dep_equip
#'
#' @param echantillon bdd de l'echantillon
#' @param entite bdd de l'entite selectionee
#' @param gr_de_ref bdd du gr de ref
#' @param bp_ba choisir le bon input bp_ba
#' @param nom_gfp_com Commune ou groupement ou autre
#' @param annee_encours permet de fixer les années
#'
#' @import dplyr
#' @import tidyr
#' @importFrom  magrittr %>% 
#'
#' @export

data_zoom_dep_equip <- function(echantillon,entite,gr_de_ref,bp_ba,nom_gfp_com,annee_encours){
  if(bp_ba == 1){
    base <- echantillon %>%
    filter(annee %in% annee_encours) %>%
    select(dep_equip_brut,terrains,constructions,reseaux,bien_meuble,autres_dep_equip) %>%
    summarise_all(sum) %>%
    mutate(terrains = terrains/dep_equip_brut,constructions = constructions/dep_equip_brut,reseaux = reseaux/dep_equip_brut,bien_meuble = bien_meuble/dep_equip_brut,autres_dep_equip = autres_dep_equip/dep_equip_brut) %>%
    gather("variables","montant",2:6) %>%
    mutate(dep_equip_brut = "Echantillon")

    base2 <- entite %>%
    filter(annee %in% annee_encours) %>%
    select(dep_equip_brut,terrains,constructions,reseaux,bien_meuble,autres_dep_equip) %>%
      summarise_all(sum) %>%
    mutate(terrains = terrains/dep_equip_brut,constructions = constructions/dep_equip_brut,reseaux = reseaux/dep_equip_brut,bien_meuble = bien_meuble/dep_equip_brut,autres_dep_equip = autres_dep_equip/dep_equip_brut) %>%
    gather("variables","montant",2:6) %>%
    mutate(dep_equip_brut = nom_gfp_com)

  base3 <- gr_de_ref %>%
    filter(annee %in% annee_encours) %>%
    select(dep_equip_brut,terrains,constructions,reseaux,bien_meuble,autres_dep_equip) %>%
    summarise_all(sum) %>%
    mutate(terrains = terrains/dep_equip_brut,constructions = constructions/dep_equip_brut,reseaux = reseaux/dep_equip_brut,bien_meuble = bien_meuble/dep_equip_brut,autres_dep_equip = autres_dep_equip/dep_equip_brut) %>%
    gather("variables","montant",2:6) %>%
    mutate(dep_equip_brut = "Gr de réf")
  base_f <- rbind(base,base2,base3)
}else if(bp_ba == 2){
  base <- echantillon %>%
    filter(annee %in% annee_encours) %>%
    select(dep_equip_brut_ba,terrains_ba,constructions_ba,reseaux_ba,bien_meuble_ba,autres_dep_equip_ba) %>%
    summarise_all(sum) %>%
    mutate(dep_equip_brut = dep_equip_brut_ba,terrains = terrains_ba/dep_equip_brut_ba,constructions = constructions_ba/dep_equip_brut_ba,reseaux = reseaux_ba/dep_equip_brut_ba,bien_meuble = bien_meuble_ba/dep_equip_brut_ba,autres_dep_equip = autres_dep_equip_ba/dep_equip_brut_ba) %>%
    select(dep_equip_brut,terrains,constructions,reseaux,bien_meuble,autres_dep_equip) %>%
    gather("variables","montant",2:6) %>%
    mutate(dep_equip_brut = "Echantillon")
  base2 <- entite %>%
    filter(annee %in% annee_encours) %>%
    select(dep_equip_brut_ba,terrains_ba,constructions_ba,reseaux_ba,bien_meuble_ba,autres_dep_equip_ba) %>%
    summarise_all(sum) %>%
    mutate(dep_equip_brut = dep_equip_brut_ba,terrains = terrains_ba/dep_equip_brut_ba,constructions = constructions_ba/dep_equip_brut_ba,reseaux = reseaux_ba/dep_equip_brut_ba,bien_meuble = bien_meuble_ba/dep_equip_brut_ba,autres_dep_equip = autres_dep_equip_ba/dep_equip_brut_ba) %>%
    select(dep_equip_brut,terrains,constructions,reseaux,bien_meuble,autres_dep_equip) %>%
    gather("variables","montant",2:6) %>%
    mutate(dep_equip_brut = nom_gfp_com)
  base3 <- gr_de_ref %>%
    filter(annee %in% annee_encours) %>%
    select(dep_equip_brut_ba,terrains_ba,constructions_ba,reseaux_ba,bien_meuble_ba,autres_dep_equip_ba) %>%
    summarise_all(sum) %>%
    mutate(dep_equip_brut = dep_equip_brut_ba,terrains = terrains_ba/dep_equip_brut_ba,constructions = constructions_ba/dep_equip_brut_ba,reseaux = reseaux_ba/dep_equip_brut_ba,bien_meuble = bien_meuble_ba/dep_equip_brut_ba,autres_dep_equip = autres_dep_equip_ba/dep_equip_brut_ba) %>%
    select(dep_equip_brut,terrains,constructions,reseaux,bien_meuble,autres_dep_equip) %>%
    gather("variables","montant",2:6) %>%
    mutate(dep_equip_brut = "Gr de réf")
  base_f <- rbind(base,base2,base3)
}else{
  base <- echantillon %>%
    filter(annee %in% annee_encours) %>%
    select(dep_equip_brut_ba,terrains_ba,constructions_ba,reseaux_ba,bien_meuble_ba,autres_dep_equip_ba,dep_equip_brut,terrains,constructions,reseaux,bien_meuble,autres_dep_equip) %>%
    summarise_all(sum) %>%
    mutate(terrains = (terrains_ba + terrains)/(dep_equip_brut_ba + dep_equip_brut),constructions = (constructions_ba + constructions)/(dep_equip_brut_ba + dep_equip_brut),reseaux = (reseaux_ba + reseaux)/(dep_equip_brut_ba + dep_equip_brut),bien_meuble = (bien_meuble_ba + bien_meuble)/(dep_equip_brut_ba + dep_equip_brut),autres_dep_equip = (autres_dep_equip_ba + autres_dep_equip)/(dep_equip_brut_ba + dep_equip_brut)) %>%
    select(dep_equip_brut,terrains,constructions,reseaux,bien_meuble,autres_dep_equip) %>%
    gather("variables","montant",2:6) %>%
    mutate(dep_equip_brut = "Echantillon")
  base2 <- entite %>%
    filter(annee %in% annee_encours) %>%
    select(dep_equip_brut_ba,terrains_ba,constructions_ba,reseaux_ba,bien_meuble_ba,autres_dep_equip_ba,dep_equip_brut,terrains,constructions,reseaux,bien_meuble,autres_dep_equip) %>%
    summarise_all(sum) %>%
    mutate(terrains = (terrains_ba + terrains)/(dep_equip_brut_ba + dep_equip_brut),constructions = (constructions_ba + constructions)/(dep_equip_brut_ba + dep_equip_brut),reseaux = (reseaux_ba + reseaux)/(dep_equip_brut_ba + dep_equip_brut),bien_meuble = (bien_meuble_ba + bien_meuble)/(dep_equip_brut_ba + dep_equip_brut),autres_dep_equip = (autres_dep_equip_ba + autres_dep_equip)/(dep_equip_brut_ba + dep_equip_brut)) %>%
    select(dep_equip_brut,terrains,constructions,reseaux,bien_meuble,autres_dep_equip) %>%
    gather("variables","montant",2:6) %>%
    mutate(dep_equip_brut = nom_gfp_com)
  base3 <- gr_de_ref %>%
    filter(annee %in% annee_encours) %>%
    select(dep_equip_brut_ba,terrains_ba,constructions_ba,reseaux_ba,bien_meuble_ba,autres_dep_equip_ba,dep_equip_brut,terrains,constructions,reseaux,bien_meuble,autres_dep_equip) %>%
    summarise_all(sum) %>%
    mutate(terrains = (terrains_ba + terrains)/(dep_equip_brut_ba + dep_equip_brut),constructions = (constructions_ba + constructions)/(dep_equip_brut_ba + dep_equip_brut),reseaux = (reseaux_ba + reseaux)/(dep_equip_brut_ba + dep_equip_brut),bien_meuble = (bien_meuble_ba + bien_meuble)/(dep_equip_brut_ba + dep_equip_brut),autres_dep_equip = (autres_dep_equip_ba + autres_dep_equip)/(dep_equip_brut_ba + dep_equip_brut)) %>%
    select(dep_equip_brut,terrains,constructions,reseaux,bien_meuble,autres_dep_equip) %>%
    gather("variables","montant",2:6) %>%
    mutate(dep_equip_brut = "Gr de réf")
  base_f <- rbind(base,base2,base3)
}

return(base_f)
}

#' Graphique zoom dep_equip
#'
#' Permet d'obtenir le graphique qui va bien avec data_zoom_dep_equip
#'
#' @param data_graph bdd pour le graph utilisez data_zoom_dep_equip
#' @param entite bdd de l'entite, permet de savoir si une entite a été selectionné
#' @param nom_gfp_com Permet de choisir entre groupement ou autre pour le graphique
#' @param bp_ba selectionne le bon subtitle
#'
#' @importFrom  magrittr %>% 
#' @import ggplot2
#' @importFrom scales percent
#'
#' @export
graph_zoom_dep_equip <- function(data_graph,entite,nom_gfp_com,bp_ba){
graph <- data_graph %>%
  ggplot(aes(x=dep_equip_brut,y=montant, fill = variables)) +
  geom_col() +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 0) +
  geom_text(aes(label=paste(round(montant*100,0),"%")),position = position_stack(vjust = 0.5)) +
  labs(title ="Structure des dépenses d'équipement brutes en 2017, en %",
       x= "",
       y = "") +
  scale_y_continuous(labels = percent) +
  theme(panel.background = element_rect(fill = "white"),axis.text.x = element_text(size=13),legend.title = element_blank(),legend.text = element_text(size= 11)) +
  scale_fill_brewer(guide = guide_legend(),
                    labels = c("terrains" = "Terrains","constructions" = "Constructions","reseaux" = "Réseaux et voirie","bien_meuble"= "Biens meubles","autres_dep_equip"="Autres dépenses d'équipement"),
                    palette="Greens")


if(nrow(entite) == 0){
  graph  <-   graph +
    scale_x_discrete(limits = c("Echantillon","Gr de réf"), label = c("Echantillon","Groupe de réf")) +
    labs(caption = "Traitement OFGL - Source : balances comptables DGFIP")
}else{
  graph  <-   graph +
    scale_x_discrete(limits = c("Echantillon",nom_gfp_com,"Gr de réf"), label = c("Echantillon",nom_gfp_com,"Groupe de réf")) +
    labs(caption = paste("Traitement OFGL - Source : balances comptables DGFIP \nCollectivité sélectionnée : ",entite$LBUDG))
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
