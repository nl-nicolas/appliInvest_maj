#' Data nuage de point
#'
#' Permet d'obtenir la base pour le graph nuage de point
#'
#' @param echantillon bdd de l'echantillon
#' @param entite bdd de l'entite selectionnee
#' @param gr_de_ref bdd du groupe de ref de l'ofgl
#' @param nom_dep_reg "Departement" ou "Region" etc...
#' @param annee_choisi permet de savoir quelle année
#'
#'
#' @import dplyr
#' @import tidyr
#' @importFrom  magrittr %>% 
#'
#' @export

data_sub_dep_equip <- function(annee_choisi,nom_dep_reg,echantillon,entite,gr_de_ref){

  if(annee_choisi == annee_encours){
    base <- echantillon %>%
      filter(annee == annee_encours) %>%
      select(subvention_204,dep_equip,population,LBUDG) %>%
      mutate(subvention_204 = subvention_204/population *1000, dep_equip = dep_equip/population *1000) %>%
      mutate(nom = "Echantillon")
    base2 <- entite %>%
      filter(annee == annee_encours) %>%
      select(subvention_204,dep_equip,population,LBUDG) %>%
      mutate(subvention_204 = subvention_204/population *1000, dep_equip = dep_equip/population *1000) %>%
      mutate(nom =  nom_dep_reg)
    base3 <- gr_de_ref %>%
      filter(annee == annee_encours) %>%
      select(subvention_204,dep_equip,population,LBUDG) %>%
      mutate(subvention_204 = subvention_204/population *1000, dep_equip = dep_equip/population *1000) %>%
      mutate(nom = "Gr de réf")
    base_f <- rbind(base,base2,base3)
  }else if(annee_choisi != annee_encours){
    base <- echantillon %>%
      select(subvention_204,dep_equip,LBUDG,population) %>%
      group_by(LBUDG) %>%
      summarise_all(sum) %>%
      mutate(subvention_204 = subvention_204/population *1000, dep_equip = dep_equip/population *1000) %>%
      mutate(nom = "Echantillon")
    base2 <- entite %>%
      select(subvention_204,dep_equip,LBUDG,population) %>%
      group_by(LBUDG) %>%
      summarise_all(sum) %>%
      mutate(subvention_204 = subvention_204/population *1000, dep_equip = dep_equip/population *1000) %>%
      mutate(nom =  nom_dep_reg)
    base3 <- gr_de_ref %>%
      select(subvention_204,dep_equip,LBUDG,population) %>%
      group_by(LBUDG) %>%
      summarise_all(sum) %>%
      mutate(subvention_204 = subvention_204/population *1000, dep_equip = dep_equip/population *1000) %>%
      mutate(nom = "Gr de réf")
    base_f <- rbind(base,base2,base3)
  }
}

#' Graphique pour le nuage de point
#'
#' Permet d'obtenir le nuage de point entre dep_equip et subvention
#'
#' @param annee Permet de savoir sur quelles annees on travaille
#' @param data_graph bdd pour le graph, doit être associé à data_sub_dep_equip
#' @param entite bdd de l'entite
#'
#' @import ggplot2
#' @importFrom  magrittr %>% 
#' @importFrom forcats fct_relevel
#' @importFrom plotly ggplotly
#'
#' @export

graph_sub_dep_equip <- function(annee,data_graph,entite){
  graph <- data_graph %>%
    ggplot(aes(x = dep_equip , y=subvention_204 , colour = nom,label = LBUDG)) +
    geom_point() +
    labs(x="Dépenses d'équipement" , y = "Subventions versées") +
    theme(panel.background = element_rect(fill = "#f7fbff", colour = "grey"),plot.title = element_text(size = 9) ,panel.grid.major = element_line(color  = "grey",size = 0.25),axis.title = element_text(size = 9),panel.grid.minor = element_line(color  = "grey",size = 0.25),axis.text = element_text(size=7),legend.title = element_blank(),legend.text = element_text(size=9)) +
    scale_colour_brewer(guide = guide_legend(),palette = "Dark2")


  if(annee == annee_encours){
    graph <- graph +
      labs(title = "Budgets principaux\nen €/hab. en 2017") 
  }else if(annee == "2012-2017"){
    graph <- graph +
      labs(title = "Budgets principaux en €/hab.\nde 2012 à 2017 (moyenne annuelle)")}
 
   graph <- ggplotly(graph)


  graph <- graph  %>%
    layout(annotations = list(x = 1, y = -0.1, text = "OFGL - Source : DGFIP", 
                                         showarrow = F, xref='paper', yref='paper', 
                                         xanchor='right', yanchor='auto', xshift=0, yshift= -0.1,
                                         font=list(size=8)))
  
  return(graph)
}
