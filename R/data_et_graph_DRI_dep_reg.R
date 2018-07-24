#' Etape data pour les DRI des dep et reg
#'
#' Permet d'obetinr la base du graphique sur les DRI pour les dep et reg
#'
#'@param echantillon Base de l'echnatillon GFP, com,dep,reg
#'@param entite Base de l'entite selectionnable type gfp ou com etc
#'@param gr_ref Base du gr de ref
#'@param bp_ba permet de choisir le input$bp_ba qui va bien
#'
#' @import dplyr
#' @import tidyr
#' @importFrom  magrittr %>% 
#'
#'@return une bdd
#'@export

data_DRI <- function(echantillon,entite,gr_ref,bp_ba){

  if(bp_ba == 1){
    echan <-   echantillon %>%
    select(annee,population,dep_equip,subvention_204,autre_dep_invest) %>%
    group_by(annee) %>%
    summarise_all(sum)
  echan_moy <- echan %>%
    summarise(dep_equip = sum(dep_equip), subvention_204=sum(subvention_204),autre_dep_invest=sum(autre_dep_invest), population = sum(population)) %>%
    mutate(dep_equip = (dep_equip/population *1000),subvention_204 = (subvention_204/population *1000),autre_dep_invest = (autre_dep_invest/population *1000)) %>%
    select(dep_equip,subvention_204,autre_dep_invest) %>%
    mutate(nom="echan_moy") %>%
    gather("variables","montant",1:3)
  echan_16 <- echan %>%
    filter(annee == annee_encours) %>%
    mutate(dep_equip = (dep_equip/population *1000),subvention_204 = (subvention_204/population *1000),autre_dep_invest = (autre_dep_invest/population *1000)) %>%
    select(dep_equip,subvention_204,autre_dep_invest) %>%
    mutate(nom="echan") %>%
    gather("variables","montant",1:3)

  # Donnees pour l'entite

  base_entite <-   entite %>%
    select(annee,population,dep_equip,subvention_204,autre_dep_invest)

  base_entite_moy <- base_entite %>%
    summarise(dep_equip = sum(dep_equip), subvention_204=sum(subvention_204),autre_dep_invest=sum(autre_dep_invest), population = sum(population)) %>%
    mutate(dep_equip = (dep_equip/population *1000),subvention_204 = (subvention_204/population *1000),autre_dep_invest = (autre_dep_invest/population *1000)) %>%
    select(dep_equip,subvention_204,autre_dep_invest) %>%
    mutate(nom= "entite_moy" ) %>%
    gather("variables","montant",1:3)
  base_entite_16 <- base_entite %>%
    filter(annee == annee_encours) %>%
    mutate(dep_equip = (dep_equip/population *1000),subvention_204 = (subvention_204/population *1000),autre_dep_invest = (autre_dep_invest/population *1000)) %>%
    select(dep_equip,subvention_204,autre_dep_invest) %>%
    mutate(nom= "entite") %>%
    gather("variables","montant",1:3)

  # Donnees pour le gr de ref

  base_gr_ref <-   gr_ref %>%
    select(annee,population,dep_equip,subvention_204,autre_dep_invest) %>%
    group_by(annee) %>%
    summarise_all(sum)
  base_gr_ref_moy <- base_gr_ref %>%
    summarise(dep_equip = sum(dep_equip), subvention_204=sum(subvention_204),autre_dep_invest=sum(autre_dep_invest), population = sum(population)) %>%
    mutate(dep_equip = (dep_equip/population *1000),subvention_204 = (subvention_204/population *1000),autre_dep_invest = (autre_dep_invest/population *1000)) %>%
    select(dep_equip,subvention_204,autre_dep_invest) %>%
    mutate(nom="gr_ref_moy") %>%
    gather("variables","montant",1:3)
  base_gr_ref_16 <- base_gr_ref %>%
    filter(annee == annee_encours) %>%
    mutate(dep_equip = (dep_equip/population *1000),subvention_204 = (subvention_204/population *1000),autre_dep_invest = (autre_dep_invest/population *1000)) %>%
    select(dep_equip,subvention_204,autre_dep_invest) %>%
    mutate(nom="gr_ref") %>%
    gather("variables","montant",1:3)

  base <- rbind(echan_16,echan_moy,base_entite_16,base_entite_moy,base_gr_ref_16,base_gr_ref_moy)
  } else if(bp_ba == 2){
    echan <-   echantillon %>%
      select(annee,population,dep_equip_ba,subvention_204_ba,autre_dep_invest_ba) %>%
      group_by(annee) %>%
      summarise_all(sum)
    echan_moy <- echan %>%
      summarise(dep_equip_ba = sum(dep_equip_ba), subvention_204_ba=sum(subvention_204_ba),autre_dep_invest_ba=sum(autre_dep_invest_ba), population = sum(population)) %>%
      mutate(dep_equip = (dep_equip_ba/population *1000),subvention_204 = (subvention_204_ba/population *1000),autre_dep_invest = (autre_dep_invest_ba/population *1000)) %>%
      select(dep_equip,subvention_204,autre_dep_invest) %>%
      mutate(nom="echan_moy") %>%
      gather("variables","montant",1:3)
    echan_16 <- echan %>%
      filter(annee == annee_encours) %>%
      mutate(dep_equip = (dep_equip_ba/population *1000),subvention_204 = (subvention_204_ba/population *1000),autre_dep_invest = (autre_dep_invest_ba/population *1000)) %>%
      select(dep_equip,subvention_204,autre_dep_invest) %>%
      mutate(nom="echan") %>%
      gather("variables","montant",1:3)

    # Donnees pour l'entite

    base_entite <-   entite %>%
      select(annee,population,dep_equip_ba,subvention_204_ba,autre_dep_invest_ba)

    base_entite_moy <- base_entite %>%
      summarise(dep_equip_ba = sum(dep_equip_ba), subvention_204_ba=sum(subvention_204_ba),autre_dep_invest_ba=sum(autre_dep_invest_ba), population = sum(population)) %>%
      mutate(dep_equip = (dep_equip_ba/population *1000),subvention_204 = (subvention_204_ba/population *1000),autre_dep_invest = (autre_dep_invest_ba/population *1000)) %>%
      select(dep_equip,subvention_204,autre_dep_invest) %>%
      mutate(nom= "entite_moy" ) %>%
      gather("variables","montant",1:3)
    base_entite_16 <- base_entite %>%
      filter(annee == annee_encours) %>%
      mutate(dep_equip = (dep_equip_ba/population *1000),subvention_204 = (subvention_204_ba/population *1000),autre_dep_invest = (autre_dep_invest_ba/population *1000)) %>%
      select(dep_equip,subvention_204,autre_dep_invest) %>%
      mutate(nom= "entite") %>%
      gather("variables","montant",1:3)

    # Donnees pour le gr de ref

    base_gr_ref <-   gr_ref %>%
      select(annee,population,dep_equip_ba,subvention_204_ba,autre_dep_invest_ba) %>%
      group_by(annee) %>%
      summarise_all(sum)
    base_gr_ref_moy <- base_gr_ref %>%
      summarise(dep_equip_ba = sum(dep_equip_ba), subvention_204_ba=sum(subvention_204_ba),autre_dep_invest_ba=sum(autre_dep_invest_ba), population = sum(population)) %>%
      mutate(dep_equip = (dep_equip_ba/population *1000),subvention_204 = (subvention_204_ba/population *1000),autre_dep_invest = (autre_dep_invest_ba/population *1000)) %>%
      select(dep_equip,subvention_204,autre_dep_invest) %>%
      mutate(nom="gr_ref_moy") %>%
      gather("variables","montant",1:3)
    base_gr_ref_16 <- base_gr_ref %>%
      filter(annee == annee_encours) %>%
      mutate(dep_equip = (dep_equip_ba/population *1000),subvention_204 = (subvention_204_ba/population *1000),autre_dep_invest = (autre_dep_invest_ba/population *1000)) %>%
      select(dep_equip,subvention_204,autre_dep_invest) %>%
      mutate(nom="gr_ref") %>%
      gather("variables","montant",1:3)

    base <- rbind(echan_16,echan_moy,base_entite_16,base_entite_moy,base_gr_ref_16,base_gr_ref_moy)
  } else if(bp_ba == 3){
    echan <-   echantillon %>%
      select(annee,population,dep_equip,dep_equip_ba,subvention_204,subvention_204_ba,autre_dep_invest_ba,autre_dep_invest,fc_i) %>%
      group_by(annee) %>%
      summarise_all(sum)
    echan_moy <- echan %>%
      summarise(dep_equip = sum(dep_equip + dep_equip_ba), subvention_204=sum(subvention_204 + subvention_204_ba - fc_i),autre_dep_invest=sum(autre_dep_invest + autre_dep_invest_ba), population = sum(population)) %>%
      mutate(dep_equip = (dep_equip/population *1000),subvention_204 = (subvention_204/population *1000),autre_dep_invest = (autre_dep_invest/population *1000)) %>%
      select(dep_equip,subvention_204,autre_dep_invest) %>%
      mutate(nom="echan_moy") %>%
      gather("variables","montant",1:3)
    echan_16 <- echan %>%
      filter(annee == annee_encours) %>%
      mutate(dep_equip = (sum(dep_equip + dep_equip_ba - fc_i)/population *1000),subvention_204 = (sum(subvention_204 + subvention_204_ba)/population *1000),autre_dep_invest = (sum(autre_dep_invest + autre_dep_invest_ba)/population *1000)) %>%
      select(dep_equip,subvention_204,autre_dep_invest) %>%
      mutate(nom="echan") %>%
      gather("variables","montant",1:3)
    
    # Donnees pour l'entite
    
    base_entite <-   entite %>%
      select(annee,population,dep_equip,dep_equip_ba,subvention_204,subvention_204_ba,autre_dep_invest,autre_dep_invest_ba,fc_i)
      
    base_entite_moy <- base_entite %>%
      summarise(dep_equip = sum(dep_equip + dep_equip_ba), subvention_204=sum(subvention_204 + subvention_204_ba - fc_i),autre_dep_invest=sum(autre_dep_invest + autre_dep_invest_ba), population = sum(population)) %>%
      mutate(dep_equip = (dep_equip/population *1000),subvention_204 = (subvention_204/population *1000),autre_dep_invest = (autre_dep_invest/population *1000)) %>%
      select(dep_equip,subvention_204,autre_dep_invest) %>%
      mutate(nom= "entite_moy" ) %>%
      gather("variables","montant",1:3)
    base_entite_16 <- base_entite %>%
      filter(annee == annee_encours) %>%
      mutate(dep_equip = (sum(dep_equip + dep_equip_ba)/population *1000),subvention_204 = (sum(subvention_204 + subvention_204_ba - fc_i)/population *1000),autre_dep_invest = (sum(autre_dep_invest + autre_dep_invest_ba)/population *1000)) %>%
      select(dep_equip,subvention_204,autre_dep_invest) %>%
      mutate(nom= "entite") %>%
      gather("variables","montant",1:3)
    
    # Donnees pour le gr de ref
    
    base_gr_ref <-   gr_ref %>%
      select(annee,population,dep_equip,dep_equip_ba,subvention_204,subvention_204_ba,autre_dep_invest,autre_dep_invest_ba,fc_i) %>%
      group_by(annee) %>%
      summarise_all(sum)
    base_gr_ref_moy <- base_gr_ref %>%
      summarise(dep_equip = sum(dep_equip + dep_equip_ba), subvention_204=sum(subvention_204 + subvention_204_ba - fc_i),autre_dep_invest=sum(autre_dep_invest + autre_dep_invest_ba), population = sum(population)) %>%
      mutate(dep_equip = (dep_equip/population *1000),subvention_204 = (subvention_204/population *1000),autre_dep_invest = (autre_dep_invest/population *1000)) %>%
      select(dep_equip,subvention_204,autre_dep_invest) %>%
      mutate(nom="gr_ref_moy") %>%
      gather("variables","montant",1:3)
    base_gr_ref_16 <- base_gr_ref %>%
      filter(annee == annee_encours) %>%
      mutate(dep_equip = (sum(dep_equip + dep_equip_ba)/population *1000),subvention_204 = (sum(subvention_204 + subvention_204_ba - fc_i)/population *1000),autre_dep_invest = (sum(autre_dep_invest + autre_dep_invest_ba)/population *1000)) %>%
      select(dep_equip,subvention_204,autre_dep_invest) %>%
      mutate(nom="gr_ref") %>%
      gather("variables","montant",1:3)
    
    base <- rbind(echan_16,echan_moy,base_entite_16,base_entite_moy,base_gr_ref_16,base_gr_ref_moy)}
}

#' Grahpique des depenses d'equipements
#'
#' @param data Base de donnee, souvent resultat de la fonction data_graph_equip
#' @param nom_gfp_com Commune ou groupement
#' @param entite base de la com ou du gfp
#' @param etiquette permet de selectionne la base pour le total
#'
#' @import ggplot2
#' @importFrom magrittr %>% 
#' @importFrom forcats fct_relevel 
#'
#' @export

graph_dep_invest <- function(data,entite,nom_gfp_com,etiquette){
  graph <- data %>%
    ggplot() +
    geom_col(aes(y=montant,x=nom,fill = fct_relevel(as.factor(variables),c("autre_dep_invest","subvention_204","dep_equip"))),position = "stack") +
    geom_text(aes(y=montant,x=nom,label = round(data$montant,0)),position = position_stack(vjust = 0.5)) +
    ggtitle("Dépenses d'investisement hors remb de la dette \n en €/hab (moyenne annuelle)") +
    ylab("") +
    xlab("") +
    coord_flip() +
    theme(panel.background = element_rect(fill = "white"),axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.text.y = element_text(size = 15),legend.title = element_blank(),legend.text = element_text(size = 9)) +
    scale_fill_brewer(labels = c(dep_equip = "Dépenses d'équipement",
                                 subvention_204= "Subventions versées",
                                 autre_dep_invest = "Autres dépenses d'investissement"),
                      limits = c("autre_dep_invest","subvention_204","dep_equip"),
                      palette="Blues")

  if(nrow(entite) == 0 ){
    graph  <-   graph + scale_x_discrete(labels = c("gr_ref_moy"=paste0("Groupe de réf \n 2012-",annee_encours),
                                                    "gr_ref"=paste0("Groupe de réf \n",annee_encours),
                                                    "echan_moy" = paste0("Echantillon \n 2012-",annee_encours),
                                                    "echan" =  paste0("Echantillon  ",annee_encours)),
                                         limits = c("gr_ref_moy","gr_ref","echan_moy","echan")) +
      labs(caption = "Traitement OFGL - Source : balances comptables DGFIP")
  } else if(nrow(entite) != 0 & entite$LBUDG == "METRO DE LYON"){
     graph  <-   graph + scale_x_discrete(labels = c("gr_ref_moy"=paste0("Groupe de réf \n 2012-",annee_encours),
                                                     "gr_ref"=paste0("Groupe de réf \n",annee_encours),
                                                     "echan_moy" = paste0("Echantillon \n 2012-",annee_encours),
                                                     "echan" =  paste0("Echantillon ",annee_encours),
                                                     "entite_moy" =  paste0("Métro Lyon","\n 2015-",annee_encours),
                                                     "entite" = paste0("Métro Lyon"," ",annee_encours)),
                                          limits = c(c("gr_ref_moy","gr_ref","entite_moy","entite","echan_moy","echan"))) +
       labs(caption = paste0("Traitement OFGL - Source : balances comptables DGFIP \nCollectivité sélectionnée :",entite$LBUDG))
    } else if(nrow(entite) != 0 ){graph  <-   graph + scale_x_discrete(labels = c("gr_ref_moy"=paste0("Groupe de réf \n 2012-",annee_encours),
                                                                              "gr_ref"=paste0("Groupe de réf \n",annee_encours),
                                                                              "echan_moy" = paste0("Echantillon \n 2012-",annee_encours),
                                                                              "echan" =  paste0("Echantillon ",annee_encours),
                                                                              "entite_moy" =  paste0(nom_gfp_com,"\n 2012-",annee_encours),
                                                                              "entite" = paste0(nom_gfp_com," ",annee_encours)),
                                                                   limits = c(c("gr_ref_moy","gr_ref","entite_moy","entite","echan_moy","echan"))) +
    labs(caption = paste0("Traitement OFGL - Source : balances comptables DGFIP \nCollectivité sélectionnée :",entite$LBUDG))}
    
  graph <- graph +
    geom_text(data= etiquette,aes(y=montant,x=nom,label = round(montant,0)),position = position_stack(vjust = 1.03),angle = -90)

  return(graph)
}
