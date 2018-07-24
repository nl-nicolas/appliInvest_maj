#'Etape data pour graph equip
#'
#'Permet d'obtenir la bonne base de donnees pour le graphique sur les depenses d'equipement
#'
#'@param echantillon Base de l'echnatillon GFP, com,dep,reg
#'@param entite Base de l'entite selectionnable type gfp ou com etc
#'@param gr_ref Base du gr de ref
#'@param gfp_ou_com Permet de travailler soit sur les gfp 1, soit sur les com 2
#'
#' @import dplyr
#' @import tidyr
#' @importFrom  magrittr %>% 
#'
#'@return une bdd
#'@export
data_graph_equip <- function(echantillon,entite,gr_ref,gfp_ou_com){

  # Donnees pour l'echantillon
  if(gfp_ou_com == 2){
    echan <-   echantillon %>%
      select(annee,population,dep_equip,dep_equip_ba) %>%
      group_by(annee) %>%
      summarise_all(sum)
    echan_16 <- echan %>%
      filter(annee == annee_encours) %>%
      mutate(dep_equip = (dep_equip/population *1000),dep_equip_ba = (dep_equip_ba/population *1000)) %>%
      select(dep_equip,dep_equip_ba) %>%
      mutate(nom="echan") %>%
      gather("variables","montant",1:2)

    # Donnees pour l'entite

    base_entite <-   entite %>%
      select(annee,population,dep_equip,dep_equip_ba)
    base_entite_16 <- base_entite %>%
      filter(annee == annee_encours) %>%
      mutate(dep_equip = (dep_equip/population *1000),dep_equip_ba = (dep_equip_ba/population *1000)) %>%
      select(dep_equip,dep_equip_ba) %>%
      mutate(nom= "entite") %>%
      gather("variables","montant",1:2)

    # Donnees pour le gr de ref

    base_gr_ref <-   gr_ref %>%
      select(annee,population,dep_equip,dep_equip_ba) %>%
      filter(!is.na(population)) %>%
      group_by(annee) %>%
      summarise_all(sum)
    base_gr_ref_16 <- base_gr_ref %>%
      filter(annee == annee_encours) %>%
      mutate(dep_equip = (dep_equip/population * 1000),dep_equip_ba = (dep_equip_ba/population *1000)) %>%
      select(dep_equip,dep_equip_ba) %>%
      mutate(nom="gr_ref") %>%
      gather("variables","montant",1:2)

    base <- rbind(echan_16,base_entite_16,base_gr_ref_16)

  }else if(gfp_ou_com == 1){

    echan <-   echantillon %>%
      select(annee,population,dep_equip,dep_equip_ba) %>%
      group_by(annee) %>%
      summarise_all(sum)
    echan_moy <- echan %>%
      summarise(dep_equip = sum(dep_equip), dep_equip_ba=sum(dep_equip_ba), population = sum(population)) %>%
      mutate(dep_equip = (dep_equip/population *1000),dep_equip_ba = (dep_equip_ba/population *1000)) %>%
      select(dep_equip,dep_equip_ba) %>%
      mutate(nom="echan_moy") %>%
      gather("variables","montant",1:2)
    echan_16 <- echan %>%
      filter(annee == annee_encours) %>%
      mutate(dep_equip = (dep_equip/population *1000),dep_equip_ba = (dep_equip_ba/population *1000)) %>%
      select(dep_equip,dep_equip_ba) %>%
      mutate(nom="echan") %>%
      gather("variables","montant",1:2)

    # Donnees pour l'entite

    base_entite <-   entite %>%
      select(annee,population,dep_equip,dep_equip_ba)
    base_entite_moy <- base_entite %>%
      summarise(dep_equip = sum(dep_equip), dep_equip_ba=sum(dep_equip_ba), population = sum(population)) %>%
      mutate(dep_equip = (dep_equip/population *1000),dep_equip_ba = (dep_equip_ba/population *1000)) %>%
      select(dep_equip,dep_equip_ba) %>%
      mutate(nom= "entite_moy" ) %>%
      gather("variables","montant",1:2)
    base_entite_16 <- base_entite %>%
      filter(annee == annee_encours) %>%
      mutate(dep_equip = (dep_equip/population *1000),dep_equip_ba = (dep_equip_ba/population *1000)) %>%
      select(dep_equip,dep_equip_ba) %>%
      mutate(nom= "entite") %>%
      gather("variables","montant",1:2)

    # Donnees pour le gr de ref

    base_gr_ref <-   gr_ref %>%
      select(annee,population,dep_equip,dep_equip_ba) %>%
      group_by(annee) %>%
      summarise_all(sum)
    base_gr_ref_moy <- base_gr_ref %>%
      summarise(dep_equip = sum(dep_equip), dep_equip_ba=sum(dep_equip_ba), population = sum(population)) %>%
      mutate(dep_equip = (dep_equip/population *1000),dep_equip_ba = (dep_equip_ba/population *1000)) %>%
      select(dep_equip,dep_equip_ba) %>%
      mutate(nom="gr_ref_moy") %>%
      gather("variables","montant",1:2)
    base_gr_ref_16 <- base_gr_ref %>%
      filter(annee == annee_encours) %>%
      mutate(dep_equip = (dep_equip/population * 1000),dep_equip_ba = (dep_equip_ba/population *1000)) %>%
      select(dep_equip,dep_equip_ba) %>%
      mutate(nom="gr_ref") %>%
      gather("variables","montant",1:2)

    base <- rbind(echan_16,echan_moy,base_entite_16,base_entite_moy,base_gr_ref_16,base_gr_ref_moy)

  }else{
    stop("gfp_ou_com 1 pour moyenne et 2016 ou 2 pour 2016")
  }
  return(base)
}

#' Grahpique des depenses d'equipements
#'
#' @param data Base de donnee, souvent resultat de la fonction data_graph_equip
#' @param nom_gfp_com Commune ou groupement
#' @param gfp_ou_com 1 pour commune 2 pour gfp
#' @param entite base de la com ou du gfp
#' @param etiquette permet de selectionne la base pour le total
#' @param gfp_ou_bc permet de modifier le titre
#'
#' @importFrom  magrittr %>% 
#' @import ggplot2
#' @importFrom forcats fct_relevel
#'
#' @export
graph_dep_equip <- function(data,entite,nom_gfp_com,gfp_ou_com,etiquette,gfp_ou_bc){
  graph <- data %>%
    ggplot() +
    geom_col(aes(y=montant,x=nom,fill = fct_relevel(as.factor(variables),c("dep_equip_ba","dep_equip")))) +
    geom_text(aes(y=montant,x=nom,label = round(data$montant,0)),position = position_stack(vjust = 0.5)) +
    ylab("") +
    xlab("") +
    coord_flip() +
    theme(panel.background = element_rect(fill = "white"),axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.text.y = element_text(size = 15),legend.title = element_blank(),legend.text = element_text(size = 9)) +
    scale_fill_brewer(palette="Blues",labels= c("dep_equip" = "Budgets principaux", "dep_equip_ba" = "Budgets annexes"),limits = c("dep_equip","dep_equip_ba"),direction = -1)

  if(gfp_ou_com == 1){
    graph <- graph + ggtitle("Dépenses d'équipement en €/hab \n(moyenne annuelle)")

  }else if(gfp_ou_com == 2){
    graph <- graph + ggtitle(paste0("Dépenses d'équipement en €/hab en ",annee_encours))

  }


  if(nrow(entite) == 0 & gfp_ou_com == 1){
    graph  <-   graph + scale_x_discrete(labels = c("gr_ref_moy"=paste0("Groupe de réf \n 2014-",annee_encours),
                                                    "gr_ref"="Groupe de réf 2017",
                                                    "echan_moy" = paste0("Echantillon \n 2014-",annee_encours),
                                                    "echan" = "Echantillon  2017"),
                                         limits = c("gr_ref_moy","gr_ref","echan_moy","echan")) +
      labs(caption = "Traitement OFGL - Source : balances comptables DGFIP")

  }
  else if(nrow(entite) != 0 & gfp_ou_com == 1){graph  <-   graph +
    labs(caption = paste0("Traitement OFGL - Source : balances comptables DGFIP \n","Collectivité sélectionée : ",entite$LBUDG)) +
    scale_x_discrete(labels = c("gr_ref_moy"=paste0("Groupe de réf \n 2014-",annee_encours),
                                "gr_ref"="Groupe de réf 2017",
                                "echan_moy" = paste0("Echantillon \n 2014-",annee_encours),
                                "echan" = "Echantillon 2017",
                                "entite_moy" =  paste0(nom_gfp_com,"\n 2014-",annee_encours),
                                "entite" = paste0(nom_gfp_com," 2017")),
                     limits = c(c("gr_ref_moy","gr_ref","entite_moy","entite","echan_moy","echan")))

  }else if(nrow(entite) == 0 & gfp_ou_com == 2){
    graph  <-   graph + scale_x_discrete(labels = c("gr_ref"="Groupe de réf",
                                                    "echan" ="Echantillon "),
                                         limits = c("gr_ref","echan")) +
      labs(caption = "Traitement OFGL - Source : balances comptables DGFIP")
  }else if(nrow(entite) != 0 & gfp_ou_com == 2){
    graph  <-   graph + scale_x_discrete(labels = c("gr_ref"="Groupe de réf",
                                                    "echan" =  "Echantillon ",
                                                    "entite" = nom_gfp_com),
                                         limits = c("gr_ref","entite","echan")) +
      labs(caption = paste0("Traitement OFGL - Source : balances comptables DGFIP \n","Collectivité sélectionée : ",entite$LBUDG))

  }
  if(gfp_ou_bc == 0){
    graph <- graph + labs(subtitle ="Groupements")
  } else if(gfp_ou_bc == 1){
    graph <- graph +
      labs(subtitle ="Bloc communal (groupements et leurs communes membres)") 
      
  }else if(gfp_ou_bc == 3){
    graph <- graph
  }
  graph <- graph +
    geom_text(data= etiquette,aes(y=montant,x=nom,label = round(montant,0)),position = position_stack(vjust = 1.03),angle = -90)

  return(graph)
}
