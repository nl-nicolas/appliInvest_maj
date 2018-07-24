#' Etape data pour le details des sub
#'
#' Permet d'obetinr la base du graphique sur les subventions
#'
#'@param echantillon Base de l'echnatillon GFP, com,dep,reg
#'@param entite Base de l'entite selectionnable type gfp ou com etc
#'@param gr_ref Base du gr de ref
#'@param annee_encours permet de choisir les annees
#'
#' @import dplyr
#' @import tidyr
#' @importFrom  magrittr %>% 
#'
#'@return une bdd
#'@export

data_sub_204 <- function(echantillon,entite,gr_ref,annee_encours){
  echan <-   echantillon %>%
    select(annee,population,sub_204_etat,sub_204_bc,sub_204_autres,sub_204_prive,autre_sub_204) %>%
    group_by(annee) %>%
    summarise_all(sum)
  echan_16 <- echan %>%
    filter(annee %in% annee_encours) %>%
    summarise(sub_204_etat = sum(sub_204_etat), sub_204_bc=sum(sub_204_bc),sub_204_autres=sum(sub_204_autres),sub_204_prive = sum(sub_204_prive),autre_sub_204 = sum(autre_sub_204), population = sum(population)) %>%
    mutate(sub_204_etat = (sub_204_etat/population *1000),sub_204_bc = (sub_204_bc/population *1000),sub_204_autres = (sub_204_autres/population *1000),sub_204_prive = (sub_204_prive/population *1000),autre_sub_204 = (autre_sub_204/population *1000)) %>%
    mutate(nom="echan") %>%
    select(-population) %>%
    gather("variables","montant",-nom)

  # Donnees pour l'entite

  base_entite <-   entite %>%
    select(annee,population,sub_204_etat,sub_204_bc,sub_204_autres,sub_204_prive,autre_sub_204) %>%
    group_by(annee) %>%
    summarise_all(sum)
  base_entite_16 <- base_entite %>%
    filter(annee %in% annee_encours) %>%
    summarise(sub_204_etat = sum(sub_204_etat), sub_204_bc=sum(sub_204_bc),sub_204_autres=sum(sub_204_autres),sub_204_prive = sum(sub_204_prive),autre_sub_204 = sum(autre_sub_204), population = sum(population)) %>%
    mutate(sub_204_etat = (sub_204_etat/population *1000),sub_204_bc = (sub_204_bc/population *1000),sub_204_autres = (sub_204_autres/population *1000),sub_204_prive = (sub_204_prive/population *1000),autre_sub_204 = (autre_sub_204/population *1000)) %>%
    mutate(nom="entite") %>%
    select(-population) %>%
    gather("variables","montant",-nom)


  # Donnees pour le gr de ref

  base_gr_ref <-   gr_ref %>%
    select(annee,population,sub_204_etat,sub_204_bc,sub_204_autres,sub_204_prive,autre_sub_204) %>%
    group_by(annee) %>%
    summarise_all(sum)
  base_gr_ref_16 <- base_gr_ref %>%
    filter(annee %in% annee_encours) %>%
    summarise(sub_204_etat = sum(sub_204_etat), sub_204_bc=sum(sub_204_bc),sub_204_autres=sum(sub_204_autres),sub_204_prive = sum(sub_204_prive),autre_sub_204 = sum(autre_sub_204), population = sum(population)) %>%
    mutate(sub_204_etat = (sub_204_etat/population *1000),sub_204_bc = (sub_204_bc/population *1000),sub_204_autres = (sub_204_autres/population *1000),sub_204_prive = (sub_204_prive/population *1000),autre_sub_204 = (autre_sub_204/population *1000)) %>%
    mutate(nom="gr_ref") %>%
    select(-population) %>%
    gather("variables","montant",-nom)


  base <- rbind(echan_16,base_entite_16,base_gr_ref_16)

}

#' Grahpique des subvention
#'
#' @param data Base de donnee, souvent resultat de la fonction data_graph_equip
#' @param nom_gfp_com Commune ou groupement
#' @param entite base de la com ou du gfp
#' @param quel_annee permet de choisir si 2017 ou 2012-2017
#'
#' @import ggplot2
#' @importFrom  magrittr %>% 
#' @importFrom forcats fct_relevel
#'
#' @export

graph_sub_204 <- function(data,entite,nom_gfp_com,quel_annee){
  graph <- data %>%
    ggplot(aes(y=montant,x=nom,fill = fct_relevel(as.factor(variables),c("autre_sub_204","sub_204_prive","sub_204_autres","sub_204_bc","sub_204_etat")))) +
    geom_col(position = "stack") +
    geom_text(aes(label = round(data$montant,0)),position = position_stack(vjust = 0.5)) +
    ggtitle("Subventions versées, en fonction des bénéficaires,\nBudgets principaux, en €/hab") +
    ylab("") +
    xlab("") +
    theme(panel.background = element_rect(fill = "#f7fbff", colour = "grey"), panel.grid.major = element_line(color  = "grey",size = 0.25),panel.grid.minor = element_line(color  = "grey",size = 0.25),axis.text.x = element_text(size=15),legend.title = element_blank(),legend.text = element_text(size=10)) +
    scale_fill_brewer(labels = c("sub_204_etat" = "Etat",
                                 "sub_204_bc"="Bloc communal",
                                 "sub_204_autres" = "Autres organismes publics ou collectivités",
                                 "sub_204_prive"="Organismes privés",
                                 "autre_sub_204"="Autres"),
                      limits = c("autre_sub_204","sub_204_prive","sub_204_autres","sub_204_bc","sub_204_etat"),
                      palette="YlGnBu")

  if(nrow(entite) == 0 ){
    graph  <-   graph + scale_x_discrete(labels = c("gr_ref"="Groupe de réf",
                                                    "echan" ="Echantillon "),
                                         limits =c("echan","gr_ref")) +
      labs(caption = "Traitement OFGL - Source : balances comptables DGFIP")
  }
  else if(nrow(entite) != 0 ){graph  <-   graph + scale_x_discrete(labels = c("gr_ref"="Groupe de réf",
                                                                              "echan" ="Echantillon",
                                                                              "entite" = nom_gfp_com),
                                                                   limits =c("echan","entite","gr_ref")) +
  labs(caption = paste0("Traitement OFGL - Source : balances comptables DGFIP \nCollectivité sélectionnée :",entite$LBUDG))
  }

  if(quel_annee == 1){
    graph <- graph + labs(subtitle = "En 2017")
  }else if(quel_annee == 2){
    graph <- graph + labs(subtitle = "De 2012 à 2017 (moyenne annuelle)")}


  return(graph)
}


#' Etape data pour le details des sub pour les reg
#'
#' Permet d'obetinr la base du graphique sur les subventions pour les reg
#'
#'@param echantillon Base de l'echnatillon GFP, com,dep,reg
#'@param entite Base de l'entite selectionnable type gfp ou com etc
#'@param gr_ref Base du gr de ref
#'@param annee_encours permet de choisir les annees
#'
#' @import dplyr
#' @import tidyr
#' @importFrom  magrittr %>% 
#'
#'@return une bdd
#'@export

data_sub_204_reg <- function(echantillon,entite,gr_ref,annee_encours){
  echan <-   echantillon %>%
    select(annee,population,sub_204_etat,sub_204_bc,sub_204_transport,sub_204_autres,sub_204_prive,autre_sub_204) %>%
    group_by(annee) %>%
    summarise_all(sum)
  echan_16 <- echan %>%
    filter(annee %in% annee_encours) %>%
    summarise(sub_204_etat = sum(sub_204_etat), sub_204_bc=sum(sub_204_bc),sub_204_transport = sum(sub_204_transport),sub_204_autres=sum(sub_204_autres),sub_204_prive = sum(sub_204_prive),autre_sub_204 = sum(autre_sub_204), population = sum(population)) %>%
    mutate(sub_204_etat = (sub_204_etat/population *1000),sub_204_bc = (sub_204_bc/population *1000),sub_204_transport=(sub_204_transport/population *1000),sub_204_autres = (sub_204_autres/population *1000),sub_204_prive = (sub_204_prive/population *1000),autre_sub_204 = (autre_sub_204/population *1000)) %>%
    mutate(nom="echan") %>%
    select(-population) %>%
    gather("variables","montant",-nom)
  
  # Donnees pour l'entite
  
  base_entite <-   entite %>%
    select(annee,population,sub_204_etat,sub_204_bc,sub_204_transport,sub_204_autres,sub_204_prive,autre_sub_204) %>%
    group_by(annee) %>%
    summarise_all(sum)
  base_entite_16 <- base_entite %>%
    filter(annee %in% annee_encours) %>%
    summarise(sub_204_etat = sum(sub_204_etat), sub_204_bc=sum(sub_204_bc),sub_204_transport = sum(sub_204_transport),sub_204_autres=sum(sub_204_autres),sub_204_prive = sum(sub_204_prive),autre_sub_204 = sum(autre_sub_204), population = sum(population)) %>%
    mutate(sub_204_etat = (sub_204_etat/population *1000),sub_204_bc = (sub_204_bc/population *1000),sub_204_transport=(sub_204_transport/population *1000),sub_204_autres = (sub_204_autres/population *1000),sub_204_prive = (sub_204_prive/population *1000),autre_sub_204 = (autre_sub_204/population *1000)) %>%
    mutate(nom="entite") %>%
    select(-population) %>%
    gather("variables","montant",-nom)
  
  
  # Donnees pour le gr de ref
  
  base_gr_ref <-   gr_ref %>%
    select(annee,population,sub_204_etat,sub_204_bc,sub_204_transport,sub_204_autres,sub_204_prive,autre_sub_204) %>%
    group_by(annee) %>%
    summarise_all(sum)
  base_gr_ref_16 <- base_gr_ref %>%
    filter(annee %in% annee_encours) %>%
    summarise(sub_204_etat = sum(sub_204_etat), sub_204_bc=sum(sub_204_bc),sub_204_transport = sum(sub_204_transport),sub_204_autres=sum(sub_204_autres),sub_204_prive = sum(sub_204_prive),autre_sub_204 = sum(autre_sub_204), population = sum(population)) %>%
    mutate(sub_204_etat = (sub_204_etat/population *1000),sub_204_bc = (sub_204_bc/population *1000),sub_204_transport=(sub_204_transport/population *1000),sub_204_autres = (sub_204_autres/population *1000),sub_204_prive = (sub_204_prive/population *1000),autre_sub_204 = (autre_sub_204/population *1000)) %>%
    mutate(nom="gr_ref") %>%
    select(-population) %>%
    gather("variables","montant",-nom)
  
  
  base <- rbind(echan_16,base_entite_16,base_gr_ref_16)
  
}

#' Grahpique des subvention
#'
#' @param data Base de donnee, souvent resultat de la fonction data_graph_equip
#' @param nom_gfp_com Commune ou groupement
#' @param entite base de la com ou du gfp
#' @param quel_annee permet de choisir si 2017 ou 2012-2017
#'
#' @import ggplot2
#' @importFrom  magrittr %>% 
#' @importFrom forcats fct_relevel
#'
#' @export

graph_sub_204_reg <- function(data,entite,nom_gfp_com,quel_annee){
  graph <- data %>%
    ggplot(aes(y=montant,x=nom,fill = fct_relevel(as.factor(variables),c("autre_sub_204","sub_204_prive","sub_204_autres","sub_204_transport","sub_204_bc","sub_204_etat")))) +
    geom_col(position = "stack") +
    geom_text(aes(label = round(data$montant,0)),position = position_stack(vjust = 0.5)) +
    ggtitle("Subventions versées, en fonction des bénéficaires,\nBudgets principaux, en €/hab") +
    ylab("") +
    xlab("") +
    theme(panel.background = element_rect(fill = "#f7fbff", colour = "grey"), panel.grid.major = element_line(color  = "grey",size = 0.25),panel.grid.minor = element_line(color  = "grey",size = 0.25),axis.text.x = element_text(size=15),legend.title = element_blank(),legend.text = element_text(size=10)) +
    scale_fill_brewer(labels = c("sub_204_etat" = "Etat",
                                 "sub_204_bc"="Bloc communal",
                                 "sub_204_transport" ="Organismes de transport",
                                 "sub_204_autres" = "Autres organismes publics ou collectivités",
                                 "sub_204_prive"="Organismes privés",
                                 "autre_sub_204"="Autres"),
                      limits = c("autre_sub_204","sub_204_prive","sub_204_autres","sub_204_transport","sub_204_bc","sub_204_etat"),
                      palette="YlGnBu")
  
  if(nrow(entite) == 0 ){
    graph  <-   graph + scale_x_discrete(labels = c("gr_ref"="Groupe de réf",
                                                    "echan" ="Echantillon "),
                                         limits =c("echan","gr_ref")) +
      labs(caption = "Traitement OFGL - Source : balances comptables DGFIP")
  }
  else if(nrow(entite) != 0 ){graph  <-   graph + scale_x_discrete(labels = c("gr_ref"="Groupe de réf",
                                                                              "echan" ="Echantillon",
                                                                              "entite" = nom_gfp_com),
                                                                   limits =c("echan","entite","gr_ref")) +
    labs(caption = paste0("Traitement OFGL - Source : balances comptables DGFIP \nCollectivité sélectionnée :",entite$LBUDG))
  }
  
  if(quel_annee == 1){
    graph <- graph + labs(subtitle = "En 2017")
  }else if(quel_annee == 2){
    graph <- graph + labs(subtitle = "De 2012 à 2017 (moyenne annuelle)")}
  
  
  return(graph)
}