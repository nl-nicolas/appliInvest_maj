if(!requireNamespace("tidyverse"))
  install.packages("tidyverse")
if(!requireNamespace("DT"))
  install.packages("DT")
if(!requireNamespace("shiny"))
  install.packages("shiny")
if(!requireNamespace("shinydashboard"))
  install.packages("shinydashboard")
if(!requireNamespace("shinyWidgets"))
  install.packages("shinyWidgets")
if(!requireNamespace("knitr"))
  install.packages("knitr")
if(!requireNamespace("RColorBrewer"))
  install.packages("RColorBrewer")
if(!requireNamespace("shinythemes"))
  install.packages("shinythemes")
if(!requireNamespace("forcats"))
  install.packages("forcats")
if(!requireNamespace("scales"))
  install.packages("scales")
if(!requireNamespace("plotly"))
  install.packages("plotly")
if(!requireNamespace("shinycssloaders"))
  install.packages("shinycssloaders")


library(tidyverse)
library(DT)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(knitr)
library(RColorBrewer)
library(shinythemes)
library(forcats)
library(scales)
library(plotly)
library(appliInvest)
library(shinycssloaders)
library(shinyjs)
library(stringr)
library(httr)
library(jsonlite)
library(htmltools)

annee_encours <- 2017

base_invest <-  appliInvest::base_invest_com %>%
  select(nom_dep,strate16,nom_com,montagne,touristique,annee,indice_pop) %>%
  filter(annee == 2017) %>%
  mutate(nom_com= as.character(nom_com))


base_invest_gfp <- appliInvest::base_invest_gfp %>%
  filter(annee == annee_encours)


base_invest_dep <- appliInvest::base_invest_dep
base_invest_dep$urb_rur_2017 <-  as.character(base_invest_dep$urb_rur_2017)




# list_dep <- read_tsv("bdd/list_dep.txt")
# 
# list_com <- read_csv2("bdd/list_com.csv")
# 
# nb_bp_ba <- read_csv2("bdd/nombre_bp_ba.csv")
# 
# 
# pop_reg <- read_csv2("bdd/pop_reg.csv")
# load("data.Rdata")

load("bdd/list_dep.Rdata")
base_invest_com_gr_ref <- read_csv2("bdd/gr_ref_com.csv")

list_dep$nom_dep <- as.character(list_dep$nom_dep) 
list_dep$nom_reg <- as.character(list_dep$nom_reg) 
list_dep$nom_reg_accent <- as.character(list_dep$nom_reg_accent) 
list_dep$nom_dep_accent <- as.character(list_dep$nom_dep_accent) 

list_dep_com <- list_dep %>% filter(!nom_dep == "METRO LYON")

base_invest <- list_dep_com %>% select(nom_dep,nom_dep_accent) %>% right_join(base_invest,by="nom_dep")

list_dep_dep <- list_dep %>%
  filter(!nom_reg_accent %in% c("Guyane","Martinique"))


list_reg <- list_dep %>% select(nom_reg_accent,nom_reg) %>% distinct()


base_invest_reg <- appliInvest::base_invest_reg %>%
  mutate_if(is.factor,as.character) %>%
  mutate(population = as.integer(str_replace_all(population," ","")))

# call_api <- function(q){
#   
#     jean <- stringr::str_replace_all( q ,pattern = " ",replacement = "%20")
#     
#   url <- paste0("http://vps546862.ovh.net:8000/dep?q=",jean)
#   res <- GET(url)
#     res <- res$content %>%
#       rawToChar() %>%
#       fromJSON() 
#     return(res)
#   
# }
# 
# call_api_t <- function(q){
#   
#   jean <- stringr::str_replace_all( q ,pattern = " ",replacement = "%20")
#   
#     url <- paste0("http://apiplumber:8000/dep?q=",paste0(jean ,collapse = ","))
#     
#   res <- GET(url)
#   
#   res$content %>%
#    rawToChar() %>%
#      fromJSON() 
# }

# call_api_com <- function(q){
#   
#   jean <- stringr::str_replace_all( q ,pattern = " ",replacement = "%20")
#   
#   url <- paste0("http://vps546862.ovh.net:8000/com?q=",paste0(jean ,collapse = ","))
#   
#   res <- GET(url)
#   
#   res$content %>%
#     rawToChar() %>%
#     fromJSON() 
# }


