library(shiny)
source('global.R')

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  
  # les values pour les observes et les étapes data
  values <- reactiveValues()#reg = NULL,dep= NULL,coms = NULL,bp_ba2 = NULL,var_select = NULL,
  #                          reg_gfp = NULL,dep_gfp= NULL,coms_gfp = NULL,bp_ba2_gfp = NULL,var_select_gfp = NULL,
  #                          reg_deps = NULL,strate_dep = NULL,dep_deps= NULL,coms_deps = NULL,bp_ba_dep_2 = NULL,dfm = NULL,dpu = NULL)

observe({
  values$reg_deps <- input$reg_echan_deps
  values$coms_deps <- input$mon_dep
  values$bp_ba_dep <- input$bp_ba_dep2
  values$strate_dep <- input$pop_dep
})
observe({
  #  if(!is.null(input$dfm) & input$dfm == 1 & !is.null(input$dpu) & input$dpu == 1){
  #   mes_dep <- list_dep %>% filter(nom_reg %in% c(values$reg_deps) & !nom_dep %in% c("Rhone","Guyane","Martinique"))
  # if(input$dfm == 1){
  #    mes_dep <- list_dep %>% filter(nom_reg %in% c(values$reg_deps) & !nom_dep %in% c("Rhone","Guyane","Martinique") & DFM == 1)
  #  }else if(input$dpu == 1){
  #    mes_dep <- list_dep %>% filter(nom_reg %in% c(values$reg_deps) & !nom_dep %in% c("Rhone","Guyane","Martinique") & DPU == 1)
  #  }else(
  #    mes_dep <- list_dep %>% filter(nom_reg %in% c(values$reg_deps) & !nom_dep %in% c("Rhone","Guyane","Martinique") & DPU == 0 & DFM == 0)
  #  )
  list_dep <- base_invest_dep %>% select(nom_dep,urb_rur_2017) %>% distinct() %>% right_join(list_dep,by="nom_dep")
  mes_dep <- list_dep %>% filter(nom_reg_accent %in% c(values$reg_deps) & !nom_dep %in% c("Rhone","Guyane","Martinique","METRO LYON") & strate16 %in% c(values$strate_dep) & urb_rur_2017 %in% c(input$dpu_dfm))
  values$dep_deps <- mes_dep$nom_dep_accent
  
  # updatePickerInput(session,
  #                   inputId = "mes_dep_selec_deps",
  #                   choices = sort(mes_dep$nom_dep_accent),
  #                   selected = sort(mes_dep$nom_dep_accent))
})

nom_dep_dep <- reactive({
  
  list_dep_ssaccent <- list_dep %>%
    filter(nom_dep_accent %in% c(values$dep_deps)) %>%
    select(nom_dep,nom_reg)
  
  list_dep_ssaccent
})

nom_dep_indiv <- reactive({
  
  list_dep_ssaccent <- list_dep %>%
    filter(nom_dep_accent %in% c(values$coms_deps)) %>%
    select(nom_dep)
  
  list_dep_ssaccent
})

# selection de l'entite
# observe({
#   mon_dep <- list_dep
#   
#   
#   
#   updateSelectizeInput(session,
#                        inputId = "mon_dep",
#                        server = TRUE,
#                        choices = sort(mon_dep$nom_dep_accent),
#                        options = list(
#                          maxOptions = 1000,
#                          label = mon_dep$nom_dep_accent,
#                          render = I(
#                            '{
#                            option: function(item, escape) {
#                            return "<div><strong>" + escape(item.value) + "</strong></div>"
#                            }
# }')),
#                          selected = "vide"
#                          ) # me permet de rendre la selection vide..
#   })

# observe({
#   if(length(input$cas_spe_deps) == 0){
#     nb_ligne1 <- base_invest_dep %>%
#       filter(nom_dep %in% c(nom_dep_dep()$nom_dep) & urb_rur_2017 %in% c(input$dpu_dfm) & strate16 %in% c(input$pop_dep)) #& cas_spe %in% c(0,input$cas_spe_deps))
#     nb_ligne <- nrow(nb_ligne1)
#   }else if(length(input$cas_spe_deps) != 0){
#     nb_ligne1 <- base_invest_dep %>%
#       filter(nom_dep %in% c(nom_dep_dep()$nom_dep) & urb_rur_2017 %in% c(input$dpu_dfm) & strate16 %in% c(input$pop_dep)) #& cas_spe %in% c(0,input$cas_spe_deps))
#     nb_ligne2 <- base_invest_dep %>%
#       filter(nom_dep %in% c(input$cas_spe_deps))
#     nb_ligne <- nrow(nb_ligne1) + nrow(nb_ligne2)}
#   
#   
#   nb_ligne_dep <- base_invest_dep %>%
#     filter(nom_dep %in% c(nom_dep_indiv()$nom_dep))
#   nb_ligne_dep_echan <- nrow(data_echan_dep()) + nrow(nb_ligne_dep)
#   
#   if(nrow(data_dep()) == 0){
#     if(nb_ligne != nb_ligne_dep_echan){
#       showElement(id = "go_dep",time = 0.1)
#     }else{
#       hide(id = "go_dep",time= 0.1)}
#   }else if(nrow(data_dep()) != 0){
#     if(nrow(nb_ligne_dep) == 0){
#       showElement(id = "go_dep",time = 0.1)
#     }else{
#       if(nb_ligne != nrow(data_echan_dep()) | nb_ligne_dep$nom_dep != data_dep()$nom_dep){
#         showElement(id = "go_dep",time = 0.1)
#       }else{
#         hide(id = "go_dep",time= 0.1)}}
#   }
# })


data_echan_dep <- eventReactive(input$go_dep,{
  
  if(length(input$cas_spe_deps) == 0){
    base <-  base_invest_dep %>%
      filter(nom_dep %in% c(nom_dep_dep()$nom_dep) & urb_rur_2017 %in% c(input$dpu_dfm) & !nom_dep %in% c("Rhone","METRO LYON","Martinique","Guyane") & strate16 %in% c(input$pop_dep))
  }else if(length(input$cas_spe_deps) != 0){
    base2 <- base_invest_dep %>%
      filter(nom_dep %in% c(input$cas_spe_deps))
    base <-  base_invest_dep %>%
      filter(nom_dep %in% c(nom_dep_dep()$nom_dep) & urb_rur_2017 %in% c(input$dpu_dfm) & !nom_dep %in% c("Rhone","METRO LYON","Martinique","Guyane") & strate16 %in% c(input$pop_dep))
    data <- rbind(base,base2)
  }
})


observeEvent(input$go_dep,{
  list_dep2 <- list_dep %>% select(nom_dep,nom_reg)
  values$nb_reg <- data_echan_dep() %>%
    left_join(list_dep,by="nom_dep") %>%
    pull(nom_reg) %>% n_distinct()
})

##Etape data pour la selection du dep
data_dep <- eventReactive(input$go_dep,{
  
  base_invest_dep %>%
    filter(nom_dep %in% c(nom_dep_indiv()$nom_dep))
  
})

#Etape data pour creer le groupe de reference
data_gr_ref_dep <- eventReactive(input$go_dep,{
  
  data <- base_invest_dep %>%
    filter(!nom_dep %in% c("Paris","Guadeloupe","La Reunion","Rhone","METRO LYON","Martinique","Guyane"))
  
  
})

data_infobox_dep <- reactive({
  echan <- data_echan_dep() %>%
    filter(annee == annee_encours) %>%
    summarise(pop_pour = sum(population)/(68732961 - 1374964)*100, pop_nb = round(sum(population)/1000000,2), dep_equip_t = sum(dep_invest + dep_invest_ba-fc_i)/1000, dep_equip_pour = (sum(dep_invest + dep_invest_ba-fc_i))/9902288*100,dep_equip_ba_bp = (sum(dep_invest_ba-fc_i)/(sum(dep_invest + dep_invest_ba-fc_i)))*100)
})
# pour les info box sur l'échantillon

output$info_echan_box1_dep <- renderUI({
  
  data <- data_echan_dep() %>%
    filter(annee == annee_encours)
  if("Martinique" %in% data_echan_dep()$nom_dep | "Guyane" %in% data_echan_dep()$nom_dep | "METRO LYON" %in% data_echan_dep()$nom_dep){
    p(nrow(data), " collectivité(s) sélectionnée(s)")
  }else{
    p(nrow(data), " département(s) sélectionné(s)")
  }
  #ajouter nb BP BA et année selectionné
  
})
output$info_echan_box7_dep <- renderUI({
  
  
  a <- 0
  b <- 0
  c <- 0
  
  if(length(input$cas_spe_deps) == 0){
    p("Dans ",sum(values$nb_reg,a,b,c), " région(s)")
  }else{
    if(("Rhone" %in% input$cas_spe_deps | "METRO LYON" %in% input$cas_spe_deps) & !"Auvergne-Rhône-Alpes" %in% input$reg_echan_deps){
      a <- 1}
    if("Martinique" %in%  c(input$cas_spe_deps)){
      b <- 1
    }
    if("Guyane" %in% input$cas_spe_deps){
      c <- 1
    }
    p("Dans ",sum(values$nb_reg,a,b,c), " région(s)")
  }
  
  
})

output$info_echan_box2_dep <- renderUI({
  #nombre d'habitant
  p(paste(data_infobox_dep()[1,2])," million(s) d'habitants")
})
output$info_echan_box3_dep <- renderUI({
  #%de la pop francaise
  p("Soit ",paste(round(data_infobox_dep()[1,1],1),"% de la population française"))
})
output$info_echan_box4_dep <- renderUI({
  #
  p(paste(round(data_infobox_dep()[1,3],1),"M€ de dépenses d'investissement dont ",round(data_infobox_dep()[1,5],1),"% en budgets annexes"))
})
output$info_echan_box5_dep <- renderUI({
  #
  p("Soit ",paste(round(data_infobox_dep()[1,4],1),"% des invest. départementaux (yc CTU et métropole de Lyon)"))
})

#Premier graph sur la structure des depenses d'invest
data_echan_graph_dequip_dep <- reactive({
  appliInvest::data_DRI(echantillon = data_echan_dep(),entite = data_dep(),gr_ref = data_gr_ref_dep(),bp_ba = input$bp_ba_dep)
})

#donne pour etiquette premier graph
data_etiquette_total_dep <- reactive({
  equitte_total <- data_echan_graph_dequip_dep() %>%
    group_by(nom) %>%
    summarise(montant=sum(montant))
})

# Premier grah
output$graph_dep_invest_dep <- renderPlot({
  graph <-  appliInvest::graph_dep_invest(etiquette = data_etiquette_total_dep(),data = data_echan_graph_dequip_dep(),entite = data_dep(),nom_gfp_com = "Département")
  if(input$bp_ba_dep == 1){
    graph + labs(subtitle = "Budgets principaux")
  }else if(input$bp_ba_dep == 2){
    graph + labs(subtitle = "Budgets annexes")
  }else{
    graph + labs(subtitle = "Budgets principaux et annexes")
  }
})

# graph nuage de point


data_nuage <- reactive({
  if(input$annee_dep == 1){
    appliInvest::data_sub_dep_equip(annee_choisi = 2017,nom_dep_reg = "Département",echantillon = data_echan_dep(),entite = data_dep(),gr_de_ref = data_gr_ref_dep())
  }else if(input$annee_dep == 2){
    appliInvest::data_sub_dep_equip(annee_choisi = "2012-2017",nom_dep_reg = "Département",echantillon = data_echan_dep(),entite = data_dep(),gr_de_ref = data_gr_ref_dep())
  }
})

# output$graph_nuage_dep <- renderPlotly({
#   
#   if(length(unique(data_echan_dep()$nom_dep)) != 1){
#     if(input$annee_dep == 1){
#       data <- data_nuage() %>%
#         filter(nom != "Gr de réf") %>%
#         mutate(dep_equip = round(dep_equip,0),subvention_204 = round(subvention_204,0))
#       data$nom <- fct_relevel(as.factor(data$nom),c("Echantillon","Département"))
#       
#       
#       appliInvest::graph_sub_dep_equip(annee = annee_encours,data_graph = data,entite = data_dep())
#       
#     }else if(input$annee_dep == 2){
#       data <- data_nuage() %>%
#         filter(nom != "Gr de réf") %>%
#         mutate(dep_equip = round(dep_equip,0),subvention_204 = round(subvention_204,0))
#       
#       data$nom <- fct_relevel(as.factor(data$nom),c("Echantillon","Département"))
#       
#       appliInvest::graph_sub_dep_equip(annee = "2012-2017",data_graph = data,entite = data_dep())}
#   }
# })

output$textgraphnuage <- renderUI({
  if(length(unique(data_echan_dep()$nom_dep)) == 1){
    p("L’échantillon ne comportant qu’une collectivité, l’illustration des disparités n’est pas significative.")
  }
})

# financement

data_graph_finance_dep <- reactive({
  appliInvest::data_graph_finan(echantillon = data_echan_dep(),entite = data_dep(), gr_de_ref = data_gr_ref_dep(),nom_gfp_com = "Département",annee_encours = 2017,bp_ba = 1)
})

output$graph_finance_dep <- renderPlot({
  appliInvest::graph_financement(data_graph = data_graph_finance_dep(),entite = data_dep(),nom_gfp_com = "Département",bp_ba =1)
})

output$phrase_dep <- renderUI ({
  montant <- data_graph_finance_dep() %>% select(montant) %>% filter(!is.na(montant))
  indice <- min(montant)
  
  if(indice < 0){
    p("Si le cumul des moyens de financement (y compris fraction négative d'épargne nette) est inférieur à 100%, la différence est financée par ponction sur le fonds de roulement. Dans le cas inverse, il y a augmentation du fonds de roulement.")
  }else{
    p("Si le cumul des moyens de financement est inférieur à 100%, la différence est financée par ponction sur le fonds de roulement. Dans le cas inverse, il y a augmentation du fonds de roulement.")
  }
})

# grph sur evol

observe({
  
  updateSelectInput(session,
                    inputId = "var_select_dep",
                    choices = list(
                      "Dépenses d'investissement hors remb." = "dep_invest",
                      "Dépenses d'équipement" = "dep_equip",
                      "Subventions versées" = "subvention_204",
                      "Recettes d'investissement hors emp." = "rec_invest",
                      "Epargne nette" = "epn",
                      "Emprunts" = "emprunt",
                      "Variation du fonds de roulement" = "var_fon_roul"))
  
})


data_graph_evol_dep <- reactive({
  appliInvest::data_grap_evol(echantillon = data_echan_dep(),entite = data_dep(),gr_de_ref = data_gr_ref_dep(),bp_ba = 1,nom_gfp_com = "Département",ma_var = input$var_select_dep)
})


output$graph_evol_dep <- renderPlot({
  
  graph_evol(data_evol =  data_graph_evol_dep(),annee_dispo = c(2012,2013,2014,2015,2016,2017),bp_ba = 1,var_select = input$var_select_dep,entite = data_dep())
})

data_graph_zoom_dep_equip_dep <- reactive({
  if(input$annee_dep2 == 1){
    appliInvest::data_zoom_dep_equip(echantillon = data_echan_dep(),entite = data_dep(),gr_de_ref = data_gr_ref_dep(),bp_ba = input$bp_ba_dep2,nom_gfp_com = "Département",annee_encours = 2017)
  }else if(input$annee_dep2 == 2){
    appliInvest::data_zoom_dep_equip(echantillon = data_echan_dep(),entite = data_dep(),gr_de_ref = data_gr_ref_dep(),bp_ba = input$bp_ba_dep2,nom_gfp_com = "Département",annee_encours = c(2012,2013,2014,2015,2016,2017))
  }
})

output$graph_zoom_dep_equip_dep <- renderPlot({
  if(input$annee_dep2 == 1){
    appliInvest::graph_zoom_dep_equip(data_graph = data_graph_zoom_dep_equip_dep(),entite = data_dep(),nom_gfp_com = 'Département',bp_ba = input$bp_ba_dep2)
  }else if(input$annee_dep2 == 2){
    graph <-   appliInvest::graph_zoom_dep_equip(data_graph = data_graph_zoom_dep_equip_dep(),entite = data_dep(),nom_gfp_com = 'Département',bp_ba = input$bp_ba_dep2)
    if(input$bp_ba_dep2 == 1){
      graph + labs(subtitle = "Budgets principaux, moyenne annuelle")
    }else if(input$bp_ba_dep2 == 2){
      graph + labs(subtitle = "Budgets annexes, moyenne annuelle")
    }else{
      graph + labs(subtitle = "Budgets principaux et annexes, moyenne annuelle")
    }
    graph <- graph + labs(title ="Structure des dépenses d'équipement brutes de 2012 à 2017, en %")
    print(graph)
  }
})

data_sub_verse_dep <-  reactive({
  if(input$annee_dep3 == 1){
    appliInvest::data_sub_204(echantillon = data_echan_dep(),entite = data_dep(),gr_ref = data_gr_ref_dep(),annee_encours = 2017)
  }else if(input$annee_dep3 == 2){
    appliInvest::data_sub_204(echantillon = data_echan_dep(),entite = data_dep(),gr_ref = data_gr_ref_dep(),annee_encours = c(2012,2013,2014,2015,2016,2017))
  }
})

output$graph_sub_204_dep <- renderPlot({
  if(input$annee_dep3 == 1){
    appliInvest::graph_sub_204(data = data_sub_verse_dep(),entite = data_dep(),nom_gfp_com = "Département",quel_annee = 1 )
  }else if(input$annee_dep3 == 2){
    appliInvest::graph_sub_204(data = data_sub_verse_dep(),entite = data_dep(),nom_gfp_com = "Département",quel_annee = 2 )
  }
  
})

data_telechager_dep <- reactive({
  
  if(nrow(data_dep()) != 0){
    
    data <- rbind(data_dep(),data_echan_dep())
    data <- data %>%
      distinct() %>%
      rename(strate17 = strate16,dep_invest_hr = dep_invest,dep_invest_hr_ba = dep_invest_ba,autre_dep_equip_brut = autres_dep_equip,autre_dep_equip_brut_ba = autres_dep_equip_ba,
             rec_invest_he = rec_invest,rec_invest_he_ba = rec_invest_ba,sub_dot_hfctva = compte13_10,sub_dot_hfctva_ba = compte13_10_ba,rem_dette=remboursement,rem_dette_ba=remboursement_ba,
             stock_dette = dette,stock_dette_ba = dette_ba,flux_croise_i = fc_i,sub_204_autre_public = sub_204_autres,vdfr = var_fon_roul,vdfr_ba = var_fon_roul_ba) %>%
      select(-planFCTVA_d,-planFCTVA_r,-planFCTVA_r_ba,-planFCTVA_d_ba,-fc_f,-autres_rec_invest_hors_planrelance,-autres_rec_invest_hors_planrelance_ba,-taxe_amenag,-taxe_amenag_ba,-amende,-amende_ba,-sub_dot_ssTAAm,-sub_dot_ssTAAm_ba) %>%
      mutate_if(is.numeric,.funs = function(x){round(x,3)})
    
  }else{data <- data_echan_dep() %>%
    rename(strate17 = strate16,dep_invest_hr = dep_invest,dep_invest_hr_ba = dep_invest_ba,autre_dep_equip_brut = autres_dep_equip,autre_dep_equip_brut_ba = autres_dep_equip_ba,
           rec_invest_he = rec_invest,rec_invest_he_ba = rec_invest_ba,sub_dot_hfctva = compte13_10,sub_dot_hfctva_ba = compte13_10_ba,rem_dette=remboursement,rem_dette_ba=remboursement_ba,
           stock_dette = dette,stock_dette_ba = dette_ba,flux_croise_i = fc_i,sub_204_autre_public = sub_204_autres,vdfr = var_fon_roul,vdfr_ba = var_fon_roul_ba) %>%
    select(-planFCTVA_d,-planFCTVA_r,-planFCTVA_r_ba,-planFCTVA_d_ba,-fc_f,-autres_rec_invest_hors_planrelance,-autres_rec_invest_hors_planrelance_ba,-taxe_amenag,-taxe_amenag_ba,-amende,-amende_ba,-sub_dot_ssTAAm,-sub_dot_ssTAAm_ba) %>%
    mutate_if(is.numeric,.funs = function(x){round(x,3)})
  
  }
})


output$export_data_dep <- downloadHandler(
  
  filename = function(){
    paste0(length(unique(data_echan_dep()$nom_dep)),"col-ofgl-keuros",Sys.Date(),".csv")},
  content = function(file) {
    write.csv2(data_telechager_dep(), file, row.names = FALSE)
  })


# output$table_dep <- renderDataTable({
#   data <- data_telechager_dep() %>%
#     dplyr::arrange(nom_dep)
#   
#   datatable(data,options = list(
#     'select' = FALSE,
#     'scrollX' = TRUE,
#     'lengthChange' =FALSE,
#     'info' = FALSE,
#     'serverSide' = TRUE,
#     dom="ftp",
#     pageLength = 10,
#     language = list(
#       'search'="Filtrer :",
#       'paginate' = list(
#         'previous' = "Précédent",
#         'next' = "Suivant"
#       ))
#   ))
#   
# })

})