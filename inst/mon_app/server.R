library(shiny)
source('global.R')

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {


  # les values pour les observes et les étapes data
   values <- reactiveValues()#reg = NULL,dep= NULL,coms = NULL,bp_ba2 = NULL,var_select = NULL,
  #                          reg_gfp = NULL,dep_gfp= NULL,coms_gfp = NULL,bp_ba2_gfp = NULL,var_select_gfp = NULL,
  #                          reg_deps = NULL,strate_dep = NULL,dep_deps= NULL,coms_deps = NULL,bp_ba_dep_2 = NULL,dfm = NULL,dpu = NULL)













  ####PArtie Commune ====

  observe({
    values$reg <- input$reg_echan_com
    
    values$dep <- input$mes_dep_selec_com 

    values$coms <- input$ma_com
    values$bp_ba2 <-input$bp_ba2
    

    })

  # nb_reg <- eventReactive(input$go,{
  # 
  #    list_reg <- list_dep %>% 
  #      select(nom_reg,nom_reg_accent) %>%
  #      distinct() %>%
  #      filter( nom_reg_accent %in% c(input$reg_echan_com))
  #   
  #   pop_reg %>%
  #     filter(nom_reg %in% list_reg$nom_reg) %>%
  #     summarise(pop = sum(population))
  # })

   # selection des departements
   observe(priority = 5,{
     mes_dep <- list_dep_com %>%
       filter(nom_reg_accent %in% c(values$reg))
     
     updatePickerInput(session,
                       inputId = "mes_dep_selec_com",
                       choices = sort(mes_dep$nom_dep_accent),
                       selected = sort(mes_dep$nom_dep_accent))
     
   })
 

data_gr_ref_com_2 <- eventReactive(input$go,{
  
  if(input$geo_gr_ref == 3 ){ 
    data <- list_dep
  } else if(input$geo_gr_ref == 2){
    data <- list_dep %>%
      filter(metropole == 2)
  }else if(input$geo_gr_ref == 1){
    data <-list_dep %>% filter(metropolehidf == 0)}
  
  base_invest %>%
    filter(nom_dep %in% data$nom_dep & indice_pop %in% c(input$pop_gr_ref) & montagne %in% c(input$montagne_gr_ref) & touristique %in% c(input$touristique_gr_ref))
  
})
 

 output$phrase_gr_ref <- renderUI({
   phrase <- "Votre groupe de référence :"

   if(input$geo_gr_ref == 1){
     geo <- "France métropolitaine hors IDF"
   }else if(input$geo_gr_ref == 2){
     geo <- "France métropolitaine"
     }else(
       geo <- "France entière")
   phrase <- geo
   if(length(input$pop_gr_ref) != 2){
    if(input$pop_gr_ref == 1){pop <- "Moins de 10 000 hab."
   }else if(input$pop_gr_ref == 0){
     pop <- "Plus de 10 000 hab."}
     phrase <- c(phrase,pop) }
   if(length(input$montagne_gr_ref) != 2){
    if(input$montagne_gr_ref == 1){mont <- "Montagne"
   }else if(input$montagne_gr_ref == 0){
     mont <- "Non montagne"}
     phrase <- c(phrase,mont)}
   if(length(input$touristique_gr_ref) != 2){
   if(input$touristique_gr_ref == 1){tour <- "Touristiques"
   }else if(input$touristique_gr_ref == 0){
     tour <- "Non touristiques"}
     phrase <- c(phrase,tour)}
  # if(length(input$montagne_gr_ref) != 2){
  #   p(phrase,br(),geo,br(),br(),mont)
  # } else if(length(input$touristique_gr_ref) != 2){
  #   p(phrase,br(),geo,br(),br(),tour)
  # }else if(length(input$pop_gr_ref) != 2){
  #   p(phrase,br(),geo,br(),pop)}
   p("Les communes de :",br(),paste0(phrase,collapse = ", "))

    })

 onclick("Clique_gr_ref", 
         toggle(id = "choix_gr_ref", anim = TRUE))

   #selection de l'entite
   # observeEvent(input$isocom,{
   #   mes_com <- base_invest %>%
   #     filter(nom_dep_accent %in% c(values$dep))
   #   
   # if(length(input$reg_echan_com) != 0){
   #  enable("ma_com")}else { alert("Votre échantillon est vide")}
   observe({
     df <- data.frame(nom_com=c("- Aucune"),stringsAsFactors = FALSE)
     df2 <- base_invest %>% select(nom_com) %>% 
        rbind(df) 

     
      updateSelectizeInput(session,
                            inputId = "ma_com",
                            server = TRUE,
                            choices = sort(df2$nom_com),
                            options = list(
                              maxOptions = 100,
                               label = sort(df2$nom_com),
                              render = I(
                               '{
                                option: function(item, escape) {
                              return "<div><strong>" + escape(item.value) + "</strong></div>"
                               }
    }')
                              ),
                        selected = NULL
                             ) # me permet de rendre la selection vide..
   # 
   #  
   #   })
})

  data_echan_com <- eventReactive(input$go,{

    list_dep_com <- list_dep_com %>% filter(nom_dep_accent %in% input$mes_dep_selec_com)
      appliInvest::base_invest_com %>%
         filter(nom_dep %in% c(list_dep_com$nom_dep) & strate16 %in% c(input$pop) & montagne %in% c(input$montagne) & touristique %in% c(input$touristique))
       }
  )
  


  ##Etape data pour la selection de la commune
  data_com <- eventReactive(input$go,{
    data <- NULL
     data <- appliInvest::base_invest_com %>%
      filter(nom_com %in% values$coms)
  })

  #Etape data pour creer le groupe de reference
  data_gr_ref_com <- eventReactive(input$go,{

    if(input$geo_gr_ref == 3 ){ 
      data <- NULL
      data <- base_invest_com_gr_ref
      } else if(input$geo_gr_ref == 2){
        data <- NULL
        data <- base_invest_com_gr_ref %>%
          filter(metropole == 2)
      }else if(input$geo_gr_ref == 1){
        data <- NULL
        data <- base_invest_com_gr_ref %>% filter(metropolehidf == 0)}
  
    data %>%
      filter(indice_pop %in% c(input$pop_gr_ref) & montagne %in% c(input$montagne_gr_ref) & touristique %in% c(input$touristique_gr_ref))

  })
  

  
  observe({
    
    nb_ligne <- base_invest %>%
      filter(nom_dep %in% c(values$dep) & strate16 %in% c(input$pop) & montagne %in% c(input$montagne) & touristique %in% c(input$touristique))
    nb_ligne <- nrow(nb_ligne)
    nb_ligne_com <- base_invest %>%
      filter(nom_com %in% c(values$coms))
    nb_ligne_com_echan <- nrow(data_echan_com()[data_echan_com()$annee == 2017,]) + nrow(nb_ligne_com)
    
    if(input$geo_gr_ref == 3 ){
      nb_ligne_gr_ref <- base_invest %>%
        filter(indice_pop %in% c(input$pop_gr_ref) & montagne %in% c(input$montagne_gr_ref) & touristique %in% c(input$touristique_gr_ref))
    } else if(input$geo_gr_ref == 2){
      dep_gr_ref <- list_dep %>%
        filter(metropole == 2)
      
      nb_ligne_gr_ref <- base_invest %>%
        filter(nom_dep %in% c(dep_gr_ref$nom_dep) & indice_pop %in% c(input$pop_gr_ref) & montagne %in% c(input$montagne_gr_ref) & touristique %in% c(input$touristique_gr_ref))
    }else if(input$geo_gr_ref == 1){
      dep_gr_ref <- list_dep %>% filter(metropolehidf == 0)
      
      nb_ligne_gr_ref <- base_invest %>%
        filter(nom_dep %in% c(dep_gr_ref$nom_dep) & indice_pop %in% c(input$pop_gr_ref) & montagne %in% c(input$montagne_gr_ref) & touristique %in% c(input$touristique_gr_ref))
    }
    nb_ligne_gr_ref <- nrow(nb_ligne_gr_ref)
    
    
    if(nrow(data_com()) == 0 ){
      if(nb_ligne != nb_ligne_com_echan | nb_ligne_gr_ref != nrow(data_gr_ref_com_2())){
        showElement(id="go")
      }}
    
    if(nrow(data_com()) != 0){
      if(nrow(nb_ligne_com) == 0){
        
        showElement(id="go")}
      else{
        if(nb_ligne != nrow(data_echan_com())){
          showElement(id="go")
        }else if(nb_ligne_com$nom_com != data_com()$nom_com){
          showElement(id="go")
        }else if(nb_ligne_gr_ref != nrow(data_gr_ref_com_2())){
          showElement(id="go")
        }
      }
    }
    
    
  })
  
  observeEvent(input$go,{
    hide("go",time = 0.1)
    # disable("ma_com")
    # addClass("go",class = "bttn-primary")
  })
  
  # Affiche des infos sur le groupe


  #fin de la ligne sur la sélection des entités


  ## pour la ligne sur les info sur léchantillon
  data_infobox <- reactive({
    echan <- data_echan_com() %>%
      filter(annee == annee_encours) %>%
      summarise(pop_pour = sum(population)/67188616*100, pop_nb = round(sum(population)/1000000,2), dep_equip_t = sum(dep_equip + dep_equip_ba)/1000, dep_equip_pour = (sum(dep_equip + dep_equip_ba))/20281353*100,dep_equip_ba_bp = (sum(dep_equip_ba)/(sum(dep_equip + dep_equip_ba)))*100)
  })
  # pour les info box sur l'échantillon

  output$info_echan_box1 <- renderUI({

    data <- data_echan_com() %>% filter(annee == 2017) %>% summarise(n=n())
    p(data$n, " communes")

      #ajouter nb BP BA et année selectionné

  })
  output$info_echan_box7 <- renderUI({
      p("Dans ",length(unique(data_echan_com()$nom_dep)), " département(s)")
      #ajouter nb BP BA et année selectionné
  })
  output$info_echan_box2 <- renderUI({
    #nombre d'habitant
    p(paste(data_infobox()[1,2])," million(s) d'habitants")
  })
  output$info_echan_box3 <- renderUI({
    #%de la pop francaise
    p("Soit ",paste(round(data_infobox()[1,1],1),"% de la population française"))
  })
  output$info_echan_box4 <- renderUI({
    #
    p(paste(round(data_infobox()[1,3],1),"M€ de dépenses d'équipement dont ",round(data_infobox()[1,5],1),"% en budgets annexes"))
  })
  output$info_echan_box5 <- renderUI({
    #
    p("Soit ",paste(round(data_infobox()[1,4],1),"% des dépenses d'équipement des communes"))
  })
  # output$info_echan_box6 <- renderText({
  #   #
  #   paste(round(data_infobox()[1,5],0),"% des BA sur le total \n des dépenses d'équipement BP + BA")
  #
  # })
  # fin de la ligne pour les infos sur échantillon


  # permet de créer la base du premier graphique

  data_echan_graph_dequip_com <- reactive({
    appliInvest::data_graph_equip(echantillon = data_echan_com(),entite = data_com(),gr_ref = data_gr_ref_com(),gfp_ou_com = 1)
  })

  #donne pour etiquette premier graph
  data_etiquette_total_com <- reactive({
    data_echan_graph_dequip_com() %>%
      group_by(nom) %>%
      summarise(montant=sum(montant))
  })

  # Premier grah
  output$grap_dep_equip <- renderPlot({
  appliInvest::graph_dep_equip(etiquette = data_etiquette_total_com(),data = data_echan_graph_dequip_com(),entite = data_com(),nom_gfp_com = "Commune",gfp_ou_com = 1,gfp_ou_bc = 3)
  })

  # Tableau d'information sur l'échantillon

  #data pour l'info
  data_stat_echan_com <- eventReactive(input$go,{
    appliInvest::data_stat_echan(data_echan_com(),gfp_ou_com = 1)
})

  output$stat_echan_moy <- renderTable(digits = 0,{
    
  if(nrow(data_com()) == 0 & (nrow(data_echan_com())/3) >= 30){  
      data_stat_echan_com() %>% 
      rename("€/hab." = Annee,"  " = variable) %>% 
      as_tibble()

    }else if(nrow(data_com()) != 0 & (nrow(data_echan_com())/3) >= 30){
    data_stat_echan_com() %>% 
      rename("€/hab." = Annee,"  " = variable) %>% 
      as_tibble()
      }else{NULL}
    })
  
  
output$stat_echan_moy_com_sup_30 <- renderUI({
    if(nrow(data_com()) == 0 & (nrow(data_echan_com())/3) < 30){
      p("Echantillon trop petit pour présenter des informations statistiques.")
    }else if(nrow(data_echan_com())/3 >= 30){
      p("1er quartile : 25% des communes de l'échantillon ont une valeur inférieure.",br(),
            "3e quartile : 25% des communes de l'échantillon ont une valeur supérieure.",br(),
            "Médiane : la moitié des communes a une valeur inférieure.",br(),
            "Traitement OFGL - Source : balances comptables DGFIP")}
  })


  # fin de la partie sur les depenses d'équipement
  # grahpique sur le financement et les evol

  # permet de créer la base du graphique sur le financement

   data_graph_finance_com <- reactive({
     data_graph_finan(echantillon = data_echan_com(),entite = data_com(),gr_de_ref = data_gr_ref_com(),nom_gfp_com = "Commune",annee_encours = 2017,bp_ba = input$bp_ba)
   })

  # graph Financement
  output$graph_finance_com <- renderPlot({
  
    graph_financement(data_graph = data_graph_finance_com(), entite = data_com(),nom_gfp_com = "Commune",bp_ba = input$bp_ba)
  })

  # phrase sur le financement si epn < 0

  output$phrase <- renderUI ({
    montant <- data_graph_finance_com() %>% select(montant) %>% filter(!is.na(montant))
    indice <- min(montant)

    if(indice < 0){
     p("Si le cumul des moyens de financement (y compris fraction négative d'épargne nette) est inférieur à 100%, la différence est financée par ponction sur le fonds de roulement. Dans le cas inverse, il y a augmentation du fonds de roulement.")
    }else{
      p("Si le cumul des moyens de financement est inférieur à 100%, la différence est financée par ponction sur le fonds de roulement. Dans le cas inverse, il y a augmentation du fonds de roulement.")
    }
  })

  # Troisieme graph avec les evolutions

  # une étape obverse pour le obtenir le bon nom de la variable
  observe(priority = -3,{
    values$bp_ba2 <- input$bp_ba2
  
    if(values$bp_ba2 %in% c(1,3)){
      updateSelectInput(session,
                        inputId = "var_select",
                        choices = list( 
                          "Dépenses d'investissement hors remb." = "dep_invest",
                          "Dépenses d'équipement" = "dep_equip",
                          "Recettes d'investissement hors emp." = "rec_invest",
                          "Epargne nette" = "epn",
                          "Emprunts" = "emprunt",
                          "Variation du fonds de roulement" = "var_fon_roul"))
    }
    else{
      updateSelectInput(session,
                        inputId = "var_select",
                        choices = list( 
                          "Dépenses d'investissement hors remb." = "dep_invest_ba",
                          "Dépenses d'équipement" = "dep_equip_ba",
                          "Recettes d'investissement hors emp." = "rec_invest_ba",
                          "Epargne nette" = "epn_ba",
                          "Emprunts" = "emprunt_ba",
                          "Variation du fonds de roulement" = "var_fon_roul_ba"))

    }

  })

  # Préparation de la base pour le graph
# 
#   data_graph_evol <- reactive({
#     values$bp_ba2 <- input$bp_ba2
# 
# 
#     data_grap_evol(echantillon = data_echan_com(),entite=data_com(),gr_de_ref=data_gr_ref_com(),bp_ba=values$bp_ba2,nom_gfp_com = "Commune",ma_var = input$var_select)
# 
#   })

  output$graph_evol <- renderPlot({
    values$bp_ba2 <- input$bp_ba2
    
    data <-  appliInvest::data_grap_evol(echantillon = data_echan_com(),entite=data_com(),gr_de_ref=data_gr_ref_com(),bp_ba=values$bp_ba2,nom_gfp_com = "Commune",ma_var = input$var_select)
    

    graph_evol(data_evol =  data,annee_dispo = c(2014,2015,2016,2017),bp_ba = values$bp_ba2,var_select = input$var_select,entite = data_com())
  })

  # les détails des recettes et dépenses

  # data_graph_zoom_rec_invest <- reactive({
  #   data_zoom_rec_invest(echantillon = data_echan_com(),entite = data_com(),gr_de_ref=data_gr_ref_com(),bp_ba = input$bp_ba3,nom_gfp_com="Commune")
  # })

  output$graph_zoom_rec_invest_com <- renderPlot({
    data <- data_zoom_rec_invest(echantillon = data_echan_com(),entite = data_com(),gr_de_ref=data_gr_ref_com(),bp_ba = input$bp_ba3,nom_gfp_com="Commune")

    graph_zoom_rec_invest(data,entite = data_com(),nom_gfp_com = "Commune",bp_ba = input$bp_ba3)
   })


  # data_graph_zoom_dep_equip <- reactive({
  #    data_zoom_dep_equip(echantillon = data_echan_com(),entite = data_com(),gr_de_ref=data_gr_ref_com(),bp_ba = input$bp_ba4,nom_gfp_com="Commune",annee_encours = annee_encours)
  #  })

    output$graph_zoom_sur_dep_com <- renderPlot({
      data <- data_zoom_dep_equip(echantillon = data_echan_com(),entite = data_com(),gr_de_ref=data_gr_ref_com(),bp_ba = input$bp_ba4,nom_gfp_com="Commune",annee_encours = annee_encours)

   graph_zoom_dep_equip(data,entite = data_com(),nom_gfp_com = "Commune",bp_ba = input$bp_ba4)
    })



  #derniere partie sur la base à télécharger

    #  data_telechager <- reactive({
    #    
    # if(nrow(data_com()) != 0){
    #   data <- rbind(data_com(),data_echan_com())
    #    data %>%
    #     distinct() %>%
    #     rename(nom_gfp_2016 = nom_complet,dep_invest_hr = dep_invest,dep_invest_hr_ba = dep_invest_ba,autre_dep_equip_brut = autres_dep_equip,autre_dep_equip_brut_ba = autres_dep_equip_ba,
    #            rec_invest_he = rec_invest,rec_invest_he_ba = rec_invest_ba,sub_dot_hfctva = compte13_10,sub_dot_hfctva_ba = compte13_10_ba,rem_dette=remboursement,rem_dette_ba=remboursement_ba,
    #            stock_dette = dette,stock_dette_ba = dette_ba,vdfr = var_fon_roul,vdfr_ba = var_fon_roul_ba,flux_croise_i = fc_i) %>%
    #     mutate(nom_gfp_2016 = as.character(nom_gfp_2016),autre_dep_invest=dep_invest_hr - dep_equip - subvention_204,autre_dep_invest=dep_invest_hr_ba - dep_equip_ba - subvention_204_ba) %>%
    #     select(-planFCTVA_r,-planFCTVA_d,-planFCTVA_r_ba,-planFCTVA_d_ba,-fc_f,-autres_rec_invest_hors_planrelance,-autres_rec_invest_hors_planrelance_ba) %>%
    #     mutate_if(is.numeric,.funs = function(x){round(x,3)}) %>%
    #      dplyr::arrange(code_insee16)
    # }else{
    #   data <- data_echan_com() 
    #   
    #   data %>%
    #     rename(nom_gfp_2016 = nom_complet,dep_invest_hr = dep_invest,dep_invest_hr_ba = dep_invest_ba,autre_dep_equip_brut = autres_dep_equip,autre_dep_equip_brut_ba = autres_dep_equip_ba,
    #            rec_invest_he = rec_invest,rec_invest_he_ba = rec_invest_ba,sub_dot_hfctva = compte13_10,sub_dot_hfctva_ba = compte13_10_ba,rem_dette=remboursement,rem_dette_ba=remboursement_ba,
    #            stock_dette = dette,stock_dette_ba = dette_ba,vdfr = var_fon_roul,vdfr_ba = var_fon_roul_ba,flux_croise_i = fc_i) %>%
    #     mutate(nom_gfp_2016 = as.character(nom_gfp_2016),autre_dep_invest=dep_invest_hr - dep_equip - subvention_204,autre_dep_invest=dep_invest_hr_ba - dep_equip_ba - subvention_204_ba) %>%
    #     select(-planFCTVA_r,-planFCTVA_d,-planFCTVA_r_ba,-planFCTVA_d_ba,-fc_f,-autres_rec_invest_hors_planrelance,-autres_rec_invest_hors_planrelance_ba) %>%
    #     mutate_if(is.numeric,.funs = function(x){round(x,3)}) %>%
    #     dplyr::arrange(code_insee16)
    #   }
    #  })


  output$export_data <- downloadHandler(

    filename = function(){
      data <- data_echan_com() %>%
        filter(annee == 2017) %>% summarise(n=n())
    paste0(data$n,"col-ofgl-keuros",Sys.Date(),".csv")},
    content = function(file){
      if(nrow(data_com()) != 0){
        data <- rbind(data_com(),data_echan_com())
        data <- data %>%
          distinct() %>%
          rename(nom_gfp_17 = nom_complet,siren_gfp_17 = siren_epci,code_insee17=code_insee16,strate17 = strate16,dep_invest_hr = dep_invest,dep_invest_hr_ba = dep_invest_ba,autre_dep_equip_brut = autres_dep_equip,autre_dep_equip_brut_ba = autres_dep_equip_ba,
                 rec_invest_he = rec_invest,rec_invest_he_ba = rec_invest_ba,sub_dot_hfctva = compte13_10,sub_dot_hfctva_ba = compte13_10_ba,rem_dette=remboursement,rem_dette_ba=remboursement_ba,
                 stock_dette = dette,stock_dette_ba = dette_ba,vdfr = var_fon_roul,vdfr_ba = var_fon_roul_ba,flux_croise_i = fc_i) %>%
          mutate(nom_gfp_17 = str_to_upper(nom_gfp_17),autre_dep_invest=dep_invest_hr - dep_equip - subvention_204,autre_dep_invest=dep_invest_hr_ba - dep_equip_ba - subvention_204_ba) %>%
          select(-planFCTVA_r,-planFCTVA_d,-planFCTVA_r_ba,-planFCTVA_d_ba,-fc_f,-autres_rec_invest_hors_planrelance,-autres_rec_invest_hors_planrelance_ba) %>%
          mutate_if(is.numeric,.funs = function(x){round(x,3)}) %>%
          dplyr::arrange(code_insee17)
      }else{
        data <- data_echan_com() 
        
        data <- data %>%
          rename(nom_gfp_17 = nom_complet,siren_gfp_17 = siren_epci,code_insee17=code_insee16,strate17 = strate16,dep_invest_hr = dep_invest,dep_invest_hr_ba = dep_invest_ba,autre_dep_equip_brut = autres_dep_equip,autre_dep_equip_brut_ba = autres_dep_equip_ba,
                 rec_invest_he = rec_invest,rec_invest_he_ba = rec_invest_ba,sub_dot_hfctva = compte13_10,sub_dot_hfctva_ba = compte13_10_ba,rem_dette=remboursement,rem_dette_ba=remboursement_ba,
                 stock_dette = dette,stock_dette_ba = dette_ba,vdfr = var_fon_roul,vdfr_ba = var_fon_roul_ba,flux_croise_i = fc_i) %>%
          mutate(nom_gfp_17 = str_to_upper(nom_gfp_17),autre_dep_invest=dep_invest_hr - dep_equip - subvention_204,autre_dep_invest=dep_invest_hr_ba - dep_equip_ba - subvention_204_ba) %>%
          select(-planFCTVA_r,-planFCTVA_d,-planFCTVA_r_ba,-planFCTVA_d_ba,-fc_f,-autres_rec_invest_hors_planrelance,-autres_rec_invest_hors_planrelance_ba) %>%
          mutate_if(is.numeric,.funs = function(x){round(x,3)}) %>%
          dplyr::arrange(code_insee17)
      }
      write_delim(data, file, delim = ";")
    })


   output$table <- renderDataTable({
     
     if(nrow(data_com()) != 0){
       data <- rbind(data_com(),data_echan_com())
       data <- data %>%
         distinct() %>%
         rename(nom_gfp_17 = nom_complet,siren_gfp_17 = siren_epci,code_insee17=code_insee16,strate17 = strate16,dep_invest_hr = dep_invest,dep_invest_hr_ba = dep_invest_ba,autre_dep_equip_brut = autres_dep_equip,autre_dep_equip_brut_ba = autres_dep_equip_ba,
                rec_invest_he = rec_invest,rec_invest_he_ba = rec_invest_ba,sub_dot_hfctva = compte13_10,sub_dot_hfctva_ba = compte13_10_ba,rem_dette=remboursement,rem_dette_ba=remboursement_ba,
                stock_dette = dette,stock_dette_ba = dette_ba,vdfr = var_fon_roul,vdfr_ba = var_fon_roul_ba,flux_croise_i = fc_i) %>%
         mutate(nom_gfp_17 = str_to_upper(nom_gfp_17),autre_dep_invest=dep_invest_hr - dep_equip - subvention_204,autre_dep_invest=dep_invest_hr_ba - dep_equip_ba - subvention_204_ba) %>%
         select(-planFCTVA_r,-planFCTVA_d,-planFCTVA_r_ba,-planFCTVA_d_ba,-fc_f,-autres_rec_invest_hors_planrelance,-autres_rec_invest_hors_planrelance_ba) %>%
         mutate_if(is.numeric,.funs = function(x){round(x,3)}) %>%
         dplyr::arrange(code_insee17)
     }else{
       data <- data_echan_com() 
       
       data <- data %>%
         rename(nom_gfp_17 = nom_complet,siren_gfp_17 = siren_epci,code_insee17=code_insee16,strate17 = strate16,dep_invest_hr = dep_invest,dep_invest_hr_ba = dep_invest_ba,autre_dep_equip_brut = autres_dep_equip,autre_dep_equip_brut_ba = autres_dep_equip_ba,
                rec_invest_he = rec_invest,rec_invest_he_ba = rec_invest_ba,sub_dot_hfctva = compte13_10,sub_dot_hfctva_ba = compte13_10_ba,rem_dette=remboursement,rem_dette_ba=remboursement_ba,
                stock_dette = dette,stock_dette_ba = dette_ba,vdfr = var_fon_roul,vdfr_ba = var_fon_roul_ba,flux_croise_i = fc_i) %>%
         mutate(nom_gfp_17 = str_to_upper(nom_gfp_17),autre_dep_invest=dep_invest_hr - dep_equip - subvention_204,autre_dep_invest=dep_invest_hr_ba - dep_equip_ba - subvention_204_ba) %>%
         select(-planFCTVA_r,-planFCTVA_d,-planFCTVA_r_ba,-planFCTVA_d_ba,-fc_f,-autres_rec_invest_hors_planrelance,-autres_rec_invest_hors_planrelance_ba) %>%
         mutate_if(is.numeric,.funs = function(x){round(x,3)}) %>%
         dplyr::arrange(code_insee17)
     }
   
     
     datatable(data,options = list(
       'select' = FALSE,
       'scrollX' = TRUE,
       'lengthChange' =FALSE,
       'info' = FALSE,
       'serverSide' = TRUE,
       dom="ftp",
       pageLength = 10,
       language = list(
         'search'="Filtrer :",
         'paginate' = list(
           'previous' = "Précédent",
           'next' = "Suivant"
         ))
     ))
   })
























   #### PARTIE GFP ====


   observe({
     values$reg_gfp <- input$reg_echan_gfp
     values$dep_gfp <- input$mes_dep_selec_gfp
     values$coms_gfp <- input$mon_gfp
   })
  observe({
    mes_dep <- list_dep %>% filter(nom_reg_accent %in% c(values$reg_gfp))

    updatePickerInput(session,
                      inputId = "mes_dep_selec_gfp",
                      choices = sort(mes_dep$nom_dep_accent),
                      selected = sort(mes_dep$nom_dep_accent))
  })
  
  nom_dep_gfp <- reactive({
    
    list_dep_gfp <- list_dep %>%
      filter(nom_dep_accent %in% c(values$dep_gfp)) %>%
      select(nom_dep)
    
    list_dep_gfp
  })

  observe({

    nb_ligne <- base_invest_gfp %>%
      filter(nom_dep %in% c(nom_dep_gfp()$nom_dep) & strate16 %in% c(input$pop_gfp) & nj_epci2016 %in% c(input$nj_epci))
    nb_ligne <- nrow(nb_ligne)
    nb_ligne_gfp <- base_invest_gfp %>%
      filter(LBUDG %in% c(values$coms_gfp))
    nb_ligne_gfp_echan <- nrow(data_echan_gfp()) + nrow(nb_ligne_gfp)

    if(input$geo_gr_ref_gfp == 3 ){
      nb_ligne_gr_ref <-   base_invest_gfp %>%
        filter(nj_epci2016 %in% c(input$nj_gr_ref_gfp))
    } else if(input$geo_gr_ref_gfp == 2){
      dep_gr_ref_gfp <- list_dep %>%
        filter(metropole == 2)

      nb_ligne_gr_ref <- base_invest_gfp %>%
        filter(nom_dep %in% c(dep_gr_ref_gfp$nom_dep) & nj_epci2016 %in% c(input$nj_gr_ref_gfp))

    }else if (input$geo_gr_ref_gfp == 1){
      dep_gr_ref_gfp <- list_dep %>% filter(metropolehidf == 0)

      nb_ligne_gr_ref <-  base_invest_gfp %>%
        filter(nom_dep %in% c(dep_gr_ref_gfp$nom_dep) & nj_epci2016 %in% c(input$nj_gr_ref_gfp))
    }
    nb_ligne_gr_ref <- nrow(nb_ligne_gr_ref)

    if(nrow(data_gfp()) == 0){
      if(nb_ligne != nb_ligne_gfp_echan | nb_ligne_gr_ref != nrow(data_gr_ref_gfp())){
        showElement(id = "go_gfp",time = 0.1)
      }else{
        hide(id = "go_gfp",time= 0.1)}
        # addClass(id = "go_gfp", class = "bttn-simple bttn-md bttn-primary")}
    }else if(nrow(data_gfp()) != 0){
      if(nrow(nb_ligne_gfp) == 0){
        showElement(id = "go_gfp",time = 0.1)
        }else{
        if(nb_ligne != nrow(data_echan_gfp()) | nb_ligne_gfp$LBUDG != data_gfp()$LBUDG | nb_ligne_gr_ref != nrow(data_gr_ref_gfp())){
          showElement(id = "go_gfp",time = 0.1)
        }else{
          hide(id = "go_gfp",time= 0.1)}}
    }
  })

  # selection de l'entite
  observe({
    mon_gfp <- base_invest_gfp %>%
      filter(nom_dep %in% c(nom_dep_gfp()$nom_dep) & strate16 %in% c(input$pop_gfp) & nj_epci2016 %in% c(input$nj_epci)) %>% select(LBUDG) %>% distinct()

    updateSelectizeInput(session,
                         inputId = "mon_gfp",
                         server = TRUE,
                         choices = mon_gfp$LBUDG,
                         options = list(
                           maxOptions = 1000,
                           label = mon_gfp$LBUDG,
                           render = I(
                             '{
                             option: function(item, escape) {
                             return "<div><strong>" + escape(item.value) + "</strong></div>"
                             }
  }')),
                         selected = "vide"
    ) # me permet de rendre la selection vide..
  })
  onclick("Clique_gr_ref_gfp", toggle(id = "choix_gr_ref_gfp", anim = TRUE))

  data_echan_gfp <- eventReactive(input$go_gfp,{

    base_invest_gfp %>%
      filter(nom_dep %in% c(nom_dep_gfp()$nom_dep) & strate16 %in% c(input$pop_gfp) & nj_epci2016 %in% c(input$nj_epci))
  })


  ##Etape data pour la selection de la commune
  data_gfp <- eventReactive(input$go_gfp,{

    data_echan_gfp() %>%
      filter(LBUDG %in% c(values$coms_gfp))

  })

  #Etape data pour creer le groupe de reference
  data_gr_ref_gfp <- eventReactive(input$go_gfp,{
    if(input$geo_gr_ref_gfp == 3 ){
      base_invest_gfp %>%
        filter(nj_epci2016 %in% c(input$nj_gr_ref_gfp))
    } else if(input$geo_gr_ref_gfp == 2){
      dep_gr_ref_gfp <- list_dep %>%
        filter(metropole == 2)

      base_invest_gfp %>%
        filter(nom_dep %in% c(dep_gr_ref_gfp$nom_dep) & nj_epci2016 %in% c(input$nj_gr_ref_gfp))

       }else if (input$geo_gr_ref_gfp == 1){
      dep_gr_ref_gfp <- list_dep %>% filter(metropolehidf == 0)

      base_invest_gfp %>%
        filter(nom_dep %in% c(dep_gr_ref_gfp$nom_dep) & nj_epci2016 %in% c(input$nj_gr_ref_gfp))
      }
})




  # Affiche des infos sur le groupe
  output$phrase_gr_ref_gfp <- renderUI({
    phrase <- "Les groupements de :"

    if(input$geo_gr_ref_gfp == 1){
      geo <- "France métropolitaine hors IDF"
    }else if(input$geo_gr_ref_gfp == 2){
      geo <- "France métropolitaine"
    }else(
      geo <- "France entière")


    jean <- paste(input$nj_gr_ref_gfp,collapse = ", ")

    p(phrase,br(),geo,br(),jean)
    })
  #fin de la ligne sur la sélection des entités


  ## pour la ligne sur les info sur léchantillon
  data_infobox_gfp <- reactive({
    echan <- data_echan_gfp() %>%
      filter(annee == annee_encours) %>%
      summarise(pop_pour = sum(population)/63646872*100, pop_nb = round(sum(population)/1000000,2), dep_equip_t = sum(dep_equip + dep_equip_ba)/1000, dep_equip_pour = (sum(dep_equip + dep_equip_ba))/9588661*100,dep_equip_ba_bp = (sum(dep_equip_ba)/(sum(dep_equip + dep_equip_ba)))*100)
  })
  # pour les info box sur l'échantillon

  output$info_echan_box1_gfp <- renderUI({

    p(nrow(data_echan_gfp()), " groupements sélectionnés")

    #ajouter nb BP BA et année selectionné

  })
  output$info_echan_box7_gfp <- renderUI({
    p("Dans ",length(unique(data_echan_gfp()$nom_dep)), " département(s)")
    #ajouter nb BP BA et année selectionné
  })
  output$info_echan_box2_gfp <- renderUI({
    #nombre d'habitant
    p(paste(data_infobox_gfp()[1,2])," million(s) d'habitants")
  })
  output$info_echan_box3_gfp <- renderUI({
    #%de la pop francaise
    p("Soit ",paste(round(data_infobox_gfp()[1,1],1),"% de la population française"))
  })
  output$info_echan_box4_gfp <- renderUI({
    #
    p(paste(round(data_infobox_gfp()[1,3],1),"M€ de dépenses d'équipement dont ",round(data_infobox_gfp()[1,5],1),"% en budgets annexes"))
  })
  output$info_echan_box5_gfp <- renderUI({
    #
    p("Soit ",paste(round(data_infobox_gfp()[1,4],1),"% des dépenses d'équipement des groupements"))
  })
  # output$info_echan_box6_gfp <- renderText({
  #   #
  #   paste(round(data_infobox_gfp()[1,5],0),"% des BA sur le total \n des dépenses d'équipement BP + BA")
  #
  # })

  data_echan_graph_dequip_gfp <- reactive({
    if(input$bc_ou_gfp == 1){
      data_echan_gfp <- data_echan_gfp() %>%
        filter(!is.na(dep_equip_com) | !is.na(dep_equip_com_ba)) %>% 
        mutate(dep_equip = dep_equip + dep_equip_com,dep_equip_ba = dep_equip_ba + dep_equip_com_ba)
      data_gfp <- data_gfp() %>%
        filter(!is.na(dep_equip_com) | !is.na(dep_equip_com_ba)) %>% 
        mutate(dep_equip = dep_equip + dep_equip_com,dep_equip_ba = dep_equip_ba + dep_equip_com_ba)
      data_gr_ref_gfp <- data_gr_ref_gfp() %>%
        filter(!is.na(dep_equip_com) | !is.na(dep_equip_com_ba)) %>% 
        mutate(dep_equip = dep_equip + dep_equip_com,dep_equip_ba = dep_equip_ba + dep_equip_com_ba)

    appliInvest::data_graph_equip(echantillon = data_echan_gfp,entite = data_gfp,gr_ref = data_gr_ref_gfp,gfp_ou_com = 2)
    }else{
      appliInvest::data_graph_equip(echantillon = data_echan_gfp(),entite = data_gfp(),gr_ref = data_gr_ref_gfp(),gfp_ou_com = 2)
    }
  })

  #donne pour etiquette premier graph
  data_etiquette_total_gfp <- reactive({
    equitte_total <- data_echan_graph_dequip_gfp() %>%
      group_by(nom) %>%
      summarise(montant=sum(montant))
  })

  
  output$gfpssbc <- renderUI({
    test <- data_echan_gfp() %>% 
      filter(is.na(dep_equip_com) | is.na(dep_equip_com_ba)) %>% 
      summarise(n = n()) %>% 
      pull(n)
    
    if(input$bc_ou_gfp == 1 & test != 0){
  p("Résultat provisoire : certaines données des dépenses d’équipement des communes membres des GFP sélectionnés ne sont pas encore disponibles"
  )
    }else{
      ""
    }

  })
  output$gfpssbc2 <- renderUI({
    test <- data_echan_gfp() %>% 
      filter(is.na(dep_equip_com) | is.na(dep_equip_com_ba)) %>% 
      summarise(n = n()) %>% 
      pull(n)
    
    if( test != 0){
      p("Résultat provisoire : certaines données des dépenses d’équipement des communes membres des GFP sélectionnés ne sont pas encore disponibles"
      )
         }else{
      ""
    }
    
  })
  
  # Premier grah
  output$grap_dep_equip_gfp <- renderPlot({
    # test <- data_echan_gfp() %>% 
    #   filter(is.na(dep_equip_com) | is.na(dep_equip_com_ba)) %>% 
    #   summarise(n = n()) %>% 
    #   pull(n)
    # if(input$bc_ou_gfp == 1 & test != 0){
    # 
    #   }else{
    appliInvest::graph_dep_equip(etiquette = data_etiquette_total_gfp(),data = data_echan_graph_dequip_gfp(),entite = data_gfp(),nom_gfp_com = "Groupement",gfp_ou_com = 2,gfp_ou_bc = input$bc_ou_gfp)
      # }
      })

  data_stat_echan_gfp <- eventReactive(input$go_gfp,{
    appliInvest::data_stat_echan(data_echan_gfp(),gfp_ou_com = 2)
  })

  output$stat_echan_moy_gfp <- renderTable(digits = 0,{
    if(nrow(data_gfp()) == 0 & (nrow(data_echan_gfp())) >= 30){  
      data_stat_echan_gfp() %>% 
        rename("€/hab., en 2017"  = variable) %>% 
        as_tibble()
      
    }else if(nrow(data_gfp()) != 0 & (nrow(data_echan_gfp())) >= 30){
      data_stat_echan_gfp() %>% 
        rename("€/hab., en 2017"  = variable) %>% 
        as_tibble()
    }else{NULL}
    
    })
  
   output$stat_echan_moy_gfp_sup_30 <- renderUI({
     if(nrow(data_echan_gfp()) < 30){
   p("Echantillon trop petit pour présenter des informations statistiques.")
     }else if (nrow(data_echan_gfp()) >= 30){
       p("1er quartile : 25% des GFP de l'échantillon ont une valeur inférieure.",br(),
             "3e quartile : 25% des GFP de l'échantillon ont une valeur supérieure.",br(),
             "Médiane : la moitié des GFP a une valeur inférieure.",br(),
             "Traitement OFGL - Source : balances comptables DGFIP")}
   })


  data_graph_finance_gfp <- reactive({
    appliInvest::data_graph_finan(echantillon = data_echan_gfp(), entite = data_gfp(),gr_de_ref = data_gr_ref_gfp(),nom_gfp_com = "Groupement",annee_encours = annee_encours,bp_ba = input$bp_ba_gfp1)
  })

  output$graph_finance_gfp <- renderPlot({
    appliInvest::graph_financement(data_graph = data_graph_finance_gfp(),entite = data_gfp(),nom_gfp_com = "Groupement",bp_ba = input$bp_ba_gfp1)
  })
  
  output$phrase_gfp <- renderUI ({
    montant <- data_graph_finance_gfp() %>% select(montant) %>% filter(!is.na(montant))
    indice <- min(montant)
    
    if(indice < 0){
      p("Si le cumul des moyens de financement (y compris fraction négative d'épargne nette) est inférieur à 100%, la différence est financée par ponction sur le fonds de roulement. Dans le cas inverse, il y a augmentation du fonds de roulement.")
    }else{
      p("Si le cumul des moyens de financement est inférieur à 100%, la différence est financée par ponction sur le fonds de roulement. Dans le cas inverse, il y a augmentation du fonds de roulement.")
    }
  })
  
  data_graph_equip_bc_gfp <- reactive({
    appliInvest::data_bc_dep_equip(echantillon = data_echan_gfp(),entite=data_gfp(),gr_ref=data_gr_ref_gfp())
  })

  output$graph_bc_gfp <- renderPlot({
    # test <- data_echan_gfp() %>% 
    #   filter(is.na(dep_equip_com) | is.na(dep_equip_com_ba)) %>% 
    #   summarise(n = n()) %>% 
    #   pull(n)
    # 
    # if(test != 0){}else{
    # 
    appliInvest::graph_bc_dep_equip(data = data_graph_equip_bc_gfp(),entite = data_gfp(),nom_gfp_com = "Groupement")
    # }
      })

  data_graph_zoom_rec_invest_gfp <- reactive({
    appliInvest::data_zoom_rec_invest(echantillon = data_echan_gfp(),entite = data_gfp(),gr_de_ref = data_gr_ref_gfp(), bp_ba = input$bp_ba_gfp2,nom_gfp_com = "Groupement")
  })

  output$graph_zoom_rec_invest_gfp <- renderPlot({
    appliInvest::graph_zoom_rec_invest(data_graph = data_graph_zoom_rec_invest_gfp(),entite = data_gfp(),nom_gfp_com = "Groupement",bp_ba = input$bp_ba_gfp2)
  })

  data_graph_zoom_dep_equip_gfp <- reactive({
    appliInvest::data_zoom_dep_equip(echantillon = data_echan_gfp(),entite = data_gfp(),gr_de_ref = data_gr_ref_gfp(), bp_ba = input$bp_ba_gfp3,nom_gfp_com = "Groupement",annee_encours = annee_encours)
  })

  output$graph_zoom_dep_equip_gfp <- renderPlot({
    appliInvest::graph_zoom_dep_equip(data_graph = data_graph_zoom_dep_equip_gfp(),entite = data_gfp(),nom_gfp_com = "Groupement",bp_ba = input$bp_ba_gfp3)
  })

  #derniere partie sur la base à télécharger

  data_telechager_gfp <- reactive({
    if(nrow(data_gfp()) != 0){
      data2 <- appliInvest::base_invest_gfp %>%
        filter(annee %in% c(input$annee_gfp_tel) & nom_dep %in% c(nom_dep_gfp()$nom_dep))
    data <- rbind(data_gfp(),data2,data_echan_gfp())
    data <- data %>%
      distinct() %>%
      mutate(vdfr = rec_invest + rec_fonc + emprunt - dep_fonc - dep_invest - remboursement,
             vdfr_ba = rec_invest_ba + rec_fonc_ba + emprunt_ba - dep_fonc_ba - dep_invest_ba - remboursement_ba) %>% 
      rename(dep_invest_hr = dep_invest,dep_invest_hr_ba = dep_invest_ba,autre_dep_equip_brut = autres_dep_equip,autre_dep_equip_brut_ba = autres_dep_equip_ba,
             rec_invest_he = rec_invest,rec_invest_he_ba = rec_invest_ba,sub_dot_hfctva = compte13_10,sub_dot_hfctva_ba = compte13_10_ba,rem_dette=remboursement,rem_dette_ba=remboursement_ba,
             stock_dette = dette,stock_dette_ba = dette_ba,flux_croise_i = fc_i,strate = strate16,nj_epci = nj_epci2016,fisc_epci = fisc_epci2016) %>%
      mutate(autre_dep_invest=dep_invest_hr - dep_equip - subvention_204,autre_dep_invest=dep_invest_hr_ba - dep_equip_ba - subvention_204_ba)  %>%
      select(-planFCTVA_r,-planFCTVA_d,-planFCTVA_r_ba,-planFCTVA_d_ba,-fc_f,-autres_rec_invest_hors_planrelance,-autres_rec_invest_hors_planrelance_ba) %>%
      mutate_if(is.numeric,.funs = function(x){round(x,3)})
    }else{
      data2 <- appliInvest::base_invest_gfp %>%
        filter(annee %in% c(input$annee_gfp_tel) & nom_dep %in% c(nom_dep_gfp()$nom_dep))
      data <- rbind(data2,data_echan_gfp())
      data <- data %>%
        mutate(vdfr = rec_invest + rec_fonc + emprunt - dep_fonc - dep_invest - remboursement,
               vdfr_ba = rec_invest_ba + rec_fonc_ba + emprunt_ba - dep_fonc_ba - dep_invest_ba - remboursement_ba) %>% 
      rename(dep_invest_hr = dep_invest,dep_invest_hr_ba = dep_invest_ba,autre_dep_equip_brut = autres_dep_equip,autre_dep_equip_brut_ba = autres_dep_equip_ba,
             rec_invest_he = rec_invest,rec_invest_he_ba = rec_invest_ba,sub_dot_hfctva = compte13_10,sub_dot_hfctva_ba = compte13_10_ba,rem_dette=remboursement,rem_dette_ba=remboursement_ba,
             stock_dette = dette,stock_dette_ba = dette_ba,flux_croise_i = fc_i,strate = strate16,nj_epci = nj_epci2016,fisc_epci = fisc_epci2016) %>% 
      mutate(autre_dep_invest=dep_invest_hr - dep_equip - subvention_204,autre_dep_invest=dep_invest_hr_ba - dep_equip_ba - subvention_204_ba) %>%
      select(-planFCTVA_r,-planFCTVA_d,-planFCTVA_r_ba,-planFCTVA_d_ba,-fc_f,-autres_rec_invest_hors_planrelance,-autres_rec_invest_hors_planrelance_ba) %>%
      mutate_if(is.numeric,.funs = function(x){round(x,3)})}
  })


  output$export_data_gfp <- downloadHandler(

    filename = function(){
      data <- data_echan_gfp() %>% filter(annee == 2017) %>% summarise(n=n())
      paste0(data$n,"col-ofgl-keuros",Sys.Date(),".csv")},
    content = function(file) {
      write.csv2(data_telechager_gfp(), file, row.names = FALSE)
    })


   output$table_gfp <- renderDataTable({

     data <- data_telechager_gfp() %>%
       filter(annee %in% c(2017,input$annee_gfp_tel)) %>%
       mutate_if(is.numeric,.funs = function(x){round(x,1)})
       
     
     datatable(data,options = list(
       'select' = FALSE,
       'scrollX' = TRUE,
       'lengthChange' =FALSE,
       'info' = FALSE,
       'serverSide' = TRUE,
       dom="ftp",
       pageLength = 10,
       language = list(
         'search'="Filtrer :",
         'paginate' = list(
           'previous' = "Précédent",
           'next' = "Suivant"
         ))
     ))
   })

























   #### Partie sur les deps ====

   observe({
    values$reg_deps <- input$reg_echan_deps
    values$dep_deps <- input$mes_dep_selec_deps
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


    updatePickerInput(session,
                      inputId = "mes_dep_selec_deps",
                      choices = sort(mes_dep$nom_dep_accent),
                      selected = sort(mes_dep$nom_dep_accent))
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
  observe({
    mon_dep <- list_dep



    updateSelectizeInput(session,
                         inputId = "mon_dep",
                         server = TRUE,
                         choices = sort(mon_dep$nom_dep_accent),
                         options = list(
                           maxOptions = 1000,
                           label = mon_dep$nom_dep_accent,
                           render = I(
                             '{
                             option: function(item, escape) {
                             return "<div><strong>" + escape(item.value) + "</strong></div>"
                             }
  }')),
                         selected = "vide"
                           ) # me permet de rendre la selection vide..
    })

  observe({
    if(length(input$cas_spe_deps) == 0){
    nb_ligne1 <- base_invest_dep %>%
       filter(nom_dep %in% c(nom_dep_dep()$nom_dep) & urb_rur_2017 %in% c(input$dpu_dfm) & strate16 %in% c(input$pop_dep)) #& cas_spe %in% c(0,input$cas_spe_deps))
    nb_ligne <- nrow(nb_ligne1)
    }else if(length(input$cas_spe_deps) != 0){
      nb_ligne1 <- base_invest_dep %>%
        filter(nom_dep %in% c(nom_dep_dep()$nom_dep) & urb_rur_2017 %in% c(input$dpu_dfm) & strate16 %in% c(input$pop_dep)) #& cas_spe %in% c(0,input$cas_spe_deps))
      nb_ligne2 <- base_invest_dep %>%
        filter(nom_dep %in% c(input$cas_spe_deps))
      nb_ligne <- nrow(nb_ligne1) + nrow(nb_ligne2)}


    nb_ligne_dep <- base_invest_dep %>%
      filter(nom_dep %in% c(nom_dep_indiv()$nom_dep))
    nb_ligne_dep_echan <- nrow(data_echan_dep()) + nrow(nb_ligne_dep)

    if(nrow(data_dep()) == 0){
      if(nb_ligne != nb_ligne_dep_echan){
        showElement(id = "go_dep",time = 0.1)
      }else{
        hide(id = "go_dep",time= 0.1)}
    }else if(nrow(data_dep()) != 0){
      if(nrow(nb_ligne_dep) == 0){
        showElement(id = "go_dep",time = 0.1)
      }else{
        if(nb_ligne != nrow(data_echan_dep()) | nb_ligne_dep$nom_dep != data_dep()$nom_dep){
          showElement(id = "go_dep",time = 0.1)
        }else{
          hide(id = "go_dep",time= 0.1)}}
    }
  })


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
      summarise(pop_pour = sum(population)/(68732961)*100, pop_nb = round(sum(population)/1000000,2), dep_equip_t = sum(dep_invest + dep_invest_ba-fc_i)/1000, dep_equip_pour = (sum(dep_invest + dep_invest_ba-fc_i))/9902288*100,dep_equip_ba_bp = (sum(dep_invest_ba-fc_i)/(sum(dep_invest + dep_invest_ba-fc_i)))*100)
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
     graph + labs(subtitle = "Budgets principaux + Budgets annexes")
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

  output$graph_nuage_dep <- renderPlotly({

    if(input$annee_dep == 1){
      data <- data_nuage() %>%
        filter(nom != "Gr de réf") %>%
        mutate(dep_equip = round(dep_equip,0),subvention_204 = round(subvention_204,0))
      data$nom <- fct_relevel(as.factor(data$nom),c("Echantillon","Département"))
      
        
        appliInvest::graph_sub_dep_equip(annee = annee_encours,data_graph = data,entite = data_dep())
    
    }else if(input$annee_dep == 2){
      data <- data_nuage() %>%
        filter(nom != "Gr de réf") %>%
        mutate(dep_equip = round(dep_equip,0),subvention_204 = round(subvention_204,0)) 
      
      data$nom <- fct_relevel(as.factor(data$nom),c("Echantillon","Département"))
      
      appliInvest::graph_sub_dep_equip(annee = "2012-2017",data_graph = data,entite = data_dep())}
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
      graph <- graph + labs(title ="Structure des dépenses d'équipement brutes de 2012 à 2017, en %",subtitle = "Budgets principaux, moyenne annuelle")
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


  output$table_dep <- renderDataTable({
    data <- data_telechager_dep() %>%
      dplyr::arrange(nom_dep)

    datatable(data,options = list(
      'select' = FALSE,
      'scrollX' = TRUE,
      'lengthChange' =FALSE,
      'info' = FALSE,
      'serverSide' = TRUE,
      dom="ftp",
      pageLength = 10,
      language = list(
        'search'="Filtrer :",
        'paginate' = list(
          'previous' = "Précédent",
          'next' = "Suivant"
        ))
    ))

  })

  
  
  
  
  
  
  #### Partie sur les regs ====
  
  observe({
    values$reg_regs <- input$reg_echan_regs
    values$coms_regs <- input$mon_reg
    values$bp_ba_reg <- input$bp_ba_reg2
    # values$strate_dep <- input$pop_dep
  })

  
  # nom_dep_reg <- reactive({
  #   
  #   list_dep_ssaccent <- list_dep %>%
  #     filter(nom_dep_accent %in% c(values$dep_deps)) %>%
  #     select(nom_dep,nom_reg)
  #   
  #   list_dep_ssaccent
  # })
  # 
  # nom_dep_indiv <- reactive({
  #   
  #   list_dep_ssaccent <- list_dep %>%
  #     filter(nom_dep_accent %in% c(values$coms_deps)) %>%
  #     select(nom_dep)
  #   
  #   list_dep_ssaccent
  # })
  
observe({
  updateSelectizeInput(inputId = "mon_reg",
                    session,
              label = "",
              selected = "vide",
              choices = sort(list_reg$nom_reg_accent))
})
  
  observe({

      nb_ligne1 <- base_invest_reg %>%
        filter(nom_reg_accent %in% c(values$reg_regs)) #& cas_spe %in% c(0,input$cas_spe_deps))
      nb_ligne <- nrow(nb_ligne1)
      text_reg <- paste(base_invest_reg[base_invest_reg$nom_reg_accent %in% values$reg_regs & base_invest_reg$annee == 2017,],sep="",collapse = "")
    
    
    nb_ligne_reg <- base_invest_reg %>%
      filter(nom_reg_accent %in% c(values$coms_regs))
    text_reg_2 <- paste(data_echan_reg()[data_echan_reg()$nom_reg_accent %in% values$reg_regs & data_echan_reg()$annee == 2017,],sep="",collapse = "")
    
    nb_ligne_reg_echan <- nrow(data_echan_reg()) + nrow(nb_ligne_reg)
    
    if(nrow(data_reg()) == 0 ){
      if(nb_ligne != nb_ligne_reg_echan | text_reg != text_reg_2){
        showElement(id = "go_reg",time = 0.1)
      }else{
        hide(id = "go_reg",time= 0.1)}
    }else if(nrow(data_reg()) != 0){
      if(nrow(nb_ligne_reg) == 0){
        showElement(id = "go_reg",time = 0.1)
      }else{
        if(nb_ligne != nrow(data_echan_reg()) | text_reg != text_reg_2 | nb_ligne_reg$nom_reg_accent != data_reg()$nom_reg_accent){
          showElement(id = "go_reg",time = 0.1)
        }else{
          hide(id = "go_reg",time= 0.1)}}
    }
  })
  
  
  data_echan_reg <- eventReactive(input$go_reg,{
    
    list_reg <- list_dep_com %>% 
      filter(nom_reg_accent %in% values$reg_regs) %>% 
      select(nom_reg) %>% 
      distinct()
    
      base_invest_reg %>%
        filter(nom_reg %in% list_reg$nom_reg)
  })
  
  
  ##Etape data pour la selection du dep
  data_reg <- eventReactive(input$go_reg,{
    list_reg <- list_dep_com %>% 
      filter(nom_reg_accent %in% values$coms_regs) %>% 
      select(nom_reg) %>% 
      distinct()
    
    base_invest_reg %>%
      filter(nom_reg %in% list_reg$nom_reg) %>%
      mutate(nom_dep = NULL)
    
  })
  
  #Etape data pour creer le groupe de reference
  data_gr_ref_reg <- eventReactive(input$go_reg,{
    base_invest_reg %>% filter(!nom_reg_accent %in% c("Corse","Guyane","Martinique","Guadeloupe","La Réunion"))
  })
  
  data_infobox_reg <- reactive({
    echan <- data_echan_reg() %>%
      filter(annee == annee_encours) %>%
      summarise(pop_pour = sum(population)/67357997*100, pop_nb = round(sum(population)/1000000,2), dep_equip_t = sum(dep_invest)/1000, dep_equip_pour = (sum(dep_invest))/9683567*100)#(sum(dep_invest_ba-fc_i)/(sum(dep_invest + dep_invest_ba-fc_i)))*100)
  })
  # pour les info box sur l'échantillon
  
  output$info_echan_box1_reg <- renderUI({
    
    data <- data_echan_reg() %>%
      filter(annee == annee_encours)
    
      p(nrow(data), " région(s) sélectionnée(s)")
    
    #ajouter nb BP BA et année selectionné
    
  })
  output$info_echan_box7_reg <- renderUI({
    #nombre d'habitant
    p("Sur 17 régions françaises")
  })
  
  output$info_echan_box2_reg <- renderUI({
    #nombre d'habitant
    p(paste(data_infobox_reg()[1,2])," million(s) d'habitants")
  })
  output$info_echan_box3_reg <- renderUI({
    #%de la pop francaise
    p("Soit ",paste(round(data_infobox_reg()[1,1],1),"% de la population française"))
  })
  output$info_echan_box4_reg <- renderUI({
    #
    p(paste(round(data_infobox_reg()[1,3],1),"M€ de dépenses d'investissement"))
  })
  output$info_echan_box5_reg <- renderUI({
    #
    p("Soit ",paste(round(data_infobox_reg()[1,4],1),"% des dépenses d'investissement des régions"))
  })
  
  #Premier graph sur la structure des depenses d'invest
  data_echan_graph_dequip_reg <- reactive({
   test_fonc <-  appliInvest::data_DRI(echantillon = data_echan_reg(),entite = data_reg(),gr_ref = data_gr_ref_reg(),bp_ba = 1)
 
    })
  
  #donne pour etiquette premier graph
  data_etiquette_total_reg <- reactive({
    equitte_total <- data_echan_graph_dequip_reg() %>%
      group_by(nom) %>%
      summarise(montant=sum(montant))
   
  })
  
  # Premier grah
  output$graph_dep_invest_reg <- renderPlot({
   graph <-   appliInvest::graph_dep_invest(etiquette = data_etiquette_total_reg(),data = data_echan_graph_dequip_reg(),entite = data_reg(),nom_gfp_com = "Région") +
     labs(subtitle = "Budgets principaux")
    
  graph
     })
  
  # graph nuage de point
  
  
  data_nuage_reg <- reactive({
    if(input$annee_reg == 1){
      appliInvest::data_sub_dep_equip(annee_choisi = 2017,nom_dep_reg = "Région",echantillon = data_echan_reg(),entite = data_reg(),gr_de_ref = data_gr_ref_reg())
    }else if(input$annee_reg == 2){
      appliInvest::data_sub_dep_equip(annee_choisi = "2012-2017",nom_dep_reg = "Région",echantillon = data_echan_reg(),entite = data_reg(),gr_de_ref = data_gr_ref_reg())
    }
  })
  
  output$graph_nuage_reg <- renderPlotly({
    
    if(input$annee_reg == 1){
      data <- data_nuage_reg() %>%
        filter(nom != "Gr de réf") %>%
        mutate(dep_equip = round(dep_equip,0),subvention_204 = round(subvention_204,0))
      data2 <- data_nuage_reg() %>%
        filter(nom == "Gr de réf") %>%
        group_by(nom) %>% 
        summarise(dep_equip = round(sum(dep_equip,na.rm = TRUE),0),subvention_204 = round(sum(subvention_204,na.rm = TRUE),0))
      data$nom <- fct_relevel(as.factor(data$nom),c("Echantillon","Région"))
      
      
      appliInvest::graph_sub_dep_equip(annee = annee_encours,data_graph = data,entite = data_reg())
      
    }else if(input$annee_reg == 2){
      data <- data_nuage_reg() %>%
        filter(nom != "Gr de réf") %>%
        mutate(dep_equip = round(dep_equip,0),subvention_204 = round(subvention_204,0)) 
      
      data$nom <- fct_relevel(as.factor(data$nom),c("Echantillon","Région"))
      
      appliInvest::graph_sub_dep_equip(annee = "2012-2017",data_graph = data,entite = data_reg())}
  

  })
  
  # financement
  
  data_graph_finance_reg <- reactive({
    appliInvest::data_graph_finan(echantillon = data_echan_reg(),entite = data_reg(), gr_de_ref = data_gr_ref_reg(),nom_gfp_com = "Région",annee_encours = 2017,bp_ba = 1)
  })
  
  output$graph_finance_reg <- renderPlot({
    appliInvest::graph_financement(data_graph = data_graph_finance_reg(),entite = data_reg(),nom_gfp_com = "Région",bp_ba =1)
  })
  
  output$phrase_reg <- renderUI ({
    montant <- data_graph_finance_reg() %>% select(montant) %>% filter(!is.na(montant))
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
                      inputId = "var_select_reg",
                      choices = list( 
                        "Dépenses d'investissement hors remb." = "dep_invest",
                        "Dépenses d'équipement" = "dep_equip",
                        "Subventions versées" = "subvention_204",
                        "Recettes d'investissement hors emp." = "rec_invest",
                        "Epargne nette" = "epn",
                        "Emprunts" = "emprunt",
                        "Variation du fonds de roulement" = "var_fon_roul"))
    
  })
  
  
  data_graph_evol_reg <- reactive({
    appliInvest::data_grap_evol(echantillon = data_echan_reg(),entite = data_reg(),gr_de_ref = data_gr_ref_reg(),bp_ba = 1,nom_gfp_com = "Région",ma_var = input$var_select_reg)
  })
  
  
  output$graph_evol_reg <- renderPlot({
    
    graph_evol(data_evol =  data_graph_evol_reg(),annee_dispo = c(2012,2013,2014,2015,2016,2017),bp_ba = 1,var_select = input$var_select_reg,entite = data_reg())
  })
  
  data_sub_verse_reg <-  reactive({
    if(input$annee_reg3 == 1){
      appliInvest::data_sub_204_reg(echantillon = data_echan_reg(),entite = data_reg(),gr_ref = data_gr_ref_reg(),annee_encours = 2017)
    }else if(input$annee_reg3 == 2){
      appliInvest::data_sub_204_reg(echantillon = data_echan_reg(),entite = data_reg(),gr_ref = data_gr_ref_reg(),annee_encours = c(2012,2013,2014,2015,2016,2017))
    }
  })
  
  output$graph_sub_204_reg <- renderPlot({
    if(input$annee_reg3 == 1){
      appliInvest::graph_sub_204_reg(data = data_sub_verse_reg(),entite = data_reg(),nom_gfp_com = "Région",quel_annee = 1 )
    }else if(input$annee_reg3 == 2){
      appliInvest::graph_sub_204_reg(data = data_sub_verse_reg(),entite = data_reg(),nom_gfp_com = "Région",quel_annee = 2 )
    }
    
  })
  
  observe({
    
    updateSelectInput(session,
                      inputId = "var_select_reg_sub",
                      choices = list(
                        "Total sub. invest. versées" ="subvention_204",
                        "Sub. versées à l'état" = "sub_204_etat",
                        "Sub. versées au bloc communal" = "sub_204_bc",
                        "Sub. versées aux orga. de transport" = "sub_204_transport",
                        "Sub. versées autres orga. public" = "sub_204_autres" ,
                        "Sub. versées aux orga. privés" = "sub_204_prive"),
                      selected = "subvention_204")
    
  })
  
  
  data_graph_evol_reg_sub <- reactive({
    appliInvest::data_grap_evol(echantillon = data_echan_reg(),entite = data_reg(),gr_de_ref = data_gr_ref_reg(),bp_ba = 1,nom_gfp_com = "Région",ma_var = input$var_select_reg_sub)
  })
  
  
  output$graph_evol_reg_sub <- renderPlot({
    
    graph_evol(data_evol =  data_graph_evol_reg_sub(),annee_dispo = c(2012,2013,2014,2015,2016,2017),bp_ba = 1,var_select = input$var_select_reg_sub,entite = data_reg())
  })
  
  data_telechager_reg <- reactive({
    
    if(nrow(data_reg()) != 0){
      
      data <- rbind(data_reg(),data_echan_reg())
      data <- data %>%
        distinct() %>%
        rename(dep_invest_hr = dep_invest,autre_dep_equip_brut = autres_dep_equip,
               rec_invest_he = rec_invest,sub_dot_hfctva = compte13_10,rem_dette=remboursement,
               stock_dette = dette,sub_204_autre_public = sub_204_autres,vdfr = var_fon_roul) %>%
        select(-X,-nom_reg_accent,-remb_avance,-dep_equip_brut,-terrains,-constructions,-reseaux,-bien_meuble,-autre_dep_equip_brut) %>%
        mutate_if(is.numeric,.funs = function(x){round(x,3)})
      
    }else{data <- data_echan_reg() %>%
      rename(dep_invest_hr = dep_invest,autre_dep_equip_brut = autres_dep_equip,
             rec_invest_he = rec_invest,sub_dot_hfctva = compte13_10,rem_dette=remboursement,
             stock_dette = dette,sub_204_autre_public = sub_204_autres,vdfr = var_fon_roul) %>%
      select(-X,-nom_reg_accent,-remb_avance,-dep_equip_brut,-terrains,-constructions,-reseaux,-bien_meuble,-autre_dep_equip_brut) %>%
      mutate_if(is.numeric,.funs = function(x){round(x,3)})
    
    }
  })
  
  
  output$export_data_reg <- downloadHandler(
    
    filename = function(){
      paste0(length(unique(data_echan_reg()$nom_reg)),"col-ofgl-keuros",Sys.Date(),".csv")},
    content = function(file) {
      write.csv2(data_telechager_reg(), file, row.names = FALSE)
    })
  
  
  output$table_reg <- renderDataTable({
    data <- data_telechager_reg() %>%
      dplyr::arrange(nom_reg)
    
    datatable(data,options = list(
      'select' = FALSE,
      'scrollX' = TRUE,
      'lengthChange' =FALSE,
      'info' = FALSE,
      'serverSide' = TRUE,
      dom="ftp",
      pageLength = 10,
      language = list(
        'search'="Filtrer :",
        'paginate' = list(
          'previous' = "Précédent",
          'next' = "Suivant"
        ))
    ))
    
  })
  
  
  })
