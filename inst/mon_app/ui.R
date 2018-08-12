library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  theme = shinytheme("cerulean"),
  # tags$head(tags$style(HTML('#var_select_style .selectize-input {color: #fff;background-color: #20c997;border-color: #20c997;border-style:outset;display: inline-block;height:40px'))),
  #1er page
  ##
  
  fluidRow(
    tags$header(
      h1(
        img(src = 'logo.png', align = "left", width = "125px"),
        img(src = 'logo2.png', align = "right", width = "152px"),
        "Cap sur la « data » de l’investissement public local",
        style = "text-align:center;background-color:#2FA4E7;color:#fff;border-collapse:collapse;height:50px;border: 2px #2FA4E7 ridge"
      )
      
    ),
    useShinyjs(),
    
    navbarPage(
      id = "navbar",
      "",
      # page de presentation du module
      
      
      tabPanel(
        title = div("Accueil",
                    style = "color:#fff;font-size:1.3em;"),
        value = "accueil",
        
        fluidPage(
          column(
            6,
            h2("Entrez au cœur de l'investissement public local"),
            div(img(
              src = "image.jpg", align = "left", width = 200
            ), style = "float:left;border:5px solid #fff"),
            p(
              "Cette application proposée par ",
              strong("l’Observatoire des finances et de la gestion publique locales"),
              " permet de",
              strong(
                "dresser un portrait financier de l’investissement public local sur un groupe de collectivités"
              ),
              ". Il fournit aux décideurs locaux et à leurs partenaires des éléments qui peuvent les accompagner dans le développement des politiques publiques locales."
            ),
            p(
              "L’utilisateur analyse les dépenses d'investissement d'un échantillon de collectivités locales, il a la possibilité d'isoler une entité parmi ce groupe.",
              strong(
                "Il dispose ensuite de représentations graphiques et de données individuelles qu’il peut extraire pour construire sa propre analyse."
              )
            ),
            p(
              "L’OFGL rappelle que ",
              strong(
                "les valeurs moyennes ne constituent pas des objectifs ou des références absolues mais reflètent une situation d’ensemble à un instant"
              ),
              ". Toutes les collectivités ne se situent pas au même stade de leur projet de territoire : les choix politiques, les réalisations passées, l’état du patrimoine, la situation géographique, les caractéristiques économiques, démographiques et sociales, la situation financière, les modes de gestion, les compétences… les facteurs différenciants sont multiples et doivent, en fonction des cas, être rappelés pour nourrir l’analyse."
            ),
            p(
              "Les données présentées sont issues de choix et de traitements de l’OFGL à partir des bases de données « Balances comptables » fournies par la DGFiP (ministère des finances), d'informations de la DGCL et des populations produites par l’INSEE."
            ),
            br(),
            
            style = "text-align:justify"
          ),
          
          column(
            6,
            h2("Le mot du président"),
            div(img(
              src = "andrelaignel.png", align = "right", width = 120
            ), style = "float:right;border:5px solid #fff"),
            p(
              strong("Les équipements publics locaux"),
              "accompagnent chacun d’entre nous dans son quotidien et tout au long de sa vie. Si nous prenons le temps d’observer le déroulement de notre journée ou de celle de nos proches, nous trouverons sur nos parcours un nombre impressionnants d’équipements : réseaux (voirie et trottoirs, eau, numérique, chauffage, transports collectifs…), bâtiments (logements, bâtiments scolaires ou administratifs,…), ouvrages d’art (ponts, tunnels,..), lieux de vie (parcs, forêts, terrain de sport,…) et de culture (médiathèques, théâtres…)."
            ),
            p(
              "La grande majorité de ces équipements publics est sous la responsabilité des collectivités locales (communes et intercommunalités, départements, régions).
              Ils sont la traduction des dépenses d’investissement réalisées au cours des 30 ou 40 dernières années mais aussi des dépenses courantes nécessaires à leur entretien ou à leur gestion au quotidien."
            ),
            p(
              strong("L’Observatoire des finances et de la gestion publique locales"),
              "est un lieu de collecte, d’analyse et de partage de l’information. A ma demande et à celle de son conseil d’orientation, il se penche actuellement sur ces investissements producteurs de valeurs et de services."
            ),
            p(
              strong("L’application « Cap sur la data de l’investissement local»"),
              "constitue l’un des volets de ce chantier, elle permet aux décideurs locaux d’obtenir des informations financières précieuses sur ce qui est réalisé dans des territoires comparables au leur. Quel niveau ? Quels financements ? Quelle évolution ? Chacun pourra, à partir de ses propres caractéristiques et objectifs, affiner ses projets et ses ambitions au service des habitants et des entreprises."
            ),
            p(strong("André Laignel"),
              br(),
              "Président de l’OFGL"),
            style = "text-align:justify;height:700px"
            )
          
      )
      
      ),
      #Fin de la page d'accueil
      
      
      
      
      #### COMMUNE====
      tabPanel(
        title = div("Communes", style = "color:#fff;font-size:1.3em;"),
        value = "com",
        
        fluidPage(###### selection des entités ====
                  
                  fluidRow(
                    column(
                      h4("1 - Sélectionnez votre échantillon de communes :", style = " text-align:center"),
                      # Choisir sa région pour filtrer l'echantillon
                      column(
                        6,
                        
                        pickerInput(
                          "reg_echan_com",
                          label = "Choisissez une (des) région(s) :",
                          choices = unique(sort(list_dep$nom_reg_accent)),
                          multiple = TRUE,
                          options = list(
                            `actions-box` = TRUE,
                            `deselect-all-text` = "Tout désélectionner",
                            `select-all-text` = "Tout sélectionner",
                            `title` = "Vide"
                          )
                        ),
                        
                        # choisir son departement, en fonction des regions donc update dans server
                        
                        pickerInput(
                          inputId = "mes_dep_selec_com",
                          label = "Affinez par département :",
                          choices = NULL,
                          choicesOpt = NULL,
                          options = list(
                            `actions-box` = TRUE,
                            `deselect-all-text` = "Tout désélectionner",
                            `select-all-text` = "Tout sélectionner",
                            `title` = "Vide"
                          ),
                          multiple = TRUE
                        )
                        
                      ),
                      
                      column(
                        # Choix d'une ou plusieurs strates de population
                        pickerInput(
                          "pop",
                          label = "Choisissez une (des) strate(s) de population :",
                          choices = list(
                            "0 - 200 hab." = "(0-200]",
                            "200 - 500 hab." = "(200-500]",
                            "500 - 2 000 hab." = "(500-2000]",
                            "2 000 - 3 500 hab. " = "(2000-3500]" ,
                            "3 500 - 5 000 hab." = "(3500-5000]" ,
                            "5 000 - 10 000 hab." =  "(5000-10000]" ,
                            "10 000 - 20 000 hab." =  "(10000-20000]" ,
                            "20 000 - 50 000 hab." =  "(20000-50000]",
                            "50 000 - 100 000 hab." = "(50000-100000]",
                            "100 000 hab. et plus"  = "100000 et plus",
                            "Paris" = "Paris"
                          ),
                          selected = list(
                            "(0-200]",
                            "Paris",
                            "(200-500]",
                            "(500-2000]" ,
                            "(2000-3500]" ,
                            "(3500-5000]" ,
                            "(5000-10000]",
                            "(10000-20000]",
                            "(20000-50000]",
                            "(50000-100000]",
                            "100000 et plus"
                          ),
                          options =  list(
                            `actions-box` = TRUE,
                            `deselect-all-text` = "Tout désélectionner",
                            `select-all-text` = "Tout sélectionner",
                            `title` = "Vide"
                          ),
                          multiple = TRUE
                        ),
                        
                        column(
                          6,
                          awesomeCheckboxGroup(
                            "touristique",
                            label = "",
                            choices = list("Touristique" = 1, "Non touristique" = 0),
                            status = "primary",
                            selected = c(1, 0)
                          )
                        ),
                        column(
                          awesomeCheckboxGroup(
                            "montagne",
                            label = "",
                            choices = list("Montagne" = 1, "Non montagne" = 0),
                            status = "primary",
                            selected = c(1, 0)
                          ),
                          width = 6
                        ),
                        width = 6
                      ),
                      
                      width = 6
                    ),
                    #fin de la première colonne de la premiere ligne
                    
                    
                    # deuxieme colonne, automatisation d'un groupe de réf
                    column(
                      3,
                      h4("2 - Groupe de référence :"),
                      tags$div(
                        uiOutput("phrase_gr_ref"),
                        
                        actionBttn(
                          "Clique_gr_ref",
                          "Affinez le groupe de référence",
                          style = "simple",
                          color = "primary",
                          size = "sm"
                        ),
                        hidden(
                          div(
                            id = "choix_gr_ref",
                            awesomeRadio(
                              "geo_gr_ref",
                              label = "",
                              choices =
                                list(
                                  "France entière" = 3,
                                  "France métropolitaine" = 2,
                                  "France métropolitaine hors IDF" = 1
                                ),
                              selected = 2,
                              status = "primary",
                              inline = TRUE
                            ),
                            awesomeCheckboxGroup(
                              "pop_gr_ref",
                              label = "",
                              choices =
                                list("Moins de 10 000 hab." = 1, "Plus de 10 000 hab." = 0),
                              selected = c(1, 0),
                              status = "primary",
                              inline = TRUE
                            ),
                            awesomeCheckboxGroup(
                              "touristique_gr_ref",
                              label = "",
                              choices =
                                list("Touristique" = 1, "Non touristique" = 0),
                              selected = c(1, 0),
                              inline = TRUE,
                              status = "primary"
                            ),
                            awesomeCheckboxGroup(
                              "montagne_gr_ref",
                              label = "",
                              choices =
                                list("Montagne" = 1, "Non montagne" = 0),
                              selected = c(1, 0),
                              status = "primary",
                              inline = TRUE
                            ),
                            style = "background:#fff"
                          )
                        )
                      )
                    ),
                    # Troisieme colonne, permet d'isoler une commune dans les graphs
                    column(
                      h4("Option, en plus de l’échantillon, isolez une commune "),
                      
                      # actionBttn("isocom", "Isoler une commune :",style="simple",color = "primary",size = "xs"),
                      
                      selectInput(
                        inputId = "ma_com",
                        label = "Saisissez les premières lettres :",
                        choices = NULL,
                        selected = NULL
                      ),
                      width = 3
                    )
                  ),
                  fluidRow(
                    div(
                      id = "jean",
                      actionBttn(
                        "go",
                        "Affichez les résultats !",
                        color = "danger",
                        style = "simple"
                      ),
                      style = "text-align:center;line-height:20px;vertical-align:middle"
                    )
                  ),
                  
                  style = "background-color:#deebf7;"),
        # fin de la premiere ligne
        
        hr(style = "height:5px"),
        ## info sur echantillon com ====
        fluidRow(
          # fluidRow(
            p(style = "margin-left:20px",
              "Les données des comptes 2017 de quelques communes et de certains budgets annexes sont encore manquantes. Ce qui explique l’absence de certaines entités dans les phases de sélection et dans les traitements réalisés."
              ),
            h3("Informations 2017 sur l'échantillon sélectionné", style = "text-align:center"),
            box(htmlOutput("info_echan_box1"),
                style = "background-color:#2FA4E7;color:#fff;border: 2px #2FA4E7 ridge;font-size:1.1em;margin: 0px 1px 1px 0px;height:40px;text-align:center;line-height:39px;vertical-align:middle;"),
            box(htmlOutput("info_echan_box7"),
                style = "background-color:#2FA4E7;color:#fff;border: 2px #2FA4E7 ridge;font-size:1.1em;margin: 0px 1px 1px 0px;height:40px;text-align:center;line-height:39px;vertical-align:middle;"),
            box(htmlOutput("info_echan_box2"),
                style = "background-color:#2FA4E7;color:#fff;border: 2px #2FA4E7 ridge;font-size:1.1em;margin: 0px 1px 1px 0px;height:40px;text-align:center;line-height:39px;vertical-align:middle"),
            box(htmlOutput("info_echan_box3"),
                style = "background-color:#2FA4E7;color:#fff;border: 2px #2FA4E7 ridge;font-size:1.1em;margin: 0px 1px 1px 0px;height:40px;text-align:center;line-height:39px;vertical-align:middle;"),
            box(htmlOutput("info_echan_box4"),
                style = "background-color:#2FA4E7;color:#fff;border: 2px #2FA4E7 ridge;font-size:1.1em;margin: 0px 1px 1px 0px;height:40px;text-align:center;line-height:39px;vertical-align:middle;"),
            box(htmlOutput("info_echan_box5"),
                style = "background-color:#2FA4E7;color:#fff;border: 2px #2FA4E7 ridge;font-size:1.1em;margin: 0px 1px 1px 0px;height:40px;text-align:center;line-height:39px;vertical-align:middle;"),
            # box(
            #   textOutput("info_echan_box6"),
            #   style = "background-color:#2FA4E7;color:#fff;border: 2px #2FA4E7 ridge;font-size:1em;margin: 0px 1px 1px 0px;height:39px;text-align:center;line-height:37px;vertical-align:middle;;font-size:1.3em;position:relative")
          # ),
          # fin de la deuxieme ligne pour info echantillon
          
          # fin de la premiere partie, selection entité
          
          
          hr(style = "height:5px;"),
          
          
          # deuxieme partie avec les graph et autes ====
          
          
          # permier graphique sur les dep equipements
          fluidRow(
            column(
              6,
              tags$h3("Enjeux financiers des dépenses d’équipement", style =
                        "text-align:center;"),
              withSpinner(plotOutput("grap_dep_equip"))
            ),
            column(
              6,
              h3(
                "Statistiques des dépenses d'équipement de l'échantillon",
                style = "text-align:center"
              ),
              tableOutput("stat_echan_moy"),
              withSpinner(htmlOutput("stat_echan_moy_com_sup_30"))
            )
          ),
          
          hr(style = "height:5px;"),
          
          # deuxieme graphique sur le shcema de fianancement
          
          fluidRow(
            column(
              6,
              tags$h3("Financement sur ressources propres ou par l’emprunt ?", style =
                        "text-align:center;"),
              fluidRow(div(
                pickerInput(
                  "bp_ba",
                  label = "",
                  choices = list(
                    "Budgets principaux (BP)" = 1,
                    "Budgets annexes (BA)" = 2,
                    "Budgets principaux et budgets annexes" = 3
                  ),
                  selected = 3,
                  options =
                    list(style = "btn-primary")
                )
                
              ),
              style = "height:100px;margin-left:10px"),
              fluidRow(withSpinner(plotOutput(
                "graph_finance_com"
              )),
              htmlOutput("phrase"),
              style = "margin-left:10px")
            ),
            
            #Troisieme graph sur evol
            
            column(
              6,
              tags$h3("Et sur plusieurs années ?", style = "text-align:center;"),
              fluidRow(div(
                column(
                  6,
                  pickerInput(
                    "bp_ba2",
                    label = "",
                    choices = list(
                      "Budgets principaux (BP)" = 1,
                      "Budgets annexes (BA)" = 2,
                      "Budgets principaux et budgets annexes" = 3
                    ),
                    selected = 3,
                    options =
                      list(style = "btn-primary")
                  )
                ),
                column(6, div(
                  pickerInput(
                    "var_select",
                    label = "",
                    choices =  NULL,
                    option = list(style =
                                    "btn-success")
                  )
                )),
                style = "height:100px"
              )),
              fluidRow(withSpinner(plotOutput("graph_evol")))
            )
          ),
          
          hr(style = "height:5px;"),
          
          # Zoom sur les dépenses d'équipement et les recettes d'investissement
          
          fluidRow(
            h3("Pour aller plus loin...", style = "text-align:center;color:#31a354;"),
            column(
              6,
              h3("La structure des recettes d'investissement", style =
                   "text-align:center;color:#31a354;"),
              pickerInput(
                "bp_ba3",
                label = "",
                choices = list(
                  "Budgets principaux (BP)" = 1,
                  "Budgets annexes (BA)" = 2,
                  "Budgets principaux et budgets annexes" = 3
                ),
                selected = 3,
                options =
                  list(style = "btn-primary")
              ),
              withSpinner(plotOutput("graph_zoom_rec_invest_com"))
            ),
            column(
              6,
              h3("La structure des dépenses d'équipement", style =
                   "text-align:center;color:#31a354;"),
              pickerInput(
                "bp_ba4",
                label = "",
                choices = list(
                  "Budgets principaux (BP)" = 1,
                  "Budgets annexes (BA)" = 2,
                  "Budgets principaux et budgets annexes" = 3
                ),
                selected = 3,
                options =
                  list(style = "btn-primary")
              ),
              withSpinner(plotOutput("graph_zoom_sur_dep_com"))
            )
          ),
          
          # derniere partie sur la recup de la donnée
          
          fluidRow(
            h3("A chaque collectivité, une situation spécifique : data", style = "text-align:center;"),
            div(
              column(
                6,
                a(
                  "Explication des variables",
                  href = "Methodologie_Communes_GFP.pdf",
                  target = "_blank"
                ),
                style = "margin-left:20px"
              ),
              column(6,
                     downloadButton("export_data", "Télécharger la base")),
              style = "margin-left:10px"
            )
          ),
          fluidRow(div(
            p("Les valeurs sont exprimées en milliers d'euros"), style = "margin-left:20px"
          )),
          fluidRow(div(dataTableOutput("table"), style =
                         "margin-left:20px"))
        )
        
        # style = "position: fixed;
        #          top: 400px;
        #          bottom: 0px;
        #          width: 100%;
        #          overflow: auto;")
        
        
      ),
      
      # fin tabPanel communes
      
      
      
      
      
      
      
      #### GROUPEMENT ====
      
      tabPanel(
        title = div("Groupements à fiscalité propre", style = "color:#fff;font-size:1.3em;"),
        value = "gfp",
        fluidPage(
          fluidRow(
            fluidRow(
              column(
                6,
                h4(
                  "1 - Sélectionnez votre échantillon de groupements à fiscalité propre (GFP) et EPT :",
                  style = " text-align:center"
                ),
                column(
                  6,
                  pickerInput(
                    "reg_echan_gfp",
                    label = "Choisissez une (des) région(s) :",
                    choices = unique(sort(list_dep$nom_reg_accent)),
                    multiple = TRUE,
                    options = list(
                      `actions-box` = TRUE,
                      `deselect-all-text` = "Tout désélectionner",
                      `select-all-text` = "Tout sélectionner",
                      `title` = "Vide"
                    )
                  ),
                  pickerInput(
                    inputId = "mes_dep_selec_gfp",
                    label = "Affinez par département :",
                    choices = NULL,
                    choicesOpt = NULL,
                    options = list(
                      `actions-box` = TRUE,
                      `deselect-all-text` = "Tout désélectionner",
                      `select-all-text` = "Tout sélectionner",
                      `title` = "Vide"
                    ),
                    multiple = TRUE
                  )
                ),
                column(
                  6,
                  pickerInput(
                    "pop_gfp",
                    label = "Choisissez une (des) strate(s) de population :",
                    choices = list(
                      "0 - 5 000 hab." = "(0-5000]",
                      "5 000 - 15 000 hab." = "(5000-15000]",
                      "15 000 - 25 000 hab." = "(15000-25000]" ,
                      "25 000 - 50 000 hab." =  "(25000-50000]" ,
                      "50 000 - 100 000 hab." =  "(50000-100000]" ,
                      "100 000 - 300 000 hab." =  "(100000-300000]",
                      "300 000 hab. et plus"  = "300000 et plus"
                    ),
                    selected = list(
                      "(0-5000]",
                      "(5000-15000]" ,
                      "(15000-25000]",
                      "(25000-50000]" ,
                      "(50000-100000]" ,
                      "(100000-300000]",
                      "300000 et plus"
                    ),
                    options =  list(
                      `actions-box` = TRUE,
                      `deselect-all-text` = "Tout désélectionner",
                      `select-all-text` = "Tout sélectionner",
                      `title` = "Vide"
                    ),
                    multiple = TRUE
                  ),
                  pickerInput(
                    "nj_epci",
                    label = "Choisissez la (les) nature(s) juridique(s) des GFP :",
                    choices = list(
                      "CC" = "CC",
                      "CA" = "CA",
                      "CU" = "CU",
                      "Métropoles" = "METRO",
                      "Métropole de Lyon" = "MET69",
                      "EPT" = "EPT"
                    ),
                    selected = list("CC", "CA", "CU", "METRO", "MET69", "EPT"),
                    options = list(`title` = "Vide"),
                    multiple = TRUE
                  )
                )
              ),
              column(
                3,
                h4("2 - Groupe de référence :"),
                tags$div(
                  htmlOutput("phrase_gr_ref_gfp") ,
                  
                  actionBttn(
                    "Clique_gr_ref_gfp",
                    "Affinez le groupe de référence",
                    style = "simple",
                    color = "primary",
                    size = "sm"
                  ),
                  hidden(
                    div(
                      id = "choix_gr_ref_gfp",
                      awesomeRadio(
                        "geo_gr_ref_gfp",
                        label = "",
                        choices =
                          list(
                            "France entière" = 3,
                            "France métropolitaine" = 2,
                            "France métropolitaine hors IDF" = 1
                          ),
                        selected = 2,
                        status = "primary",
                        inline = TRUE
                      ),
                      awesomeCheckboxGroup(
                        "nj_gr_ref_gfp",
                        label = "Nature juridique :",
                        choices = list(
                          "CC" = "CC",
                          "CA" = "CA",
                          "CU" = "CU",
                          "Métropoles" = "METRO",
                          "Mét. LYON" = "MET69",
                          "EPT" = "EPT"
                        ),
                        selected = list("CC", "CA", "CU", "METRO"),
                        status = "primary",
                        inline = TRUE
                      ),
                      style = "background:#fff"
                    )
                  )
                )
              ),
              # Troisieme colonne, permet d'isoler une commune dans les graphs
              column(
                h4("Option, en plus de l’échantillon, isolez un groupement (périmètre 2017)"),
                selectizeInput(
                  inputId = "mon_gfp",
                  label = "Saisissez les premières lettres :",
                  choices = NULL,
                  selected = NULL
                ),
                width = 3
              )
            ),
            fluidRow(
              actionBttn(
                "go_gfp",
                "Affichez les résultats !",
                color = "danger",
                style = "simple"
              ),
              style = "text-align:center;line-height:20px;vertical-align:middle"
            ),
            
            style = "background-color:#deebf7;"
          ),
          # fin de la premiere ligne
          
          hr(style = "height:5px"),
          
          # deuxieme ligne, info sur echantillon
          fluidRow(
            p(
              "Le périmètre des groupements à fiscalité propre pouvant évoluer très fortement d'une année sur l'autre, les graphiques suivants portent sur les données 2017 des groupements tels qu'ils existaient cette année-là."
            ),
            p(
              "Toutefois, les données individuelles détaillées, accessibles à la suite des graphiques, permettent également d'accéder aux données 2014, 2015 et 2016 pour les groupements à fiscalités propre existants à ces dates."
            ),
            p("Les données des comptes 2017 de quelques groupements et de certains budgets annexes sont encore manquantes. Ce qui explique l’absence de certaines entités dans les phases de sélection et dans les traitements réalisés."),
            h3("Informations 2017 sur l'échantillon sélectionné", style = "text-align:center"),
            box(htmlOutput("info_echan_box1_gfp"),
                style = "background-color:#2FA4E7;color:#fff;border: 2px #2FA4E7 ridge;font-size:1em;margin: 0px 1px 1px 0px;height:39px;text-align:center;line-height:37px;vertical-align:middle;font-size:1.3em"),
            box(htmlOutput("info_echan_box7_gfp"),
                style = "background-color:#2FA4E7;color:#fff;border: 2px #2FA4E7 ridge;font-size:1em;margin: 0px 1px 1px 0px;height:39px;text-align:center;line-height:37px;vertical-align:middle;font-size:1.3em"),
            box(htmlOutput("info_echan_box2_gfp"),
                style = "background-color:#2FA4E7;color:#fff;border: 2px #2FA4E7 ridge;font-size:1em;margin: 0px 1px 1px 0px;height:39px;text-align:center;line-height:37px;vertical-align:middle;font-size:1.3em"),
            box(htmlOutput("info_echan_box3_gfp"),
                style = "background-color:#2FA4E7;color:#fff;border: 2px #2FA4E7 ridge;font-size:1em;margin: 0px 1px 1px 0px;height:39px;text-align:center;line-height:37px;vertical-align:middle;font-size:1.3em"),
            box(htmlOutput("info_echan_box4_gfp"),
                style = "background-color:#2FA4E7;color:#fff;border: 2px #2FA4E7 ridge;font-size:1em;margin: 0px 1px 1px 0px;height:39px;text-align:center;line-height:37px;vertical-align:middle;font-size:1.3em"),
            box(htmlOutput("info_echan_box5_gfp"),
                style = "background-color:#2FA4E7;color:#fff;border: 2px #2FA4E7 ridge;font-size:1em;margin: 0px 1px 1px 0px;height:39px;text-align:center;line-height:37px;vertical-align:middle;font-size:1.3em")#,
            # box(
            #   textOutput("info_echan_box6_gfp"),
            #   style ="background-color:#2FA4E7;color:#fff;border: 2px #2FA4E7 ridge;font-size:1em;margin: 0px 1px 1px 0px;height:39px;text-align:center;line-height:37px;vertical-align:middle;font-size:1.3em")
          ),
          
          # fin de la premiere partie, selection entité
          
          
          hr(style = "height:5px;"),
          
          fluidRow(
            column(
              6,
              h3("Enjeux financiers des dépenses d'équipement", style =
                   "text-align:center;"),
              pickerInput(
                "bc_ou_gfp",
                label = "",
                choices = list("Bloc communal" = 1, "Groupement" = 0),
                selected = 0,
                options = list(style =
                                 "btn-primary", `title` = "Vide")
              ),
              htmlOutput("gfpssbc"),
              withSpinner(plotOutput("grap_dep_equip_gfp"))
            ),
            column(
              6,
              h3(
                "Statistiques des dépenses d'équipement de l'échantillon",
                style = "text-align:center;"
              ),
              tableOutput("stat_echan_moy_gfp"),
              withSpinner(htmlOutput("stat_echan_moy_gfp_sup_30"))
            )
          ),
          fluidRow(
            column(
              6,
              h3("Financement sur ressources propres ou par emprunts", style =
                   "text-align:center;"),
              pickerInput(
                "bp_ba_gfp1",
                label = "",
                choices = list(
                  "Budgets principaux (BP)" = 1,
                  "Budgets annexes (BA)" = 2,
                  "Budgets principaux et budgets annexes" = 3
                ),
                selected = 3,
                options = list(style =
                                 "btn-primary", `title` = "Vide")
              ),
              withSpinner(plotOutput("graph_finance_gfp")),
              htmlOutput("phrase_gfp")
            ),
            column(
              6,
              h3(
                "Poids des groupements dans les dépenses d'équipement : vision territoriale",
                style = "text-align:center;"
              ),
              htmlOutput("gfpssbc2"),
              withSpinner(plotOutput("graph_bc_gfp"))
            )
          ),
          fluidRow(
            h3("Pour aller plus loin...", style = "text-align:center;color:#31a354;"),
            column(
              6,
              h3("La structure des recettes d'investissement", style =
                   "text-align:center;color:#31a354;"),
              pickerInput(
                "bp_ba_gfp2",
                label = "",
                choices = list(
                  "Budgets principaux (BP)" = 1,
                  "Budgets annexes (BA)" = 2,
                  "Budgets principaux et budgets annexes" = 3
                ),
                selected = 3,
                options = list(style =
                                 "btn-primary", `title` = "Vide")
              ),
              withSpinner(plotOutput("graph_zoom_rec_invest_gfp"))
            ),
            column(
              6,
              h3("La structure des dépenses d'équipement", style =
                   "text-align:center;color:#31a354;"),
              pickerInput(
                "bp_ba_gfp3",
                label = "",
                choices = list(
                  "Budgets principaux (BP)" = 1,
                  "Budgets annexes (BA)" = 2,
                  "Budgets principaux et budgets annexes" = 3
                ),
                selected = 3,
                options = list(style =
                                 "btn-primary", `title` = "Vide")
              ),
              withSpinner(plotOutput("graph_zoom_dep_equip_gfp"))
            )
          ),
          fluidRow(
            h3("A chaque collectivité, une situation spécifique : data", style = "text-align:center;"),
            column(
              6,
              a(
                "Explication des variables",
                href = "Methodologie_Communes_GFP.pdf",
                target = "_blank"
              ),
              style = "margin-left:20px"
            ),
            column(
              6,
              awesomeCheckboxGroup(
                inputId = "annee_gfp_tel",
                label = p(
                  strong("Ajoutez des années "),
                  "pour l'export des données individuelles",
                  br(),
                  "(tous les groupements des départements de l'échantillon) :"
                ),
                choices = c("2016" = 2016,"2015" = 2015, "2014" = 2014),
                inline = TRUE,
                status = "primary"
              ),
              downloadButton("export_data_gfp", "Télécharger la base")
            )
          ),
          fluidRow(p(
            "Les valeurs sont exprimées en milliers d'euros"
          )),
          fluidRow(dataTableOutput("table_gfp"))
          
        )
      ),
      # fin tabPanel GFP
      
      
      
      
      
      
      
      
      
      #### DEPARTEMENT ====
      
      
      tabPanel(
        title = div("Départements", style = "color:#fff;font-size:1.3em;"),
        value = "dept",
        fluidPage(
          id = "page_dep",
          fluidRow(
            fluidRow(
              column(
                6,
                h4("1 - Sélectionnez votre échantillon de départements :", style =
                     " text-align:center"),
                column(
                  6,
                  pickerInput(
                    "reg_echan_deps",
                    label = "Choisissez une (des) région(s) :",
                    choices = unique(sort(list_dep_dep$nom_reg_accent)),
                    multiple = TRUE,
                    options = list(
                      `actions-box` = TRUE,
                      `deselect-all-text` = "Tout désélectionner",
                      `select-all-text` = "Tout sélectionner",
                      `title` = "Vide"
                    )
                  ),
                 
                  pickerInput(
                    "pop_dep",
                    label = "Choisissez une (des) strate(s) de population :",
                    choices = list(
                      "0 - 250 000 hab." = "(0-250000]",
                      "250 000 - 500 000 hab." = "(250000-500000]",
                      "500 000 - 750 000 hab." = "(500000-750000]",
                      "750 000 - 1 000 000 hab. " = "(750000-1000000]" ,
                      "1 000 000 hab. et plus" =  "1000000 et plus"
                    ),
                    selected = list(
                      "(0-250000]",
                      "(250000-500000]" ,
                      "(500000-750000]" ,
                      "(750000-1000000]",
                      "1000000 et plus"
                    ),
                    options =  list(
                      `actions-box` = TRUE,
                      `deselect-all-text` = "Tout désélectionner",
                      `select-all-text` = "Tout sélectionner",
                      `title` = "Vide"
                    ),
                    multiple = TRUE
                  ),
                  
                  pickerInput(
                    "dpu_dfm",
                    label = "Urbain / rural :",
                    choices = c("Urbain","Rural","DOM"),
                    multiple = TRUE,
                    selected = c("Urbain","Rural","DOM")
                    )
                  
                ),
                column(
                  6,
                  pickerInput(
                    inputId = "mes_dep_selec_deps",
                    label = "Affinez par département, si besoin :",
                    choices = NULL,
                    choicesOpt = NULL,
                    options = list(
                      `actions-box` = TRUE,
                      `deselect-all-text` = "Tout désélectionner",
                      `select-all-text` = "Tout sélectionner",
                      `title` = "Vide"
                    ),
                    multiple = TRUE
                  ),
                  pickerInput(
                    "cas_spe_deps",
                    label = "Ajoutez des collectivités spécifiques :",
                    choices = list(
                      "Métropole de Lyon" = "METRO LYON",
                      "Département Rhône" = "Rhone",
                      "CTU Martinique" = "Martinique",
                      "CTU Guyane" = "Guyane"
                    ),
                    options = list(`title` = "Vide"),
                    multiple = TRUE
                  )
                )
              ),
              column(3,
                     h4("2 - Groupe de référence :"),
                     tags$div(
                       p(
                         "Le groupe de référence est composé des départements de France métropolitaine hors Paris, Rhône et métropole de Lyon."
                       ),
                       htmlOutput("info_gr_dep")
                     )),
              # Troisieme colonne, permet d'isoler une commune dans les graphs
              column(
                h4("Option, en plus de l’échantillon, vous pouvez isoler un département "),
                selectizeInput(
                  inputId = "mon_dep",
                  label = "Saisissez les premières lettres :",
                  choices = NULL,
                  selected = NULL
                ),
                width = 3
              )
            ),
            fluidRow(
              actionBttn(
                "go_dep",
                "Affichez les résultats !",
                color = "danger",
                style = "simple"
              ),
              style = "text-align:center;line-height:20px;vertical-align:middle"
            ),
            
            style = "background-color:#deebf7;"
          ),
          
          fluidRow(
            h3("Informations 2017 sur l'échantillon sélectionné", style = "text-align:center"),
            box(htmlOutput("info_echan_box1_dep"),
                style = "background-color:#2FA4E7;color:#fff;border: 2px #2FA4E7 ridge;font-size:1em;margin: 0px 1px 1px 0px;height:39px;text-align:center;line-height:37px;vertical-align:middle;font-size:1.3em"),
            box(htmlOutput("info_echan_box7_dep"),
                style = "background-color:#2FA4E7;color:#fff;border: 2px #2FA4E7 ridge;font-size:1em;margin: 0px 1px 1px 0px;height:39px;text-align:center;line-height:37px;vertical-align:middle;font-size:1.3em"),
            box(htmlOutput("info_echan_box2_dep"),
                style = "background-color:#2FA4E7;color:#fff;border: 2px #2FA4E7 ridge;font-size:1em;margin: 0px 1px 1px 0px;height:39px;text-align:center;line-height:37px;vertical-align:middle;font-size:1.3em"),
            box(htmlOutput("info_echan_box3_dep"),
                style = "background-color:#2FA4E7;color:#fff;border: 2px #2FA4E7 ridge;font-size:1em;margin: 0px 1px 1px 0px;height:39px;text-align:center;line-height:37px;vertical-align:middle;font-size:1.3em"),
            box(htmlOutput("info_echan_box4_dep"),
                style = "background-color:#2FA4E7;color:#fff;border: 2px #2FA4E7 ridge;font-size:1em;margin: 0px 1px 1px 0px;height:39px;text-align:center;line-height:37px;vertical-align:middle;font-size:1.3em"),
            box(htmlOutput("info_echan_box5_dep"),
                style = "background-color:#2FA4E7;color:#fff;border: 2px #2FA4E7 ridge;font-size:1em;margin: 0px 1px 1px 0px;height:39px;text-align:center;line-height:37px;vertical-align:middle;font-size:1.3em")
            
          ),
          # fin de la deuxieme ligne pour info echantillon
          
          
          
          # Les graphiques
          
          fluidRow(
            column(
              6,
              h3("Enjeux financiers des dépenses d'investissement...", style =
                   "text-align:center;"),
              pickerInput(
                "bp_ba_dep",
                label = "",
                choices = list(
                  "Budgets principaux (BP)" = 1,
                  "Budgets annexes (BA)" = 2,
                  "Budgets principaux et budgets annexes" = 3
                ),
                selected = 1,
                options = list(style =
                                 "btn-primary", `title` = "Vide")
              ),
              withSpinner(plotOutput("graph_dep_invest_dep"))
            ),
            column(
              6,
              h3("... et l'illustration des disparités individuelles", style =
                   "text-align:center;"),
              pickerInput(
                "annee_dep",
                label = "",
                choices = list("2017" = 1,
                               "2012-2017" =
                                 2),
                options = list(style =
                                 "btn-primary", `title` = "Vide"),
                selected = 1
              ),
              withSpinner(plotlyOutput("graph_nuage_dep"))
            )
          ),
          fluidRow(
            column(
              6,
              h3("Financement sur ressources propres ou par emprunt", style =
                   "text-align:center;"),
              div(style = "height:25px"),
              withSpinner(plotOutput("graph_finance_dep")),
              htmlOutput("phrase_dep")
            ),
            column(
              6,
              tags$h3("Et sur plusieurs années ?", style = "text-align:center;"),
              fluidRow(div(column(6),
                           column(
                             6, div(
                               pickerInput(
                                 "var_select_dep",
                                 label = "",
                                 choices =  NULL,
                                 option = list(style =
                                                 "btn-success")
                               )
                             )
                           ),
                           style = "height:100px")),
              fluidRow(withSpinner(plotOutput("graph_evol_dep")))
            )
          ),
          fluidRow(
            h3("Pour aller plus loin...", style = "text-align:center;color:#31a354;"),
            column(
              6,
              h3("La structure des dépenses d'équipement", style =
                   "text-align:center;color:#31a354;"),
              column(
                6,
                pickerInput(
                  "annee_dep2",
                  label = "",
                  choices = list("2017" = 1,
                                 "2012-2017" =
                                   2),
                  options = list(style =
                                   "btn-primary"),
                  selected = 1
                )
              ),
              column(
                6,
                pickerInput(
                  "bp_ba_dep2",
                  label = "",
                  choices = list(
                    "Budgets principaux (BP)" = 1,
                    "Budgets annexes (BA)" = 2,
                    "Budgets principaux et budgets annexes" = 3
                  ),
                  selected = 1,
                  options = list(style =
                                   "btn-primary")
                )
              ),
              withSpinner(plotOutput("graph_zoom_dep_equip_dep"))
            ),
            column(
              6,
              h3("Le détail des subventions versées", style =
                   "text-align:center;color:#31a354;"),
              pickerInput(
                "annee_dep3",
                label = "",
                choices = list("2017" = 1,
                               "2012-2017" =
                                 2),
                options = list(style =
                                 "btn-primary"),
                selected = 1
              ),
              withSpinner(plotOutput("graph_sub_204_dep"))
            )
          ),
          
          fluidRow(
            h3("A chaque collectivité, une situation spécifique : data", style = "text-align:center;"),
            column(
              6,
              a(
                "Explication des variables",
                href = "Methodologie_departements.pdf",
                target = "_blank"
              ),
              style = "margin-left:20px"
            ),
            column(6,
                   downloadButton("export_data_dep", "Télécharger la base"))
          ),
          fluidRow(p(
            "Les valeurs sont exprimées en milliers d'euros"
          )),
          fluidRow(dataTableOutput("table_dep"))
          
        )
      ),
      
      
      
      
      #### Régions =====
      
      tabPanel(
        title = div("Régions", style = "color:#fff;font-size:1.3em;"),
        value = "reg",
        fluidPage(
          id = "page_reg",
          fluidRow(
            fluidRow(
              column(
                6,
                h4(
                  "1 - Sélectionnez votre échantillon de régions (yc. CTU et Corse) :",
                  style = " text-align:center"
                ),
                column(
                  12,
                  pickerInput(
                    "reg_echan_regs",
                    label = "Choisissez une (des) région(s) :",
                    choices = unique(sort(list_dep$nom_reg_accent)),
                    selected = unique(sort(list_dep$nom_reg_accent)),
                    multiple = TRUE,
                    options = list(
                      `actions-box` = TRUE,
                      `deselect-all-text` = "Tout désélectionner",
                      `select-all-text` = "Tout sélectionner",
                      `title` = "Vide"
                    )
                  )
                )
              ),
              column(3,
                     h4("2 - Groupe de référence :"),
                     tags$div(
                       p(
                         "Le groupe de référence est composé des régions de France métropolitaine hors collectivité territoriale de Corse."
                       )
                       
                     )),
              # Troisieme colonne, permet d'isoler une commune dans les graphs
              column(
                h4("Option, en plus de l’échantillon, vous pouvez isoler une région "),
                selectizeInput(
                  inputId = "mon_reg",
                  label = "",
                  selected = NULL,
                  choices = NULL
                  
                  # maxOptions = 1000,
                  #                                                                       label = unique(sort(list_dep_dep$nom_reg_accent)),
                  #                                                                       render = I(
                  #                                                                         '{
                  #                            option: function(item, escape) {
                  #                            return "<div><strong>" + escape(item.value) + "</strong></div>"
                  #                            }
                  # }'))
                ),
                width = 3
              )
            ),
            fluidRow(
              actionBttn(
                "go_reg",
                "Affichez les résultats !",
                color = "danger",
                style = "simple"
              ),
              style = "text-align:center;line-height:20px;vertical-align:middle"
            ),
            
            style = "background-color:#deebf7;"
          ),
          hr(),
          
          p(
            strong(
              "Les régions portent la responsabilité de la gestion d’une partie des fonds européens sur leur territoire.
              Les modalités d’enregistrement comptable de ces fonds peuvent être sources de disparités fortes dans les données de l’investissement de chaque région.
              Certaines peuvent notamment enregistrer une partie de ces fonds en budgets annexes.
              Ces derniers ne sont pas pris en compte dans les graphiques présentés."
            )
            ),
          
          fluidRow(
            h3("Informations 2017 sur l'échantillon sélectionné", style = "text-align:center"),
            
            box(htmlOutput("info_echan_box1_reg"),
                style = "background-color:#2FA4E7;color:#fff;border: 2px #2FA4E7 ridge;font-size:1em;margin: 0px 1px 1px 0px;height:39px;text-align:center;line-height:37px;vertical-align:middle;font-size:1.3em"),
            box(htmlOutput("info_echan_box7_reg"),
                style = "background-color:#2FA4E7;color:#fff;border: 2px #2FA4E7 ridge;font-size:1em;margin: 0px 1px 1px 0px;height:39px;text-align:center;line-height:37px;vertical-align:middle;font-size:1.3em"),
            
            box(htmlOutput("info_echan_box2_reg"),
                style = "background-color:#2FA4E7;color:#fff;border: 2px #2FA4E7 ridge;font-size:1em;margin: 0px 1px 1px 0px;height:39px;text-align:center;line-height:37px;vertical-align:middle;font-size:1.3em"),
            box(htmlOutput("info_echan_box3_reg"),
                style = "background-color:#2FA4E7;color:#fff;border: 2px #2FA4E7 ridge;font-size:1em;margin: 0px 1px 1px 0px;height:39px;text-align:center;line-height:37px;vertical-align:middle;font-size:1.3em"),
            box(htmlOutput("info_echan_box4_reg"),
                style = "background-color:#2FA4E7;color:#fff;border: 2px #2FA4E7 ridge;font-size:1em;margin: 0px 1px 1px 0px;height:39px;text-align:center;line-height:37px;vertical-align:middle;font-size:1.3em"),
            box(htmlOutput("info_echan_box5_reg"),
                style = "background-color:#2FA4E7;color:#fff;border: 2px #2FA4E7 ridge;font-size:1em;margin: 0px 1px 1px 0px;height:39px;text-align:center;line-height:37px;vertical-align:middle;font-size:1.3em")
            
          ),
          # fin de la deuxieme ligne pour info echantillon
          
          
          
          # Les graphiques
          
          fluidRow(
            column(
              6,
              h3("Enjeux financiers des dépenses d'investissement...", style =
                   "text-align:center;"),
              
              withSpinner(plotOutput("graph_dep_invest_reg"))
            ),
            column(
              6,
              h3("... et l'illustration des disparités individuelles", style =
                   "text-align:center;"),
              pickerInput(
                "annee_reg",
                label =
                  "",
                choices = list("2017" = 1,
                               "2012-2017" =
                                 2),
                options =
                  list(style = "btn-primary", `title` = "Vide"),
                selected =
                  1
              ),
              withSpinner(plotlyOutput("graph_nuage_reg"))
            )
          ),
          fluidRow(
            column(
              6,
              h3("Financement sur ressources propres ou par emprunt", style =
                   "text-align:center;"),
              div(style = "height:25px"),
              withSpinner(plotOutput("graph_finance_reg")),
              htmlOutput("phrase_reg")
            ),
            column(
              6,
              tags$h3("Et sur plusieurs années ?", style = "text-align:center;"),
              fluidRow(div(column(6),
                           column(
                             6, div(
                               pickerInput(
                                 "var_select_reg",
                                 label = "",
                                 choices =  NULL,
                                 option = list(style =
                                                 "btn-success")
                               )
                             )
                           ),
                           style = "height:100px")),
              fluidRow(withSpinner(plotOutput("graph_evol_reg")))
            )
          ),
          fluidRow(
            h3("Pour aller plus loin...", style = "text-align:center;color:#31a354;"),
            column(
              6,
              h3("Le détail des subventions versées", style =
                   "text-align:center;color:#31a354;"),
              pickerInput(
                "annee_reg3",
                label =
                  "",
                choices = list("2017" = 1,
                               "2012-2017" =
                                 2),
                options =
                  list(style = "btn-primary"),
                selected =
                  1
              ),
              withSpinner(plotOutput("graph_sub_204_reg"))
            ),
            column(
              6,
              tags$h3("Et sur plusieurs années ?", style = "text-align:center;color:#31a354;"),
              fluidRow(div(column(
                6, div(
                  pickerInput(
                    "var_select_reg_sub",
                    label = "",
                    choices =  NULL,
                    option = list(style =
                                    "btn-success")
                  )
                )
              ),
              style =
                "height:100px")),
              fluidRow(withSpinner(plotOutput(
                "graph_evol_reg_sub"
              )))
              
            )
          ),
          fluidRow(
            h3("A chaque collectivité, une situation spécifique : data", style = "text-align:center;"),
            column(
              6,
              a(
                "Explication des variables",
                href = "Methodologie_regions.pdf",
                target = "_blank"
              ),
              style = "margin-left:20px"
            ),
            column(6,
                   downloadButton("export_data_reg", "Télécharger la base"))
          ),
          fluidRow(p(
            "Les valeurs sont exprimées en milliers d'euros"
          )),
          fluidRow(dataTableOutput("table_reg"))
          
          
            )
          ),
      
      tabPanel(
        title = div("Méthodologie", style = "color:#fff;font-size:1.3em;"),
        
        fluidPage(div(includeHTML("www/methodo.html")))
        # column(3,h5("Les derniers Cap sur :"),br(),
        #              tags$ul(
        #                tags$li(a("Cap sur les subventions d’équipement versées entre collectivités locales - n°1, mars 2018 ",href ="https://www.collectivites-locales.gouv.fr/files/files/dgcl_v2/OFGL/ofgl-cap-sur.pdf",target = "_blank")),
        #                tags$li(a("Cap sur les allégements de fiscalité locale et leurs compensations – n°2, avril 2018 ",href ="https://www.collectivites-locales.gouv.fr/files/files/dgcl_v2/OFGL/ofgl_-_cap_sur_-_ndeg2_-_04-2018_-_allegements_fiscalite_locale_et_leurs_compensations.pdf",target = "_blank"))
        #
        #              ),style="height:200px;background-color:#9ecae1;position:fixed;right:5px"))
        
      ),
      
      tags$footer(
        fluidRow(column(
          6,
          h5("Nous suivre :", style = "color:#fff;"),
          img(src = "In-White-14px.png"),
          a(
            "Linkedin",
            href = "https://www.linkedin.com/company/ofgl",
            target = "_blank",
            style = "color:#fff"
          ),
          br(),
          a(
            "Site internet",
            href = "https://www.collectivites-locales.gouv.fr/ofgl",
            target = "_blank",
            style = "color:#fff"
          )
        ),
        column(
          6,
          h5("Nous contacter :", style = "color:#fff;"),
          p("contact@ofgl.fr", style = "color:#fff;")
        )),
        fluidRow(
          div("Observatoire des finances et de la gestion publique locales"),
          style = "text-align:center;"
        ),
        style = "text-align:center;background-color:#2FA4E7;color:#fff;height:100px"
      )
    ),
    
    
    tags$style(
      type = "text/css",
      ".shiny-output-error { visibility: hidden; }",
      ".shiny-output-error:before { visibility: hidden; }"
    )
  )
  ))
