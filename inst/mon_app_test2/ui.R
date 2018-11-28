library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # theme = shinytheme("cerulean"),
  h1("Test sur les départements sans JS et plotly"),
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
        # withSpinner(plotlyOutput("graph_nuage_dep")),
        htmlOutput("textgraphnuage")
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
    ))#,
    # fluidRow(dataTableOutput("table_dep"))
    
  )
)
)
)