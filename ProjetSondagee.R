# ⚙️ Bibliothèques
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(plotly)
library(readxl)
library(writexl)
library(DT)
library(waiter)
library(summarytools)
library(DescTools)
library(corrplot)
library(car)
library(nortest)
library(moments)
library(purrr)
library(tidyr)
library(scales)
library(viridis)
library(RColorBrewer)

# Configuration des options
options(DT.options = list(
  language = list(url = "//cdn.datatables.net/plug-ins/1.10.25/i18n/French.json"),
  pageLength = 10,
  dom = 'Bfrtip',
  buttons = c('copy', 'csv', 'excel')
))

# 🎨 UI
ui <- dashboardPage(
  dashboardHeader(
    title = span("TunSample Pro", style = "font-weight: bold; color: white;"),
    titleWidth = 300
  ),
  dashboardSidebar(
    width = 300,
    fileInput(
      "file_upload",
      "📥 Importer fichier Excel",
      accept = c(".xlsx", ".xls")
    ),
    sidebarMenu(
      menuItem("Accueil", tabName = "home", icon = icon("home")),
      menuItem(
        "Tableau de bord",
        tabName = "dashboard",
        icon = icon("dashboard")
      ),
      menuItem("SAS", tabName = "sas", icon = icon("random")),
      menuItem(
        "Stratifié",
        tabName = "stratified",
        icon = icon("layer-group")
      ),
      menuItem(
        "Comparaison",
        tabName = "comparison",
        icon = icon("balance-scale")
      )
    )
  ),
  dashboardBody(
    useShinyjs(),
    use_waiter(),
    tags$head(tags$style(
      HTML(
        "
        .box-title { font-weight: bold; }
        .small-box { margin-bottom: 15px; }
        .nav-tabs-custom .nav-tabs li.active {
          border-top-color: #3c8dbc;
        }
        .box.box-solid.box-primary > .box-header {
          background-color: #3c8dbc;
        }
        .box.box-solid.box-success > .box-header {
          background-color: #00a65a;
        }
        .box.box-solid.box-info > .box-header {
          background-color: #00c0ef;
        }
        .stat-box {
          padding: 10px;
          border-radius: 5px;
          margin-bottom: 15px;
          background-color: #f8f9fa;
          border-left: 4px solid #3c8dbc;
        }
        .stat-title {
          font-weight: bold;
          margin-bottom: 5px;
        }
        .stat-value {
          font-size: 18px;
          font-weight: bold;
        }
        .stat-subtitle {
          font-size: 12px;
          color: #6c757d;
        }
      "
      )
    )),
    tabItems(
      # Onglet Accueil
      tabItem(tabName = "home",
              fluidRow(
                box(
                  width = 12,
                  title = "Bienvenue dans TunSample Pro",
                  status = "primary",
                  solidHeader = TRUE,
                  h3("Application d'échantillonnage statistique", style = "font-weight: bold;"),
                  p(
                    "Cette application permet de générer des échantillons statistiques à partir d'un cadre tunisien.",
                    style = "font-size: 16px;"
                  ),
                  tags$ul(
                    tags$li("📁 Importez un fichier Excel contenant vos données", style = "font-size: 14px;"),
                    tags$li(
                      "🧮 Choisissez une méthode d'échantillonnage (SAS ou stratifié)",
                      style = "font-size: 14px;"
                    ),
                    tags$li(
                      "📊 Visualisez et analysez les résultats avec des graphiques interactifs",
                      style = "font-size: 14px;"
                    ),
                    tags$li(
                      "📈 Effectuez des tests statistiques avancés sur vos échantillons",
                      style = "font-size: 14px;"
                    ),
                    tags$li("💾 Téléchargez vos échantillons au format Excel", style = "font-size: 14px;")
                  ),
                  hr(),
                  h4("Fonctionnalités principales", style = "font-weight: bold;"),
                  fluidRow(column(
                    4,
                    div(
                      class = "stat-box",
                      div(class = "stat-title", "Échantillonnage Aléatoire Simple"),
                      p("Sélection aléatoire d'unités avec probabilités égales"),
                      actionButton("goto_sas", "Explorer SAS", class = "btn-primary btn-sm")
                    )
                  ),
                  column(
                    4,
                    div(
                      class = "stat-box",
                      div(class = "stat-title", "Échantillonnage Stratifié"),
                      p("Division en strates homogènes avec allocation proportionnelle"),
                      actionButton("goto_stratified", "Explorer Stratifié", class = "btn-success btn-sm")
                    )
                  ),
                  column(
                    4,
                    div(
                      class = "stat-box",
                      div(class = "stat-title", "Analyses Statistiques"),
                      p("Tests statistiques avancés et visualisations interactives"),
                      actionButton("goto_dashboard", "Voir Tableau de bord", class = "btn-info btn-sm")
                    )
                  ))
                )
              )),
      
      # Onglet Tableau de bord
      tabItem(
        tabName = "dashboard",
        fluidRow(
          valueBoxOutput("total_obs_box", width = 3),
          valueBoxOutput("num_vars_box", width = 3),
          valueBoxOutput("cat_vars_box", width = 3),
          valueBoxOutput("missing_values_box", width = 3)
        ),
        fluidRow(
          tabBox(
            title = "Aperçu et Analyse des Données",
            width = 12,
            tabPanel(
              "Aperçu des données",
              DTOutput("data_preview"),
              downloadButton("download_data", "Télécharger les données", class = "btn-primary")
            ),
            tabPanel(
              "Statistiques descriptives",
              selectInput("desc_var_select", "Sélectionner une variable", choices = NULL),
              verbatimTextOutput("desc_stats"),
              plotlyOutput("desc_plot", height = "400px")
            ),
            tabPanel("Distributions",
                     fluidRow(
                       column(
                         3,
                         selectInput("dist_var_select", "Variable", choices = NULL),
                         selectInput(
                           "dist_plot_type",
                           "Type de graphique",
                           choices = c(
                             "Histogramme" = "hist",
                             "Densité" = "density",
                             "Boîte à moustaches" = "boxplot",
                             "QQ-Plot" = "qqplot"
                           )
                         )
                       ),
                       column(9, plotlyOutput("dist_plot", height = "500px"))
                     )),
            tabPanel("Corrélations",
                     fluidRow(
                       column(
                         3,
                         checkboxGroupInput("corr_vars", "Variables à inclure", choices = NULL),
                         selectInput(
                           "corr_method",
                           "Méthode de corrélation",
                           choices = c(
                             "Pearson" = "pearson",
                             "Spearman" = "spearman",
                             "Kendall" = "kendall"
                           )
                         ),
                         checkboxInput("corr_show_values", "Afficher les valeurs", TRUE)
                       ),
                       column(9, plotOutput("corr_plot", height = "600px"))
                     ))
          )
        )
      ),
      
      # Onglet SAS (Échantillonnage Aléatoire Simple)
      tabItem(tabName = "sas",
              fluidRow(
                column(
                  3,
                  box(
                    title = "Paramètres d'échantillonnage",
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    numericInput("sas_n", "Taille de l'échantillon", 30, min = 1),
                    sliderInput("sas_n_slider", "Ajuster la taille", 1, 100, 30, step = 1),
                    selectInput("sas_var", "Variable comparative", choices = NULL),
                    selectInput("sas_graph_var", "Variable pour graphique", choices = NULL),
                    selectInput(
                      "sas_graph_type",
                      "Type de graphique",
                      choices = c(
                        "Barres" = "bar",
                        "Circulaire" = "pie",
                        "Boîte à moustaches" = "box",
                        "Histogramme" = "hist",
                        "Densité" = "density"
                      )
                    ),
                    actionButton("sas_run", "🎲 Lancer l'échantillonnage", class = "btn-primary btn-block"),
                    br(),
                    downloadButton("download_sas", "💾 Télécharger l'échantillon", class = "btn-default btn-block")
                  ),
                  conditionalPanel(
                    condition = "input.sas_run > 0",
                    box(
                      title = "Statistiques de l'échantillon",
                      width = NULL,
                      status = "info",
                      solidHeader = TRUE,
                      uiOutput("sas_stats_summary")
                    )
                  )
                ),
                column(
                  9,
                  conditionalPanel(condition = "input.sas_run == 0",
                                   box(
                                     width = NULL,
                                     background = "light-blue",
                                     div(
                                       style = "text-align: center; padding: 20px;",
                                       icon("random", "fa-4x"),
                                       h3("Échantillonnage Aléatoire Simple"),
                                       p(
                                         "Configurez les paramètres et cliquez sur 'Lancer l'échantillonnage' pour générer un échantillon aléatoire simple."
                                       ),
                                       actionButton("sas_run_intro", "Lancer l'échantillonnage", class = "btn-primary")
                                     )
                                   )),
                  conditionalPanel(
                    condition = "input.sas_run > 0",
                    tabBox(
                      width = NULL,
                      tabPanel("Échantillon",
                               DTOutput("sas_table")),
                      tabPanel("Graphiques",
                               fluidRow(
                                 column(6, plotlyOutput("sas_compare_plot", height = "400px")),
                                 column(6, plotlyOutput("sas_custom_graph", height = "400px"))
                               )),
                      tabPanel(
                        "Statistiques descriptives",
                        fluidRow(column(
                          12, verbatimTextOutput("sas_desc_stats")
                        )),
                        fluidRow(column(
                          6, plotlyOutput("sas_dist_plot", height = "350px")
                        ),
                        column(
                          6, plotlyOutput("sas_boxplot", height = "350px")
                        ))
                      ),
                      tabPanel(
                        "Tests statistiques",
                        fluidRow(column(
                          4,
                          selectInput(
                            "sas_test_type",
                            "Type de test",
                            choices = c(
                              "Test de Student (t-test)" = "ttest",
                              "Test de normalité" = "normality",
                              "Test du Chi-2" = "chisq",
                              "ANOVA" = "anova"
                            )
                          )
                        ),
                        column(8, uiOutput("sas_test_params"))),
                        hr(),
                        verbatimTextOutput("sas_test_result"),
                        plotlyOutput("sas_test_plot", height = "350px")
                      ),
                      tabPanel(
                        "Intervalles de confiance",
                        fluidRow(
                          column(
                            4,
                            selectInput("sas_ic_var", "Variable", choices = NULL),
                            sliderInput("sas_ic_level", "Niveau de confiance", 0.8, 0.99, 0.95, step = 0.01)
                          ),
                          column(8, plotlyOutput("sas_ic_plot", height = "350px"))
                        ),
                        verbatimTextOutput("sas_ic_result")
                      )
                    )
                  )
                )
              )),
      
      # Onglet Stratifié - MODIFIÉ
      tabItem(tabName = "stratified",
              fluidRow(
                column(
                  3,
                  box(
                    title = "Paramètres de stratification",
                    width = NULL,
                    status = "success",
                    solidHeader = TRUE,
                    numericInput("strat_n", "Taille totale de l'échantillon", 50, min = 1),
                    sliderInput("strat_n_slider", "Ajuster la taille", 1, 100, 50, step = 1),
                    selectInput("strat_var", "Variable de stratification", choices = NULL),
                    selectInput("strat_aux_var", "Variable auxiliaire", choices = NULL),
                    # Suppression du radioButtons pour les méthodes d'allocation
                    helpText("Méthode d'allocation: Proportionnelle"),
                    actionButton("strat_run", "🎲 Lancer l'échantillonnage", class = "btn-success btn-block"),
                    br(),
                    downloadButton("download_strat", "💾 Télécharger l'échantillon", class = "btn-default btn-block")
                  ),
                  conditionalPanel(
                    condition = "input.strat_run > 0",
                    box(
                      title = "Statistiques par strate",
                      width = NULL,
                      status = "info",
                      solidHeader = TRUE,
                      uiOutput("strat_stats_summary")
                    )
                  )
                ),
                column(
                  9,
                  conditionalPanel(condition = "input.strat_run == 0",
                                   box(
                                     width = NULL,
                                     background = "green",
                                     div(
                                       style = "text-align: center; padding: 20px;",
                                       icon("layer-group", "fa-4x"),
                                       h3("Échantillonnage Stratifié"),
                                       p(
                                         "Configurez les paramètres et cliquez sur 'Lancer l'échantillonnage' pour générer un échantillon stratifié."
                                       ),
                                       actionButton("strat_run_intro", "Lancer l'échantillonnage", class = "btn-success")
                                     )
                                   )),
                  conditionalPanel(
                    condition = "input.strat_run > 0",
                    tabBox(
                      width = NULL,
                      tabPanel(
                        "Allocations",
                        plotlyOutput("strat_alloc_plot", height = "300px"),
                        DTOutput("strat_alloc")
                      ),
                      # Nouvel onglet pour les allocations détaillées
                      tabPanel(
                        "Allocations détaillées",
                        plotlyOutput("strat_alloc_detailed_plot", height = "300px"),
                        DTOutput("strat_alloc_detailed")
                      ),
                      tabPanel("Échantillon",
                               DTOutput("strat_sample")),
                      tabPanel(
                        "Graphiques",
                        fluidRow(
                          column(
                            4,
                            selectInput("strat_graph_var", "Variable", choices = NULL),
                            selectInput(
                              "strat_graph_type",
                              "Type de graphique",
                              choices = c(
                                "Barres" = "bar",
                                "Circulaire" = "pie",
                                "Boîte à moustaches" = "box",
                                "Histogramme" = "hist",
                                "Densité" = "density"
                              )
                            )
                          ),
                          column(8, plotlyOutput("strat_graph", height = "400px"))
                        ),
                        hr(),
                        plotlyOutput("strat_compare_plot", height = "400px")
                      ),
                      tabPanel(
                        "Statistiques descriptives",
                        selectInput("strat_desc_var", "Variable", choices = NULL),
                        verbatimTextOutput("strat_desc_stats"),
                        fluidRow(column(
                          6, plotlyOutput("strat_dist_plot", height = "350px")
                        ),
                        column(
                          6, plotlyOutput("strat_boxplot_by_strata", height = "350px")
                        ))
                      ),
                      tabPanel(
                        "Tests statistiques",
                        fluidRow(column(
                          4,
                          selectInput(
                            "strat_test_type",
                            "Type de test",
                            choices = c(
                              "Test de Student (t-test)" = "ttest",
                              "Test de normalité" = "normality",
                              "Test du Chi-2" = "chisq",
                              "ANOVA" = "anova"
                            )
                          )
                        ),
                        column(8, uiOutput(
                          "strat_test_params"
                        ))),
                        hr(),
                        verbatimTextOutput("strat_test_result"),
                        plotlyOutput("strat_test_plot", height = "350px")
                      )
                    )
                  )
                )
              )),
      
      # Onglet Comparaison
      tabItem(tabName = "comparison",
              fluidRow(
                box(
                  title = "Comparaison des méthodes d'échantillonnage",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  conditionalPanel(
                    condition = "input.sas_run == 0 || input.strat_run == 0",
                    div(
                      style = "text-align: center; padding: 20px;",
                      icon("exclamation-triangle", "fa-4x"),
                      h3("Échantillons manquants"),
                      p(
                        "Veuillez générer les deux types d'échantillons (SAS et Stratifié) pour pouvoir les comparer."
                      ),
                      fluidRow(column(
                        6,
                        actionButton("goto_sas_from_comp", "Générer échantillon SAS", class = "btn-primary btn-block")
                      ),
                      column(
                        6,
                        actionButton(
                          "goto_strat_from_comp",
                          "Générer échantillon Stratifié",
                          class = "btn-success btn-block"
                        )
                      ))
                    )
                  ),
                  conditionalPanel(
                    condition = "input.sas_run > 0 && input.strat_run > 0",
                    tabBox(
                      width = 12,
                      tabPanel(
                        "Statistiques comparatives",
                        fluidRow(column(
                          4, selectInput("comp_var", "Variable à comparer", choices = NULL)
                        ),
                        column(
                          8, plotlyOutput("comp_boxplot", height = "350px")
                        )),
                        hr(),
                        fluidRow(
                          column(
                            6,
                            h4("Statistiques SAS", style = "text-align: center;"),
                            verbatimTextOutput("comp_sas_stats")
                          ),
                          column(
                            6,
                            h4("Statistiques Stratifié", style = "text-align: center;"),
                            verbatimTextOutput("comp_strat_stats")
                          )
                        )
                      ),
                      tabPanel(
                        "Précision des estimations",
                        fluidRow(
                          column(
                            4,
                            selectInput("comp_precision_var", "Variable", choices = NULL),
                            sliderInput(
                              "comp_precision_level",
                              "Niveau de confiance",
                              0.8,
                              0.99,
                              0.95,
                              step = 0.01
                            )
                          ),
                          column(8, plotlyOutput("comp_precision_plot", height = "400px"))
                        ),
                        verbatimTextOutput("comp_precision_stats")
                      ),
                      tabPanel(
                        "Représentativité",
                        fluidRow(
                          column(
                            4,
                            selectInput("comp_rep_var", "Variable catégorielle", choices = NULL),
                            checkboxInput("comp_rep_normalize", "Normaliser les fréquences", TRUE)
                          ),
                          column(8, plotlyOutput("comp_rep_plot", height = "400px"))
                        ),
                        verbatimTextOutput("comp_rep_chisq")
                      ),
                      tabPanel(
                        "Tests de différence",
                        fluidRow(
                          column(
                            4,
                            selectInput("comp_test_var", "Variable", choices = NULL),
                            selectInput(
                              "comp_test_type",
                              "Type de test",
                              choices = c(
                                "Test de Student (t-test)" = "ttest",
                                "Test de Mann-Whitney" = "wilcox",
                                "Test F (variances)" = "ftest"
                              )
                            )
                          ),
                          column(8, plotlyOutput("comp_test_plot", height = "350px"))
                        ),
                        verbatimTextOutput("comp_test_result")
                      )
                    )
                  )
                )
              ))
    )
  )
)

# 🧠 Serveur
server <- function(input, output, session) {
  # Valeurs réactives
  rv <- reactiveValues(
    data = NULL,
    sas = NULL,
    strat = NULL,
    strat_alloc = NULL,
    strat_alloc_detailed = NULL,
    # Nouvelle valeur réactive pour l'allocation détaillée
    numeric_vars = NULL,
    categorical_vars = NULL
  )
  
  # Waiter pour les chargements
  w <- Waiter$new(html = spin_fading_circles(),
                  color = "rgba(0, 0, 0, 0.7)")
  
  # Navigation entre onglets
  observeEvent(input$goto_sas, {
    updateTabItems(session, "sidebarMenu", "sas")
  })
  
  observeEvent(input$goto_stratified, {
    updateTabItems(session, "sidebarMenu", "stratified")
  })
  
  observeEvent(input$goto_dashboard, {
    updateTabItems(session, "sidebarMenu", "dashboard")
  })
  
  observeEvent(input$goto_sas_from_comp, {
    updateTabItems(session, "sidebarMenu", "sas")
  })
  
  observeEvent(input$goto_strat_from_comp, {
    updateTabItems(session, "sidebarMenu", "stratified")
  })
  
  observeEvent(input$sas_run_intro, {
    updateTabItems(session, "sidebarMenu", "sas")
    click("sas_run")
  })
  
  observeEvent(input$strat_run_intro, {
    updateTabItems(session, "sidebarMenu", "stratified")
    click("strat_run")
  })
  
  # Synchroniser les sliders avec les inputs numériques
  observeEvent(input$sas_n_slider, {
    updateNumericInput(session, "sas_n", value = input$sas_n_slider)
  })
  
  observeEvent(input$sas_n, {
    updateSliderInput(session, "sas_n_slider", value = input$sas_n)
  })
  
  observeEvent(input$strat_n_slider, {
    updateNumericInput(session, "strat_n", value = input$strat_n_slider)
  })
  
  observeEvent(input$strat_n, {
    updateSliderInput(session, "strat_n_slider", value = input$strat_n)
  })
  
  # Chargement du fichier
  observeEvent(input$file_upload, {
    w$show()
    
    tryCatch({
      df <- read_excel(input$file_upload$datapath)
      rv$data <- df
      
      # Identifier les variables numériques et catégorielles
      numeric_vars <- names(df)[sapply(df, is.numeric)]
      categorical_vars <- setdiff(names(df), numeric_vars)
      
      rv$numeric_vars <- numeric_vars
      rv$categorical_vars <- categorical_vars
      
      # Mettre à jour les sélecteurs
      updateSelectInput(session, "sas_var", choices = categorical_vars)
      updateSelectInput(session, "sas_graph_var", choices = names(df))
      updateSelectInput(session, "strat_var", choices = categorical_vars)
      updateSelectInput(session, "strat_aux_var", choices = names(df))
      updateSelectInput(session, "desc_var_select", choices = names(df))
      updateSelectInput(session, "dist_var_select", choices = numeric_vars)
      updateCheckboxGroupInput(session,
                               "corr_vars",
                               choices = numeric_vars,
                               selected = numeric_vars[1:min(5, length(numeric_vars))])
      
      # Ajuster les tailles max des échantillons
      updateSliderInput(session, "sas_n_slider", max = nrow(df))
      updateSliderInput(session, "strat_n_slider", max = nrow(df))
      updateNumericInput(session, "sas_n", max = nrow(df))
      updateNumericInput(session, "strat_n", max = nrow(df))
      
      # Notification de succès
      showNotification("Fichier chargé avec succès!", type = "message")
    }, error = function(e) {
      showNotification(paste("Erreur lors du chargement du fichier:", e$message),
                       type = "error")
    })
    
    w$hide()
  })
  
  # Tableau de bord - Aperçu des données
  output$data_preview <- renderDT({
    req(rv$data)
    datatable(rv$data, options = list(scrollX = TRUE))
  })
  
  # Tableau de bord - Value boxes
  output$total_obs_box <- renderValueBox({
    req(rv$data)
    valueBox(
      nrow(rv$data),
      "Observations",
      icon = icon("database"),
      color = "blue"
    )
  })
  
  output$num_vars_box <- renderValueBox({
    req(rv$numeric_vars)
    valueBox(
      length(rv$numeric_vars),
      "Variables numériques",
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  output$cat_vars_box <- renderValueBox({
    req(rv$categorical_vars)
    valueBox(
      length(rv$categorical_vars),
      "Variables catégorielles",
      icon = icon("list"),
      color = "purple"
    )
  })
  
  output$missing_values_box <- renderValueBox({
    req(rv$data)
    missing_count <- sum(is.na(rv$data))
    valueBox(
      missing_count,
      "Valeurs manquantes",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  # Tableau de bord - Statistiques descriptives
  output$desc_stats <- renderPrint({
    req(rv$data, input$desc_var_select)
    var_data <- rv$data[[input$desc_var_select]]
    
    if (is.numeric(var_data)) {
      cat("Statistiques pour la variable numérique:",
          input$desc_var_select,
          "\n\n")
      summary_stats <- summary(var_data)
      cat("Minimum:", summary_stats[1], "\n")
      cat("1er Quartile:", summary_stats[2], "\n")
      cat("Médiane:", summary_stats[3], "\n")
      cat("Moyenne:", mean(var_data, na.rm = TRUE), "\n")
      cat("3ème Quartile:", summary_stats[5], "\n")
      cat("Maximum:", summary_stats[6], "\n")
      cat("Écart-type:", sd(var_data, na.rm = TRUE), "\n")
      cat("Variance:", var(var_data, na.rm = TRUE), "\n")
      cat(
        "Coefficient de variation:",
        sd(var_data, na.rm = TRUE) / mean(var_data, na.rm = TRUE) * 100,
        "%\n"
      )
      cat("Asymétrie (Skewness):",
          skewness(var_data, na.rm = TRUE),
          "\n")
      cat("Aplatissement (Kurtosis):",
          kurtosis(var_data, na.rm = TRUE),
          "\n")
      cat("Valeurs manquantes:", sum(is.na(var_data)), "\n")
    } else {
      cat("Statistiques pour la variable catégorielle:",
          input$desc_var_select,
          "\n\n")
      freq_table <- table(var_data)
      prop_table <- prop.table(freq_table) * 100
      result <- data.frame(
        Catégorie = names(freq_table),
        Fréquence = as.vector(freq_table),
        Pourcentage = as.vector(prop_table)
      )
      print(result)
      cat("\nNombre de catégories:", length(freq_table), "\n")
      cat("Mode (catégorie la plus fréquente):",
          names(freq_table)[which.max(freq_table)],
          "\n")
      cat("Valeurs manquantes:", sum(is.na(var_data)), "\n")
    }
  })
  
  # Tableau de bord - Graphique descriptif
  output$desc_plot <- renderPlotly({
    req(rv$data, input$desc_var_select)
    var_data <- rv$data[[input$desc_var_select]]
    
    if (is.numeric(var_data)) {
      # Pour les variables numériques
      p <- plot_ly() %>%
        add_histogram(
          x = var_data,
          name = "Histogramme",
          marker = list(color = "#3c8dbc")
        ) %>%
        add_trace(
          x = var_data,
          type = "box",
          name = "Boîte à moustaches",
          marker = list(color = "#00a65a")
        ) %>%
        layout(
          title = paste("Distribution de", input$desc_var_select),
          xaxis = list(title = input$desc_var_select),
          yaxis = list(title = "Fréquence"),
          barmode = "overlay"
        )
    } else {
      # Pour les variables catégorielles
      freq_table <- table(var_data)
      df <- data.frame(Catégorie = names(freq_table),
                       Fréquence = as.vector(freq_table))
      
      p <-
        plot_ly(
          df,
          x = ~ Catégorie,
          y = ~ Fréquence,
          type = "bar",
          marker = list(color = "#3c8dbc")
        ) %>%
        layout(
          title = paste("Distribution de", input$desc_var_select),
          xaxis = list(title = ""),
          yaxis = list(title = "Fréquence")
        )
    }
    
    return(p)
  })
  # Tableau de bord - Graphique de distribution
  output$dist_plot <- renderPlotly({
    req(rv$data, input$dist_var_select, input$dist_plot_type)
    var_data <- rv$data[[input$dist_var_select]]
    
    if (!is.numeric(var_data)) {
      return(NULL)
    }
    
    if (input$dist_plot_type == "hist") {
      # Histogramme
      p <-
        plot_ly(
          x = var_data,
          type = "histogram",
          marker = list(color = "#3c8dbc")
        ) %>%
        layout(
          title = paste("Histogramme de", input$dist_var_select),
          xaxis = list(title = input$dist_var_select),
          yaxis = list(title = "Fréquence")
        )
    } else if (input$dist_plot_type == "density") {
      # Courbe de densité
      density_data <- density(var_data, na.rm = TRUE)
      df <- data.frame(x = density_data$x, y = density_data$y)
      
      p <-
        plot_ly(
          df,
          x = ~ x,
          y = ~ y,
          type = "scatter",
          mode = "lines",
          fill = "tozeroy",
          line = list(color = "#3c8dbc"),
          fillcolor = "rgba(60, 141, 188, 0.3)"
        ) %>%
        layout(
          title = paste("Densité de", input$dist_var_select),
          xaxis = list(title = input$dist_var_select),
          yaxis = list(title = "Densité")
        )
    } else if (input$dist_plot_type == "boxplot") {
      # Boîte à moustaches
      p <-
        plot_ly(
          y = var_data,
          type = "box",
          marker = list(color = "#3c8dbc")
        ) %>%
        layout(
          title = paste("Boîte à moustaches de", input$dist_var_select),
          yaxis = list(title = input$dist_var_select)
        )
    } else if (input$dist_plot_type == "qqplot") {
      # QQ-Plot
      # Supprimer les NA avant de traiter les données
      var_data_clean <- var_data[!is.na(var_data)]
      
      # Créer les données QQ-plot sans les tracer
      qqnorm_data <- qqnorm(var_data_clean, plot.it = FALSE)
      df <- data.frame(x = qqnorm_data$x, y = qqnorm_data$y)
      
      # Calculer manuellement les points de la ligne QQ
      y_sorted <-
        sort(var_data_clean, na.last = NA)  # Utiliser na.last au lieu de na.rm
      n <- length(y_sorted)
      
      if (n >= 4) {
        # Vérifier qu'il y a assez de points pour calculer les quartiles
        q1 <- y_sorted[floor(0.25 * n)]
        q3 <- y_sorted[ceiling(0.75 * n)]
        x1 <- qnorm(0.25)
        x3 <- qnorm(0.75)
        slope <- (q3 - q1) / (x3 - x1)
        intercept <- q1 - slope * x1
        
        # Créer le graphique avec plotly
        p <- plot_ly() %>%
          add_trace(
            data = df,
            x = ~ x,
            y = ~ y,
            type = "scatter",
            mode = "markers",
            marker = list(color = "#3c8dbc"),
            name = "Points"
          )
        
        # Ajouter la ligne manuellement
        x_range <- range(df$x)
        y_range <- intercept + slope * x_range
        
        p <- p %>% add_trace(
          x = x_range,
          y = y_range,
          type = "scatter",
          mode = "lines",
          line = list(color = "red"),
          name = "Ligne théorique"
        ) %>%
          layout(
            title = paste("QQ-Plot de", input$dist_var_select),
            xaxis = list(title = "Quantiles théoriques"),
            yaxis = list(title = "Quantiles observés")
          )
      } else {
        # Pas assez de données pour calculer les quartiles
        p <- plot_ly() %>%
          add_trace(
            data = df,
            x = ~ x,
            y = ~ y,
            type = "scatter",
            mode = "markers",
            marker = list(color = "#3c8dbc"),
            name = "Points"
          ) %>%
          layout(
            title = paste(
              "QQ-Plot de",
              input$dist_var_select,
              "(données insuffisantes pour la ligne théorique)"
            ),
            xaxis = list(title = "Quantiles théoriques"),
            yaxis = list(title = "Quantiles observés")
          )
      }
    }
    
    return(p)
  })
  # Tableau de bord - Matrice de corrélation
  output$corr_plot <- renderPlot({
    req(rv$data, input$corr_vars)
    
    if (length(input$corr_vars) < 2) {
      return(NULL)
    }
    
    # Extraire les variables sélectionnées
    data_subset <- rv$data[, input$corr_vars, drop = FALSE]
    
    # Calculer la matrice de corrélation
    corr_matrix <-
      cor(data_subset,
          use = "pairwise.complete.obs",
          method = input$corr_method)
    
    # Créer le graphique de corrélation
    corrplot(
      corr_matrix,
      method = "color",
      type = "upper",
      order = "hclust",
      addCoef.col = ifelse(input$corr_show_values, "black", "none"),
      tl.col = "black",
      tl.srt = 45,
      diag = FALSE,
      col = colorRampPalette(c("#6D9EC1", "white", "#E46726"))(200),
      title = paste("Matrice de corrélation (", input$corr_method, ")"),
      mar = c(0, 0, 2, 0)
    )
  })
  
  # Téléchargement des données
  output$download_data <- downloadHandler(
    filename = function() {
      paste("donnees_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(rv$data, path = file)
    }
  )
  
  # SAS - Génération de l'échantillon
  observeEvent(input$sas_run, {
    req(rv$data)
    w$show()
    
    # Échantillonnage aléatoire simple
    set.seed(as.numeric(Sys.time()))
    sample_size <- min(input$sas_n, nrow(rv$data))
    rv$sas <- rv$data[sample(nrow(rv$data), sample_size),]
    
    # Mettre à jour les sélecteurs pour les tests
    updateSelectInput(session, "sas_ic_var", choices = rv$numeric_vars)
    
    w$hide()
  })
  
  # SAS - Affichage de l'échantillon
  output$sas_table <- renderDT({
    req(rv$sas)
    datatable(rv$sas, options = list(scrollX = TRUE))
  })
  
  # SAS - Résumé des statistiques
  output$sas_stats_summary <- renderUI({
    req(rv$sas, rv$data)
    
    tagList(
      div(class = "stat-value", nrow(rv$sas)),
      div(class = "stat-subtitle", "observations"),
      hr(),
      div(class = "stat-title", "Taux d'échantillonnage"),
      div(class = "stat-value", paste0(round(
        nrow(rv$sas) / nrow(rv$data) * 100, 2
      ), "%")),
      hr(),
      div(class = "stat-title", "Variables"),
      div(class = "stat-value", ncol(rv$sas)),
      hr(),
      div(class = "stat-title", "Méthode"),
      div(class = "stat-subtitle", "Échantillonnage aléatoire simple sans remise")
    )
  })
  
  # SAS - Graphique de comparaison
  output$sas_compare_plot <- renderPlotly({
    req(rv$data, rv$sas, input$sas_var)
    
    # Compter les fréquences dans les deux ensembles
    data_counts <- table(rv$data[[input$sas_var]])
    sample_counts <- table(rv$sas[[input$sas_var]])
    
    # Créer un dataframe pour plotly
    categories <-
      unique(c(names(data_counts), names(sample_counts)))
    df <- data.frame(
      Catégorie = rep(categories, 2),
      Fréquence = c(
        sapply(categories, function(cat)
          ifelse(cat %in% names(data_counts), data_counts[cat], 0)),
        sapply(categories, function(cat)
          ifelse(cat %in% names(sample_counts), sample_counts[cat], 0))
      ),
      Type = rep(c("Cadre", "Échantillon"), each = length(categories))
    )
    
    # Créer le graphique
    p <-
      plot_ly(
        df,
        x = ~ Catégorie,
        y = ~ Fréquence,
        color = ~ Type,
        type = "bar",
        colors = c(
          "Cadre" = "#3c8dbc",
          "Échantillon" = "#f56954"
        )
      ) %>%
      layout(
        title = paste("Comparaison des distributions de", input$sas_var),
        xaxis = list(title = ""),
        yaxis = list(title = "Fréquence"),
        barmode = "group",
        legend = list(orientation = "h", y = 1.1)
      )
    
    return(p)
  })
  
  # SAS - Graphique personnalisé
  output$sas_custom_graph <- renderPlotly({
    req(rv$sas, input$sas_graph_var, input$sas_graph_type)
    
    var_data <- rv$sas[[input$sas_graph_var]]
    
    if (input$sas_graph_type == "bar") {
      # Diagramme à barres (pour variables catégorielles)
      if (is.numeric(var_data)) {
        # Pour les variables numériques, créer des classes
        breaks <- pretty(range(var_data, na.rm = TRUE), n = 10)
        var_data <-
          cut(var_data,
              breaks = breaks,
              include.lowest = TRUE)
      }
      
      freq_table <- table(var_data)
      df <- data.frame(Catégorie = names(freq_table),
                       Fréquence = as.vector(freq_table))
      
      p <-
        plot_ly(
          df,
          x = ~ Catégorie,
          y = ~ Fréquence,
          type = "bar",
          marker = list(color = "#00a65a")
        ) %>%
        layout(
          title = paste("Distribution de", input$sas_graph_var),
          xaxis = list(title = ""),
          yaxis = list(title = "Fréquence")
        )
    } else if (input$sas_graph_type == "pie") {
      # Diagramme circulaire
      if (is.numeric(var_data)) {
        # Pour les variables numériques, créer des classes
        breaks <- pretty(range(var_data, na.rm = TRUE), n = 10)
        var_data <-
          cut(var_data,
              breaks = breaks,
              include.lowest = TRUE)
      }
      
      freq_table <- table(var_data)
      df <- data.frame(Catégorie = names(freq_table),
                       Fréquence = as.vector(freq_table))
      
      p <-
        plot_ly(
          df,
          labels = ~ Catégorie,
          values = ~ Fréquence,
          type = "pie",
          marker = list(colors = colorRampPalette(brewer.pal(9, "Set3"))(length(freq_table)))
        ) %>%
        layout(
          title = paste("Distribution de", input$sas_graph_var),
          showlegend = TRUE
        )
    } else if (input$sas_graph_type == "box") {
      # Boîte à moustaches
      if (!is.numeric(var_data)) {
        return(NULL)
      }
      
      p <-
        plot_ly(
          y = var_data,
          type = "box",
          marker = list(color = "#00a65a")
        ) %>%
        layout(
          title = paste("Boîte à moustaches de", input$sas_graph_var),
          yaxis = list(title = input$sas_graph_var)
        )
    } else if (input$sas_graph_type == "hist") {
      # Histogramme
      if (!is.numeric(var_data)) {
        return(NULL)
      }
      
      p <-
        plot_ly(
          x = var_data,
          type = "histogram",
          marker = list(color = "#00a65a")
        ) %>%
        layout(
          title = paste("Histogramme de", input$sas_graph_var),
          xaxis = list(title = input$sas_graph_var),
          yaxis = list(title = "Fréquence")
        )
    } else if (input$sas_graph_type == "density") {
      # Courbe de densité
      if (!is.numeric(var_data)) {
        return(NULL)
      }
      
      density_data <- density(var_data, na.rm = TRUE)
      df <- data.frame(x = density_data$x, y = density_data$y)
      
      p <-
        plot_ly(
          df,
          x = ~ x,
          y = ~ y,
          type = "scatter",
          mode = "lines",
          fill = "tozeroy",
          line = list(color = "#00a65a"),
          fillcolor = "rgba(0, 166, 90, 0.3)"
        ) %>%
        layout(
          title = paste("Densité de", input$sas_graph_var),
          xaxis = list(title = input$sas_graph_var),
          yaxis = list(title = "Densité")
        )
    }
    
    return(p)
  })
  
  # SAS - Statistiques descriptives
  output$sas_desc_stats <- renderPrint({
    req(rv$sas)
    
    cat("STATISTIQUES DESCRIPTIVES DE L'ÉCHANTILLON SAS\n")
    cat("=============================================\n\n")
    
    # Statistiques générales
    cat("Taille de l'échantillon:", nrow(rv$sas), "\n")
    cat("Taux d'échantillonnage:",
        round(nrow(rv$sas) / nrow(rv$data) * 100, 2),
        "%\n\n")
    
    # Statistiques pour chaque variable numérique
    cat("VARIABLES NUMÉRIQUES\n")
    cat("-------------------\n\n")
    
    for (var in rv$numeric_vars) {
      var_data <- rv$sas[[var]]
      cat("Variable:", var, "\n")
      cat("  Minimum:", min(var_data, na.rm = TRUE), "\n")
      cat("  Maximum:", max(var_data, na.rm = TRUE), "\n")
      cat("  Moyenne:", mean(var_data, na.rm = TRUE), "\n")
      cat("  Médiane:", median(var_data, na.rm = TRUE), "\n")
      cat("  Écart-type:", sd(var_data, na.rm = TRUE), "\n")
      cat("  Coefficient de variation:",
          round(
            sd(var_data, na.rm = TRUE) / mean(var_data, na.rm = TRUE) * 100,
            2
          ),
          "%\n")
      cat("  Valeurs manquantes:", sum(is.na(var_data)), "\n\n")
    }
    
    # Statistiques pour chaque variable catégorielle
    cat("VARIABLES CATÉGORIELLES\n")
    cat("-----------------------\n\n")
    
    for (var in rv$categorical_vars) {
      var_data <- rv$sas[[var]]
      cat("Variable:", var, "\n")
      
      freq_table <- table(var_data)
      prop_table <- prop.table(freq_table) * 100
      
      for (i in 1:length(freq_table)) {
        cat("  ",
            names(freq_table)[i],
            ":",
            freq_table[i],
            "(",
            round(prop_table[i], 2),
            "%)\n")
      }
      
      cat("  Nombre de catégories:", length(freq_table), "\n")
      cat("  Mode:", names(freq_table)[which.max(freq_table)], "\n")
      cat("  Valeurs manquantes:", sum(is.na(var_data)), "\n\n")
    }
  })
  
  # SAS - Graphiques de distribution
  output$sas_dist_plot <- renderPlotly({
    req(rv$sas, input$sas_graph_var)
    
    var_data <- rv$sas[[input$sas_graph_var]]
    
    if (!is.numeric(var_data)) {
      return(NULL)
    }
    
    # Histogramme avec courbe de densité
    hist_data <- hist(var_data, plot = FALSE)
    density_data <- density(var_data, na.rm = TRUE)
    
    p <- plot_ly() %>%
      add_histogram(
        x = var_data,
        name = "Histogramme",
        marker = list(color = "rgba(0, 166, 90, 0.5)")
      ) %>%
      add_trace(
        x = density_data$x,
        y = density_data$y * length(var_data) * diff(hist_data$breaks)[1],
        type = "scatter",
        mode = "lines",
        name = "Densité",
        line = list(color = "#00a65a", width = 2)
      ) %>%
      layout(
        title = paste("Distribution de", input$sas_graph_var),
        xaxis = list(title = input$sas_graph_var),
        yaxis = list(title = "Fréquence"),
        barmode = "overlay"
      )
    
    return(p)
  })
  
  # SAS - Boîte à moustaches
  output$sas_boxplot <- renderPlotly({
    req(rv$sas, input$sas_graph_var, input$sas_var)
    
    var_data <- rv$sas[[input$sas_graph_var]]
    
    if (!is.numeric(var_data)) {
      return(NULL)
    }
    
    # Boîte à moustaches par groupe
    p <-
      plot_ly(
        rv$sas,
        y = ~ get(input$sas_graph_var),
        color = ~ get(input$sas_var),
        type = "box"
      ) %>%
      layout(
        title = paste(
          "Boîte à moustaches de",
          input$sas_graph_var,
          "par",
          input$sas_var
        ),
        yaxis = list(title = input$sas_graph_var),
        xaxis = list(title = input$sas_var),
        boxmode = "group"
      )
    
    return(p)
  })
  
  # SAS - Paramètres des tests statistiques
  output$sas_test_params <- renderUI({
    req(rv$sas, input$sas_test_type)
    
    if (input$sas_test_type == "ttest") {
      tagList(
        selectInput("sas_ttest_var", "Variable", choices = rv$numeric_vars),
        numericInput("sas_ttest_mu", "Valeur de référence (μ0)", 0),
        radioButtons(
          "sas_ttest_alt",
          "Hypothèse alternative",
          choices = c(
            "Différent de μ0" = "two.sided",
            "Supérieur à μ0" = "greater",
            "Inférieur à μ0" = "less"
          ),
          selected = "two.sided"
        )
      )
    } else if (input$sas_test_type == "normality") {
      tagList(
        selectInput("sas_norm_var", "Variable", choices = rv$numeric_vars),
        radioButtons(
          "sas_norm_test",
          "Test de normalité",
          choices = c(
            "Shapiro-Wilk" = "shapiro",
            "Kolmogorov-Smirnov" = "ks",
            "Anderson-Darling" = "ad"
          ),
          selected = "shapiro"
        )
      )
    } else if (input$sas_test_type == "chisq") {
      tagList(
        selectInput(
          "sas_chisq_var1",
          "Variable 1",
          choices = rv$categorical_vars
        ),
        selectInput(
          "sas_chisq_var2",
          "Variable 2",
          choices = rv$categorical_vars
        )
      )
    } else if (input$sas_test_type == "anova") {
      tagList(
        selectInput(
          "sas_anova_var",
          "Variable numérique",
          choices = rv$numeric_vars
        ),
        selectInput(
          "sas_anova_group",
          "Variable de groupement",
          choices = rv$categorical_vars
        )
      )
    }
  })
  
  # SAS - Résultats des tests statistiques
  output$sas_test_result <- renderPlotly({
    req(rv$sas, input$sas_test_type)
    
    if (input$sas_test_type == "ttest" &&
        !is.null(input$sas_ttest_var)) {
      # Test de Student
      var_data <- rv$sas[[input$sas_ttest_var]]
      test_result <- t.test(
        var_data,
        mu = input$sas_ttest_mu,
        alternative = input$sas_ttest_alt
      )
      
      cat("TEST DE STUDENT (t-test)\n")
      cat("=======================\n\n")
      cat("Variable:", input$sas_ttest_var, "\n")
      cat("Hypothèse nulle (H0): μ =", input$sas_ttest_mu, "\n")
      cat("Hypothèse alternative (H1):",
          ifelse(input$sas_ttest_alt == "two.sided",
                 paste("μ ≠", input$sas_ttest_mu),
                 ifelse(input$sas_ttest_alt == "greater",
                        paste("μ >", input$sas_ttest_mu),
                        paste("μ <", input$sas_ttest_mu))),
          "\n\n")
      
      cat("Résultats:\n")
      cat("  Statistique t:", test_result$statistic, "\n")
      cat("  Degrés de liberté:", test_result$parameter, "\n")
      cat("  p-value:", test_result$p.value, "\n")
      cat("  Intervalle de confiance à 95%:", test_result$conf.int[1], "-", test_result$conf.int[2], "\n")
      cat("  Moyenne de l'échantillon:", test_result$estimate, "\n\n")
      
      cat("Conclusion:\n")
      if (test_result$p.value < 0.05) {
        cat("  Rejet de H0 au seuil de 5% (p-value < 0.05).\n")
        cat("  Il existe une différence statistiquement significative entre la moyenne de l'échantillon et la valeur de référence.\n")
      } else {
        cat("  Non-rejet de H0 au seuil de 5% (p-value ≥ 0.05).\n")
        cat("  Il n'y a pas de différence statistiquement significative entre la moyenne de l'échantillon et la valeur de référence.\n")
      }
      
    } else if (input$sas_test_type == "normality" &&
               !is.null(input$sas_norm_var)) {
      # Test de normalité
      var_data <- rv$sas[[input$sas_norm_var]]
      
      cat("TEST DE NORMALITÉ\n")
      cat("=================\n\n")
      cat("Variable:", input$sas_norm_var, "\n")
      cat("Test utilisé:",
          ifelse(input$sas_norm_test == "shapiro", "Shapiro-Wilk",
                 ifelse(input$sas_norm_test == "ks", "Kolmogorov-Smirnov", "Anderson-Darling")),
          "\n\n")
      
      if (input$sas_norm_test == "shapiro") {
        test_result <- shapiro.test(var_data)
        cat("Résultats:\n")
        cat("  Statistique W:", test_result$statistic, "\n")
        cat("  p-value:", test_result$p.value, "\n\n")
      } else if (input$sas_norm_test == "ks") {
        test_result <- ks.test(
          var_data,
          "pnorm",
          mean = mean(var_data, na.rm = TRUE),
          sd = sd(var_data, na.rm = TRUE)
        )
        cat("Résultats:\n")
        cat("  Statistique D:", test_result$statistic, "\n")
        cat("  p-value:", test_result$p.value, "\n\n")
      } else if (input$sas_norm_test == "ad") {
        test_result <- ad.test(var_data)
        cat("Résultats:\n")
        cat("  Statistique A:", test_result$statistic, "\n")
        cat("  p-value:", test_result$p.value, "\n\n")
      }
      
      cat("Conclusion:\n")
      if (test_result$p.value < 0.05) {
        cat("  Rejet de H0 au seuil de 5% (p-value < 0.05).\n")
        cat("  La distribution de la variable ne suit pas une loi normale.\n")
      } else {
        cat("  Non-rejet de H0 au seuil de 5% (p-value ≥ 0.05).\n")
        cat("  La distribution de la variable suit approximativement une loi normale.\n")
      }
      
      cat("\nStatistiques complémentaires:\n")
      cat("  Asymétrie (Skewness):", skewness(var_data, na.rm = TRUE), "\n")
      cat("  Aplatissement (Kurtosis):", kurtosis(var_data, na.rm = TRUE), "\n")
      cat("  Note: Pour une distribution normale, l'asymétrie est proche de 0 et l'aplatissement proche de 3.\n")
      
    } else if (input$sas_test_type == "chisq" &&
               !is.null(input$sas_chisq_var1) && !is.null(input$sas_chisq_var2)) {
      # Test du Chi-2
      var1_data <- rv$sas[[input$sas_chisq_var1]]
      var2_data <- rv$sas[[input$sas_chisq_var2]]
      
      cont_table <- table(var1_data, var2_data)
      
      cat("TEST DU CHI-2 D'INDÉPENDANCE\n")
      cat("============================\n\n")
      cat("Variables:", input$sas_chisq_var1, "et", input$sas_chisq_var2, "\n\n")
      cat("Tableau de contingence:\n")
      print(cont_table)
      cat("\n")
      
      test_result <- chisq.test(cont_table)
      
      cat("Résultats:\n")
      cat("  Statistique χ²:", test_result$statistic, "\n")
      cat("  Degrés de liberté:", test_result$parameter, "\n")
      cat("  p-value:", test_result$p.value, "\n\n")
      
      cat("Conclusion:\n")
      if (test_result$p.value < 0.05) {
        cat("  Rejet de H0 au seuil de 5% (p-value < 0.05).\n")
        cat("  Il existe une association statistiquement significative entre les deux variables.\n")
      } else {
        cat("  Non-rejet de H0 au seuil de 5% (p-value ≥ 0.05).\n")
        cat("  Il n'y a pas d'association statistiquement significative entre les deux variables.\n")
      }
      
      expected <- test_result$expected
      cat("\nVérification des conditions d'application:\n")
      cat("  Nombre de cellules avec effectif théorique < 5:",
          sum(expected < 5), "sur", length(expected), "\n")
      if (sum(expected < 5) > 0.2 * length(expected)) {
        cat("  Attention: Plus de 20% des cellules ont un effectif théorique < 5.\n")
        cat("  Le test du Chi-2 peut ne pas être fiable dans ces conditions.\n")
      }
      
    } else if (input$sas_test_type == "anova" &&
               !is.null(input$sas_anova_var) && !is.null(input$sas_anova_group)) {
      # ANOVA
      var_data <- rv$sas[[input$sas_anova_var]]
      group_data <- rv$sas[[input$sas_anova_group]]
      
      formula <- as.formula(paste(input$sas_anova_var, "~", input$sas_anova_group))
      
      cat("ANALYSE DE VARIANCE (ANOVA)\n")
      cat("===========================\n\n")
      cat("Variable dépendante:", input$sas_anova_var, "\n")
      cat("Variable de groupement:", input$sas_anova_group, "\n\n")
      
      cat("Statistiques descriptives par groupe:\n")
      group_stats <- aggregate(
        var_data,
        by = list(group_data),
        FUN = function(x) c(n = length(x), mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))
      )
      colnames(group_stats) <- c("Groupe", "Statistiques")
      
      for (i in 1:nrow(group_stats)) {
        cat("  ", group_stats$Groupe[i],
            ": n =", group_stats$Statistiques[i, "n"],
            ", moyenne =", round(group_stats$Statistiques[i, "mean"], 2),
            ", écart-type =", round(group_stats$Statistiques[i, "sd"], 2), "\n")
      }
      cat("\n")
      
      anova_result <- aov(formula, data = rv$sas)
      anova_summary <- summary(anova_result)
      
      cat("Tableau ANOVA:\n")
      print(anova_summary)
      cat("\n")
      
      f_value <- anova_summary[[1]]$`F value`[1]
      p_value <- anova_summary[[1]]$`Pr(>F)`[1]
      
      cat("Conclusion:\n")
      if (p_value < 0.05) {
        cat("  Rejet de H0 au seuil de 5% (p-value < 0.05).\n")
        cat("  Il existe des différences significatives entre les groupes.\n")
        cat("\nTest post-hoc de Tukey (comparaisons multiples):\n")
        tukey_result <- TukeyHSD(anova_result)
        print(tukey_result)
      } else {
        cat("  Non-rejet de H0 au seuil de 5% (p-value ≥ 0.05).\n")
        cat("  Pas de différence significative entre les groupes.\n")
      }
      
      cat("\nTest d'homogénéité des variances (Levene):\n")
      levene_result <- leveneTest(formula, data = rv$sas)
      print(levene_result)
      
      if (levene_result$`Pr(>F)`[1] < 0.05) {
        cat("  Les variances ne sont pas homogènes (p < 0.05).\n")
        cat("  Attention: L'hypothèse d'homogénéité des variances n'est pas respectée.\n")
      } else {
        cat("  Les variances sont homogènes (p ≥ 0.05).\n")
      }
    }
  })
  
  
  # SAS - Graphique pour les tests statistiques
  output$sas_test_plot <- renderPlotly ({
    req(rv$sas, input$sas_test_type)
    
    
    if (input$sas_test_type == "ttest" &&
        !is.null(input$sas_ttest_var)) {
      # Graphique pour t-test
      var_data <- rv$sas[[input$sas_ttest_var]]
      
      # Histogramme avec ligne pour la moyenne et la valeur de référence
      p <- plot_ly() %>%
        add_histogram(
          x = var_data,
          name = "Distribution",
          marker = list(color = "rgba(0, 166, 90, 0.5)")
        ) %>%
        add_trace(
          x = c(mean(var_data, na.rm = TRUE), mean(var_data, na.rm = TRUE)),
          y = c(0, 10),
          type = "scatter",
          mode = "lines",
          name = "Moyenne échantillon",
          line = list(
            color = "red",
            width = 2,
            dash = "solid"
          )
        ) %>%
        add_trace(
          x = c(input$sas_ttest_mu, input$sas_ttest_mu),
          y = c(0, 10),
          type = "scatter",
          mode = "lines",
          name = "Valeur de référence",
          line = list(
            color = "blue",
            width = 2,
            dash = "dash"
          )
        ) %>%
        layout(
          title = paste("Distribution de", input$sas_ttest_var),
          xaxis = list(title = input$sas_ttest_var),
          yaxis = list(title = "Fréquence")
        )
      
      return(p)
      
    } else if (input$sas_test_type == "normality" &&
               !is.null(input$sas_norm_var)) {
      # Graphique pour test de normalité
      var_data <- rv$sas[[input$sas_norm_var]]
      
      # QQ-Plot
      qqnorm_data <- qqnorm(var_data, plot.it = FALSE)
      df <- data.frame(x = qqnorm_data$x, y = qqnorm_data$y)
      
      p <- plot_ly() %>%
        add_trace(
          data = df,
          x = ~ x,
          y = ~ y,
          type = "scatter",
          mode = "markers",
          marker = list(color = "#00a65a"),
          name = "Points"
        ) %>%
        add_trace(
          x = c(min(df$x, na.rm = TRUE), max(df$x, na.rm = TRUE)),
          y = c(min(df$y, na.rm = TRUE), max(df$y, na.rm = TRUE)),
          type = "scatter",
          mode = "lines",
          line = list(color = "red"),
          name = "Ligne théorique"
        ) %>%
        layout(
          title = paste("QQ-Plot de", input$sas_norm_var),
          xaxis = list(title = "Quantiles théoriques"),
          yaxis = list(title = "Quantiles observés")
        )
      
      return(p)
      
    } else if (input$sas_test_type == "chisq" &&
               !is.null(input$sas_chisq_var1) &&
               !is.null(input$sas_chisq_var2)) {
      # Graphique pour test du Chi-2
      var1_data <- rv$sas[[input$sas_chisq_var1]]
      var2_data <- rv$sas[[input$sas_chisq_var2]]
      
      # Tableau de contingence
      cont_table <- table(var1_data, var2_data)
      
      # Convertir en dataframe pour plotly
      df <- as.data.frame(cont_table)
      colnames(df) <- c("Var1", "Var2", "Freq")
      
      # Graphique en mosaïque
      p <-
        plot_ly(
          df,
          x = ~ Var1,
          y = ~ Freq,
          color = ~ Var2,
          type = "bar"
        ) %>%
        layout(
          title = paste(
            "Tableau de contingence:",
            input$sas_chisq_var1,
            "vs",
            input$sas_chisq_var2
          ),
          xaxis = list(title = input$sas_chisq_var1),
          yaxis = list(title = "Fréquence"),
          barmode = "stack"
        )
      
      return(p)
      
    } else if (input$sas_test_type == "anova" &&
               !is.null(input$sas_anova_var) &&
               !is.null(input$sas_anova_group)) {
      # Graphique pour ANOVA
      
      # Boîte à moustaches par groupe
      p <-
        plot_ly(
          rv$sas,
          y = ~ get(input$sas_anova_var),
          color = ~ get(input$sas_anova_group),
          type = "box"
        ) %>%
        layout(
          title = paste(
            "Boîte à moustaches de",
            input$sas_anova_var,
            "par",
            input$sas_anova_group
          ),
          yaxis = list(title = input$sas_anova_var),
          xaxis = list(title = input$sas_anova_group)
        )
      
      return(p)
    }
  })
  
    
    # SAS - Intervalles de confiance
    output$sas_ic_result <- renderPrint({
      req(rv$sas, input$sas_ic_var, input$sas_ic_level)
      
      var_data <- rv$sas[[input$sas_ic_var]]
      
      cat("INTERVALLE DE CONFIANCE\n")
      cat("======================\n\n")
      cat("Variable:", input$sas_ic_var, "\n")
      cat("Niveau de confiance:", input$sas_ic_level * 100, "%\n\n")
      
      # Calcul de l'intervalle de confiance
      ic_result <- MeanCI(var_data, conf.level = input$sas_ic_level)
      
      cat("Résultats:\n")
      cat("  Moyenne:", mean(var_data, na.rm = TRUE), "\n")
      cat("  Erreur standard:",
          sd(var_data, na.rm = TRUE) / sqrt(length(var_data)),
          "\n")
      cat("  Borne inférieure:", ic_result[2], "\n")
      cat("  Borne supérieure:", ic_result[3], "\n")
      cat("  Largeur de l'intervalle:",
          ic_result[3] - ic_result[2],
          "\n\n")
      
      cat("Interprétation:\n")
      cat(
        "  Avec un niveau de confiance de",
        input$sas_ic_level * 100,
        "%, la vraie moyenne de la population\n"
      )
      cat("  se situe entre", ic_result[2], "et", ic_result[3], ".\n")
    })
    
    # SAS - Graphique pour les intervalles de confiance
    output$sas_ic_plot <- renderPlotly({
      req(rv$sas, input$sas_ic_var, input$sas_ic_level)
      
      var_data <- rv$sas[[input$sas_ic_var]]
      
      # Calcul de l'intervalle de confiance
      ic_result <- MeanCI(var_data, conf.level = input$sas_ic_level)
      mean_val <- mean(var_data, na.rm = TRUE)
      lower_bound <- ic_result[2]
      upper_bound <- ic_result[3]
      
      # Histogramme avec ligne pour la moyenne et les bornes de l'IC
      p <- plot_ly() %>%
        add_histogram(
          x = var_data,
          name = "Distribution",
          marker = list(color = "rgba(0, 166, 90, 0.5)")
        ) %>%
        add_trace(
          x = c(mean_val, mean_val),
          y = c(0, 10),
          type = "scatter",
          mode = "lines",
          name = "Moyenne",
          line = list(color = "red", width = 2)
        ) %>%
        add_trace(
          x = c(lower_bound, lower_bound),
          y = c(0, 10),
          type = "scatter",
          mode = "lines",
          name = "Borne inférieure",
          line = list(
            color = "blue",
            width = 2,
            dash = "dash"
          )
        ) %>%
        add_trace(
          x = c(upper_bound, upper_bound),
          y = c(0, 10),
          type = "scatter",
          mode = "lines",
          name = "Borne supérieure",
          line = list(
            color = "blue",
            width = 2,
            dash = "dash"
          )
        ) %>%
        layout(
          title = paste(
            "Intervalle de confiance à",
            input$sas_ic_level * 100,
            "% pour",
            input$sas_ic_var
          ),
          xaxis = list(title = input$sas_ic_var),
          yaxis = list(title = "Fréquence")
        )
      
      return(p)
    })
    
    # SAS - Téléchargement de l'échantillon
    output$download_sas <- downloadHandler(
      filename = function() {
        paste("echantillon_SAS_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        write_xlsx(rv$sas, path = file)
      }
    )
    
    # Stratifié - Génération de l'échantillon - MODIFIÉ
    observeEvent(input$strat_run, {
      req(rv$data, input$strat_var, input$strat_aux_var)
      w$show()
      
      # Grouper les données par strate
      strata <- split(rv$data, rv$data[[input$strat_var]])
      
      # Allocation proportionnelle uniquement (suppression des autres méthodes)
      # Allocation proportionnelle
      strata_sizes <- sapply(strata, nrow)
      total_size <- sum(strata_sizes)
      proportions <- strata_sizes / total_size
      allocations <- round(proportions * input$strat_n)
      
      # S'assurer que les allocations ne dépassent pas les tailles des strates
      allocations <- pmin(allocations, sapply(strata, nrow))
      
      # Ajuster pour s'assurer que la somme des allocations = strat_n
      diff <- input$strat_n - sum(allocations)
      if (diff != 0) {
        # Ajouter ou soustraire la différence à la plus grande strate
        idx <- which.max(allocations)
        allocations[idx] <- allocations[idx] + diff
      }
      
      # Créer le tableau des allocations
      strata_names <- names(strata)
      strata_sizes <- sapply(strata, nrow)
      strata_props <- strata_sizes / sum(strata_sizes)
      
      rv$strat_alloc <- data.frame(
        Strate = strata_names,
        Taille = strata_sizes,
        Proportion = strata_props,
        Allocation = allocations,
        Methode = "Proportionnelle" # Toujours proportionnelle maintenant
      )
      
      # Nouvelle partie: allocation détaillée avec subdivision par variable auxiliaire
      # Créer une allocation détaillée par strate et variable auxiliaire
      detailed_alloc <- list()
      
      for (strat_name in strata_names) {
        stratum_data <- strata[[strat_name]]
        stratum_alloc <- allocations[which(strata_names == strat_name)]
        
        # Subdiviser par variable auxiliaire
        if (is.factor(stratum_data[[input$strat_aux_var]]) ||
            is.character(stratum_data[[input$strat_aux_var]])) {
          # Pour variables catégorielles
          aux_groups <-
            split(stratum_data, stratum_data[[input$strat_aux_var]])
          aux_sizes <- sapply(aux_groups, nrow)
          aux_props <- aux_sizes / sum(aux_sizes)
          aux_allocs <- round(aux_props * stratum_alloc)
          
          # Ajuster pour s'assurer que la somme = allocation de la strate
          aux_diff <- stratum_alloc - sum(aux_allocs)
          if (aux_diff != 0 && length(aux_allocs) > 0) {
            idx <- which.max(aux_allocs)
            aux_allocs[idx] <- aux_allocs[idx] + aux_diff
          }
          
          for (aux_name in names(aux_groups)) {
            aux_alloc <- aux_allocs[which(names(aux_groups) == aux_name)]
            detailed_alloc[[paste(strat_name, aux_name, sep = "_")]] <-
              list(
                Strate = strat_name,
                Auxiliaire = aux_name,
                Taille = aux_sizes[which(names(aux_groups) == aux_name)],
                Proportion = aux_props[which(names(aux_groups) == aux_name)],
                Allocation = aux_alloc,
                Data = aux_groups[[aux_name]]
              )
          }
        } else {
          # Pour variables numériques, créer des classes
          breaks <-
            quantile(stratum_data[[input$strat_aux_var]],
                     probs = seq(0, 1, 0.25),
                     na.rm = TRUE)
          if (length(unique(breaks)) < 5) {
            # Si pas assez de valeurs uniques, utiliser des breaks manuels
            min_val <-
              min(stratum_data[[input$strat_aux_var]], na.rm = TRUE)
            max_val <-
              max(stratum_data[[input$strat_aux_var]], na.rm = TRUE)
            breaks <- seq(min_val, max_val, length.out = 5)
          }
          
          aux_classes <-
            cut(stratum_data[[input$strat_aux_var]],
                breaks = breaks,
                include.lowest = TRUE)
          aux_groups <- split(stratum_data, aux_classes)
          aux_sizes <- sapply(aux_groups, nrow)
          aux_props <- aux_sizes / sum(aux_sizes)
          aux_allocs <- round(aux_props * stratum_alloc)
          
          # Ajuster pour s'assurer que la somme = allocation de la strate
          aux_diff <- stratum_alloc - sum(aux_allocs)
          if (aux_diff != 0 && length(aux_allocs) > 0) {
            idx <- which.max(aux_allocs)
            aux_allocs[idx] <- aux_allocs[idx] + aux_diff
          }
          
          for (aux_name in names(aux_groups)) {
            aux_alloc <- aux_allocs[which(names(aux_groups) == aux_name)]
            detailed_alloc[[paste(strat_name, aux_name, sep = "_")]] <-
              list(
                Strate = strat_name,
                Auxiliaire = aux_name,
                Taille = aux_sizes[which(names(aux_groups) == aux_name)],
                Proportion = aux_props[which(names(aux_groups) == aux_name)],
                Allocation = aux_alloc,
                Data = aux_groups[[aux_name]]
              )
          }
        }
      }
      
      # Convertir la liste d'allocations détaillées en dataframe
      detailed_df <- do.call(rbind, lapply(detailed_alloc, function(x) {
        data.frame(
          Strate = x$Strate,
          Auxiliaire = x$Auxiliaire,
          Taille = x$Taille,
          Proportion = x$Proportion,
          Allocation = x$Allocation
        )
      }))
      rownames(detailed_df) <- NULL
      
      # Stocker l'allocation détaillée
      rv$strat_alloc_detailed <- detailed_df
      
      # Échantillonnage basé sur l'allocation détaillée
      strat_samples <- list()
      
      for (i in 1:nrow(detailed_df)) {
        strat_name <- detailed_df$Strate[i]
        aux_name <- detailed_df$Auxiliaire[i]
        alloc_size <- detailed_df$Allocation[i]
        
        # Récupérer les données correspondantes
        detail_key <- paste(strat_name, aux_name, sep = "_")
        if (alloc_size > 0 && detail_key %in% names(detailed_alloc)) {
          detail_data <- detailed_alloc[[detail_key]]$Data
          
          # S'assurer que nous ne demandons pas plus d'observations que disponibles
          sample_size <- min(alloc_size, nrow(detail_data))
          
          if (sample_size > 0) {
            strat_samples[[i]] <-
              detail_data[sample.int(nrow(detail_data), sample_size),]
          }
        }
      }
      
      # Combiner les échantillons
      rv$strat <- do.call(rbind, strat_samples)
      
      # Mettre à jour les sélecteurs pour les graphiques
      updateSelectInput(session, "strat_graph_var", choices = names(rv$data))
      updateSelectInput(session, "strat_desc_var", choices = rv$numeric_vars)
      
      # Mettre à jour les sélecteurs pour la comparaison
      if (!is.null(rv$sas)) {
        updateSelectInput(session, "comp_var", choices = rv$numeric_vars)
        updateSelectInput(session, "comp_precision_var", choices = rv$numeric_vars)
        updateSelectInput(session, "comp_rep_var", choices = rv$categorical_vars)
        updateSelectInput(session, "comp_test_var", choices = rv$numeric_vars)
      }
      
      w$hide()
    })
    
    # Stratifié - Affichage des allocations
    output$strat_alloc <- renderDT({
      req(rv$strat_alloc)
      datatable(rv$strat_alloc, options = list(scrollX = TRUE))
    })
    
    # Stratifié - Affichage des allocations détaillées - NOUVEAU
    output$strat_alloc_detailed <- renderDT({
      req(rv$strat_alloc_detailed)
      datatable(rv$strat_alloc_detailed, options = list(scrollX = TRUE))
    })
    
    # Stratifié - Graphique des allocations
    output$strat_alloc_plot <- renderPlotly({
      req(rv$strat_alloc)
      
      p <-
        plot_ly(
          rv$strat_alloc,
          x = ~ Strate,
          y = ~ Allocation,
          type = "bar",
          marker = list(color = "#00a65a")
        ) %>%
        layout(
          title = "Allocations par strate",
          xaxis = list(title = ""),
          yaxis = list(title = "Nombre d'observations")
        )
      
      return(p)
    })
    
    # Stratifié - Graphique des allocations détaillées - NOUVEAU
    output$strat_alloc_detailed_plot <- renderPlotly({
      req(rv$strat_alloc_detailed)
      
      p <- plot_ly(
        rv$strat_alloc_detailed,
        x = ~ Strate,
        y = ~ Allocation,
        color = ~ Auxiliaire,
        type = "bar"
      ) %>%
        layout(
          title = paste("Allocation détaillée par strate et variable auxiliaire"),
          xaxis = list(title = "Strate"),
          yaxis = list(title = "Allocation"),
          barmode = "stack"
        )
      
      return(p)
    })
    
    # Stratifié - Affichage de l'échantillon
    output$strat_sample <- renderDT({
      req(rv$strat)
      datatable(rv$strat, options = list(scrollX = TRUE))
    })
    
    # Stratifié - Résumé des statistiques par strate
    output$strat_stats_summary <- renderUI({
      req(rv$strat, rv$strat_alloc, rv$data)
      
      strata_list <-
        lapply(unique(rv$strat[[input$strat_var]]), function(strat_val) {
          strat_size <-
            nrow(rv$strat[rv$strat[[input$strat_var]] == strat_val,])
          strat_alloc <-
            rv$strat_alloc$Allocation[rv$strat_alloc$Strate == strat_val]
          strat_prop <-
            rv$strat_alloc$Proportion[rv$strat_alloc$Strate == strat_val]
          
          div(
            div(class = "stat-title", paste("Strate:", strat_val)),
            div(class = "stat-value", strat_size),
            div(class = "stat-subtitle", paste0(
              round(strat_prop * 100, 1), "% du total"
            )),
            hr()
          )
        })
      
      tagList(
        div(class = "stat-value", nrow(rv$strat)),
        div(class = "stat-subtitle", "observations totales"),
        hr(),
        div(class = "stat-title", "Taux d'échantillonnage"),
        div(class = "stat-value", paste0(round(
          nrow(rv$strat) / nrow(rv$data) * 100, 2
        ), "%")),
        hr(),
        div(class = "stat-title", "Répartition par strate"),
        strata_list
      )
    })
    
    # Stratifié - Graphique personnalisé
    output$strat_graph <- renderPlotly({
      req(rv$strat,
          input$strat_graph_var,
          input$strat_graph_type)
      
      var_data <- rv$strat[[input$strat_graph_var]]
      
      if (input$strat_graph_type == "bar") {
        # Diagramme à barres (pour variables catégorielles)
        if (is.numeric(var_data)) {
          # Pour les variables numériques, créer des classes
          breaks <- pretty(range(var_data, na.rm = TRUE), n = 10)
          var_data <-
            cut(var_data,
                breaks = breaks,
                include.lowest = TRUE)
        }
        
        freq_table <- table(var_data)
        df <- data.frame(Catégorie = names(freq_table),
                         Fréquence = as.vector(freq_table))
        
        p <-
          plot_ly(
            df,
            x = ~ Catégorie,
            y = ~ Fréquence,
            type = "bar",
            marker = list(color = "#00a65a")
          ) %>%
          layout(
            title = paste("Distribution de", input$strat_graph_var),
            xaxis = list(title = ""),
            yaxis = list(title = "Fréquence")
          )
      } else if (input$strat_graph_type == "pie") {
        # Diagramme circulaire
        if (is.numeric(var_data)) {
          # Pour les variables numériques, créer des classes
          breaks <- pretty(range(var_data, na.rm = TRUE), n = 10)
          var_data <-
            cut(var_data,
                breaks = breaks,
                include.lowest = TRUE)
        }
        
        freq_table <- table(var_data)
        df <- data.frame(Catégorie = names(freq_table),
                         Fréquence = as.vector(freq_table))
        
        p <-
          plot_ly(
            df,
            labels = ~ Catégorie,
            values = ~ Fréquence,
            type = "pie",
            marker = list(colors = colorRampPalette(brewer.pal(9, "Set3"))(length(freq_table)))
          ) %>%
          layout(
            title = paste("Distribution de", input$strat_graph_var),
            showlegend = TRUE
          )
      } else if (input$strat_graph_type == "box") {
        # Boîte à moustaches
        if (!is.numeric(var_data)) {
          return(NULL)
        }
        
        p <-
          plot_ly(
            y = var_data,
            type = "box",
            marker = list(color = "#00a65a")
          ) %>%
          layout(
            title = paste("Boîte à moustaches de", input$strat_graph_var),
            yaxis = list(title = input$strat_graph_var)
          )
      } else if (input$strat_graph_type == "hist") {
        # Histogramme
        if (!is.numeric(var_data)) {
          return(NULL)
        }
        
        p <-
          plot_ly(
            x = var_data,
            type = "histogram",
            marker = list(color = "#00a65a")
          ) %>%
          layout(
            title = paste("Histogramme de", input$strat_graph_var),
            xaxis = list(title = input$strat_graph_var),
            yaxis = list(title = "Fréquence")
          )
      } else if (input$strat_graph_type == "density") {
        # Courbe de densité
        if (!is.numeric(var_data)) {
          return(NULL)
        }
        
        density_data <- density(var_data, na.rm = TRUE)
        df <- data.frame(x = density_data$x, y = density_data$y)
        
        p <-
          plot_ly(
            df,
            x = ~ x,
            y = ~ y,
            type = "scatter",
            mode = "lines",
            fill = "tozeroy",
            line = list(color = "#00a65a"),
            fillcolor = "rgba(0, 166, 90, 0.3)"
          ) %>%
          layout(
            title = paste("Densité de", input$strat_graph_var),
            xaxis = list(title = input$strat_graph_var),
            yaxis = list(title = "Densité")
          )
      }
      
      return(p)
    })
    
    # Stratifié - Graphique de comparaison
    output$strat_compare_plot <- renderPlotly({
      req(rv$data, rv$strat, input$strat_var)
      
      # Compter les fréquences dans les deux ensembles
      data_counts <- table(rv$data[[input$strat_var]])
      sample_counts <- table(rv$strat[[input$strat_var]])
      
      # Créer un dataframe pour plotly
      categories <- unique(c(names(data_counts), names(sample_counts)))
      df <- data.frame(
        Catégorie = rep(categories, 2),
        Fréquence = c(
          sapply(categories, function(cat)
            ifelse(cat %in% names(data_counts), data_counts[cat], 0)),
          sapply(categories, function(cat)
            ifelse(cat %in% names(sample_counts), sample_counts[cat], 0))
        ),
        Type = rep(c("Cadre", "Échantillon"), each = length(categories))
      )
      
      # Créer le graphique
      p <-
        plot_ly(
          df,
          x = ~ Catégorie,
          y = ~ Fréquence,
          color = ~ Type,
          type = "bar",
          colors = c(
            "Cadre" = "#3c8dbc",
            "Échantillon" = "#00a65a"
          )
        ) %>%
        layout(
          title = paste("Comparaison des distributions de", input$strat_var),
          xaxis = list(title = ""),
          yaxis = list(title = "Fréquence"),
          barmode = "group",
          legend = list(orientation = "h", y = 1.1)
        )
      
      return(p)
    })
    
    # Stratifié - Statistiques descriptives
    output$strat_desc_stats <- renderPrint({
      req(rv$strat, input$strat_desc_var, input$strat_var)
      
      cat("STATISTIQUES DESCRIPTIVES PAR STRATE\n")
      cat("===================================\n\n")
      cat("Variable:", input$strat_desc_var, "\n")
      cat("Variable de stratification:", input$strat_var, "\n\n")
      
      # Statistiques par strate
      strata <- split(rv$strat, rv$strat[[input$strat_var]])
      
      for (strat_name in names(strata)) {
        stratum <- strata[[strat_name]]
        var_data <- stratum[[input$strat_desc_var]]
        
        cat("Strate:", strat_name, "\n")
        cat("  Nombre d'observations:", length(var_data), "\n")
        
        if (is.numeric(var_data)) {
          cat("  Minimum:", min(var_data, na.rm = TRUE), "\n")
          cat("  Maximum:", max(var_data, na.rm = TRUE), "\n")
          cat("  Moyenne:", mean(var_data, na.rm = TRUE), "\n")
          cat("  Médiane:", median(var_data, na.rm = TRUE), "\n")
          cat("  Écart-type:", sd(var_data, na.rm = TRUE), "\n")
          cat("  Coefficient de variation:",
              round(
                sd(var_data, na.rm = TRUE) / mean(var_data, na.rm = TRUE) * 100,
                2
              ),
              "%\n")
        } else {
          # Pour les variables catégorielles
          freq_table <- table(var_data)
          prop_table <- prop.table(freq_table) * 100
          
          for (i in 1:length(freq_table)) {
            cat(
              "  ",
              names(freq_table)[i],
              ":",
              freq_table[i],
              "(",
              round(prop_table[i], 2),
              "%)\n"
            )
          }
        }
        
        cat("\n")
      }
      
      # Statistiques globales
      var_data <- rv$strat[[input$strat_desc_var]]
      
      cat("Statistiques globales:\n")
      cat("  Nombre d'observations:", length(var_data), "\n")
      
      if (is.numeric(var_data)) {
        cat("  Minimum:", min(var_data, na.rm = TRUE), "\n")
        cat("  Maximum:", max(var_data, na.rm = TRUE), "\n")
        cat("  Moyenne:", mean(var_data, na.rm = TRUE), "\n")
        cat("  Médiane:", median(var_data, na.rm = TRUE), "\n")
        cat("  Écart-type:", sd(var_data, na.rm = TRUE), "\n")
        cat("  Coefficient de variation:",
            round(
              sd(var_data, na.rm = TRUE) / mean(var_data, na.rm = TRUE) * 100,
              2
            ),
            "%\n")
      } else {
        # Pour les variables catégorielles
        freq_table <- table(var_data)
        prop_table <- prop.table(freq_table) * 100
        
        for (i in 1:length(freq_table)) {
          cat(
            "  ",
            names(freq_table)[i],
            ":",
            freq_table[i],
            "(",
            round(prop_table[i], 2),
            "%)\n"
          )
        }
      }
    })
    
    # Stratifié - Graphiques de distribution
    output$strat_dist_plot <- renderPlotly({
      req(rv$strat, input$strat_desc_var)
      
      var_data <- rv$strat[[input$strat_desc_var]]
      
      if (!is.numeric(var_data)) {
        return(NULL)
      }
      
      # Histogramme avec courbe de densité
      hist_data <- hist(var_data, plot = FALSE)
      density_data <- density(var_data, na.rm = TRUE)
      
      p <- plot_ly() %>%
        add_histogram(
          x = var_data,
          name = "Histogramme",
          marker = list(color = "rgba(0, 166, 90, 0.5)")
        ) %>%
        add_trace(
          x = density_data$x,
          y = density_data$y * length(var_data) * diff(hist_data$breaks)[1],
          type = "scatter",
          mode = "lines",
          name = "Densité",
          line = list(color = "#00a65a", width = 2)
        ) %>%
        layout(
          title = paste("Distribution de", input$strat_desc_var),
          xaxis = list(title = input$strat_desc_var),
          yaxis = list(title = "Fréquence"),
          barmode = "overlay"
        )
      
      return(p)
    })
    
    # Stratifié - Boîte à moustaches par strate
    output$strat_boxplot_by_strata <- renderPlotly({
      req(rv$strat, input$strat_desc_var, input$strat_var)
      
      var_data <- rv$strat[[input$strat_desc_var]]
      
      if (!is.numeric(var_data)) {
        return(NULL)
      }
      
      # Boîte à moustaches par strate
      p <-
        plot_ly(
          rv$strat,
          y = ~ get(input$strat_desc_var),
          color = ~ get(input$strat_var),
          type = "box"
        ) %>%
        layout(
          title = paste(
            "Boîte à moustaches de",
            input$strat_desc_var,
            "par",
            input$strat_var
          ),
          yaxis = list(title = input$strat_desc_var),
          xaxis = list(title = input$strat_var)
        )
      
      return(p)
    })
    
    # Stratifié - Paramètres des tests statistiques
    output$strat_test_params <- renderUI({
      req(rv$strat, input$strat_test_type)
      
      if (input$strat_test_type == "ttest") {
        tagList(
          selectInput("strat_ttest_var", "Variable", choices = rv$numeric_vars),
          numericInput("strat_ttest_mu", "Valeur de référence (μ0)", 0),
          radioButtons(
            "strat_ttest_alt",
            "Hypothèse alternative",
            choices = c(
              "Différent de μ0" = "two.sided",
              "Supérieur à μ0" = "greater",
              "Inférieur à μ0" = "less"
            ),
            selected = "two.sided"
          )
        )
      } else if (input$strat_test_type == "normality") {
        tagList(
          selectInput("strat_norm_var", "Variable", choices = rv$numeric_vars),
          radioButtons(
            "strat_norm_test",
            "Test de normalité",
            choices = c(
              "Shapiro-Wilk" = "shapiro",
              "Kolmogorov-Smirnov" = "ks",
              "Anderson-Darling" = "ad"
            ),
            selected = "shapiro"
          )
        )
      } else if (input$strat_test_type == "chisq") {
        tagList(
          selectInput(
            "strat_chisq_var1",
            "Variable 1",
            choices = rv$categorical_vars
          ),
          selectInput(
            "strat_chisq_var2",
            "Variable 2",
            choices = rv$categorical_vars
          )
        )
      } else if (input$strat_test_type == "anova") {
        tagList(
          selectInput(
            "strat_anova_var",
            "Variable numérique",
            choices = rv$numeric_vars
          ),
          selectInput(
            "strat_anova_group",
            "Variable de groupement",
            choices = rv$categorical_vars
          )
        )
      }
    })
    
    # Stratifié - Résultats des tests statistiques
    output$strat_test_result <- renderPrint({
      req(rv$strat, input$strat_test_type)
      
      if (input$strat_test_type == "ttest" &&
          !is.null(input$strat_ttest_var)) {
        # Test de Student
        var_data <- rv$strat[[input$strat_ttest_var]]
        test_result <-
          t.test(
            var_data,
            mu = input$strat_ttest_mu,
            alternative = input$strat_ttest_alt
          )
        
        cat("TEST DE STUDENT (t-test)\n")
        cat("=======================\n\n")
        cat("Variable:", input$strat_ttest_var, "\n")
        cat("Hypothèse nulle (H0): μ =", input$strat_ttest_mu, "\n")
        cat(
          "Hypothèse alternative (H1):",
          ifelse(
            input$strat_ttest_alt == "two.sided",
            paste("μ ≠", input$strat_ttest_mu),
            ifelse(
              input$strat_ttest_alt == "greater",
              paste("μ >", input$strat_ttest_mu),
              paste("μ <", input$strat_ttest_mu)
            )
          ),
          "\n\n"
        )
        
        cat("Résultats:\n")
        cat("  Statistique t:", test_result$statistic, "\n")
        cat("  Degrés de liberté:", test_result$parameter, "\n")
        cat("  p-value:", test_result$p.value, "\n")
        cat(
          "  Intervalle de confiance à 95%:",
          test_result$conf.int[1],
          "-",
          test_result$conf.int[2],
          "\n"
        )
        cat("  Moyenne de l'échantillon:",
            test_result$estimate,
            "\n\n")
        
        cat("Conclusion:\n")
        if (test_result$p.value < 0.05) {
          cat("  Rejet de H0 au seuil de 5% (p-value < 0.05).\n")
          cat(
            "  Il existe une différence statistiquement significative entre la moyenne de l'échantillon et la valeur de référence.\n"
          )
        } else {
          cat("  Non-rejet de H0 au seuil de 5% (p-value ≥ 0.05).\n")
          cat(
            "  Il n'y a pas de différence statistiquement significative entre la moyenne de l'échantillon et la valeur de référence.\n"
          )
        }
        
      } else if (input$strat_test_type == "normality" &&
                 !is.null(input$strat_norm_var)) {
        # Test de normalité
        var_data <- rv$strat[[input$strat_norm_var]]
        
        cat("TEST DE NORMALITÉ\n")
        cat("=================\n\n")
        cat("Variable:", input$strat_norm_var, "\n")
        cat(
          "Test utilisé:",
          ifelse(
            input$strat_norm_test == "shapiro",
            "Shapiro-Wilk",
            ifelse(
              input$strat_norm_test == "ks",
              "Kolmogorov-Smirnov",
              "Anderson-Darling"
            )
          ),
          "\n\n"
        )
        
        if (input$strat_norm_test == "shapiro") {
          test_result <- shapiro.test(var_data)
          cat("Résultats:\n")
          cat("  Statistique W:", test_result$statistic, "\n")
          cat("  p-value:", test_result$p.value, "\n\n")
        } else if (input$strat_norm_test == "ks") {
          test_result <-
            ks.test(
              var_data,
              "pnorm",
              mean = mean(var_data, na.rm = TRUE),
              sd = sd(var_data, na.rm = TRUE)
            )
          cat("Résultats:\n")
          cat("  Statistique D:", test_result$statistic, "\n")
          cat("  p-value:", test_result$p.value, "\n\n")
        } else if (input$strat_norm_test == "ad") {
          test_result <- ad.test(var_data)
          cat("Résultats:\n")
          cat("  Statistique A:", test_result$statistic, "\n")
          cat("  p-value:", test_result$p.value, "\n\n")
        }
        
        cat("Conclusion:\n")
        if (test_result$p.value < 0.05) {
          cat("  Rejet de H0 au seuil de 5% (p-value < 0.05).\n")
          cat("  La distribution de la variable ne suit pas une loi normale.\n")
        } else {
          cat("  Non-rejet de H0 au seuil de 5% (p-value ≥ 0.05).\n")
          cat("  La distribution de la variable suit approximativement une loi normale.\n")
        }
        
        # Statistiques d'asymétrie et d'aplatissement
        cat("\nStatistiques complémentaires:\n")
        cat("  Asymétrie (Skewness):",
            skewness(var_data, na.rm = TRUE),
            "\n")
        cat("  Aplatissement (Kurtosis):",
            kurtosis(var_data, na.rm = TRUE),
            "\n")
        cat(
          "  Note: Pour une distribution normale, l'asymétrie est proche de 0 et l'aplatissement proche de 3.\n"
        )
        
      } else if (input$strat_test_type == "chisq" &&
                 !is.null(input$strat_chisq_var1) &&
                 !is.null(input$strat_chisq_var2)) {
        # Test du Chi-2
        var1_data <- rv$strat[[input$strat_chisq_var1]]
        var2_data <- rv$strat[[input$strat_chisq_var2]]
        
        # Tableau de contingence
        cont_table <- table(var1_data, var2_data)
        
        cat("TEST DU CHI-2 D'INDÉPENDANCE\n")
        cat("============================\n\n")
        cat("Variables:",
            input$strat_chisq_var1,
            "et",
            input$strat_chisq_var2,
            "\n\n")
        
        cat("Tableau de contingence:\n")
        print(cont_table)
        cat("\n")
        
        # Test du Chi-2
        test_result <- chisq.test(cont_table)
        
        cat("Résultats:\n")
        cat("  Statistique χ²:", test_result$statistic, "\n")
        cat("  Degrés de liberté:", test_result$parameter, "\n")
        cat("  p-value:", test_result$p.value, "\n\n")
        
        cat("Conclusion:\n")
        if (test_result$p.value < 0.05) {
          cat("  Rejet de H0 au seuil de 5% (p-value < 0.05).\n")
          cat(
            "  Il existe une association statistiquement significative entre les deux variables.\n"
          )
        } else {
          cat("  Non-rejet de H0 au seuil de 5% (p-value ≥ 0.05).\n")
          cat(
            "  Il n'y a pas d'association statistiquement significative entre les deux variables.\n"
          )
        }
        
        # Vérification des conditions d'application
        expected <- test_result$expected
        cat("\nVérification des conditions d'application:\n")
        cat(
          "  Nombre de cellules avec effectif théorique < 5:",
          sum(expected < 5),
          "sur",
          length(expected),
          "\n"
        )
        if (sum(expected < 5) > 0.2 * length(expected)) {
          cat("  Attention: Plus de 20% des cellules ont un effectif théorique inférieur à 5.\n")
          cat("  Le test du Chi-2 peut ne pas être fiable dans ces conditions.\n")
        }
        
      } else if (input$strat_test_type == "anova" &&
                 !is.null(input$strat_anova_var) &&
                 !is.null(input$strat_anova_group)) {
        # ANOVA
        var_data <- rv$strat[[input$strat_anova_var]]
        group_data <- rv$strat[[input$strat_anova_group]]
        
        # Formule pour ANOVA
        formula <-
          as.formula(paste(input$strat_anova_var, "~", input$strat_anova_group))
        
        cat("ANALYSE DE VARIANCE (ANOVA)\n")
        cat("===========================\n\n")
        cat("Variable dépendante:", input$strat_anova_var, "\n")
        cat("Variable de groupement:",
            input$strat_anova_group,
            "\n\n")
        
        # Statistiques descriptives par groupe
        cat("Statistiques descriptives par groupe:\n")
        group_stats <- aggregate(
          var_data,
          by = list(group_data),
          FUN = function(x)
            c(
              n = length(x),
              mean = mean(x, na.rm = TRUE),
              sd = sd(x, na.rm = TRUE)
            )
        )
        colnames(group_stats) <- c("Groupe", "Statistiques")
        
        for (i in 1:nrow(group_stats)) {
          cat(
            "  ",
            group_stats$Groupe[i],
            ": n =",
            group_stats$Statistiques[i, "n"],
            ", moyenne =",
            round(group_stats$Statistiques[i, "mean"], 2),
            ", écart-type =",
            round(group_stats$Statistiques[i, "sd"], 2),
            "\n"
          )
        }
        cat("\n")
        
        # ANOVA
        anova_result <- aov(formula, data = rv$strat)
        anova_summary <- summary(anova_result)
        
        cat("Tableau ANOVA:\n")
        print(anova_summary)
        cat("\n")
        
        # Extraction des résultats
        f_value <- anova_summary[[1]]$`F value`[1]
        p_value <- anova_summary[[1]]$`Pr(>F)`[1]
        
        cat("Conclusion:\n")
        if (p_value < 0.05) {
          cat("  Rejet de H0 au seuil de 5% (p-value < 0.05).\n")
          cat(
            "  Il existe des différences statistiquement significatives entre les groupes.\n"
          )
          
          # Test post-hoc de Tukey
          cat("\nTest post-hoc de Tukey (comparaisons multiples):\n")
          tukey_result <- TukeyHSD(anova_result)
          print(tukey_result)
        } else {
          cat("  Non-rejet de H0 au seuil de 5% (p-value ≥ 0.05).\n")
          cat(
            "  Il n'y a pas de différence statistiquement significative entre les groupes.\n"
          )
        }
        
        # Test d'homogénéité des variances (Levene)
        cat("\nTest d'homogénéité des variances (Levene):\n")
        levene_result <- leveneTest(formula, data = rv$strat)
        print(levene_result)
        
        if (levene_result$`Pr(>F)`[1] < 0.05) {
          cat("  Les variances ne sont pas homogènes (p < 0.05).\n")
          cat("  Attention: L'hypothèse d'homogénéité des variances n'est pas respectée.\n")
        } else {
          cat("  Les variances sont homogènes (p ≥ 0.05).\n")
        }
      }
    })
    
    # Stratifié - Graphique pour les tests statistiques
    output$strat_test_plot <- renderPlotly({
      req(rv$strat, input$strat_test_type)
      
      if (input$strat_test_type == "ttest" &&
          !is.null(input$strat_ttest_var)) {
        # Graphique pour t-test
        var_data <- rv$strat[[input$strat_ttest_var]]
        
        # Histogramme avec ligne pour la moyenne et la valeur de référence
        p <- plot_ly() %>%
          add_histogram(
            x = var_data,
            name = "Distribution",
            marker = list(color = "rgba(0, 166, 90, 0.5)")
          ) %>%
          add_trace(
            x = c(
              mean(var_data, na.rm = TRUE),
              mean(var_data, na.rm = TRUE)
            ),
            y = c(0, 10),
            type = "scatter",
            mode = "lines",
            name = "Moyenne échantillon",
            line = list(
              color = "red",
              width = 2,
              dash = "solid"
            )
          ) %>%
          add_trace(
            x = c(input$strat_ttest_mu, input$strat_ttest_mu),
            y = c(0, 10),
            type = "scatter",
            mode = "lines",
            name = "Valeur de référence",
            line = list(
              color = "blue",
              width = 2,
              dash = "dash"
            )
          ) %>%
          layout(
            title = paste("Distribution de", input$strat_ttest_var),
            xaxis = list(title = input$strat_ttest_var),
            yaxis = list(title = "Fréquence")
          )
        
        return(p)
        
      } else if (input$strat_test_type == "normality" &&
                 !is.null(input$strat_norm_var)) {
        # Graphique pour test de normalité
        var_data <- rv$strat[[input$strat_norm_var]]
        
        # QQ-Plot
        qqnorm_data <- qqnorm(var_data, plot.it = FALSE)
        df <- data.frame(x = qqnorm_data$x, y = qqnorm_data$y)
        
        p <- plot_ly() %>%
          add_trace(
            data = df,
            x = ~ x,
            y = ~ y,
            type = "scatter",
            mode = "markers",
            marker = list(color = "#00a65a"),
            name = "Points"
          ) %>%
          add_trace(
            x = c(min(df$x, na.rm = TRUE), max(df$x, na.rm = TRUE)),
            y = c(min(df$y, na.rm = TRUE), max(df$y, na.rm = TRUE)),
            type = "scatter",
            mode = "lines",
            line = list(color = "red"),
            name = "Ligne théorique"
          ) %>%
          layout(
            title = paste("QQ-Plot de", input$strat_norm_var),
            xaxis = list(title = "Quantiles théoriques"),
            yaxis = list(title = "Quantiles observés")
          )
        
        return(p)
        
      } else if (input$strat_test_type == "chisq" &&
                 !is.null(input$strat_chisq_var1) &&
                 !is.null(input$strat_chisq_var2)) {
        # Graphique pour test du Chi-2
        var1_data <- rv$strat[[input$strat_chisq_var1]]
        var2_data <- rv$strat[[input$strat_chisq_var2]]
        
        # Tableau de contingence
        cont_table <- table(var1_data, var2_data)
        
        # Convertir en dataframe pour plotly
        df <- as.data.frame(cont_table)
        colnames(df) <- c("Var1", "Var2", "Freq")
        
        # Graphique en mosaïque
        p <-
          plot_ly(
            df,
            x = ~ Var1,
            y = ~ Freq,
            color = ~ Var2,
            type = "bar"
          ) %>%
          layout(
            title = paste(
              "Tableau de contingence:",
              input$strat_chisq_var1,
              "vs",
              input$strat_chisq_var2
            ),
            xaxis = list(title = input$strat_chisq_var1),
            yaxis = list(title = "Fréquence"),
            barmode = "stack"
          )
        
        return(p)
        
      } else if (input$strat_test_type == "anova" &&
                 !is.null(input$strat_anova_var) &&
                 !is.null(input$strat_anova_group)) {
        # Graphique pour ANOVA
        
        # Boîte à moustaches par groupe
        p <-
          plot_ly(
            rv$strat,
            y = ~ get(input$strat_anova_var),
            color = ~ get(input$strat_anova_group),
            type = "box"
          ) %>%
          layout(
            title = paste(
              "Boîte à moustaches de",
              input$strat_anova_var,
              "par",
              input$strat_anova_group
            ),
            yaxis = list(title = input$strat_anova_var),
            xaxis = list(title = input$strat_anova_group)
          )
        
        return(p)
      }
    })
    
    # Stratifié - Téléchargement de l'échantillon
    output$download_strat <- downloadHandler(
      filename = function() {
        paste("echantillon_stratifie_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        write_xlsx(rv$strat, path = file)
      }
    )
    
    # Comparaison - Boîte à moustaches
    output$comp_boxplot <- renderPlotly({
      req(rv$sas, rv$strat, input$comp_var)
      
      # Créer un dataframe combiné pour plotly
      sas_data <- rv$sas[[input$comp_var]]
      strat_data <- rv$strat[[input$comp_var]]
      
      df <- data.frame(Valeur = c(sas_data, strat_data),
                       Méthode = c(rep("SAS", length(sas_data)), rep("Stratifié", length(strat_data))))
      
      # Boîte à moustaches
      p <- plot_ly(
        df,
        y = ~ Valeur,
        color = ~ Méthode,
        type = "box",
        colors = c(
          "SAS" = "#f56954",
          "Stratifié" = "#00a65a"
        )
      ) %>%
        layout(
          title = paste(
            "Comparaison de",
            input$comp_var,
            "entre les méthodes d'échantillonnage"
          ),
          yaxis = list(title = input$comp_var),
          boxmode = "group"
        )
      
      return(p)
    })
    
    # Comparaison - Statistiques SAS
    output$comp_sas_stats <- renderPrint({
      req(rv$sas, input$comp_var)
      
      var_data <- rv$sas[[input$comp_var]]
      
      cat("STATISTIQUES - ÉCHANTILLON SAS\n")
      cat("=============================\n\n")
      cat("Variable:", input$comp_var, "\n")
      cat("Taille de l'échantillon:", length(var_data), "\n\n")
      
      if (is.numeric(var_data)) {
        cat("Minimum:", min(var_data, na.rm = TRUE), "\n")
        cat("Maximum:", max(var_data, na.rm = TRUE), "\n")
        cat("Moyenne:", mean(var_data, na.rm = TRUE), "\n")
        cat("Médiane:", median(var_data, na.rm = TRUE), "\n")
        cat("Écart-type:", sd(var_data, na.rm = TRUE), "\n")
        cat("Coefficient de variation:",
            round(
              sd(var_data, na.rm = TRUE) / mean(var_data, na.rm = TRUE) * 100,
              2
            ),
            "%\n")
        cat("Erreur standard:",
            sd(var_data, na.rm = TRUE) / sqrt(length(var_data)),
            "\n")
        
        # Intervalle de confiance à 95%
        ic <- MeanCI(var_data, conf.level = 0.95)
        cat("IC à 95%:", ic[2], "-", ic[3], "\n")
      } else {
        # Pour les variables catégorielles
        freq_table <- table(var_data)
        prop_table <- prop.table(freq_table) * 100
        
        for (i in 1:length(freq_table)) {
          cat(names(freq_table)[i],
              ":",
              freq_table[i],
              "(",
              round(prop_table[i], 2),
              "%)\n")
        }
      }
    })
    
    # Comparaison - Statistiques Stratifié
    output$comp_strat_stats <- renderPrint({
      req(rv$strat, input$comp_var)
      
      var_data <- rv$strat[[input$comp_var]]
      
      cat("STATISTIQUES - ÉCHANTILLON STRATIFIÉ\n")
      cat("===================================\n\n")
      cat("Variable:", input$comp_var, "\n")
      cat("Taille de l'échantillon:", length(var_data), "\n\n")
      
      if (is.numeric(var_data)) {
        cat("Minimum:", min(var_data, na.rm = TRUE), "\n")
        cat("Maximum:", max(var_data, na.rm = TRUE), "\n")
        cat("Moyenne:", mean(var_data, na.rm = TRUE), "\n")
        cat("Médiane:", median(var_data, na.rm = TRUE), "\n")
        cat("Écart-type:", sd(var_data, na.rm = TRUE), "\n")
        cat("Coefficient de variation:",
            round(
              sd(var_data, na.rm = TRUE) / mean(var_data, na.rm = TRUE) * 100,
              2
            ),
            "%\n")
        cat("Erreur standard:",
            sd(var_data, na.rm = TRUE) / sqrt(length(var_data)),
            "\n")
        
        # Intervalle de confiance à 95%
        ic <- MeanCI(var_data, conf.level = 0.95)
        cat("IC à 95%:", ic[2], "-", ic[3], "\n")
      } else {
        # Pour les variables catégorielles
        freq_table <- table(var_data)
        prop_table <- prop.table(freq_table) * 100
        
        for (i in 1:length(freq_table)) {
          cat(names(freq_table)[i],
              ":",
              freq_table[i],
              "(",
              round(prop_table[i], 2),
              "%)\n")
        }
      }
    })
    
    # Comparaison - Précision des estimations
    output$comp_precision_plot <- renderPlotly({
      req(rv$sas,
          rv$strat,
          input$comp_precision_var,
          input$comp_precision_level)
      
      sas_data <- rv$sas[[input$comp_precision_var]]
      strat_data <- rv$strat[[input$comp_precision_var]]
      
      # Calcul des intervalles de confiance
      sas_ic <-
        MeanCI(sas_data, conf.level = input$comp_precision_level)
      strat_ic <-
        MeanCI(strat_data, conf.level = input$comp_precision_level)
      
      # Créer un dataframe pour plotly
      df <- data.frame(
        Méthode = c("SAS", "Stratifié"),
        Moyenne = c(
          mean(sas_data, na.rm = TRUE),
          mean(strat_data, na.rm = TRUE)
        ),
        IC_inf = c(sas_ic[2], strat_ic[2]),
        IC_sup = c(sas_ic[3], strat_ic[3])
      )
      
      # Graphique des intervalles de confiance
      p <-
        plot_ly(
          df,
          x = ~ Méthode,
          y = ~ Moyenne,
          type = "scatter",
          mode = "markers",
          error_y = list(
            type = "data",
            symmetric = FALSE,
            array = df$IC_sup - df$Moyenne,
            arrayminus = df$Moyenne - df$IC_inf
          ),
          marker = list(
            size = 10,
            color = c("#f56954", "#00a65a")
          )
        ) %>%
        layout(
          title = paste(
            "Intervalles de confiance à",
            input$comp_precision_level * 100,
            "% pour",
            input$comp_precision_var
          ),
          yaxis = list(title = input$comp_precision_var),
          xaxis = list(title = "")
        )
      
      return(p)
    })
    
    # Comparaison - Statistiques de précision
    output$comp_precision_stats <- renderPrint({
      req(rv$sas,
          rv$strat,
          input$comp_precision_var,
          input$comp_precision_level)
      
      sas_data <- rv$sas[[input$comp_precision_var]]
      strat_data <- rv$strat[[input$comp_precision_var]]
      
      # Calcul des intervalles de confiance
      sas_ic <-
        MeanCI(sas_data, conf.level = input$comp_precision_level)
      strat_ic <-
        MeanCI(strat_data, conf.level = input$comp_precision_level)
      
      # Calcul des erreurs standard
      sas_se <- sd(sas_data, na.rm = TRUE) / sqrt(length(sas_data))
      strat_se <-
        sd(strat_data, na.rm = TRUE) / sqrt(length(strat_data))
      
      # Calcul des largeurs des intervalles
      sas_width <- sas_ic[3] - sas_ic[2]
      strat_width <- strat_ic[3] - strat_ic[2]
      
      cat("COMPARAISON DE LA PRÉCISION DES ESTIMATIONS\n")
      cat("==========================================\n\n")
      cat("Variable:", input$comp_precision_var, "\n")
      cat("Niveau de confiance:",
          input$comp_precision_level * 100,
          "%\n\n")
      
      cat("Échantillon SAS:\n")
      cat("  Taille:", length(sas_data), "\n")
      cat("  Moyenne:", mean(sas_data, na.rm = TRUE), "\n")
      cat("  Erreur standard:", sas_se, "\n")
      cat("  IC:", sas_ic[2], "-", sas_ic[3], "\n")
      cat("  Largeur de l'IC:", sas_width, "\n\n")
      
      cat("Échantillon Stratifié:\n")
      cat("  Taille:", length(strat_data), "\n")
      cat("  Moyenne:", mean(strat_data, na.rm = TRUE), "\n")
      cat("  Erreur standard:", strat_se, "\n")
      cat("  IC:", strat_ic[2], "-", strat_ic[3], "\n")
      cat("  Largeur de l'IC:", strat_width, "\n\n")
      
      # Comparaison des précisions
      precision_ratio <- sas_width / strat_width
      
      cat("Comparaison:\n")
      cat("  Ratio des largeurs d'IC (SAS/Stratifié):",
          precision_ratio,
          "\n")
      
      if (precision_ratio > 1) {
        cat(
          "  L'échantillonnage stratifié est",
          round(precision_ratio, 2),
          "fois plus précis que l'échantillonnage SAS.\n"
        )
      } else if (precision_ratio < 1) {
        cat(
          "  L'échantillonnage SAS est",
          round(1 / precision_ratio, 2),
          "fois plus précis que l'échantillonnage stratifié.\n"
        )
      } else {
        cat("  Les deux méthodes ont une précision similaire.\n")
      }
    })
    
    # Comparaison - Représentativité
    output$comp_rep_plot <- renderPlotly({
      req(rv$data, rv$sas, rv$strat, input$comp_rep_var)
      
      # Compter les fréquences dans les trois ensembles
      data_counts <- table(rv$data[[input$comp_rep_var]])
      sas_counts <- table(rv$sas[[input$comp_rep_var]])
      strat_counts <- table(rv$strat[[input$comp_rep_var]])
      
      # Normaliser si demandé
      if (input$comp_rep_normalize) {
        data_counts <- prop.table(data_counts) * 100
        sas_counts <- prop.table(sas_counts) * 100
        strat_counts <- prop.table(strat_counts) * 100
      }
      
      # Créer un dataframe pour plotly
      categories <-
        unique(c(
          names(data_counts),
          names(sas_counts),
          names(strat_counts)
        ))
      df <- data.frame(
        Catégorie = rep(categories, 3),
        Fréquence = c(
          sapply(categories, function(cat)
            ifelse(cat %in% names(data_counts), data_counts[cat], 0)),
          sapply(categories, function(cat)
            ifelse(cat %in% names(sas_counts), sas_counts[cat], 0)),
          sapply(categories, function(cat)
            ifelse(cat %in% names(strat_counts), strat_counts[cat], 0))
        ),
        Type = rep(c("Cadre", "SAS", "Stratifié"), each = length(categories))
      )
      
      # Créer le graphique
      p <-
        plot_ly(
          df,
          x = ~ Catégorie,
          y = ~ Fréquence,
          color = ~ Type,
          type = "bar",
          colors = c(
            "Cadre" = "#3c8dbc",
            "SAS" = "#f56954",
            "Stratifié" = "#00a65a"
          )
        ) %>%
        layout(
          title = paste("Comparaison des distributions de", input$comp_rep_var),
          xaxis = list(title = ""),
          yaxis = list(
            title = ifelse(
              input$comp_rep_normalize,
              "Pourcentage (%)",
              "Fréquence"
            )
          ),
          barmode = "group",
          legend = list(orientation = "h", y = 1.1)
        )
      
      return(p)
    })
    
    # Comparaison - Test du Chi-2 pour la représentativité
    output$comp_rep_chisq <- renderPrint({
      req(rv$data, rv$sas, rv$strat, input$comp_rep_var)
      
      # Compter les fréquences dans les trois ensembles
      data_counts <- table(rv$data[[input$comp_rep_var]])
      sas_counts <- table(rv$sas[[input$comp_rep_var]])
      strat_counts <- table(rv$strat[[input$comp_rep_var]])
      
      # Assurer que toutes les catégories sont présentes dans chaque ensemble
      categories <-
        unique(c(
          names(data_counts),
          names(sas_counts),
          names(strat_counts)
        ))
      
      # Créer des tableaux complets
      data_full <-
        sapply(categories, function(cat)
          ifelse(cat %in% names(data_counts), data_counts[cat], 0))
      sas_full <-
        sapply(categories, function(cat)
          ifelse(cat %in% names(sas_counts), sas_counts[cat], 0))
      strat_full <-
        sapply(categories, function(cat)
          ifelse(cat %in% names(strat_counts), strat_counts[cat], 0))
      
      # Test du Chi-2 pour SAS vs Cadre
      sas_expected <- data_full * (sum(sas_full) / sum(data_full))
      sas_chisq <- sum((sas_full - sas_expected) ^ 2 / sas_expected)
      sas_df <- length(categories) - 1
      sas_pvalue <- 1 - pchisq(sas_chisq, sas_df)
      
      # Test du Chi-2 pour Stratifié vs Cadre
      strat_expected <- data_full * (sum(strat_full) / sum(data_full))
      strat_chisq <-
        sum((strat_full - strat_expected) ^ 2 / strat_expected)
      strat_df <- length(categories) - 1
      strat_pvalue <- 1 - pchisq(strat_chisq, strat_df)
      
      cat("TEST DE REPRÉSENTATIVITÉ (CHI-2)\n")
      cat("===============================\n\n")
      cat("Variable:", input$comp_rep_var, "\n\n")
      
      cat("Échantillon SAS vs Cadre:\n")
      cat("  Statistique χ²:", sas_chisq, "\n")
      cat("  Degrés de liberté:", sas_df, "\n")
      cat("  p-value:", sas_pvalue, "\n")
      
      if (sas_pvalue < 0.05) {
        cat("  Conclusion: L'échantillon SAS n'est PAS représentatif du cadre (p < 0.05).\n\n")
      } else {
        cat("  Conclusion: L'échantillon SAS est représentatif du cadre (p ≥ 0.05).\n\n")
      }
      
      cat("Échantillon Stratifié vs Cadre:\n")
      cat("  Statistique χ²:", strat_chisq, "\n")
      cat("  Degrés de liberté:", strat_df, "\n")
      cat("  p-value:", strat_pvalue, "\n")
      
      if (strat_pvalue < 0.05) {
        cat(
          "  Conclusion: L'échantillon Stratifié n'est PAS représentatif du cadre (p < 0.05).\n\n"
        )
      } else {
        cat(
          "  Conclusion: L'échantillon Stratifié est représentatif du cadre (p ≥ 0.05).\n\n"
        )
      }
      
      # Comparaison des deux méthodes
      cat("Comparaison des méthodes:\n")
      if (sas_pvalue >= 0.05 && strat_pvalue >= 0.05) {
        cat("  Les deux méthodes produisent des échantillons représentatifs du cadre.\n")
        if (sas_chisq < strat_chisq) {
          cat("  L'échantillon SAS est légèrement plus représentatif (χ² plus faible).\n")
        } else if (strat_chisq < sas_chisq) {
          cat(
            "  L'échantillon Stratifié est légèrement plus représentatif (χ² plus faible).\n"
          )
        } else {
          cat("  Les deux méthodes ont une représentativité similaire.\n")
        }
      } else if (sas_pvalue >= 0.05) {
        cat("  Seul l'échantillon SAS est représentatif du cadre.\n")
      } else if (strat_pvalue >= 0.05) {
        cat("  Seul l'échantillon Stratifié est représentatif du cadre.\n")
      } else {
        cat("  Aucune des deux méthodes ne produit un échantillon représentatif du cadre.\n")
        if (sas_chisq < strat_chisq) {
          cat("  L'échantillon SAS est néanmoins plus proche du cadre (χ² plus faible).\n")
        } else if (strat_chisq < sas_chisq) {
          cat(
            "  L'échantillon Stratifié est néanmoins plus proche du cadre (χ² plus faible).\n"
          )
        }
      }
    })
    
    # Comparaison - Tests de différence
    output$comp_test_plot <- renderPlotly({
      req(rv$sas, rv$strat, input$comp_test_var)
      
      sas_data <- rv$sas[[input$comp_test_var]]
      strat_data <- rv$strat[[input$comp_test_var]]
      
      # Créer un dataframe combiné pour plotly
      df <- data.frame(Valeur = c(sas_data, strat_data),
                       Méthode = c(rep("SAS", length(sas_data)), rep("Stratifié", length(strat_data))))
      
      # Graphique de densité
      p <- plot_ly() %>%
        add_trace(
          x = sas_data,
          type = "histogram",
          histnorm = "probability density",
          name = "SAS",
          marker = list(color = "rgba(245, 105, 84, 0.5)")
        ) %>%
        add_trace(
          x = strat_data,
          type = "histogram",
          histnorm = "probability density",
          name = "Stratifié",
          marker = list(color = "rgba(0, 166, 90, 0.5)")
        ) %>%
        layout(
          title = paste(
            "Distribution de",
            input$comp_test_var,
            "par méthode d'échantillonnage"
          ),
          xaxis = list(title = input$comp_test_var),
          yaxis = list(title = "Densité"),
          barmode = "overlay"
        )
      
      return(p)
    })
    
    # Comparaison - Résultats des tests de différence
    output$comp_test_result <- renderPrint({
      req(rv$sas,
          rv$strat,
          input$comp_test_var,
          input$comp_test_type)
      
      sas_data <- rv$sas[[input$comp_test_var]]
      strat_data <- rv$strat[[input$comp_test_var]]
      
      cat("TEST DE DIFFÉRENCE ENTRE LES MÉTHODES D'ÉCHANTILLONNAGE\n")
      cat("=====================================================\n\n")
      cat("Variable:", input$comp_test_var, "\n")
      cat(
        "Test utilisé:",
        ifelse(
          input$comp_test_type == "ttest",
          "Test de Student (t-test)",
          ifelse(
            input$comp_test_type == "wilcox",
            "Test de Mann-Whitney",
            "Test F (variances)"
          )
        ),
        "\n\n"
      )
      
      cat("Statistiques descriptives:\n")
      cat(
        "  SAS: n =",
        length(sas_data),
        ", moyenne =",
        mean(sas_data, na.rm = TRUE),
        ", écart-type =",
        sd(sas_data, na.rm = TRUE),
        "\n"
      )
      cat(
        "  Stratifié: n =",
        length(strat_data),
        ", moyenne =",
        mean(strat_data, na.rm = TRUE),
        ", écart-type =",
        sd(strat_data, na.rm = TRUE),
        "\n\n"
      )
      
      if (input$comp_test_type == "ttest") {
        # Test de Student
        test_result <- t.test(sas_data, strat_data)
        
        cat("Résultats du test de Student:\n")
        cat("  Statistique t:", test_result$statistic, "\n")
        cat("  Degrés de liberté:", test_result$parameter, "\n")
        cat("  p-value:", test_result$p.value, "\n")
        cat(
          "  Intervalle de confiance à 95%:",
          test_result$conf.int[1],
          "-",
          test_result$conf.int[2],
          "\n\n"
        )
        
        cat("Conclusion:\n")
        if (test_result$p.value < 0.05) {
          cat("  Rejet de H0 au seuil de 5% (p-value < 0.05).\n")
          cat(
            "  Il existe une différence statistiquement significative entre les moyennes des deux échantillons.\n"
          )
        } else {
          cat("  Non-rejet de H0 au seuil de 5% (p-value ≥ 0.05).\n")
          cat(
            "  Il n'y a pas de différence statistiquement significative entre les moyennes des deux échantillons.\n"
          )
        }
        
      } else if (input$comp_test_type == "wilcox") {
        # Test de Mann-Whitney
        test_result <- wilcox.test(sas_data, strat_data)
        
        cat("Résultats du test de Mann-Whitney:\n")
        cat("  Statistique W:", test_result$statistic, "\n")
        cat("  p-value:", test_result$p.value, "\n\n")
        
        cat("Conclusion:\n")
        if (test_result$p.value < 0.05) {
          cat("  Rejet de H0 au seuil de 5% (p-value < 0.05).\n")
          cat(
            "  Il existe une différence statistiquement significative entre les distributions des deux échantillons.\n"
          )
        } else {
          cat("  Non-rejet de H0 au seuil de 5% (p-value ≥ 0.05).\n")
          cat(
            "  Il n'y a pas de différence statistiquement significative entre les distributions des deux échantillons.\n"
          )
        }
        
      } else if (input$comp_test_type == "ftest") {
        # Test F pour les variances
        test_result <- var.test(sas_data, strat_data)
        
        cat("Résultats du test F (variances):\n")
        cat("  Statistique F:", test_result$statistic, "\n")
        cat(
          "  Degrés de liberté:",
          test_result$parameter[1],
          "et",
          test_result$parameter[2],
          "\n"
        )
        cat("  p-value:", test_result$p.value, "\n")
        cat("  Ratio des variances:", test_result$estimate, "\n")
        cat(
          "  Intervalle de confiance à 95%:",
          test_result$conf.int[1],
          "-",
          test_result$conf.int[2],
          "\n\n"
        )
        
        cat("Conclusion:\n")
        if (test_result$p.value < 0.05) {
          cat("  Rejet de H0 au seuil de 5% (p-value < 0.05).\n")
          cat(
            "  Il existe une différence statistiquement significative entre les variances des deux échantillons.\n"
          )
          
          if (test_result$estimate > 1) {
            cat(
              "  La variance de l'échantillon SAS est significativement plus grande que celle de l'échantillon Stratifié.\n"
            )
            cat(
              "  L'échantillonnage stratifié produit des estimations plus précises pour cette variable.\n"
            )
          } else {
            cat(
              "  La variance de l'échantillon Stratifié est significativement plus grande que celle de l'échantillon SAS.\n"
            )
            cat(
              "  L'échantillonnage SAS produit des estimations plus précises pour cette variable.\n"
            )
          }
        } else {
          cat("  Non-rejet de H0 au seuil de 5% (p-value ≥ 0.05).\n")
          cat(
            "  Il n'y a pas de différence statistiquement significative entre les variances des deux échantillons.\n"
          )
          cat(
            "  Les deux méthodes d'échantillonnage produisent des estimations de précision similaire pour cette variable.\n"
          )
        }
      }
    })
  }
  
  # 🚀 Lancement de l'application
  shinyApp(ui, server)