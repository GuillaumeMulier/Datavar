#' visu_data
#'
#' Rapid visualisation of the data with the informations given by the datavar
#'
#' Open a simple shinyapp to graphically explore the variables. If a plot is satisfactory enough, you can copy paste the code to reproduce it. You'll need to load library \code{ggplot2}.
#'
#' @param data The data.frame of the dataset you want to explore.
#' @param datavar The data.frame of the datavar containing metadata on the dataset.
#' @param langue For now, "fr" for french or "eng" for english.
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' visu_data(data = mtcars, datavar = datavarr, langue = "eng")
visu_data <- function(data, datavar, langue = "fr") {

  langue <- verif_langue(langue)
  # datavar <- verif_datavar(datavar)

  data_set <- rlang::enexpr(data)

  ui <- shiny::fluidPage(
    # 1ère ligne de sélection des variables
    shiny::fluidRow(
      shiny::column(5,
             shiny::selectInput(inputId = "var_x",
                                label = if (langue == "fr") {"Variable à décrire"} else {"Described variable"},
                                choices = c("", unique(datavar$var)),
                                selected = "",
                                multiple = FALSE)),
      shiny::column(2),
      shiny::column(5,
             shiny::selectInput(inputId = "var_y",
                                label = if (langue == "fr") {"Variable de croisement"} else {"Crossing variable"},
                                choices = c("", unique(datavar$var)),
                                selected = "",
                                multiple = FALSE))),
    # 2ème ligne pour les customisations
    shiny::fluidRow(
      shiny::column(5,
             shiny::tabsetPanel(id = "select_geom",
                                type = "hidden",
                                shiny::tabPanel("empty",
                                                shiny::textOutput(outputId = "nothing") %>%
                                                  shiny::tagAppendAttributes(style = "color: blue; font-face: bold; font-size:12px")),
                                shiny::tabPanel("quanti",
                                                shiny::radioButtons(inputId = "geom_quanti",
                                                                    label = if (langue == "fr") {"Quel geom utiliser ?"} else {"What geom to use?"},
                                                                    choices = if (langue == "fr") {
                                                                      c("Histogramme" = "histo", "Boxplot" = "box", "Points" = "points")
                                                                    } else {
                                                                      c("Histogramm" = "histo", "Boxplot" = "box", "Points" = "points")
                                                                    }, selected = "box")),
                                shiny::tabPanel("quali",
                                                shiny::radioButtons(inputId = "geom_quali",
                                                                    label = if (langue == "fr") {"Quel geom utiliser ?"} else {"What geom to use?"},
                                                                    choiceValues = "bar",
                                                                    choiceNames = "Barplot",
                                                                    selected = "bar"))),

             ),
      shiny::column(2,
             shiny::actionButton(inputId = "plot",
                                 label = if (langue == "fr") {"Dessiner le graphe !"} else {"Plot the graph!"},
                                 class = "btn-info")),
      shiny::column(5,
             shiny::tabsetPanel(id = "select_facet",
                                type = "hidden",
                                shiny::tabPanel("empty",
                                                shiny::textOutput(outputId = "nothing_y") %>%
                                                  shiny::tagAppendAttributes(style = "color: blue; font-face: bold; font-size:12px")),
                                shiny::tabPanel("quanti",
                                                shiny::textOutput(outputId = "erreur_y") %>%
                                                  shiny::tagAppendAttributes(style = "color: red; font-face: bold; font-size:15px; text-decoration: underline; text-decoration-color: red")),
                                shiny::tabPanel("quali",
                                                shiny::radioButtons(inputId = "facet",
                                                                    label = if (langue == "fr") {"Comment croiser ?"} else {"How to cross?"},
                                                                    choiceValues = c("facet", "axe"),
                                                                    selected = "axe",
                                                                    choiceNames = if (langue == "fr") {
                                                                      c("Facet", "Axe")
                                                                    } else {
                                                                      c("Facet", "Axis")
                                                                    })))

      )),
    # 3ème ligne le graphique
    shiny::fluidRow(
      shiny::column(8, shiny::plotOutput(outputId = "graphe", height = "800px")),
      shiny::column(4, shiny::verbatimTextOutput(outputId = "code_display"))
    )
    )



  server <- function(input, output, session) {

    # Nom des variables
    varX <- shiny::eventReactive(input$plot, input$var_x)
    varY <- shiny::eventReactive(input$plot, input$var_y)

    choixX <- shiny::reactive({
      if (input$var_x == "") {
        "empty"
      } else if (datavar$type[datavar$var == input$var_x] == "binary") {
        "quali"
      } else {
        datavar$type[datavar$var == input$var_x]
      }
    })
    choixY <- shiny::reactive({
      if (input$var_y == "") {
        "empty"
      } else if (datavar$type[datavar$var == input$var_y] == "binary") {
        "quali"
      } else {
        datavar$type[datavar$var == input$var_y]
      }
    })
    nomX <- shiny::reactive({
      if (is.na(datavar$nomvariable[datavar$var == varX()]) ||datavar$nomvariable[datavar$var == varX()] == "") {
        varX()
      } else {
        datavar$nomvariable[datavar$var == varX()]
      }
    })
    nomY <- shiny::reactive({
      if (is.na(datavar$nomvariable[datavar$var == varY()]) ||datavar$nomvariable[datavar$var == varY()] == "") {
        varY()
      } else {
        datavar$nomvariable[datavar$var == varY()]
      }
    })

    # Thème ggplot
    ggplot2::theme_set(ggplot2::theme_light(base_size = 13))

    # Messages affichés sur l'app
    output$nothing <- shiny::renderText({
      if (langue == "fr") {
        paste0("Choisir une variable.")
      } else {
        paste0("Choose a variable.")
      }
    })
    output$nothing_y <- shiny::renderText({
      if (langue == "fr") {
        paste0("Pas de variable de croisement.")
      } else {
        paste0("No crossing variable.")
      }
    })
    output$erreur_y <- shiny::renderText({
      if (choixX() != "quanti") {
        if (langue == "fr") {
          paste0("/!\\ La variable de croisement ne peut pas être quantitative.")
        } else {
          paste0("/!\\ Crossing variable can't be continuous.")
        }
      } else {
        if (langue == "fr") {
          paste0("Le graphe sera un scatterplot peu importe ce qui est choisi à gauche.")
        } else {
          paste0("Graph will be a scatterplot no matter what you choose in the left panel.")
        }
      }
    })

    shiny::observeEvent(input$var_x, {
      shiny::updateSelectInput(inputId = "var_y",
                        choices = c("", unique(datavar$var[datavar$var != input$var_x])),
                        selected = if (input$var_y == input$var_x) {""} else {input$var_y})
      shiny::updateTabsetPanel(inputId = "select_geom", selected = choixX())
    })
    shiny::observeEvent(input$var_y, {
      shiny::updateTabsetPanel(inputId = "select_facet", selected = choixY())
    })

    shiny::observeEvent(input$plot, {

      # Choix du geom
      if (choixX() == "quanti") {
        geomm <- if (input$geom_quanti == "histo") {
          "ggplot2::geom_histogram()"
        } else if (input$geom_quanti == "box") {
          "ggplot2::geom_boxplot()"
        } else if (input$geom_quanti == "points") {
          "ggplot2::geom_point()"
        }
      } else if (choixX() == "quali") {
        geomm <- "ggplot2::geom_bar()"
      }


      # Cas simple avec une description univariée
      if (choixY() == "empty") {

        fct_labeller <- "delete"
        facet <- "delete"
        couleur <- "delete"

        # Choix des aesthetics
        if (choixX() == "quanti") {

          titre <- paste0("ggplot2::ggtitle('", if (langue == "fr") "Distribution de : " else "Distribution of: ", nomX(), "')")

          if (input$geom_quanti == "histo") {
            esthetique <- paste0("ggplot2::aes(x = ", varX(),")")
            scaleX <- paste0("ggplot2::scale_x_continuous(name = '", nomX(), "')")
            scaleY <- paste0("ggplot2::scale_y_continuous(name = '", if (langue == "fr") "Effectif" else "Count", "')")
          } else if (input$geom_quanti == "box") {
            esthetique <- paste0("ggplot2::aes(y = ", varX(), ")")
            scaleX <- paste0("ggplot2::scale_x_continuous(name = NULL, breaks = NULL)")
            scaleY <- paste0("ggplot2::scale_y_continuous(name = '", nomX(), "')")
          } else if (input$geom_quanti == "points") {
            esthetique <- paste0("ggplot2::aes(x = 1, y = ", varX(), ")")
            scaleX <- paste0("ggplot2::scale_x_continuous(name = NULL, breaks = NULL)")
            scaleY <- paste0("ggplot2::scale_y_continuous(name = '", nomX(), "')")
          }

        } else if (choixX() == "quali") {

          titre <- paste0("ggplot2::ggtitle('", if (langue == "fr") "Répartition de : " else "Repartition of: ", nomX(), "')")

          if (input$geom_quali == "bar") {
            esthetique <- paste0("ggplot2::aes(x = factor(", varX(), "))")
            scaleX <- paste0("ggplot2::scale_x_discrete(name = '", nomX(), "')")
            scaleY <- paste0("ggplot2::scale_y_continuous(name = '", if (langue == "fr") "Effectif" else "Count", "')")
          }

        }

      } else { # Moins simple quand la description est bivariée

        # Choix des aesthetics
        if (choixX() == "quanti") {
          titre <- paste0("ggplot2::ggtitle('",
                          if (langue == "fr") "Distribution de : " else "Distribution of: ", nomX(),
                          if (langue == "fr") " en fonction de " else " based on ", nomY(), "')")
          if (input$geom_quanti == "histo") {
            scaleX <- paste0("ggplot2::scale_x_continuous(name = '", nomX(), "')")
            scaleY <- paste0("ggplot2::scale_y_continuous(name = '", if (langue == "fr") "Effectif" else "Count", "')")
            if (input$facet == "axe") {
              esthetique <- paste0("ggplot2::aes(x = ", varX(),", fill = factor(", varY(), "))")
              geomm <- gsub("\\(\\)", "\\(position = 'dodge'\\)", geomm)
              facet <- "delete"
              fct_labeller <- "delete"
              couleur <- paste0("ggplot2::scale_fill_discrete(name = '", nomY(), "')")
            } else if (input$facet == "facet") {
              facet <- paste0("ggplot2::facet_wrap(ggplot2::vars(", varY(),
                              "), ncol = 1, labeller = ggplot2::labeller(", varY(),
                              " = fct_labeller))")
              fct_labeller <- paste0("fct_labeller <- c(",
                                     paste(paste0("'", unique(factor(data[[varY()]])), "' = '", nomY(), if (langue == "fr") " " else "", ": ", unique(factor(data[[varY()]])), "'"), collapse = ", "),
                                     ")")
              esthetique <- paste0("ggplot2::aes(x = ", varX(),", fill = factor(", varY(), "))")
              geomm <- gsub("\\(\\)", "\\(show.legend = FALSE\\)", geomm)
              couleur <- "delete"
            }
          } else if (input$geom_quanti == "box") {
            scaleY <- paste0("ggplot2::scale_y_continuous(name = '", nomX(), "')")
            geomm <- gsub("\\(\\)", "\\(alpha = .6, show.legend = FALSE\\)", geomm)
            couleur <- "delete"
            if (input$facet == "axe") {
              esthetique <- paste0("ggplot2::aes(x = factor(", varY(), "), y = ", varX(), ", fill = factor(", varY(), "))")
              facet <- "delete"
              fct_labeller <- "delete"
              scaleX <- paste0("ggplot2::scale_x_discrete(name = '", nomY(), "')")
            } else if (input$facet == "facet") {
              esthetique <- paste0("ggplot2::aes(y = ", varX(), ", fill = factor(", varY(), "))")
              facet <- paste0("ggplot2::facet_wrap(ggplot2::vars(", varY(), "), nrow = 1, labeller = ggplot2::labeller(", varY(), " = fct_labeller))")
              fct_labeller <- paste0("fct_labeller <- c(",
                                     paste(paste0("'", unique(factor(data[[varY()]])), "' = '", nomY(), if (langue == "fr") " " else "", ": ", unique(factor(data[[varY()]])), "'"), collapse = ", "),
                                     ")")
              scaleX <- paste0("ggplot2::scale_x_continuous(name = NULL, breaks = NULL)")
            }
          } else if (input$geom_quanti == "points") {
            scaleY <- paste0("ggplot2::scale_y_continuous(name = '", nomX(), "')")
            geomm <- gsub("\\(\\)", "\\(size = 4, show.legend = FALSE\\)", geomm)
            couleur <- "delete"
            if (input$facet == "axe") {
              esthetique <- paste0("ggplot2::aes(x = factor(", varY(), "), y = ", varX(), ", color = factor(", varY(), "))")
              scaleX <- paste0("ggplot2::scale_x_discrete(name = '", nomY(), "')")
              facet <- "delete"
              fct_labeller <- "delete"
            } else if (input$facet == "facet") {
              esthetique <- paste0("ggplot2::aes(x = 1, y = ", varX(), ", color = factor(", varY(), "))")
              facet <- paste0("ggplot2::facet_wrap(ggplot2::vars(", varY(), "), nrow = 1, labeller = ggplot2::labeller(", varY(), " = fct_labeller))")
              fct_labeller <- paste0("fct_labeller <- c(",
                                     paste(paste0("'", unique(factor(data[[varY()]])), "' = '", nomY(), if (langue == "fr") " " else "", ": ", unique(factor(data[[varY()]])), "'"), collapse = ", "),
                                     ")")
              scaleX <- paste0("ggplot2::scale_x_continuous(name = NULL, breaks = NULL)")
            }
          }
          if (choixX() == "quanti" & choixY() == "quanti") {
            esthetique <- paste0("ggplot2::aes(x = ", varY(), ", y = ", varX(), ")")
            geomm <- paste0("ggplot2::geom_point(size = 4)")
            facet <- "delete"
            fct_labeller <- "delete"
            couleur <- "delete"
            scaleX <- paste0("ggplot2::scale_x_continuous(name = '", nomX(), "')")
            scaleY <- paste0("ggplot2::scale_y_continuous(name = '", nomY(), "')")
          }
        } else if (choixX() == "quali") {
          shiny::req(choixY() != "quanti")
          titre <- paste0("ggplot2::ggtitle('",
                          if (langue == "fr") "Répartition de : " else "Repartition of: ", nomX(),
                          if (langue == "fr") " en fonction de " else " based on ", nomY(), "')")
          scaleY <- paste0("ggplot2::scale_y_continuous(name = '", if (langue == "fr") "Effectif" else "Count", "')")
          if (input$geom_quali == "bar") {
              esthetique <- paste0("ggplot2::aes(x = factor(", varX(), "), fill = factor(", varY(), "))")
            if (input$facet == "axe") {
              geomm <- gsub("\\(\\)", "\\(position = 'dodge'\\)", geomm)
              facet <- "delete"
              fct_labeller <- "delete"
              scaleX <- paste0("ggplot2::scale_x_discrete(name = '", nomX(), "')")
              couleur <- paste0("ggplot2::scale_fill_discrete(name = '", nomY(), "')")
            } else if (input$facet == "facet") {
              facet <- paste0("ggplot2::facet_wrap(ggplot2::vars(", varY(), "), ncol = 1, labeller = ggplot2::labeller(", varY(), " = fct_labeller))")
              fct_labeller <- paste0("fct_labeller <- c(",
                                     paste(paste0("'", unique(factor(data[[varY()]])), "' = '", nomY(), if (langue == "fr") " " else "", ": ", unique(factor(data[[varY()]])), "'"), collapse = ", "),
                                     ")")
              geomm <- gsub("\\(\\)", "\\(show.legend = FALSE\\)", geomm)
              couleur <- "delete"
              scaleX <- paste0("ggplot2::scale_x_discrete(name = '", nomX(), "')")
            }
          }
        }

      }

      code <- shiny::reactive({
        inter <- paste(paste0("ggplot2::ggplot(data = ", data_set, ", ", esthetique, ")"), geomm, facet, scaleX, scaleY, couleur, titre, sep = " + ")
        inter <- gsub(" \\+ delete", "", inter)
        inter <- gsub(" \\+  \\+ ", "", inter)
        if (fct_labeller != "delete") inter <- paste0(fct_labeller, ";", inter) else inter <- inter
      })

      output$graphe <- shiny::renderPlot({
        eval(parse(text = code()))
      })

      output$code_display <- shiny::renderText({
        affich <- gsub("ggplot2::", "", code())
        affich <- paste0(affich, " + theme_light(base_size = 13)")
        affich <- gsub("\\+", "\\+\n   ", affich)
        gsub(";", "\n", affich)
      })

    })

  }

  shiny::shinyApp(ui, server)

}

