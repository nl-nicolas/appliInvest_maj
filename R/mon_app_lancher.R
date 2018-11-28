#' Lancer l'application
#'
#' Permet de lancer l'application
#'
#' @importFrom shiny runApp
#'
#'@export

shiny_mon_app <- function(){
  appDir <- system.file("mon_app", package = "appliInvest")
  shiny::runApp(port=3838,host = "0.0.0.0",appDir, display.mode = "normal")}



#' Lancer l'application test
#'
#' Permet de lancer l'application test
#'
#' @importFrom shiny runApp
#'
#'@export

shiny_mon_app_test <- function(){
  appDir <- system.file("mon_app_test", package = "appliInvest")
  shiny::runApp(port=3838,host = "0.0.0.0",appDir, display.mode = "normal")}


#' Lancer l'application test dep
#'
#' Permet de lancer l'application test dep
#'
#' @importFrom shiny runApp
#'
#'@export

shiny_mon_app_dep <- function(){
  appDir <- system.file("mon_app_test2", package = "appliInvest")
  shiny::runApp(port=3838,host = "0.0.0.0",appDir, display.mode = "normal")}

#' Lancer l'application test dep2
#'
#' Permet de lancer l'application test dep2
#'
#' @importFrom shiny runApp
#'
#'@export

shiny_mon_app_dep2 <- function(){
  appDir <- system.file("mon_app_dep_ss_update", package = "appliInvest")
  shiny::runApp(port=3838,host = "0.0.0.0",appDir, display.mode = "normal")}