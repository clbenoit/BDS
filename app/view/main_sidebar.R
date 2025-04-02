#' # app/view/main_sidebar.R
#' 
#' box::use(
#'   shiny[h3, moduleServer, NS, tagList, fluidRow,
#'         column, req, observe, reactive,
#'         observeEvent,  bindCache, selectizeInput,
#'         updateSelectizeInput, checkboxGroupInput,
#'         icon, dateRangeInput,
#'         updateDateRangeInput, br],
#'   dplyr[`%>%`, filter]
#' )
#' 
#' #' @export
#' ui <- function(id) {
#'   ns <- NS(id)
#'   tagList(
#'     fluidRow(
#' 
#'     )
#'   )
#' }
#' 
#' #' @export
#' server <- function(id, con, data, variables) {
#'   moduleServer(id, function(input, output, session) {
#'     
#'   })
#' }
