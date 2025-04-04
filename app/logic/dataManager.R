box::use(
  R6[R6Class],
  shiny[reactiveValues, observeEvent, shinyOptions, req, reactive],
  RSQLite[SQLite],
  DBI[dbReadTable, dbConnect, dbGetQuery, dbExistsTable, dbExecute],
  #dplyr[`%>%`, filter],
  stats[setNames],
  shinybusy[remove_modal_spinner, show_modal_spinner]
)

#' @export
DataManager <- R6::R6Class(
  classname = "DataManager",
  public = list(
    con = NULL,
    data = reactiveValues(
       run_bds_match = NULL,
       #refresh_count = NULL,
       refresh_count = 0
    ),
    loadDB = function(con) {
      print("inside load DB")
      self$con <- con
      shinybusy::show_modal_spinner(
        spin = "double-bounce",
        color = "#112446",
        text = "Loading database metadata")
      
      print("oki")
      
      # If entry does not exist, insert the new entry
      if(!dbExistsTable(con, "runs")){
        dbExecute(con, "
              CREATE TABLE runs (
              ID INTEGER PRIMARY KEY AUTOINCREMENT,
              RUN TEXT NOT NULL,
              BDS_NUMBER TEXT NOT NULL
              )
            ")
      }
      
      #if(DBI::dbExistsTable(con, "runs")){
        self$data$run_bds_match <- reactive({
          dbGetQuery(con, "SELECT * FROM runs")
        })
      #}
      remove_modal_spinner()
    }
  )
)
