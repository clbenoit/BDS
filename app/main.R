## Import dependencies
box::use(
  shiny[bootstrapPage, div, moduleServer, NS,
        renderUI, tags, uiOutput, actionButton,
        selectizeInput, updateSelectizeInput,
        shinyOptions, bindCache, bindEvent,
        observe, observeEvent, req, fluidRow,
        p, icon, h2, column, textInput, span, 
        tagList, conditionalPanel, HTML, a, h4, showNotification,
        showModal, modalDialog, br, modalButton, removeModal],
  RSQLite[SQLite],
  DT[dataTableOutput, renderDataTable, datatable, JS],
  DBI[dbReadTable, dbConnect, dbGetQuery, dbExecute, dbExistsTable],
  bslib[bs_theme, page_navbar,
        nav_item, nav_menu, nav_panel, nav_spacer, sidebar],
  config[get],
  cachem[cache_disk],
  shiny.router[route_link, router_ui, route, router_server],
  shinyjs[useShinyjs],
  dplyr[`%>%`, select, mutate],
  shinyWidgets[sendSweetAlert],
  bsplus[bs_embed_tooltip, shiny_iconlink ],
)
## Import shiny modules
box::use(
  app/view/main_sidebar,
  app/logic/dataManager[DataManager],
  app/logic/validateInputs[validate_inputs],
)

link_author <- tags$a(
  icon("id-card"), "About the author",
  href = "https://clbenoit.github.io/portfolio/ ",
  target = "_blank"
)
link_doc <- tags$a(
  icon("book"), "Documentation",
  href = route_link("documentation")
)
link_github <- tags$a(
  icon("github"), "Source code",
  href = "https://github.com/clbenoit",
  target = "_blank"
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  useShinyjs()
  bootstrapPage(
    router_ui(
      route("main",
            div(style = "height:100vh;",
                page_navbar(id = ns("page_navbar"),
                            theme = bs_theme(bootswatch = "lumen",
                                             #bg = "#FCFDFD", # greeen
                                             #fg = "rgb(25, 125, 85)"),
                                             bg = "#FCFDFD",  ### pink
                                             #bg = "#7D8039",
                                             fg = "rgb(209,8,157)"),
                            sidebar = sidebar(#id = ns("main_sidebar"),
                                              open = TRUE,
                                              #title = "Filter observations",
                                              span(shiny::h6("Run name",
                                                      shiny_iconlink(name = "info-circle") %>%
                                                        bs_embed_tooltip("format: 250312_RUNNAME"), style = "text-align: center;")),
                                              textInput(ns("run"), label = NULL),
                                              span(shiny::h6("BDS Number",
                                                             shiny_iconlink(name = "info-circle") %>%
                                                               bs_embed_tooltip("format: BDS-1234567891-12"), style = "text-align: center;")),
                                              textInput(ns("bds"), label = NULL),
                                              actionButton(ns("add"),
                                                           "Add Entry",
                                                           class = "btn btn-primary")),
                            title = "My BDS Number",
                            #header = "HEADER !!",
                            nav_panel(
                              #title = "Explore",
                              title = NULL,
                              dataTableOutput(ns("table"))
                            ),
                            nav_spacer(),
                            nav_menu(
                              title = "Links", align = "right",
                              nav_item(link_github),
                              nav_item(link_author),
                              nav_item(link_doc)
                            )
                          )
                ),
      ),
      route("documentation",
            tagList(
              div(class = "padding",
                  a("Go back to the app", href = route_link("main"))
              ),
              div(class = "padding",
                  #shiny::includeMarkdown("app/static/md/documentation.md")
              )
            )
    ),
    # footer = HTML(
    #   '<footer>
    #                 <!-- SVG image with a clickable link -->
    #                  <a href="https://www.appsilon.com/" target="_blank">
    #                  <img src="appsilon.svg"></img>
    #                  </a>
    #                 <!-- Text on the right side of the footer -->
    #                 <span> Copyright <a href="https://observation-international.org" target="_blank">
    #                   Observation International</a> 2024
    #                 </span>
    #                </footer>')
    ),
    # footer = HTML(
    #   '<footer>
    #                 <!-- SVG image with a clickable link -->
    #                  <a href="https://www.appsilon.com/" target="_blank">
    #                  <img src="appsilon.svg"></img>
    #                  </a>
    #                 <!-- Text on the right side of the footer -->
    #                 <span> Copyright <a href="https://observation-international.org" target="_blank">
    #                   Observation International</a> 2024
    #                 </span>
    #                </footer>')
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    Sys.setenv(R_CONFIG_ACTIVE = "devel")
    config <- get()
    
    if(!file.exists(config$db_path)){
      dir.create("inst/extdata", recursive = TRUE)
      message("Created new SQLite database at: ", config$db_path)
    } else {
      message("Opening existing SQLite database at: ", config$db_path)
    }
    con <- dbConnect(SQLite(), config$db_path)
    
    if (get("cache_directory") == "tempdir") {
      tempdir <- tempdir()
      dir.create(file.path(tempdir, "cache"))
      print(paste0("using following cache directory : ",
                   file.path(tempdir, "cache")))
      shinyOptions(cache = cache_disk(file.path(tempdir, "cache")))
    } else {
      print(paste0("using following cache directory : ",
                   get("cache_directory")))
      shinyOptions(cache = cache_disk(get("cache_directory")))
    }
    
    router_server("main")
  
    DataManager <- DataManager$new()
    
    observeEvent(DataManager$data$refresh_count, {
      DataManager$loadDB(con)
    })
    
    output$table <- renderDataTable({
      req(DataManager$data$run_bds_match())
      print(utils::head(DataManager$data$run_bds_match()))
      table <- DataManager$data$run_bds_match() %>% select(c("RUN","BDS_NUMBER")) %>% 
        mutate(Delete = paste0('<button id="', ns("delete_"), RUN, 
                               '" class="btn btn-danger btn-sm">Delete</button>'))      

      datatable(table, escape = FALSE, selection = "none", rownames = FALSE, 
                options = list(dom = 't', paging = FALSE),
                callback = JS(paste0("
              table.on('click', 'button', function() {
                var data = table.row($(this).parents('tr')).data();
                console.log('Button Clicked for RUN:', data[0]);  // Debugging
                Shiny.setInputValue('", ns("delete_id"), "', data[0], {priority: 'event'});
              });
            ")))  
      
    })

    observeEvent(input$add, {
      new_run <- input$run
      new_bds <- input$bds
      
      print("Adding new BDS number")
      
      # Validate the inputs format
      if (validate_inputs(new_run, new_bds)) {
        
        # Check if the combination of RUN and BDS_NUMBER already exists
        existing_entry <- dbGetQuery(DataManager$con, 
                                     "SELECT COUNT(*) FROM runs WHERE RUN = ? OR BDS_NUMBER = ?",
                                     params = list(new_run, new_bds))
        
        # If entry already exists, show a notification and stop further processing
        if (existing_entry$`COUNT(*)` > 0) {
          sendSweetAlert(
            session = session,
            title = HTML('<span style="color: red; font-weight: bold;">
                      <i class="fa fa-triangle-exclamation" style="margin-right: 10px;"></i>
                      Duplicate Entry Found!
                    </span>'),
            text = HTML("Either RUN and BDS Number already exists in the database."),
            type = "error", html = TRUE
          )
        } else {
          print(paste("Adding : ", input$delete_id))
          dbExecute(DataManager$con,
                    "INSERT INTO runs (RUN, BDS_NUMBER) VALUES (?, ?)",
                    params = list(new_run, new_bds))
          
          DataManager$data$refresh_count <- DataManager$data$refresh_count +  1
          
          # Success message if the entry was added
          sendSweetAlert(
            session = session,
            title = 'BDS number added successfully !',
            type = "success", html = TRUE
          )
        }
        
      } else {
        # Show an error message if input format is invalid
        sendSweetAlert(
          session = session,
          title = HTML('<span style="color: red; font-weight: bold;">
                    <i class="fa fa-triangle-exclamation" style="margin-right: 10px;"></i>
                    Invalid Format!
                   </span>'),
          text = HTML("RUN: 250312_RUNNAME <br> BDS: BDS-1234567891-12"),
          type = "error", html = TRUE
        )
      }
    })
    
    observeEvent(input$delete_id, {
      showModal(
        modalDialog(
          title = "Confirm Deletion",
          "Are you sure you want to delete this entry?",
          footer = tagList(
            actionButton(ns("confirm_delete"), "Yes", class = "btn btn-danger"),
            modalButton("Cancel")
          )
        )
      )
    })
    
    observeEvent(input$confirm_delete, {
      removeModal()
      req(input$delete_id)
      
      print(paste("Removing : ", input$delete_id))
      
      # Remove the row from database
      dbExecute(DataManager$con, 
                "DELETE FROM runs WHERE RUN = ?", 
                params = list(input$delete_id))
      
      # Refresh the table
      DataManager$data$refresh_count <- DataManager$data$refresh_count +  1
      
      showNotification("BDS number deleted successfully", type = "message")
    })
    
    
  })
}
