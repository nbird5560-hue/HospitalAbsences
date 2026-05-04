#' var_select UI Function
#'
#' @description \emph{mod_var_select.R} creates and builds infrastructure for the
#' variable selection checkboxes and level filtering in the Data Selection Panel.
#' This set of UI objects acts as variable-mappable filtration keys. See in-line
#' comments for more info.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_var_select_ui <- function(id, display_label, include_selections = TRUE) {
  ns <- NS(id)
  tagList(
    # Checkbox input to include variable
    checkboxInput(ns("include"), paste("Include", display_label), value = FALSE),
    # Allows for a modified call of this module to include a drop down input
    # for data filtration
    if (include_selections){
      conditionalPanel(
        condition = paste0("input['", ns("include"), "'] == true"),
        selectizeInput(
          inputId = ns("selections"),
          label = paste("Filter",display_label),
          selected ="Select All",
          choices = NULL,
          multiple = TRUE,
          # options dynamically updated via server
          options = list(placeholder = 'Select levels...')
        )
      )
    }
  )
}


#' var_select Server Functions
#'
#' @noRd
mod_var_select_server <- function(id, data_reactive, col_name) {
  moduleServer(id, function(input, output, session) {

    observe({
      req(data_reactive())
      df <- data_reactive()

      # Variable-mappable drop-down selection choices
      raw_choices <- sort(unique(df[[col_name]]))
      choices <- c("Select All", raw_choices)
      updateSelectizeInput(
        session,
        "selections",
        choices = choices,
        selected = "Select All",
        server = TRUE
      )
    })


    observeEvent(input$selections, {
      req(input$selections)
      current <- input$selections

      # Forces "Select All" option to only be selected alone
      if ("Select All" %in% current && length(current) > 1) {
        if (current[length(current)] == "Select All") { # "Select All" most recently clicked
          updateSelectizeInput(session, "selections", selected = "Select All")
        } else {
          updateSelectizeInput(session, "selections", selected = setdiff(current, "Select All"))
        }
      }
    }, ignoreNULL = TRUE)


    return(list(
      include = reactive({input$include}), # If checkbox is marked to include variable
      selected_levels = reactive({ # Levels to be filtered with
        if (is.null(input$selections) || "Select All" %in% input$selections) {
          return(NULL) # Null used as sign not to filter
        }
        input$selections
        })
    ))
  })
}
