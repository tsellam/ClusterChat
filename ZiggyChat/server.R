library(shiny)
source("wrap_clustine.R")


#######################################
# Initializes session-wide variables #
#######################################
r_objects <- reactiveValues(
  query = character(0),
  board_elements = list(),
  elements_type  = list(),
  n_elts = 0
)

###################
# Utils functions #
###################
update_chat <- function(user, new_text){
  new_content <- paste0(
    tags$span(class="username", tags$abbr(title=Sys.time(), user)),
    ": ",
    new_text
  )

  r_objects$n_elts <<- r_objects$n_elts + 1
  r_objects$board_elements[[r_objects$n_elts]] <<- new_content
  r_objects$elements_type[[r_objects$n_elts]]  <<- "text"
}

update_plot <- function(plot){
  update_chat('Clustine', "Here is the plot:")

  r_objects$n_elts <<- r_objects$n_elts + 1
  r_objects$board_elements[[r_objects$n_elts]] <<- renderPlot({plot})
  r_objects$elements_type[[r_objects$n_elts]]  <<- "plot"
}

update_table <- function(table){
  update_chat('Clustine', "Here is a sample from your selection:")

  r_objects$n_elts <<- r_objects$n_elts + 1
  r_objects$board_elements[[r_objects$n_elts]] <<- renderTable({table})
  r_objects$elements_type[[r_objects$n_elts]]  <<- "table"
}

reset_chat <- function(){
  r_objects$elements_type  <<- list()
  r_objects$board_elements <<- list()
  r_objects$n_elts <<- 0
}

####################
# Main server code #
####################
shinyServer(function(input, output, session) {

  # Renders the elements on chat window
  output$chat <- renderUI({

    if (r_objects$n_elts == 0) return()

    all_elements <- lapply(1:r_objects$n_elts, function(i){
      if (r_objects$elements_type[[i]] == "text"){
        tags$div(class="text",
          HTML(r_objects$board_elements[[i]])
        )

      } else if (r_objects$elements_type[[i]] == "plot"){
          tags$div(class="plot", r_objects$board_elements[[i]]
        )

      } else if (r_objects$elements_type[[i]] == "table"){
          tags$div(class="table",r_objects$board_elements[[i]]
        )
      }
    })

    tagList(all_elements)
  })


  # Updates when the button is pressed
  observe({
    if (input$send < 1){
      return()
    } else {
      isolate({
        # Updates UI
        update_chat("User", input$entry)
        updateTextInput(session, "entry", value="")
        # Updates query object
        r_objects$query <<- input$entry
      })
    }
  })

  # Triggers Clustine when a query comes in the queue
  observe({
    r_objects$query
    if (length(r_objects$query) > 0)
      isolate({
        answer <- clustine_react(r_objects$query)
        if (!is.null(answer$plot))  update_plot(answer$plot)
        if (!is.null(answer$table)) update_table(answer$table)
        update_chat("Clustine", answer$text)
        r_objects$query <<- character(0)

        if (answer$reset){
          cat("Reseting!")
          reset_chat()
          r_objects$query <<- "Zoom in 1"
        }

      })
  })

# Initializes the session
  isolate({
    update_chat("Clustine", "Welcome user. Shall we get to work?")
    #r_objects$query <<- "Zoom in 1"
  })
})