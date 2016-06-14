library(shiny)

shinyUI(
  bootstrapPage(
    includeCSS("shinychat.css"),

    includeScript("sendOnEnter.js"),

    div(class = "container-fluid",

      div(class = "row-fluid",

          tags$head(tags$title("Clustine")),

          div(class="span6", style="padding: 10px 0px;",
              h1("Clustine"),
              h4("Let's write that query together")
          )

      ),

      div(class = "row-fluid",

        mainPanel(

          uiOutput("chat"),

          fluidRow(
            div(class="span10",
              textInput("entry", "")
            ),
            div(class="span2 center",
                actionButton("send", "Send")
            )
          )

        )

        # sidebarPanel(
        #   textInput("user", "Your User ID:", value=""),
        #   tags$hr(),
        #   h5("Connected Users"),
        #   uiOutput("userList"),
        #   tags$hr()
        # )

      )
    )
  )
)
