library(shiny)

shinyUI(
  fluidPage(
    includeCSS("shinychat.css"),
    includeScript("sendOnEnter.js"),

    fluidRow(
      tags$head(tags$title("Clustine")),
      div(style="padding: 10px 10px;",
          h1("Clustine"),
          h4("Let's write that query together")
      )
    ),

    fluidRow(
      div(class="chat-container", style="padding: 10px 10px;",
        uiOutput("chat")
      )
    ),

    fluidRow(
      column(10,
        textInput("entry", "", width ="100%")
      ),
      column(2,
        actionButton("send", "Send")
      )
    )
  )
)
