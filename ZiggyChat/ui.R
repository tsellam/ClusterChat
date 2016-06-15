library(shiny)

shinyUI(
  fillPage(padding = 10,
    includeCSS("shinychat.css"),
    includeScript("sendOnEnter.js"),

    fillRow(
        div(
          h1("Clustine"),
          h4("Let's write that query together")
      ), height="90px"
    ),

    fillRow(
      div(id="chat-container",
        uiOutput("chat")
      ), height="70%"
    ),

    fillRow(
      fillCol(
        textInput("entry", "", width ="100%")
      ),
      fillCol(
        actionButton("send", "Send")
      ), height="50px"
    )
  )
)
