library(shiny)
library(shinyjs)

ui <- function(request) {
  fluidPage(
    ######################################
    # Lines to be added for any shinyapp #
    ######################################
    useShinyjs(),
    tags$head(
      tags$script(HTML("
        function sendToPARENT(status, score) {
          window.parent.postMessage({ 
            status: status,
            score: score 
          }, '*');
        }
        Shiny.addCustomMessageHandler('sendToPARENT',
          function(message) {
            sendToPARENT(message.status, message.score);
          }
        );
      "))
    ),
    ######################################
    # Lines to be added for any shinyapp #
    ######################################
    mainPanel(
      h3(textOutput("instruction")),
      radioButtons(
        "choices", "",
        choices = as.character(sample(1:4)),
        selected = character(0)
      ),
      actionButton("submit", "Submit"),
      verbatimTextOutput("score")
    )
  )
}

server <- function(input, output, session) {
  score <- NA
  task <- sample(c("smallest", "largest"), 1)
  numbers <- sample(1:4)
  correct_answer <- ifelse(
    task == "smallest",
    min(numbers),
    max(numbers)
  )
  
  output$instruction <- renderText({
    paste("Choose the", task, "number:")
  })
  
  observeEvent(input$submit, {
    runjs("$('#choices input[type=radio]').prop('disabled', true);")
    
    if (is.null(input$choices) || input$choices == "") {
      score <<- 0
    } else if (as.numeric(input$choices) == correct_answer) {
      score <<- 100
    } else {
      score <<- 50
    }
    
    output$score <- renderText({
      ifelse(
        score == 100,
        paste0(
          "Correct! The ", task, " number is ",
          correct_answer, ". Grade: ", score
        ),
        paste0(
          "Incorrect! The ", task, " number is ",
          correct_answer, ". Grade: ", score
        )
      )
    })
    
    ######################################
    # Lines to be added for any shinyapp #
    ######################################
    if (score == 100) {
      session$sendCustomMessage(
        type = 'sendToPARENT',
        message = list(status = "passed", score = score)
      )
    } else if (score == 50) {
      session$sendCustomMessage(
        type = 'sendToPARENT',
        message = list(status = "incomplete", score = score)
      )
    } else if (score == 0) {
      session$sendCustomMessage(
        type = 'sendToPARENT',
        message = list(status = "failed", score = score)
      )
    }
    ######################################
    # Lines to be added for any shinyapp #
    ######################################
  })
}

shinyApp(ui, server)
