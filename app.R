library(shiny)

dat <- read.csv2("lexeis.txt", stringsAsFactors=FALSE)
names(dat) <- c("eng", "gre")


scramble <- function(x) {
  paste(sample(strsplit(x, "")[[1]]), collapse="")
}

ui <- fluidPage(
  tags$script('
    $(document).on("keydown", function (e) {
      if (e.keyCode==13) Shiny.onInputChange("spCheck", [e.keyCode, e.timeStamp]);
    });
  '),
  titlePanel("Kathryn's english game"),
  tabsetPanel(
    tabPanel("Spelling",
      htmlOutput("spQuestion", style="border: solid; border-width: 1pt; margin:1em; padding: 1em"),
      fluidRow(
        column(4,
          textInput("spReply", "Write the word/phrase in English:")
        ),
        column(4,
          htmlOutput("spResult", style="border: solid; border-width: 1pt; margin:1em; padding: 1em"),
          htmlOutput("spScore", style="margin:1em; padding: 1em"),
          actionButton("spGiveUp", "I give up", width="100%")
        ),
        column(4,
          actionButton("spNext", "Next", width="8em")
        )
      )
    ),
    tabPanel("Translation",
      fluidRow(
        column(6, htmlOutput("trQuestion", style="border: solid; border-width: 1pt; margin:1em; padding: 1em")),
        column(6, 
          htmlOutput("trResult", style="border: solid; border-width: 1pt; margin:1em; padding: 1em"),
          fluidRow(
            column(6, htmlOutput("trScore", style="margin:1em; padding: 1em")),
            column(6, actionButton("trNext", "Next", width="8em"))
          )
        )
      ),
      fluidRow(
        column(3, actionButton("tr1", "1", width="100%")),
        column(3, actionButton("tr2", "2", width="100%")),
        column(3, actionButton("tr3", "3", width="100%")),
        column(3, actionButton("tr4", "4", width="100%"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  spCounter <- 0
  spCur <- 0
  spFoundIt <- FALSE
  spScore <- reactiveValues(correct = 0, wrong = 0)

  trCur <- 0
  trChoices <- c(0,0,0,0)
  trFoundIt <- FALSE
  trScore <- reactiveValues(correct = 0, wrong = 0)
  trReply <- reactiveVal(NULL)
  trResultStr <- reactiveVal("")

  output$spQuestion <- renderUI({
    input$spNext
    spCur <<- sample(1:nrow(dat), 1)
    HTML(sprintf("What is the English word/phrase for: 
        <h3 style=\"display:inline\">%s</h3> ??<br/>
        The word/phrase in English, with scrambled letters: 
        <h3 style=\"display:inline\">%s</h3><br/>", 
      dat$gre[spCur], scramble(dat$eng[spCur])))
  })
  
  output$spResult <- renderUI({
    input$spCheck
    input$spNext
    if (trimws(isolate(input$spReply))=="" || spCounter!=input$spNext) {
      spCounter <<- input$spNext
      return("")
    } else {
      if (tolower(isolate(input$spReply))==tolower(dat$eng[spCur])) {
        if (!spFoundIt) {
          spFoundIt <<- TRUE
          spScore$correct <- isolate(spScore$correct) + 1
        }
        return("Correct! Well done!")
      } else {
        spScore$wrong <- isolate(spScore$wrong) + 1
        return("Wrong... Try again...")
      }
    }
  })
  
  observeEvent(input$spNext, {
    updateTextInput(session, "spReply", value="")
    spFoundIt <<- FALSE
    updateActionButton(session, "spGiveUp", "I give up")
  })
  
  observeEvent(input$spGiveUp, {
    spFoundIt <<- TRUE
    updateActionButton(session, "spGiveUp", label=sprintf("Correct is \"%s\"", dat$eng[spCur]))
  })
  
  output$spScore <- renderUI({
    HTML(sprintf("Correct answers: %s<br/>Wrong answers: %s", spScore$correct, spScore$wrong))
  })

  output$trQuestion <- renderUI({
    input$trNext
    trResultStr("")
    trReply(NULL)
    trFoundIt <<- FALSE
    trCur <<- sample(1:nrow(dat), 1)
    trChoices <<- sample(c(trCur, sample((1:nrow(dat))[-trCur], 3)))
    updateActionButton(session, "tr1", label=dat$gre[trChoices[1]])
    updateActionButton(session, "tr2", label=dat$gre[trChoices[2]])
    updateActionButton(session, "tr3", label=dat$gre[trChoices[3]])
    updateActionButton(session, "tr4", label=dat$gre[trChoices[4]])
    HTML(sprintf("Τι σημαίνει στα ελληνικά: 
        <h3 style=\"display:inline\">%s</h3> ??", 
      dat$eng[trCur]))
  })
  
  output$trResult <- renderUI({
    trResultStr()
  })

  output$trScore <- renderUI({
    HTML(sprintf("Correct answers: %s<br/>Wrong answers: %s", trScore$correct, trScore$wrong))
  })
  
  observeEvent(input$tr1, { trReply(1) })
  observeEvent(input$tr2, { trReply(2) })
  observeEvent(input$tr3, { trReply(3) })
  observeEvent(input$tr4, { trReply(4) })
  
  observe({
    if (!is.null(trReply())) {
      if (trCur == trChoices[trReply()]) {
        trResultStr("You're correct!")
        trFoundIt <<- TRUE
        trScore$correct <- isolate(trScore$correct) + 1
      } else {
        trResultStr("You're wrong...")
        trScore$wrong <- isolate(trScore$wrong) + 1
      }
    }
  })

}

shinyApp(ui = ui, server = server)
