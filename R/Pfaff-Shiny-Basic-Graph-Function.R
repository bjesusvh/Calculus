##########################################################
###  Interactive Applet
###  The Graph of a Function
###  By Thomas J. Pfaff
###  For The Text Applied Calculus with R
##########################################################

## Packages

#install.packages("shiny")
#install.packages("Deriv")
library(shiny)
library(Deriv)

## Colors

MyRed <- "#be4242"

## Define functions

temp <- list(
  f1 = function(x) { x^2 },
  f2 = function(x) { sin(x) },
  f3 = function(x) { exp(x) },
  f4 = function(x) { x^3 -15*x +1 },
  f5 = function(x) { -x^4/4- x^3/3 + 7*x^2 +14*x - 40 }
)

FunctionList <- c(
  "f(x)=x^2" = "f1",
  "f(x)=sin(x)" = "f2",
  "f(x)=exp(x)" = "f3",
  "f(x)=x^3 -15*x +1" = "f4",
  "f(x)=x^4/4- x^3/3 + 7*x^2 +14*x - 40" = "f5"
)

## Graph x axis range

minX <- -5
maxX <- 5

## Define User Interface

ui <- fluidPage(

# App title

  titlePanel("Graphing Functions"),
  
  # Sidebar layout with input and output definitions ----
 
  sidebarLayout(
    sidebarPanel(
      selectInput("g1", label = "Function", choices = FunctionList),
      sliderInput("x", "x", min = minX, max = maxX, value = 0, step=0.1)
      ),

    # Main panel for graph

    mainPanel(
	 	plotOutput(outputId = "Graph")
    )
  )
)

## Define server information

server <- function(input, output, session) {

  # Create a reactive expression

  output$Graph <- renderPlot( {
    if (input$g1 == "f1") {i <- 1}
    if (input$g1 == "f2") {i <- 2}
    if (input$g1 == "f3") {i <- 3}
    if (input$g1 == "f4") {i <- 4}
    if (input$g1 == "f5") {i <- 5}

  # Create graph

  par(mar = c(5, 5, 2, 2))
  curve(temp[[i]](x), minX, maxX, xlab = "", ylab = "",
        lwd = 2, xaxs = "i", yaxs = "i", cex.axis = 1.5)
    title(ylab = "f(x)", cex.lab = 1.5, line = 4)
    title(xlab = "x", cex.lab = 1.5, line = 4)
    grid(NULL, NULL, col = "black")
    abline(h = 0, lwd = 1.5, col = "black")
    points(input$x, temp[[i]](input$x), col = "red", cex = 2, pch = 16)
    segments(input$x, 0, input$x, temp[[i]](input$x),
             lty = 2, col = MyRed, lwd = 2)
    segments(minX, temp[[i]](input$x), input$x, temp[[i]](input$x),
             lty = 2, col = MyRed, lwd = 2)
    axis(1, line = 2, at = input$x, label = input$x, col.axis = MyRed,
         cex.axis = 1.5, tcl = 2, lwd.ticks = 3, col.ticks = MyRed)
    axis(2, line = 2, at = temp[[i]](input$x), label = round(temp[[i]](input$x), 2),
         col.axis = MyRed, cex.axis = 1.5, tcl = 2, lwd.ticks = 3, col.ticks = MyRed)
  })
}

## run application

shinyApp(ui, server)

