library(shiny)
data = shiny.data.in
af.res = af.res
lvp.res = lvp.res

chois = unlist(paste('\'',names(data),'\'',"=",'\'',names(data),'\'',sep=""))

reactiveSvg <- function (outputId) {
  HTML(paste("<div id=\"", 
             outputId, 
             "\" class=\"shiny-network-output\"><svg /></div>",
             sep=""))
}

## separater:
## 


# Define UI for dataset viewer application
shinyUI(fluidPage(
  titlePanel(""),
  fluidRow(
    column(3,
           wellPanel(
             conditionalPanel(
               condition = "input.tabs1 == 2",
               #p("You can use the settings below to adjust the y-axis 
               #  OR you can click and drag on the plot to highlight a 
               #  region that you'd like to zoom in on."),
               #numericInput("lvp.y.max", "Maximum for the y axis:", round(max(lvp.res$lk$LL))+5),
               #numericInput("lvp.y.min", "Minimum for the y axis:", round(min(lvp.res$lk$LL))-5),
               radioButtons("highlight",label="Highlight models with which variable?", 
                            choices=names(coef(full.model))[!names(coef(full.model))=="(Intercept)"]),
               #p("Not yet implemented:"),
               #radioButtons("highlight.model",label="Highlight which 'optimal' model?", 
               #             choices=c("Fence" = "fence",
               #                       "Forwards AIC" = "fAIC",
               #                       "Backwards AIC" = "bAIC",
               #                       "Forwards BIC" = "fBIC",
               #                       "Backwards BIC" = "bBIC")),
               selectInput("boot_lvp","Bootstrap?",choices=c("No","Yes")),
               conditionalPanel(
                 condition = "input.boot_lvp == 'Yes'",
                 p("Note that only models that have a non-zero bootstrap
              probability are shown.")
               )
               #selectInput("classic.mode",label="Classic mode? (Used in development)", 
               #            choices=c("FALSE","TRUE"))
             ),
             conditionalPanel(
               condition = "input.tabs1 == 1",
               selectInput(inputId="choose_vars",label="Select variables to plot:",
                           choices=names(data),multiple=TRUE,selected=names(data)[1:3]),
               radioButtons("factor_var_logical", label="Is there a factor variable?",
                            choices = c("No" = 0,
                                        "Yes" = 1)),
               conditionalPanel(
                 condition = "input.factor_var_logical == 1",
                 selectInput(inputId="factor_var",label="Factor variable:",
                             choices=names(data),multiple=FALSE,selected = NULL)
               ),
               radioButtons("table_data_logical", label="Table of data?",
                            choices = c("No" = 0,
                                        "Yes" = 1)),
               conditionalPanel(
                 condition = "input.table_data_logical == 1",
                 selectInput(inputId="table_data_vars",label="Include all variables in table?",
                             choices=c("No" = 0,
                                       "Yes" = 1))
               )
             ),
             conditionalPanel(
               condition = "input.tabs1 == 3",
               uiOutput("afUI")
             ),
             conditionalPanel(
               condition = "input.tabs1 == 3",
               radioButtons("bo",label="Best Only",choices=c("TRUE"=TRUE,
                                                             "FALSE" = FALSE))
             )
           ),
           wellPanel(
             icon("warning"),
             tags$small("The mplot package is under active development."),
             tags$small("Report issues here: "),
             HTML(paste("<a href=http://github.com/garthtarr/mplot/issues>")),
             icon("github"),
             HTML(paste("</a>"))
           )
    ),
    # add a button to run pairedVis such that it opens in a new window
    #selectInput("yvar", "Response variable:", choices = names(data)),
    column(9,
           tabsetPanel(
             tabPanel("Scatterplot",
                      includeHTML("scatterplot.js"),
                      reactiveSvg(outputId = "scatterplot"),
                      br(),br(),
                      dataTableOutput(outputId="outputTable"),
                      value=1),
             tabPanel("Model stability", list(htmlOutput("lvp.gvis")#,
                                              #plotOutput("lvp.classic"),
                                              #htmlOutput("lvp.boot.gvis")
             ),
             value=2),
             # aslo think about perhaps adding in the MELCC (maximum
             # enveloping lower convex curve)
             tabPanel("Fence", 
                      if(!is.null(af.res)){
                        list(htmlOutput("af.gvis"),
                             br(),br(),
                             verbatimTextOutput("af.verb"))
                      } else {
                        list(htmlOutput("af.run.gvis"),
                             br(),br(),
                             verbatimTextOutput("af.run.verb"))
                      },
                      value=3),
             tabPanel("Variable inclusion", list(htmlOutput("vip.gvis")),
                      value=4),
             id="tabs1")
    )
  )
))


