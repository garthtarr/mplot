library(shiny)
data = shiny.data.in
chois = unlist(paste('\'',names(data),'\'',"=",'\'',names(data),'\'',sep=""))

reactiveSvg <- function (outputId) {
  HTML(paste("<div id=\"", 
             outputId, 
             "\" class=\"shiny-network-output\"><svg /></div>",
             sep=""))
}




# Define UI for dataset viewer application
shinyUI(
  fluidPage(
    headerPanel("Model stability/selection or variable inclusion plots"),
    sidebarPanel(
      conditionalPanel(
        condition = "input.tabs1 == 2",
        #p("You can use the settings below to adjust the y-axis 
        #  OR you can click and drag on the plot to highlight a 
        #  region that you'd like to zoom in on."),
        #numericInput("lvp.y.max", "Maximum for the y axis:", round(max(lvp.res$lk$LL))+5),
        #numericInput("lvp.y.min", "Minimum for the y axis:", round(min(lvp.res$lk$LL))-5),
        radioButtons("highlight",label="Highlight models with which variable?", 
                     choices=names(data)[names(data)!=yname]),
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
                    choices=names(data),multiple=TRUE),
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
      )
    ),
    # add a button to run pairedVis such that it opens in a new window
    #selectInput("yvar", "Response variable:", choices = names(data)),
    mainPanel(
      tabsetPanel(
        tabPanel("Scatterplot",
                 includeHTML("scatterplot.js"),
                 reactiveSvg(outputId = "scatterplot"),
                 br(),br(),
                 dataTableOutput(outputId="outputTable"),
                 value=1),
        tabPanel("Model stability", list(htmlOutput("lvp.gvis"),
                                         #plotOutput("lvp.classic"),
                                         htmlOutput("lvp.boot.gvis")),
                 value=2),
        # aslo think about perhaps adding in the MELCC (maximum
        # enveloping lower convex curve)
        tabPanel("Fence", list(htmlOutput("af.gvis"),
                               br(),br(),
                               verbatimTextOutput("af.verb")),
                 value=3),
        tabPanel("Variable inclusion", list(htmlOutput("vip.gvis")),
                 value=4),
        id="tabs1")
    ) 
  )
)


