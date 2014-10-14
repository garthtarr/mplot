#' Model selection and stability curves
#' 
#' Opens a shiny GUI to investigate a range of model selection 
#' and stability issues
#' 
#' 
#' @param mf a fitted model.
#' @param vis (optional) an object of type vis created using the
#'   vis() function.
#' @param af (optional) an object of type af created using the af()
#'   function.
#' @export
#' @examples
#' n = 100
#' set.seed(11)
#' e = rnorm(n)
#' x1 = rnorm(n)
#' x2 = rnorm(n)
#' x3 = x1^2
#' x4 = x2^2
#' x5 = x1*x2
#' y = 1 + x1 + x2 + e
#' dat = round(data.frame(y,x1,x2,x3,x4,x5),2)
#' lm1 = lm(y~.,data=dat)
#' v1 = vis(lm1,n.cores=3)
#' af1 = af(lm1,n.cores=3)
#' mplot(lm1,vis=v1,af=af1)

mplot = function(mf,vis=NULL,af=NULL){
  # make the data globally accessible so shiny can access it
  # this may not be necessary, but I don't know how to pass
  # a (local) data frame through to the shiny server
  full.model = mf
  data =  model.frame(mf)
  if(class(af)=="af"){
    af.res = af
  } else {
    af.res = NULL
  }
  #else if(is.null(af)) { could run automatically?
  #  if(class(mf)=="lm"){
  #    cat("Adaptive fence in progress... \n")
  #    af.res = af(mf, n.cores = detectCores())
  #  } 
  #}
  if(class(vis)=="vis"){
    lvp.res = vis
  } else {
    lvp.res = NULL
  }
  #else if(is.null(vis)){ could run automatically?
  #  if(class(mf)=="lm"){
  #    cat("Boostrapping for the model selection plots...")
  #    lvp.res = vis(mf,B=B,nvmax = af.res$k.range$k.max)
  #  } 
  
  #chois = unlist(paste('\'',names(data),'\'',"=",'\'',names(data),'\'',sep=""))
  
  reactiveSvg <- function (outputId) {
    HTML(paste("<div id=\"", 
               outputId, 
               "\" class=\"shiny-network-output\"><svg /></div>",
               sep=""))
  }
  
  shinyApp(
    ui=fluidPage(
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
                 ),
                 conditionalPanel(
                   condition = "input.tabs1 == 4",
                   "Variable inclusion plots"
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
        column(9,
               tabsetPanel(
                 tabPanel("Scatterplot",
                          includeHTML(system.file('mplot','scatterplot.js', package='mplot')),
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
    ),
    server = function(input, output) {
      
      output$caption <- renderText({
        input$caption
      })
      
      output$pairs <- renderPlot({
        pairs(data)
      })
      
      output$bplot <- renderPlot({
        boxplot(data)
      })
      
      output$lvp.classic <- renderPlot({
        if(input$classic.mode){
          which.var = input$highlight
          find.var = function(x,which.var){
            is.element(which.var,x)
          }
          cols = apply(lvp.res$model,1,find.var,which.var=which.var)
          plot(lvp.res$lk$LL~lvp.res$lk$k,col=(as.numeric(cols)*3+1),pch=19,
               ylab="-2*log-likelihood",xlab="Number of parameters",
               ylim = c(input$lvp.y.min,input$lvp.y.max))
        } else NULL
      })
      
      output$lvp.gvis <- renderGvis({
        if(input$boot_lvp=="No"){
          plot(lvp.res,shiny=TRUE,
               highlight=input$highlight,which="lvk")
        } else if(input$boot_lvp=="Yes") {
          plot(lvp.res,shiny=TRUE,
               highlight=input$highlight,which="boot")
        }
      })
      
      output$vip.gvis <- renderGvis({
        plot(lvp.res,shiny=TRUE,which="vip")
      })
      
      output$afUI = renderUI({
        if(is.null(af.res)){
          actionButton(inputId="test", label="test", icon = NULL)
        }
      })
      
      af.run <- reactive({
        if(is.null(af.res)){
          af.res = af(full.model)
        }
      })
      
      output$af.run.gvis <- renderGvis({
        input$test
        af.run()
        plot(af.res,html.only=TRUE,best.only=input$bo)
      })
      
      output$af.run.verb = renderPrint({
        input$test
        summary(af.res)
      })
      
      AF_STAT <- reactive({is.null(af.res)})
      
      output$af.gvis <- renderGvis({
        input$test
        if(!is.null(af.res)){
          plot(af.res,html.only=TRUE,best.only=input$bo)
        } else return(NULL)
      })
      
      output$af.verb = renderPrint({
        summary(af.res)
      })
      
      # The output$view depends on both the databaseInput reactive expression
      # and input$obs, so will be re-executed whenever input$dataset or 
      # input$obs is changed. 
      output$view = renderDataTable(data, 
                                    options = list(pageLength = 20,
                                                   lengthMenu = list(c(20, 50, -1), 
                                                                     c('20', '50', 'All')),
                                                   searching = FALSE))
      
      ### Scatterplot Tab
      choices<-reactive({
        input$choose_vars
      })
      factor_var<-reactive({
        input$factor_var
      })
      output$scatterplot<-reactive({
        if(input$factor_var_logical==1){
          splot.data = as.matrix(na.omit(data[,c(factor_var(),choices())]))
          colnames(splot.data)[1]="factor"
          splot.data
        } else {
          as.matrix(na.omit(data[,c(choices())]))
        }
      })
      output$outputTable <- renderDataTable({
        if(input$table_data_logical==1){
          displayDF <- as.matrix(data) # baseData$df #data sent to d3.js 
          n=dim(displayDF)[1]
          dfFilter <- input$mydata[1:n] # passed from the web interface
          if (is.null(dfFilter)){
            # means no selection has been made
            dfFilter = rep(TRUE,n)
          }
          displayDF <- as.data.frame(cbind(names=row.names(displayDF), 
                                           displayDF))
          dfFilter[dfFilter==''] = TRUE
          dfFilter[dfFilter=='greyed'] = FALSE
          if(input$factor_var_logical==0){
            # don't need to worry about adding the factor variable in
            if(input$table_data_vars==0){
              return(as.matrix(displayDF[dfFilter == TRUE,choices(),drop=FALSE]))
            } else if(input$table_data_vars==1){
              return(as.matrix(displayDF[dfFilter == TRUE,,drop=FALSE]))
            }
          } else if(input$factor_var_logical==1){
            if(input$table_data_vars==0){
              return(as.matrix(displayDF[dfFilter == TRUE,
                                         c(factor_var(),choices()),
                                         drop=FALSE]))
            } else if(input$table_data_vars==1){
              return(as.matrix(displayDF[dfFilter == TRUE,,drop=FALSE]))
            }
          }
        } else {
          return(NULL)
        }
      }, 
      options = list(pageLength = 20,
                     lengthMenu = list(c(20, 50, -1), c('20', '50', 'All')),
                     searching = FALSE)
      )
    }
  )
}

