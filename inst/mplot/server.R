library(shiny)
data = shiny.data.in
af.res = af.res
lvp.res = lvp.res

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
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
    af.res <<- af(full.model)
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
})