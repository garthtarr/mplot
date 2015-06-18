#' Model selection and stability curves
#' 
#' Opens a shiny GUI to investigate a range of model selection 
#' and stability issues
#' 
#' 
#' @param mf a fitted model.
#' @param ... objects of type vis or af.
#' @export
#' @import shiny
#' @import shinydashboard
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
#' \dontrun{
#' v1 = vis(lm1,n.cores=3)
#' af1 = af(lm1,n.cores=3)
#' mplot(lm1,v1,af1)
#' }
#' 


mplot = function(mf,...){
  full.model = mf
  input_list <- list(...)
  af.res = af.res.screened = NULL
  lvp.res = lvp.res.screened = NULL
  anyScreen = FALSE
  for(j in 1:length(input_list)){
    if(class(input_list[[j]])=="af"){
      if(input_list[[j]]$screen){
        af.res.screened = input_list[[j]]
        anyScreen=TRUE
      } else {
        af.res = input_list[[j]]
      }
    }
    if(class(input_list[[j]])=="vis"){
      if(input_list[[j]]$screen){
        lvp.res.screened = input_list[[j]]
        anyScreen=TRUE
      } else {
        lvp.res = input_list[[j]]
      }
    }
    #     if(class(input_list[[j]])=="bglmnet"){
    #       if(input_list[[j]]$screen){
    #         glmnet.res.screened = input_list[[j]]
    #         anyScreen=TRUE
    #       } else {
    #         glmnet.res = input_list[[j]]
    #       }
    #     }
  }
  
  ui = dashboardPage(
    dashboardHeader(title="mplot",disable = FALSE),
    dashboardSidebar(
      sidebarMenu(
        id="lvp",
        menuItem(text="Variable importance",icon=icon("sliders"),tabName = "vip"),
        menuItem(text="Adaptive fence",icon=icon("area-chart"),tabName="af"),
        conditionalPanel(condition = "input.lvp=='af'",
                         radioButtons("bo",label="Best Only",
                                      choices=c("TRUE"=TRUE, "FALSE" = FALSE),
                                      inline=TRUE)),
        menuItem(text="Model stability",icon=icon("bar-chart"),tabName="lvp"),
        conditionalPanel(condition = "input.lvp=='lvp'",
                         selectInput("highlight",label="Highlight models with:",
                                      choices=names(coef(full.model))[!names(coef(full.model))=="(Intercept)"]),
                                     #inline=FALSE), #TRUE), change back when wrapping is fixed
                         radioButtons("boot_lvp","Bootstrap?",
                                      choices=c("Yes","No"),
                                      selected = "Yes",
                                      inline=TRUE),
                         sliderInput(inputId = "min.prob", label="Min probability with label",
                                     min = 0, max=1, value = 0.3)),
        radioButtons("classic",label="Classic plots",
                     choices=c("Yes"=TRUE,"No" = FALSE),
                     selected=FALSE,inline=TRUE),
        conditionalPanel(condition = "input.classic=='TRUE' & input.lvp=='lvp'",
                         sliderInput(inputId = "max.circle", label="Max circle size",
                                     min = 0,max=0.5,value = 0.35),
                         radioButtons(inputId = "text",label="Add text",
                                      choices=c("Yes"=TRUE,"No" = FALSE),
                                      selected=FALSE,inline=TRUE)
        ),
        conditionalPanel(condition = "input.classic=='TRUE' & input.lvp=='lvp' & input.text=='TRUE'",
                         sliderInput(inputId = "srt", label="Label rotation",
                                     min = -45, max=45, value = -30)),
        radioButtons("screen",label="Screen",
                     choices = c("Yes"=TRUE,"No" = FALSE),
                     selected = FALSE,
                     inline = TRUE)
        #menuItem(text="Bootstrapping glmnet",tabName="glmnet",icon=icon("tasks"),
        #menuSubItem("Show plot",tabName = "glmnet", selected=FALSE),
        #radioButtons("glmplot",label="Plot type",choices=c("Variables"="variables",
        #                                                     "Models" = "models")),
        #conditionalPanel(
        #    condition = "input.glmplot == 'models'",
        #    sliderInput("plb",label="Minimum probability to be plotted",min=0,max=0.5,value=0.05),
        #    radioButtons("highlight.glmnet",label="Highlight models with which variable?",
        #                 choices=names(coef(full.model))[!names(coef(full.model))=="(Intercept)"])
        #   )
        #                   )
      ),
      br(),
      box(
        width = 12, background = "black",
        icon("warning"),
        ("The mplot package is under active development."),
        ("Report issues here: "),
        HTML(paste("<a href='http://github.com/garthtarr/mplot/issues' target='_blank'>")),
        icon("github"),
        HTML(paste("</a>"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName="lvp", 
                box(title = "Model stability plot",status="primary",
                    solidHeader = TRUE,width = 12,
                    conditionalPanel(condition = "input.classic=='FALSE'",
                                     htmlOutput("lvp.gvis")),
                    conditionalPanel(condition = "input.classic=='TRUE'",
                                     plotOutput("lvp.classic"))
                ),
                box(title = "R output", status = "primary",
                    solidHeader = TRUE, collapsible = TRUE,
                    width=12, collapsed = FALSE,
                    verbatimTextOutput("boot.verb"))),
        tabItem(tabName="vip", 
                box(title = "Variable importance plot",
                    status="info",
                    solidHeader = TRUE, width=12,
                    conditionalPanel(condition = "input.classic=='FALSE'",
                                     htmlOutput("vip.gvis")),
                    conditionalPanel(condition = "input.classic=='TRUE'",
                                     plotOutput("vip.classic")))
        ),
        tabItem(tabName="af",
                box(title = "Adaptive fence", status="warning",
                    solidHeader = TRUE, width=12,
                    conditionalPanel(condition = "input.classic=='FALSE'",
                                     htmlOutput("af.gvis")),
                    conditionalPanel(condition = "input.classic=='TRUE'",
                                     plotOutput("af.classic"))),
                box(title = "R output", status = "warning",
                    solidHeader = TRUE, collapsible = TRUE,
                    width=12, collapsed = FALSE,
                    verbatimTextOutput("af.verb")))#,
        #tabItem(tabName="glmnet", 
        #        box(title = "Bootstrapping glmnet",
        #        status = "success",solidHeader = TRUE,
        #        width=12, 
        #        htmlOutput("glmnet.gvis")))
      )
    )
  )
  
  server = function(input, output) {
    
    #### Model seleciton plot
    output$lvp.gvis <- googleVis::renderGvis({
      if(input$screen){ 
        lvp.data = lvp.res.screened 
      } else {
        lvp.data = lvp.res  
      }
      if(input$boot_lvp=="No"){
        plot(lvp.data,html.only=TRUE,
             highlight=input$highlight,which="lvk")
      } else if(input$boot_lvp=="Yes") {
        plot(lvp.data,html.only=TRUE,
             highlight=input$highlight,which="boot")
      }
    })
    output$lvp.classic <- renderPlot({
      if(input$screen){ 
        lvp.data = lvp.res.screened 
      } else {
        lvp.data = lvp.res 
      }
      if(input$boot_lvp=="No"){
        plot(lvp.data, highlight=input$highlight,
             which="lvk", classic=TRUE)
      } else if(input$boot_lvp=="Yes") {
        plot(lvp.data, highlight=input$highlight,
             which="boot", classic=TRUE, max.circle=input$max.circle,
             text=input$text, min.prob=input$min.prob, srt = input$srt)
      }
    })
    
    
    #### Variable importance plots
    output$vip.gvis <- googleVis::renderGvis({
      if(input$screen){ 
        lvp.data = lvp.res.screened 
      } else {
        lvp.data = lvp.res 
      }
      plot(lvp.data,html.only=TRUE,which="vip")
    })
    output$vip.classic <- renderPlot({
      if(input$screen){ 
        lvp.data = lvp.res.screened 
      } else {
        lvp.data = lvp.res 
      }
      plot(lvp.data,classic=TRUE,which="vip")
    })
    
    #### Adaptive fence plots
    output$af.gvis <- googleVis::renderGvis({
      if(input$screen){ 
        af.data = af.res.screened 
      } else {
        af.data = af.res 
      }
      if(!is.null(af.data)){
        plot(af.data,html.only=TRUE,best.only=input$bo)
      } else return(NULL)
    })
    output$af.classic <- renderPlot({
      if(input$screen){ 
        af.data = af.res.screened 
      } else {
        af.data = af.res 
      }
      if(!is.null(af.data)){
        plot(af.data,classic=TRUE,best.only=input$bo)
      } else return(NULL)
    })
    
    output$af.verb = renderPrint({
      if(input$screen){ 
        af.data = af.res.screened 
      } else {
        af.data = af.res 
      }
      summary(af.data)
    })
    
    output$boot.verb = renderPrint({
      if(input$screen){ 
        lvp.data = lvp.res.screened 
      } else {
        lvp.data = lvp.res 
      }
      print(lvp.data,min.prob=input$min.prob)
    })
    
    #     ### Bootstrapping glmnet ###
    #     output$glmnet.gvis <- googleVis::renderGvis({
    #       if(input$screen){
    #         glmnet.data = glmnet.res.screened
    #       } else {
    #         glmnet.data = glmnet.res
    #       }
    #       if(!is.null(glmnet.data)){
    #         plot(glmnet.data, html.only=TRUE, which=input$glmplot,
    #              plb=input$plb, highlight=input$highlight.glmnet)
    #       } else return(NULL)
    #     })
    
  }
  
  shinyApp(ui, server)
}

