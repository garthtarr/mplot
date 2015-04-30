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
#' \dontrun{
#' v1 = vis(lm1,n.cores=3)
#' af1 = af(lm1,n.cores=3)
#' mplot(lm1,vis=v1,af=af1)
#' }
#' 


mplot = function(mf,...){
  full.model = mf
  input_list <- list(...)
  af.res = af.res.screened = NULL
  lvp.res = lvp.res.screened = NULL
  for(j in 1:length(input_list)){
    if(class(input_list[[j]])=="af"){
      if(input_list[[j]]$screen){
        af.res.screened = input_list[[j]]
      } else {
        af.res = input_list[[j]]
      }
    }
    if(class(input_list[[j]])=="vis"){
      if(input_list[[j]]$screen){
        lvp.res.screened = input_list[[j]]
      } else {
        lvp.res = input_list[[j]]
      }
    }
    if(class(input_list[[j]])=="bglmnet"){
      if(input_list[[j]]$screen){
        glmnet.res.screened = input_list[[j]]
      } else {
        glmnet.res = input_list[[j]]
      }
    }
  }
  
  shinyApp(
    ui=dashboardPage(
      dashboardHeader(title="mplot"),
      dashboardSidebar(
        sidebarMenu(id="lvp",
                    menuItem(text="Model stability",icon=icon("bar-chart"),
                             menuSubItem("Show plot",tabName = "lvp", selected=TRUE),
                             radioButtons("highlight",label="Highlight models with which variable?",
                                          choices=names(coef(full.model))[!names(coef(full.model))=="(Intercept)"]),
                             radioButtons("boot_lvp","Bootstrap?",
                                          choices=c("Yes","No"),
                                          selected = "Yes",
                                          inline=TRUE),
                             conditionalPanel(
                               condition = "input.boot_lvp == 'Yes'",
                               
                               box(width=12, background = "navy",
                                   ("Only models that have a non-zero bootstrap probability are shown.")
                               )
                               
                             ),
                             radioButtons("screen.lvp",label="Screen",
                                          choices = c("Yes"=TRUE,"No" = FALSE),
                                          selected = FALSE,
                                          inline=TRUE),
                             br()
                    ),
                    menuItem(text="Variable importance",icon=icon("sliders"),
                             menuSubItem("Show plot",tabName = "vip", selected=FALSE),
                             radioButtons("screen.vip",label="Screen",
                                          choices=c("FALSE" = FALSE, "TRUE"=TRUE)),
                             br()),
                    menuItem(text="Adaptive fence",icon=icon("area-chart"),
                             menuSubItem("Show plot",tabName = "af", selected=FALSE),
                             radioButtons("bo",label="Best Only",
                                          choices=c("TRUE"=TRUE, "FALSE" = FALSE)),
                             radioButtons("screen.af",label="Screen",
                                          choices=c("FALSE" = FALSE,"TRUE"=TRUE)),
                             br()),
                    menuItem(text="Bootstrapping glmnet",tabName="glmnet",icon=icon("tasks"),
                             menuSubItem("Show plot",tabName = "glmnet", selected=FALSE),
                             radioButtons("glmplot",label="Plot type",choices=c("Variables"="variables",
                                                                                "Models" = "models")),
                             conditionalPanel(
                               condition = "input.glmplot == 'models'",
                               sliderInput("plb",label="Minimum probability to be plotted",min=0,max=0.5,value=0.05),
                               radioButtons("highlight.glmnet",label="Highlight models with which variable?",
                                            choices=names(coef(full.model))[!names(coef(full.model))=="(Intercept)"])
                             ),
                             radioButtons("screen.glmnet",label="Screen",
                                          choices=c("FALSE" = FALSE, "TRUE"=TRUE)),
                             br()
                    )
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
                      htmlOutput("lvp.gvis"))),
          tabItem(tabName="vip", 
                  box(title = "Variable importance plot",
                      status="info",
                      solidHeader = TRUE, width=12,
                      htmlOutput("vip.gvis"))),
          tabItem(tabName="af",
                  box(title = "Adaptive fence", status="warning",
                      solidHeader = TRUE, width=12,
                      htmlOutput("af.gvis")),
                  box(title = "R output", status = "warning",
                      solidHeader = TRUE, collapsible = TRUE,
                      width=12, collapsed = FALSE,
                      verbatimTextOutput("af.verb"))),
          tabItem(tabName="glmnet", 
                  box(title = "Bootstrapping glmnet",
                      status = "success",solidHeader = TRUE,
                      width=12, 
                      htmlOutput("glmnet.gvis")))
        )
      )
    ),
    shinyServer(function(input, output) {
      
      output$lvp.gvis <- renderGvis({
        if(input$screen.lvp){
          lvp.data = lvp.res.screened
        } else {
          lvp.data = lvp.res
        }
        if(input$boot_lvp=="No"){
          plot(lvp.data,shiny=TRUE,
               highlight=input$highlight,which="lvk")
        } else if(input$boot_lvp=="Yes") {
          plot(lvp.data,shiny=TRUE,
               highlight=input$highlight,which="boot")
        }
      })
      
      output$vip.gvis <- renderGvis({
        if(input$screen.vip){
          lvp.data = lvp.res.screened
        } else {
          lvp.data = lvp.res
        }
        plot(lvp.data,shiny=TRUE,which="vip")
      })
      
      output$af.gvis <- renderGvis({
        input$test
        if(input$screen.af){
          af.data = af.res.screened
        } else {
          af.data = af.res
        }
        if(!is.null(af.data)){
          plot(af.data,html.only=TRUE,best.only=input$bo)
        } else return(NULL)
      })
      
      output$af.verb = renderPrint({
        if(input$screen.af){
          af.data = af.res.screened
        } else {
          af.data = af.res
        }
        summary(af.data)
      })
      
      ### Bootstrapping glmnet ###
      output$glmnet.gvis <- renderGvis({
        if(input$screen.glmnet){
          glmnet.data = glmnet.res.screened
        } else {
          glmnet.data = glmnet.res
        }
        if(!is.null(glmnet.data)){
          plot(glmnet.data, html.only=TRUE, which=input$glmplot,
               plb=input$plb, highlight=input$highlight.glmnet)
        } else return(NULL)
      })
      
    }
    )
  )
}

