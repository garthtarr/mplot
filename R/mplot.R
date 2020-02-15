#' Model selection and stability curves
#'
#' Opens a shiny GUI to investigate a range of model selection
#' and stability issues
#'
#'
#' @param mf a fitted model.
#' @param ... objects of type `vis` or `af` or `bglmnet`.
#' @export
#' @import shiny
#' @import shinydashboard
#' @references Tarr G, Mueller S and Welsh AH (2018). mplot: An R Package for 
#'   Graphical Model Stability and Variable Selection Procedures. 
#'   Journal of Statistical Software, 83(9), pp. 1-28. doi: 10.18637/jss.v083.i09
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
#' lm1 = lm(y ~ ., data = dat)
#' \dontrun{
#' v1 = vis(lm1)
#' af1 = af(lm1)
#' bg1 = bglmnet(lm1)
#' mplot(lm1, v1, af1, bg1)
#' }
#'

mplot = function(mf, ...){
  full.model = mf
  input_list <- list(...)
  af.res = af.res.screened = NULL
  lvp.res = lvp.res.screened = NULL
  glmnet.res = glmnet.res.screened = NULL
  # to prevent the note:
  # mplot : server: no visible binding for global variable anyscreen
  anyscreen = NULL
  anyScreen = FALSE
  for (j in 1:length(input_list)) {
    if (class(input_list[[j]]) == "af") {
      if (input_list[[j]]$screen) {
        af.res.screened = input_list[[j]]
        anyScreen = TRUE
      } else {
        af.res = input_list[[j]]
      }
    }
    if (class(input_list[[j]]) == "vis") {
      if (input_list[[j]]$screen) {
        lvp.res.screened = input_list[[j]]
        anyScreen = TRUE
      } else {
        lvp.res = input_list[[j]]
      }
    }
    if (class(input_list[[j]]) == "bglmnet") {
      if (input_list[[j]]$screen) {
        glmnet.res.screened = input_list[[j]]
        anyScreen = TRUE
      } else {
        glmnet.res = input_list[[j]]
      }
    } 
  }
  
  ui = dashboardPage(
    dashboardHeader(title = "mplot", disable = FALSE),
    dashboardSidebar(
      sidebarMenu(
        id = "lvp",
        menuItem(text = "Variable inclusion", icon = icon("sliders"), tabName = "vip"),
        menuItem(text = "Adaptive fence", icon = icon("area-chart"), tabName = "af"),
        conditionalPanel(condition = "input.lvp=='af'",
                         radioButtons("bo", label = "Best Only",
                                      choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                                      inline = TRUE)),
        menuItem(text = "Model stability", icon = icon("bar-chart"), tabName = "lvp"),
        menuItem(text = "Bootstrap glmnet", icon = icon("line-chart"), tabName = "bglmnet"),
        conditionalPanel(
          condition = "input.lvp=='lvp' | input.lvp=='bglmnet'",
          selectInput("highlight", label = "Highlight models with:",
                      choices = if (lvp.res$use.glmulti) base::all.vars(stats::formula(full.model))[-1] else names(stats::coef(full.model))[!names(stats::coef(full.model)) == "(Intercept)"]),
          conditionalPanel(
            condition = "input.lvp=='lvp'",
            radioButtons("boot_lvp", "Bootstrap?",
                         choices=c("Yes", "No"),
                         selected = "Yes",
                         inline = TRUE)
            ),
          uiOutput("nbestui"),
          sliderInput(inputId = "min.prob", 
                      label = "Min probability with label",
                      min = 0, max = 1, value = 0.3)),
        conditionalPanel(
          condition = "input.classic=='TRUE' & input.lvp=='lvp' & input.boot_lvp=='Yes'",
          sliderInput(inputId = "max.circle", 
                      label = "Max circle size",
                      min = 1, max = 30, value = 15),
          radioButtons(inputId = "text",label = "Add text",
                       choices = c("Yes" = TRUE, "No" = FALSE),
                       selected = FALSE, inline = TRUE)
        ),
        conditionalPanel(
          condition = "input.classic=='TRUE' & input.lvp=='lvp' & input.boot_lvp=='Yes' & input.text=='TRUE'",
          sliderInput(inputId = "srt", 
                      label = "Label rotation",
                      min = -45, max = 90, value = 45)
        ),
        conditionalPanel(
          condition = "input.lvp=='lvp' | input.lvp=='af' | input.lvp=='vip'",# | input.lvp=='bglmnet'",
          radioButtons("classic", label = "Interactive plots",
                       choices = c("Yes" = FALSE, "No" = TRUE),
                       selected = FALSE, inline = TRUE)
        ),
        uiOutput("screenui")
        
      )
      ,
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
    )
    ,
    dashboardBody(
      tabItems(
        tabItem(tabName = "lvp",
                box(title = "Model stability plot", status = "primary",
                    solidHeader = TRUE, width = 12,
                    conditionalPanel(condition = "input.classic=='FALSE'",
                                     htmlOutput("lvp.gvis")),
                    conditionalPanel(condition = "input.classic=='TRUE'",
                                     plotOutput("lvp.classic"))
                ),
                box(title = "R output", status = "primary",
                    solidHeader = TRUE, collapsible = TRUE, 
                    width = 12, collapsed = FALSE, 
                    verbatimTextOutput("boot.verb"))),
        tabItem(tabName = "vip",
                box(title = "Variable inclusion plot",
                    status = "info",
                    solidHeader = TRUE, width = 12,
                    conditionalPanel(condition = "input.classic=='FALSE'",
                                     htmlOutput("vip.gvis")),
                    conditionalPanel(condition = "input.classic=='TRUE'",
                                     plotOutput("vip.classic")))
        ),
        tabItem(tabName = "af",
                box(title = "Adaptive fence", status = "warning",
                    solidHeader = TRUE, width = 12,
                    conditionalPanel(condition = "input.classic=='FALSE'",
                                     htmlOutput("af.gvis")),
                    conditionalPanel(condition = "input.classic=='TRUE'",
                                     plotOutput("af.classic"))),
                box(title = "R output", status = "warning",
                    solidHeader = TRUE, collapsible = TRUE,
                    width = 12, collapsed = FALSE,
                    verbatimTextOutput("af.verb"))),
        tabItem(tabName = "bglmnet",
                box(title = "Bootstrapped glmnet variable importance plot",
                    status = "success", solidHeader = TRUE, width = 12,
                    collapsible = TRUE, collapsed = FALSE,
                    htmlOutput("bglmnet.gvis")),
                box(title = "Bootstrapped glmnet model stability plot",
                    status = "success", solidHeader = TRUE, width = 12,
                    collapsible = TRUE, collapsed = FALSE,
                    htmlOutput("bglmnet.gvis2")))
      )
    )
  )
  
  server = function(input, output) {
    
    output$anyscreen = reactive({
      anyscreen
    })
    
    output$screenui = renderUI({
      conditionalPanel(condition = "output.anyscreen=='TRUE'",
                       radioButtons(inputId = "screen", label = "Screen",
                                    choices = c("Yes" = TRUE,"No" = FALSE),
                                    selected = FALSE,
                                    inline = TRUE))
    })
    
    output$nbestui = renderUI({
      kf = max(lvp.res$res.single.pass$k)
      max_n_mod = max(choose((kf - 1), 0:(kf - 1)))
      conditionalPanel(condition = "input.lvp=='lvp' & input.boot_lvp=='No'",
                       selectInput("nbest", 
                                   label = "Max number of models to display at each dimension",
                                   choices = c(1, 5, 10, max_n_mod),
                                   selected = max_n_mod))
    })
    
    visdat = reactive({
      validate(
        need(!is.null(lvp.res.screened) | !is.null(lvp.res), 
             "You did not include a vis object in the call to mplot.")
      )
      req(input$screen) # seems this is needed to avoid a warning message
      # Warning: Error in if: argument is of length zero
      if (input$screen) {
        validate(
          need(!is.null(lvp.res.screened), 
               "You did not include a screened vis object in the call to mplot.")
        )
        return(lvp.res.screened)
      } else {
        validate(
          need(!is.null(lvp.res), 
               "You did not include an unscreened vis object in the call to mplot.")
        )
        return(lvp.res)
      }
    })
    
    afdat = reactive({
      validate(
        need(!is.null(af.res.screened) | !is.null(af.res), 
             "You did not include an af object in the call to mplot.")
      )
      if (input$screen) {
        validate(
          need(!is.null(af.res.screened), 
               "You did not include a screened af object in the call to mplot.")
        )
        return(af.res.screened)
      } else {
        validate(
          need(!is.null(af.res), 
               "You did not include an unscreened af object in the call to mplot.")
        )
        return(af.res)
      }
    })
    
    bglmnetdat = reactive({
      validate(
        need(!is.null(glmnet.res.screened) | !is.null(glmnet.res), 
             "You did not include a bglmnet object in the call to mplot.")
      )
      
      if (input$screen) {
        validate(
          need(!is.null(glmnet.res.screened), 
               "You did not include a screened bglmnet object in the call to mplot.")
        )
        return(glmnet.res.screened)
      } else {
        validate(
          need(!is.null(glmnet.res), 
               "You did not include an unscreened bglmnet object in the call to mplot.")
        )
        return(glmnet.res)
      }
    })
    
    #### Model seleciton plot
    output$lvp.gvis <- googleVis::renderGvis({
      # req(input$boot_lvp)
      lvp.data = visdat()
      if (input$boot_lvp == "No") {
        graphics::plot(lvp.data, shiny = TRUE, interactive = TRUE,
                       highlight = input$highlight, which = "lvk", nbest = as.numeric(input$nbest))
      } else if (input$boot_lvp == "Yes") {
        graphics::plot(lvp.data, shiny = TRUE, interactive = TRUE,
                       highlight = input$highlight, which = "boot")
      }
    })
    
    output$lvp.classic <- renderPlot({
      lvp.data = visdat()
      # req(input$boot_lvp)
      if (input$boot_lvp == "No") {
        graphics::plot(lvp.data, highlight = input$highlight,
                       which = "lvk", interactive = FALSE, nbest = as.numeric(input$nbest))
      } else if (input$boot_lvp == "Yes") {
        graphics::plot(lvp.data, highlight = input$highlight,
                       which = "boot", interactive = FALSE, max.circle = input$max.circle,
                       text = input$text, min.prob = input$min.prob, srt = input$srt)
      }
    })
    
    #### Variable inclusion plots
    output$vip.gvis <- googleVis::renderGvis({
      lvp.data = visdat()
      graphics::plot(lvp.data, interactive = TRUE, 
                     shiny = TRUE, which = "vip")
    })
    
    output$vip.classic <- renderPlot({
      lvp.data = visdat()
      graphics::plot(lvp.data, interactive = FALSE, 
                     which = "vip")
    })
    
    output$boot.verb = renderPrint({
      lvp.data = visdat()
      print(lvp.data, min.prob = input$min.prob)
    })
    
    #### Adaptive fence plots
    output$af.gvis <- googleVis::renderGvis({
      af.data = afdat()
      if (!is.null(af.data)) {
        graphics::plot(af.data, interactive = TRUE, 
                       shiny = TRUE, best.only = input$bo)
      } else return(NULL)
    })
    
    output$af.classic <- renderPlot({
      af.data = afdat()
      if (!is.null(af.data)) {
        graphics::plot(af.data, interactive = FALSE, best.only = input$bo)
      } else return(NULL)
    })
    
    output$af.verb = renderPrint({
      af.data = afdat()
      summary(af.data)
    })
    
    ### Bootstrapping glmnet 
    output$bglmnet.gvis <- googleVis::renderGvis({
      bglmnet.data = bglmnetdat()
      graphics::plot(bglmnet.data, interactive = TRUE, 
                     shiny = TRUE, which = "vip")
    })
    
    output$bglmnet.gvis2 <- googleVis::renderGvis({
      bglmnet.data = bglmnetdat()
      graphics::plot(bglmnet.data, shiny = TRUE, interactive = TRUE,
                     highlight = input$highlight,
                     which = "boot", min.prob = input$min.prob)
    })
  }
  
  shinyApp(ui, server)
}
