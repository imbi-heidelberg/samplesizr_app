# This is the samplezir web application made with shiny. 
# You can run the application by clicking
# the 'Run App' button above.

library(shiny)
library(markdown)
library(samplesizr)

max_sliders <- 10

ui <- navbarPage("samplesizr!",
  navbarMenu("Two groups",
    tabPanel("z Test",
      sidebarLayout(
        sidebarPanel(
          radioButtons("z_type",
            "Type of Alternative",
            c("One-Sided" = "z_one",
            "Two-Sided" = "z_two"
            ),
            "z_two"
          ),
          sliderInput("z_effect",
            "Effect size on alternative",
            min = .01,
            max = 50,
            value = 10, 
            step = .01
          ),
          sliderInput("z_sd",
            "Standard deviation of sample",
            min = 0.1,
            max = 100,
            value = 20, 
            step = .01
          ),
          sliderInput("z_alpha",
            "Desired Type I Error alpha",
            min = .005,
            max = .2,
            value = .05, 
            step = .005
          ),
          sliderInput("z_pwr",
            "Desired power",
            min = 0.5,
            max = .99,
            value = .8, 
            step = .01
          ),
          sliderInput("z_r",
            "Desired Allocation",
            min = 0.1,
            max = 10,
            value = 1, 
            step = .1
          )
        ),
        mainPanel(
          verbatimTextOutput("ztest")
        )
      )
    ),
    tabPanel("t Test",
      sidebarLayout(
        sidebarPanel(
          sliderInput("t_effect",
            "Effect size on alternative",
            min = 0,
            max = 50,
            value = 10, 
            step = .01
          ),
          sliderInput("t_sd",
            "Standard deviation of sample",
            min = 0.1,
            max = 100,
            value = 20, 
            step = .01
          ),
          sliderInput("t_alpha",
            "Desired Type I Error alpha",
            min = .005,
            max = .2,
            value = .05, 
            step = .005
          ),
          sliderInput("t_pwr",
           "Desired power",
           min = 0.5,
           max = .99,
           value = .8, 
           step = .01
          ),
          sliderInput("t_r",
           "Desired Allocation",
           min = 0.1,
           max = 10,
           value = 1, 
           step = .1
          )
        ),
        mainPanel(
          verbatimTextOutput("ttest")
        )
      )
    ),
    tabPanel("ANCOVA",
      sidebarLayout(
        sidebarPanel(
          sliderInput("ancova_effect",
            "Effect size on alternative",
            min = 0,
            max = 50, 
            value = 10, 
            step = .01
          ),
          sliderInput("ancova_corr",
            "Correlation to covariate C",
             min = 0,
             max = 1,
             value = .3, 
             step = .01
          ),
          sliderInput("ancova_sd",
            "Standard deviation of sample",
            min = .01,
            max = 100,
            value = 20, 
            step = .01
          ),
          sliderInput("ancova_alpha",
             "Desired Type I Error alpha",
             min = .005,
             max = .2,
             value = .05, 
             step = .005
          ),
          sliderInput("ancova_pwr",
            "Desired power",
            min = 0.5,
            max = .99,
            value = .8, 
            step = .01
          ),
          sliderInput("ancova_r",
            "Desired Allocation",
            min = 0.1,
            max = 10,
            value = 1, 
            step = .1
          ),
          checkboxInput("ancova_gs",
            "Guenther/Schouten correction",
            value = TRUE
          )
        ),
        mainPanel(
          verbatimTextOutput("ancova")
        )
      )
    ),
    tabPanel("Chi-square Test",
      sidebarLayout(
        sidebarPanel(
          sliderInput("chisq_p_Y",
            "Event rate of group Y on alternative",
            min = 0,
            max = 1,
            value = .5, 
            step = .01
          ),
          sliderInput("chisq_p_X",
            "Event rate of group X on alternative",
            min = 0,
            max = 1,
            value = .3, 
            step = .01
          ),
          sliderInput("chisq_alpha",
            "Desired Type I Error alpha",
            min = .005,
            max = .2,
            value = .05, 
            step = .005
          ),
          sliderInput("chisq_pwr",
            "Desired power",
            min = 0.5,
            max = .99,
            value = .8, 
            step = .01
          ),
          sliderInput("chisq_r",
            "Desired Allocation",
            min = 0.1,
            max = 10,
            value = 1, 
            step = .1
          ),
          checkboxInput("chisq_power.exact",
            "Berechnung zur exakten Power",
            value = TRUE
          )
        ),
        mainPanel(
          verbatimTextOutput("chisq")
        )
      )
    )
  ),
  navbarMenu("k > 2 groups",
    tabPanel("f Test",
      sidebarLayout(
        sidebarPanel(
          sliderInput("f_n.groups", 
            "Number of groups",
            min = 3,
            max = 10,
            value = 3,
            step = 1
          ),
          uiOutput(
            "f_mu_A"
          ),
          sliderInput("f_sd",
                      "Standard deviation of sample",
                      min = 0.1,
                      max = 100,
                      value = 20, 
                      step = .01
          ),
          sliderInput("f_alpha",
            "Desired Type I Error alpha",
            min = .005,
            max = .2,
            value = .05, 
            step = .005
          ),
          sliderInput("f_pwr",
            "Desired power",
            min = 0.5,
            max = 0.99,
            value = .8, 
            step = .01
          )
        ),
        mainPanel(
          verbatimTextOutput("f")
        )
      )
    ),
    tabPanel("Chi-square Test",
     sidebarLayout(
       sidebarPanel(
         sliderInput("chisq_m_n.groups", 
            "Number of groups",
            min = 3,
            max = 10,
            value = 3,
            step = 1
         ),
         uiOutput(
           "chisq_m_p_A"
         ),
         sliderInput("chisq_m_alpha",
           "Desired Type I Error alpha",
           min = .005,
           max = .2,
           value = .05, 
           step = .005
         ),
         sliderInput("chisq_m_pwr",
           "Desired power",
           min = 0.5,
           max = 0.99,
           value = .8, 
           step = .01
         )
       ),
       mainPanel(
         verbatimTextOutput("chisq_mult_groups")
       )
     )
    )
  ),
  tabPanel("About",
    fluidRow(
      column(6,
        includeMarkdown("README.md")
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$ztest <- renderPrint({
    
    effect <- input$z_effect
    alpha  <- input$z_alpha
    power  <- input$z_pwr
    sd     <- input$z_sd
    r      <- input$z_r
    
    z_out    <- samplesizr::n_ztest(
      effect = effect,
      sd     = sd,
      alpha  = alpha, 
      power  = power, 
      r      = r
    )
    print(z_out)
    
  })
  
  output$ttest <- renderPrint({
    
    effect <- input$t_effect
    alpha  <- input$t_alpha
    power  <- input$t_pwr
    sd     <- input$t_sd
    r      <- input$t_r
    
    t_out    <- samplesizr::n_ttest(
      effect = effect,
      sd     = sd,
      alpha  = alpha, 
      power  = power, 
      r      = r
    )
    print(t_out)
    
  })
  
  output$chisq <- renderPrint({
    
    p_Y    <- input$chisq_p_Y
    p_X    <- input$chisq_p_X
    alpha  <- input$chisq_alpha
    power  <- input$chisq_pwr
    r      <- input$chisq_r
    power.exact <- input$chisq_power.exact
    
    chisq_out  <- samplesizr::n_chisq(
      p_Y    = p_Y,
      p_X    = p_X,
      alpha  = alpha, 
      power  = power, 
      r      = r,
      power.exact = power.exact
    )
    print(chisq_out)
    
  })
  
  output$ancova <- renderPrint({
    
    effect <- input$ancova_effect
    corr   <- input$ancova_corr
    sd     <- input$ancova_sd
    alpha  <- input$ancova_alpha
    power  <- input$ancova_pwr
    r      <- input$ancova_r
    gs     <- input$ancova_gs
    
    ancova_out  <- samplesizr::n_ancova(
      effect = effect,
      corr   = corr,
      sd     = sd,
      alpha  = alpha,
      power  = power, 
      r      = r,
      gs     = gs
    )
    print(ancova_out)
    
  })
  
  output$f <- renderPrint({
    mu_A <- c()
    n.groups <- input$f_n.groups
    sd    <- input$f_sd
    alpha <- input$f_alpha
    power <- input$f_pwr
    
    for (i in 1:n.groups){
      mu_A[i] <- eval(parse(text = paste0("input$mu_",i)))
    }
    
    if (any(mu_A != 0)){
      f_out <-  n_ftest(
        mu_A     = mu_A,
        sd       = sd, 
        n.groups = n.groups,
        alpha    = alpha,
        power    = power
      )
      print(f_out)
    } 
  })
  
  output$f_mu_A <- renderUI({
    f_mu_A_input_list <- lapply(1:input$f_n.groups, function(i) {
      mu_number_name <- paste("mu_", i, sep="")
      uiOutput(mu_number_name)
    } )
    do.call(tagList, f_mu_A_input_list)
  })
  
  for (i in 1:max_sliders) {
    local({
      my_i <- i
      mu_number_name <- paste("mu_", my_i, sep="")
      
      output[[mu_number_name]] <- renderUI({
        sliderInput(
          mu_number_name,
          paste0("Erwarteter Wert Gruppe ", my_i),
          min = -100,
          max = 100,
          value = 0,
          step = 1
        )
      })
    })
  }
  
  output$chisq_mult_groups <- renderPrint({
    p_A <- c()
    n.groups <- input$chisq_m_n.groups
    alpha <- input$chisq_m_alpha
    power <- input$chisq_m_pwr
    
    for (i in 1:n.groups){
      p_A[i] <- eval(parse(text = paste0("input$p_",i)))
    }
    
    if (any(p_A != .5)){ 
      chisq_m_out <- n_chisq_mult_groups(
        p_A      = p_A,
        n.groups = n.groups,
        alpha    = alpha,
        power    = power
      )
      print(chisq_m_out)
    }
  })
  
  output$chisq_m_p_A <- renderUI({
    chisq_m_p_A_input_list <- lapply(1:input$chisq_m_n.groups, function(i) {
      p_number_name <- paste("p_", i, sep="")
      uiOutput(p_number_name)
    }
    )
    do.call(tagList, chisq_m_p_A_input_list)
  })
  
  for (i in 1:max_sliders) {
    local({
      my_i <- i
      p_number_name <- paste("p_", my_i, sep="")
      
      output[[p_number_name]] <- renderUI({
        sliderInput(
          p_number_name,
          paste0("Erwartete Rate Gruppe ", my_i),
          min = 0,
          max = 1,
          value = .5,
          step = .005
        )
      })
    })
  }
  
}

# Run the application 
shinyApp(ui = ui, server = server)