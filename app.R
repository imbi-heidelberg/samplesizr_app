# This is the samplezir web application made with shiny. 
# You can run the application by clicking
# the 'Run App' button above.

library(shiny)
library(markdown)
library(samplesizr)
library(tidyverse)

max_sliders <- 10

ui <- navbarPage("samplesizr",
  tabPanel("Overview",
    includeMarkdown("README.md")
  ),
  tabPanel("Z-Test",
    sidebarLayout(
      sidebarPanel(
        actionButton("z_button", "Update output"),
        h3("Assumptions:"),
        tags$div(title = "Supercoole Erklärung!",
          numericInput("z_mean_x", label = h5("Mean of control group"), 
                       value = 0, step = 0.025)
        ),
        numericInput("z_mean_y", label = h5("Mean of intervention group"), 
                     value = 0.3, step = 0.025), 
        numericInput("z_sd_y_and_x", label = h5("Standard deviation in both group"), 
                     value = 0.3, step = 0.025), 
        # TODO: verify that values are sensible / minimium differenc of ??? 
        tags$br(),
        h3("Operatic characteristics:"),
        sliderInput("z_alpha",
          "Maximal type I error rate",
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
        tags$br(),
        h3("Other:"),
        sliderInput("z_r",
          "Desired Allocation",
          min = 0.1,
          max = 10,
          value = 1, 
          step = .1
        ),
        tags$br()
      ),
      mainPanel(
        plotOutput("z_assumptions_plot"),
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
  ),
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
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
# Z-Test =======================================================================
  z_input <- reactive({
    effect <- input$z_mean_y - input$z_mean_x
    alpha  <- input$z_alpha
    power  <- input$z_pwr
    sd     <- input$z_sd_y_and_x
    r      <- input$z_r
    
    std_effect <- effect/sd
    if (abs(std_effect) < .05) {
      showModal(modalDialog(
        title = "Important message",
        "This is an important message!"
      ))
    }
    
    return(list(r = r, alpha = alpha, power = power, sd = sd, 
                mu_x = input$z_mean_x, mu_y = input$z_mean_y
              )
    )
  })
  
  z_input_lazy <- reactive({
    input$z_button
    isolate(z_input())
  })
  
  output$ztest <- renderPrint({
    
    input$z_button
    tmp <- isolate(z_input())
    
    z_out    <- samplesizr::n_ztest(
      effect = abs(tmp$mu_y - tmp$mu_x),
      sd     = tmp$sd,
      alpha  = tmp$alpha, 
      power  = tmp$power, 
      r      = tmp$r
    )
    
    print(z_out)
    
  })
  
  output$z_assumptions_plot <- renderPlot({
    
    tmp <- z_input_lazy()
    
    min_v <- floor(min(tmp$mu_x, tmp$mu_y) - 4*tmp$sd)
    max_v <- ceiling(max(tmp$mu_x, tmp$mu_y) + 4*tmp$sd)
    
    data_frame(
      x = rep(seq(min_v, max_v, length.out = 101), 2),
      group = c(rep("Control", 101), rep("Intervention", 101)),
      mean = c(rep(tmp$mu_x, 101), rep(tmp$mu_y, 101)),
      sd = tmp$sd
    ) %>%
    mutate(
      PDF = dnorm(x, mean, sd)
    ) %>%
      ggplot(aes(x, PDF, color = group)) + geom_line()
    
  })
  
  
  
# t-Test =======================================================================
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