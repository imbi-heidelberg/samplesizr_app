# This is the samplezir web application made with shiny. 
# You can run the application by clicking
# the 'Run App' button above.

library(shiny)
library(markdown)
library(samplesizr)
library(tidyverse)

max_sliders <- 10

ui <- navbarPage("samplesizr",
  tabPanel("Overview",#=========================================================
    includeMarkdown("README.md")
  ),
  tabPanel("z test",#===========================================================
    h2("Sample size calculation for the two-sided z test"),
    sidebarLayout(
      sidebarPanel(
        actionButton("z_button", "Calculate!", width = '100%'),
        tags$hr(),
        h4("Data assumptions"),
        tags$div(title = "Value of primary endpoint (control group)",
          numericInput("z_mean_x", label = "Mean of control group", 
                       value = 60, step = 0.025)
        ),
        tags$div(title = "Value of primary endpoint (intervention group). See NOTE.",
          numericInput("z_mean_y", label = "Mean of intervention group", 
                     value = 70, step = 0.025)
        ),
        p("NOTE: Specify the minimal clinical significance to make you call
          the intervention superior to control regarding the primary endpoint."),
        tags$div(title = "Equal for both groups",
          numericInput("z_sd_y_and_x", 
            label = "Standard deviation in both groups", 
            value = 20, step = 0.025)
        ),
        tags$hr(),
        h4("Type I/II Error characteristics"),
        sliderInput("z_alpha", "Maximal type I error rate (two-sided)",
          min = .005, max = .2, value = .05, step = .001
        ),
        sliderInput("z_pwr", "Desired power",
          min = 0.5, max = .99, value = .9, step = .01
        ),
        tags$hr(),
        h4("Other"),
        tags$div(title = "Ratio of group sizes (larger group) : (smaller group)",
          sliderInput("z_r", "Desired ratio",
            min = 1, max = 10, value = 1, step = 1
          )
        ),
        radioButtons("z_r_x_or_y", "Larger group",  
          choiceNames = c("Intervention", "Control"), choiceValues = c(1,2)
        )
      ),
      mainPanel(
        verbatimTextOutput("ztest")
      )
    )
  ),
  tabPanel("t test",#===========================================================
    h2("Sample size calculation for the two-sided t-test"),
    sidebarLayout(
     sidebarPanel(
        actionButton("t_button", "Calculate!", width = '100%'),
        tags$hr(),
        h4("Planning assumptions"),
        tags$div(title = "Value of primary endpoint (control group)",
          numericInput("t_mean_x", label = "Mean of control group", 
            value = 60, step = 0.025)
        ),
        tags$div(title = "Value of primary endpoint (intervention group). See NOTE.",
          numericInput("t_mean_y", label = "Mean of intervention group", 
            value = 70, step = 0.025)
        ),
        p("NOTE: Specify the difference such that the intervention
          is clinically relevant superior to control regarding the primary endpoint."),
        tags$div(title = "Equal for both groups",
          numericInput("t_sd_y_and_x", 
            label = "Standard deviation (same in both groups)", 
            value = 20, step = 0.025)
        ),
        tags$hr(),
        h4("Type I/II error characteristics"),
        sliderInput("t_alpha", "Maximal type I error rate (two-sided)",
                    min = .005, max = .2, value = .05, step = .001
        ),
        sliderInput("t_pwr", "Desired power",
                    min = 0.5, max = .99, value = .9, step = .01
        ),
        tags$hr(),
        h4("Other specifications"),
        tags$div(title = "Ratio of group sizes (larger group) : (smaller group)",
          sliderInput("t_r", "Desired Ratio",
            min = 1, max = 10, value = 1, step = 1
          )
        ),
        radioButtons("t_r_x_or_y", "Larger group",  
          choiceNames = c("Intervention", "Control"), choiceValues = c(1,2)
        )
      ),
      mainPanel(
        verbatimTextOutput("ttest")
      )
    )
  ),
  tabPanel("ANCOVA",#===========================================================
    h2("Sample size calculation for the Analysis of Covariance (ANCOVA)"),
    sidebarLayout(
      sidebarPanel(
        actionButton("ancova_button", "Calculate!", width = '100%'),
        tags$hr(),
        h4("Planning assumptions"),
        tags$div(title = "Value of primary endpoint (control group)",
                 numericInput("ancova_mean_x", label = "Mean of control group", 
                              value = 60, step = 0.025)
        ),
        tags$div(title = "Value of primary endpoint (intervention group). See NOTE.",
                 numericInput("ancova_mean_y", label = "Mean of intervention group", 
                              value = 70, step = 0.025)
        ),
        p("NOTE: Specify the difference such that the intervention
          is clinically relevant superior to control regarding the primary endpoint."),
        numericInput("ancova_corr",
          label = "Correlation to covariate C",
          value = .5,
          step  = 0.005
        ),
        tags$div(title = "Equal for both groups",
          numericInput("ancova_sd_y_and_x", 
            label = "Standard deviation (same in both groups)", 
            value = 20, step = 0.025
          )
        ),
        tags$hr(),
        h4("Type I/II error characteristics"),
        sliderInput("ancova_alpha", "Maximal type I error rate (two-sided)",
          min = .005, max = .2, value = .05, step = .001
        ),
        sliderInput("ancova_pwr", "Desired power",
          min = 0.5, max = .99, value = .9, step = .01
        ),
        tags$hr(),
        h4("Other specifications"),
        tags$div(title = "Ratio of group sizes (larger group) : (smaller group)",
          sliderInput("ancova_r", "Desired Ratio",
            min = 1, max = 10, value = 1, step = 1
          )
        ),
        radioButtons("ancova_r_x_or_y", "Larger group",  
          choiceNames = c("Intervention", "Control"), choiceValues = c(1,2)
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
  tabPanel("Chi-square test",#==================================================
    h2("Sample size calculation for the Chi-Square test"),
    sidebarLayout(
      sidebarPanel(
        actionButton("chisq_button", "Calculate!", width = '100%'),
        tags$hr(),
        h4("Planning assumptions"),
        numericInput("chisq_p_Y",
          label = "Event rate: intervention group",
          value = .5, 
          step = .001
        ),
        numericInput("chisq_p_X",
          label = "Event rate: control group",
          value = .3, 
          step = .001
        ),
        p("NOTE: Specify the minimal absolute rate difference such that the 
          intervention is clinically relevant superior to control regarding
          the primary endpoint."),
        tags$hr(),
        h4("Type I/II error characteristics"),
        sliderInput("chisq_alpha",
          "Maximal Type I error rate",
          min = .005,
          max = .2,
          value = .05, 
          step = .001
        ),
        sliderInput("chisq_pwr",
          "Desired power",
          min = 0.5,
          max = .99,
          value = .8, 
          step = .01
        ),
        tags$hr(),
        h4("Other specifications"),
        tags$div(title = "Ratio of group sizes (larger group) : (smaller group)",
          sliderInput("chisq_r", "Desired Ratio",
            min = 1, max = 10, value = 1, step = 1
          )
        ),
        radioButtons("chisq_r_x_or_y", "Larger group",  
          choiceNames = c("Intervention", "Control"), choiceValues = c(1,2)
        ),
        checkboxInput("chisq_power.exact",
          "Calculate with exact power",
          value = TRUE
        )
      ),
      mainPanel(
        verbatimTextOutput("chisq")
      )
    )
  ),
  tabPanel("Fisher-Boschloo test",#=============================================
    h2("Sample size calculation for the Fisher-Boschloo test"),
    sidebarLayout(
      sidebarPanel(
        actionButton("fb_button", "Calculate!", width = '100%'),
        tags$hr(),
        h4("Planning assumptions"),
        numericInput("fb_p_Y",
          label = "Event rate: intervention group",
          value = .5, 
          step = .001
        ),
        numericInput("fb_p_X",
          label = "Event rate: control group",
          value = .3, 
          step = .001
        ),
        tags$hr(),
        h4("Type I/II error characteristics"),
        sliderInput("fb_alpha",
          "Maximal Type I error rate",
          min = .005,
          max = .2,
          value = .025, 
          step = .001
        ),
        sliderInput("fb_pwr",
          "Desired power",
          min = 0.5,
          max = .99,
          value = .8, 
          step = .01
        ),
        tags$hr(),
        h4("Other specifications"),
        tags$div(title = "Ratio of group sizes (larger group) : (smaller group)",
          sliderInput("fb_r", "Desired ratio",
            min = 1, max = 10, value = 1, step = 1
          )
        ),
        radioButtons("fb_r_x_or_y", "Larger group",  
          choiceNames = c("Intervention", "Control"), choiceValues = c(1,2)
        ),
        radioButtons("fb_calc_speed", "Level of accuracy",
                     choices = c("1" = .05,"2" = .01,"3" = .001),
                     selected = .001,
                     inline = TRUE
        ),
        p("Note: This defines the step width the algorithm is working with.
          Level 1 uses high step width for a fast calculation.
          Level 3 uses low step width but will need a long time to calculate.
          WARNING. Using Level 1 might not deliver an accurate result."),
        checkboxInput("fb_exact",
                      "Exact calculation",
                      value = TRUE
        )
      ),
      mainPanel(
        verbatimTextOutput("fisher_boschloo")
      )
    )
  ),
 
  tabPanel("F test",#===========================================================
    h2("Sample size calculation for the F-Test"),
    h4("comparing k > 2 groups"),
    sidebarLayout(
     sidebarPanel(
        actionButton("f_button", "Calculate!", width = '100%'),
        tags$hr(),
        sliderInput("f_n.groups", 
          "Number of groups",
          min = 3,
          max = 10,
          value = 3,
          step = 1
        ),
        tags$hr(),
        h4("Planning assumptions"),
        uiOutput(
          "f_mu_A"
        ),
        numericInput("f_sd",
          label = "Standard deviation (same in all groups)",
          value = 20, 
          step = .01
        ),
        tags$hr(),
        h4("Type I/II error characteristics"),
        sliderInput("f_alpha",
          "Maximal Type I error rate",
          min = .005,
          max = .2,
          value = .05, 
          step = .001
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
  tabPanel("Chi-square test",#==================================================
    h2("Sample size calculation for the Chi-square test"),
    h4("comparing k > 2 groups"),
    sidebarLayout(
     sidebarPanel(
       actionButton("chisq_m_button", "Calculate!", width = '100%'),
       tags$hr(),
       sliderInput("chisq_m_n.groups", 
          "Number of groups",
          min = 3,
          max = 10,
          value = 3,
          step = 1
       ),
       tags$hr(),
       h4("Planning assumptions"),
       uiOutput(
         "chisq_m_p_A"
       ),
       tags$hr(),
       h4("Type I/II error characteristics"),
       sliderInput("chisq_m_alpha",
         "Maximal type I error rate",
         min = .005,
         max = .2,
         value = .05, 
         step = .001
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
  ),
  inverse = TRUE,
  footer = "[1] M. Kieser, Fallzahlberechnung in der medizinischen 
     Forschung (2018), 1th Edition, Springer"
)


# Define server logic ==========================================================
server <- function(input, output) {
  
# Z-Test =======================================================================

  z_input <- eventReactive(input$z_button,{
    effect <- input$z_mean_y - input$z_mean_x
    alpha  <- input$z_alpha
    power  <- input$z_pwr
    sd     <- input$z_sd_y_and_x
    r      <- input$z_r
    r_inv  <- input$z_r_x_or_y
    
    if (r_inv == 2) { r <- (1/r) }
    
    return(list(r = r, alpha = alpha, power = power, sd = sd, 
        mu_x = input$z_mean_x, mu_y = input$z_mean_y
    ))
  })
  
  z_input_lazy <- reactive({
    input$z_button
    isolate(z_input())
  })
  
  z_output <- reactive({
    
    input$z_button
    
    z_in  <- z_input_lazy()
 
    z_out <- n_ztest(
      effect = z_in$mu_y - z_in$mu_x,
      sd     = z_in$sd,
      alpha  = z_in$alpha, 
      power  = z_in$power, 
      r      = z_in$r
    )
    return(z_out)
  })
  
  output$ztest <- renderPrint({ print(z_output()) })
  
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
      ggplot(aes(x, PDF, color = group)) + 
      geom_line() + 
      theme_classic(base_size = 18) + 
      theme(legend.position = "top") +
      scale_x_continuous(name = "primary endpoint") +
      scale_y_continuous(name = "") + 
      scale_color_discrete(name = "")
    
  })
  
  output$z_density_plot <- renderPlot({
    
    z_out <- z_output()
    
    effect <- z_out$effect
    n_X    <- z_out$n_X
    r      <- z_out$r
    sd     <- z_out$sd
    power  <- z_out$power_out
    
    nc <-  sqrt( (r/(1+r)) * n_X ) * (effect / sd)
    z_beta  <- qnorm(power)
    x_vline <- nc + z_beta
    x_vline <- nc - z_beta
    min_x <- floor( min(0, nc) - 4 )
    max_x <- ceiling( max(0, nc) + 4 )
    
    data_frame(
      z     = rep(seq(min_x, max_x, length.out = 101), 2),
      group = c(rep("Hypothesis", 101), rep("Alternative", 101)),
      mean  = c(rep(0, 101), rep(nc, 101)),
      sd = 1
    ) %>%
      mutate(
        PDF = dnorm(z, mean, sd)
      ) %>%
      ggplot(aes(z, PDF, color = group)) +
      geom_line()  +
      geom_vline(xintercept = x_vline, lty = 3) +
      geom_vline(xintercept = - x_vline, lty = 3) +
      theme_classic(base_size = 18) +
      scale_color_discrete(name="") +
      theme(legend.position = "top") +
      scale_x_continuous(name = "Z") +
      scale_y_continuous(name = "")
  })
  
  output$z_effect_plot <- renderPlot({
    
    z_out <- z_output()
    
    effect <- z_out$effect
    n_X    <- z_out$n_X
    alpha  <- z_out$alpha
    r      <- z_out$r
    sd     <- z_out$sd
    power  <- z_out$power
    
    min_x <- 0
    max_x <- effect + power * effect
    x = seq(from = min_x, to = max_x, length.out = 101)
    
    f <- function(x) {power_ztest(effect = x, sd=sd, n_X=n_X, alpha = alpha, r = r)}
    
    vert_line <- x[which.min(abs(f(x) - power))]
    vert_tick <- round(vert_line, 3)
    
    data_frame(
      x = x,
      y = f(x)
    ) %>%
    ggplot(aes(x, y)) + geom_line() + 
      theme_classic(base_size = 18) +
      geom_hline(yintercept = power, lty = 2) +
      geom_vline(xintercept = vert_line, lty = 3) +
      scale_y_continuous(
        name = "Power", 
        breaks = c(0, .5, power, 1)
      ) + 
      scale_x_continuous(
        name   = "Effect", 
        breaks = c(min_x, effect, max_x)
      )
    
  })
  
# t-Test =======================================================================
  t_input <- eventReactive(input$t_button,{
    effect <- input$t_mean_y - input$t_mean_x
    alpha  <- input$t_alpha
    power  <- input$t_pwr
    sd     <- input$t_sd_y_and_x
    r      <- input$t_r
    r_inv  <- input$t_r_x_or_y
    
    if (r_inv == 2) { r <- (1/r) }
    
    return(list(r = r, alpha = alpha, power = power, sd = sd, 
                mu_x = input$t_mean_x, mu_y = input$t_mean_y
    ))
  })
  
  t_input_lazy <- reactive({
    input$t_button
    isolate(t_input())
  })
  
  t_output <- reactive({
    
    input$t_button
    
    t_in  <- t_input_lazy()
    
    t_out <- n_ttest(
      effect = t_in$mu_y - t_in$mu_x,
      sd     = t_in$sd,
      alpha  = t_in$alpha, 
      power  = t_in$power, 
      r      = t_in$r
    )
    return(t_out)
  })
  
  output$ttest <- renderPrint({ print(t_output()) })
  
# chisq test ===================================================================
 
  chisq_input <- eventReactive(input$chisq_button,{
    p_Y    <- input$chisq_p_Y
    p_X    <- input$chisq_p_X
    alpha  <- input$chisq_alpha
    power  <- input$chisq_pwr
    r      <- input$chisq_r
    power.exact <- input$chisq_power.exact
    r_inv  <- input$chisq_r_x_or_y
    
    if (r_inv == 2) { r <- (1/r) }
    
    return(list(r = r, alpha = alpha, power = power, 
      p_Y = p_Y, p_X = p_X, power.exact = power.exact
    ))
  })
  
  chisq_input_lazy <- reactive({
    input$chisq_button
    isolate(chisq_input())
  })
  
  chisq_output <- reactive({
    
    input$chisq_button
    
    chisq_in  <- chisq_input_lazy()
    
    chisq_out  <- n_chisq(
      p_Y    = chisq_in$p_Y,
      p_X    = chisq_in$p_X,
      alpha  = chisq_in$alpha, 
      power  = chisq_in$power, 
      r      = chisq_in$r,
      power.exact = chisq_in$power.exact
    )
    return(chisq_out)
  })
  
  output$chisq <- renderPrint({ print(chisq_output()) })
  
  
# fisher boschloo test =========================================================

  fb_input <- eventReactive(input$fb_button,{
    p_Y   <- input$fb_p_Y
    p_X   <- input$fb_p_X
    alpha <- input$fb_alpha
    power <- input$fb_pwr
    r     <- input$fb_r
    exact <- input$fb_exact
    r_inv <- input$fb_r_x_or_y
    SW    <- as.numeric(input$fb_calc_speed)
    
    if (r_inv == 2) { r <- (1/r) }
    
    return(list(r = r, alpha = alpha, power = power, p_Y = p_Y, 
                p_X = p_X, exact = exact, SW = SW
    ))
  })
  
  fb_input_lazy <- reactive({
    input$fb_button
    isolate(fb_input())
  })
  
  fb_output <- reactive({
    
    input$fb_button
    
    fb_in  <- fb_input_lazy()
    
    fb_out  <- n_fisher_boschloo(
      p_Y    = fb_in$p_Y,
      p_X    = fb_in$p_X,
      alpha  = fb_in$alpha, 
      power  = fb_in$power, 
      r      = fb_in$r,
      exact  = fb_in$exact,
      SW     = fb_in$SW
    )
    return(fb_out)
  })
  
  output$fisher_boschloo <- renderPrint({ print(fb_output()) })
  
  
# ANCOVA =======================================================================
  
  ancova_input <- eventReactive(input$ancova_button,{
    effect <- input$ancova_mean_y - input$ancova_mean_x
    alpha  <- input$ancova_alpha
    power  <- input$ancova_pwr
    sd     <- input$ancova_sd_y_and_x
    corr   <- input$ancova_corr
    gs     <- input$ancova_gs
    r      <- input$ancova_r
    r_inv  <- input$ancova_r_x_or_y
    
    if (r_inv == 2) { r <- (1/r) }
    
    return(list(r = r, alpha = alpha, power = power, sd = sd, 
                mu_x = input$ancova_mean_x, mu_y = input$ancova_mean_y,
                corr = corr, gs = gs
    ))
  })
  
  ancova_input_lazy <- reactive({
    input$ancova_button
    isolate(ancova_input())
  })
  
  ancova_output <- reactive({
    
    input$ancova_button
    
    ancova_in  <- ancova_input_lazy()
    
    ancova_out <- n_ancova(
      effect = ancova_in$mu_y - ancova_in$mu_x,
      corr   = ancova_in$corr,
      sd     = ancova_in$sd,
      alpha  = ancova_in$alpha, 
      power  = ancova_in$power, 
      r      = ancova_in$r,
      gs     = ancova_in$gs
    )
    return(ancova_out)
  })
  
  output$ancova <- renderPrint({ print(ancova_output()) })
  
# F TEST =======================================================================
  
  f_input <- eventReactive(input$f_button,{
    n.groups <- input$f_n.groups
    sd    <- input$f_sd
    alpha <- input$f_alpha
    power <- input$f_pwr
    mu_A  <- c()
    
    for (i in 1:n.groups){
      mu_A[i] <- eval(parse(text = paste0("input$mu_",i)))
    }
   
    return(list(mu_A = mu_A, n.groups = n.groups,
                alpha = alpha, power = power, sd = sd))
  })
  
  f_input_lazy <- reactive({
    input$f_button
    isolate(f_input())
  })
  
  f_output <- reactive({
    
    input$f_button
    
    f_in  <- f_input_lazy()

    
    if (any(f_in$mu_A != 0)){
      f_out <- n_ftest(
        mu_A = f_in$mu_A,
        sd   = f_in$sd,
        n.groups = f_in$n.groups,
        alpha = f_in$alpha, 
        power = f_in$power
      )
    }
    return(f_out)
  })
  
  output$f <- renderPrint({ print(f_output()) })

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
        numericInput(
          mu_number_name,
          label = paste0("Mean: group ", my_i),
          value = 0,
          step = .001
        )
      })
    })
  }
  
# Chisquare mult. groups =======================================================
  
  chisq_m_input <- eventReactive(input$chisq_m_button,{
    n.groups <- input$chisq_m_n.groups
    alpha <- input$chisq_m_alpha
    power <- input$chisq_m_pwr
    p_A  <- c()
    
    for (i in 1:n.groups){
      p_A[i] <- eval(parse(text = paste0("input$p_",i)))
    }
    
    return(list(p_A = p_A, n.groups = n.groups,
                alpha = alpha, power = power)
           )
  })
  
  chisq_m_input_lazy <- reactive({
    input$chisq_m_button
    isolate(chisq_m_input())
  })
  
  chisq_m_output <- reactive({
    
    input$chisq_m_button
    
    chisq_m_in  <- chisq_m_input_lazy()
    
    
    if (any(chisq_m_in$p_A != .5)){
      chisq_m_out <- n_chisq_mult_groups(
        p_A = chisq_m_in$p_A,
        n.groups = chisq_m_in$n.groups,
        alpha = chisq_m_in$alpha, 
        power = chisq_m_in$power
      )
    }
    return(chisq_m_out)
  })
  
  output$chisq_mult_groups <- renderPrint({ print(chisq_m_output()) })
  
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
          paste0("Rate: group ", my_i),
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