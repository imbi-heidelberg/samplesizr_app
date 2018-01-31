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
  tabPanel("z-Test",#===========================================================
    h2("Sample size calculation for the two-sided z-test"),
    sidebarLayout(
      sidebarPanel(
        actionButton("z_button", "Calculate!", width = '100%'),
        tags$hr(),
        h4("Data assumptions"),
        tags$div(title = "Value of primary endpoint (control group)",
          numericInput("z_mean_x", label = h5("Mean of control group"), 
                       value = 60, step = 0.025)
        ),
        tags$div(title = "Value of primary endpoint (intervention group). See NOTE.",
          numericInput("z_mean_y", label = h5("Mean of intervention group"), 
                     value = 70, step = 0.025)
        ),
        p("NOTE: Specify the minimal clinical significance to make you call
          the intervention superior to control regarding the primary endpoint."),
        tags$div(title = "Equal for both groups",
          numericInput("z_sd_y_and_x", 
            label = h5("Standard deviation in both groups"), 
            value = 20, step = 0.025)
        ),
        tags$hr(),
        h4("Type I/II Error characteristics"),
        sliderInput("z_alpha", "Maximal type I error rate",
          min = .005, max = .2, value = .05, step = .005
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
        tags$div(title = "[1] M. K., Fallzahlberechnung in der medizinischen 
          Forschung (2018), 1th Edition, Springer",
          p("Note that the sample size for a one-sided level
            &alpha; / 2 z-test is identical to the sample size for a two-sided 
            level &alpha; test.
            The methods used for calculation the sample size are explained
            on the pages 13 - 16 in [1]."
          )
        ),
        tags$hr(),
        p("The null hypothesis: control and 
          intervention group have the same mean in the primary endpoint."),
        p("The alternative hypothesis: There is a 
          difference between the two groups in the primary endpoint."),
        p("Please specify the difference of minimal clinical relevance you
          need to call the new intervention superior in your data assumptions."),
        h4("Data assumptions"),
        plotOutput("z_assumptions_plot"),
        p("Plot 1. Normal distribution assumption. 
          The model-distribution on your assumptions for control and intervention 
          group are shown."
        ),
        tags$hr(),
        conditionalPanel(
          "input.z_button && 
          (Math.abs(input.z_mean_y - input.z_mean_x) / input.z_sd_y_and_x ) >= .05",
          h3("Results"),
          verbatimTextOutput("ztest"),
          tags$br(),
          h4("Test statistic"),
          plotOutput("z_density_plot"),
          p("Plot 2. The distribution of the Z statistic under hypothesis
            and one side of the alternative is shown. 
            The vertical line marks the border between a decision for the 
            hypothesis/alternative."
          ),
          tags$br(),
          h4("Power curve"),
          plotOutput("z_effect_plot"),
          p("Plot 3. The dependency effect - power for fixed sample size
            is shown. To see the sample size look in the results.
            Effect is defined as the specified mean difference between
            the two groups.
            The lines mark the border where the desired power is reached."
          )
        ),
        conditionalPanel(
         "input.z_button &&
         ( Math.abs(input.z_mean_y - input.z_mean_x) / input.z_sd_y_and_x ) < .05",
         h3("Problem!"),
         p("The effect on the intervention you have specified is 0 or really small. 
           Please check your input. You want to perform sample size calculation 
           for small effects? Use the R package samplesizr or comparable software instead.")
         )
      )
    )
  ),
  tabPanel("t Test",#===========================================================
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
  tabPanel("ANCOVA",#===========================================================
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
  tabPanel("Chi-square Test",#==================================================
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
  tabPanel("Chi-square Test",#==================================================
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
      z = rep(seq(min_x, max_x, length.out = 101), 2),
      group = c(rep("Hypothesis", 101), rep("Alternative", 101)),
      mean = c(rep(0, 101), rep(nc, 101)),
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
  
# ANCOVA =======================================================================
  
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
  
# F TEST =======================================================================
  
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
  
# Chisquare mult. groups =======================================================
  
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