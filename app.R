# This is the samplezir web application made with shiny. You can run the application by clicking
# the 'Run App' button above.
#
# 

library(shiny)
library(markdown)
library(samplesizr)

ui <- navbarPage("samplesizr!",
  navbarMenu("Two Groups",
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
            min = 0,
            max = .2,
            value = .05, 
            step = .005
          ),
          sliderInput("t_pwr",
           "Desired power",
           min = 0.5,
           max = 1,
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
    
    tmp    <- samplesizr::n_ztest(
      effect = effect,
      sd     = sd,
      alpha  = alpha, 
      power  = power, 
      r      = r
    )
    print(tmp)
    
  })
  
  output$ttest <- renderPrint({
    
    effect <- input$t_effect
    alpha  <- input$t_alpha
    power  <- input$t_pwr
    sd     <- input$t_sd
    r      <- input$t_r
    
    tmp    <- samplesizr::n_ttest(
      effect = effect,
      sd     = sd,
      alpha  = alpha, 
      power  = power, 
      r      = r
    )
    print(tmp)
    
  })
  
  output$future <- renderText({
    print("This feature is still at work")
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(cars)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)