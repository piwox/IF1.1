#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Rozklad zysku/straty z portfela delta-neutralnego"),
   
   sidebarLayout(
      sidebarPanel(
         radioButtons("step", NULL, 
                      c("Monthly" = '21',
                        "Weekly"  = '5',
                        "Daily"   = '1')),
                        # "2 times a day" = 2,
                        # "4 times a day" = 4,
                        # "12 times a day" = 12,
                        # "24 times a day" = 24))
        checkboxInput("dis", "Discount", TRUE),
        checkboxInput("cost", "Transaction costs", FALSE),
        textAreaInput("nsim", "Number of paths", value = 5000,resize="both"),
        sliderInput("sigma", "Market assumed volatility", min=0, max=70, value=sigma_wig*sqrt(n_days_0)*100,post='%'),
        #sliderInput("sigma2", "Market assumed volatility", min=0, max=70, value=sigma_wig*sqrt(third_fri_sep)*100,post='%')
        radioButtons("c_p","Option type",c("Call"='c',"Put"='p'))
      ),
      # Show a plot
      mainPanel(
         plotlyOutput("distPlot")
      )
   )
)


server <- function(input, output) {
   
   output$distPlot <- renderPlotly({
      n_days = third_fri_sep
      paths_wig <- SimPaths(n_sim = strtoi(input$nsim))
     
      if(input$dis)
      {
         if(input$cost)
            d <-  PortfolioValueMatrixCosts(c_p = input$c_p, step = strtoi(input$step),
                                            sigma = input$sigma/sqrt(n_days_0*100^2))[third_fri_sep,]
         else
            d <-  PortfolioValueMatrix(c_p = input$c_p, step = strtoi(input$step),
                                       sigma = input$sigma/sqrt(n_days_0*100^2))[third_fri_sep,]
      }else{
         
         if(input$cost)
            d <-  PortfolioValueMatrixCosts2(c_p = input$c_p, step = strtoi(input$step),
                                             sigma = input$sigma/sqrt(n_days_0*100^2))[third_fri_sep,]
         else
            d <-  PortfolioValueMatrix2(c_p = input$c_p, step = strtoi(input$step),
                                        sigma = input$sigma/sqrt(n_days_0*100^2))[third_fri_sep,]
      }
      df = as.data.frame(d)
      ggplotly(ggplot(df, aes(x = d, y=..density..)) + geom_histogram(bins=100, color = 'darkgrey', fill = 'red')+
               scale_x_continuous()+xlim(c(-100,150)+(!input$dis)*90)+theme_economist())#geom_boxplot() + geom_jitter(width=0.2)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

