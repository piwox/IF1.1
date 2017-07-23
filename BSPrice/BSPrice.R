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
   titlePanel("Charakterystyki opcji"),
   
   sidebarLayout(
      sidebarPanel(
         radioButtons("greek", NULL, 
                      c("Price" = 'price',"Delta"='delta',"Gamma"='gamma')),
         radioButtons("c_p","Choose type:", c("Call"='c',"Put"='p')
                    ),
         checkboxInput("bin","Binary",FALSE),
         sliderInput("time",
                     "Number of days to expire:",
                     min = 0,
                     max = n_days_0-1,
                     value = n_days_0-1,
                     step=3,
                     animate= animationOptions(250)),
        
         sliderInput("strike",
                     "Strike value:",
                     min = 1500,
                     max = 3500,
                     value = 2300,
                     step = 100,
                     animate= TRUE),
         sliderInput("sigma",
                     "Sigma value:",
                     min = 0,
                     max = 0.1,
                     value = sigma_wig,
                     animate= TRUE)
       
      ),
      # Show a plot
      mainPanel(
         plotlyOutput("distPlot")
      )
   )
)


server <- function(input, output) {
  
   output$distPlot <- renderPlotly({
   
      b     <- input$bin
      c     <- input$c_p
      if(b)
         c  <- paste0('b', c)
      s     <- seq(1500, 3500, by = 1)
      
      if(input$greek == 'price')
         x  <- BSPrice(S_t = s, strike = input$strike, t = input$time, sigma = input$sigma,
                       c_p = c)
      if(input$greek == 'delta')
         x  <- Delta(S_t = s, strike = input$strike, t = input$time, sigma = input$sigma,
                     c_p = c)
      if(input$greek == 'gamma')
         x  <- Gamma(S_t = s, strike = input$strike, t = input$time, sigma = input$sigma,
                     c_p = c)
      ggplotly(ggplot(as.data.frame(x), aes(s, x)) + geom_line(col = 'darkred',lwd = 1) + theme_economist() +
         geom_vline(xintercept = input$strike, col = 'darkblue',alpha=0.3)) 
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

