library("shiny")
library("plotly")
library("EpiModel")

ui <- fluidPage(
  titlePanel("Assignment 3"),
  
  navbarPage("Number Vs Time: DCM and Stochastic Model",
             tabPanel("Introduction",
                      mainPanel(p("This assignment focuses on the covid trasmission rates under two different factors,
                      masks usage and covid carrier. The resource I used comes from one of the powerpoint slides from the lectures.
                      As professor explained, we can see that the highest transmission rates occur when a non-masked covid 19 carrier
                      interacts with a non-masked healthy person. We can also see that the lowest transmission rates
                      occur when a masked covid 19 carrier interacts with a masked healthy person. However, I thought it would be intersting
                      to use this situation and manipulate the amount of interaction between health and covid 19 carrier individuals."),
                      
                      
                      
                      p("When doing so we can see that overall, the amount of interaction largely affects the tranmission
                      probability as well. With higher amounts of interaction we can see that the transmission permeability
                      drastically increases in most cases. Likewise, with less to minimal interaction we can see that even the
                      highest permeability cases show drastic decreases in tranmission probability. Through this shiny app project, 
                      you will get to see visuals from deterministic models and individual contact model effectively showing the permeability 
                      rates over time based on different confidence intervals and different interaction levels."))),
                               
             tabPanel("DCM",
                      sidebarLayout(
                        sidebarPanel(  
                          sliderInput(inputId = "time",
                                            label = "Select time Range:",
                                            min = 0,
                                            max = 500,
                                            value = 50)
                        
                      ),
                      mainPanel(
                        plotOutput("mod")))),
            tabPanel("Stochastic Model",
                    sidebarLayout(
                      sidebarPanel(  
                        sliderInput(inputId = "time",
                           label = "Select time Range:",
                           min = 0,
                           max = 300,
                           value = 50)
             ),
             mainPanel(
               plotOutput("ploticm")))),
            tabPanel("Interpretation",
                     mainPanel(p("This graphs overall help the general audience to understand how effective it is to 
                               limit interaction with others in terms of covid tranmsission probability. We see 
                               through the DCM plot and the Stochastic plots that this correlation is consistent. 
                               We can also see a gnereal fluctation in i.num in the stochastic plot as the r.num increases, 
                               indicating that permeability rates increased when interaction rates increased." ),
                               
                               p("Overall, the interpretation of these graphs indicate that there is a strong causal relationship
                                 between interaction rates and transmission rates. This intends to inform the 
                                 general audience to stay isolated if they are a covid carrier and also
                                 generally avoid going to areas that have higher transmission chances due
                                 to crowd, exposure etc. "))
            
            )
))

   
server <- function(input, output, session){
  
output$intro <- renderText({
  "Hello"
})

output$interpretation <- renderText({
  "Hello"
})

  output$mod <- renderPlot({
param <- param.dcm(inf.prob = 0.1, act.rate = 1, rec.rate = 0.05)
init <- init.dcm(s.num = 999, i.num = 4,r.num = 0)
control <- control.dcm(type = "SIR", nsteps = 500, dt = 0.5)
mod <- (dcm(param, init, control))

#plot(mod)

par(mar = c(3.2, 3, 2, 1), mgp = c(2, 1, 0), mfrow = c(1, 2))
par(mar=c(3,3,3,3))
mod <- plot(mod, popfrac = FALSE, alpha = 0.5,
     lwd = 4, main = "DCM SIR PLOT", xlim = c(0, input$time) )


#### Individual contact model

})
  
  output$ploticm <- renderPlot({
  param.icm <- param.icm(inf.prob = c(0.1), act.rate = 0.5, rec.rate = 0.01)
  init.icm <- init.icm(s.num = 500, i.num = 1,r.num = 0)
  control.icm <- control.icm(type = "SIR", nsims = 100, nsteps = 300)
  mod.icm <- icm(param.icm, init.icm, control.icm)
  ploticm <- plot(mod.icm, main = "Stochastic PLOT", xlim = c(0, input$time))
  })
}
dev.off
shinyApp(ui, server)



