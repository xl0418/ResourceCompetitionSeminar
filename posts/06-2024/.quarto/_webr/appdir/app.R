Sim_droop <- function(tt, rin, mu0, mu1, vmax, k, temp_mean, temp_amp, tau, qmin, m, n0, r0, q0, y = 1.3) {
  nt <- c(n0)
  rt <- c(r0)
  qt <- c(q0)
  tempt <- c(0)
  mu_t <- c(0)
  for(ti in 1:(tt*100)){
    temp_nt <- nt[ti]
    temp_rt <- rt[ti]
    temp_qt <- qt[ti]

    temp <- temp_mean + temp_amp * sin(2 * pi * ti / tau)
    mu_inf <- mu0 * exp(mu1 * temp)
    v <- vmax * temp_rt / (temp_rt + k)
    
    drt <- ((rin - temp_rt) - v * temp_nt)/100
    dqt <- (v - mu_inf * (1 - qmin / temp_qt) * temp_qt)/100
    dnt <- (mu_inf * (1 - qmin / temp_qt) - m) * temp_nt / 100
    
    temp_rt <- max(temp_rt + drt, 0)
    temp_qt <- max(temp_qt + dqt, 0)
    temp_nt <- max(temp_nt + dnt, 0)
    
  
    nt <- c(nt, temp_nt)
    rt <- c(rt, temp_rt)
    qt <- c(qt, temp_qt)
    tempt <- c(tempt, temp)
    mu_t <- c(mu_t, mu_inf)
  }
  return(data.frame(nt = nt, rt = rt, qt = qt,
                    tempt=tempt, mut = mu_t, cc = 1:(tt*100+1)))
  
}

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  headerPanel(''),
  
  fluidRow(
    column(12,
           textOutput("text"),
           tags$head(tags$style("#text{color: black;
                                 font-size: 30px;
                                 font-style: italic;
                                 }"
           )
           ),
           plotlyOutput("plot1", width=800, height = 400),
           plotlyOutput("plot2", width=800, height = 400)
    )
  ),
  
  fluidRow(
    column(4,
           div(style="height: 80px;",sliderInput('temp_amp', HTML("Temperature amplitude"), 5, min = 0.1, max = 5))
    ),
    column(4,
           div(style="height: 80px;",sliderInput('tau', HTML("Period"), 5, min = 1, max = 50))
    )
  )
  
  
)

server <- function(input, output, session) {
  output$text <- renderText({
    "Droop model + periodic forcing"
  })
  
  
  
  
  # sim lv model
  sim_result <- reactive({
    Sim_droop(tt = 30, rin = 10, mu0 = 1, mu1 = 1, vmax=10, k = 1,
              temp_mean = 0, temp_amp = input$temp_amp, tau = input$tau,
              qmin = 1, m = 0.5, n0 = 1, r0 = 1, q0 = 1)
  })
  # browser()
  
  output$plot1 <- renderPlotly({
    
    p <- plot_ly(data = sim_result()) %>%
      add_trace(x = ~cc, y = ~nt, type = 'scatter', mode = 'lines', name = 'N', line = list(color = 'green')) %>%
      add_trace(x = ~cc, y = ~rt, type = 'scatter', mode = 'lines', name = 'R', line = list(color = 'blue')) %>%
      add_trace(x = ~cc, y = ~qt, type = 'scatter', mode = 'lines', name = 'Q', line = list(color = 'orange')) %>%
      #x limit
      layout(xaxis = list(range = c(0, 1000))) %>%
      # x label
      layout(xaxis = list(title = "Time"),yaxis = list(title = "N"))
    p
    
  })
  
  output$plot2 <- renderPlotly({
    
    p <- plot_ly(data = sim_result()) %>%
      add_trace(x = ~cc, y = ~tempt, type = 'scatter', mode = 'lines', name = 'temp', line = list(color = 'red', width = 4)) %>%
      #x limit
      layout(xaxis = list(range = c(0, 100)))  %>%
      # x label
      layout(xaxis = list(title = "Time"),yaxis = list(title = "Temperature"))
    p
    
  })
  
}
# Create Shiny app ----
shinyApp(ui = ui, server = server)
