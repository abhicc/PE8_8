library(babynames)
data("babynames")


beatles <- babynames %>% 
  filter(name %in% c("John", "Paul", "George", "Ringo")) %>%   # option 2 
  group_by(name, year) %>% 
  dplyr::summarize(popularity = sum(n))  # no input associated

ui8 <- fluidPage(
  
  fluidRow(
    column(4,
           numericInput(inputId = "startyear",
                        label = "Select start year:",
                        min = 1880,
                        max = 2017,
                        value = 1880)),
    column(4,
           numericInput(inputId = "endyear",
                        label = "Select end year:",
                        min = 1880,
                        max = 2017,
                        value = 2017)),
    column(4,
           checkboxGroupInput(inputId = "names",
                              label = "Select name(s):",
                              choices = c("John", "Paul", "George", "Ringo"),
                              selected = "John")),
    plotOutput("lineplot")
  )
)

server8 <- function(input, output){
  
  output$lineplot <- renderPlot({
    
    ggplot(data = beatles %>% filter(year >= input$startyear, year <= input$endyear,
                                     name %in% input$names)) +
      geom_line(aes(x = year, y = popularity, color = name), size = 2)
    
  })
}

shinyApp(ui = ui8, server = server8)

