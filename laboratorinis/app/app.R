library(shiny)
library(shinydashboard)
library(tidyverse)

data = read_csv("../data/lab_sodra.csv")

new_data = data %>%
  filter(ecoActCode == 479100) %>%
  mutate(month_value = as.integer(substr(month, 5, 7)))

ui = dashboardPage(
  skin = "green",
  dashboardHeader(title = "Uzdaroji Akcine bendrove Bermeta"),
  dashboardSidebar(selectizeInput( inputId = "imones_pavadinimas", 
  label = "Imones pavadinimas", choices = unique(new_data$name), selected = NULL )),
  dashboardBody( tabsetPanel( tabPanel("Vidutinio atlyginimo grafikas", plotOutput("plot1")),
  tabPanel("Lentele", tableOutput("table")), tabPanel("Apdraustuju darbuotoju grafikas",
  plotOutput("plot2")),
  tabPanel("Mokesciai", plotOutput("plot3")))))

server = function(input, output, session) { selected_data = reactive(
{ new_data %>%filter(name == input$imones_pavadinimas)})
  output$table = renderTable({selected_data() %>%select(-name)})
  
  output$plot1 =renderPlot({ selected_data() %>%
      ggplot(aes(x = month_value, y = avgWage)) +
      geom_point() +
      geom_line(colour = "green") +
      scale_x_continuous(
        name = "Month",
        breaks = 1:12,
        limits = c(1, 12)
      ) +
      labs(x = "Month", y = "avgWage") +
      theme_minimal() +
      theme(axis.text.x = element_blank())
  })
  
  output$plot2 = renderPlot({selected_data() %>%
      ggplot(aes(x = month_value, y = numInsured)) +
      geom_point() +
      geom_line(colour = "green") +
      scale_x_continuous(
        name = "Month",
        breaks = 1:12,
        limits = c(1, 12)
      ) +
      labs(x = "Month", y = "Count") +
      theme_minimal()
  })
  
  output$plot3 = renderPlot({selected_data() %>%
      ggplot(aes(x = month_value, y = tax)) +
      geom_point() +
      geom_line(colour = "green") +
      scale_x_continuous(name = "Month", breaks = 1:12,
      limits = c(1, 12)) +labs(x = "Month", y = "avgWage") +
      theme_minimal()
  })
}
shinyApp(ui, server)
