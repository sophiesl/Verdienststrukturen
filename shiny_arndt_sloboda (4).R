library(shiny)
library(dplyr)
library(ggplot2)


#um Shiny App zu laden, die datenverdienst.csv speichern und den Pfad anpassen !!!

datenverdienst_neu10 <- read.csv("C:/Users/sunny/OneDrive/Dokumente/datenverdienst.csv")



# Legenden für Wirtschaftszweige

# nicht doppelt einblenden

wirtschaftszweig_legende <- c("Land- u. Forstwirtschaft, Fischerei", "Bergbau",
                              "Energie- u.Wasserversorgung", "Baugewerbe",
                              "Handel; KFZ", "Verkehr u. Lagerei",
                              "Gastgewerbe", "Information u. Kommunikation",
                              "Finanz- u. Versicherungsdienstl.",
                              "Grundstücks- u. Wohnungswesen, freiberufliche Dienstleistungen",
                              "Sonstige wirtschaftliche Dienstleistungen",
                              "Öffentliche Verwaltung",
                              "Gesundheits- u. Sozialwesen",
                              "Kunst, Unterhaltung u. Erholung",
                              "Sonstige Dienstleistungen")

# UI 
ui <- fluidPage(
  titlePanel("Gender Pay Gap"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "wirtschaftszweige",
                  label = "Wirtschaftszweige:",
                  min = 1,
                  max = 15,
                  value = 1),
      sliderInput(inputId = "wochenstunden",
                  label = "Wochenstunden:",
                  min = 20,
                  max = 40,
                  value = 30),
      # Legende für Wirtschaftszweige
      HTML(paste0("<p>Legende für Wirtschaftszweige:<br>",
                  paste(1:15, "= ", wirtschaftszweig_legende, collapse = "<br>"), "</p>"))
    ),
    mainPanel(
      plotOutput(outputId = "barChart"),
      verbatimTextOutput(outputId = "differenceOutput")
    )
  )
)

# SERVER

server <- function(input, output) {
  filtered_data <- reactive({
    datenverdienst_neu10 %>%
      filter(Wirtschaftszweig == input$wirtschaftszweige,
             wochenstunden == input$wochenstunden)
  })
  
  output$barChart <- renderPlot({
    #  ggplot bar chart 'Durchschnittlicher Bruttomonatsverdienst nach Wirtschaftszweig'
    
    ggplot(filtered_data(), aes(x = ef21, y = factor(Wirtschaftszweig, levels = 15:1), fill = factor(Geschlecht))) +
      geom_bar(stat = "summary", fun = "mean", position = "dodge", width = 0.6) +
      labs(title = "Durchschnittl. Bruttomonatsverdienst nach Wirtschaftszweig",
           x = "Bruttomonatsverdienst",
           y = "Geschlecht") +
      scale_fill_manual(values = c("1" = "blue", "2" = "pink"),
                        labels = c("Männlich", "Weiblich")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.text = element_text(size = 8),
            legend.position = "bottom",
            plot.caption = element_text(size = 6)) +  
      guides(fill = guide_legend(title = "Geschlecht")) +
      coord_flip()  
  })
  
  output$differenceOutput <- renderText({
    male_avg <- filtered_data() %>%
      filter(Geschlecht == 1) %>%
      summarise(avg = mean(ef21))
    female_avg <- filtered_data() %>%
      filter(Geschlecht == 2) %>%
      summarise(avg = mean(ef21))
    
    difference <- male_avg$avg - female_avg$avg
    
    formatted_difference <- scales::dollar(difference, prefix = "€")
    
    paste("Gender Pay Gap in €:", formatted_difference)
  })
}

# RUN

shinyApp(ui = ui, server = server)



