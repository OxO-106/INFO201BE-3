library(shiny)
library(tidyverse)

# Define UI for application
ui <- fluidPage(
  tabsetPanel(
    tabPanel(
      "About",
      h2("Purpose"),
      h2("Source"),
      h2("Image")
    ),
    tabPanel(
      "Financial",
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            "finBtn",
            "Choose which financial perspective you wish to analyze",
            choices = c("Income Category", "Card Category"),
            selected = "Income Category"
          ),
          radioButtons(
            "value",
            "Choose which data you wish to analysis",
            choices = c("Overview", "Credit Limit", "Average Ulitilization Ratio", "Months on book", "Total Transaction Amount"),
            selected = "Overview"
          ),
          uiOutput("incomeBtn"),
          uiOutput("cardBtn")
        ),
        mainPanel(
          h2("Description"),
          verbatimTextOutput("info"),
          h2("Histogram"),
          plotOutput("fin_plot")
        )
      )
    ),
    tabPanel(
      "Social"
    ),
    tabPanel(
      "Biological"
    ),
    tabPanel(
      "Conclusion",
      tableOutput("fin_table1"),
      tableOutput("fin_table2")
    )
  )
)
# Define server logic
server <- function(input, output) {
  churn <- read_delim('BankChurners.csv')
  
  output$incomeBtn <- renderUI({
    if (input$finBtn == "Income Category"){
      radioButtons(
        "income",
        "Choose the income category",
        choices = c("Less than $40K", "$40K - $60K", "$60K - $80K", "$80K - $120K", "$120K +"),
        selected = "Less than $40K"
      )
    }
  })
  
  output$cardBtn <- renderUI({
    if (input$finBtn == "Card Category"){
      radioButtons(
        "card",
        "Choose the card category",
        choices = c("Blue", "Silver", "Gold", "Platinum"),
        selected = "Blue"
      )
    }
  })
  
  output$fin_plot <- renderPlot({
    fin_dataIncome <- reactive({
      if (input$value == "Overview"){
        churn %>% 
          filter(Income_Category != "Unknown") %>% 
          group_by(Income_Category, Attrition_Flag) %>% 
          summarise(X = Income_Category) %>% 
          filter(Income_Category == input$income)
      } else if (input$value == "Credit Limit"){
        churn %>% 
          filter(Income_Category != "Unknown") %>% 
          group_by(Income_Category, Attrition_Flag) %>% 
          summarise(X = Credit_Limit) %>% 
          filter(Income_Category == input$income)
      } else if (input$value == "Average Ulitilization Ratio"){
        churn %>% 
          filter(Income_Category != "Unknown") %>% 
          group_by(Income_Category, Attrition_Flag) %>% 
          summarise(X = Avg_Utilization_Ratio) %>% 
          filter(Income_Category == input$income)
      } else if (input$value == "Months on book"){
        churn %>% 
          filter(Income_Category != "Unknown") %>% 
          group_by(Income_Category, Attrition_Flag) %>% 
          summarise(X = Months_on_book) %>% 
          filter(Income_Category == input$income)
      } else if (input$value == "Total Transaction Amount"){
        churn %>% 
          filter(Income_Category != "Unknown") %>% 
          group_by(Income_Category, Attrition_Flag) %>% 
          summarise(X = Total_Trans_Amt) %>% 
          filter(Income_Category == input$income)
      }
    })
    
    fin_dataCard <- reactive({
      if (input$value == "Overview"){
        churn %>% 
          group_by(Card_Category, Attrition_Flag) %>% 
          summarise(X = Card_Category) %>% 
          filter(Card_Category == input$card)
      } else if (input$value == "Credit Limit"){
        churn %>% 
          group_by(Card_Category, Attrition_Flag) %>% 
          summarise(X = Credit_Limit) %>% 
          filter(Card_Category == input$card)
      } else if (input$value == "Average Ulitilization Ratio"){
        churn %>% 
          group_by(Card_Category, Attrition_Flag) %>% 
          summarise(X = Avg_Utilization_Ratio) %>% 
          filter(Card_Category == input$card)
      } else if (input$value == "Months on book"){
        churn %>% 
          group_by(Card_Category, Attrition_Flag) %>% 
          summarise(X = Months_on_book) %>% 
          filter(Card_Category == input$card)      
      } else if (input$value == "Total Transaction Amount"){
        churn %>% 
          group_by(Card_Category, Attrition_Flag) %>% 
          summarise(X = Total_Trans_Amt) %>% 
          filter(Card_Category == input$card)      
      }
    })
    
    if (input$finBtn == "Income Category") {
      if (input$value == "Overview"){
        ggplot(data = fin_dataIncome(), mapping = aes(x = X, fill = as.factor(Attrition_Flag))) +
          ggtitle("Overview of Existing or Attritted Customer with different income category") +
          geom_bar(position = "dodge")
      }else{
        ggplot(data = fin_dataIncome(), mapping = aes(x = X, fill = as.factor(Attrition_Flag))) +
          ggtitle(paste("Relationship of income and", input$value)) +
          geom_histogram(position = "dodge")
      }
    } else {
      if (input$value == "Overview"){
        ggplot(data = fin_dataCard(), mapping = aes(x = X, fill = as.factor(Attrition_Flag))) +
          ggtitle("Overview of Existing or Attritted Customer with different card category") +
          geom_bar(position = "dodge")
      }else{
        ggplot(data = fin_dataCard(), mapping = aes(x = X, fill = as.factor(Attrition_Flag))) +
          ggtitle(paste("Relationship of card category and", input$value)) +
          geom_histogram(position = "dodge")
      }
    }
  })
  
  output$info <- renderPrint({
    cat("The histograms can show the difference between the financial situation of the existing and attritted customer.\n")
    cat("I chose two columns in our dataset that I believe could show the customer's financial situation. The columns \n")
    cat("are Income Category and Card Category.\n\n")
    cat("The charts are intended to show how existing and attritted customers' financial situation differs and help find \n")
    cat("the common traits of churning customers.")
  })
  
  fin_table1_data <- churn %>% 
    filter(Income_Category != "Unknown") %>% 
    group_by(Income_Category, Attrition_Flag) %>% 
    summarise(count = n()) %>% 
    summarise(Ratio_of_Attritted_and_Existing_Customer  = lag(count) / count) %>% 
    filter(!is.na(Ratio_of_Attritted_and_Existing_Customer))
  
  fin_table2_data <- churn %>% 
    group_by(Card_Category, Attrition_Flag) %>% 
    summarise(count = n()) %>% 
    summarise(Ratio_of_Attritted_and_Existing_Customer  = lag(count) / count) %>% 
    filter(!is.na(Ratio_of_Attritted_and_Existing_Customer))
  
  output$fin_table1 <- renderTable({
    fin_table1_data
  })
  
  output$fin_table2 <- renderTable({
    fin_table2_data
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
