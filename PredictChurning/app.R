library(shiny)
library(tidyverse)

# Define UI for application
ui <- fluidPage(
  tabsetPanel(
    # The landing page
    tabPanel(
      "About",
      h2("Purpose"),
      h2("Source"),
      h2("Image")
    ),
    
    # The financial perspective page
    tabPanel(
      "Financial",
      sidebarLayout(
        # Sidebar layout with three groups of radio buttons
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
        
        # Main panel with text and plot
        mainPanel(
          h2("Description"),
          verbatimTextOutput("info"),
          h2("Histogram"),
          plotOutput("fin_plot")
        )
      )
    ),
    
    # The social perspective page
    tabPanel(
      "Social"
    ),
    
    # The biological perspective page
    tabPanel(
      "Biological"
    ),
    
    # The conclusion page
    tabPanel(
      "Conclusion",
      h2("Conclusion"),
      h3("Financial Triats"),
      verbatimTextOutput("fin_con"),
      tableOutput("fin_table1"),
      tableOutput("fin_table2"),
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Reading the csv file
  churn <- read_delim('BankChurners.csv')
  
  # Render the INCOME radio buttons
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
  
  # Render the CARD radio buttons  
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
  
  # Render the histograms
  output$fin_plot <- renderPlot({
    # Filter the income data according to radio button selection
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
    
    # Filter the card data based on radio  button selection
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
    
    # Draw the graph based on Income/Card selection and Overview/etc. selection
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
  
  # Render text description for financial page
  output$info <- renderPrint({
    cat("The histograms can show the difference between the financial situation of the existing and attritted customer.\n")
    cat("I chose two columns in our dataset that I believe could show the customer's financial situation. The columns \n")
    cat("are Income Category and Card Category.\n\n")
    cat("The charts are intended to show how existing and attritted customers' financial situation differs and help find \n")
    cat("the common traits of churning customers.")
  })
  
  # Filter data to show the ratio of attrited and existing customer based on income and card category
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
  
  output$fin_con <- renderPrint({
    cat("There is no clear relationship between the income level of the customer and the probability of churning. However, customers with higher card level has more probability of churn.")
  })
  
  # Render tables for conclusion page to show the relationship between income/card with churning behavior
  output$fin_table1 <- renderTable({
    fin_table1_data
  })
  
  output$fin_table2 <- renderTable({
    fin_table2_data
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
