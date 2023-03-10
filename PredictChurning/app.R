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
      "Social",
      # Sidebar layout with three widgets to filter conditions of interest such as
      #marital status,education level and number of dependents
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput(
            "marital",
            "Choose marital status", 
            choices = c("Married","Single","Divorced"),
            selected = "Married"
          ),
          uiOutput("select"),
          uiOutput("select2")
        ),
        
        # Main panel with text and output
        mainPanel(
          h2("Description"),
          verbatimTextOutput("info2"),
          h2("Scatterplot"),
          plotOutput("plot")
        )
      )
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
      h3("Social Traits"),
      verbatimTextOutput("social_con"),
      tableOutput("social_table1"),
      tableOutput("social_table2"),
      tableOutput("social_table3")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Reading the csv file
  churn <- read_delim('BankChurners.csv')
  
  # ================= FINANCE ===================
  
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
  
  # ================= SOCIAL ===================
  
  # Render the level of education widget box input 
  output$select <- renderUI ({
    checkboxGroupInput(
      "edu", 
      "Choose education level", 
      choices = c("High School", "Graduate", "Uneducated", "College", "Post-Graduate", "Doctorate"),
      selected = "High School"
    )
  })
  
  # Render the count of dependents widget box input
  output$select2 <- renderUI ({
    checkboxGroupInput(
      "dependent", 
      "Choose count of dependents", 
      choices = c(0,1,2,3,4,5),
      selected = 0
    )
  })
  
  #Filter the data based on check box group input selection of the social identities
  #of the credit card holders. 
  mar <- reactive({
    s1 <- churn %>%
      filter(Marital_Status %in% input$marital & Education_Level %in% input$edu & Dependent_count %in% input$dependent)
  })
  
  # Draw the graph of Total transaction amount against total transaction count 
  # of the credit card holders based on filtering of the conditions of the social
  # aspects of credit card holders
  output$plot <- renderPlot({
    ggplot(data = mar(), mapping = aes(x=Total_Trans_Ct, y= Total_Trans_Amt, col = as.factor(Attrition_Flag))) + 
      ggtitle("Plot of Total Transaction Amount Against Total Transaction Count of the Credit Card Holders") +
      geom_point(aes(size = Credit_Limit))
  })
  
  ## Render text description for page on social aspects of credit card holders
  output$info2 <- renderPrint ({
    cat("This page can help bank managers analyze how social aspects such as number of dependents, education level and marital\n")
    cat("status affect customer's decision of churning. Total transaction amount is plotted in the x axis and total transaction\n")
    cat("count is plotted in the y axis. Each datapoint represents a costumer and the size of the datapoint is given by their\n")
    cat("credit card limit. Red datapoints represents card holders who churned out, green represents holders who kept using\n")
    cat("credit card services. \n\nUser can manipulate the graph by filtering the marital status, education level and counts of\n")
    cat("dependents of the credit card holders, which are the three widgets on the sidebar to make analysis on how these tags\n")
    cat("affect churning behaviors. This interactive page allows studies on specific groups of credit card holders based on their\n")
    cat("social identities. For example, if you want to study would the churning pattern be different for people who earned high\nschool degree and people who earned doctor degree.")
  })
   
  # Render text description for conclusion on social aspects of credit card holders
  output$social_con <- renderPrint({
    cat("This pattern is bserved in all marital status, level of education and counts of dependents: ")
    cat("Holders who have higher transaction counts tend to have higher transaction amounts. \nIn addition, people who have higher transaction counts and amounts are less likely to churn.\n")
    cat("\nFor people with education level of high school, there is higher possibility for them to churn with medium transaction amount and counts than people with doctorate degrees. \n")
    cat("Holders with higher counts of dependents are less likely to churn with medium transaction amount and counts, while holders with lower counts of dependent are more likely to churn \nwith medium transaction. The reason may be that holders with higher counts of dependents have to take more responsibility, so they are less likely to churn out.\n")
  })
  
  # Filter conditions of education level, marital status and number of dependents to show 
  # the ratio of attrited and existing customers. 
  social_table1_data <- churn %>%
    filter(!Education_Level == "Unknown") %>%
    group_by(Education_Level, Attrition_Flag) %>%
    summarise(count = n()) %>%
    summarise(Ratio_of_Attritted_and_Exisitng_Customer = lag(count)/count) %>%
    filter(!is.na(Ratio_of_Attritted_and_Exisitng_Customer))
  
  social_table2_data <- churn %>%
    filter(!Marital_Status == "Unknown") %>%
    group_by(Marital_Status, Attrition_Flag) %>%
    summarise(count = n()) %>%
    summarise(Ratio_of_Attritted_and_Exisitng_Customer = lag(count)/count) %>%
    filter(!is.na(Ratio_of_Attritted_and_Exisitng_Customer))
  
  social_table3_data  <- churn %>%
    filter(!Dependent_count == "Unknown") %>%
    group_by(Dependent_count, Attrition_Flag) %>%
    summarise(count = n()) %>%
    summarise(Ratio_of_Attritted_and_Exisitng_Customer = lag(count)/count) %>%
    filter(!is.na(Ratio_of_Attritted_and_Exisitng_Customer))
  
  # Render tables for conclusion page to show relationships between the social identities of
  # card holders and churning behavior
  output$social_table1 <- renderTable({
    social_table1_data
  })
  
  output$social_table2 <- renderTable({
    social_table2_data
  })
  
  output$social_table3 <- renderTable({
    social_table3_data
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
