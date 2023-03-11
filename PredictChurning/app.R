library(shiny)
library(tidyverse)

# Define UI for application
ui <- fluidPage(
  tabsetPanel(
    
    # The landing page
    tabPanel(
      "About",
      h2("Purpose"),
      verbatimTextOutput("purpose"),
      h2("Source"),
      verbatimTextOutput("source"),
      img(src = 'Churn.png')
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
      "Biological",
      sidebarLayout(
        sidebarPanel(
          
          #two radio buttons to affect the plot
          #affecting gender and education level
          radioButtons(
            "identity",
            "Select Gender",
            choices= c("M","F"),
            selected = "M"
          ),
          radioButtons(
            "identity2",
            "Select",
            choices = c("Uneducated",
                        "High School",
                        "College",
                        "Graduate",
                        "Post-Graduate",
                        "Doctorate"),
            selected = "Uneducated"
          )
        ),
        mainPanel(
          
          #the plot and description
          h2("Description"),
          verbatimTextOutput("description"),
          h2("Histogram"),
          plotOutput("bioPlot")
        )
      )
    ),
    
    # The conclusion page
    tabPanel(
      "Conclusion",
      h2("Conclusion"),
      
      # Financial part
      h3("Financial Triats"),
      verbatimTextOutput("fin_con"),
      tableOutput("fin_table1"),
      tableOutput("fin_table2"),
      
      # Social Traits
      h3("Social Traits"),
      verbatimTextOutput("social_con"),
      tableOutput("social_table1"),
      tableOutput("social_table2"),
      tableOutput("social_table3"),
      
      # Biological Part
      h3("Biological Traits"),
      verbatimTextOutput("conclusion"),
      h4("Age in relation to Churn rate"),
      tableOutput("ltData"),
      
      # Boarder Implication and Future Ideas
      h2("Boarder Implication"),
      verbatimTextOutput("impli"),
      h2("Data Quality"),
      verbatimTextOutput("quality"),
      h3("Harming Group"),
      verbatimTextOutput("harm"),
      h2("Future Ideas"),
      verbatimTextOutput("future")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Reading the csv file
  churn <- read_delim('BankChurners.csv')
  
  # ================= ABOUT  ==================
  # the purpose of our data analysis
  output$purpose <- renderPrint({
    cat("Users will purchase a credit card (or sometimes multiple) and close their account once welcome benefits have been received, but before annual fees occur.")
    cat("This is bad for banks because\nchurners don’t pay yearly interest and don’t usually pay the banks fees. This makes churners some of the bank’s least profitable customers…")
    cat("The target audience for this project is\nbank business managers, who are responsible for consumer credit card portfolios. These businesses managers are looking for the following insights from consumer credit data:\n\n")
    cat("1. What traits in customers are the key when identifying reasons for credit card churning?\n")
    cat("2. How can bank managers predict the likelihood of credit card churning prior to customers dropping off a credit card?\n")
    cat("3. How can bank managers better design credit card services to turn customers’ decisions around\n")
  })
  
  # the source of our data
  output$source <- renderPrint({
    cat("Data Source: Kaggle dataset: static .csv file named “Predicting Credit Card Customer Segmentation”")
  })

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
  
  # ==================== BIOLOGICAL ======================
  #plot to show the relation between age gender and education level
  output$bioPlot <- renderPlot({
    iden <- input$identity
    iden2 <- input$identity2
    churn %>%
      filter(Gender == iden) %>% 
      filter(!Education_Level == "Unknown") %>% 
      filter(Education_Level == iden2) %>% 
      select(c("Education_Level", "Gender", "Customer_Age", "Attrition_Flag")) %>% 
      ggplot() +
      geom_histogram(aes(x = Customer_Age, fill = Attrition_Flag),bins = 45, position="dodge2") +
      labs(title = "Amount of People By Age depending on Gender and Eduaction",
           y = "Count",
           x = 'Age')
    
  })
  
  output$description <-  renderPrint({
    #description of the histogram
    cat("This histogram analyzes traits related to biology to determine if there is any indication of whether or not the customer\n")
    cat("will churn or not. We analyze this by using the columns of Customer_Age, Gender, and Education_Level. Which helps us to\n")
    cat("better visualise these factors relations to churning.")
  })
  
  output$conclusion <-  renderPrint({
    #description of the histogram
    cat("Biological traits seem to have some indication on whether or not a customer will churn. Age seems to be slightly correlated with the highest percent of people who churn\n")
    cat("falling into the age groups of 27-37 and 57-67. However, the Biological Trait of Gender doesn't seem to be a large indicator of whether a customer will churn. This information\n")
    cat("can point to the fact that some actions are common at different ages. While gender is less accurate.")
  })
  
  output$ltData <- renderTable({
    churn %>%
      #table to show highest churn rate based on age
      filter(!Education_Level == "Unknown") %>% 
      select(c("Education_Level", "Gender", "Customer_Age", "Attrition_Flag")) %>% 
      group_by(Customer_Age, Attrition_Flag) %>% 
      reframe(count = n(), .groups = "drop") %>% 
      reframe(age = Customer_Age ,percent_who_churned = lag(count)/count) %>% 
      filter(!is.na(percent_who_churned)) %>% 
      arrange(desc(percent_who_churned)) %>% 
      head(10)
  })
  
  # =========== CONCLUSION ==============
  # Implication
  output$impli <- renderPrint({
    cat("For bank business managers, our analysis provides two courses of action for implementing tangible improvements to the design of their credit card services.\n\n")
    cat("1. When predicting churn rates, focus on traits such as transaction counts, transaction amounts, and age, while deprioritizing traits like income level and gender.\n")
    cat("2. Design goal for credit card services should be centered around increasing transaction count/amount. This means reducing annual fees, and moving away from designing services\n")
    cat("for a particular income level.")
  })
  
  # Data quality
  output$quality <- renderPrint({
    cat("The data for this project comes in the form of a static .csv file named “Predicting Credit Card Customer Segmentation”. Dataset did contain a number of unknowns, but otherwise\n")
    cat("required minimal manipulation to work with. This indicates that the data is reliable and gives unbiased results. The only reliability concern we noticed was the small relative\n")
    cat("quantity of data entries on churn incidents.")
  })
  
  # harming group
  output$harm <- renderPrint({
    cat("Insights gained from our analysis could lead to banking systems which mistake ordinary people for ‘churners’, this is a population group that stands to be inadvertently harmed\n")
    cat("as a result of our analysis.")
  })
  
  # future ideas
  output$future <- renderPrint({
    cat("Future ideas for improving the project include creating a machine learning model which uses the important traits revealed in the current version of our project. This model would\n")
    cat("then be used to predict the likelihood of a user churning.")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
