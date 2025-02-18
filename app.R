#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)
library(plotly)
library(tidyr)
library(tidyverse)
library(rsconnect)

# Load the data
sales_data <- read.csv("Sales.csv")
bills_data <- read.csv("Bills.csv")
footfall_data <- read.csv("Walk_Ins.csv")

sales_data[is.na(sales_data)] <- 0
bills_data[is.na(bills_data)] <- 0
footfall_data[is.na(footfall_data)] <- 0

# Convert the Date column to Date format
sales_data$Date <- as.Date(sales_data$Date, format = "%m/%d/%Y")

# Add 'MonthName' to the dataset based on 'Month' column
sales_data$MonthName <- month.name[sales_data$Month]

# Define UI
tab_page1 <- tabPanel(
  "Daily Sales by Location",
  titlePanel("Daily Sales by Location and Month"),
  sidebarLayout(
    sidebarPanel(
      selectInput("firstchoice", "Select the location:", 
                  choices = list("C5", "Himbutana", "C7", "Makola")),
      
      selectInput("secondchoice", "Select the month:", 
                  choices = month.name),
      
      uiOutput("date_slider")
    ),
    mainPanel(
      plotlyOutput("timeseries")
    )
  )
)

tab_page2 <- tabPanel(
  "Total Sales Trend",
  titlePanel("Monthly Total Sales Trend by Location"),
  sidebarLayout(
    sidebarPanel(
      selectInput("location", "Select the location:", 
                  choices = list("C5", "Himbutana", "C7", "Makola", "Nurseries","Online"))
    ),
    mainPanel(
      plotlyOutput("total_sales_trend")
    )
  )
)

tab_page3 <- tabPanel(
  "Weekly vs Weekend Average Sales",
  titlePanel("Weekly Average Sales vs Weekend Average Sales"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "location_choice", 
                  label = "Select Location:", 
                  choices = c("C5", "Himbutana", "C7", "Makola", "Online"), 
                  selected = "C5"),
      selectInput(inputId = "month_choice", 
                  label = "Select Month:",
                  choices = month.name, # All months for the user to choose
                  selected = "January") # Default to January
    ),
    mainPanel(
      plotlyOutput("week_avg_vs_weekend_avg")
    )
  )
)

tab_page4 <- tabPanel(
  "Holiday vs Non-Holiday Average Sales",
  titlePanel("Holiday Average Sales vs Non-Holiday Average Sales"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "location_choice_4", 
                  label = "Select Location:", 
                  choices = c("C5", "Himbutana", "C7", "Makola", "Online"), 
                  selected = "C5"),
      selectInput(inputId = "month_choice_4", 
                  label = "Select Month:",
                  choices = month.name, # All months for the user to choose
                  selected = "January") # Default to January
    ),
    mainPanel(
      plotlyOutput("holiday_vs_non_holiday_avg")
    )
  )
)

tab_page5 <- tabPanel(
  "Market Share by Location",
  titlePanel("Location-Wise Market Share"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "month_choice_5", 
                  label = "Select Month:",
                  choices = month.name, # All months for the user to choose
                  selected = "January") # Default to January
    ),
    mainPanel(
      plotlyOutput("market_share_pie")
    )
  )
)

tab_page6 <- tabPanel(
  "Average Bill Value by Location",
  titlePanel("Location-Wise Average Bill Value"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "month_choice_avg",
        label = "Select Month:",
        choices = month.name, 
        selected = "January"
      ),
      selectInput(
        inputId = "location_choice_avg",
        label = "Select Location:",
        choices = c("C5", "Himbutana", "C7", "Makola", "Nursery"),
        selected = "C5"
      )
    ),
    mainPanel(
      plotlyOutput("avg_bill_plot")
    )
  )
)


tab_page7 <- tabPanel(
  "Monthly Footfall by Location",
  titlePanel("Monthly Footfall by Location"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "month_choice_footfall",
        label = "Select Month:",
        choices = month.name,
        selected = "January"
      ),
      selectInput(
        inputId = "location_choice_footfall",
        label = "Select Location:",
        choices = c("C5", "Himbutana", "C7", "Makola", "Nawana", "Horakelle"),
        selected = "Colombo 5"
      )
    ),
    mainPanel(
      plotlyOutput("footfall_plot")
    )
  )
)


ui <- navbarPage("Sales Data", tab_page1, tab_page2, tab_page3, tab_page4, 
                 tab_page5, tab_page6, tab_page7)

# Define server logic
server <- function(input, output, session) {
  
  # Tab 1: Daily Sales by Location
  output$date_slider <- renderUI({
    req(input$secondchoice)
    
    # Get the start and end dates for the selected month
    month_data <- sales_data %>%
      filter(MonthName == input$secondchoice)
    
    min_date <- min(month_data$Date)
    max_date <- max(month_data$Date)
    
    sliderInput("slider", "Select the date range:",
                min = min_date, max = max_date, 
                value = c(min_date, max_date), 
                timeFormat = "%b %d, %Y")
  })
  
  output$timeseries <- renderPlotly({
    req(input$firstchoice, input$secondchoice, input$slider)
    
    location_column <- input$firstchoice
    
    # Filter data based on input choices
    filtered_data <- sales_data %>%
      filter(MonthName == input$secondchoice,
             Date >= input$slider[1], Date <= input$slider[2]) %>%
      select(Date, Week, location_column) %>%
      rename(Sales = !!location_column)
    
    filtered_data$Sales <- as.numeric(gsub(",", "", filtered_data$Sales))
    
    # Get the start of each week for reference lines
    week_lines <- filtered_data %>%
      group_by(Week) %>%
      summarize(StartOfWeek = min(Date))
    
    # Add custom hover labels
    filtered_data <- filtered_data %>%
      mutate(HoverText = paste(
        "Date:", format(Date, "%b %d, %Y"),
        "<br>Sales: LKR", scales::comma(Sales)
      ))
    
    # Create the plot with hover customization
    p <- plot_ly(
      filtered_data,
      x = ~Date,
      y = ~Sales,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(color = 'blue'),
      marker = list(size = 6, color = 'blue'),
      text = ~HoverText,
      hoverinfo = "text"  # Use custom hover text
    ) %>%
      layout(
        title = paste("Daily Sales for", input$firstchoice, "in", input$secondchoice),
        xaxis = list(title = "Date", showgrid = TRUE),
        yaxis = list(
          title = "Sales (LKR)", 
          showgrid = TRUE, 
          tickformat = "₹,0.0f", 
          tickprefix = "LKR "
        )
      )
    
    # Add vertical lines for start of weeks
    for (i in seq_len(nrow(week_lines))) {
      p <- p %>%
        add_lines(
          x = c(week_lines$StartOfWeek[i], week_lines$StartOfWeek[i]),
          y = c(min(filtered_data$Sales, na.rm = TRUE), max(filtered_data$Sales, na.rm = TRUE)),
          line = list(color = "red", dash = "dash"),
          inherit = FALSE,
          showlegend = FALSE
        )
    }
    
    p
  })
  
  
  
  # Tab 2: Monthly Total Sales Trend
  output$total_sales_trend <- renderPlotly({
    req(input$location)
    
    location_column <- input$location
    
    # Grouping and summarizing data
    total_sales_data <- sales_data %>%
      group_by(MonthName, Month) %>%
      summarize(
        TotalSales = sum(as.numeric(gsub(",", "", !!sym(location_column))), na.rm = TRUE)
      ) %>%
      arrange(Month)  # Ensure data is sorted by Month
    
    # Create a ggplot
    ggplot_plot <- ggplot(total_sales_data, aes(x = MonthName, y = TotalSales, group = 1)) +
      geom_point(aes(text = paste("Total Sales:", scales::dollar_format(prefix = "LKR ")(TotalSales))),
                 color = "blue", size = 2) +  # Reduce dot size
      geom_line(color = "blue", size = 0.7) +  # Reduce line thickness
      labs(
        title = paste("Monthly Total Sales Trend for", input$location),
        x = "Month",
        y = "Total Sales (LKR)"
      ) +
      scale_x_discrete(limits = month.name) +  # Order months correctly
      scale_y_continuous(labels = scales::dollar_format(prefix = "LKR ")) +  # Format y-axis as currency
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    # Convert ggplot to plotly with custom tooltips
    ggplotly(ggplot_plot, tooltip = "text")
  })
  
  
  # Tab 3: Weekday vs. Weekend Sales
  output$week_avg_vs_weekend_avg <- renderPlotly({
    # Filter data for the selected location and month
    filtered_data <- sales_data %>%
      select(Date, IS_Weekend, Month, Week, input$location_choice) %>%
      mutate(
        Sales = as.numeric(gsub(",", "", .data[[input$location_choice]])),
        MonthName = factor(Month, labels = month.name)
      ) %>%
      filter(MonthName == input$month_choice) %>% # Filter based on selected month
      group_by(MonthName, Week, IS_Weekend) %>%
      summarize(WeeklySales = sum(Sales, na.rm = TRUE), .groups = "drop") %>%
      mutate(WeekendLabel = ifelse(IS_Weekend == 1, "Weekend", "Weekday"))
    
    # Calculate average sales for weekdays and weekends
    weekly_avg <- filtered_data %>%
      group_by(MonthName, Week) %>%
      summarize(AvgWeeklySales = mean(WeeklySales, na.rm = TRUE), .groups = "drop")
    
    weekend_avg <- filtered_data %>%
      filter(WeekendLabel == "Weekend") %>%
      group_by(MonthName, Week) %>%
      summarize(AvgWeekendSales = mean(WeeklySales, na.rm = TRUE), .groups = "drop")
    
    # Join weekly and weekend averages
    avg_data <- left_join(weekly_avg, weekend_avg, by = c("MonthName", "Week"))
    
    # Create the plot
    ggplot_plot <- ggplot(avg_data, aes(x = Week)) +
      geom_line(aes(y = AvgWeeklySales, color = "Weekly Average"), size = 0.7) +
      geom_line(aes(y = AvgWeekendSales, color = "Weekend Average"), size = 0.7) +
      geom_point(
        aes(
          y = AvgWeeklySales, 
          color = "Weekly Average", 
          text = paste("Avg Weekly Sales:", scales::dollar_format(prefix = "LKR ")(AvgWeeklySales))
        ), 
        size = 2
      ) +
      geom_point(
        aes(
          y = AvgWeekendSales, 
          color = "Weekend Average", 
          text = paste("Avg Weekend Sales:", scales::dollar_format(prefix = "LKR ")(AvgWeekendSales))
        ), 
        size = 2
      ) +
      labs(
        title = paste("Weekly Avg Sales vs Weekend Avg Sales for", input$location_choice),
        x = "Week",
        y = "Sales (LKR)",
        color = "Sales Type"
      ) +
      scale_y_continuous(labels = scales::dollar_format(prefix = "LKR ")) +
      scale_color_manual(values = c("Weekly Average" = "blue", "Weekend Average" = "red")) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "top"
      )
    
    # Convert to interactive plot with custom tooltips
    ggplotly(ggplot_plot, tooltip = "text")
  })
  
  
  # Tab 4: Holiday vs. Regular Day Sales
  output$holiday_vs_non_holiday_avg <- renderPlotly({
    # Filter data for the selected location and month
    filtered_data <- sales_data %>%
      select(Date, IS_Holiday, Month, input$location_choice_4) %>%
      mutate(
        Sales = as.numeric(gsub(",", "", .data[[input$location_choice_4]])),
        MonthName = factor(Month, labels = month.name)
      ) %>%
      filter(MonthName == input$month_choice_4) # Filter based on selected month
    
    # Calculate average sales for holidays and non-holidays
    avg_sales_data <- filtered_data %>%
      group_by(MonthName, IS_Holiday) %>%
      summarize(AverageSales = mean(Sales, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        HolidayType = ifelse(IS_Holiday == 1, "Holiday", "Non-Holiday")
      )
    
    # Create the plot
    ggplot_plot <- ggplot(avg_sales_data, aes(x = HolidayType, y = AverageSales, fill = HolidayType)) +
      geom_bar(
        stat = "identity", 
        position = "dodge", 
        show.legend = FALSE,
        aes(
          text = paste(
            "Average Sales:", scales::dollar_format(prefix = "LKR ")(AverageSales)
          )
        )
      ) +
      labs(
        title = paste("Holiday vs Non-Holiday Average Sales for", input$location_choice_4),
        x = "Holiday Type",
        y = "Average Sales (LKR)"
      ) +
      scale_y_continuous(labels = scales::dollar_format(prefix = "LKR ")) +
      scale_fill_manual(values = c("Holiday" = "red", "Non-Holiday" = "blue")) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)
      )
    
    # Convert to interactive plot with custom tooltips
    ggplotly(ggplot_plot, tooltip = "text")
  })
  
  
  # Tab 5: Location wise market share
  output$market_share_pie <- renderPlotly({
    # Filter data for the selected month
    filtered_data <- sales_data %>%
      select(Date, Month, C5, Himbutana, C7, Makola, Online, Nurseries) %>%
      mutate(
        MonthName = factor(Month, labels = month.name),
        C5 = as.numeric(gsub(",", "", C5)),
        Himbutana = as.numeric(gsub(",", "", Himbutana)),
        C7 = as.numeric(gsub(",", "", C7)),
        Makola = as.numeric(gsub(",", "", Makola)),
        Online = as.numeric(gsub(",", "", Online)),
        Nurseries = as.numeric(gsub(",", "", Nurseries))
      ) %>%
      filter(MonthName == input$month_choice_5) # Filter based on selected month
    
    # Summarize total sales by location
    market_share_data <- filtered_data %>%
      summarize(
        C5 = sum(C5, na.rm = TRUE),
        Himbutana = sum(Himbutana, na.rm = TRUE),
        C7 = sum(C7, na.rm = TRUE),
        Makola = sum(Makola, na.rm = TRUE),
        Online = sum(Online, na.rm = TRUE),
        Nurseries = sum(Nurseries, na.rm = TRUE)
      ) %>%
      pivot_longer(cols = c(C5, Himbutana, C7, Makola, Online, Nurseries), 
                   names_to = "Location", values_to = "TotalSales") %>%
      mutate(
        MarketShare = TotalSales / sum(TotalSales) * 100
      )
    
    # Create the pie chart
    pie_chart <- plot_ly(
      market_share_data, 
      labels = ~Location, 
      values = ~MarketShare, 
      type = "pie",
      textinfo = "label+percent",
      hoverinfo = "text",
      text = ~paste(
        "Location:", Location,
        "<br>Market Share:", round(MarketShare, 2), "%",
        "<br>Total Sales: LKR", scales::comma(TotalSales)
      ),
      marker = list(colors = c("red", "blue", "green", "orange", "grey", "purple"))
    ) %>%
      layout(
        title = paste("Market Share by Location for", input$month_choice_5),
        showlegend = TRUE
      )
    
    pie_chart
  })
  
  # Tab 6: Location wise average bill value
  output$avg_bill_plot <- renderPlotly({
    req(input$month_choice_avg, input$location_choice_avg) # Ensure inputs are available
    
    # Prepare and filter data
    avg_bill_data <- bills_data %>%
      mutate(
        Date = as.Date(Date, format = "%m/%d/%Y"), # Convert Date column
        MonthName = factor(Month, levels = 1:12, labels = month.name), # Map month numbers to names
        Sales = as.numeric(gsub(",", "", .[[paste0(input$location_choice_avg, "_Sale")]])), # Dynamic sales column
        Bills = .[[paste0(input$location_choice_avg, "_Bills")]], # Dynamic bills column
        Avg_Bill_Value = ifelse(Bills > 0, Sales / Bills, NA) # Handle division by zero
      ) %>%
      filter(MonthName == input$month_choice_avg) %>% # Filter by selected month
      arrange(Date) # Sort by date
    
    # Create the ggplot
    plot <- ggplot(avg_bill_data, aes(x = Date, y = Avg_Bill_Value, group = 1)) +
      geom_point(
        aes(text = paste(
          "Date:", Date,
          "<br>Average Bill Value:", scales::dollar_format(prefix = "LKR ")(Avg_Bill_Value)
        )),
        color = "blue", size = 2 # Add points with hover labels
      ) +
      geom_line(color = "blue", size = 0.7) + # Add line to connect points
      labs(
        title = paste("Daily Average Bill Value for", input$location_choice_avg, "in", input$month_choice_avg),
        x = "Date",
        y = "Average Bill Value (LKR)"
      ) +
      scale_y_continuous(labels = scales::dollar_format(prefix = "LKR ")) + # Format y-axis
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)
      )
    
    # Convert ggplot to Plotly for interactivity
    ggplotly(plot, tooltip = "text")
  })
  
  # Tab 7: Location wise footfall
  output$footfall_plot <- renderPlotly({
    # Ensure required inputs are available
    req(input$month_choice_footfall, input$location_choice_footfall)
    
    # Filter and prepare data dynamically within the reactive context
    footfall_data_filtered <- footfall_data %>%
      mutate(
        Date = as.Date(Date, format = "%m/%d/%Y"),  # Convert Date column to Date format
        MonthName = factor(Month, levels = 1:12, labels = month.name),  # Map numeric months to month names
        Footfall = as.numeric(.data[[input$location_choice_footfall]])  # Ensure Footfall column is numeric
      ) %>%
      filter(MonthName == input$month_choice_footfall) %>%  # Filter data for the selected month
      select(Date, Footfall)  # Select relevant columns
    
    # Create the plot
    footfall_plot <- ggplot(footfall_data_filtered, aes(x = Date, y = Footfall)) +
      geom_point(aes(text = paste("Footfall:", Footfall)), color = "darkgreen", size = 2) +  # Add points
      geom_line(color = "darkgreen", size = 1) +  # Add line
      labs(
        title = paste("Daily Footfall for", input$location_choice_footfall, "in", input$month_choice_footfall),
        x = "Date",
        y = "Footfall"
      ) +
      scale_y_continuous(labels = scales::comma_format()) +  # Format y-axis with commas
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10)
      )
    
    # Convert ggplot to Plotly with tooltips
    ggplotly(footfall_plot, tooltip = "text")
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)









