# P-P-shiny-dashboard
This is a Shiny web application designed to analyze and visualize sales, billing, and footfall data across multiple locations. The app provides interactive dashboards using plotly and ggplot2 to explore daily sales trends, market shares, footfall patterns, and more.

**Live Demo:** https://pand-dashboard.shinyapps.io/sales_data/   
**GitHub Repository:** 

## Features:
**Daily Sales by Location:** Track daily sales with custom date ranges.  
**Monthly Total Sales Trend:** Visualize monthly sales trends for different locations.  
**Weekly vs Weekend Average Sales:** Compare weekly and weekend sales patterns.  
**Holiday vs Non-Holiday Sales:** Analyze sales differences between holidays and regular days.  
**Market Share Analysis:** Explore the market share of different locations.  
**Average Bill Value:** Calculate and visualize average bill values per location.  
**Monthly Footfall:** Track footfall trends across locations.  

## Setup Instructions:
1. Install R and RStudio if you haven’t already.  
2. Install required libraries: install.packages(c("shiny", "dplyr", "plotly", "tidyr", "tidyverse", "rsconnect"))  
3. Open app.R in RStudio and click Run App.

## How to Use
1. Launch the app as described above.  
2. Navigate between tabs to explore different dashboards.  
3. Use dropdowns and sliders to filter data by location, month, and date.

## Data Assumptions & Processing
1. Missing values are replaced with 0.
2. Sales amounts are converted to numeric by removing commas.
3. Months are displayed using their full names.
4. Currency is displayed as LKR (Sri Lankan Rupees).
