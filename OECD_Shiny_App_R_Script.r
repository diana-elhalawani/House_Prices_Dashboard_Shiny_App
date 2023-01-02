########## GENERAL INFORMATION ##########
# Data : OECD Housing Data
# Link data OECD: https://data.oecd.org/price/housing-prices.htm
# Link data "Full country names" (country equivalence 3 letter code - ISO 3166 alpha-3) : https://laendercode.net/fr/3-letter-list.html
# Link to app : https://diana-elhalawani.shinyapps.io/Evolution_of_House_Prices_By_Country_Over_Time_OECD/ 


# Packages ----
library(shiny) # Required to run any Shiny app
library(ggplot2)  # For creating pretty plots
library(data.table) # For fast aggregation of large data
library(magrittr) # It provides a mechanism for chaining commands with a forward-pipe operator : %>%
library(bslib) # For shiny app theme
library(plotly) # For adjusting the graph 

########## PREPARATION OF THE DATA ##########
# Loading data ----
HP = fread("HP.csv")

# Data Inspection
#str(HP)
#lapply(HP,unique)

# Selection of Quarterly data only
HP = HP[FREQUENCY == "Q"]

# Changing the format of dates in the TIME column so we can use in plots
HP$TIME = gsub("Q1","01-01",HP$TIME)
HP$TIME = gsub("Q2","04-01",HP$TIME)
HP$TIME = gsub("Q3","07-01",HP$TIME)
HP$TIME = gsub("Q4","10-01",HP$TIME)

# Convert the time column to class date
HP$TIME = as.Date(HP$TIME)

# Delete column 8 "Flags Codes" (it only contains "NA" values)
HP = HP[,-8]

# Create a CountryName table that has the full name of the country for each abbreviation from Full Country Name csv file
CountryName = fread("Full_country_names.csv")

# Merge the "HP" and "Country name" tables to have the full name of the country and not the abbreviation
HP = merge(HP,CountryName, by.x = 'LOCATION', by.y = 'Code ISO 3166 alpha-3', all.x=TRUE)

# Create a list of the countries in the data
Countries=unique(HP$LOCATION)

# Define the index recalculation function.
# It takes the year and subject (nominal or real) as inputs selected by the user in Shiny App later
indexRecal= function (baseYear, subject) {
  
  # Sub-select the values for baseYear and subject for each country
  # This creates a "reference table" with country and Value at that base year
  # Number of observations varies based on data availability (not all countries have data for all years)
  
  Value0 = HP[TIME==baseYear & SUBJECT == subject, .(Value,LOCATION)]
  
  # Merge the two tables using an inner join to exclude NA values
  
  HP=merge(HP,Value0,by.x="LOCATION",by.y="LOCATION",all.x=FALSE)
  
  # Calculate a new field called House_Price_Evolution by dividing the values in the 2 columns and multiplying by 100
  
  HP[SUBJECT == subject, House_Price_Evolution:= 100*Value.x/Value.y]
  
  return (HP)
  
}

# Define the ggplot in the main panel of the Shiny App
HP_plot = function (subject, baseYear, select_country_1, select_country_2, select_date_1, select_date_2, scale){
  
  # Assign the HP data with recalculated index (based on user's selection) to a new data table that will be plotted
  HP_recal = indexRecal(baseYear,subject)
  
  #Define the ggplot components
  p = ggplot( HP_recal[SUBJECT==subject],
              aes(x=TIME, y = House_Price_Evolution, group = COUNTRY)) + 
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank())+
    geom_line(colour = "grey") + 
    geom_line(data = HP_recal[COUNTRY== select_country_1 & SUBJECT == subject], 
              aes(x=TIME, y = House_Price_Evolution, col = COUNTRY),
              size = 2) +
    geom_line(data = HP_recal[COUNTRY== select_country_2 & SUBJECT == subject], 
              aes(x=TIME, y = House_Price_Evolution, col = COUNTRY),
              size = 2)+
    geom_point(data = HP_recal[COUNTRY== select_country_1 & TIME == baseYear], shape=23, fill="white", color="black", size=2) +
    theme(legend.position = "bottom")+
    theme(axis.text.x=element_text(angle=60, hjust=1)) +
    scale_x_date(limits = as.Date(c(select_date_1,select_date_2)), date_breaks = "1 year",date_labels = "%Y")
  
  # Adjust the legend's position
  p1= ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0.35, y = 1))
  
  # Enable to show the graph in logarithmic scale on the y-axis
  if (scale == "Logarithmic") {
    p1 = p +
      scale_y_log10(name = "House_Price_Evolution (logarithmic scale)",
                    limits = c(50,200))    
  }
  
  return(p1)
}

########## SHINY APP ##########
# ui.R ----
ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "minty"),
  titlePanel(("Evolution of House Prices by Country Over Time")),
  
  # Provide a drop down list to select date (base year) upon which index calculations will be made for each country
  
  wellPanel(
    selectInput(inputId = "baseYear",label= "Select Base Year for Index Calculation:",choices = sort(unique(HP$TIME)),selected = as.Date("2011-01-01"))
    
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # Header
      
      h3("Compare two countries"),
      
      # Provide a drop down list to select first country
      
      selectInput(inputId = "select_country_1",
                  label = "1. Select a country",
                  choices = sort(unique(HP$COUNTRY)),
                  selected = "France"),
      
      #Provide a drop down list to select second country
      
      selectInput(inputId = "select_country_2",
                  label = "2. Select a country",
                  choices = sort(unique(HP$COUNTRY)),
                  selected = "Japan"),
      
      # Provide a drop down list to select start year to show on x-axis
      
      selectInput(inputId = "select_date_1",
                  label = "3. Select a starting date",
                  choices = sort(unique(HP$TIME)),
                  selected = "2001-01-01"),
      
      # Provide a drop down list to select end year to show on x-axis
      
      selectInput(inputId = "select_date_2",
                  label = "4. Select an ending date",
                  choices = sort(unique(HP$TIME)),
                  selected = "2022-07-01"),
      
      # Provide a selection option to display either nominal or real data
      
      radioButtons(inputId = "select_subject",
                   label = "House price index in :",
                   choices = c("Real" = "REAL", "Nominal"="NOMINAL"),
                   selected = "REAL", inline = TRUE),
      
      # Provide a selection option to show the graph in linear or logarithmic scale
      
      radioButtons(inputId = "scale",
                   label = "Scale :",
                   choices = c("Linear","Logarithmic"),
                   selected = "Linear", inline = TRUE),
      
      # Display graph title based on the countries inputed by the user
      
      textOutput("selected_var")
      
    ),
    mainPanel(
      plotlyOutput("graph"),
      
      textOutput("source"),
      
      textOutput("comment")
    )
  )
  
)


# server.R ----
server = function(input, output,session) {
  
  # Create a ggplot using the HP_plot function defined above
  output$graph = renderPlotly({HP_plot(input$select_subject,
                                       input$baseYear,
                                       input$select_country_1,
                                       input$select_country_2,
                                       input$select_date_1,
                                       input$select_date_2,
                                       input$scale)})
  
  # Create an output text that changes with the inputs. It will be displayed on side panel
  
  output$selected_var = renderText({
    paste(input$select_subject, "house prices evolution in", input$select_country_1, "and", input$select_country_2)
  })
  
  # Create an output text showing the data source
  
  output$source = renderText("Data source : OECD")
  
  # Create an output text with further description for users
  
  output$comment = renderText("Comment : Selected dates are per quarter")
  
}


# Run the app
shinyApp(ui, server)