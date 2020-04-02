# A shiny app that analyses whether there are correlations among population, GDP per capita, and life expectancy. The population is the countries of the European single market.
# source of life expectancy stats: https://stats.oecd.org/index.aspx?queryid=30114
# source of gdp and population stats - various online statistics, but I can't remember the exact sources.

# global variables, packages and imports
library(shiny)
library(shinydashboard)
library(DT)
library(shinyWidgets)
library(readr)
library(ggplot2)
library(stringr)
library(scales)
gdp_life <- read_csv('gdp_life_expectancy.csv')


# make it a dashboard with two pages (one for GDP and population, the other for XXX)
ui <- dashboardPage(
    skin = "green",
   
    
    # Application title
    dashboardHeader(title = "Exploring the Correlation Coefficients Among Population, GDP per Capita and Life Expectancy"),
    
    # Show a plot of the generated distribution
    dashboardSidebar(
        setBackgroundColor(
                           color = "#f5ce42",
                           shinydashboard = TRUE
        ),
        sidebarMenu(
            menuItem(text = "Data", tabName = "data"),
            menuItem(text = "Population and GDP per Capita", tabName = "popGdp"),
            menuItem(text = "Population and Life Expectancy", tabName = "popLife")
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "data",
                    DTOutput("table1")),
            tabItem(tabName = "popGdp",
                    plotOutput("plot1"), textOutput("correlationStatement")),
            tabItem(tabName = "popLife",
                    textOutput("placeholderText"))
    )
)
)



# Define server logic required to draw a histogram
server <- function(input, output) {

    output$plot1 <- renderPlot({
        
        nice_pop <- gdp_life$population
        
        ggplot(data = gdp_life, aes(x=population, y=gdp_per_capita)) + geom_point() + scale_x_continuous(labels=label_comma()) + scale_y_continuous(labels=label_comma())
    })
    
    correl <- format(cor(gdp_life$population, gdp_life$gdp_per_capita), digits=2, scientific = FALSE)
    
    output$correlationStatement <- renderText({
        str_glue("The correlation between population and gdp_per_capita is {correl}")
    })
    
    output$placeholderText <- renderText("This page will show the correlation coefficient of population and life expectancy.")

    output$table1 <- renderDT(gdp_life)
}

# Run the application 
shinyApp(ui = ui, server = server)
