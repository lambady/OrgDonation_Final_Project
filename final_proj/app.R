
library(shiny)
library(readr)
library(tidyverse)
library(broom)
library(gganimate)
library(plotly)
library(sf)

joined_data <- readRDS("joined_data.RDS")
geometry_data <- readRDS("geometrydata.RDS")
joined_geom <- readRDS("joined_geom.RDS")
joined_untidy <-readRDS("joined_untidy.RDS")

# Define UI for application that draws a histogram
ui <- navbarPage(
    "Organ Donation Registration in New York",
    tabPanel("Data",
             tabsetPanel(
                 tabPanel("Maps",
             fluidPage(
                 h3("How do the Demographic Characteristics of New York Counties Compare with their Organ Donor Registration Rates?"),
                 br(),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "plot_type",
                             "Choose a Demographic Factor",
                             c("Income", "Age", "Race")
                         ),
                         h6("Do age, income, or race (specifically the percent of the population that is white) correlate with registration rates?"),
                         h6("What other factors might be at play?")),
                     mainPanel(plotlyOutput("Map"),
                               plotlyOutput("Map2"))))),
                tabPanel("Plots",
            fluidPage(
                h3("Bringing Registration Rates and Demographic Information Together"),
                br(),
                sidebarLayout(
                    sidebarPanel(
                        selectInput(
                            "plot_type2",
                            "Choose a Demographic Factor",
                            c("Income", "Age", "Race")
                        ),
                        h6("Do age, income, or race (specifically the percent of the population that is white), correlate with registration rates?"),
                        h6("What other factors might be at play?")),
                    mainPanel(plotOutput("Image")))))
             )),
    tabPanel("Model", 
             fluidPage(
                 titlePanel("Statistical Analysis"),
            sidebarLayout(
                sidebarPanel(
                 h4("Explanation"),
                 p("Regression is a useful tool for better understanding this data. The graphs, which show the linear model of county 
                 registration rates by various demographic factors, can tell us how much an increase in a certain demographic factor, 
                  like average age, affects registration rates."),
                 p("Below the graph is information on a multiple regression performed to compare the effects of the demographic factors 
                   on registration rates."),
                 selectInput("regression_factor", 
                                    "Demographic Factor", 
                                    c("Income", "Age", "Race"))),
                mainPanel(
                     h2("Linear Regression"),
                     p("This graph plots demographic factors on the x-axis with
                        the percentage of a county's population that is registered as an organ donor on the y-axis."),
                     plotOutput("regression_graph"),
                     h4("Multiple Regression Information"),
                     p("This table shows the average coefficient value 
                         (slope of the regression line for percent of the populations and offsets for the other demographic factors), as well as the 5th and 95th percentile 
                         values to give an indication of uncertainty associated with 
                         the coefficient. Percent white is the reference term, while the values below it are offsets."),
                    tableOutput("regression_table"))),
             )),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("With increasing rates of disease in the kidney, lung, and liver diseases, post-mortem donation has become an important way to 
               save lives. This project analyzes rates of organ donation registration in the counties of New York state, attempting to 
               see if any demographic factors seem to be correlated with registration rates. From this analysis, it looks as though race is most 
               correlated to registration. This is consistent with other studies that have found that many minorities hold mistrust for the medical system, stemming from 
               historical injustices carried out by the medical establishment. Other important factors that have been identified as influencing the decision 
               to register as an organ donor include personal or family experience with transplanation/donation and religious beliefs."),
             p("Related Articles:
               Russell, E., Robinson, D. H., Thompson, N. J., Perryman, J. P., & Arriola, K. R. (2012). 
               Distrust in the healthcare system and organ donation intentions among African Americans. 
               Journal of community health, 37(1), 40–47. https://doi.org/10.1007/s10900-011-9413-3"),
             p("Ralph, A., Chapman, J.R., Gillis, J., Craig, J.C., Butow, P., Howard, K., Irving, M., Sutanto, B. and Tong, A. (2014), 
               Family Perspectives on Deceased Organ Donation: Thematic Synthesis of Qualitative Studies. 
               American Journal of Transplantation, 14: 923-935. doi:10.1111/ajt.12660"),
             h3("Data Source"),
             p("The demographic data (age, race, income) was collected from the American Community Survey, 2015. The donor registration
             data is from the New York State Donate Life Registry, found at health.data.ny.gov."),
             h3("About Me"),
             p("My name is Leena Ambady and I study the history of science. 
             You can reach me at lambady@college.harvard.edu.")))

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$Image <- renderPlot({
   
     joined_data %>%
           filter(year == "2015") %>%
           ggplot(aes(x = x_eligible_population_enrolled, 
                      y = fct_reorder(county, x_eligible_population_enrolled), 
                      color = case_when(
                          input$plot_type2 == "Income" ~ Median_income,
                          input$plot_type2 == "Age" ~ Median_age,
                          input$plot_type2 == "Race" ~ Perc_white
                      ))) + 
           geom_point(alpha = 0.7) +
           theme(axis.text.y = element_text(size = 5.5)) +
            scale_fill_viridis_c(direction = -1) +
            scale_color_viridis_c(direction = -1) + 
           labs(x = "% of Population Registered as an Organ Donor", 
                y = "County",
                title = "New York Organ Donation Registration Rates by County in 2015",
                subtitle = "Registration Rates Measured Monthly",
                color = case_when(
                    input$plot_type2 == "Income" ~ "Median Income in the County",
                    input$plot_type2 == "Age" ~ "Median Age in the County",
                    input$plot_type2 == "Race" ~ "Percentage of White Residents"))

    })
    
    output$regression_table <- renderTable({
        options(scipen = 999)

        joined2 <-
            joined_data %>%
            rename("APerc_white" = "Perc_white")

        model <-
            lm(x_eligible_population_enrolled ~ APerc_white + Median_income + Median_age, data = joined2)

        model %>%
            tidy(conf.int = TRUE) %>%
            mutate("Coefficient" = round(estimate, 3),
                   "Upper Bound" = round(conf.high, 3),
                   "Lower Bound" = round(conf.low, 3)) %>%
            select(term, Coefficient, 'Lower Bound', 'Upper Bound')
        })
    
    # Need to change/reverse the colors 
    
    output$Map <- renderPlotly ({
       geometry_data2 <- geometry_data %>%
            mutate(Perc_white = (estimate/summary_est) *100) 
       
       p <- geometry_data2 %>%
            filter(variable == case_when(
                input$plot_type == "Income" ~ "Median_income",
                input$plot_type == "Age" ~ "Median_age",
                input$plot_type == "Race" ~ "Number_white")) %>%
            ggplot(aes(fill =  ifelse(variable == "Number_white", Perc_white, estimate),
                       geometry = geometry, 
                       text = case_when(input$plot_type == "Income" ~ paste(county, "County, $", estimate),
                                        input$plot_type == "Age" ~ paste(county, "County,", estimate, "years old"),
                                        input$plot_type == "Race" ~ paste(county, "County,", round(Perc_white, 2), "% White")))) +
            geom_sf() +
            labs(fill = case_when(
                input$plot_type == "Income" ~ "Median Income",
                input$plot_type == "Age" ~ "Median Age",
                input$plot_type == "Race" ~ "Percentage of White Residents"),
                 title = "Demographics in New York by County, 2015",
                 subtitle = "Source: American Communities Survey, 2015") +
            theme_classic() +
            theme(axis.line = element_blank()) +
            theme(axis.text = element_blank()) +
            theme(axis.ticks = element_blank()) +
            scale_fill_viridis_c(direction = -1) +
            scale_color_viridis_c(direction = -1)
        
        ggplotly(p, tooltip = "text") 
        
    })
    
    output$Map2 <- renderPlotly ({  
        h <- joined_geom %>%
            ggplot(aes(fill = x_eligible_population_enrolled,
                       geometry = geometry, 
                       text = paste(county, "County,", x_eligible_population_enrolled, "% Registered"))) +
            geom_sf() +
            labs(fill = "Percent Registered",
                 title = "Percent of the Population Registered as an Organ Donor, 2015",
                 subtitle = "Source: HealthData.gov, 2015") +
            theme_classic() +
            theme(axis.line = element_blank()) +
            theme(axis.text = element_blank()) +
            theme(axis.ticks = element_blank()) +
            scale_fill_viridis_c(direction = -1) +
            scale_color_viridis_c(direction = -1)
        
        ggplotly(h, tooltip = "text")
           # # style(hoverinfo = "text",
           # #              hovertext = paste("% Registered", joined_geom$x_eligible_population_enrolled))
         

       
    })
 
    output$regression_graph <- renderPlot({
        joined_data %>%
            filter(year == "2015") %>%
            ggplot(aes(x = case_when(
                input$regression_factor == "Income" ~ Median_income,
                input$regression_factor == "Age" ~ Median_age,
                input$regression_factor == "Race" ~ Perc_white), 
                       y = x_eligible_population_enrolled)) + 
            geom_point() +
            geom_smooth(method = "lm", se = TRUE) +
            labs(x = case_when(
                input$regression_factor == "Income" ~ "Median Income",
                input$regression_factor == "Age" ~ "Median Age",
                input$regression_factor == "Race" ~ "Percent White"),
                y = "Percent of County Population Registered as an Organ Donor",
                title = "Linear Regression: County Organ Donation Registration Rates by Demographic Factors")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
