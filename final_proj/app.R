
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

# Define UI for application 
ui <- navbarPage(
    "Organ Donation Registration in New York",
    
    # I want three tabs, data, Model, and about 
    
    tabPanel("Data",
             tabsetPanel(
    # I also want subtabs for this page, one displaying maps and one plots
                 
                 tabPanel("Maps",
             fluidPage(
                 h3("How do the Demographic Characteristics of New York Counties Compare with their Organ Donor Registration Rates?"),
                 br(),
                 sidebarLayout(
                     sidebarPanel(
                         
    # Formatting the dropdown options 
                         selectInput(
                             "plot_type",
                             "Choose a Demographic Factor",
                             c("Income", "Age", "Race")
                         ),
                         
    # I want some prompting questions underneath the dropdown
    
                         h6("Do age, income, or race (specifically the percent of the population that is white) correlate with registration rates?"),
                         h6("What other factors might be at play?")),
    
    # I will have two maps diplay on this page 
    
                     mainPanel(plotlyOutput("Map"),
                               plotlyOutput("Map2"))))),
                tabPanel("Plots",
            fluidPage(
                h3("Bringing Registration Rates and Demographic Information Together"),
    
    # added a break for aesthetic reasons
                br(),
                sidebarLayout(
                    sidebarPanel(
    
    # Similar process to make the dropdowns
                        
                        selectInput(
                            "plot_type2",
                            "Choose a Demographic Factor",
                            c("Income", "Age", "Race")
                        ),
                        h6("Do age, income, or race (specifically the percent of the population that is white), correlate with registration rates?"),
                        h6("What other factors might be at play?")),
    
    # I want to change this plot later, but for now it is fine
    
                    mainPanel(plotOutput("Image")))))
             )),
    
    # My second tab will have my regression information 
    
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
                 
    # Drop down for the plots on this page
            
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
    
    # This tab was more straightfoward, just text 
    
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

# Define server logic 
server <- function(input, output) {
    output$Image <- renderPlot({
        
 joined_data %>%

 # Just looking at data from 2015 here
            
           filter(year == "2015") %>%
            
 # Have to use case_when a lot here to format things differently for my
 # different inputs/dropdown options. Not sure if this is the most efficient
 # way, but it works
            
           ggplot(aes(x = x_eligible_population_enrolled, 
                      y = fct_reorder(county, x_eligible_population_enrolled), 
                      color = case_when(
                          input$plot_type2 == "Income" ~ Median_income,
                          input$plot_type2 == "Age" ~ Median_age,
                          input$plot_type2 == "Race" ~ Perc_white
                      ))) + 
           geom_point(alpha = 0.7) +
           theme(axis.text.y = element_text(size = 5.5)) +
    
    # So the higher numbers are darker 
            
            scale_fill_viridis_c(direction = -1) +
            scale_color_viridis_c(direction = -1) + 
           labs(x = "% of Population Registered as an Organ Donor", 
                y = "County",
                title = "New York Organ Donation Registration Rates by County in 2015",
                subtitle = "Registration Rates Measured Monthly",
                
    # Also have to use case_when so the label of the legend changes based on the
    # dropdown option
    
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
    
    output$Map <- renderPlotly ({
        p<- geometry_data %>%
            
    # Want to get the percent white, use this as my "Race" category 
            
            mutate(Perc_white = (estimate/summary_est) *100) %>%
    
    # Largely same logic as the plot, need to use case_when a lot 
            filter(variable == case_when(
                input$plot_type == "Income" ~ "Median_income",
                input$plot_type == "Age" ~ "Median_age",
                input$plot_type == "Race" ~ "Number_white")) %>%
        
    # Here, I had to use ifelse because perc_white was a new column whereas the
    # Median_age and Median_income numbers were in one column together, estimate
            
            ggplot(aes(fill =  ifelse(variable == "Number_white", Perc_white, estimate),
                       geometry = geometry, 
    
    # Had some trouble formatting what text I wanted to appear during the hover, but this fixed it 
    
                       text = case_when(input$plot_type == "Income" ~ paste(county, "County, $", estimate),
                                        input$plot_type == "Age" ~ paste(county, "County,", estimate, "years old"),
                                        input$plot_type == "Race" ~ paste(county, "County,", round(Perc_white, 2), "% White")))) +
            geom_sf() +
    
    # Labels dependent on the dropdown option 
            
            labs(fill = case_when(
                input$plot_type == "Income" ~ "Median Income",
                input$plot_type == "Age" ~ "Median Age",
                input$plot_type == "Race" ~ "Percentage of White Residents"),
                 title = "Demographics in New York by County, 2015",
                 subtitle = "Source: American Communities Survey, 2015") +
            
    # getting rid of the extra background on the map
            
            theme_classic() +
            theme(axis.line = element_blank()) +
            theme(axis.text = element_blank()) +
            theme(axis.ticks = element_blank()) +
            
    # Higher incomes, ages, % white will be darker 
            
            scale_fill_viridis_c(direction = -1) +
            scale_color_viridis_c(direction = -1)
        
        ggplotly(p, tooltip = "text") 
        
    })
    
    output$Map2 <- renderPlotly ({  
        h <- joined_geom %>%
            ggplot(aes(fill = x_eligible_population_enrolled,
                       geometry = geometry, 
            
     # Needed to use paste to tailor this label 
                       text = paste(county, "County,", x_eligible_population_enrolled, "% Registered"))) +
            geom_sf() +
            labs(fill = "Percent Registered",
                 title = "Percent of the Population Registered as an Organ Donor, 2015",
                 subtitle = "Source: HealthData.gov, 2015") +
            theme_classic() +
    
    # Again, wanted to get rid of any distractions on the map and keep it simple 
            
            theme(axis.line = element_blank()) +
            theme(axis.text = element_blank()) +
            theme(axis.ticks = element_blank()) +
            scale_fill_viridis_c(direction = -1) +
            scale_color_viridis_c(direction = -1)
        
        ggplotly(h, tooltip = "text")
        
        # I might come back to the highlight, but for right now I won't use it 
           # # style(hoverinfo = "text",
           # #              hovertext = paste("% Registered", joined_geom$x_eligible_population_enrolled))
         

       
    })
 
    output$regression_graph <- renderPlot({
        joined_data %>%
            
    # Want to be able to look at the regression for % registered vs. income, age, and race. 
            
            ggplot(aes(x = case_when(
                input$regression_factor == "Income" ~ Median_income,
                input$regression_factor == "Age" ~ Median_age,
                input$regression_factor == "Race" ~ Perc_white), 
                       y = x_eligible_population_enrolled)) + 
            
    # Had to change year to a factor so the color wouldn't be continuous
            geom_point(aes(color = as.factor(year))) +
            geom_smooth(method = "lm", se = TRUE) +
            
    # Making the labels dependent on drop down choice 
            
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
