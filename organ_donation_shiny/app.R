
library(shiny)
library(readr)
library(tidyverse)
library(broom)
library(gganimate)
library(plotly)
library(sf)
library(shinythemes)
library(gt)

# All my cleaned datasets loaded in 

joined_data <- readRDS("joined_data.RDS")
geometry_data <- readRDS("geometrydata.RDS")
joined_geom <- readRDS("joined_geom.RDS")
joined_untidy <-readRDS("joined_untidy.RDS")
growth_rates <- readRDS("growth_rates.RDS")

# Define UI for application 
ui <-
    navbarPage(
    "Organ Donation Registration in New York",
    
    # I want three tabs, data, Model, and about 
    
    tabPanel("Data",
             tabsetPanel(
    # I also want subtabs for this page, one displaying maps and one plots
                 
                 tabPanel("Maps",
             fluidPage(
                 h3("Map Comparison: How do the Demographic Characteristics of Counties Compare with their Organ Donor Registration Rates?"),
                 br(),
                 sidebarLayout(
                     sidebarPanel(
                         
    # Formatting the dropdown options 
                         p("This project seeks to look at organ donor registration rates across different demographic groups.
                           Choose a factor below to explore how demographic patterns compare to registration patterns."),
                         selectInput(
                             "plot_type",
                             "Choose a demographic factor to display on the map",
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
                h3("Visualizing Registration Rates and Growth"),
                p("The first plot gives us a sense of the variation of registration rates in New York counties
                  in 2015, and how demographic factors like income, age, and the percentage of the population that is White
                  map on to these rates."),
                p("But it is also interesting to look at the change, or growth in registration rates. Organ procurement organizations are 
                  responsible for educating citizens about organ donation and encouraging them to donate. According to growth rates from
                  2008-2016, are rates increasing more rapidly in certain counties than in others?"),
    
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
    
                    mainPanel(plotlyOutput("Image"),
                              plotlyOutput("growth_graph")))))
             )),
    
    # My second tab will have my regression information 
    
    tabPanel("Model", 
             fluidPage(
            sidebarLayout(
                sidebarPanel(
                 h4("Explanation"),
                 p("Regression is a useful tool for better understanding this data. The graphs, which show the linear model of county 
                 registration rates by various demographic factors, can tell us how much an increase in a certain demographic factor, 
                  like average age, is associated with a change in registration rates."),
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
                     p("This table shows the intercept, coefficients, and interaction terms for a multiple regression which takes % of the eligible population
                       registered as the outcome, and age, income, and % of the population that is white as explanatory variables. The values indicate
                       that for every one unit increase in age and race, there is about a 2% increase in registration. However, note
                       that the confidence interval around age is quite wide."),
                    gt_output("regression_table"),
                    br(),
                    h4("Correlation Information"),
                    h5("What is the correlation between county demographic factors and organ donor registration rates?"),
                    br(),
                    gt_output("correlation_table"))))),
    
    # This tab was more straightfoward, just text 
    
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("With increasing rates of disease in the kidney, lung, and liver diseases, post-mortem donation has become an important way to 
               save lives. This project analyzes rates of organ donation registration in the counties of New York state, attempting to 
               see if any demographic factors seem to be correlated with registration rates."),
             p("From my analysis, it looks as though race is most 
               correlated to registration. This is consistent with other studies that have found that many minorities hold mistrust for the medical system, stemming from 
               historical injustices carried out by the medical establishment. Other important factors that have been identified as influencing the decision 
               to register as an organ donor include personal or family experience with transplanation/donation and religious beliefs."),
             p("In this analysis, I chose to use % of the population that is white as a proxy race, instead of looking at numbers for 
               several different races. I made this decision because I really wanted to focus on organ donation rates in minorities as compared to
               others, but espeically after visualizig my data I do think that it would be fruitful in the future to do a more through and granular
               analysis of race."),
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
             p("My name is Leena Ambady, I'm from California and I study the history of science. I'm excited to learn more about R and Shiny, and 
             you can reach me at lambady@college.harvard.edu."),
             p("Code for my project can be found here: https://github.com/lambady/OrgDonation_Final_Project")))

# Define server logic 
server <- function(input, output) {
    
# How the county registration rates compare? Is there any pattern with race, income, or age? 
    output$Image <- renderPlotly({
        
 u <-joined_data %>%

 # Just looking at data from 2015 here
            
          filter(year == "2016") %>%
        filter(month == "11") %>%
            
 # Have to use case_when a lot here to format things differently for my
 # different inputs/dropdown options. Not sure if this is the most efficient
 # way, but it works
            
           ggplot(aes(y = x_eligible_population_enrolled, 
                      x = fct_reorder(county, x_eligible_population_enrolled), 
                      fill = case_when(
                          input$plot_type2 == "Income" ~ Median_income,
                          input$plot_type2 == "Age" ~ Median_age,
                          input$plot_type2 == "Race" ~ Perc_white),
                      text = case_when(
                          input$plot_type2 == "Income" ~ 
                              paste(county, "County: $", Median_income),
                          input$plot_type2 == "Age" ~ 
                              paste(county, "County:", Median_age, "years old"),
                          input$plot_type2 == "Race" ~
                              paste(county, "County:", round(Perc_white, 2), "% white")
                      ))) + 
           geom_col() +
        # Rotating the axis labels so they are more legible 
            
        theme(axis.text.x = element_text(size = 5.5, angle = 60)) +
    
    # So the higher numbers are darker 
            
            scale_fill_viridis_c(direction = -1) +
            scale_color_viridis_c(direction = -1) + 
           labs(y = "% of Population Registered", 
                x = "County",
                title = "New York Organ Donation Registration Rates by County in 2015",
                
    # Also have to use case_when so the label of the legend changes based on the
    # dropdown option
    
                fill = case_when(
                    input$plot_type2 == "Income" ~ "Median Income in the County",
                    input$plot_type2 == "Age" ~ "Median Age in the County",
                    input$plot_type2 == "Race" ~ "Percentage of White Residents"))
 
 ggplotly(u, tooltip = "text")
    

    })
    
# Displaying the coefficient and confidence interval for my multiple regression 
    
    output$regression_table <- render_gt({
        options(scipen = 999)

        joined2 <-
            joined_data %>%
            filter(month == "11") 

        model <-
            lm(x_eligible_population_enrolled ~ Perc_white * Median_income * Median_age, data = joined2)

        model %>%
            tidy(conf.int = TRUE) %>%
            mutate("Coefficient" = round(estimate, 3),
                   "Upper Bound" = round(conf.high, 3),
                   "Lower Bound" = round(conf.low, 3),
                   "Variable" = term) %>%
            select(Variable, Coefficient, 'Lower Bound', 'Upper Bound') %>%
          gt() %>%
          tab_header("Multiple Regression Coefficients and Confidence Intervals")
        })
    
  # Displaying the correlation coefficients with % Registered, used a gt table 
    
    output$correlation_table <- render_gt({
      joined_data %>%
        summarize('Median Age in County' = cor(Median_age, x_eligible_population_enrolled),
                  'Median Income in County' = cor(Median_income, x_eligible_population_enrolled),
                  'Percent of the County that is White' = cor(Perc_white, x_eligible_population_enrolled)) %>%
        gt() %>%
        tab_header("Correlation Coefficients")
    })
    
    # Map of demographic factors 
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
    
    # Map of % Registered -- will change this to vary with the years 
    output$Map2 <- renderPlotly ({  
        h <- joined_geom %>%
            filter(month == "11") %>%
            filter(year == "2015") %>%
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
        
        # Tooltip lets me control what appears dugin the hover
        
        ggplotly(h, tooltip = "text")
        
    })
 
    output$regression_graph <- renderPlot({
        joined_data %>%
            filter(month == "11") %>%
    # Want to be able to look at the regression for % registered vs. income,
    # age, and race.
            
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
                title = "Linear Regression: County Organ Donation Registration Rates by Demographic Factors",
                color = "Year")
    })
    
    # I wanted to display not just the registration rates at one time, but also
    # how they change over time, and if they are increasing more quickly in some
    # counties than others
    
    output$growth_graph <- renderPlotly({
        
        w <- growth_rates %>%
            ggplot(aes(x = fct_reorder(county, diff), 
                       y = diff, 
                       fill = case_when(
                           input$plot_type2 == "Income" ~ Median_income,
                           input$plot_type2 == "Age" ~ Median_age,
                           input$plot_type2 == "Race" ~ Perc_white),
                       text = case_when(
                           input$plot_type2 == "Income" ~ 
                               paste(county, "County: $", Median_income),
                           input$plot_type2 == "Age" ~ 
                               paste(county, "County:", Median_age, "years old"),
                           input$plot_type2 == "Race" ~
                               paste(county, "County:", round(Perc_white, 2), "% white")
                       ))) + 
            geom_col() +
            theme(axis.text.x = element_text(size = 5.5, angle = 60)) +
            
            # Rotating the axis labels so they are more legible 
            
            # So the higher numbers are darker 
            labs(y = "% Growth in Organ Donor Registration", 
                 x = "County",
                 title = "% Growth in Organ Donor Registration Rates by County in NY, 2008-2016",
                 
                 # Also have to use case_when so the label of the legend changes based on the
                 # dropdown option
                 
                 fill = case_when(
                     input$plot_type2 == "Income" ~ "Median Income in the County",
                     input$plot_type2 == "Age" ~ "Median Age in the County",
                     input$plot_type2 == "Race" ~ "Percentage of White Residents")) +
        
        
            scale_fill_viridis_c(direction = -1) +
            scale_color_viridis_c(direction = -1)
      
        # Using tooltip allows me to control what shows up when the mouse hovers
        # over the graph
        
        ggplotly(w, tooltip = "text")
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
