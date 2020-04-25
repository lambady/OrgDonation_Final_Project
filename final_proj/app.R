
library(shiny)
library(readr)
library(tidyverse)
library(broom)
library(gganimate)
library(plotly)

joined_data <- readRDS("joined_data.RDS")
geometry_data <- readRDS("geometrydata.RDS")
joined_geom <- readRDS("joined_geom.RDS")
joined_untidy <-readRDS("joined_untidy.RDS")

# Define UI for application that draws a histogram
ui <- navbarPage(
    "Organ Donation Registration in New York",
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("With increasing rates of disease in the kidney, lung, and liver, post-mortem donation has become an important way to 
               save lives. This project analyzes rates of organ donation registration in the counties of New York state, attempting to 
               see what factors might facilitate registration, and what factors might pose a barrier to registration. So far, it looks as 
               though minorities are uncomfortable registering as organ donors, and I will look at other data to see why this might be."),
             h3("About Me"),
             p("My name is Leena Ambady and I study the history of science. 
             You can reach me at lambady@college.harvard.edu.")),
    tabPanel("Model",
             fluidPage(
                 titlePanel("Model Title"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "plot_type",
                             "Choose a Factor",
                             c("Income", "Age", "Race")
                         )),
                     mainPanel(plotOutput("Image"),
                               plotlyOutput("Map"),
                               plotlyOutput(("Map2")))),
             )),
    tabPanel("Statistical Analysis", 
             fluidPage(
            sidebarLayout(
                sidebarPanel(
                 h4("Explanation"),
                 p("This shows a regression attempting to explain county organ donor registration rates by the 
                              proportion of a county's population that is White. Do counties with a greater concentration of minorities have lower organ donation rates, 
                              and is this a product of medical mistrust? The purpose of running this regression was to help encourage organ donation procurement agencies to target
                              education to minorities, as well as send a message to the overall health care system and historical and current mistreatment of minorities by the healthcare system
                              can affect crucial systems like organ donation.")
                                      ),
                 selectInput("regression_factor", 
                                    "Demographic Factor", 
                                    c("Income", "Age", "Race"))),
                mainPanel(
                     h2("Linear Regression"),
                     p("This graph plots demographic factrs on the x-axis with
                        the percentage of a county's population that is registered as an organ donor on the y-axis."),
                     plotOutput("regression_graph"),
                     h4("Regression Information"),
                     p("This table shows the average coefficient value 
                         (slope of the regression line), the 5th and 95th percentile 
                         values to give an indication of uncertainty associated with 
                         the term."),
                    tableOutput("regression_table"),
                                       )
             ))
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$Image <- renderPlot({
   
       joined_data %>%
           filter(year == "2015") %>%
           ggplot(aes(x = x_eligible_population_enrolled, 
                      y = fct_reorder(county, x_eligible_population_enrolled), 
                      color = case_when(
                          input$plot_type == "Income" ~ Median_income,
                          input$plot_type == "Age" ~ Median_age,
                          input$plot_type == "Race" ~ Perc_white
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
                    input$plot_type == "Income" ~ "Median Income in the County",
                    input$plot_type == "Age" ~ "Median Age in the County",
                    input$plot_type == "Race" ~ "Percentage of White Residents"))
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
        p <- geometry_data %>%
            mutate(Perc_white = (estimate/summary_est) *100) %>%
            filter(variable == case_when(
                input$plot_type == "Income" ~ "Median_income",
                input$plot_type == "Age" ~ "Median_age",
                input$plot_type == "Race" ~ "Number_white")) %>%
            ggplot(aes(fill =  ifelse(variable == "Number_white", prop_white, estimate),
                       geometry = geometry, text = paste(county, "County"))) +
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
        
        ggplotly(p) %>%
            highlight(
                "plotly_hover",
                selected = attrs_selected(line = list(color = "black")))
        
    })
    
    output$Map2 <- renderPlotly ({  
        h <- joined_geom %>%
            ggplot(aes(fill = x_eligible_population_enrolled,
                       geometry = geometry, 
                       text = paste(county, "County"))) +
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
        
        ggplotly(h) %>%
            highlight(
                "plotly_hover",
                selected = attrs_selected(line = list(color = "black")))
       
       
    })
 
    output$regression_graph <- renderPlot({
        joined %>%
            filter(year == "2015") %>%
            ggplot(aes(x = case_when(
                input$regression_factor == "Income" ~ Median_income,
                input$regression_factor == "Age" ~ Median_age,
                input$regression_factor == "Race" ~ Perc_white), 
                       y = x_eligible_population_enrolled)) + 
            geom_point() +
            geom_smooth(method = "lm", se = TRUE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
