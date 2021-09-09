library(shiny)
library(shinydashboard)
library(tidyverse)
library(broom)
library(kableExtra)

coffee_ratings = read_csv(file = "merged_data_cleaned.csv")



ui <- dashboardPage(
    dashboardHeader(title = "Coffee Dashboard"),
    
    dashboardSidebar(
        selectInput("v_country", "Country", choices = coffee_ratings %>% 
                        select(Country.of.Origin) %>% 
                        distinct() %>% 
                        arrange(Country.of.Origin) %>% 
                        drop_na())
    ),
    dashboardBody(
        fluidRow(plotOutput("coffee_flavor")), 
        fluidRow(plotOutput("coffee_variety")),
        fluidRow(tableOutput("coffee_table")),
        fluidRow(plotOutput("coffee_dif"))
        
    )
)

server <- function(input, output) { 
    
    output$coffee_flavor <- renderPlot({
        
        coffee_ratings %>% 
            filter(Country.of.Origin == input$v_country) %>% 
            select(Aroma:Cupper.Points) %>% 
            gather() %>% 
            group_by(key) %>% 
            summarise(value = mean(value)) %>% 
            ungroup() %>% 
            mutate(key = str_replace(key, "_", " ") %>% str_to_title()) %>% 
            mutate(key = fct_reorder(key, value)) %>% 
            ggplot(aes(x = key, y = value, color = key)) + 
            geom_point(size = 5) + 
            geom_segment(aes(x = key, xend = key, y = value, yend = 0)) + 
            theme(legend.position = "none") + 
            ylab("") + 
            xlab("") + 
            coord_flip() + 
            labs(title = "Average Flavor Profile")
    })
    
    output$coffee_variety <- renderPlot({
        
        coffee_ratings %>% 
            filter(Country.of.Origin == input$v_country) %>% 
            select(Variety) %>% 
            drop_na() %>% 
            count(Variety) %>% 
            mutate(Variety = fct_reorder(Variety, n)) %>% 
            ggplot(aes(x = n, y = Variety, fill = Variety)) + 
            geom_col() + 
            ylab("") + 
            labs(title = "Bean Variety") + 
            theme(legend.position = "none")
    })
    
    
    
    output$coffee_table <- function(){
        
        coffee_ratings %>% 
            filter(Country.of.Origin == input$v_country) %>% 
            select(Coffee_ID, Taste_Points = Total.Cup.Points, Species, Country = Country.of.Origin, Region, Owner = In.Country.Partner, Farm.Name, altitude_mean_meters) %>% 
            drop_na()%>%
            group_by(Species, Region) %>% 
            arrange(desc(Taste_Points)) %>% 
            slice(1) %>% 
            ungroup() %>% 
            mutate(Region = str_trunc(Region, 12, "right")) %>% 
            arrange(desc(Taste_Points)) %>% 
            kable() %>% 
            kable_styling() %>% 
            scroll_box(height = "500px", width = "800px")
    }
    
    
    output$coffee_dif <- renderPlot({
        coffee_ratings %>% 
            select(Country.of.Origin, Aroma:Cupper.Points) %>% 
            mutate(highlight = if_else(Country.of.Origin == input$v_country, "Highlight", "No-Highlight")) %>% 
            select(-Country.of.Origin) %>% 
            gather(key = "key", value = "value", -highlight) %>% 
            group_by(key) %>% 
            do(t_test = t.test(value~highlight, data = .) %>% tidy()) %>% 
            unnest(t_test) %>% 
            mutate(diffference = case_when(
                conf.low < 0 & conf.high < 0 ~ "Different",
                conf.low > 0 & conf.high > 0 ~ "Different",
                TRUE ~ "Not-Different"
            )) %>% 
            mutate(key = str_replace(key, "_", " ") %>% str_to_title()) %>% 
            mutate(key = fct_reorder(key, -estimate)) %>% 
            ggplot(aes(x = key, y = estimate, color = diffference)) + 
            geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) + 
            geom_hline(yintercept = 0, linetype = "dashed") + 
            coord_flip() + 
            theme(legend.position = "none") + 
            xlab("") + 
            labs(title = "Difference in Flavor Profiles (T-Test)")
    })
     
}

shinyApp(ui = ui, server = server, options = list(height = 1080))
