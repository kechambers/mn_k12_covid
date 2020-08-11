
# Load packages and functions ---------------------------------------------

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(lubridate)
library(janitor)
library(here)
library(DT)
library(readxl)
library(tibbletime)
library(scales)

theme_set(theme_minimal())

rolling_sum_14 <- rollify(sum, window = 14)


# Get county populations --------------------------------------------------

county_pop <- 
    read_excel(here::here("data","mn_county_estimates_sdc_2019_tcm36-442553.xlsx"), 
               sheet = "COUNTY_ESTIMATES_2019",
               .name_repair = janitor::make_clean_names) %>% 
    select("county" = county_name, "pop" = total_population_2019)


# Get cases by county from jhu --------------------------------------------

county_cases_jhu <- 
    read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv") %>% 
    clean_names() %>% 
    filter(province_state == "Minnesota")

county_jhu <- 
    county_cases_jhu %>% 
    left_join(county_pop, by = c("admin2" = "county"))

county_jhu_sum <-
    county_jhu %>% 
    select("county" = admin2, pop, starts_with("x")) %>% 
    pivot_longer(names_to = "date", values_to = "cases", c(-county, -pop)) %>% 
    mutate(date = str_remove(date, "x")) %>% 
    mutate(date = mdy(date)) %>% 
    filter(date >= "2020-03-01")


# Get cases by county from nyt --------------------------------------------


county_cases_nyt <- 
    read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>% 
    filter(state == "Minnesota") %>% 
    complete(state, date, fill = list(cases = 0, deaths = 0)) %>% 
    ungroup() 

county_nyt <- 
    county_cases_nyt %>% 
    left_join(county_pop, by = c("county" = "county"))

county_nyt_sum <- 
    county_nyt %>% 
    filter(date >= "2020-03-01")


# Create policy tibble ----------------------------------------------------

policies <- 
    tribble(
        ~"Policy Option", ~"Range (14-day case rate per 10K people)",
        #--|--
        "in-person", "0 to less than 10",
        "elementary in-person, secondary hybrid", "10 to less than 20",
        "hybrid for all", "20 to less than 30",
        "elementary hybrid, secondary distance", "30 to less than 50",
        "distance for all", "50 or more" 
    )

policies_styled <- 
    datatable(policies, options = list(dom = 't', ordering = FALSE), rownames = FALSE) %>% 
        formatStyle("Policy Option", 
                    backgroundColor = 
                        styleEqual(values = c("#EDD9A3", 
                                          "#F79C79", 
                                          "#F2637F", 
                                          "#CA3C97", 
                                          "#872CA2"),
                               levels = c("in-person",
                                          "elementary in-person, secondary hybrid",
                                          "hybrid for all",
                                          "elementary hybrid, secondary distance",
                                          "distance for all")
        ),
        fontWeight = 'bold'
        )


# Define UI for application that draws a histogram
ui <- fluidPage(column(
    width = 10,
    offset = 1,
    fluidRow(p(h1("Determing a Safe Learning Model for MN"))),
    fluidRow(dataTableOutput("policyTable")),
    tags$br(),
    fluidRow(
        column(
            width = 5,
            pickerInput(
                inputId = "counties",
                label = "Select one or more counties",
                choices = sort(unique(county_pop$county)),
                selected = c("Nicollet", "Le Sueur"),
                multiple = TRUE
            )
        ),
        column(
            width = 3,
            offset = 1,
            pickerInput(
                inputId = "data",
                label = "Choose data source",
                choices = c("NYTimes", "CSSE@JHU"),
                selected = "NYTimes"
            )
        )
    ),
    fluidRow(plotOutput("countyPlot")),
    fluidRow(
        withTags({
            div(class="header", checked=NA,
                h5("MN recommends that schools make decisions about their safe learning model based on 
                the number of cases by county of residence in Minnesota over 14 days, per 10,000 people. 
                The state",
                   a(href="https://www.health.state.mn.us/diseases/coronavirus/stats/wschool.pdf", "publishes this number" , target="_blank"),
                   "once a week on Saturdays for the previous 14 days (e.g., 7/25/2020 includes cases between 7/12/2020 and 7/25/20). 
                   The visualization above shows those official weekly numbers (circles) but also tracks
                this number daily (bars). The number is calculated using",
                   a(href="https://mn.gov/admin/demography/data-by-topic/population-data/our-estimates/", 
                     "state county population estimates from 2019",
                     target="_blank"),
                   "and joined with either data provided by the New York Times",
                   a(href="https://github.com/nytimes/covid-19-data", "(https://github.com/nytimes/covid-19-data)",
                     target="_blank"),
                "or by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University",
                a(href="https://github.com/CSSEGISandData/COVID-19", "(https://github.com/CSSEGISandData/COVID-19)",
                  target="_blank"),
                   tags$br(),
                   tags$br(),
                   "Code available at",
                   a(href="https://github.com/kechambers/mn_k12_covid", "https://github.com/kechambers/mn_k12_covid",
                     target="_blank")
                )
            )
        })
    )
))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    county_all <- reactive({
        switch(input$data,
               "NYTimes" = county_nyt_sum,
               "CSSE@JHU" = county_jhu_sum,
               )
    })
    
    county_selected <- reactive({
        county_all() %>% 
            filter(county %in% input$counties) %>% 
            group_by(date) %>%
            summarise(cases = sum(cases),
                      combined_pop = sum(pop)) %>%
            mutate(new_cases = cases - lag(cases, 1)) %>% 
            mutate(moving_sum = rolling_sum_14(new_cases)) %>% 
            mutate(pop_adj = combined_pop/10000) %>% 
            mutate(case_by_pop_adj = moving_sum/pop_adj) %>% 
            mutate(school_type = case_when(
                case_by_pop_adj <= 9.99 ~ "in-person",
                between(case_by_pop_adj, 10, 19.99)  ~ "elementary in-person, secondary hybrid",
                between(case_by_pop_adj, 20, 29.99)  ~ "hybrid for all",
                between(case_by_pop_adj, 30, 49.99)  ~ "elementary hybrid, secondary distance",
                case_by_pop_adj >= 50  ~ "distance for all",
                TRUE ~ NA_character_
            ),
            school_type = factor(school_type, levels = c("in-person",
                                                         "elementary in-person, secondary hybrid",
                                                         "hybrid for all",
                                                         "elementary hybrid, secondary distance",
                                                         "distance for all"
                                                         ),
                                 ordered = TRUE
                                 ) 
            ) %>% 
            filter(!is.na(school_type))
    })
    
    county_decision_days <- reactive({
        county_selected() %>% 
            filter(wday(date) == 7)
    })
        
    
    # output$table <- renderTable({
    #     
    #     county_data_long$date <- format(county_data_long$date,'%Y-%m-%d')
    #     
    #     table_data <- 
    #         county_data_long %>% 
    #         filter(county == input$counties) %>% 
    #         select(date,
    #                "total_cases" = cases, new_cases, 
    #                "14_day_sum" = moving_sum,
    #                "14_by_10K" = case_by_pop_adj,
    #                school_type) %>% 
    #         tail(5)
    # })

    output$policyTable <- renderDataTable({
        policies_styled
    })
    
    output$countyPlot <- renderPlot({
        
        county <- 
            ggplot(county_selected(), aes(x = date, y = case_by_pop_adj, fill = school_type)) +
            geom_col(width = 0.5, alpha = 0.8, show.legend = FALSE) +
            geom_segment(data = . %>% filter(date == "2020-05-30"),
                         aes(x = date, y = case_by_pop_adj + 10, xend = date, yend = case_by_pop_adj), color = "black", hjust = 1, vjust = 0.5) +
            geom_text(data = . %>% filter(date == "2020-05-30"), 
                      aes(x = date, y = case_by_pop_adj + 10, label = "Saturday's\n14-day cases/10K"), 
                      color = "black", size = 5, hjust = 0, vjust = 0.5) +
            geom_point(data = . %>% filter(wday(date) == 7),
                       aes(y = case_by_pop_adj, fill = school_type), 
                       color = "black", size = 10, stroke = 1, shape = 21) +
            geom_text(data = . %>% 
                          filter(wday(date) == 7),
                      aes(y = case_by_pop_adj, label = round(case_by_pop_adj, 1)), 
                      color = "black", size = 3.25, hjust = 0.5, vjust = 0.5, fontface = "bold") +
            
            expand_limits(y = c(0, 50)) +
            scale_x_date(labels = label_date_short(format = c("", "%b", "%d"),
                                                   sep = "\n")) +
            scale_fill_manual(values = c("#EDD9A3", "#F79C79", "#F2637F", "#CA3C97", "#872CA2"),
                              drop = FALSE) +
            theme(
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                strip.text = element_text(size = 12, face = "bold", hjust = 0),
                plot.title = element_text(size = 18, face = "bold"),
                axis.text = element_text(size = 16),
                axis.ticks.x = element_line(color = "black"),
                axis.line.x = element_line(color = "black"),
                legend.title = element_blank(),
                legend.position='none'
            ) +
            labs(title = NULL,
                 caption = NULL,
                 y = NULL,
                 x = NULL)
        
        county
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
