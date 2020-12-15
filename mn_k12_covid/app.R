
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
library(viridis)
library(RColorBrewer)
library(maps)
library(mapproj)

rolling_sum_14 <- rollify(sum, window = 14)

start_date <- "2020-03-01"

county_fill <- colorRampPalette(rev(brewer.pal(11, "Spectral"))) 

theme_set(theme_minimal())

theme_map <- function(base_size=12, base_family="") {
  require(grid)
  theme_minimal(base_size=base_size, base_family=base_family) +
    theme(plot.title = element_text(face = "bold", size = 16),
          plot.subtitle = element_text(size = 14))%+replace%
    theme(axis.line=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          legend.text = element_text(size = 12),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid=element_blank(),
          panel.spacing=unit(0, "lines"),
          plot.background=element_blank()
    )
}

# Import map data ---------------------------------------------------------

us_counties <- map_data("county") %>% 
  filter(region == "minnesota") %>% 
  mutate(subregion = str_to_title(subregion)) %>% 
  mutate(subregion = str_replace(subregion, "St Louis", "St. Louis"),
         subregion = str_replace(subregion, "Mcleod", "McLeod"),
         subregion = str_replace(subregion, "Lake Of The Woods", "Lake of the Woods"),
         subregion = str_replace(subregion, "Lac Qui Parle", "Lac qui Parle"),
         )

# Get county populations --------------------------------------------------

county_pop <- 
    read_excel(here::here("data","mn_county_estimates_sdc_2019_tcm36-442553.xlsx"), 
               sheet = "COUNTY_ESTIMATES_2019",
               .name_repair = janitor::make_clean_names) %>% 
    select("county" = county_name, "pop" = total_population_2019)


# Get cases by county from nyt --------------------------------------------

county_cases_nyt <- 
    read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>% 
    filter(state == "Minnesota") %>% 
    complete(state, date, fill = list(cases = 0, deaths = 0)) %>% 
    ungroup() 

county_nyt_add_pop <- 
    county_cases_nyt %>% 
    left_join(county_pop, by = c("county" = "county")) %>% 
    filter(date >= "2020-03-01")

county_all <- 
  county_nyt_add_pop %>%
    group_by(county, date) %>%
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
    ungroup() %>% 
    filter(!is.na(school_type))

county_all_cumulative <- 
  county_nyt_add_pop %>%
  group_by(county) %>%
  slice_tail(n = 1) %>% 
  mutate(case_by_pop_adj_sum = cases/(pop/10000)) %>% 
  ungroup()

last_updated <-
  county_all_cumulative %>%
  arrange(date) %>%
  slice(1) %>% 
  pull(date)

# Join cases with location for mapping ------------------------------------

cases_state_map <- county_all %>% 
  left_join(us_counties, by = c("county" = "subregion"))

cases_state_total <- county_all_cumulative %>% 
  left_join(us_counties, by = c("county" = "subregion"))

case_max <- max(cases_state_map$case_by_pop_adj)
case_total_max <- max(cases_state_total$case_by_pop_adj_sum)

# Get MDH cases by zipcode ------------------------------------------------

cases_zip_existing <- 
    read_csv(here::here("data", "cases_zip.csv")) %>% 
    clean_names() %>% 
    mutate(zipcode = as.character(zipcode))

# Used to add new MDH weekly on Thursdays
# cases_zip_new <-
#     read_csv(here::here("data","wmapcz50.csv")) %>%
#     clean_names() %>%
#     select(zipcode = zip, cases) %>%
#     mutate(date = "12/10/2020")
# 
# cases_zip <-
#     bind_rows(cases_zip_existing, cases_zip_new)
# 
# write_csv(cases_zip, here::here("data", "cases_zip.csv"))


# Get zipcode for counties -------------------------------------------------

# https://www.unitedstateszipcodes.org/zip-code-database/

county_zips <- 
    read_csv(here::here("data", "zip_code_database.csv")) %>% 
    clean_names() %>% 
    filter(state == "MN") %>% 
    select(zip, primary_city, county)

# Join with cases by zip --------------------------------------------------

cases_zip_county <- 
    left_join(cases_zip_existing, county_zips, by = c("zipcode" = "zip")) %>% 
    mutate(county = str_remove(county, " County")) %>% 
    mutate(date = parse_date(date, "%m/%d/%Y")) %>% 
    filter(cases != "<=5") %>% 
    mutate(cases = as.numeric(cases)) %>% 
    distinct()

# Create policy tibble ----------------------------------------------------

policies <- 
    tribble(
        ~"Mode", ~"14-day total/10K",
        #--|--
        "in-person", "0-10",
        "elementary in-person, secondary hybrid", "10-20",
        "hybrid for all", "20-30",
        "elementary hybrid, secondary distance", "30-50",
        "distance for all", "50+" 
    )

policies_styled <- 
    datatable(policies, options = list(dom = 't', ordering = FALSE), rownames = FALSE) %>% 
        formatStyle("Mode", 
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
        ) %>% 
        formatStyle("14-day total/10K", 
                backgroundColor = 
                    styleEqual(values = c("#EDD9A3", 
                                          "#F79C79", 
                                          "#F2637F", 
                                          "#CA3C97", 
                                          "#872CA2"),
                               levels = c("0-10",
                                          "10-20",
                                          "20-30",
                                          "30-50",
                                          "50+")
                    ),
                fontWeight = 'bold'
        )


# Define UI for application that draws a histogram
ui <- fluidPage(
  style = "padding-top: 100px;",
  
    column(
      width = 10,
      offset = 1,
      p(h1("MN Guidance on Mode of School Instruction")),
      withTags({
        div(
          class = "header",
          checked = NA,
          h5(
            "MN recommends that schools make decisions about their safe learning model based on
                the number of cases by county of residence in Minnesota over 14 days, per 10,000 people.
                The state",
            a(href = "https://www.health.state.mn.us/diseases/coronavirus/stats/wschool.pdf", "publishes this number" , target =
                "_blank"),
            "once a week on Saturdays for the previous 14 days (e.g., 7/25/2020 includes cases between 7/12/2020 and 7/25/20).
                   The visualization below shows those official weekly numbers (circles) but also tracks
                this number daily (bars) for the county you select from the right sidebar. The number is calculated using",
            a(
              href = "https://mn.gov/admin/demography/data-by-topic/population-data/our-estimates/",
              "state county population estimates from 2019",
              target = "_blank"
            ),
            "and joined with the data provided by the New York Times",
            a(
              href = "https://github.com/nytimes/covid-19-data",
              "(https://github.com/nytimes/covid-19-data)",
              target = "_blank"
            ),
            tags$br(),
            p(h6(
              "Code available at",
              a(
                href = "https://github.com/kechambers/mn_k12_covid",
                "https://github.com/kechambers/mn_k12_covid",
                target = "_blank"
              )
            )),
            h6(tags$em(paste0("Data updated ", as.Date(last_updated))), align = "right")
          )
        )
      }),
      plotOutput("countyPlot"),
      dataTableOutput("policyTable"),
      tags$br(),
      h5(
        "How does your selected county compare to all counties in the state? 
        Counties are sorted in descending order based on their most recent total number of cases in the past 14 days per 10,000 people.
        The selected county will be highlighted."
      ),
      plotOutput("countyTimePlot", height = 700),
      tags$br(),
      h5("Where are the counties with the highest scores located? The total number of cases in the past 14 days per 10,000 people
         calculated from the most recent date selected is shown for each county."),
      plotOutput("stateMapPlot", height = 600),
      tags$hr(),
      tags$br(),
      h5("Where are the counties with the highest number of cumulative cases?
         The total number of cases per 10,000 people from 2020-03-01 to", last_updated),
      plotOutput("stateCumulativePlot", height = 600),
      tags$hr(),
      tags$br(),
      h5("Within the selected county, what proportion of all cases can be attributed to each zipcode?"),
      plotOutput("zipcodePlot"),
      uiOutput("zipSelection"),
      tags$br()
      # fluidRow(plotOutput("countyCompPlot", height = 800))
    ),
  absolutePanel(
    top = 0,
    left = 0,
    right = 0,
    fixed = TRUE,
    style = "padding: 1px; background: #ffffff;",
    style = "opacity: 0.95",
    draggable = TRUE,
    column(
      width = 10,
      offset = 1,
      tags$br(),
      column(
        width = 3,
        pickerInput(
          inputId = "counties",
          label = "Select a county",
          choices = sort(unique(county_pop$county)),
          selected = c("Nicollet")
        )
      ),
      column(
        width = 9,
        sliderInput(
          width = "100%",
          inputId = "dates",
          label = "Change date range",
          min = as.Date(start_date),
          max = as.Date(today() - days(1)),
          value = c(as.Date("2020-08-01", "%Y-%m-%d"), as.Date(today() - days(1))),
          timeFormat = "%b %d"
        )
      ),
    ),
  )
  )

# Define server logic
server <- function(input, output) {
  

# Reactive ui -------------------------------------------------------------

  zips_for_county <- reactive({
    cases_zip_county %>%
      filter(county == input$counties) %>%
      group_by(date, zipcode) %>%
      summarise(n = sum(cases)) %>%
      mutate(percentage = n / sum(n))
  })
  
  output$zipSelection <- renderUI({
    pickerInput(
      input = "zips",
      label = "Show zipcodes", 
      choices = unique(sort(as.character(zips_for_county()$zipcode))),
      selected = unique(sort(as.character(zips_for_county()$zipcode))),
      multiple = TRUE
    )
  })
  
# Policy Table ------------------------------------------------------------

  output$policyTable <- renderDataTable({
    policies_styled
  })  
    
    

# Zip code proportion plot ------------------------------------------------

    
    zip_comp_data <- reactive({
        zips_for_county() %>% 
        filter(zipcode %in% input$zips)
    })
    
    output$zipcodePlot <- renderPlot({
        ggplot(zip_comp_data(), aes(x = date, y = percentage, fill = zipcode)) +
            geom_area(alpha = 0.6 , size = .5, color = "white") +
            scale_fill_viridis(discrete = T) +
        theme(
              axis.text = element_text(size = 16),
              axis.ticks.x = element_line(color = "black"),
              axis.line.x = element_line(color = "black"),
              legend.title = element_blank(),
              legend.text = element_text(size = 14),
        ) +
        labs(title = NULL,
             caption = NULL,
             y = NULL,
             x = NULL)
        
    })
    

# Selected county timecourse plot --------------------------------------------------
    
    
    county_selected <- reactive({
      county_all %>% 
        filter(county %in% input$counties) %>% 
        filter(between(date, input$dates[1], input$dates[2]))
    })
    
    county_decision_days <- reactive({
      county_selected() %>% 
        filter(wday(date) == 7)
    })
    
    output$countyPlot <- renderPlot({
        
        county <- 
            ggplot(county_selected(), aes(x = date, y = case_by_pop_adj, fill = school_type)) +
            geom_col(width = 0.5, alpha = 0.8, show.legend = FALSE) +
            geom_segment(data = . %>% filter(wday(date) == 7) %>% slice(2),
                         aes(x = date, y = case_by_pop_adj + 9, xend = date, yend = case_by_pop_adj), color = "black", hjust = 1, vjust = 0.5) +
            geom_text(data = . %>% filter(wday(date) == 7) %>% slice(2), 
                      aes(x = date, y = case_by_pop_adj + 10, label = "Saturdays"), 
                      color = "black", size = 5, hjust = 0, vjust = 0.5) +
            geom_point(data = . %>% filter(wday(date) == 7),
                       aes(y = case_by_pop_adj, fill = school_type), 
                       color = "black", size = 3, stroke = 0.5, shape = 21) +
            geom_text(data = . %>% 
                          filter(wday(date) == 7) %>% slice_tail(n = 5),
                      aes(y = case_by_pop_adj + 2.5, label = round(case_by_pop_adj, 1)), 
                      color = "black", size = 4.25, hjust = 0.5, vjust = 0.5) +
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
                legend.position = 'none'
            ) +
            labs(title = NULL,
                 caption = NULL,
                 y = NULL,
                 x = NULL)
        
        county
        
    })
    

# County timecourse comparison plot ---------------------------------------

    
    county_time <- reactive({
        county_all %>% 
            filter(between(date, input$dates[1], input$dates[2]))
    })
    
    output$countyTimePlot <- renderPlot({
        ggplot(county_time(), aes(x = date, y = fct_reorder(county, case_by_pop_adj, .fun = last, .desc = FALSE), fill = school_type)) +
            geom_tile(color="white",size = 0.2, show.legend = FALSE, alpha = 0.5) +
            geom_hline(data = . %>% filter(county %in% input$counties), aes(yintercept = county)) + 
            geom_tile(data = . %>% filter(county %in% input$counties), color="white",size = 0.2, show.legend = FALSE) +
            scale_fill_manual(values = c("#EDD9A3", "#F79C79", "#F2637F", "#CA3C97", "#872CA2"), na.value = "grey90",
                              drop = FALSE) +
            theme(panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  strip.text = element_text(size = 12, face = "bold", hjust = 0),
                  plot.title = element_text(size = 18, face = "bold"),
                  axis.text = element_text(size = 16),
                  axis.text.y = element_text(size = 10),
                  axis.ticks.x = element_line(color = "black"),
                  axis.line.x = element_line(color = "black"),
                  legend.title = element_blank(),
                  legend.position='none'
            ) +
            labs(title = NULL,
                 caption = NULL,
                 y = NULL,
                 x = NULL)
    })
    

# State map of cases by county --------------------------------------------

    
    cases_state_map_date <- reactive({
      cases_state_map %>%
        filter(date == input$dates[2])
    })

    output$stateMapPlot <- renderPlot({
      
      county_map <- 
        ggplot(cases_state_map_date(), aes(x = long, y = lat, fill = case_by_pop_adj)) +
        geom_polygon(aes(group = group), color = "gray90", size = 0.1) +
        coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
        scale_fill_gradientn(colours = county_fill(100), limits = c(0,case_max)) +
        # scale_fill_viridis_c(limits = c(0,case_max)) +
        theme_map() +
        labs(title = NULL,
             fill = NULL)
      
      county_map
    })
    
    
    output$stateCumulativePlot <- renderPlot({
      
      county_map <- 
        ggplot(cases_state_total, aes(x = long, y = lat, fill = case_by_pop_adj_sum)) +
        geom_polygon(aes(group = group), color = "gray90", size = 0.1) +
        coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
        scale_fill_gradientn(colours = county_fill(100), limits = c(0,case_total_max)) +
        theme_map() +
        labs(title = NULL,
             fill = NULL)
      
      county_map
    })

# County comparison by last measurement -----------------------------------
    
    # county_last_day <- reactive({
    #   county_all %>% 
    #     group_by(county) %>% 
    #     slice_tail(1) %>% 
    #     ungroup()
    # })
    # 
    # output$countyCompPlot <- renderPlot({
    #   
    #   counties <- 
    #     ggplot(county_last_day(), aes(x = fct_reorder(county, case_by_pop_adj), y = case_by_pop_adj, fill = school_type)) +
    #     geom_col(width = 0.5, alpha = 0.5, show.legend = FALSE) +
    #     geom_col(data = . %>% filter(county %in% input$counties), width = 0.5, alpha = 1, show.legend = FALSE) +
    #     geom_point(data = . %>% filter(county %in% input$counties), shape = 23, size = 4, show.legend = FALSE) +
    #     expand_limits(y = c(0, 50)) +
    #     scale_fill_manual(values = c("#EDD9A3", "#F79C79", "#F2637F", "#CA3C97", "#872CA2"),
    #                       drop = FALSE) +
    #     theme(
    #       panel.grid.major.y = element_blank(),
    #       panel.grid.minor.y = element_blank(),
    #       strip.text = element_text(size = 12, face = "bold", hjust = 0),
    #       plot.title = element_text(size = 18, face = "bold"),
    #       axis.text = element_text(size = 16),
    #       axis.text.y = element_text(size = 12),
    #       axis.ticks.x = element_line(color = "black"),
    #       axis.line.x = element_line(color = "black"),
    #       legend.title = element_blank(),
    #       legend.position='none'
    #     ) +
    #     labs(title = NULL,
    #          caption = NULL,
    #          y = NULL,
    #          x = NULL) +
    #     coord_flip()
    #   
    #   counties
    #   
    # })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
