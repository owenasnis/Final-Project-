library(shiny)
library(shinythemes)
library(tidyverse)
library(tidycensus)
library(usmap)
library(rstanarm)
library(gtsummary)
library(plotly)
library(tidycensus)
library(sf)
library(ggthemes)
census_api_key("19ea08aa1b10f210a3a218b15486e5a38506a4cf", 
               install = TRUE, overwrite = TRUE)

nhgis <- read_csv("raw_data/nhgis.csv") %>% 
    mutate(FIPS = paste0(STATEA, COUNTYA), 
           white_pct = (AF2ME002 / AF2ME001) * 100,
           raw_white = AF2ME002, 
           black_pct = (AF2ME003 / AF2ME001) * 100, 
           raw_black = AF2ME003, 
           asian_pct = (AF2ME005 / AF2ME001) * 100,
           raw_asian = AF2ME005, 
           other_pct = ((AF2ME004 + AF2ME006 + AF2ME007 + AF2ME008) / AF2ME001) 
           * 100,
           raw_other = AF2ME004 + AF2ME006 + AF2ME007 + AF2ME008, 
           high_school_degree = ((AF4OE017 + AF4OE018) / AF4OE001) * 100,
           raw_high_school_degree = AF4OE017 + AF4OE018, 
           college_no_degree = ((AF4OE019 + AF4OE020) / AF4OE001) * 100,
           raw_college_no_degree = AF4OE019 + AF4OE020, 
           bachelors_degree = (AF4OE022 / AF4OE001) * 100, 
           raw_bachelors_degree = AF4OE022, 
           grad_degree = ((AF4OE023 + AF4OE024 + AF4OE023) / AF4OE001) * 100,
           raw_grad_degree = AF4OE023 + AF4OE024 + AF4OE023,  
           median_household_income = AF49E001) %>% 
    select(FIPS, white_pct, black_pct, asian_pct, other_pct, high_school_degree, 
           college_no_degree, bachelors_degree, grad_degree, 
           median_household_income, raw_white, raw_black, raw_asian, raw_other, 
           raw_high_school_degree, raw_college_no_degree, raw_bachelors_degree, 
           raw_grad_degree)

nhgis_pop <- read_csv("raw_data/pop.csv") %>%  
    mutate(FIPS = paste0(STATEA, COUNTYA), 
           pop = AF2LE001) %>% 
    select(FIPS, pop)

countypres <- read_csv("raw_data/countypres_2000-2016.csv", 
                       col_types = cols(year = col_double(),
                                        state = col_character(),
                                        state_po = col_character(),
                                        county = col_character(),
                                        FIPS = col_character(),
                                        office = col_character(),
                                        candidate = col_character(),
                                        party = col_character(),
                                        candidatevotes = col_double(),
                                        totalvotes = col_double(),
                                        version = col_double())) %>%
    filter(state_po %in% c("PA", "WI", "MI"), 
           party %in% c("democrat", "republican"), 
           year %in% c(2012, 2016)) %>% 
    select(- state, - office, - version) %>% 
    mutate(vote_share = (candidatevotes/totalvotes) * 100, 
           party = case_when(party == "democrat" ~ "D", 
                             party == "republican" ~ "R")) %>%
    pivot_wider(names_from = "candidate", 
                values_from = "vote_share") %>% 
    group_by(FIPS, year) %>% 
    summarize(Obama = mean(`Barack Obama`, na.rm = TRUE), 
              Clinton = mean(`Hillary Clinton`, na.rm = TRUE),
              Romney = mean(`Mitt Romney`, na.rm = TRUE), 
              Trump = mean(`Donald Trump`, na.rm = TRUE),
              .groups = "keep") %>% 
    mutate(dem_vs = if_else(year == 2012, Obama, Clinton), 
           rep_vs = if_else(year == 2012, Romney, Trump)) %>% 
    select(- Obama, - Clinton, - Romney, - Trump) %>% 
    left_join(nhgis, by = "FIPS") %>% 
    left_join(nhgis_pop, by = "FIPS") 

countypres12 <- countypres %>% 
    filter(year == 2012) %>% 
    mutate(nonwhite_pct = black_pct + asian_pct + other_pct, 
           less_college_pct = high_school_degree + college_no_degree, 
           college_more_pct = bachelors_degree + grad_degree)

countypres16 <- countypres %>% 
    filter(year == 2016) %>% 
    mutate(nonwhite_pct = black_pct + asian_pct + other_pct, 
           less_college_pct = high_school_degree + college_no_degree, 
           college_more_pct = bachelors_degree + grad_degree)

rustbelt_geometry <- get_acs(geography = "county", 
                             state = c(26, 42, 55), 
                             geometry = TRUE, 
                             variables = "B19013_001") %>% 
    select(geometry, GEOID, NAME) %>% 
    rename(fips = GEOID)

map12 <- countypres12 %>%
    ungroup() %>% 
    select(FIPS, dem_vs, rep_vs) %>%
    left_join(nhgis_pop, by = "FIPS") %>% 
    rename(fips = FIPS) %>% 
    mutate(difference = dem_vs - rep_vs) %>% 
    left_join(rustbelt_geometry, by = "fips") %>% 
    mutate(dem_vs = round(dem_vs, digits = 1), 
           rep_vs = round(rep_vs, digits = 1), 
           state = case_when(str_sub(fips, 1, 2) == 26 ~ "Michigan", 
                             str_sub(fips, 1, 2) == 42 ~ "Pennsylvania", 
                             str_sub(fips, 1, 2) == 55 ~ "Wisconsin")) %>%   
    group_by(state) %>% 
    mutate(pop_rank = rank(desc(pop)), 
           total_counties = n())

map16 <- countypres16 %>% 
    ungroup() %>% 
    select(FIPS, dem_vs, rep_vs) %>%
    left_join(nhgis_pop, by = "FIPS") %>% 
    rename(fips = FIPS) %>% 
    mutate(difference = dem_vs - rep_vs) %>% 
    left_join(rustbelt_geometry, by = "fips") %>% 
    mutate(dem_vs = round(dem_vs, digits = 1), 
           rep_vs = round(rep_vs, digits = 1), 
           state = case_when(str_sub(fips, 1, 2) == 26 ~ "Michigan", 
                             str_sub(fips, 1, 2) == 42 ~ "Pennsylvania", 
                             str_sub(fips, 1, 2) == 55 ~ "Wisconsin")) %>%   
    group_by(state) %>% 
    mutate(pop_rank = rank(desc(pop)), 
           total_counties = n())

trend <- countypres %>%
    mutate(dem_diff = dem_vs - rep_vs) %>%  
    select(FIPS, year, dem_diff) %>%
    group_by(FIPS) %>% 
    summarize(dem_chg = diff(dem_diff), 
              .groups = "drop") %>%
    left_join(nhgis_pop, by = "FIPS") %>%
    rename(fips = FIPS) %>%
    left_join(rustbelt_geometry, by = "fips") %>% 
    mutate(dem_chg = round(dem_chg, digits = 1)) %>% 
    mutate(state = case_when(str_sub(fips, 1, 2) == 26 ~ "Michigan", 
                             str_sub(fips, 1, 2) == 42 ~ "Pennsylvania", 
                             str_sub(fips, 1, 2) == 55 ~ "Wisconsin")) %>%   
    group_by(state) %>% 
    mutate(pop_rank = rank(desc(pop)), 
           total_counties = n())

ui <- navbarPage(
    "The Battleground: Wisconsin, Michigan and Pennsylvania in Contemporary Presidential Elections",
    theme = shinytheme("sandstone"), 
    tabPanel("About the Project",
                 mainPanel(
                     fluidRow(
                     column(4,
                     plotOutput("wimap")), 
                 column(4, 
             h3("Wisconsin (10 Electoral Votes)"),
             p("In 2012, President Barack Obama defeated Mitt Romney in 
               Wisconsin by 205,204 votes (roughly 6.7%), claiming the state's 
               10 electoral votes. However, four years later, Donald Trump 
               defeated Hillary Clinton in Wisconsin by 22,748 votes(roughly 
               0.7%). In four years, Wisconsin's electorate swung by 227,952 
               votes.")),
             column(4,
             imageOutput("wiseal")))),
             fluidRow(
                 column(4, 
                        imageOutput("miseal")),
                 column(4, 
             h3("Michigan (16 Electoral Votes)"), 
             p("In 2012, President Obama defeated Romney in Michigan by 449,238 
               votes (roughly 9.5%), claiming the state's 16 electoral votes. 
               Four years later, Trump defeated Clinton in Michigan by 10,704 
               votes (roughly 0.3%). In four years, Michigan's electorate swung 
               by 459,942 votes.")),
             column(4, 
                    plotOutput("mimap"))), 
             fluidRow(
                 column(4, 
                      plotOutput("pamap")),
             column(4,
             h3("Pennsylvania (20 Electoral Votes)"), 
             p("In 2012, President Obama defeated Romney in Pennsylvania by 
               287,865 votes (roughly 5.2%). claiming the commonwealth's 20 
               electoral votes. Four years later, Trump defeated Clinton in 
               Pennsylvania by 44,292 votes (roughly 0.7%). In four years, 
               Pennsylvania's electorate swung by 332,157 votes.")),
             column(4, 
                    imageOutput("paseal"))),
             h3("The Project: Motivation and Summary"), 
             p("Wisconsin, Michigan and Pennsylvania have 46 votes in the 
               electoral college, and since 1988, these states have voted for 
               the same presidential candidate. Therefore, these three 
               battleground states swing the electoral college by 92 votes, 
               which in recent elections has decided the presidential election. 
               Considering how influential these three battleground states are, 
               it's important to understand voting tendencies in these states. A 
               few weeks ago, Wisconsin, Michigan and Pennsylvania voted 
               together again for Joseph R. Biden Jr., making the 
               Pennsylvania-born Democrat the 46th President of the United 
               States. However, comprehensive data is not yet availible for the 
               2020 Election. Therefore, in this project, results from the 2012 
               and 2016 elections are studied. From 2012 to 2016, the electorate 
               in Wisconsin, Michigan and Pennsylvania swung by 1,020,051 votes 
               towards Republicans, awarding Donald Trump all 46 electoral votes 
               and the presidency. In order to understand this drastic swing, I 
               study how parts of the electorate changed preference between 
               election cycles, specifically the major demographic categories of 
               race, education attainment, monthly income and population within 
               counties."), 
             h3("About Me"),
             p("My name is Owen Asnis and I am an A.B. candidate in Government 
               on the Public Policy track at Harvard College (class of 2023). 
               You can reach me at owenasnis@gmail.com or 
               oasnis@college.harvard.edu.")),  
    tabPanel("Results: President", 
             fluidPage(
                 titlePanel("Results: President"), 
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("year",
                                     "Select an election year:", 
                                     choices = list("2012", "2016"))),
                     mainPanel(
                         plotlyOutput("results"))))), 
    tabPanel("What Changed?", 
             fluidPage(
                 titlePanel("What Changed?"), 
                 mainPanel(plotlyOutput("swingsmap")),
                 fluidPage(column(8, 
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("state",
                                     "Select a state:", 
                                     choices = list("Wisconsin",
                                                    "Michigan", 
                                                    "Pennsylvania"))),
                     mainPanel(
                         plotOutput("countychange"))))))), 
    tabPanel("Models", 
             fluidPage(
                 titlePanel("Model: Major Demographics and Vote Share"), 
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("variable", 
                                     "Select a variable to run regression model:", 
                                     choices = list("white_pct", 
                                                    "nonwhite_pct", 
                                                    "less_college_pct", 
                                                    "college_more_pct"))), 
                     mainPanel(
                         plotOutput("pp"))))))

server <- function(input, output) {
    output$wimap <- renderPlot({
    plot_usmap(include = "WI", 
               regions = "counties")
    }, height = 250, width = 250)
    output$wiseal <- renderImage({
        list(src = "wisconsin.png", 
             width = 250, 
             height = 250, 
             alt = "This is alternative text") 
    }, deleteFile = FALSE)
    output$miseal <- renderImage({
        list(src = "michigan.png", 
             width = 250, 
             height = 250, 
             alt = "This is alternative text")
    }, deleteFile = FALSE)
    output$mimap <- renderPlot({
        plot_usmap(include = "MI", 
                   regions = "counties")
    }, height = 250, width = 250)
    output$pamap <- renderPlot({
        plot_usmap(include = "PA", 
                   regions = "counties")
    }, height = 250, width = 250)
    output$paseal <- renderImage({
        list(src = "pennsylvania.png", 
             width = 250, 
             height = 250,
             alt = "This is alternative text")
    }, deleteFile = FALSE)
    output$results <- renderPlotly({
        if(input$year == "2012"){
            ggplotly(ggplot(data = map12, 
                            aes(geometry = geometry, 
                                text = paste("2012 RESULTS: PRESIDENT", "<br>", 
                                             NAME, "<br>",
                                             "Obama/Biden (D - inc):", dem_vs, "%", "<br>", 
                                             "Romney/Ryan (R):", rep_vs, "%", "<br>", 
                                             "State Population Rank:", pop_rank, "of", total_counties))) +
                         geom_sf(aes(fill = difference),
                                 show.legend = FALSE) + 
                         theme_map() + 
                         scale_fill_gradient2(name = "Vote Share", 
                                              low = "red2",
                                              mid = "white",
                                              high = "blue3", 
                                              midpoint = 0) + 
                         labs(title = "2012 Results By County (hover for results)"), 
                     tooltip = c("text")) %>%
                layout(showlegend = FALSE)  
        }
        else if(input$year == "2016"){
            ggplotly(ggplot(data = map16, 
                            aes(geometry = geometry, 
                                text = paste("2016 RESULTS: PRESIDENT", "<br>", 
                                NAME, "<br>",
                                "Clinton/Kaine (D):", dem_vs, "%", "<br>", 
                                "Trump/Pence (R):", rep_vs, "%", "<br>", 
                                "State Population Rank:", pop_rank, "of", total_counties)))+
                         geom_sf(aes(fill = difference),
                                 show.legend = FALSE) + 
                         theme_map() + 
                         scale_fill_gradient2(name = "Vote Share", 
                                              low = "red2",
                                              mid = "white",
                                              high = "blue3", 
                                              midpoint = 0) + 
                         labs(title = "2016 Results By County (hover for results)"), 
                     tooltip = c("text")) %>%
                layout(showlegend = FALSE)  
        }
    })
    output$swingsmap <- renderPlotly({
        ggplotly(ggplot(data = trend, aes(geometry = geometry, 
                                          text = paste("COUNTY SWINGS", "<br>", 
                                                       NAME, "<br>", 
                                                       "Swing:", "+", abs(dem_chg), "%", 
                                                       case_when(dem_chg > 0 ~ "towards Democrats", 
                                                                 dem_chg < 0 ~ "towards Republicans", 
                                                                 dem_chg == 0 ~ "EVEN"), "<br>", 
                                                       "State Population Rank:", pop_rank, "of", total_counties))) + 
                     geom_sf(aes(fill = dem_chg), 
                             show.legend = FALSE) + 
                     theme_map() +
                     labs(title = "County Swings (hover for swings)") + 
                     scale_fill_gradient2( low = "red2",
                                          mid = "white",
                                          high = "blue3", 
                                          midpoint = 0), 
                 tooltip = c("text")) %>% 
            layout(showlegend = FALSE)
    })
    output$countychange <- renderPlot({
        trend %>% 
            filter(state == input$state) %>% 
        ggplot(aes(x = dem_chg)) + 
            geom_histogram(binwidth = 5, 
                           color = "white", 
                           fill = "dodgerblue2") + 
            scale_x_continuous(breaks = c(-30, -20, -10, 0, 10), 
                               labels = c("+30% R", "+20% R", "+10% R", 
                                          "Even", "+10% D")) +
            labs(title = "County Swings in Selected State", 
                 x = "Vote Share Change", 
                 y = "Number of Couties Selected State", 
                 subtitle = "Most counties swing towards Republicans") + 
            theme_economist() +  
            geom_vline(xintercept = 0, 
                       alpha = 0.5, 
                       color = "purple", 
                       )
    })
    output$pp <- renderPlot({
        post_12 <- countypres12 %>%
            ungroup() %>% 
            select(rep_vs, input$variable) %>% 
            rename(select_variable = input$variable) %>% 
            stan_glm(formula = rep_vs ~ select_variable, 
                     refresh = 0, 
                     family = gaussian()) %>% 
            as_tibble() %>% 
            select(select_variable) %>% 
            mutate(year = "2012")
        
        post_16 <- countypres16 %>%
            ungroup() %>% 
            select(rep_vs, input$variable) %>% 
            rename(select_variable = input$variable) %>% 
            stan_glm(formula = rep_vs ~ select_variable, 
                     refresh = 0, 
                     family = gaussian()) %>% 
            as_tibble() %>% 
            select(select_variable) %>% 
            mutate(year = "2016")
        
        post_predictions <- bind_rows(post_12, post_16)
        
        ggplot(data = post_predictions, aes(x = select_variable, fill = year)) + 
            geom_histogram(aes(y = after_stat(count / sum(count))), 
                           alpha = 0.65, 
                           bins = 100, 
                           color = "white", 
                           position = "identity") + 
            scale_y_continuous(labels = scales::percent_format()) + 
            labs(title = "Posterior Probability Distribution", 
                 y = "Probability", 
                 x = "Predicted % influence on Republican vote share") + 
            theme_economist() + 
            scale_fill_manual(name = "Election Year", 
                              values = c("royalblue", "gold1"))
    })
}

shinyApp(ui = ui, server = server)