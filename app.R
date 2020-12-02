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

nhgis <- read_csv("raw_data/nhgis.csv") %>% 
    mutate(FIPS = paste0(STATEA, COUNTYA), 
           white_pct = (AF2ME002 / AF2ME001) * 100,
           raw_white = AF2ME002, 
           black_pct = (AF2ME003 / AF2ME001) * 100, 
           raw_black = AF2ME003, 
           asian_pct = (AF2ME005 / AF2ME001) * 100,
           raw_asian = AF2ME005, 
           other_pct = (AF2ME004 + + AF2ME006 + AF2ME007 + AF2ME008 / AF2ME001) 
           * 100,
           raw_other = AF2ME004 + + AF2ME006 + AF2ME007 + AF2ME008, 
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

dem_trend <- countypres %>%
    mutate(dem_diff = dem_vs - rep_vs) %>%  
    select(FIPS, year, dem_diff) %>% 
    group_by(FIPS) %>% 
    summarize(dem_chg = diff(dem_diff), 
              .groups = "drop") 

swings <- dem_trend %>% 
    arrange(dem_chg)

swings_map_r <- swings %>% 
    select(FIPS, dem_chg) %>% 
    filter(dem_chg < 0) %>% 
    rename(fips = FIPS, value = dem_chg)

swings_map_d <- swings %>% 
    select(FIPS, dem_chg) %>% 
    filter(dem_chg > 0) %>% 
    rename(fips = FIPS, value = dem_chg)

demographics <- countypres %>% 
    ungroup() %>% 
    filter(year == 2016) %>% 
    select(-year, -dem_vs, -rep_vs) 

top_25_summary_stats <- swings %>% 
    arrange(desc(abs(dem_chg))) %>% 
    head(25) %>% 
    left_join(demographics, by = "FIPS") %>% 
    summarize(total_white = sum(raw_white), 
              total_nonwhite = sum(raw_black + raw_asian + raw_other), 
              total_less_college = sum(raw_high_school_degree + 
                                           raw_college_no_degree), 
              total_college_more = sum(raw_bachelors_degree + raw_grad_degree), 
              total_population = sum(pop), 
              avg_pop = mean(pop), 
              avg_income = mean(median_household_income)) %>% 
    mutate(white_pct = (total_white / total_population) * 100, 
           nonwhite_pct = (total_nonwhite / total_population) * 100, 
           less_college_pct = (total_less_college / total_population) * 100, 
           college_more_pct = (total_college_more / total_population) * 100) %>%  
    select(white_pct, nonwhite_pct, less_college_pct, college_more_pct, avg_pop, 
           avg_income)

summary_stats <- swings %>% 
    left_join(demographics, by = "FIPS") %>% 
    summarize(total_white = sum(raw_white), 
              total_nonwhite = sum(raw_black + raw_asian + raw_other), 
              total_less_college = sum(raw_high_school_degree + 
                                           raw_college_no_degree), 
              total_college_more = sum(raw_bachelors_degree + raw_grad_degree), 
              total_population = sum(pop), 
              avg_pop = mean(pop),
              avg_income = mean(median_household_income)) %>% 
    mutate(white_pct = (total_white / total_population) * 100, 
           nonwhite_pct = (total_nonwhite / total_population) * 100, 
           less_college_pct = (total_less_college / total_population) * 100, 
           college_more_pct = (total_college_more / total_population) * 100) %>%  
    select(white_pct, nonwhite_pct, less_college_pct, college_more_pct, avg_pop, 
           avg_income)

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
    rename(fips = FIPS) %>% 
    mutate(difference = dem_vs - rep_vs) %>% 
    left_join(rustbelt_geometry, by = "fips") %>% 
    mutate(dem_vs = round(dem_vs, digits = 1), 
           rep_vs = round(rep_vs, digits = 1))

map16 <- countypres16 %>% 
    ungroup() %>% 
    select(FIPS, dem_vs, rep_vs) %>% 
    rename(fips = FIPS) %>% 
    mutate(difference = dem_vs - rep_vs) %>% 
    left_join(rustbelt_geometry, by = "fips") %>% 
    mutate(dem_vs = round(dem_vs, digits = 1), 
           rep_vs = round(rep_vs, digits = 1))

ui <- navbarPage(
    "The Blue Wall: How Wisconsin, Michigan and Pennsylvania Decide Elections",
    theme = shinytheme("sandstone"), 
    tabPanel("About the Project",
             h3("The State of Wisconsin (10 Electoral Votes)"),
             p("In 2012, President Barack Obama defeated Mitt Romney in 
               Wisconsin by 205,204 votes (roughly 6.7%), claiming the state's 
               10 electoral votes. However, four years later, Donald Trump 
               defeated Hillary Clinton in Wisconsin by 22,748 votes(roughly 
               0.7%). In four years, Wisconsin's electorate swung by 227,952 
               votes."),
             h3("The State of Michigan (16 Electoral Votes)"), 
             p("In 2012, President Obama defeated Romney in Michigan by 449,238 
               votes (roughly 9.5%), claiming the state's 16 electoral votes. 
               Four years later, Trump defeated Clinton in Michigan by 10,704 
               votes (roughly 0.3%). In four years, Michigan's electorate swung 
               by 459,942 votes."),
             h3("The Commonwealth of Pennsylvania (20 Electoral Votes)"), 
             p("In 2012, President Obama defeated Romney in Pennsylvania by 
               287,865 votes (roughly 5.2%). claiming the commonwealth's 20 
               electoral votes. Four years later, Trump defeated Clinton in 
               Pennsylvania by 44,292 votes (roughly 0.7%). In four years, 
               Pennsylvania's electorate swung by 332,157 votes."), 
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
                 titlePanel("Recent Results: President"), 
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("select",
                                     "Election Year:", 
                                     choices = list("2012", "2016"))),
                     mainPanel(
                         plotlyOutput("results", width = "100%"))))), 
    tabPanel("What Changed?", 
             fluidPage(
                 titlePanel("2012 versus 2016: Swing Counties"), 
                 mainPanel(plotOutput("trend"), 
                           plotOutput("rpickup"), 
                           plotOutput("dpickup")))), 
    tabPanel("Models and Stats", 
             fluidPage(
                 titlePanel("Model: Major Demographics and Vote Share"), 
                 mainPanel(tableOutput("compare")))))

server <- function(input, output) {
    output$results <- renderPlotly({
        if(input$select == "2012"){
            ggplotly(ggplot(data = map12, 
                            aes(geometry = geometry, 
                                text = paste(NAME, "<br>",
                                             "Barack H. Obama (D):", dem_vs, "%", "<br>", 
                                             "Mitt Romney (R):", rep_vs, "%"))) +
                         geom_sf(aes(fill = difference < 0),
                                 show.legend = FALSE) + 
                         theme_map() + 
                         scale_fill_manual(values = c("deepskyblue", "red1")),
                     tooltip = c("text")) %>%
                layout(showlegend = FALSE) 
        }
        else if(input$select == "2016"){
            ggplotly(ggplot(data = map16, 
                            aes(geometry = geometry, 
                                text = paste(NAME, "<br>",
                                             "Hillary R. Clinton (D):", dem_vs, "%", "<br>", 
                                             "Donald J. Trump (R):", rep_vs, "%"))) +
                         geom_sf(aes(fill = difference < 0),
                                 show.legend = FALSE) + 
                         theme_map() + 
                         scale_fill_manual(values = c("deepskyblue", "red1")),
                     tooltip = c("text")) %>%
                layout(showlegend = FALSE)  
        }
    })
    output$trend <- renderPlot({
        dem_trend %>% 
            mutate(state = case_when(str_sub(FIPS, 1, 2) == 26 ~ "Michigan", 
                                     str_sub(FIPS, 1, 2) == 42 ~ "Pennsylvania", 
                                     str_sub(FIPS, 1, 2) == 55 ~ "Wisconsin")) %>%
            ggplot(aes(x = dem_chg, fill = state)) + 
            geom_density(alpha = 0.75) + 
            scale_x_continuous(breaks = c(-30, -20, -10, 0, 10), 
                               labels = c("+30% R", "+20% R", "+10% R", 
                                          "Even", "+10% D")) + 
            scale_fill_manual(breaks = c("Michigan", "Pennsylvania", "Wisconsin"), 
                              values = c("darkgreen", "midnightblue", "red3"), 
                              name = "County State") +
            labs(title = "Vote Share Change from 2012 to 2016", 
                 subtitle = "Republicans gain in nearly all MI, PA and WI counties", 
                 x = "Vote Share Change", 
                 y = "Percentage of Counties") + 
            theme_minimal() + 
            scale_y_continuous(labels = scales::percent_format())
    })
    output$rpickup <- renderPlot({
        plot_usmap(include = c("WI", "MI", "PA"), 
                   regions = "counties", 
                   data = swings_map_r, 
                   values = "value") + 
            scale_fill_gradient2(name = "Swing", 
                                 low = "red1",
                                 mid = "white",
                                 high = "darkblue", 
                                 midpoint = 0, 
                                 breaks = c(0, -10, -20, -30), 
                                 labels = c("Even", "+10% R", "+20% R", "+30% R")) + 
            labs(title = "Counties Where Republicans Outperformed 2012", 
                 subtitle = "Huge rural gains, and minor gains in urban centers", 
                 caption = "Source: MIT Election Lab")
    })
    output$dpickup <- renderPlot({
        plot_usmap(include = c("WI", "MI", "PA"), 
                   regions = "counties", 
                   data = swings_map_d, 
                   values = "value") + 
            scale_fill_gradient2(name = "Swing", 
                                 low = "red1",
                                 mid = "white",
                                 high = "darkblue", 
                                 midpoint = 0, 
                                 breaks = c(0, 10), 
                                 labels = c("Even", "+10% D")) + 
            labs(title = "Counties Where Democrats Outperformed 2012", 
                 subtitle = "Minor suburban gains, but failure in rural areas and urban centers", 
                 caption = "Source: MIT Election Lab")
    })
    output$compare <- renderTable({
        full_join(top_25_summary_stats, summary_stats, 
                  by = c("white_pct", "nonwhite_pct", 
                         "less_college_pct", "college_more_pct", 
                         "avg_pop", "avg_income")) %>% 
            mutate(Source = c("Top 25 Swing Counties", "All Counties")) %>% 
            rename("White %" = white_pct, 
                   "Non-white %" = nonwhite_pct, 
                   "No College Degree %" = less_college_pct, 
                   "At Least College Degree %" = college_more_pct, 
                   "Average Population" = avg_pop, 
                   "Average Median Household Income" = avg_income) %>% 
            relocate("Source", .before = "White %")
    })
}

shinyApp(ui = ui, server = server)