library(shiny)
library(tidyverse)
library(tidycensus)
library(usmap)
library(rstanarm)
library(gtsummary)

nhgis <- read_csv("raw_data/nhgis.csv") %>% 
    mutate(FIPS = paste0(STATEA, COUNTYA), 
           white_pct = (AF2ME002 / AF2ME001) * 100,
           raw_white = AF2ME002, 
           black_pct = (AF2ME003 / AF2ME001) * 100, 
           raw_black = AF2ME003, 
           asian_pct = (AF2ME005 / AF2ME001) * 100,
           raw_asian = AF2ME005, 
           other_pct = (AF2ME009 / AF2ME001) * 100,
           raw_other = AF2ME009, 
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

e2012 <- countypres %>% 
    filter(year == 2012) %>% 
    ungroup() %>% 
    select(FIPS, "dem_vs") %>% 
    rename(fips = FIPS, value = dem_vs)

e2016 <- countypres %>% 
    filter(year == 2016) %>% 
    ungroup() %>% 
    select(FIPS, "dem_vs") %>%
    rename(fips = FIPS, value = dem_vs)

dem_trend <- countypres %>%
    mutate(dem_diff = dem_vs - rep_vs) %>%  
    select(FIPS, year, dem_diff) %>% 
    group_by(FIPS) %>% 
    summarize(dem_chg = diff(dem_diff), 
              .groups = "drop") 

swings <- dem_trend %>% 
    arrange(dem_chg)

swings_map <- swings %>% 
    select(FIPS, dem_chg) %>% 
    rename(fips = FIPS, value = dem_chg)

ui <- navbarPage(
    "The Blue Wall: Wisconsin, Michigan and Pennslyvania in 2012 and 2016",  
    tabPanel("Recent Results",
             fluidPage(
                 titlePanel("Election Results by County: President"),
                 mainPanel(plotOutput("Results12"), 
                           plotOutput("Results16"), 
                           plotOutput("Swingmap")))),
    tabPanel("About", 
             titlePanel("About"),
             h3("Background and Motivations"),
             p("In 2012, Barack Obama won a second term as the 44th President 
               of the United States by claiming the battleground states of 
               Florida, Ohio, and Iowa. Four years later, Donald Trump defeated
               Hillary Clinton in Florida, Ohio and Iowa. However, even with 
               these critical battleground victories, Trump still falls 10 
               electoral votes short. Wisconsin, Michigan and Pennslyvania, 
               also known as the Blue Wall, stood strong in 2012, but crumbled 
               in 2016, giving Trump the White House. Therefore, in 
               contemporary United States Presidential Elections, Wisconsin, 
               Michigan and Pennsylvania represent the median states. In other 
               words, the candidate who performs best in Wisconsin, Michigan and 
               Pennslyvania, wins the electoral college. Just a few weeks ago in 
               the 2020 election, Joe Biden defeated Donald Trump in all three 
               states and was elected the 46th President of the United States. 
               However, data is not completely availible yet for the 2020 
               election. Soon, a similar analysis can be done for the 2016 and 
               2020 elections, but for now, this analysis focuses on the 2012
               and 2016 elections. Given the importance of these three major 
               battlegrounds, it's important to ask: Why did these states vote 
               the way they did and which demographic categories are the 
               strongest predictors? I try my best to answer this question in my
               project."),
             h3("About Me"),
             p("My name is Owen Asnis and I am an A.B. candidate in Government 
               on the Public Policy track at Harvard College (class of 2023). 
               You can reach me at owenasnis@gmail.com or 
               oasnis@college.harvard.edu.")))


server <- function(input, output) {
    output$Results12 <- renderPlot({
        plot_usmap(include = c("WI", "MI", "PA"), 
                   regions = "counties", 
                   data = e2012, 
                   values = "value") + 
            scale_fill_gradient2(name = "Vote Share", 
                                 low = "red1",
                                 mid = "white",
                                 high = "darkblue", 
                                 midpoint = 50, 
                                 breaks = c(25, 50, 75), 
                                 labels = c("+25% R", "E", "+25% D")) + 
            labs(title = "2012 Presidential Election Results By County", 
                 subtitle = "Barack Obama wins all 46 electoral votes", 
                 caption = "Source: MIT Election Lab") 
    })
    output$Results16 <- renderPlot({
        plot_usmap(include = c("WI", "MI", "PA"), 
                   regions = "counties", 
                   data = e2016, 
                   values = "value") + 
            scale_fill_gradient2(name = "Vote Share", 
                                 low = "red1",
                                 mid = "white",
                                 high = "darkblue", 
                                 midpoint = 50, 
                                 breaks = c(25, 50, 75), 
                                 labels = c("+25% R", "E", "+25% D")) + 
            labs(title = "2016 Presidential Election Results By County", 
                 subtitle = "Donald Trump wins all 46 electoral votes", 
                 caption = "Source: MIT Election Lab") 
    })
    output$Swingmap <- renderPlot({
        plot_usmap(include = c("WI", "MI", "PA"), 
                   regions = "counties", 
                   data = swings_map, 
                   values = "value") + 
            scale_fill_gradient2(name = "Swing", 
                                 low = "red1",
                                 mid = "white",
                                 high = "darkblue", 
                                 midpoint = 0, 
                                 breaks = c(10, 0, -10, -20, -30), 
                                 labels = c("+10% D", "Even", "+10% R", 
                                            "+20% R", "+30% R")) + 
            labs(title = "2016 Change From 2012", 
                 subtitle = "Republicans gain in rural areas, while Democrats fail to grow lead in urban centers", 
                 caption = "Source: MIT Election Lab")
    })
}

shinyApp(ui = ui, server = server)
