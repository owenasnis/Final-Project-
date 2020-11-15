library(shiny)
library(tidyverse)
library(ggthemes)

presidents <- read_csv("raw_data/1976-2016-president.csv", 
                       col_type = cols(year = col_double(),
                                       state = col_character(),
                                       state_po = col_character(),
                                       state_fips = col_double(),
                                       state_cen = col_double(),
                                       state_ic = col_double(),
                                       office = col_character(),
                                       candidate = col_character(),
                                       party = col_character(),
                                       writein = col_logical(),
                                       candidatevotes = col_double(),
                                       totalvotes = col_double(),
                                       version = col_double(),
                                       notes = col_logical())) %>% 
    filter(party %in% c("republican", "democrat"), 
           state %in% c("Arizona", "Florida", "Michigan", 
                        "North Carolina", "Pennsylvania", "Wisconsin")) %>% 
    drop_na(candidate) %>% 
    mutate(candidate_vote_percentage = (candidatevotes / totalvotes) * 100) %>%
    select(year, state, party, candidate_vote_percentage)

county <- read_csv("raw_data/countypres_2000-2016.csv", 
                   col_type = cols(year = col_double(),
                                   state = col_character(),
                                   state_po = col_character(),
                                   county = col_character(),
                                   FIPS = col_double(),
                                   office = col_character(),
                                   candidate = col_character(),
                                   party = col_character(),
                                   candidatevotes = col_double(),
                                   totalvotes = col_double(),
                                   version = col_double())) 

# Define UI for application that draws a histogram
ui <- navbarPage(
    "2020 Battleground States: Where the Election Will Be Won and Lost",
    tabPanel("Recent Battleground Electoral History",
             fluidPage(
                 titlePanel("Voting Trends in Battleground States"),
                     mainPanel(plotOutput("Battleground")))),
    tabPanel("Pennsylvania Battleground County Trends",
             fluidPage(
                 titlePanel("Trends in PA Battleground Counties"),
                 mainPanel(plotOutput("PA")))),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("My project focuses on presidential election analysis by studying 
               the 2020 swing states. Specifically, I will study demographic 
               trends within the swing states. By looking at history 
               and discovering trends, we can make a prediction for how the 
               states will vote in the upcoming 2020 election. For my first 
               graph, I used the U.S. President 1976-2016 data set from the MIT 
               Election Data + Science Lab. I wrangled the data and created a
               column that contains the percent vote share of the two major 
               parties. I selected six swing states from the upcoming election 
               and visualized the percentage vote share for each party over the 
               past thirty years. Here is my repository: 
               https://github.com/owenasnis/Final-Project- . This repository
               contains the Rmd where I wrangled my data."),
             h3("About Me"),
             p("My name is Owen Asnis and I study Government at Harvard.  
             You can reach me at oasnis@college.harvard.edu or 
               owenasnis@gmail.com.")))

server <- function(input, output) {
    output$Battleground <- renderPlot({
        presidents %>% 
            ggplot(aes(x = year, y = candidate_vote_percentage, 
                       color = party)) + 
            geom_line() + 
            facet_wrap(~ state) + 
            scale_color_manual(labels = c("Democrat", "Republican"), 
                               values = c("blue2", "red2"), 
                               name = "Candidate Party", 
                               breaks = c("democrat", "republican")) + 
            theme_bw() + 
            labs(title = "Recent Electoral History of 2020 Swing States", 
                 subtitle = "How will states trend in 2020?", 
                 x = "Election Year (1976 - 2016)", 
                 y = "Percentage Party Vote Share")
    })
    output$PA <- renderPlot({
        county %>% 
            filter(state_po == "PA", 
                   county %in% c("Erie", "Chester", "Berks", 
                                 "Lackawanna", "Bucks", "Northampton"), 
                   party %in% c("democrat", "republican")) %>% 
            select(- state, - state_po, - FIPS, - office, - version) %>% 
            mutate(vote_share = candidatevotes/totalvotes) %>% 
            ggplot(aes(x = year, y = vote_share, color = party)) + 
            geom_line() + 
            facet_wrap(~ county) + 
            scale_y_continuous(labels = scales::percent_format()) + 
            theme_fivethirtyeight() + 
            scale_color_manual(labels = c("Democrat", "Republican"), 
                               values = c("blue2", "red2"), 
                               name = "Candidate Party", 
                               breaks = c("democrat", "republican")) + 
            labs(title = "Recent Voting History in Pennsylvania Swing Counties", 
                 subtitle = "Republicans Improving in Most Pennsylvania Swing Counties",
                 x = "Election Year", y = "Candidate Vote Share")
})}
 
shinyApp(ui = ui, server = server)
