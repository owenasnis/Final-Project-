library(shiny)
library(tidyverse)

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

# Define UI for application that draws a histogram
ui <- navbarPage(
    "2020 Battleground States: Where the Election Will Be Won and Lost",
    tabPanel("Recent Battleground Electoral History",
             fluidPage(
                 titlePanel("Voting Trends in Battleground States"),
                     mainPanel(plotOutput("line_plot")))),
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
               past thirty years."),
             h3("About Me"),
             p("My name is Owen Asnis and I study Government at Harvard.  
             You can reach me at oasnis@college.harvard.edu or 
               owenasnis@gmail.com.")))

server <- function(input, output) {
    output$line_plot <- renderPlot({
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
}

 
shinyApp(ui = ui, server = server)
