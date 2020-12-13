library(shiny)
library(shinythemes)
library(tidyverse)
library(tidycensus)
library(usmap)
library(rstanarm)
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
           total_counties = n(), 
           devo = case_when(pop < 50000 ~ "Rural",
                            pop >= 50000 & pop < 200000 ~ "Urban/Suburban", 
                            pop >= 200000 & pop < 500000 ~ "Small City", 
                            pop >= 500000 & pop <= 1000000 ~ "City", 
                            pop > 1000000 ~ "Metropolis")) 

demographics <- countypres %>% 
    ungroup() %>% 
    filter(year == 2016) %>% 
    select(-year, -dem_vs, -rep_vs, -pop) %>% 
    rename(fips = FIPS)

top_25_summary_stats <- trend %>% 
    ungroup() %>% 
    arrange(desc(abs(dem_chg))) %>% 
    head(25) %>% 
    left_join(demographics, by = "fips") %>% 
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

summary_stats <- trend %>%
    ungroup() %>% 
    left_join(demographics, by = "fips") %>% 
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

ui <- navbarPage(
    "The Battleground: Wisconsin, Michigan and Pennsylvania in Contemporary 
    Presidential Elections",
    theme = shinytheme("sandstone"), 
    tabPanel("About the Project",
                 mainPanel(
                     fluidRow(
                     column(4, 
                     plotOutput("wimap")), 
                 column(4, align = "center",
             h3("Wisconsin (10 Electors)"),
             p("In 2012, President Barack Obama defeated Mitt Romney in 
               Wisconsin by 205,204 votes (roughly 6.7%), claiming the state's 
               10 electoral votes. However, four years later, Donald Trump 
               defeated Hillary Clinton in Wisconsin by 22,748 votes(roughly 
               0.7%). In four years, Wisconsin's electorate swung by 227,952 
               votes.")),
             column(4, 
             imageOutput("wiseal"))),
             fluidRow(
                 column(4,
                        imageOutput("miseal")),
                 column(4, align = "center",
             h3("Michigan (16 Electors)"), 
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
             column(4,align = "center",
             h3("Pennsylvania (20 Electors)"), 
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
               counties. Data collected for the analysis is from the MIT 
               Election Lab and IPUMS NHGIS."), 
             h3("About Me"),
             p("My name is Owen Asnis and I am an A.B. candidate in Government 
               on the Public Policy track at Harvard College (class of 2023). 
               You can reach me at oasnis@college.harvard.edu. My GitHub profile
               and repository for this project can be found at 
               https://github.com/owenasnis."))),  
    tabPanel("Results: President",
             fluidPage(
                 titlePanel("Results: President"),
                 h3("Hover for Results"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("year",
                                     "Select an election year:", 
                                     choices = list("2012", "2016"))),
                     mainPanel(
                         plotlyOutput("results"))))), 
    tabPanel("What Changed?", 
             fluidPage(
                 column(12,
                 titlePanel("What Changed?"),
                 h3("County Trends: 2016 as compared to 2012 (Hover for 
                    Swings)"),
                 mainPanel(plotlyOutput("swingsmap"))),
                 p("Republicans gained in nearly all counties. In Wisconsin, 
                   Donald Trump flipped many counties in the state's South-West
                   region and gained nearly 9 points in Brown County, home of 
                   Green Bay - the state's 4th most populous county. In 
                   addition, Trump soared just South of Milwaukee, in Racine 
                   County and Kenosha County. In Michigan, Trump outperformed 
                   Romney nearly everywhere, most notably in Wayne County and 
                   Macomb County - the Detroit metropolitan area. Additionally, 
                   Trump outperformed Romney by nearly 20 points in Genesee 
                   County - home of Flint and the state's 5th most populous 
                   county. In Pennsylvania, Hillary Clinton performed relatively
                   well in the Philadelphia suburbs and Allegheny County - home 
                   of Pittsburgh. However, Trump outperformed Romney in the 
                   state's smaller cities, especially Luzerne County - home 
                   of Wilkes-Barre, Lackawanna County - home of Scranton - and 
                   Erie County - home of Erie."), 
                 h3("County Trends By State"), 
                 fluidPage(
                     column(12,
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("state",
                                     "Select a state:", 
                                     choices = list("Wisconsin",
                                                    "Michigan", 
                                                    "Pennsylvania"))),
                     mainPanel(
                         plotOutput("countychange")))))),
             h3("County Trends by Developed Environment (Hover for Swings)"), 
             fluidPage(
                 column(12,
                       sidebarLayout(
                           sidebarPanel(
                               selectInput("devo", 
                                           "Developed Environment:", 
                                           choices = list("Rural", 
                                                          "Urban/Suburban", 
                                                          "Small City", 
                                                          "City", 
                                                          "Metropolis"))),
                           mainPanel(
                               plotlyOutput("devop"))))), 
             p("It's difficult to designate developed environments for counties. 
               For this project, county population was used. Any county with a 
               population of under 50,000 was designated as rural. While 50,000 
               may seem large for a rural county, for these populous states, I 
               felt like this cutoff best represented the states' smaller, more 
               rural counties. Counties with a population between 50,000 and 
               200,000 were designated as Urban/Suburban. Some suburban counties
               could certainly have more than 200,000, such as the suburbs of 
               Philadelphia, however these designations are meant to give a 
               general sense for the size of the county. Counties with a 
               population between 200,000 and 500,000 were designated as a Small 
               City. Counties with a population between 500,000 and 1,000,000 
               were designated as a City. Finally, counties with a population 
               above 1,000,000 were designated as a metropolis, or a large 
               city. These designations show that Trump significantly 
               outperformed expectations in rural counties - Trump outperformed 
               Romney in every rural county. Trump also performed extremely well
               in Urban/Suburban counties - in all but three, Trump outperformed 
               Romney. Overall, Clinton performed well in Small City counties in 
               Wisconsin and Michigan, but Trump performed well in Small City 
               counties in Pennsylvania. Clinton largely outperformed Obama in 
               City counties, with the exception being Macomb County, Michigan, 
               part of the Detroit metropolitan area. Clinton saw solid gains in 
               cities such as Madison, Milwaukee, Grand Rapids, and the 
               Philadelphia metropolitan area. In the four metropolis counties, 
               Trump outperformed Clinton in Detroit, however the three others 
               were largely unchanged from 2012.")), 
    tabPanel("Models", 
             fluidPage(
                 column(12, 
                 titlePanel("Models and Statistics"),
                 h3("Model: Race and Education Attainment by Year"), 
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("variable", 
                                     "Select a variable to run regression 
                                     model:", 
                                     choices = c("white_pct", 
                                                    "nonwhite_pct", 
                                                    "less_college_pct", 
                                                    "college_more_pct"))), 
                     mainPanel(
                         plotOutput("pp"))))),
             p("This graph represents the posterior probability distribution for
               a regression model. The regression model calculates the expected 
               change in Republican vote share for every percentage point 
               increase in the dempgraphic categories of race and education 
               attainment in Wisconsin, Michigan and Pennsylvania counties. The 
               models were separated by election year to study the differences 
               in voting tendancies between the 2012 presidential election and 
               the 2016 presidential election. According to the models, race may 
               have had a stronger influence in predicting republican vote share
               in 2016, however, this prediction isn't made with overwhelming 
               confidence. The graphs show, with uncertainty, that white voters 
               tended to more strongly support Republicans in 2016, whereas 
               non-white voters tended to more strongly support Democrats. The 
               models were able to make clear, confident predictions in relation 
               to education attainment. Voters without a college degree were 
               more likely to support Republicans in 2016, and voters with at 
               least a college degree were more likely to support Democrats. 
               This prediction can be made with a good amount of certainty, 
               because the histograms do not overlap."), 
             h3("Statistics: Top 25 Swing Counties versus Overall"), 
             fluidPage(
                 column(12,
                        mainPanel(
                            tableOutput("stats")))), 
             p("Further statistical analysis proves that the swing towards Trump 
               in 2016 was powered by white, less educated, rural voters. The 
               table above displays average statistics from the top 25 swing 
               counties in 2016, all of which heavily swung towards Trump. To be 
               clear, for the white %, non-white %, no college degree %, and at 
               least college degree %, the stats were calculated using the 
               population of all counties combined, whereas the average 
               population and the average median household income are calculated 
               with each county population separated. The counties that swung 
               for Trump were very different from the counties in general. 
               Counties that swung for Trump tended to be significantly less 
               diverse, less educated and smaller. The median household income 
               was, on average, lower for swing counties, however, there wasn't 
               an enormous difference.")), 
    tabPanel("Moving Forward", 
                 titlePanel("The Future of the Battleground"),
                 h3("2020: Joseph R. Biden Jr. Flips the Battleground"),
             fluidPage(
                 column(6, 
                     plotOutput("bidenblue")), 
                 column(6,
                        imageOutput("joebiden"))), 
             p("Just a few weeks ago, Joseph R. Biden Jr. of Scranton, 
               Pennsylvania flipped the battleground once again. However, 
               comprehensive, complete data is not yet availible. From what we 
               know so far, it appears as though Biden paired marginal swings 
               with booms in turnout to turn the states blue once again. For 
               both parties, there is a lot of work to be done in the near 
               future in Wisconsin, Michigan and Pennsylvania. For Democrats, 
               the focus will be on the Metropolis, City, and Small City 
               counties, as the rural areas appear to have solidified for the 
               Republicans. The focus for Democrats should center around Green 
               Bay, Madison, Milwaukee, Detroit, Grand Rapids, Ann Arbor, Flint, 
               Pittsburgh, Philadelphia, Wilkes-Barre, Scranton, Erie and other 
               urban centers. Republicans will certainly contest many of the 
               Small City counties, and focus their efforts on driving up 
               support in the more rural areas. While so much has changed 
               between 2012 and 2020, one thing remains the same: the path to 
               the White House runs through Wisconsin, Michigan and 
               Pennsylvania.")))

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
                                             "Obama/Biden (D - inc):", 
                                             dem_vs, "%", "<br>", 
                                             "Romney/Ryan (R):", rep_vs, "%", 
                                             "<br>", "State Population Rank:", 
                                             pop_rank, "of", total_counties))) +
                         geom_sf(aes(fill = difference),
                                 show.legend = FALSE) + 
                         theme_map() + 
                         scale_fill_gradient2(name = "Vote Share", 
                                              low = "red2",
                                              mid = "white",
                                              high = "blue3", 
                                              midpoint = 0), 
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
                                "State Population Rank:", pop_rank, "of", 
                                total_counties)))+
                         geom_sf(aes(fill = difference),
                                 show.legend = FALSE) + 
                         theme_map() + 
                         scale_fill_gradient2(name = "Vote Share", 
                                              low = "red2",
                                              mid = "white",
                                              high = "blue3", 
                                              midpoint = 0),
                     tooltip = c("text")) %>%
                layout(showlegend = FALSE)  
        }
    })
    output$swingsmap <- renderPlotly({
        ggplotly(ggplot(data = trend, 
                        aes(geometry = geometry, 
                            text = paste("COUNTY SWINGS", "<br>", 
                                         NAME, "<br>", 
                                         "Swing:", "+", 
                                         abs(dem_chg), "%", 
                                         case_when(dem_chg > 0 ~ 
                                                       "towards Democrats", 
                                                   dem_chg < 0 ~ 
                                                       "towards Republicans", 
                                                   dem_chg == 0 ~ "EVEN"), 
                                         "<br>", "State Population Rank:", 
                                         pop_rank, "of", total_counties))) + 
                     geom_sf(aes(fill = dem_chg), 
                             show.legend = FALSE) + 
                     theme_map() +
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
            labs(title = "County Trends in Selected State", 
                 x = "Vote Share Change", 
                 y = "Number of Couties in Selected State", 
                 subtitle = "Most counties swing towards Republicans") + 
            theme_economist() +  
            geom_vline(xintercept = 0, 
                       alpha = 0.5, 
                       color = "purple")
    })
    output$devop <- renderPlotly({
            ggplotly(ggplot(data = trend %>% 
                                    filter(devo == input$devo), 
                            aes(geometry = geometry, 
                                text = paste("COUNTY SWINGS", "<br>", 
                                             NAME, "<br>", 
                                             "Swing:", "+", abs(dem_chg), "%", 
                                             case_when(dem_chg > 0 ~ 
                                                           "towards Democrats", 
                                                       dem_chg < 0 ~ 
                                                          "towards Republicans", 
                                                       dem_chg == 0 ~ "EVEN"), 
                                             "<br>", "State Population Rank:", 
                                             pop_rank, "of", total_counties))) + 
                         geom_sf(aes(fill = dem_chg), show.legend = FALSE) + 
                         theme_map() +
                         scale_fill_gradient2( low = "red2",
                                               mid = "white",
                                               high = "blue3", 
                                               midpoint = 0), 
            tooltip = c("text")) %>%
            layout(showlegend = FALSE)
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
                 x = "% Influence on Republican Vote Share 
                 For Every 1% Increase in Variable") + 
            theme_economist() + 
            scale_fill_manual(name = "Election Year", 
                              values = c("royalblue", "gold1"))
    })
    output$stats <- renderTable({
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
    output$bidenblue <- renderPlot({
        plot_usmap(regions = "states", 
                   include = c("WI", "MI", "PA"), 
                   fill = "blue1", 
                   size = 1.15)
    }) 
    output$joebiden <- renderImage({
        list(src = "biden.jpg",
             height = 350, 
             width = 500, 
             alt = "This is alternative text") 
    }, deleteFile = FALSE)
}

shinyApp(ui = ui, server = server)