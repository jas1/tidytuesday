
# imports -----------------------------------------------------------------

library(dplyr)
library(readr)
library(skimr) #; install.packages("skimr")
library(igraph)
library(visNetwork)
library(shiny)
library(shinydashboard)
library(stringr)


# load data ---------------------------------------------------------------

# load data
small_trains <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-26/small_trains.csv") 

# filter data
df_for_graph <- small_trains %>% 
    select(departure_station,arrival_station,year,month,journey_time_avg,total_num_trips) %>% 
    rename(from=departure_station) %>% 
    rename(to=arrival_station)

uniques_graph <- small_trains %>%    
        rename(from=departure_station) %>%
        rename(to=arrival_station) %>% 
        count(from,to) %>% 
        mutate(is_tgv=str_detect(from,"TGV")|str_detect(to,"TGV")) %>% 
        mutate(color=if_else(is_tgv,'red','blue'))

# make directed graph
trains_graph <- igraph::graph_from_data_frame(uniques_graph,directed = TRUE)


# UI ----------------------------------------------------------------------
header <- dashboardHeader(
    title="Tidy tuesday challenge: Week 2019-02-26 french train delays",
    titleWidth = 770#,
    #dropdownMenu(dropdownMenuOutput("msg_menu"))
)
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("French Trains As Graph",
                 tabName = "graph_pov"
        ),
        menuItem("Contact", 
                 href="https://www.juliospairani.com" )
    )
)

body <- dashboardBody(
    tabItems(
        # TAB graph pov --------------------------------------------------------------------------
        tabItem(tabName = "graph_pov",
                visNetworkOutput("output_network")
                )# end of graph pov
        ) # tab items end
    )# body end

ui <- dashboardPage(header, sidebar, body)


# SERVER ------------------------------------------------------------------
server <- function(input, output,session) {
    reactive_network <- reactive({
        set.seed(12345)
        visNetwork:::visIgraph(trains_graph) %>% 
            visNetwork:::visOptions(  selectedBy= list(variable = "label"), # esto hace aparecer combos en la red.
                                      highlightNearest = list(enabled = TRUE, hover = TRUE))
    })
    
    output$output_network <- renderVisNetwork({

        reactive_network()
    })
}



# shiny app ---------------------------------------------------------------
shinyApp(ui = ui, server = server)