
library(shinydashboard)
library(shiny)
library(dplyr)#install.packages("dplyr")
library(readr)#install.packages("readr")
library(tidyr)#install.packages("tidyr")
# library(igraph)#install.packages("igraph") #install.packages("XML")
#install.packages("scales")#install.packages("lazyeval")#install.packages("lazyeval")
library(collapsibleTree);#install.packages("collapsibleTree",dependencies = TRUE)
# library(formattable)#install.packages("formattable")


phd_file <- here::here("data","2019","2019-02-19","phd_by_field.csv")
phd_field <- readr::read_csv(phd_file) 
# phd_field <- phd_raw %>%  mutate(path_string = paste("world", data$group, data$subgroup, data$subsubgroup, sep = "/"))
# sample: http://shiny.rstudio.com/gallery/submitbutton-demo.html
# UI

# library(data.tree)
# data$pathString <- paste("world", data$group, data$subgroup, data$subsubgroup, sep = "/")
# population <- as.Node(data)
# 
# 
# circlepackeR::circlepackeR(phd_raw)


shiny_ui <- fluidPage(
    titlePanel("Tidytuesday - PhDs Awarded by Field"),
    fluidRow(
        column(3, 
               wellPanel(
                selectInput(
                    "year", label = "Year:",
                    choices = 2008:2017, selected = 2017,
                    selectize = FALSE),
                div(valueBoxOutput("phds"))

        )),
        column(9,
               tabsetPanel(type = "tabs",
                           tabPanel("Tree view", collapsibleTreeOutput("tree")),
                           tabPanel("By Broad Field", DT::dataTableOutput("broad")),
                           tabPanel("Top 10 Fields", DT::dataTableOutput("fields")))

        )
    )
)



# server

shiny_server <- function(input, output) {
    
    count_phds <- reactive({
        
        count_total <- phd_field %>%
            filter(
                year == input$year,
                !is.na(n_phds)
            ) %>% 
            ungroup() %>%
            summarise(total = sum(n_phds)) %>%
            pull() %>% prettyNum(., big.mark = ",")
        
        count_total 
    })
    
    output$phds <- renderValueBox({
            valueBox(value = count_phds(), 
                     subtitle = "Total awarded",
                     color = "aqua")
    })
    
    output$tree <- renderCollapsibleTree({
        phd_field %>%
            filter(
                year == input$year,
                !is.na(n_phds)
            ) %>%
            collapsibleTreeSummary(
                hierarchy = c("broad_field", "major_field", "field"),
                attribute = "n_phds"
            )
    })
    
    output$fields <- DT::renderDataTable({ 
        DT::datatable(
        phd_field %>%
            filter(year == input$year) %>%
            replace_na(list(n_phds = 0)) %>%
            group_by(field) %>%
            summarise(Total = sum(n_phds)) %>%
            top_n(10) #%>%
            # formattable(list(
            #     Total = color_tile("white", "#56B4E9")
            )
    })
    
    output$broad <- DT::renderDataTable({
        DT::datatable(
        phd_field %>%
            filter(year == input$year) %>%
            replace_na(list(n_phds = 0)) %>%
            group_by(broad_field) %>%
            summarise(Total = round(sum(n_phds) / 1000)) %>%
            rename(`Broad field` = broad_field) 
        # %>%
            # formattable(list(
            #     Total = color_tile("white", "orange")
            )
    })
}


shinyApp(ui = shiny_ui , server =shiny_server)