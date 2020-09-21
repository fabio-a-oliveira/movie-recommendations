# Housekeeping -----------------------------------------------------------------

library(tidyverse)
library(shiny)


# Pre-processing ---------------------------------------------------------------

load(file = "movies.RData")
movies$selection <- 
    movies$selection %>% 
    mutate("quality modifier" = `average rating`^2/25,
           "popularity modifier" = `number of ratings` / 
               max(`number of ratings`))


# User Interface ---------------------------------------------------------------

ui <- fluidPage(
    
    tags$h2("Movie Recommender"),
    
    tags$p("Select a tab to receive either a random recommendation or a recommendation based on your favorite movies"),
    
    tabsetPanel(
        tabPanel("Random",
                 wellPanel(
                     fluidRow(tags$h4(tags$strong("Instructions:"))),
                     fluidRow("Set the recommendation options and hit Submit to receive a recommendation with randomly selected movies.")
                 ),
                 
                 wellPanel(
                     fluidRow(tags$h4(tags$strong("Options:"))),
                     checkboxInput(inputId = "preferBest",label = "Prefer best movies", value = TRUE),
                     checkboxInput(inputId = "preferPopular", label = "Prefer most popular movies", value = TRUE),
                     numericInput(inputId = "numberRecommendationsRandom", label = "Desired number of movie recommendations:", value = 10, min = 1, max = 50),
                     actionButton(inputId = "submitRandom", label = "SUBMIT")
                 ),
                 
                 tableOutput(outputId = "recommendationRandom")
        ),
        tabPanel("Favorites",
                 wellPanel(
                     fluidRow(tags$h4(tags$strong("Instructions:"))),
                     fluidRow("Select your favorite movies and hit Submit to receive a recommendation with similar movies.")
                 ),
                 
                 wellPanel(
                     fluidRow(tags$h4(tags$strong("Options:"))),
                     selectInput(inputId = "favMovie01",label="Choose your 3 favorite movies",choices = arrange(movies$selection,title)$title, size = 5, selectize = FALSE, selected = movies$selection$title[which(movies$selection$movieId == 260)]),
                     selectInput(inputId = "favMovie02",label=NULL,choices = arrange(movies$selection,title)$title, size = 5, selectize = FALSE, selected = movies$selection$title[which(movies$selection$movieId == 1196)]),
                     selectInput(inputId = "favMovie03",label=NULL,choices = arrange(movies$selection,title)$title, size = 5, selectize = FALSE, selected = movies$selection$title[which(movies$selection$movieId == 1210)]),
                     numericInput(inputId = "numberRecommendationsFavs", label = "Desired number of movie recommendations:", value = 10, min = 1, max = 50),
                     actionButton(inputId = "submitFavs", label = "SUBMIT")
                 ),
                 
                 tableOutput(outputId = "recommendationFavs")
 
        )
    )
    
    
)

# Server -----------------------------------------------------------------------

server <- function(input, output) {
    
    recRandom <- eventReactive(input$submitRandom, {
        
        best <- as.numeric(input$preferBest)
        popular <- as.numeric(input$preferPopular)
        
        weights <- 
            (best * movies$selection$`quality modifier` + 1 - best) *
            (popular * movies$selection$`popularity modifier` + 1 - popular)
        
        movies$selection %>% 
            slice_sample(n = input$numberRecommendationsRandom,
                         weight_by = weights,
                         replace = TRUE) %>% 
            transmute(title,
                      "average (total)" = paste(round(`average rating`,1),"/5 (",
                                                `number of ratings`,')',sep=""),
                      genres)
    })
    output$recommendationRandom <- renderTable(recRandom())

    
    recFavs <- eventReactive(input$submitFavs, {
        
        favs <- c(input$favMovie01,input$favMovie02,input$favMovie03)
        favsId <-
            movies$selection %>% 
            filter(title %in% favs) %>% 
            pull(movieId)
        
        movies$correlations[,as.character(favsId)] %>% 
            as.data.frame() %>% 
            rownames_to_column("movieId") %>% 
            mutate(movieId = as.integer(movieId)) %>% 
            pivot_longer(cols = -movieId,
                         names_to = "fav",
                         values_to = "cor") %>% 
            group_by(movieId) %>% 
            summarise(score = mean(cor),
                      .groups = 'drop') %>% 
            left_join(movies$selection, by = "movieId") %>%
            filter(!(movieId %in% favsId)) %>% 
            slice_max(order_by = score, n = input$numberRecommendationsFavs) %>% 
            arrange(desc(score)) %>% 
            transmute(title,
                      "average (total)" = paste(round(`average rating`,1),"/5 (",
                                                `number of ratings`,')',sep=""),
                      genres)
        
    })
    output$recommendationFavs <- renderTable(recFavs())
    
    
}

# Run the application ----------------------------------------------------------

shinyApp(ui = ui, server = server)
