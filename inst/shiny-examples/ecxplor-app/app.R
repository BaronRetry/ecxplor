rm(list = ls())

source("../../../R/load.R")
source("../../../R/model.R")
source("../../../R/output.R")
source("../../../R/viz.R")

library(dplyr)
library(network)
library(networkD3)
library(sna)
library(GGally)

library(shiny)

hs_rev_year <- "1996"
hs_rev_digit <- "6"
input_year <- 2014
phi_cutoff <- 0.55

model_object <- runModel("baci", hs_rev_year, hs_rev_digit, input_year, TRUE)



ui <- fluidPage(

    titlePanel("Economic Complexity Explorer"),

    sidebarLayout(

        sidebarPanel(

            selectInput(inputId = "focus_product",
                        label = "Product focus:",
                        choices = rownames(model_object[["proximity"]])),
            sliderInput(inputId = "search_depth",
                        label = "Search depth:",
                        min = 1,
                        max = 25,
                        value = 10)


        ),

        mainPanel(

            forceNetworkOutput(outputId = "product_space_plot")

        )

    )
)


server <- function(input, output) {

    output$product_space_plot <- renderForceNetwork(        {
            prepared_model <- prepareFocusedModelObject(model_object, input$focus_product, input$search_depth)
            forceNetwork(Links = prepared_model[["d3_edges"]],
                         Nodes = prepared_model[["d3_nodes"]],
                         Source="from",
                         Target="to",
                         Group="from",
                         NodeID = "idn",
                         linkWidth = 1,
                         linkColour = "#afafaf",
                         fontSize=12,
                         zoom=T,
                         legend=T,
                         opacity = 1,
                         width = 1600, height = 1600)


    })


}

shinyApp(ui, server)
