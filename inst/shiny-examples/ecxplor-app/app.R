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

choices <- rownames(model_object[["proximity"]])
choices_panel <- tbl_df(data.frame(product = choices,
                                   stringsAsFactors = FALSE))
product_info_panel <- model_object[["product_info"]]

choices_info_panel <- tbl_df(merge(choices_panel, product_info_panel, all.x = TRUE))
choices_to_display <- choices_info_panel[["product"]]
names(choices_to_display) <- choices_info_panel[["name"]]
unnamed_choices_to_display <- choices_to_display[is.na(names(choices_to_display))]
named_choices_to_display <- choices_to_display[!is.na(names(choices_to_display))]

names(unnamed_choices_to_display) <- unnamed_choices_to_display
final_choices_to_display <- c(named_choices_to_display, unnamed_choices_to_display)


ui <- fluidPage(

    titlePanel("Economic Complexity Explorer"),

    sidebarLayout(

        sidebarPanel(

            selectizeInput(inputId = "focus_product",
                           label = "Product focus:",
                           choices = final_choices_to_display,
                           multiple = FALSE,
                           options = list(maxItems = 6000)),
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
