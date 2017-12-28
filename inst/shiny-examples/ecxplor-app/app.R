library(ggplot2)
library(dplyr)
library(network)
library(networkD3)
library(sna)
library(GGally)
library(shiny)

library(ecxplor)

choices <- rownames(model_object[["proximity"]])
choices_panel <- tbl_df(data.frame(product = choices,
                                   stringsAsFactors = FALSE))

product_info_panel <- model_object[["product_info"]]

choices_info_panel <- tbl_df(merge(choices_panel, product_info_panel, all.x = TRUE))
choices_to_display <- choices_info_panel[["product"]]
names(choices_to_display) <- choices_info_panel[["name"]]
unnamed_choices_to_display <- choices_to_display[is.na(names(choices_to_display))]3
named_choices_to_display <- choices_to_display[!is.na(names(choices_to_display))]

names(unnamed_choices_to_display) <- unnamed_choices_to_display
final_choices_to_display <- c(named_choices_to_display, unnamed_choices_to_display)

exports_panel <- model_object[["exports"]]

ui <- fluidPage(

    titlePanel("Economic Complexity Explorer"),

    sidebarLayout(

        sidebarPanel(

            selectInput(inputId = "data_tag",
                        label = "Data source:",
                        choices = c("OEC", "BACI"),
                        multiple = FALSE),
            selectInput(inputId = "hs_rev_year",
                        label = "HS revision year:",
                        choices = c("1992", "1996", "2002", "2007"),
                        multiple = FALSE),
            selectInput(inputId = "hs_rev_digit",
                        label = "Product scheme digits:",
                        choices = c("4", "6"),
                        multiple = FALSE),
            selectInput(inputId = "input_year",
                        label = "Product scheme digits:",
                        choices = c("4", "6"),
                        multiple = FALSE),

            selectInput(inputId = "focus_product",
                        label = "Product focus:",
                        choices = final_choices_to_display,
                        multiple = FALSE),
            sliderInput(inputId = "search_depth",
                        label = "Search depth:",
                        min = 1,
                        max = 1000,
                        value = 50)

        ),

        mainPanel(

            forceNetworkOutput(outputId = "product_space_plot"),
            tableOutput(outputId = "product_focus_info"),
            tableOutput(outputId = "exports_focus_info")

        )

    )
)


server <- function(input, output) {

    output$model_object_label <- renderText( {

        hs_rev_year <- "1996"
        hs_rev_digit <- "6"
        input_year <- 2014
        phi_cutoff <- 0.55

        model_object <- runModel("baci", hs_rev_year, hs_rev_digit, input_year, TRUE)

    })

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

    output$product_focus_info <- renderTable(
        product_info_panel %>% filter(product == input$focus_product)
    )

    output$exports_focus_info <- renderTable(
        exports_panel %>% filter(product == input$focus_product) %>%
        arrange(year, country, product) %>%
        select(year, country, export_val)
    )


}

shinyApp(ui, server)
