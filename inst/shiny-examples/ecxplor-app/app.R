library(networkD3)
library(shiny)

library(ecxplor)
library(ecxplorappdata)

choices <- rownames(ecxplorappdata::model_obj[["proximity"]])
choices_panel <- tbl_df(data.frame(product = choices,
                                   stringsAsFactors = FALSE))

product_info_panel <- ecxplorappdata::model_obj[["products_info"]]

choices_info_panel <- tbl_df(merge(choices_panel, product_info_panel, all.x = TRUE))
choices_to_display <- choices_info_panel[["product"]]
names(choices_to_display) <- choices_info_panel[["name"]]
unnamed_choices_to_display <- choices_to_display[is.na(names(choices_to_display))]
named_choices_to_display <- choices_to_display[!is.na(names(choices_to_display))]

names(unnamed_choices_to_display) <- unnamed_choices_to_display
final_choices_to_display <- c(named_choices_to_display, unnamed_choices_to_display)

exports_panel <- ecxplorappdata::model_obj[["exports"]]

ui <- fluidPage(

    titlePanel("Economic Complexity Explorer"),

    sidebarLayout(

        sidebarPanel(

            selectInput(inputId = "focus_product",
                        label = "Product focus:",
                        choices = final_choices_to_display,
                        multiple = FALSE),
            sliderInput(inputId = "search_depth",
                        label = "Search depth:",
                        min = 1,
                        max = 250,
                        value = 12)

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

    })

    output$product_space_plot <- renderForceNetwork(        {
            prepared_model <- prepareFocusedModelObject(ecxplorappdata::model_obj, input$focus_product, input$search_depth)
            forceNetwork(Links = prepared_model[["d3_edges"]],
                         Nodes = prepared_model[["d3_nodes"]],
                         Source = "from",
                         Target = "to",
                         Value = "weight",
                         Group = "from",
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
