makeNetworkFromModel <- function(model_object, phi_cutoff_value) {

    proximity_matrix <- model_object[["proximity"]]

    product_codes <- rownames(proximity_matrix)
    n_products <- length(product_codes)

    network_edges_from <- character(n_products ^ 2)
    network_edges_to <- character(n_products ^ 2)
    network_edges_weight = numeric(n_products ^ 2)

    i_edge <- 0

    for (i_row in 1:nrow(proximity_matrix)) {

        for (i_col in 1:ncol(proximity_matrix)) {

            from_code <- product_codes[[i_row]]
            to_code <- product_codes[[i_col]]

            weight_value <- proximity_matrix[i_row, i_col]

            row <- c(from_code, to_code, weight_value)

            network_edges_from[i_edge] <- from_code
            network_edges_to[i_edge] <- to_code
            network_edges_weight[i_edge] <- weight_value

            i_edge <- i_edge + 1

        }

    }

    network_edges_panel <- tbl_df(data.frame(from = network_edges_from,
                                             to = network_edges_to,
                                             weight = network_edges_weight))

    nice_edges_panel <- network_edges_panel %>% filter(weight > phi_cutoff_value) %>% filter(from != to)

    network_nodes_panel <- nice_edges_panel %>% distinct(from)

    exports_panel <- model_object[["exports"]]

    result <- list(nodes = network_nodes_panel,
                   edges = nice_edges_panel)

    return(result)

}

makeFocusedNetworkFromModel <- function(model_object, focus_product_code, n_search_depth) {

    findNextProducts <- function(proximity_matrix, focus_product_code, n_search_depth) {

        if (n_search_depth == 0) {
            return(c())
        } else {

            product_codes <- rownames(proximity_matrix)
            n_products <- length(product_codes)

            proximities <- proximity_matrix[, product_codes[product_codes == focus_product_code]]
            prox <- proximities[order(unlist(proximities))]
            prox_codes <- names(prox[(n_products - n_search_depth):n_products])

            filtered_proximity_matrix <- proximity_matrix[!(product_codes %in% prox_codes), !(product_codes %in% prox_codes)]

            result <- c(prox_codes, unlist(lapply(prox_codes, function(x) findNextProducts(filtered_proximity_matrix, x, n_search_depth - 1))))


            return(result)
        }

    }

    proximity_matrix <- model_object[["proximity"]]

    focus_products <- findNextProducts(proximity_matrix, focus_product_code, n_search_depth)
    focused_proximity_matrix <- proximity_matrix[rownames(proximity_matrix) %in% focus_products, colnames(proximity_matrix) %in% focus_products]

    product_codes <- focus_products
    proximity_matrix <- focused_proximity_matrix

    n_products <- length(product_codes)

    network_edges_from <- character(n_products ^ 2)
    network_edges_to <- character(n_products ^ 2)
    network_edges_weight = numeric(n_products ^ 2)

    i_edge <- 0

    for (i_row in 1:nrow(proximity_matrix)) {

        for (i_col in 1:ncol(proximity_matrix)) {

            from_code <- product_codes[[i_row]]
            to_code <- product_codes[[i_col]]

            weight_value <- proximity_matrix[i_row, i_col]

            row <- c(from_code, to_code, weight_value)

            network_edges_from[i_edge] <- from_code
            network_edges_to[i_edge] <- to_code
            network_edges_weight[i_edge] <- weight_value

            i_edge <- i_edge + 1

        }

    }

    network_edges_panel <- tbl_df(data.frame(from = network_edges_from,
                                             to = network_edges_to,
                                             weight = network_edges_weight))

    nice_edges_panel <- network_edges_panel %>% filter(from != to)

    network_nodes_panel <- nice_edges_panel %>% distinct(from)

    result <- list(nodes = network_nodes_panel,
                   edges = nice_edges_panel)

    return(result)

}

prepareFocusedModelObject <- function(model_object, product_code, depth_cutoff) {

    model_network_result <- makeFocusedNetworkFromModel(model_object, product_code, depth_cutoff)


    nodes_panel <- model_network_result[["nodes"]]
    edges_panel <- model_network_result[["edges"]]

    product_info_panel <- loadOECProductsInfoPanel(hs_rev_year, hs_rev_digit)
    countries_info_panel <- loadBACICountriesInfoPanel()

    edges_d3 <- data.frame(from = as.numeric(factor(edges_panel$from)) - 1,
                           to = as.numeric(factor(edges_panel$to)) - 1 )

    nodes_d3 <- cbind(idn = factor(nodes_panel$from,
                                   levels = nodes_panel$from),
                      nodes_panel)



    result <- list()

    result[["model"]] <- model_object
    result[["network"]] <- model_network_result
    result[["d3_nodes"]] <- nodes_d3
    result[["d3_edges"]] <- edges_d3
    result[["product_info"]] <- product_info_panel

    return(result)

}



## focusModelObject <- function(prepared_model, product_code) {

##     nodes_d3 <- prepared_model[["d3_nodes"]]
##     edges_d3 <- prepared_model[["d3_edges"]]

##     nodes_panel <- tbl_df(nodes_d3)
##     edges_panel <- tbl_df(edges_d3)

##     focused_nodes_panel <- nodes_panel %<%

## }
