## computeRCA
##
## Takes the results of prepare_input_data and computes the the revealed
## comparative advantage, or RCA, matrix. Each row represents a country and each
## column represents a product, in the standard order. For more details about
## the RCA, see Atlas p.25, Technical Box 2.2.

computeRCA <- function(exports_panel) {

    country_exports_panel <- exports_panel %>% group_by(country) %>%
        summarize(country_export_val = sum(export_val))

    product_exports_panel <- exports_panel %>% group_by(product) %>%
        summarize(product_export_val = sum(export_val))

    country_product_exports_panel <- exports_panel %>% group_by(country, product) %>%
        summarize(export_val = sum(export_val))

    total_exports_panel <- exports_panel %>% summarize(total_export_val = sum(export_val))

    calcs_panel <- country_product_exports_panel %>%
        inner_join(country_exports_panel) %>%
        inner_join(product_exports_panel) %>% rowwise() %>%
        mutate(total_export_val = total_exports_panel[[1]]) %>%
        mutate(rca_numerator = export_val / country_export_val) %>%
        mutate(rca_denominator = product_export_val / total_export_val) %>%
        mutate(rca = rca_numerator / rca_denominator)

    rca_panel <- calcs_panel %>% select(country, product, rca)

    return(rca_panel)

}

## computeComplexity
##
## Takes country codes, product codes, and the RCA matrix. This function creates
## the panel that will be translated into the matrix M. Each row represents a
## country and each column represents a product, in the standard order. If a
## country has RCA > 1 for some product, the entry for that country and product
## in M will be 1; otherwise it is 0.  For more details about the matrix M, see
## Atlas p.25, Technical Box 2.2.

computeComplexity <- function(rca_panel) {

    calcs_panel <- rca_panel %>% mutate(complexity = ifelse(rca > 1.0, 1.0, 0.0))
    complexity_panel <- calcs_panel %>% select(country, product, complexity)
    return(complexity_panel)

}

## computeM
##
## Takes the panel produced by computeComplexity and turns it into a matrix
## with the correct row (country) and column (product) names (codes).

computeM <- function(complexity_panel) {

    countries_panel <- complexity_panel %>% select(country) %>% distinct()
    products_panel <- complexity_panel %>% select(product) %>% distinct()
    names(products_panel) <- "country"

    countries <- sort(as.character(countries_panel[[1]]))
    products <- sort(as.character(products_panel[[1]]))

    n_countries <- length(countries)
    n_products <- length(products)

    country_keys <- as.factor(c(sapply(countries, function(x) {
        cbind(rep(x, times = n_products))
    })))

    product_keys <- rep(products, times = n_countries)

    keys_panel <- tbl_df(data.frame(country = country_keys, product = product_keys))
    values_panel <- keys_panel %>% left_join(complexity_panel)

    complexity_vals <- values_panel[["complexity"]]

    raw_m_matrix <- matrix(complexity_vals, nrow = n_countries, ncol = n_products,
                                    byrow = TRUE, dimnames = list(countries, products))
    raw_m_matrix[is.na(raw_m_matrix)] <- 0

    m_matrix <- raw_m_matrix[, which(!apply(raw_m_matrix, 2, FUN = function(x) { all(x == 0) }))]
    m_matrix <- raw_m_matrix[which(!apply(raw_m_matrix, 1, FUN = function(x) { all(x == 0) })), ]

    return(m_matrix)

}

## computeDiversity0
##
## Takes the complexity (M) matrix and computes the initial values of
## diversity, called k_{c,0} in the Atlas. For more details, see Atlas p.24,
## Technical Box 2.1.

computeDiversity0 <- function(M_matrix) {
    return(rowSums(M_matrix))
}

## computeUbiquity0
##
## Takes the complexity (M) matrix and computes the initial values of
## ubiquity, called k_{p,0} in the Atlas. For more details, see Atlas p.24,
## Technical Box 2.1.

computeUbiquity0 <- function(M_matrix) {
    return(colSums(M_matrix))
}

## computeMTilde
##
## Takes the complexity (M) matrix, and the initial values of diversity
## and ubiquity, and computes M-tilde_{pp'} as it would be called in the Atlas.
## This is the precursor to computing the product and economic complexity
## indices (PCIs/ECIs). For more details, see Atlas p.24, Technical Box 2.1.

computeMTilde <- function(M, diversity_0, ubiquity_0, flag) {

    k_c_0 <- diversity_0
    k_p_0 <- ubiquity_0

    dim_M <- dim(M)
    N_c <- dim_M[[1]]
    N_p <- dim_M[[2]]

    dimnames_M <- dimnames(M)
    countries <- dimnames_M[[1]]
    products <- dimnames_M[[2]]

    if (flag == "PCI") {

        K_p <- matrix(rep(k_p_0, N_c), nrow = N_p, ncol = N_c,
                      dimnames = list(products, countries))
        K_c <- matrix(rep(k_c_0, N_p), nrow = N_c, ncol = N_p,
                      dimnames = list(countries, products))

        M_a <- t(M) / K_p
        M_b <- M / K_c

    } else if (flag == "ECI") {

        K_p <- matrix(rep(k_p_0, N_c), nrow = N_p, ncol = N_c,
                      dimnames = list(products, countries))
        K_c <- matrix(rep(k_c_0, N_p), nrow = N_c, ncol = N_p,
                      dimnames = list(countries, products))

        M_a <- M / K_c
        M_b <- t(M) / K_p

    }

    M_tilde <- M_a %*% M_b

    return(M_tilde)

}

## computeRanks
##
## Takes an M_tilde (either by country or by product) and computes the
## appropriate ranking indicator (either ECI or PCI) by finding the
## eigenvectors of M_tilde, taking the one associated with the second
## eigenvalue, and normalizing its values. For more details, see Atlas
## p.24, Technical Box 2.1.

computeRanks <- function(M_tilde) {

    eigen_calcs <- eigen(M_tilde)

    raw_rank <- eigen_calcs[[2]][, 2]

    result <- (raw_rank - mean(raw_rank)) / sd(raw_rank)

    names(result) <- dimnames(M_tilde)[[1]]

    return(Re(result))

}

## computeProximity
##
## Takes complexity (M) and the initial values of ubiquity and computes a
## measure of the proximity (phi) between two products. Proximity is defined
## as the conditional probability that a country exports one product given
## that it already exports another. This measurement can be used to enable
## visualizations of the product space. See Atlas p.52, Technical Box 5.1.

computeProximity <- function(M, ubiquity_0) {

    N_p <- length(ubiquity_0)

    k_c_0_A <- ubiquity_0
    k_c_0_B <- ubiquity_0

    K_A <- matrix(rep(k_c_0_A, times = N_p), nrow = N_p, ncol = N_p)
    K_B <- matrix(rep(k_c_0_A, times = N_p), nrow = N_p, ncol = N_p, byrow = TRUE)

    numer <- t(M) %*% M
    denom <- pmax(K_A, K_B)

    phi <- numer / denom

    return(phi)

}

## computeDistance
##
## Takes complexity (M) and proximity (phi) and computes a distance measure
## between countries and products. If a country exports most of the goods
## related to a product, the distance between that country and that product
## is short (close to 0). Vice versa, if a country exports few of the goods
## related to a product, the distance between that country and that product
## is long (close to 1). See Atlas p.54, Technical Box 5.4.

computeDistance <- function(M, phi) {

    dim_M <- dim(M)
    N_c <- dim_M[[1]]
    N_p <- dim_M[[2]]

    numer <- (1 - M) %*% phi
    denom <- matrix(rep(colSums(phi), times = N_c), nrow = N_c, ncol = N_p, byrow = TRUE)

    d <- numer / denom

    return(d)

}

## computeOutlook
##
## Takes complexity (M), the PCI, and distance and computes a weighted measure
## of the total value of the products that a country is not exporting, weighted
## by how close they are to what the country is currently exporting. See Atlas
## p.54, Technical Box 5.4.

computeOutlook <- function(M, pci, distance) {

    d <- distance

    term <- (1 - d) * (1 - M)
    result <- term %*% pci

    return(result)

}

## computeOutlookGain
##
## Takes complexity (M), proximity (phi), the PCI, and distance, and computes
## the contribution of any one product in opening up opportunities for
## producing more advanced products in any given country. See Atlas p.54,
## Technical Box 5.4.

computeOutlookGain <- function(M, phi, pci, distance) {

    d <- distance

    dim_M <- dim(M)
    N_c <- dim_M[[1]]
    N_p <- dim_M[[2]]

    (1 - d) %*% pci

    phi_scaling_term <- matrix(
        rep(1 / colSums(phi), times = N_p),
        nrow = N_p,
        ncol = N_p,
        dimnames = dimnames(phi),
        byrow = TRUE)

    scaled_phi_term <- phi_scaling_term * phi

    result <- matrix(
        rep(0, times = N_c * N_p),
        nrow = N_c,
        ncol = N_p,
        dimnames = dimnames(M),
        byrow = TRUE)

    for (i in 1:N_c) {

        m_this_i <- M[i, ]

        m_term <- 1 - m_this_i

        m_term_by_phi <- t(scaled_phi_term) * m_term

        sum_term <- t(m_term_by_phi %*% pci)

        d_this_i <- d[i, ]

        minus_term <- d_this_i * pci

        gain <- sum_term - minus_term

        result[i, ] <- gain

    }

    return(result)

}

## runModel
##
## Runs the entire model program and returns all the results wrapped in
## a single object. Takes 5 arguments:
##
## * data_tag = "baci" or "oec"
## * product_code_rev = "1992" (oec), "1996" (baci, oec),
##                      "2002" (oec), "2007" (oec)
## * product_code_digit = "4" or "6"
## * ab_flag = TRUE (include AB data) or FALSE
##
## Note that for BACI data, the input_year is ignored when loading exports,
## but applied when determining product names from codes.

runModel <- function(data_tag, product_code_rev, product_code_digit, year, ab_flag, updateProgress = NULL) {

    updateMessage <- function(message_text) {

        print(message_text)

        if (is.function(updateProgress)) {
            updateProgress(detail = message_text)
        }

    }

    model <- list()

    message_text <- paste0("Loading dataset ... ", Sys.time())
    updateMessage(message_text)

    exports_panel <- loadExportsPanel(data_tag,
                                      product_code_rev,
                                      product_code_digit,
                                      year,
                                      ab_flag)

    message_text <- paste0("Computing RCA ... ", Sys.time())
    updateMessage(message_text)

    rca_panel <- computeRCA(exports_panel)

    message_text <- paste0("Computing complexity ... ", Sys.time())
    updateMessage(message_text)

    complexity_panel <- computeComplexity(rca_panel)

    message_text <- paste0("Computing M matrix ... ", Sys.time())
    updateMessage(message_text)

    m <- computeM(complexity_panel)

    message_text <- paste0("Computing M-tilde ... ", Sys.time())
    updateMessage(message_text)

    diversity_0 <- computeDiversity0(m)
    ubiquity_0 <- computeUbiquity0(m)
    m_tilde_eci <- computeMTilde(m, diversity_0, ubiquity_0, "ECI")
    m_tilde_pci <- computeMTilde(m, diversity_0, ubiquity_0, "PCI")

    message_text <- paste0("Computing ECI/PCI ... ", Sys.time())
    updateMessage(message_text)

    eci <- computeRanks(m_tilde_eci)
    pci <- computeRanks(m_tilde_pci)

    message_text <- paste0("Computing distance measures ... ", Sys.time())
    updateMessage(message_text)

    phi <- computeProximity(m, ubiquity_0)
    distance <- computeDistance(m, phi)

    message_text <- paste0("Computing opportunity measures ... ", Sys.time())
    updateMessage(message_text)

    outlook <- computeOutlook(m, pci, distance)
    outlook_gain <- computeOutlookGain(m, phi, pci, distance)

    message_text <- paste0("All done! ", Sys.time())
    updateMessage(message_text)

    model[["data_tag"]] <- data_tag
    model[["product_code_rev"]] <- product_code_rev
    model[["product_code_digit"]] <- product_code_digit
    model[["input_year"]] <- year
    model[["ab_flag"]] <- ab_flag

    model[["exports"]] <- exports_panel
    model[["rca"]] <- rca_panel
    model[["complexity"]] <- complexity_panel
    model[["m"]] <- m
    model[["eci"]] <- eci
    model[["pci"]] <- pci
    model[["proximity"]] <- phi
    model[["distance"]] <- distance
    model[["opportunity"]] <- outlook
    model[["opportunity_gain"]] <- outlook_gain

    return(model)

}
