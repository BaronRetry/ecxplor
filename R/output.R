#' Generate Basic Output
#'
#' \code{generateBasicOutput} uses the model input to create some standardized
#' csv output. Output is dumped into the current working directory for now.
#'
#' @param model Output from \code{\link{runModel}}.
#' @export

generateBasicOutput <- function(model) {

    data_tag <- model[["data_tag"]]
    product_code_rev <- model[["product_code_rev"]]
    product_code_digit <- model[["product_code_digit"]]
    input_year <- model[["input_year"]]
    countries_info_panel <- model[["countries_info"]]
    products_info_panel <- model[["products_info"]]
    exports_panel <- model[["exports"]]

    lag0_year <- input_year
    lag1_year <- input_year - 1
    lag2_year <- input_year - 2

    exports_lag0_panel <- exports_panel %>%
        filter(year == lag0_year) %>%
        rename_(.dots = setNames(list("export_val"), paste0("export_val_", lag0_year))) %>%
        select(-one_of("year"))

    exports_lag1_panel <- exports_panel %>%
        filter(year == lag1_year) %>%
        rename_(.dots = setNames(list("export_val"), paste0("export_val_", lag1_year))) %>%
        select(-one_of("year"))

    exports_lag2_panel <- exports_panel %>%
        filter(year == lag2_year) %>%
        rename_(.dots = setNames(list("export_val"), paste0("export_val_", lag2_year))) %>%
        select(-one_of("year"))

    exports_all_panel <- exports_panel %>% group_by(country, product) %>%
        summarize(export_val_all = sum(export_val))

    dimnames_m <- dimnames(model[["m"]])
    country_codes <- dimnames_m[[1]]
    product_codes <- dimnames_m[[2]]

    country_codes_panel <- tbl_df(data.frame(country = country_codes,
                                             stringsAsFactors = FALSE))

    product_codes_panel <- tbl_df(data.frame(product = product_codes,
                                             stringsAsFactors = FALSE))

    nice_panel <- merge(exports_all_panel, countries_info_panel, by = c("country"), all.x = TRUE)
    nice_panel <- merge(nice_panel, products_info_panel, by = c("product"), all.x = TRUE)
    nice_panel <- merge(nice_panel, exports_lag2_panel, by = c("country", "product"), all.x = TRUE)
    nice_panel <- merge(nice_panel, exports_lag1_panel, by = c("country", "product"), all.x = TRUE)
    nice_panel <- merge(nice_panel, exports_lag0_panel, by = c("country", "product"), all.x = TRUE)

    nice_panel[is.na(nice_panel)] <- 0

    distance <- model[["distance"]]

    n_countries <- length(country_codes)
    n_products <- length(product_codes)

    distance_panel <- tbl_df(data.frame(country = rep(row.names(distance), ncol(distance)),
                                        product = rep(colnames(distance), each = nrow(distance)),
                                        distance_val = as.vector(distance),
                                        stringsAsFactors = FALSE))

    gain <- model[["opportunity_gain"]]

    gain_panel <- tbl_df(data.frame(country = rep(row.names(gain), ncol(gain)),
                                        product = rep(colnames(gain), each = nrow(gain)),
                                        opportunity_gain_val = as.vector(gain),
                                        stringsAsFactors = FALSE))


    nice_panel <- tbl_df(merge(nice_panel, gain_panel, by = c("country", "product")))

    nice_panel_names <- names(nice_panel)
    sum_names <- nice_panel_names[grepl("export_val", nice_panel_names)]

    country_total_panel <- nice_panel %>% group_by(country) %>%
        summarize_each_(funs(sum), sum_names)

    product_total_panel <- nice_panel %>% group_by(product) %>%
        summarize_each_(funs(sum), sum_names)


    eci <- model[["eci"]]
    eci_panel <- tbl_df(data.frame(country = names(eci),
                                   eci = eci,
                                   stringsAsFactors = FALSE))

    opportunity <- model[["opportunity"]]

    opportunity_panel <- tbl_df(data.frame(country = row.names(opportunity),
                                           opportunity_val = as.vector(opportunity),
                                           stringsAsFactors = FALSE))


    nice_country_panel <- tbl_df(merge(eci_panel, country_total_panel, all.x = TRUE))
    nice_country_panel <- tbl_df(merge(nice_country_panel, countries_info_panel, all.x = TRUE))
    nice_country_panel <- tbl_df(merge(nice_country_panel, opportunity_panel, all.x = TRUE))


    pci <- model[["pci"]]
    pci_panel <- tbl_df(data.frame(product = names(pci),
                                   pci = pci,
                                   stringsAsFactors = FALSE))

    ab_exports_panel <- exports_panel %>% filter(country == "zalta")

    ab_exports_lag0_panel <- ab_exports_panel %>% filter(year == lag0_year) %>%
        rename_(.dots = setNames(list("export_val"), paste0("ab_export_val_", lag0_year))) %>%
        select(-one_of("year", "country"))

    ab_exports_lag1_panel <- ab_exports_panel %>% filter(year == lag1_year) %>%
        rename_(.dots = setNames(list("export_val"), paste0("ab_export_val_", lag1_year))) %>%
        select(-one_of("year", "country"))

    ab_exports_lag2_panel <- ab_exports_panel %>% filter(year == lag2_year) %>%
        rename_(.dots = setNames(list("export_val"), paste0("ab_export_val_", lag2_year))) %>%
        select(-one_of("year", "country"))

    ab_gain_panel <- gain_panel %>% filter(country == "zalta") %>%
        select(-one_of("country"))

    nice_product_panel <- tbl_df(merge(product_total_panel, products_info_panel, all.x = TRUE))
    nice_product_panel <- tbl_df(merge(nice_product_panel, pci_panel, all.x = TRUE))
    nice_product_panel <- tbl_df(merge(nice_product_panel, ab_exports_lag2_panel, all.x = TRUE))
    nice_product_panel <- tbl_df(merge(nice_product_panel, ab_exports_lag1_panel, all.x = TRUE))
    nice_product_panel <- tbl_df(merge(nice_product_panel, ab_exports_lag0_panel, all.x = TRUE))
    nice_product_panel <- tbl_df(merge(nice_product_panel, ab_gain_panel, all.x = TRUE))

    model_tag <- paste0(data_tag, "-", product_code_rev, "-", product_code_digit, ", ", input_year)

    write.csv(nice_country_panel, file = paste0(model_tag, " countries.csv"))
    write.csv(nice_product_panel, file = paste0(model_tag, " products.csv"))

    write.csv(nice_panel, file = paste0(model_tag, " output.csv"))

}
