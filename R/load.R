checkHSRevisionYearAndDigits <- function(hs_rev_year, hs_digits) {

    if (!(hs_rev_year %in% c("1992", "1996", "2002", "2007"))) {
        print("error: no such HS revision year")
        return(FALSE)
    }

    if (!(hs_digits %in% c("4", "6"))) {
        print("error: no such HS coding length")
        return(FALSE)
    }

    return(TRUE)

}

downloadOECExports <- function(hs_rev_year, hs_digits) {

    if (!(checkHSRevisionYearAndDigits(hs_rev_year, hs_digits))) {
        return(NULL)
    }

    oec_base_url <- "https://atlas.media.mit.edu/"

    hs_rev_code <- substr(hs_rev_year, 3, 4)

    oec_data_filename <- paste0("year_origin_hs",
                                hs_rev_code,
                                "_", hs_digits,
                                ".tsv.bz2")

    oec_data_url <- paste0(oec_base_url,
                           "/static/db/raw/",
                           oec_data_filename)

    download.file(url = oec_data_url, destfile = file.path(path.package("ecxplor"),
                                                           "data",
                                                           oec_data_filename))

}

downloadOECProductsInfo <- function(hs_rev_year, hs_digits) {

    if (!(checkHSRevisionYearAndDigits(hs_rev_year, hs_digits))) {
        return(NULL)
    }

    oec_base_url <- "https://atlas.media.mit.edu/"

    hs_rev_code <- substr(hs_rev_year, 3, 4)

    oec_info_filename <- paste0("products_hs_",
                                hs_rev_code,
                                ".tsv.bz2")

    oec_info_url <- paste0(oec_base_url, "static/db/raw/", oec_info_filename)

    download.file(oec_info_url, file.path(path.package("ecxplor"),
                                          "data",
                                          oec_info_filename))

}

downloadOECCountriesInfo <- function() {

    oec_base_url <- "https://atlas.media.mit.edu/"

    oec_countries_filename <- "country_names.tsv.bz2"
    oec_countries_url <- paste0(oec_base_url, "static/db/raw/", oec_countries_filename)
    download.file(oec_countries_url, file.path(path.package("ecxplor"),
                                               "data",
                                               oec_countries_filename))

}



downloadBACIExports <- function(rev_year) {

    baci_base_url <- "http://talentedco.in/Data/BACI/"

    baci_data_filename <- paste0("baci12_",
                                 rev_year,
                                 ".csv")

    baci_data_url <- paste0(baci_base_url,
                            baci_data_filename)

    download.file(url = baci_data_url, destfile = file.path(path.package("ecxplor"),
                                                            "data",
                                                            baci_data_filename))

}

downloadBACIProductsInfo <- function(hs_rev_year, hs_digits) {


}

downloadBACICountriesInfo <- function() {

    baci_base_url <- "http://talentedco.in/Data/BACI/"

    baci_countries_filename <- "country_code_baci12.csv"
    baci_countries_url <- paste0(baci_base_url, baci_countries_filename)
    download.file(baci_countries_url, file.path(path.package("ecxplor"),
                                                "data",
                                                baci_countries_filename))

}

loadOECProductsInfoPanel <- function(hs_rev_year, hs_digits) {

    if (!(checkHSRevisionYearAndDigits(hs_rev_year, hs_digits))) {
        return(NULL)
    }

    hs_rev_tag <- substr(hs_rev_year, 3, 4)
    hs_code_colname <- paste0("hs", hs_rev_tag)

    products_file_name <- paste0("products_hs_",
                                hs_rev_tag,
                                ".tsv.bz2")

    products_file_path <- file.path(path.package("ecxplor"), "data", products_file_name)

    if (!(file.exists(products_file_path))) {
        downloadOECProductsInfo(hs_rev_year, hs_digits)
    }

    products_panel <- tbl_df(read.csv(products_file_path,
                                      header = TRUE,
                                      sep = "\t",
                                      quote = "\"",
                                      colClasses = c("character", "character", "character"),
                                      stringsAsFactors = FALSE,
                                      na.string = c("NULL")))

    new_data_names <- gsub(hs_code_colname, "product", names(products_panel))
    names(products_panel) <- new_data_names

    digit_products <- c()

    if (hs_digits == "4") {
        digit_products <- products_panel[["product"]][nchar(products_panel[["product"]]) == 4]
    } else if (hs_digits == "6") {
        digit_products <- products_panel[["product"]][nchar(products_panel[["product"]]) == 6]
    }

    nice_products_panel <- products_panel[products_panel["product"] == digit_products, ]

    return(nice_products_panel)

}

loadOECCountriesInfoPanel <- function() {

    countries_info_filename <- "country_names.tsv.bz2"

    countries_file_path <- file.path(path.package("ecxplor"), "data", countries_info_filename)

    if (!(file.exists(countries_file_path))) {
        downloadOECCountriesInfo()
    }

    countries_panel <- tbl_df(read.csv(countries_file_path,
                                       header = TRUE,
                                       sep = "\t",
                                       quote = "\"",
                                       colClasses = c("character", "character", "character"),
                                       stringsAsFactors = FALSE,
                                       na.string = c("NULL")))

    names(countries_panel) <- c("country_id", "country", "country_name")

    return(countries_panel)

}

loadOECExportsPanel <- function(hs_rev_year, hs_digits) {

    if (!(checkHSRevisionYearAndDigits(hs_rev_year, hs_digits))) {
        return(NULL)
    }

    hs_rev_tag <- substr(hs_rev_year, 3, 4)
    hs_code_colname <- paste0("hs", hs_rev_tag)

    data_file_prefix = "year_origin_hs"
    col_classes <- c("integer", "factor", "factor", "numeric", "numeric", "numeric", "numeric")
    col_names <- c("year", "origin", "product", "export_val", "import_val", "export_rca", "import_rca")

    oec_exports_filename <- paste0(data_file_prefix, hs_rev_tag, "_", hs_digits, ".tsv.bz2")
    exports_file_path <- file.path(path.package("ecxplor"), "data", oec_exports_filename)

    if (!(file.exists(exports_file_path))) {
        downloadOECExports(hs_rev_year, hs_digits)
    }

    exports_panel <- tbl_df(read.csv(exports_file_path,
                                     header = TRUE,
                                     sep = "\t",
                                     colClasses = col_classes,
                                     stringsAsFactors = FALSE,
                                     na.strings = c("NULL")))

    new_exports_names <- gsub(hs_code_colname, "product", names(exports_panel))
    new_exports_names <- gsub("origin", "country", new_exports_names)
    names(exports_panel) <- new_exports_names

    return(exports_panel)

}


loadBACICountriesInfoPanel <- function() {

    col_classes <- c("character", "character", "character", "character")
    col_names <- c("name", "iso2", "country", "country_id")

    countries_file_path <- file.path(path.package("ecxplor"), "data", "country_code_baci12.csv")

    if (!(file.exists(countries_file_path))) {
        downloadBACICountriesInfo()
    }

    countries_info_panel <- tbl_df(read.csv(countries_file_path,
                                            header = TRUE,
                                            sep = ",",
                                            colClasses = col_classes,
                                            stringsAsFactors = FALSE,
                                            na.strings = c("NULL")))

    countries_info_panel[["iso2"]] <- tolower(countries_info_panel[["iso2"]])
    countries_info_panel[["iso3"]] <- tolower(countries_info_panel[["iso3"]])

    names(countries_info_panel) <- col_names

    countries_info_panel[["country_id"]] <- sprintf("%03d", as.integer(countries_info_panel[["country_id"]]))

    return(countries_info_panel)

}

loadBACIExportsPanel <- function() {

    col_classes <- c("integer", "character", "character", "character",  "numeric", "numeric")
    col_names <- c("year", "product", "country_id", "destination_id", "export_val", "export_quantity")

    raw_panel <- c()

    for (year_tag in c("2012", "2013", "2014")) {

        data_file_name <- paste0("baci12_", year_tag, ".csv")
        data_file_path <- file.path(path.package("ecxplor"), "data", data_file_name)

        if (!(file.exists(data_file_path))) {
            downloadBACIExports(year_tag)
        }

        if (is.null(raw_panel)) {

            raw_panel <- tbl_df(read.csv(data_file_path,
                                         header = TRUE,
                                         sep = ",",
                                         colClasses = col_classes,
                                         stringsAsFactors = FALSE,
                                         na.strings = c("NULL")))


        } else {

            data_panel <- tbl_df(read.csv(data_file_path,
                                          header = TRUE,
                                          sep = ",",
                                          colClasses = col_classes,
                                          stringsAsFactors = FALSE,
                                          na.strings = c("NULL")))

            raw_panel <- rbind(raw_panel, data_panel)

        }

    }

    names(raw_panel) <- col_names

    country_info_panel <- loadBACICountriesInfoPanel()
    merged_panel <- tbl_df(merge(raw_panel, country_info_panel))
    unscaled_panel <- tbl_df(merged_panel[c("year", "country", "product", "export_val")])
    scaled_panel <- unscaled_panel %>% mutate(export_val = 1000 * export_val)


    sum_panel <- scaled_panel %>% arrange(year, country, product) %>%
        group_by(year, country, product) %>%
        summarize(export_val = sum(export_val))
    baci_panel <- sum_panel %>% filter(!is.na(country))

    return(tbl_df(baci_panel))

}

downloadAlbertaExports <- function() {

    goa_base_url <- "http://talentedco.in/Data/GOA/"
    goa_exports_filename <- "abExports.csv"

    goa_exports_url <- paste0(goa_base_url, goa_exports_filename)
    download.file(goa_exports_url, file.path(path.package("ecxplor"),
                                                "data",
                                                goa_exports_filename))

}

loadAlbertaExportsPanel <- function() {

    data_file_path <- file.path(path.package("ecxplor"), "data", "abExports.csv")

    if (!(file.exists(data_file_path))) {
        downloadAlbertaExports()
    }

    col_classes <- c("integer", "character", "character", "numeric")
    col_names <- c("year", "country", "product", "export_val")

    data_panel <- tbl_df(read.csv(data_file_path,
                                  header = TRUE,
                                  sep = ",",
                                  colClasses = col_classes,
                                  stringsAsFactors = FALSE,
                                  na.strings = c("NULL")))

    names(data_panel) <- col_names

    return(data_panel)

}

loadExportsPanel <- function(name, hs_rev_year, hs_digit, input_year, ab_flag) {

    ## The functions below (inefficiently) load all data of a certain type.

    if (name == "oec") {
        raw_exports_panel <- loadOECExportsPanel(hs_rev_year, hs_digit)
    } else if (name == "baci") {
        ## Note that if we choose BACI data we ignore hs_rev_year here.
        raw_exports_panel <- loadBACIExportsPanel()
    }

    if (ab_flag == TRUE) {
        ab_panel <- loadAlbertaExportsPanel()
        target_panel <- rbind(raw_exports_panel, ab_panel)
    } else {
        target_panel <- raw_exports_panel
    }

    ## Only here do we use input_year to determine min_year and apply the filter.

    min_year = input_year - 2

    exports_panel <- target_panel %>% select(year, country, product, export_val) %>%
        filter(!is.na(export_val)) %>%
        filter(year >= min_year & year <= input_year)

    return(exports_panel)

}
