## This line needs to be changed to the location of the export data.
master_data_path <- "/Users/smyrna/Data/"
## master_data_path <- "C:/Users/smyrna/Data/"

## Folder structure is as required below.
goa_data_path <- paste0(master_data_path, "GOA/")
oec_data_path <- paste0(master_data_path, "Observatory of Economic Complexity/")
baci_data_path <- paste0(master_data_path, "BACI/")

oec_data_filenames <- c("year_origin_hs92_4.tsv.bz2" = FALSE,
                        "year_origin_hs92_6.tsv.bz2" = FALSE,
                        "year_origin_hs96_4.tsv.bz2" = FALSE,
                        "year_origin_hs96_6.tsv.bz2" = FALSE,
                        "year_origin_hs02_4.tsv.bz2" = FALSE,
                        "year_origin_hs02_6.tsv.bz2" = FALSE,
                        "year_origin_hs07_4.tsv.bz2" = FALSE,
                        "year_origin_hs07_6.tsv.bz2" = FALSE)

oec_info_filenames <- c("products_hs_92.tsv.bz2" = FALSE,
                        "products_hs_96.tsv.bz2" = FALSE,
                        "products_hs_02.tsv.bz2" = FALSE,
                        "products_hs_07.tsv.bz2" = FALSE)

oec_base_url <- "https://atlas.media.mit.edu/"

checkDataDirectory <- function() {

    data_filenames <- dir(path.package("ecxplor", "data"))

    oec_data_filenames[oec_data_filenames %in% data_filenames] <- TRUE
    oec_info_filenames[oec_info_filenames %in% data_filenames] <- TRUE

}


downloadOECData <- function(hs_rev_year, hs_rev_digit) {

    hs_rev_code <- substr(hs_rev_year, 3, 4)

    oec_data_filename <- paste0("year_origin_hs,"
                                hs_rev_code,
                                "_", hs_rev_digits,
                                ".tsv.bz2")

    oec_data_url <- paste0(oec_base_address,
                           "/static/db/raw/",
                           oec_data_filename)

    download.file(oec_data_url, file.path(path.package("ecxplor", "data"),
                                          oec_data_filename))

    oec_data_filenames[oec_data_filenames == oec_data_filename] <- TRUE

}

downloadOECInfo <- function(hs_rev_year, hs_rev_digit) {

    oec_base_url <- "https://atlas.media.mit.edu/"

    hs_rev_code <- substr(hs_rev_year, 3, 4)

    oec_info_filename <- paste0("products_hs,"
                                hs_rev_code,
                                "_", hs_rev_digits,
                                ".tsv.bz2")

    oec_info_url <- paste0(oec_base_address,
                           "/static/db/raw/",
                           oec_info_filename)

    download.file(oec_data_url, file.path(path.package("ecxplor",
                                                       "data",
                                                       oec_info_filename)))

}

loadOECInfoPanel <- function(hs_rev_year, hs_rev_digit) {

    hs_rev_tag <- substr(hs_rev_year, 3, 4)
    hs_code_colname <- paste0("hs", hs_rev_tag)

    product_file_path <- paste0(oec_data_path, "products_hs_", hs_rev_tag, ".tsv")

    print(product_file_path)

    product_panel <- tbl_df(read.csv(product_file_path,
                                     header = TRUE,
                                     sep = "\t",
                                     quote = "\"",
                                     colClasses = c("character", "character", "character"),
                                     stringsAsFactors = FALSE,
                                     na.string = c("NULL")))

    new_data_names <- gsub(hs_code_colname, "product", names(product_panel))
    names(product_panel) <- new_data_names

    digit_products <- c()

    if (hs_rev_digit == "4") {
        digit_products <- product_panel[["product"]][nchar(product_panel[["product"]]) == 4]
    } else if (hs_rev_digit == "6") {
        digit_products <- product_panel[["product"]][nchar(product_panel[["product"]]) == 6]
    }

    nice_product_panel <- product_panel[product_panel["product"] == digit_products, ]

    product_panels[[hs_rev_year]] <- product_panel

    return(product_panels)

}

loadObservatoryTradePanels <- function(hs_rev_years, hs_digits, trade_tag) {

    trade_panels <- list()

    for (hs_rev_year in hs_rev_years) {

        hs_rev_group <- list()

        hs_rev_tag <- substr(hs_rev_year, 3, 4)
        hs_code_colname <- paste0("hs", hs_rev_tag)

        for (hs_digit in hs_digits) {

            hs_rev_digit_group <- list()

            if (trade_tag == "export") {

                data_file_prefix = "year_origin_hs"
                col_classes <- c("integer", "factor", "factor", "numeric", "numeric", "numeric", "numeric")
                col_names <- c("year", "origin", "product", "export_val", "import_val", "export_rca", "import_rca")

            } else {

                data_file_prefix = "year_origin_destination_hs"
                col_classes <- c("integer", "factor", "factor", "character", "character", "character")
                col_names <- c("year", "origin", "dest", "product", "export_val", "import_val")


            }

            data_file_path <- paste0(oec_data_path, data_file_prefix, hs_rev_tag, "_", hs_digit, ".tsv")

            print(data_file_path)

            data_panel <- tbl_df(read.csv(data_file_path,
                                          header = TRUE,
                                          sep = "\t",
                                          colClasses = col_classes,
                                          stringsAsFactors = FALSE,
                                          na.strings = c("NULL")))

            new_data_names <- gsub(hs_code_colname, "product", names(data_panel))
            new_data_names <- gsub("origin", "country", new_data_names)
            names(data_panel) <- new_data_names

            hs_rev_group[[hs_digit]] <- data_panel

        }

        trade_panels[[hs_rev_year]] <- hs_rev_group


    }

    return(trade_panels)

}


loadBACICountryInfoPanel <- function() {

    col_classes <- c("character", "character", "character", "character")
    col_names <- c("name", "iso2", "country", "country_id")


    country_file_path <- paste0(baci_data_path, "country_code_baci12.csv")
    country_info_panel <- tbl_df(read.csv(country_file_path,
                                          header = TRUE,
                                          sep = ",",
                                          colClasses = col_classes,
                                          stringsAsFactors = FALSE,
                                          na.strings = c("NULL")))

    country_info_panel[["iso2"]] <- tolower(country_info_panel[["iso2"]])
    country_info_panel[["iso3"]] <- tolower(country_info_panel[["iso3"]])

    names(country_info_panel) <- col_names

    country_info_panel[["country_id"]] <- sprintf("%03d", as.integer(country_info_panel[["country_id"]]))

    return(country_info_panel)


}

## loadBACITradePanel
##
## Load BACI exports for the years 2012, 2013, and 2014.

loadBACITradePanel <- function() {

    col_classes <- c("integer", "character", "character", "character",  "numeric", "numeric")
    col_names <- c("year", "product", "country_id", "destination_id", "export_val", "export_quantity")

    raw_panel <- c()

    for (year_tag in c("2012", "2013", "2014")) {

        data_file_path <- paste0(baci_data_path, "baci12_", year_tag, ".csv")

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

    country_info_panel <- loadBACICountryInfoPanel()
    merged_panel <- tbl_df(merge(raw_panel, country_info_panel))
    unscaled_panel <- tbl_df(merged_panel[c("year", "country", "product", "export_val")])
    scaled_panel <- unscaled_panel %>% mutate(export_val = 1000 * export_val)


    sum_panel <- scaled_panel %>% arrange(year, country, product) %>%
        group_by(year, country, product) %>%
        summarize(export_val = sum(export_val))
    baci_panel <- sum_panel %>% filter(!is.na(country))

    return(tbl_df(baci_panel))

}

## loadAlbertaTradePanel
##
## Load all available Alberta trade data.

loadAlbertaTradePanel <- function() {

    data_file_path <- paste0(goa_data_path, "abExports.csv")

    print(data_file_path)

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

## loadExportsPanel
##
## Assemble the appropriate panel of exports based on the input arguments:
##
## * name = "oec" (Observatory) or "baci" (BACI)
## * hs_rev_year = "1992" (oec), "1996" (baci, oec),
##                      "2002" (oec), "2007" (oec)
## * hs_digit = "4" or "6"
## * input_year = the year for which complexity will be computed (ignored for
##                BACI data!!!)
## * ab_flag = TRUE (include AB data) or FALSE

loadExportsPanel <- function(name, hs_rev_year, hs_digit, input_year, ab_flag) {

    ## The functions below (inefficiently) load all data of a certain type.

    if (name == "oec") {

        raw_exports_panel <- loadObservatoryTradePanels(hs_rev_year,
                                                        hs_digit,
                                                        "export")[[1]][[1]]

    } else if (name == "baci") {

        ## Note that if we choose BACI data we ignore hs_rev_year here.
        raw_exports_panel <- loadBACITradePanel()

    }

    if (ab_flag == TRUE) {

        ab_panel <- loadAlbertaTradePanel()
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
