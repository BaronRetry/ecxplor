#' ecxplor: Economic Complexity Explorer
#'
#'
#' @importFrom stats sd setNames
#' @importFrom utils download.file read.csv write.csv
#' @import dplyr
#'
#' @docType package
#' @name ecxplor

NULL
## NULL

globalVariables(names = c("year",
                          "country",
                          "product",
                          "hs_rev_digit",
                          "hs_rev_year",
                          "export_val",
                          "rca",
                          "complexity",
                          "country_export_val",
                          "product_export_val",
                          "total_export_val",
                          "rca_numerator",
                          "rca_denominator",
                          "pci",
                          "distance",
                          "weight",
                          "from",
                          "to"),
                package = "ecxplor")

Sys.setenv(TZ = "GMT")
