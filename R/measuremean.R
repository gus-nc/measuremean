#' Multiple Measurement Morphometric Mean
#'
#' @param x a dataframe
#' @param orientation how the multiple measurements are stacked in the dataframe.
#' 1 if across rows, and 2 if across columns
#' @param voucher name of the column with specimen ID
#' @param uniqueid Column to be removed if 'orientation = 1', and a column
#' with an unique identifier is included
#'
#' @returns a dataframe with calculated means for each trait
#'
#' @examples
#'file_path <- system.file("extdata", "data_rows.csv", package = "measuremean")
#'data_rows <- read.csv(file_path, check.names = FALSE)
#'avgdf_rows <- measuremean(data_rows,
#'                          orientation = 1, voucher = "Voucher",
#'                          uniqueid = "UniqueID")
#'avgdf_rows
#'
#'
#'file_path <- system.file("extdata", "data_columns.csv", package = "measuremean")
#' ata_columns <- read.csv(file_path, check.names = FALSE)
#'avgdf_columns <- measuremean(data_columns, orientation = 2, voucher = "Vouchers")
#'
#' @export

install_if_needed <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}


measuremean <- function(x, orientation = 1, voucher, uniqueid = NULL) {
  install_if_needed("dplyr")
  if (missing(voucher)) {
    stop("Error: speciment voucher missing")
  }
  if (orientation == 1) {
    # by row (recommended)
    if (!is.null(uniqueid)) {
      x <- dplyr::select(x, !uniqueid) # Remove the Unique ID if it exists
    }
    numeric_columns <- names(x)[sapply(x, is.numeric)]
    result_data <- dplyr::group_by(x, .data[[voucher]]) |> # Voucher is the name of the column with specimens ID
      dplyr::summarise(across(numeric_columns, mean))
    result_data_plus <- merge(result_data, unique(x[!sapply(x, is.numeric)]),
                              by = voucher
    )
  }

  if (orientation == 2) {
    # By columns (check if apply)
    numeric_columns <- unique(names(x)[sapply(x, is.numeric)])
    colnames(x) <- make.unique(colnames(x))
    vouchernames <- x[[voucher]]

    result_data <- dplyr::tibble(!!voucher := vouchernames)
    for (n in numeric_columns) {
      measure <- rowMeans(x[, grepl(n, names(x))], na.rm = TRUE)
      result_data[[n]] <- measure
    }
    result_data_plus <- merge(result_data, x[!sapply(x, is.numeric)],
                              by = voucher
    )
  }
  return(result_data_plus)
}
