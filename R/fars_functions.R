#' Reading data from a file
#'
#' This function checks whether a file exists or not and if the file
#' does exist, the data from the file is read and stored within the
#' data variable.
#'
#' @parameter filename A file from which the data has to be read
#'
#' @return This function stores the value of a data frame from the file
#'
#' @import file.exists()
#' @import stop()
#' @import read_csv() from the readr package
#' @import tbl_df() from the dplyr package
#'
#'
#' @examples
#' fars_read('accident.csv')
#'
#  @export

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Changing the filename according to the accident year
#'
#' This function adds up the year of the accident to
#' the filename and prints it.
#'
#'
#' @parameter year An integer which specifies the year of accident
#'
#' @return This function returns the value of the filename
#'
#' @import as.integer()
#' @import sprintf()
#'
#' @note Passing any other data type value can cause an error
#'
#'
#' @examples
#' make_filename(2015)
#' make_filename(2013)
#'
#' @export
#'


make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}
#' Selecting month and year from given file and operating on given input
#'
#' This function performs a select operation on the set of data and returns the required
#' years
#'
#'
#' @parameter years A numeric value on which the operation is performed
#'
#' @return This function returns a vector of the selected years
#'
#' @import make_filename()
#' @import tryCatch()
#' @import warning()
#' @import lappy()
#'
#'
#' @examples
#' fars_read_years(2015)
#'
#' @export
#'
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}
#' Summarizing data according to the years
#'
#' This function stores the vector of years which is returned from the fars_read_years
#' function and summarize the given data according to the years and months
#'
#'
#' @parameter years A numeric value which has to be operated on
#'
#' @return This function returns the summary of the data grouping the months and the years
#'
#' @import fars_read_years()
#' @import bind_rows()
#' @import spread() from the tidyr package
#' @import group_by() from the dplyr package
#' @import summary() from the dplyr package
#'
#'
#' @examples
#' fars_summarize_years(2014)
#'
#' @export
#'
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Mapping the state
#'
#' This function maps the input state identifying them
#' by the state number and year
#'
#' @parameter year A numeric value of the accident year
#' @parameter state.num The input of state number
#'
#' @return This function returns the map of the input state
#'
#' @import make_filename()
#' @import fars_read()
#' @import as.integer()
#' @import tbl_df()
#'
#'
#' @examples
#' fars_map_state(1 , 2015)
#'
#' @export
#'


fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
