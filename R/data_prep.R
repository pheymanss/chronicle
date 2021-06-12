#' Warns if any of the passed column names is missing from the data provided.
#'
#' @param dt A data.frame.
#' @param cols A vector of column names.
#'
#' @return The vector of all columns present in dt.
#' @export
#'
#' @examples
#' chronicle::check_cols(mtcars, c('cyl', 'made_up_column'))
#' @import data.table
#' @importFrom magrittr %>%
check_cols <- function(dt, cols){
  if(!is.data.frame(dt)){
    stop('dt must be a data.frame')
  }
  present <- cols %in% colnames(dt)
  if(!all(present)){
    warning(paste(cols[!present], collapse = ', '), ' not present in the provided data.frame')
  }
  return(cols[present])
}


#' Change column classes with a named vector
#'
#' @param dt Table whose column types will be changed
#' @param character The columns that will be coerced to character.
#' @param integer The columns that will be coerced to integer.
#' @param double The columns that will be coerced to double.
#' @param logical The columns that will be coerced to logical.
#' @param factor The columns that will be coerced to factor.
#'
#' @return Changes by reference the types of the specified columns
#' @export
#'
#' @examples
#' library(chronicle)
#' iris_changed <- chronicle::set_classes(dt = iris,
#'                                        character = 'Species',
#'                                        integer = c('Sepal.Length', 'Sepal.Width'))
#' purrr::map_chr(iris_changed, class)
set_classes <- function(dt,
                        character = NULL,
                        integer = NULL,
                        double = NULL,
                        logical = NULL,
                        factor = NULL){
  data.table::setDT(dt)

  # integer
  if(!is.null(integer)){
    integer <- chronicle::check_cols(dt, integer)
    dt[, (integer) := purrr::map(.SD, as.integer), .SDcols = integer][]
  }

  # character
  if(!is.null(character)){
    character <- chronicle::check_cols(dt, character)
    dt[, (character) := purrr::map(.SD, as.character), .SDcols = character][]
  }

  # double
  if(!is.null(double)){
    double <- chronicle::check_cols(dt, double)
    dt[, (double) := purrr::map(.SD, as.double), .SDcols = double][]
  }

  # logical
  if(!is.null(logical)){
    logical <- chronicle::check_cols(dt, logical)
    dt[, (logical) := purrr::map(.SD, as.logical), .SDcols = logical][]
  }

  # factor
  if(!is.null(factor)){
    factor <- chronicle::check_cols(dt, factor)
    dt[, (factor) := purrr::map(.SD, as.factor), .SDcols = factor][]
  }

  return(dt)
}

