#' Warns if any of the provided column names is missing from the data
#'
#' @param dt A data.frame
#' @param cols A vector of column names
#'
#' @return The vector of all columns present in
#' @export
#'
#' @examples check_cols(mpg, c('cyl','carb','not'))
check_cols <- function(dt, cols){
  if(!is.data.frame(dt)){
    stop('dt must be a data.frame')
  }
  present <- cols %in% colnames(dt)
  if(!all(present)){
    warning(paste(cols[!present], collapse = ', '), ' not present in the provided data.frame. Will be discarded.')
  }
  return(cols[present])
}



#' Change column classes with a named vector
#'
#' @param dt Table whose column types will be changed by reference
#' @param character A character vector specifiying which columns that will be coerced to character.
#' @param integer A character vector specifiying which columns that will be coerced to integer.
#' @param double A character vector specifiying which columns that will be coerced to double.
#' @param logical A character vector specifiying which columns that will be coerced to logical.
#' @param factor A character vector specifiying which columns that will be coerced to factor.
#'
#' @return Changes by reference the types of the specified columns
#' @export
#'
#' @examples
#' iris_changed <- set_classes(dt = idt, character = 'Species', integer = c('Sepal.Length', 'Sepal.Width'))
#' purrr::map(iris_changed, class)
set_classes <- function(dt,
                        character = NULL,
                        integer = NULL,
                        double = NULL,
                        logical = NULL,
                        factor = NULL){
  setDT(dt)

  col_names <- colnames(dt)

  # character
  if(!is.null(character)){
    character <- check_cols(dt, character)
    dt[, (character) := purrr::map(.SD, as.character), .SDcols = character][]
  }

  # integer
  if(!is.null(integer)){
    integer <- check_cols(dt, integer)
    dt[, (integer) := purrr::map(.SD, as.integer), .SDcols = integer][]
  }

  # double
  if(!is.null(double)){
    double <- check_cols(dt, double)
    dt[, (double) := purrr::map(.SD, as.double), .SDcols = double][]
  }

  # logical
  if(!is.null(logical)){
    logical <- check_cols(dt, logical)
    dt[, (logical) := purrr::map(.SD, as.logical), .SDcols = logical][]
  }

  # factor
  if(!is.null(factor)){
    factor <- check_cols(dt, factor)
    dt[, (factor) := purrr::map(.SD, as.factor), .SDcols = factor][]
  }
}
