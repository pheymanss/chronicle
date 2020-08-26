#' Add text to a chronicle Rmarkdown report
#'
#' @param report character containing the text of an Rmarkdown report header (and possibly more chunks). Easily create one with chronicle::new_report()
#' @param text The text that will be added to the report
#' @param text_title The title of the text section. Default is NULL.
#' @param title_level Level of the section title of this text (ie, number of # on Rmarkdown syntax.)
#'
#' @return The text of the Rmarkdown report plus an additional section with the text.
#' @export
#'
#' @examples
add_text <- function(report, text, text_title = NULL, title_level = 2){
  if(!is.null(text_title)){
    report <- add_title(report, text_title, title_level = title_level)
  }
  report <- paste(report, text, sep = '\n\n')
}

#' Add a titled section to a chronicle Rmarkdown report
#'
#' @param report character containing the text of an Rmarkdown report header (and possibly more chunks). Easily create one with chronicle::new_report()
#' @param title The title to be added as a section.
#' @param title_level Level of the section title (ie, number of # on Rmarkdown syntax.)
#'
#' @return The text of the Rmarkdown report plus an additional section with the text
#' @export
#'
#' @examples
add_title <- function(report, title, title_level = 1){
  report <- report %>% paste(paste(paste(rep('#', title_level), collapse = ''), title), sep = '\n\n')
  return(report)
}


#' #' Add formated code chunks to a chronicle Rmarkdown report
#'
#' Beware that code indentation of the chronicle call will affect the indentation of the chunk, so make sure not to leave unintended indentation in the 'code' parameter on this function call.
#'
#' @param report character containing the text of an Rmarkdown report header (and possibly more chunks). Easily create one with chronicle::new_report()
#' @param code Character string to be formated as an R code chunk. This code will not be excecuted when the report is rendered.
#' @param code_title The title of the code section. Default is NULL.
#' @param title_level Level of the section title (ie, number of # on Rmarkdown syntax.)
#'
#' @return The text of the Rmarkdown report plus an additional section with the code chunk.
#' @export
#'
#' @examples
add_code <- function(report, code, code_title = NULL, title_level = 2){
  if(!is.null(code_title)){
    report <- add_title(report, code_title, title_level = title_level)
  }
  report <- report %>% paste('```{r,eval = FALSE}',
                                   code,
                                   '```',
                             sep = '\n')
}
