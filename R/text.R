#' Add text to a chronicle Rmarkdown report
#'
#' @param report Character string containing all the R Markdown chunks previously added. Default is '', an empty report.
#' @param text The text that will be added to the report
#' @param text_title The title of the text section. Default is NULL.
#' @param title_level Level of the section title of this text (ie, number of # on Rmarkdown syntax.)
#'
#' @return The text of the Rmarkdown report plus an additional section with the text.
#' @export
#'
#' @examples
#' html_report <- add_text(text = 'This is the text that will be seen outside of any chunk',
#'                         text_title = 'Text title')
#' cat(html_report)
add_text <- function(report = '', text, text_title = NULL, title_level = 2){
  if(!is.null(text_title)){
    report <- add_title(report, text_title, title_level = title_level)
  }
  report <- paste(report, text, sep = '\n\n')
}

#' Add a titled section to a chronicle Rmarkdown report
#'
#' @param report Character string containing all the R Markdown chunks previously added. Default is '', an empty report.
#' @param title The title to be added as a section.
#' @param title_level Level of the section title (ie, number of # on Rmarkdown syntax.)
#'
#' @return The text of the Rmarkdown report plus an additional section by the given title.
#' @export
#'
#' @examples
#' html_report <- add_title(report = '',
#'                          title = 'Just the title here')
#' cat(html_report)
add_title <- function(report = '', title, title_level = 1){
  report <- report %>% paste(paste(paste(rep('#', title_level), collapse = ''), title), sep = '\n\n')
  return(report)
}


#' #' Add formatted code chunks to a chronicle Rmarkdown report
#'
#' Beware that code indentation of the chronicle call will affect the indentation of the chunk, so make sure not to leave unintended indentation in the 'code' parameter on this function call.
#'
#' @param report Character string containing all the R Markdown chunks previously added. Default is '', an empty report.
#' @param code Character string to be formated as an R code chunk. This code will not be excecuted when the report is rendered.
#' @param code_title The title of the code section. Default is NULL.
#' @param title_level Level of the section title (ie, number of # on Rmarkdown syntax.)
#' @param eval Execute the code sent to the chunk. Default is FALSE.
#'
#' @return The text of the Rmarkdown report plus an additional section with the code chunk.
#' @export
#'
#' @examples
#' html_report <- add_code(report = '',
#'                         code_title = 'Code comes after this title',
#'                         code = 'f <- function(x, y){paste(x,y)}
#' f("a", "b")')
# 'cat(html_report)
add_code <- function(report = '', code, code_title = NULL, title_level = 2, eval = FALSE){
  if(!is.null(code_title)){
    report <- add_title(report, code_title, title_level = title_level)
  }
  report <- report %>% paste(paste0('```{r,eval = ', eval, '}'),
                                   code,
                                   '```',
                             sep = '\n\n')
}
