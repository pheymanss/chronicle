#' Add text to a chronicle Rmarkdown report
#'
#' @param report Character string containing all the R Markdown chunks previously added. Default is '', an empty report.
#' @param text The text that will be added to the report.
#' @param text_title The title of the text section. Default is NULL.
#' @param title_level Level of the section title of this text (ie, number of # on Rmarkdown syntax.) Default is 1.
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
    report <- chronicle::add_title(report = report,
                                   title =  text_title,
                                   title_level = title_level)
  }
  report <- glue::glue('{report}
{paste(text, collapse = "\n\n")}
')

  return(report)
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
  report <- glue::glue('{report}

{chronicle::rmd_title_level(title_level)} {title}

')

  return(report)
}


#' Add formatted code chunks to a chronicle R Markdown report
#'
#' Beware that code indentation of the chronicle call will affect the indentation of the chunk, so make sure not to leave unintended indentation in the 'code' parameter on this function call.
#'
#' @param report Character string containing all the R Markdown chunks previously added. Default is '', an empty report.
#' @param code The code that will be added to the report. Mind the indentation on the call, since spaces between quotations will be preserved.
#' @param code_title The title of the text section. Default is NULL.
#' @param title_level Level of the section title of this text (ie, number of # on Rmarkdown syntax.)
#' @param eval Run the code instead of just display it. Default is TRUE.
#' @param echo Whether to display the source code in the output document. Default is FALSE.
#' @param message Whether to preserve messages on rendering. Default is FALSE.
#' @param warning Whether to preserve warnings on rendering. Default is FALSE.
#' @param fig_width Width of the figures printed from this code.
#' @param fig_height Height of the figures printed from this code.
#'
#' @return The text of the Rmarkdown report plus an additional section with the code chunk.
#' @export
#'
#' @examples
#' html_report <- add_code(report = '',
#'                         code_title = 'Code comes after this title',
#'                         code = 'f <- function(x, y){paste(x,y)},
#' f("a", "b")',
#'                         eval = FALSE,
#'                         echo = TRUE,
#'                         fig_width = 12,
#'                         fig_height = 8)
#' cat(html_report)
add_code <- function(report = '',
                     code,
                     code_title = NULL,
                     title_level = 2,
                     eval = TRUE,
                     echo = TRUE,
                     message = FALSE,
                     warning = FALSE,
                     fig_width = NULL,
                     fig_height = NULL){
  if(!is.null(code_title)){
    report <- chronicle::add_title(report = report,
                                  title = code_title,
                                  title_level = title_level)
  }
  open_chunk <- paste0('```{r, eval=', eval,
                       ', warning=', warning,
                       ', message=', message,
                       ', echo=', echo,
                       if(!is.null(fig_width)){paste(', fig.width=', fig_width)}else{', fig.width=params$figure_width'},
                       if(!is.null(fig_width)){paste(', fig.height=', fig_height)}else{', fig.height=params$figure_height'},
                       '}')


  report <- glue::glue('{report}

                        {open_chunk}
                        {code}
                        ```')
  return(report)
}

