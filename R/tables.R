#' Add a table to a chronicle report
#'
#' @param report Character string containing all the R Markdown chunks previously added. Default is '', an empty report.
#' @param table data.frame to print on the report.
#' @param table_title title of the table. Default is no title.
#' @param title_level Level of the section title of this plot (ie, number of # on Rmarkdown syntax.)
#' @param html_table_type Either print a knitr::kable table or a DT htmlwidget.
#' @param table_params Single character string with any additional parameters to be passed to either knitr::kable() or DT::datatable(), depending on html_table_type
#'
#' @return An R Markdown file as a character string, now containing a chunk for the specified table.
#'
#' @export
#'
#' @examples
#' html_report <- add_table(table = iris,
#'                          table_title = 'Iris measures',
#'                          html_table_type = 'kable')
#' cat(html_report)
add_table <- function(report = '',
                      table,
                      table_title = NULL,
                      title_level = 2,
                      html_table_type = c('DT', 'kable'),
                      table_params = NULL){
  html_table_type <- match.arg(arg = html_table_type, choices = c('DT', 'kable'))

  report <- paste(report, # report object (Rmarkdown file text)
                  paste(paste(rep('#', title_level), collapse = ''), table_title),
                  paste('```{r, echo = FALSE, message = FALSE, warning = FALSE}', # r chunk header
                        # create_table
                        paste(paste(c(ifelse(test = html_table_type == 'DT',
                                      yes = 'DT::datatable(',
                                      no = 'knitr::kable('),
                                      deparse(substitute(table)),
                                      table_params[1],
                                      ')'),
                                    collapse = '')
                          , collapse = '\n'),
                        #finish
                        '```', sep = '\n'),
                  sep = '\n\n')
  return(report)
}

