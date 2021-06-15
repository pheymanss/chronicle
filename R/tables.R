#' Add a table to a chronicle report
#'
#' @param report Character string containing all the R Markdown chunks previously added. Default is '', an empty report.
#' @param table data.frame to print on the report.
#' @param table_title title of the table. Default is no title.
#' @param title_level Level of the section title of this plot (ie, number of # on Rmarkdown syntax.)
#' @param html_table_type Either print a knitr::kable table or a DT htmlwidget.
#' @param table_params A named list of additional parameters to be passed to either knitr::kable() or DT::datatable(), depending on html_table_type
#' @param fig_width Width of the figures printed from this code.
#' @param fig_height Height of the figures printed from this code.
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
                      table_params = NULL,
                      fig_width = NULL,
                      fig_height = NULL
                      ){
  html_table_type <- match.arg(arg = html_table_type, choices = c('DT', 'kable'))

  if(!is.null(table_title)){
    table_title <- paste0('\n', chronicle::rmd_title_level(title_level), ' ', table_title)
  }


  table_call <- paste('```{r, echo=FALSE, message=FALSE, warning=FALSE}',
                      # create_table
                      paste(paste(c(ifelse(test = html_table_type == 'DT',
                                           yes = 'DT::datatable(',
                                           no = 'knitr::kable('),
                                    deparse(substitute(table)),
                                    table_params[1],
                                    ')'),
                                  collapse = '')
                            , collapse = '\n'),
                      '```', sep = '\n')

  report <- glue::glue("{report}
{ifelse(is.null(table_title), '', table_title)}
{table_call}", .trim=TRUE)

  return(report)
}
