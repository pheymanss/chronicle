#' Add a table to a chronicle report
#'
#' @param report character containing the text of an Rmarkdown report header (and possibly more chunks). Easily create one with chronicle::new_report()
#' @param table data.frame to print on the report.
#' @param table_title title of the table. Default is no title.
#' @param title_level Level of the section title of this plot (ie, number of # on Rmarkdown syntax.)
#' @param html_table_type Either print a knitr::kable table or a DT htmilwidget. kable tables look good with prettydoc (which is TRUE by default on new_report()), DT (https://rstudio.github.io/DT/) are interactive table widgets.
#' @param table_params Single character string with any additional parameters to be passed to either knitr::kable() or DT::datatable(), depending on html_table_type
#'
#' @return
#' @export
#'
#' @examples
add_table <- function(report = new_report(),
                      table,
                      table_title = NULL,
                      title_level = 2,
                      html_table_type = c('DT', 'kable'),
                      table_params = NULL){
  html_table_type <- match.arg(arg = html_table_type, choices = c('DT', 'kable'))

  if(html_table_type == 'kable' & !grepl(pattern = 'prettydoc', x = report)){
    warning(paste('kable tables without prettydoc is somewhat bad-looking. \nConsider setting add_table(html_table_type = "DT") or new_report(pretty_doc = "TRUE").'))
  }
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

