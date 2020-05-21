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
                                      table_params,
                                      ')'),
                                    collapse = '')
                          , collapse = '\n'),
                        #finish
                        '```', sep = '\n'),
                  sep = '\n\n')
  return(report)
}



testdotdotdot <- function(...){
  print(deparse(substitute(...)))
}

testdotdotdot(a = 'a')
