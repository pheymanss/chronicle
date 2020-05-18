
#' Create the initial Rmarkdown header for a report
#'
#' @param title Title of the report.
#' @param author Author of the repor.
#' @param prettydoc Logical indicating whether or not to use prettydoc formatting on the html report.
#' @param prettydoc_theme Name of the theme used on prettydoc. Default is cayman.
#' @param highlight Rmarkdown highlight theming
#' @param number_sections
#'
#' @return a String conainitng an Rmarkdown header
#'
#' @examples
#' new_report('Report Header Only', author = 'Mysterious Developer', prettydoc = FALSE)
#' new_report('Prettier Report Header', author = 'Same person', prettydoc = TRUE)
new_report <- function(title = 'New chronicle Report',
           author = 'chronicle user',
           prettydoc = TRUE,
           prettydoc_theme = 'leonids',
           highlight = 'github',
           number_sections = FALSE){
  header <- c('---',
              paste('title: ', title),
              'date: "`r Sys.Date()`"',
              paste('author: ', author),
              ifelse(test = prettydoc,
                     yes = paste('output: \n  prettydoc::html_pretty: \n',
                                 '  theme:', prettydoc_theme),
                     no = 'output: html_document'),
              if(number_sections){'number_sections: true'},
              '---',
              '\n',
              '```{r, echo = FALSE, message = FALSE, warning = FALSE}',
              '# If you want this report to be reproducible, add on this chunk all',
              '# the libraries, data loading and preprocessing done before executing',
              '# the chronicle report.',
              '',
              '```'
              ) %>% paste(collapse = '\n')
  return(header)
}


#' Render the report using current environment
#'
#' @param report The caracter string created by chronicle functions
#' @param filename The name of the .html created
#' @param directory The directory in which to render the .html report
#' @param keep_rmd Whether or not to keep the .Rmd file. Default is false.
#'
#' @examples new_report() %>% render_report(filename = 'test_report')
render_report <- function(report, filename = 'Chronicle report', directory = getwd(), keep_rmd = FALSE){
  #wrtie the report as an Rmarkdown file
  rmd_file <- paste0(filename, '.Rmd')
  readr::write_lines(report, rmd_file)

  # render the Rmarkdown file
  rmarkdown::render(input = rmd_file, output_file = paste0(filename, '.html'), output_dir = directory, clean = TRUE, quiet = TRUE)
  if(!keep_rmd){file.remove(rmd_file)}
}


