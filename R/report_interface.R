#' Create the initial Rmarkdown header for a report
#'
#' @param title Title of the report.
#' @param author Author of the repor.
#' @param prettydoc Logical indicating whether or not to use prettydoc formatting on the html report.
#' @param prettydoc_theme Name of the theme used on prettydoc. Default is cayman.
#' @param highlight Rmarkdown highlight theming
#' @param number_sections Whether or not to numnber the sections and subsections fo the report.
#' @param table_of_content Whether or not to build a table fo content at the begining of the report.
#' @param table_of_content_depth The depth of sections and subsections to be displayed on the table of content.
#' @param fig_width Set the global figure width or the rmarkdown file.
#' @param fig_height Set the global figure height or the rmarkdown file.
#'
#' @return a String conainitng an Rmarkdown header
#' @export
#' @examples
#' library(magrittr)
#' library(chronicle)
#' new_report('Simple Report Header', author = 'Anonymous Developer', prettydoc = FALSE)
#' new_report('Prettier Report Header', author = 'The same person as before', prettydoc = TRUE)
new_report <- function(title = 'New chronicle Report',
                       author = 'chronicle user',
                       prettydoc = TRUE,
                       prettydoc_theme = 'leonids',
                       highlight = 'github',
                       number_sections = FALSE,
                       table_of_content = FALSE,
                       table_of_content_depth = 1,
                       fig_width = NULL,
                       fig_height = NULL){
  header <- c('---',
              paste('title: ', title),
              'date: "`r Sys.Date()`"',
              paste('author: ', author),
              ifelse(test = prettydoc,
                     yes = paste('output: \n  prettydoc::html_pretty: \n',
                                 '   theme:', prettydoc_theme),
                     no = 'output:\n  html_document:'),
              if(number_sections){'    number_sections: true'},
              if(table_of_content){c('    toc: true', paste('    toc_depth:', table_of_content_depth))},
              if(!is.null(fig_width)){c(paste0('    fig_width: ', fig_width))},
              if(!is.null(fig_height)){c(paste0('    fig_height: ', fig_height))},
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
#' @param render_html Whether or not to render the report as an interactive hmtl file.
#' @param render_pdf Whether or not to render the report as a PDF file. Keep in mind that while the file will be much more lightweight, you will lose all the interactivity the html provides.
#'
#' @export
#'
#' @examples
#' library(chronicle)
#' library(magrittr)
#' new_report() %>% render_report(filename = 'test_report')
#' file.remove('test_report.html')
render_report <- function(report, filename = 'Chronicle report', directory = getwd(), keep_rmd = FALSE, render_html = TRUE, render_pdf = FALSE){

  # check if additional github packages are installed, and if not ask the user to install them or not render a PDF file
  # if(render_pdf){
  #   if(!('webshot2' %in% installed.packages())){
  #     warning('Due to an incompatibility between plotly (the html interactive plot library used for plots in chronicle) and webshot (the library for rendering .html files as .pdfs), rendering PDF files with chronicle requires a currently in-development package called webshot2, which in turn requires another in-development package called chromote.')
  #     install_chromote_webshot2 <- grepl('y', readline(prompt = 'Do you want to proceed installing both webshot2 and chromote from github?\nIf not, the excecution will continue but will not be able to render the PDF file. yes|no:'))
  #     if(install_chromote_webshot2){
  #       devtools::install_github("rstudio/chromote")
  #       devtools::install_github("rstudio/webshot2")
  #     }else{
  #       render_pdf <- FALSE
  #     }
  #   }
  # }

  #wrtie the report as an Rmarkdown file
  rmd_file <- paste0(filename, '.Rmd')
  readr::write_lines(report, rmd_file)

  # render the Rmarkdown file
  if(render_html){
    rmarkdown::render(input = rmd_file, output_file = paste0(filename, '.html'), output_dir = directory, clean = TRUE, quiet = TRUE)
  }

#
#   if(render_pdf & 'webshot2' %in% utils::installed.packages()){
#     webshot2::rmdshot(rmd_file, paste0(filename, '.pdf'), delay = 30)
#   }
  if(!keep_rmd){file.remove(rmd_file)}
}

#' Wrapper to update chronicle from github
#'
#' Quality of life improvement until I submit the package to CRAN. Runs devtools::install_github('pheymanss/chronicle')
update_chronicle <- function(){
  devtools::install_github('pheymanss/chronicle')
}



