#' Render the report using all objects from the global environment
#'
#' @param report The character string created by chronicle functions
#' @param filename The name of the .html file(s) created.
#' @param output_format The format of the R Markdown file. Default is prettydoc. Currently supported: 'prettydoc', 'ioslides', 'tufte', 'flexdashboard', 'slidy_presentation', 'html_document' and 'html_notebook'.
#' @param directory The directory in which to render the .html report
#' @param keep_rmd Whether or not to keep the .Rmd file. Default is false.
#' @param title Title of the report.
#' @param author Author of the report.
#' @param prettydoc_theme Name of the theme used on prettydoc. Default is leonids.
#' @param highlight Rmarkdown highlight theming. Default is github highlighting.
#' @param number_sections Whether or not to number the sections and subsections fo the report.
#' @param table_of_content Whether or not to include a table fo content at the beginning of the report.
#' @param table_of_content_depth The depth of sections and subsections to be displayed on the table of content.
#' @param fig_width Set the global figure width or the rmarkdown file.
#' @param fig_height Set the global figure height or the rmarkdown file.
#' @param render_html Whether or not to render the R Markdown to get the html files. Default is TRUE.
#'
#' @return Renders the report as an HTML file.
#'
#' @export
#'
#' @examples
#' # report_demo <- add_title(title = 'This is how a chronicle report looks', title_level = 1) %>%
#' # add_density(dt = iris, groups = 'Species', value = 'Sepal.Length', faceted = F) %>%
#' #   add_boxplot(dt = iris, groups = 'Species', value = 'Sepal.Length') %>%
#' #   add_barplot(dt = iris, bars = 'Species', value = 'Sepal.Length')
#' #   add_table(table = iris,
#' #             table_title = 'This is the iris dataset. Smells good!',
#' #             html_table_type = 'kable') %>%
#' #   add_table(table = mpg,
#' #             table_title = 'And this is mpg',
#' #             html_table_type = 'DT')
#' #render_report(report = report_demo,
#' #              title = 'Demo Output',
#' #              author = 'This is the author',
#' #              filename = 'demo_output',
#' #              output_format = 'prettydoc',
#' #              keep_rmd = TRUE)
render_report <- function(report = '',
                          title = 'Report Output',
                          author = 'Author Name',
                          filename = paste('report', gsub(x = Sys.Date(), pattern = '-', replacement = ''), sep = '_'),
                          output_format = 'prettydoc',
                          prettydoc_theme = 'leonids',
                          highlight = 'github',
                          number_sections = FALSE,
                          table_of_content = FALSE,
                          table_of_content_depth = 1,
                          fig_width = 11,
                          fig_height = 5,
                          directory = getwd(),
                          keep_rmd = FALSE,
                          render_html = TRUE){

  output_format <- match.arg(output_format, c('prettydoc',
                                              # 'bookdown',
                                              # 'pagedown',
                                              'ioslides',
                                              # 'github_document',
                                              'tufte',
                                              # 'xaringan',
                                              # 'rolldown',
                                              'flexdashboard',
                                              'slidy_presentation',
                                              'html_document',
                                              'html_notebook'))

  # build the yaml parameters of
  header <- paste(
    # title, date, author
    paste('---',
          paste('title: ', title),
          'date: "`r Sys.Date()`"',
          paste('author: ', author),
          sep = '\n'),
    # output file configs
    output_config(output_format = output_format ),
    # close yaml and add first chunk
    paste('---',
          '\n',
          '```{r, echo = FALSE, message = FALSE, warning = FALSE}',
          'library(chronicle)',
          '# If you want this report to be reproducible, add on this chunk all',
          '# the libraries, data loading and preprocessing done before executing',
          '# the chronicle report.',
          '',
          '```',
          sep = '\n'),
  sep = '\n') %>% purrr::set_names(output_format)

#   if(enable_dark_mode){
#     header <- paste(header,
# '```{css, echo = FALSE}
# @media (prefer-colorc-scheme: dark) {
#   body {
#     backgroud-color: black;
#     filter: invert(1);
#   }
# }
# ```', sep = '\n\n')
#   }

  reports <- paste(header, report, sep = '\n')

  # write the report as an Rmarkdown file
  rmd_filenames <- paste0(directory, '/', filename, '_', output_format, '.Rmd')
  purrr::walk2(.x = reports,
               .y = rmd_filenames,
               .f = readr::write_lines)

  # render the Rmarkdown file
  if(render_html){
    purrr::walk2(.x = rmd_filenames,
                 .y = paste0(filename, '_', output_format, '.html'),
                 .f = ~rmarkdown::render(input = .x,
                                         output_file = .y,
                                         output_dir = directory,
                                         clean = TRUE,
                                         quiet = TRUE))
  }

  if(!keep_rmd){purrr::walk(.x = rmd_filenames, .f = file.remove)}
}

#' Wrapper to update chronicle from github
#'
#' Quality of life improvement until I submit the package to CRAN. Runs devtools::install_github('pheymanss/chronicle')
# update_chronicle <- function(){
#   devtools::install_github('pheymanss/chronicle')
# }



#' Build the yaml output specification for an R Markdown
#'
#' Currently supported: prettydoc, ioslides, tufte, flexdashboard, slidy_presentation,
#' html_document, html_notebook.
#'
#' @param output_format The format of the R Markdown file.
#' @param number_sections Whether or not to number the sections and subsections of the report.
#' @param table_of_content Whether or not to include a table fo content at the beginning of the report.
#' @param table_of_content_depth The depth of sections and subsections to be displayed on the table of content.
#' @param fig_width Set the global figure width or the rmarkdown file.
#' @param fig_height Set the global figure height or the rmarkdown file.
#' @param prettydoc_theme Name of the theme used on prettydoc. Default is leonids.
#'
#' @return The lines needed in the yaml header of an R Markdown file to render as the specified output type.
#' @export
#'
#' @examples
#' cat(output_config('prettydoc'))
#' cat(output_config('ioslides'))
output_config <- function(output_format,
                          number_sections = FALSE,
                          table_of_content = FALSE,
                          table_of_content_depth = 1,
                          fig_width = 11,
                          fig_height = 5,
                          prettydoc_theme = 'leonids'){

  output_conf <- data.table::fcase(
  # prettydoc
    output_format == 'prettydoc',
    paste('output:
  prettydoc::html_pretty:
    theme:', prettydoc_theme),
  # bookdown,
  #   output_format == 'bookdown',
  #   'output:
  # bookdown::gitbook:',
  # pagedown
  #   output_format == 'pagedown',
  #   'output:
  # pagedown::book_crc:',
  # ioslides
    output_format == 'ioslides',
    'output:
  ioslides_presentation:
    widescreen: true',
  # github_document
  #   output_format == 'github_document',
  #   'output:
  # github_document:',
  # tufte
    output_format == 'tufte',
    'output:
  tufte::tufte_html:
    tufte_variant: envisioned',
  # xaringan
  #   output_format == 'xaringan',
  # 'output:
  # xaringan::moon_reader:
  #   lib_dir: libs
  #   nature:
  #     countIncrementalSlides: false',
  # rolldown
  # output_format == 'rolldown',
  # 'output:
  # rolldown::scrollama_sidebar:',
  # flexdashboard
  output_format == 'flexdashboard',
  'output:
  flexdashboard::flex_dashboard:',
  # slidy_presentation
  output_format == 'slidy_presentation',
  'output:
  slidy_presentation:',
  # html_document
  output_format == 'html_document',
  'output:html_document:',
  # html_notebook
  output_format == 'html_notebook',
  'output:html_notebook:') %>%
    purrr::set_names(output_format)

  output_conf <- paste(output_conf,
        paste(c(
          ifelse(number_sections, '    number_sections: true', '    number_sections: false'),
          ifelse(table_of_content, '    toc: true', '    toc: false'),
          if(!is.null(fig_width)){c(paste0('    fig_width: ', fig_width))},
          if(!is.null(fig_height)){c(paste0('    fig_height: ', fig_height))}),
          collapse = '\n'), sep = '\n')

  return(output_conf)
}

#' Create the initial Rmarkdown header for a report
#'
#' @param title Title of the report.
#' @param author Author of the report.
#' @param prettydoc Whether or not to use prettydoc formatting on the html report. Default is TRUE.
#' @param prettydoc_theme Name of the theme used on prettydoc. Default is leonids.
#' @param highlight Rmarkdown highlight theming. Default is github highlighting.
#' @param number_sections Whether or not to number the sections and subsections fo the report.
#' @param table_of_content Whether or not to include a table fo content at the beginning of the report.
#' @param table_of_content_depth The depth of sections and subsections to be displayed on the table of content.
#' @param fig_width Set the global figure width or the rmarkdown file.
#' @param fig_height Set the global figure height or the rmarkdown file.
#'
#' @return A string containing an R Markdown header
#' @export
#' @examples
#' library(magrittr)
#' library(chronicle)
#' new_report(title = 'Simple Report Header',
#'            author = 'Anonymous Developer',
#'            prettydoc = FALSE)
#' new_report(title = 'Prettier Report Header',
#'            author = 'The same person as before',
#'            prettydoc = TRUE)
new_report <- function(title = 'New chronicle Report',
                       author = 'chronicle user',
                       prettydoc = TRUE,
                       prettydoc_theme = 'leonids',
                       highlight = 'github',
                       number_sections = FALSE,
                       table_of_content = FALSE,
                       table_of_content_depth = 1,
                       fig_width = 11,
                       fig_height = 5){

  warning('new_report() has been deprecated. As of versin 0.2.0, all parameters
previously passed to new_report can be passed to render_report() directly.')

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
              'library(chronicle)',
              '# If you want this report to be reproducible, add on this chunk all',
              '# the libraries, data loading and preprocessing done before executing',
              '# the chronicle report.',
              '',
              '```'
  ) %>% paste(collapse = '\n')
  return(header)
}
