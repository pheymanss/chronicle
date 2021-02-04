#' Render the report using all objects from the global environment
#'
#' @param report Character string containing all the R Markdown chunks previously added (through chronicle::add_* functions.) Default is '', an empty report.
#' @param output_format The format of the R Markdown file. Default is prettydoc. Currently  <- : 'prettydoc', 'ioslides', 'tufte', 'flexdashboard', 'slidy_presentation', 'html_document' and 'html_notebook'.
#' @param filename The name of the .html file(s) created.
#' @param title Title of the report.
#' @param author Author of the report.
#' @param directory The directory in which to render the .html report
#' @param keep_rmd Whether or not to keep the .Rmd file. Default is false.
#' @param render_reports Whether or not to render the reports. Default is true Set render_reports = FALSE and keep_rmd = TRUE to only build the R Markdown files
#' @param number_sections Whether or not to number the sections and subsections fo the report.
#' @param table_of_content Whether or not to include a table fo content at the beginning of the report. Some formats does not allow overriding this.
#' @param table_of_content_depth The depth of sections and subsections to be displayed on the table of content.
#' @param fig_width Set the global figure width or the rmarkdown file.
#' @param fig_height Set the global figure height or the rmarkdown file.
#' @param rmdformats_theme The theme to be used for [rmdformats](https://github.com/juba/rmdformats) outputs. Default is "downcute", and possible values are "downcute", "robobook", "material", "readthedown", "html_clean", "html_docco".
#' @param prettydoc_theme Name of the theme used on [prettydoc](https://prettydoc.statr.me/themes.html). Default is "leonids", and ossible values are "cayman", "tactile", "architect", "leonids", "hpstr".
#' @param docx_reference_file The path for a blank Microsoft Word document to use as template for the 'word_document' output.
#' @param pptx_reference_file The path for a blank Microsoft PowerPoint document to use as template for the 'powerpoint_presentation' output.
#' @param html_theme The theme to be used for [hmtl_document](https://www.datadreaming.org/post/r-markdown-theme-gallery/) outputs. Default is "simplex".
#' @param rticles_template The theme to be used fo [rticles](https://github.com/rstudio/rticles). Default is "arxiv_article"
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
                          output_format = 'rmdformats',
                          filename = paste('report',
                                           gsub(x = Sys.Date(),
                                                pattern = '-',
                                                replacement = ''),
                                           sep = '_'),
                          title = 'Output title',
                          author = 'Author Name',
                          directory = getwd(),
                          keep_rmd = FALSE,
                          render_reports = TRUE,
                          number_sections = FALSE,
                          table_of_content = FALSE,
                          table_of_content_depth = 1,
                          fig_width = 11,
                          fig_height = 5,
                          rmdformats_theme = 'downcute',
                          prettydoc_theme = 'leonids',
                          docx_reference_file = NULL,
                          pptx_reference_file = NULL,
                          rticles_template = 'arxiv_article',
                          html_theme = 'simplex',
                          custom_output = NULL
                          ){

  # build the yaml parameters of
  headers <- chronicle::output_config(output_format = output_format,
                                      title = title,
                                      author = author,
                                      number_sections = number_sections,
                                      table_of_content = table_of_content,
                                      table_of_content_depth = table_of_content_depth,
                                      fig_width = fig_width,
                                      fig_height = fig_height,
                                      rmdformats_theme = rmdformats_theme,
                                      prettydoc_theme = prettydoc_theme,
                                      docx_reference_file = docx_reference_file,
                                      pptx_reference_file = pptx_reference_file,
                                      html_theme = html_theme,
                                      rticles_template = rticles_template,
                                      custom_output = NULL)

  reports <- paste(headers, report, sep = '\n')

  # write the report as an Rmarkdown file
  rmd_filenames <- paste0(glue::glue('{directory}/{filename}'),
                          if(length(output_format) > 1){paste0('_', output_format)},
                          '.Rmd')

  purrr::walk2(.x = reports,
               .y = rmd_filenames,
               .f = readr::write_lines)

  # render the Rmarkdown file
  if(render_reports){
   output_filenames <- paste0(filename,
                              if(length(output_format) > 1){paste0('_', output_format)},
                              chronicle::file_extension(output_format))

    purrr::walk2(.x = rmd_filenames,
                 .y = output_filenames,
                 .f = ~rmarkdown::render(input = .x,
                                         output_file = .y,
                                         output_dir = directory,
                                         clean = TRUE,
                                         quiet = TRUE))
  }

  if(!keep_rmd){purrr::walk(.x = rmd_filenames, .f = file.remove)}
}


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
#' @param title Title of the report.
#' @param author Author of the report.
#' @param rmdformats_theme The theme to be used for [rmdformats](https://github.com/juba/rmdformats) outputs. Default is "downcute", and possible values are "downcute", "robobook", "material", "readthedown", "html_clean", "html_docco".
#' @param prettydoc_theme Name of the theme used on [prettydoc](https://prettydoc.statr.me/themes.html). Default is "leonids", and ossible values are "cayman", "tactile", "architect", "leonids", "hpstr".
#' @param docx_reference_file The path for a blank Microsoft Word document to use as template for the 'word_document' output.
#' @param pptx_reference_file The path for a blank Microsoft PowerPoint document to use as template for the 'powerpoint_presentation' output.
#' @param html_theme The theme to be used for [hmtl_document](https://www.datadreaming.org/post/r-markdown-theme-gallery/) outputs. Default is "simplex".
#' @param rticles_template
#' @param custom_output
#'
#' @return The lines needed in the yaml header of an R Markdown file to render as the specified output type.
#' @export
#'
#' @examples
#' cat(output_config('prettydoc'))
#' cat(output_config('ioslides'))
output_config <- function(output_format,
                          title = 'Output title',
                          author = 'Author Name',
                          number_sections = FALSE,
                          table_of_content = FALSE,
                          table_of_content_depth = 1,
                          fig_width = 11,
                          fig_height = 5,
                          rmdformats_theme = 'downcute',
                          prettydoc_theme = 'leonids',
                          docx_reference_file = NULL,
                          pptx_reference_file = NULL,
                          html_theme = 'simplex',
                          rticles_template = 'arxiv_article',
                          custom_output = NULL){

  # remove non-supported formats
  supported <- output_format %in% c('bookdown',
                                    'flexdashboard',
                                    'github_document',
                                    'html_document',
                                    'html_notebook',
                                    'ioslides',
                                    'pagedown',
                                    'pdf',
                                    'powerpoint_presentation',
                                    'prettydoc',
                                    'rmdformats',
                                    'rolldown',
                                    'rticles',
                                    'slidy_presentation',
                                    'tufte_handout',
                                    'tufte_html',
                                    'word_document',
                                    'xaringan')

  if(any(!supported)){
    warning('"', paste(output_format[!supported], collapse = ', '),
            '" not supported as ouput_format, will be ignored and no output will be created.')
    output_format <- output_format[supported]
  }

  # identify which outputs need static plots
  set_static <- output_format %in% c('pdf',
                                     'pagedown',
                                     'github_document',
                                     'rolldown',
                                     'word_document',
                                     'powerpoint_presentation',
                                     'tufte_handout',
                                     'rticles')

  # build the yaml entry for each output_format
  output_conf <- data.table::fcase(
    # INTERACTIVE  FORMATS ---
    # rmdformats
    output_format == 'rmdformats',
    paste0('output: rmdformats::', rmdformats_theme),
    # prettydoc
    output_format == 'prettydoc',
    paste('output:
  prettydoc::html_pretty:
    theme:', prettydoc_theme) %>%
    paste(
      ifelse(number_sections, '    number_sections: true', '    number_sections: false'),
      ifelse(table_of_content, '    toc: true', '    toc: false'),
      if(!is.null(fig_width)){c(glue::glue('    fig_width: {fig_width}'))},
      if(!is.null(fig_height)){c(glue::glue('    fig_height: {fig_height}'))},
      sep = '\n'),
  # bookdown,
    output_format == 'bookdown',
    'output:
  bookdown::gitbook:
    self_contained: true',
  # ioslides
  output_format == 'ioslides',
  'output:
  ioslides_presentation:
    widescreen: true',
  # tufte
  output_format == 'tufte_html',
  'output:
  tufte::tufte_html: default',
  # tufte_variant: envisioned',
  # xaringan
    output_format == 'xaringan',
  'output:
  xaringan::moon_reader:
    lib_dir: libs
    nature:
      countIncrementalSlides: false',
  # rolldown
  output_format == 'rolldown',
  'output:
  rolldown::scrollama_sidebar',
  # flexdashboard
  output_format == 'flexdashboard',
  'output:
  flexdashboard::flex_dashboard',
  # slidy_presentation
  output_format == 'slidy_presentation',
  'output:
  slidy_presentation',
  # html_document
  output_format == 'html_document',
  paste('output:html_document:
  html_theme:', html_theme) %>%
    paste(
      ifelse(number_sections, '  number_sections: true', '  number_sections: false'),
      ifelse(table_of_content, '  toc: true', '  toc: false'),
      if(table_of_content){'  toc_float: true'},
      if(!is.null(fig_width)){c(glue::glue('  fig_width: {fig_width}'))},
      if(!is.null(fig_height)){c(glue::glue('  fig_height: {fig_height}'))},
      sep = '\n'),
  # html_notebook
  output_format == 'html_notebook',
  'output: html_notebook',
  # STATIC FORMATS ---
  # pagedown
  output_format == 'pagedown',
  'output:
  pagedown::book_crc',
  # github_document
  output_format == 'github_document',
  'output:
  github_document',
  # pdf
  output_format == 'pdf',
  'output: pdf_document',
  # word
  output_format == 'word_document',
  ifelse(test = is.null(docx_reference_file),
         yes = 'output: word_document',
         no = 'output: word_document:
    reference_doc: docx_reference_file'),
  # powerpoint
  output_format == 'powerpoint_presentation',
  ifelse(test = is.null(docx_reference_file),
         yes = 'output: powerpoint_presentation',
         no = 'output: powerpoint_presentation:
    reference_doc: pptx_reference_file'),
  # tufte_handout
  output_format == 'tufte_handout',
  'output: tufte::tufte_handout',
  # # rticles
  output_format == 'rticles',
  'output:
  rticles::jss_article:
    pandoc_args: [-V, documentclass=?]')

  header <- paste(
    # title, date, author
    paste('---',
          paste('title: ', title),
          'date: "`r Sys.Date()`"',
          paste('author: ', author),
          sep = '\n'),
    # output file configs
    c(output_conf, custom_output),
    # close yaml and add first chunk
    paste('---',
          '\n',
          '```{r, echo = FALSE, message = FALSE, warning = FALSE}',
          'library(chronicle)',
          paste('set_static <-', set_static),
          '# If you want this report to be reproducible, add on this chunk all',
          '# the libraries, data loading and preprocessing done before executing',
          '# the chronicle report.',
          '',
          '```',
          sep = '\n'),
    sep = '\n') %>%
    purrr::set_names(output_format)

  return(header)
}


#' Parse the file extension for each R Markdown output format
#'
#' Currently supports:
#'
#'  *  rmdformats
#'  *  prettydoc
#'  *  bookdown
#'  *  ioslides
#'  *  tufte_html
#'  *  xaringan
#'  *  rolldown
#'  *  flexdashboard
#'  *  slidy_presentation
#'  *  html_document
#'  *  html_notebook
#'  *  pagedown
#'
#' @param file_type R Markdown output formats.
#'
#' @return The file extension corresponding to the provided formats (".html", "pdf", ".md", ".docx", ".pptx")
#' @export
#'
#' @examples file_extension(c('prettydoc', 'word_document', 'tufte_handout'))
file_extension <- function(file_type){
  data.table::fcase(file_type %in% c("rmdformats", "prettydoc", "bookdown",
                                     "ioslides", "tufte_html", "xaringan", "rolldown", "flexdashboard",
                                     "slidy_presentation", "html_document", "html_notebook", "pagedown"),
                    '.html',
                    file_type %in% c('pdf', 'tufte_handout', 'rticles'),
                    '.pdf',
                    file_type == 'github_document',
                    '.md',
                    file_type == 'word_document',
                    '.docx',
                    file_type == 'powerpoint_presentation',
                    '.pptx')
}
