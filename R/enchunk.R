#' Transforms a function call into an Rmarkdown chunk
#'
#' @param report Character string containing all the R Markdown chunks previously added. Default is '', an empty report.
#' @param fun Function to call.
#' @param params List of parameters to be passed to fun.
#' @param chunk_title Title of the Rmarkdown chunk. If NULL, chronicle will try to parse a generic title based on the function and parameters passed using make_title()
#' @param title_level Level of the section title of this plot (ie, number of # on Rmarkdown syntax.)
#' @param echo Whether to display the source code in the output document. Default is FALSE.
#' @param message Whether to preserve messages on rendering. Default is FALSE.
#' @param warning Whether to preserve warnings on rendering. Default is FALSE.
#' @param fig_width Width of the plot (in inches).
#' @param fig_height Height of the plot (in inches).
#' @param guess_title If TRUE, tries to generate a generic title for chronicle::make_* family of functions (eg 'Sepal.Length vs Sepal.Width by Species' for make_scatter)
#'
#' @return An rmarkdown chunk as a character string.
#' @export
#' @examples
#' library(chronicle)
#' html_chunk <- add_chunk(fun = chronicle::make_barplot,
#'                         params = list(dt = 'iris',
#'                                       value = 'Sepal.Width',
#'                                       bars = 'Species'))
#' cat(html_chunk)
add_chunk <- function(report = '',
                      fun,
                      params,
                      chunk_title = NULL,
                      title_level = 2,
                      echo = FALSE,
                      message = FALSE,
                      warning = FALSE,
                      fig_width = NULL,
                      fig_height = NULL,
                      guess_title = TRUE){
  # plot title
  chunk_title <-  paste(
    # title level
    strrep('#', title_level),
    # if missing chunk title, create a generic one
    ifelse(test = is.null(chunk_title) & guess_title,
           yes = chronicle::make_title(fun, params),
           no = c(chunk_title, ' ')))

  # build chunk header with given parameters
  chunk_header <- paste0('```{r, ',
                         paste(c(paste0('echo=', echo),
                                 paste0('message=', message),
                                 paste0('warning=', warning),
                                 if(!is.null(fig_width)){paste('fig.width=', fig_width)}else{'fig.width=params$figure_width'},
                                 if(!is.null(fig_width)){paste('fig.height=', fig_height)}else{'fig.height=params$figure_height'}),
                               collapse = ', '),
                         '}')

  # parse function name
  fun_name <- deparse(substitute(fun))

  # assemble function call
  fun_call <- chronicle::assemble_call(fun_name, params, non_char = c('dt', 'plot_palette', 'plot_palette_generator','static'))

  # enclose and the call around Rmarkdown header and closing
  chunk <- paste(chunk_title, chunk_header, fun_call, '```', sep = '\n')

  report <- glue::glue('{report}\n\n{chunk}')
  return(report)
}


#' Adds additional quotations to character values
#'
#' This is useful when assembling functions calls, where you specify parameter
#' names and character values at the same time.
#'
#' @param x List or named vector
#' @param except Vector specifying the names of the elements that should not be enquoted.
#' @param single_quote Use single quotes (') instead of double quotes ("). Default is TRUE.
#' @param collapse If not NULL, collapse the values into a single vector using this value as the separator. Default is NULL.
#'
#' @return The list or named vector, with additional quotes around the appropriate values
#' @export
#'
#' @examples
#' params = list(a = TRUE, b = FALSE, c = 'ABC', d = 15)
#' add_quotes(params)
#' add_quotes(params, except = 'c')
add_quotes <- function(x, except = NULL, single_quote = TRUE, collapse = NULL){
  # identify which variables should be wrapped in quotes (characters not in except)
  quote_this <- purrr::map_lgl(x, is.character) & if(is.null(names(x))){TRUE}else{!(names(x) %in% except)}

  # enquote
  if(single_quote){
    properly_enquoted <- purrr::map_if(x, quote_this, ~paste0("'", .x, "'"))
  }else{
    properly_enquoted <- purrr::map_if(x, quote_this, ~paste0('"', .x, '"'))
  }

  # collapse into single string
  if(!is.null(collapse)){
    properly_enquoted <- paste(properly_enquoted, collapse = collapse)
  }

  return(properly_enquoted)
}


#' Assembles a formatted function call from a function and a list of parameters
#'
#' @param fun_name Name of the function to be called (must be a character or coercible to a character).
#' @param params Named list or vector containing the parameters for the fun call.
#' @param non_char Names of the parameters whose values should not be interpreted
#' as character values
#'
#' @return A character string with the formatted function call.
#' @export
#'
#' @examples
#' chronicle::assemble_call(fun_name = 'base::sapply',
#'                          params = list(X = 'iris',
#'                                        FUN= 'class'))
#' chronicle::assemble_call(fun_name = 'base::sapply',
#'                          params = list(X = 'iris',
#'                                        FUN= 'class'),
#'                          non_char = c('X', 'FUN'))
assemble_call <- function(fun_name, params, non_char = NULL){
  fun_name <- as.character(fun_name)

  # define indent length for good looking printing
  param_sep <- paste0(',\n', strrep(x = ' ', times = nchar(fun_name) + 1))

  # build the parameter assignment, pairing each parameter name to its corresponding value
  vals_assignment <- paste(names(params),
                           chronicle::add_quotes(params, except = non_char),
                           sep = ' = ',
                           collapse = param_sep)

  return(glue::glue('{fun_name}({vals_assignment})'))
}

#' Returns the count of '#' corresponding to a given title level
#'
#' @param level R Markdonw title level
#'
#' @return '#', '##', '###' and so on, depending on the title level
#' @export
#' @examples
#' rmd_title_level(1)
#' rmd_title_level(3)
rmd_title_level <- function(level){
  strrep(x = '#', times = level)
}

#' Guess a title out of function parameters
#'
#' Detects which make_* function is passed and builds a generic name based on its parameters.
#' @param fun chronicle make_* function
#' @param params parameters for fun
#'
#' @return A generic title for the plot
#' @export
#' @examples
#' make_title(fun = chronicle::make_barplot,
#'                      params = list(value = 'Amount',
#'                                    bars = 'Country',
#'                                    break_bars_by = 'Region'))
#'
#' make_title(fun = chronicle::make_raincloud,
#'            params = list(value = 'value',
#'                          groups = 'species'))
make_title <- function(fun, params){
  plot_title <- NULL
  # switch function does not support closures, so it has to be done with indentical + if elses
  # barplot
  if(identical(fun, chronicle::make_barplot)){
    plot_title <- paste0(ifelse(is.null(params$value), 'Count', params$value),
                         ' by ',
                         params$bars,
                         if(!is.null(params$break_bars_by)){paste(' and', params$break_bars_by)}
                         )
    # boxplot
  }else if(identical(fun, chronicle::make_boxplot)){
    groupings <- if(!is.null(params$groups)){paste(", by", knitr::combine_words(c(params$groups, params$split_groups_by)))}
    plot_title <- glue::glue('Distribution of {params$value}{groupings}')
    # density, histogram, raincloud, violin
  }else if(identical(fun, chronicle::make_density) |
           identical(fun, chronicle::make_histogram) |
           identical(fun, chronicle::make_raincloud) |
           identical(fun, chronicle::make_violin)){
    plot_title <- paste0('Distribution of ', params$value, if(!is.null(params$groups)){paste(' by', params$groups)})
    # dygraph
  }else if(identical(fun, chronicle::make_dygraph)){
    plot_title <- paste0('Evolution of ', params$value, if(!is.null(params$groups)){paste(' by', params$groups)})
    # density
  }else if(identical(fun, chronicle::make_lineplot) | identical(fun, chronicle::make_scatterplot)){
    plot_title <- paste0(params$x, ' vs ', params$y, if(!is.null(params$groups)){paste(' by', params$groups)})
  }else{
    plot_title <-  ''
  }
  return(plot_title)
}


