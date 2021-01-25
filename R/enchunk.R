#' Transforms a function call into an Rmarkdown chunk
#'
#' @param report Character string containing the text of an Rmarkdown report header (and possibly more chunks). Easily create one with chronicle::new_report(), and if NULL, this will be the default value.
#' @param dt_expr Name of the table to run fun on.
#' @param fun Function to call.
#' @param params List of parameters to be passed to fun.
#' @param chunk_title Title of the Rmarkdown chunk. If NULL, chronicle will try to parse a generic title based on the function and parameters passed using make_title()
#' @param title_level Level of the section title of this plot (ie, number of # on Rmarkdown syntax.)
#' @param echo Whether to display the source code in the output document. Default is FALSE.
#' @param message Whether to preserve messages on rendering. Default is FALSE.
#' @param warning Whether to preserve warnings on rendering. Default is FALSE.
#' @param fig_width Width of the plot (in inches).
#' @param fig_height Height of the plot (in inches).
#'
#' @return An rmarkdown chunk as a character string.
#' @export
#' @examples
#' library(chronicle)
#' html_chunk <- add_chunk(fun = make_barplot,
#'                         dt = 'iris',
#'                         params = list(value = 'Sepal.Width',
#'                                       bars = 'Species'))
#' cat(html_chunk)
add_chunk <- function(report = '',
                      dt_expr = NULL,
                      fun,
                      params,
                      chunk_title = NULL,
                      title_level = 2,
                      echo = FALSE,
                      message = FALSE,
                      warning = FALSE,
                      fig_width = NULL,
                      fig_height = NULL){
  # chack that dt_expr is the name of the table, not the table
  if(!is.character(dt_expr)){
    stop('Could not parse plot data expression. dt_expr should be a character with the name of the table')
  }

  # plot title
  chunk_title <-  paste(
    # title level
    paste(rep('#', title_level), collapse = ''),
    # if missing chunk title, create a generic one
    ifelse(test = is.null(chunk_title),
           yes = make_title(fun, params),
           no = chunk_title))

  # build chunk header with given parameters
  chunk_header <- paste0('```{r, ',
                         paste(c(paste('echo = ', echo),
                                 paste('message = ', message),
                                 paste('warning = ', warning),
                                 if(!is.null(fig_width)){paste('fig.width =', fig_width)},
                                 if(!is.null(fig_width)){paste('fig.height =', fig_height)}),
                               collapse = ', '),
                         '}')

  # parse function call
  fun_name <- deparse(substitute(fun))
  vals_assignment <- paste(names(params), paste0("'", params, "'"), sep = ' = ', collapse = ', ')

  fun_call <- glue::glue('{fun_name}(dt = {dt_expr}, ') %>% paste0(vals_assignment) %>%  paste0(')')

  # enclose and the call around Rmarkdown header and closing
  chunk <- paste(chunk_title, chunk_header, fun_call, '```', sep = '\n')

  report <- paste(report, chunk, sep = '\n\n')
  return(report)
}


#' Guess a title out of function parameters
#'
#' Detects which make_* function is passed and builds a generic name based on its parameters.
#' @param fun chronicle make_* function
#' @param params parameters for fun
#'
#' @return A generic title for the plot
#' @export
#' @examples make_title(fun = make_barplot,
#'                      params = list(value = 'Amount',
#'                                    bars = 'Country',
#'                                    break_bars_by = 'Region'))
make_title <- function(fun, params){
  plot_title <- NULL
  # switch function does not support closures, so it has to be done with indentical + if elses
  # barplot
  if(identical(fun, chronicle::make_barplot)){
    plot_title <- paste0(ifelse(test = is.null(params$value),
                                yes = 'Count',
                                no = params$value),
                         ' by ',
                         params$bars,
                         if(!is.null(params$break_bars_by)){paste(' and', params$break_bars_by)})
    # boxplot
  }else if(identical(fun, chronicle::make_boxplot)){
    plot_title <- paste0('Distribution of ', params$value, if(!is.null(params$groups)){paste(' by', params$groups)})
    # violin
  }else if(identical(fun, chronicle::make_violin)){
    plot_title <- paste0('Distribution of ', params$value, if(!is.null(params$groups)){paste(' by', params$groups)})
    # dygraph
  }else if(identical(fun, chronicle::make_dygraph)){
    plot_title <- paste0('Evolution of ', params$value, if(!is.null(params$groups)){paste(' by', params$groups)})
    # density
  }else if(identical(fun, chronicle::make_density)){
    plot_title <- paste0('Distribution of ', params$value, if(!is.null(params$groups)){paste(' by', params$groups)})
    # histogram
  }else if(identical(fun, chronicle::make_histogram)){
    plot_title <- paste0('Distribution of ', params$value, if(!is.null(params$groups)){paste(' by', params$groups)})
    # line plot
  }else if(identical(fun, chronicle::make_lineplot)){
    plot_title <- paste0(params$x, ' vs ', params$y, if(!is.null(params$groups)){paste(' by', params$groups)})
  }else{
    plot_title <-  ''
  }
  return(plot_title)
}


