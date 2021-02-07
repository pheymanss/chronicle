#' Create a bar plot from a data frame through ggplotly
#'
#' @param dt data.frame containing the data to plot.
#' @param bars Name of the column containing the different groups.
#' @param value Name of the columns to use as value on the y axis of the plot. If NULL (default), counts will be used.
#' @param break_bars_by Name of the categorical variable used to break each bar
#' @param horizontal Plot the bars horizontally. Default is FALSE
#' @param sort_by_value Sort the bars by value. Default is FALSE
#' @param sort_decreasing Sort the values decreasingly. Default is TRUE, but sort_by_value must also be TRUE.
#' @param ggtheme ggplot2 theme function to apply. Default is ggplot2::theme_minimal.
#' @param x_axis_label Label for the x axis.
#' @param y_axis_label Label for the y axis.
#' @param plot_palette Character vector of hex codes specifying the colors to use on the plot.
#' @param plot_palette_generator Palette from the viridis package used in case plot_palette is unspecified or insufficient for the number of colors required
#' @param static If TRUE, the output will be static ggplot chart instead of an interactive ggplotly chart. Default is FALSE.
#'
#' @export
#' @return A plotly-ized version of a ggplot bar plot.
#'
#' @examples
#' make_barplot(dt = iris, bars = 'Species', value = 'Sepal.Length')
#' make_barplot(dt = ggplot2::mpg,
#'              bars = 'manufacturer',
#'              break_bars_by = 'model',
#'              value = 'cty',
#'              horizontal = TRUE,
#'              sort_by_value = TRUE)
#' @importFrom rlang .data
make_barplot <- function(dt,
                         bars,
                         value = NULL,
                         break_bars_by = NULL,
                         horizontal = FALSE,
                         sort_by_value = FALSE,
                         sort_decreasing = TRUE,
                         ggtheme = 'minimal',
                         x_axis_label = NULL,
                         y_axis_label = NULL,
                         plot_palette = NULL,
                         plot_palette_generator = 'plasma',
                         static = FALSE){

  # check that the specified columns are present in the data
  dt_cols <- c(bars, value, break_bars_by)
  if(any((!dt_cols %in% colnames(dt)))){
    stop(paste(setdiff(dt_cols, colnames(dt)), collapse = ', '), ' not found on dt.')
  }


  dt1 <- data.table::setDT(copy(dt))
  # coerce to character
  dt1[[bars]] <- as.character(dt1[[bars]])
  if(!is.null(break_bars_by)){
    dt1[[break_bars_by]] <- as.character(dt1[[break_bars_by]])

    # avoid a redundant specification
    if(bars == break_bars_by){
      break_bars_by <- NULL
    }
  }

  # summarise table for plot. If no value is specified, use counts
  if(is.null(value)){
    value = 'Count'
    plot_dt <- dt1[, list(Count = .N), by = c(bars, break_bars_by)]
  }else{
    plot_dt <- dt1[, list(value = sum(get(value))), by = c(bars, break_bars_by)]
    data.table::setnames(plot_dt, 'value', value)
  }

  if(as.logical(sort_by_value)){
    # if horizontal, flip the value of sort_decreasing (will apply ggplot2::coord_flip)
    if(as.logical(horizontal)){
      sort_decreasing <- !as.logical(sort_decreasing)
    }

    # if given break bars by, summarize and sort by total
    if(!is.null(break_bars_by)){
      bar_order <- plot_dt[, list(total_value = sum(get(value))), by = c(bars)] %>%
        data.table::setorderv(cols = 'total_value',
                              order = ifelse(test = sort_decreasing,
                                             -1,
                                             1))
      plot_dt[, (bars) := factor(get(bars), levels = bar_order[[bars]])]
    }else{
      data.table::setorderv(x = plot_dt,
                            cols = value,
                            order = ifelse(test = sort_decreasing, -1, 1)
      )
      plot_dt[, (bars) := factor(get(bars), levels = get(bars))]
    }
  }

  # check how many colors are needed for plotting
  plot_palette_length <- ifelse(test = is.null(break_bars_by),
                                yes = data.table::uniqueN(plot_dt[[bars]]),
                                no = data.table::uniqueN(plot_dt[[break_bars_by]]))

  # map the gg theme to its corresponding ggplot2::theme_ function
  ggtheme <- switch(ggtheme,
                    'bw' = ggplot2::theme_bw,
                    'classic' = ggplot2::theme_classic,
                    'dark' = ggplot2::theme_dark,
                    'gray' = ggplot2::theme_gray,
                    'grey' = ggplot2::theme_grey,
                    'light' = ggplot2::theme_light,
                    'linedraw' = ggplot2::theme_linedraw,
                    'minimal' = ggplot2::theme_minimal,
                    'void' = ggplot2::theme_void,
                    ggplot2::theme_minimal)

  # map the generator to its corresponding viridis palette
  plot_palette_generator <- switch(plot_palette_generator,
                                   'cividis' = viridis::cividis,
                                   'inferno' = viridis::inferno,
                                   'magma' = viridis::magma,
                                   'plasma' = viridis::plasma,
                                   'viridis' = viridis::viridis,
                                   viridis::magma)

  #if not provided, use palette from viridis::plasma
  if(is.null(plot_palette)){
    plot_palette <- plot_palette_generator(plot_palette_length, begin = 0, end = .8)
  }else if(plot_palette_length > length(plot_palette)){
    warning('Insufficient palette length provided for a bar plot of ',
            value, ' by ', ifelse(test = is.null(break_bars_by),
                                  yes = bars,
                                  no = break_bars_by),
            '. Adding the missing ', (plot_palette_length - length(plot_palette)),
            ' colors from plot_palette_generator')
    plot_palette <- c(plot_palette,
                      plot_palette_generator(plot_palette_length - length(plot_palette), begin = 0, end = .8))
  }

  # create bar plot
  barplot <- ggplot2::ggplot(plot_dt,
                             ggplot2::aes(x = .data[[bars]],
                                          y = .data[[value]],
                                          fill = .data[[ifelse(test = is.null(break_bars_by),
                                                               yes = bars,
                                                               no = break_bars_by)]])) +
    ggplot2::geom_bar(stat = 'identity', alpha = .95) +
    ggtheme() +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
                   plot.background =  ggplot2::element_rect(fill = "transparent", colour = NA)) +
    ggplot2::scale_y_continuous(labels = scales::number_format(accuracy = 0.01,
                                                               decimal.mark = '.',
                                                               big.mark = ','))  +
    ggplot2::scale_fill_manual(values = plot_palette)

  # axes
  if(!is.null(x_axis_label)){
    barplot <- barplot + ggplot2::xlab(x_axis_label)
  }
  if(!is.null(y_axis_label)){
    barplot <- barplot + ggplot2::ylab(y_axis_label)
  }

  if(as.logical(horizontal)){
    barplot <- barplot + ggplot2::coord_flip()
  }

  if(!static){
    barplot <- plotly::ggplotly(barplot,
                               tooltip = c('x', 'y', if(!is.null(break_bars_by)){'fill'})) # %>%
      # plotly::layout(paper_bgcolor  = "rgba(0, 0, 0, 0)",
      #                grid = '',
      #                plot_bgcolor = "rgba(0, 0, 0, 0)")
  }
  return(barplot)
}



#' Add a bar plot to a chronicle report
#'
#' @param report Character string containing all the R Markdown chunks previously added. Default is '', an empty report.
#' @param dt Table with the data for the plot.
#' @param bars Name of the columns containing the different groups.
#' @param value Name of the columns to use as values on the y axis of the plot. If NULL (default), counts will be used.
#' @param break_bars_by Name of the categorical variable used to break each bar
#' @param horizontal Plot the bars horizontally. Default is FALSE
#' @param sort_by_value Sort the bars by value. Default is FALSE
#' @param sort_decreasing Sort the values decreasingly. Default is TRUE, but sort_by_value must also be TRUE.
#' @param ggtheme ggplot2 theme function to apply. Default is ggplot2::theme_minimal.
#' @param x_axis_label Label for the x axis.
#' @param y_axis_label Label for the y axis.
#' @param plot_palette Character vector of hex codes specifying the colors to use on the plot.
#' @param plot_palette_generator Palette from the viridis package used in case plot_palette is unspecified or insufficient for the number of colors required.
#' @param barplot_title Title of the bar plot  section on the report. If NULL, chronicle will try to parse a generic title using make_title()
#' @param title_level Level of the section title of this plot (ie, number of # on Rmarkdown syntax.)
#' @param echo Whether to display the source code in the output document. Default is FALSE.
#' @param message Whether to preserve messages on rendering. Default is FALSE.
#' @param warning Whether to preserve warnings on rendering. Default is FALSE.
#' @param fig_width Width of the plot (in inches).
#' @param fig_height Height of the plot (in inches).
#'
#' @return An rmarkdown file as a character string, now containing a chunk for adding the specified bar plot.
#' @export
#'
#' @examples
#' html_report <- add_barplot(report = '',
#'                            dt = iris,
#'                            bars = 'Species',
#'                            value = 'Sepal.Length')
#' cat(html_report)
add_barplot <- function(report = '',
                        dt,
                        bars,
                        value = NULL,
                        break_bars_by = NULL,
                        horizontal = FALSE,
                        sort_by_value = FALSE,
                        sort_decreasing = TRUE,
                        ggtheme = 'minimal',
                        x_axis_label = NULL,
                        y_axis_label = NULL,
                        plot_palette = NULL,
                        plot_palette_generator = NULL,
                        barplot_title = NULL,
                        title_level = 2,
                        echo = FALSE,
                        message = FALSE,
                        warning = FALSE,
                        fig_width = NULL,
                        fig_height = NULL){

  # if a data.frame is provided, check if the specified columns are present
  if(is.data.frame(dt)){
    dt_cols <- c(bars, value, break_bars_by)
    if(any((!dt_cols %in% colnames(dt)))){
      stop(paste(setdiff(dt_cols, colnames(dt)), collapse = ', '), ' not found on dt.')
    }
  }

  params <- list(bars = bars,
                 value = value,
                 break_bars_by = break_bars_by,
                 horizontal = horizontal,
                 sort_by_value = sort_by_value,
                 sort_decreasing = sort_decreasing,
                 ggtheme = ggtheme,
                 x_axis_label = x_axis_label,
                 y_axis_label = y_axis_label,
                 plot_palette = plot_palette,
                 plot_palette_generator = plot_palette_generator) %>%
    purrr::discard(is.null)

  report <- chronicle::add_chunk(report = report,
                                 dt_expr = ifelse(test = is.character(dt),
                                                  yes = dt,
                                                  no = deparse(substitute(dt))),
                                 fun = make_barplot,
                                 params = params,
                                 chunk_title = barplot_title,
                                 title_level = title_level,
                                 echo = echo,
                                 message = message,
                                 warning = warning,
                                 fig_width = fig_width,
                                 fig_height = fig_height)
  return(report)
}
