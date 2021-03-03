#' Create a scatter plot from a data frame through ggplotly
#'
#' @param dt data.frame containing the data to plot.
#' @param x Value on the x axis.
#' @param y Value on the y axis.
#' @param groups Name of the column containing the different groups.
#' @param faceted If TRUE (default), each group will be plotted separately.
#' @param scales From ggplot2::facet_wrap: Should scales be 'fixed', 'free', or free in one dimension ('free_x', 'free_y'). Default is 'fixed'.
#' @param show_trend If TRUE, adds a ggplot2::geom_smooth() line to the plot.
#' @param trend_method The method ggplot2::geom_smooth will use. Default is 'loess', which is a local polynomial regression fit
#' @param ggtheme ggplot2 theme function to apply. Default is ggplot2::theme_minimal.
#' @param x_axis_label Label for the x axis.
#' @param y_axis_label Label for the y axis.
#' @param plot_palette Character vector of hex codes specifying the colors to use on the plot.
#' @param plot_palette_generator Palette from the viridis package, used in case plot_palette is unspecified or insufficient for the number of colors required.
#' @param static If TRUE, the output will be static ggplot chart instead of an interactive ggplotly chart. Default is FALSE.
#'
#' @export
#' @return A plotly-ized version of a grouped ggplot scatter plot.
#'
#' @examples
#' make_scatterplot(dt = ggplot2::mpg,
#'               x = 'hwy',
#'               y = 'cty',
#'               groups = 'manufacturer',
#'               faceted = FALSE)
#'
#' make_scatterplot(dt = ggplot2::mpg,
#'               x = 'hwy',
#'               y = 'cty',
#'               groups = 'manufacturer',
#'               faceted = TRUE,
#'               scales = 'free')
#'
#' @importFrom rlang .data
make_scatterplot <- function(dt,
                          x,
                          y,
                          groups = NULL,
                          faceted = FALSE,
                          scales = 'fixed',
                          show_trend = FALSE,
                          trend_method = 'loess',
                          ggtheme = 'minimal',
                          x_axis_label = NULL,
                          y_axis_label = NULL,
                          plot_palette = NULL,
                          plot_palette_generator = 'plasma',
                          static = FALSE){


  dt_cols <- c(x, y, groups)
  if(any((!dt_cols %in% colnames(dt)))){
    stop(paste(setdiff(dt_cols, colnames(dt)), collapse = ', '), ' not found on dt.')
  }

  # check how many colors are needed for plotting
  plot_palette_length <- ifelse(test = is.null(groups),
                                yes = 1,
                                no = data.table::uniqueN(dt[[groups]]))


  # map the gg theme to its? corresponding ggplot2::theme_ function
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
                                   viridis::plasma)

  #if not provided, use palette from viridis::plasma
  if(is.null(plot_palette)){
    plot_palette <- plot_palette_generator(plot_palette_length, begin = 0, end = .80)
  }else if(plot_palette_length > length(plot_palette)){
    warning('Insufficient palette length provided for a scatter plot of ',
            x, ',', y, if(!is.null(groups)){paste(' by', groups)},
            '. Adding the missing ', (plot_palette_length - length(plot_palette)),
            ' colors from plot_palette_generator')
    plot_palette <- c(plot_palette,
                      plot_palette_generator(plot_palette_length - length(plot_palette), begin = 0, end = .8))
  }

  # create the plot structure depending of the group
  null_groups <- is.null(groups)
  if(null_groups){
    # make a dummy group variable
    groups <- 'groups'
    dt$groups <- 'A'
  }
  scatterplot <- ggplot2::ggplot(dt,
                              ggplot2::aes(x = .data[[x]],
                                           y = .data[[y]],
                                           color = .data[[groups]])) +
    ggplot2::geom_point(alpha = .85) +
    ggtheme() +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
                   plot.background =  ggplot2::element_rect(fill = "transparent", colour = NA)) +
    ggplot2::scale_y_continuous(labels = scales::number_format(accuracy = 0.01,
                                                               decimal.mark = '.',
                                                               big.mark = ',')) +
    ggplot2::scale_color_manual(values = plot_palette)

  if(null_groups){
    scatterplot <- scatterplot + ggplot2::theme(legend.position = 'none')
  }

  if(is.numeric(dt[[x]])){
    scatterplot <- scatterplot +
      ggplot2::scale_x_continuous(labels = scales::number_format(accuracy = 0.01,
                                                                 decimal.mark = '.',
                                                                 big.mark = ','))
  }

  # axes
  if(!is.null(x_axis_label)){
    scatterplot <- scatterplot + ggplot2::xlab(x_axis_label)
  }
  if(!is.null(y_axis_label)){
    scatterplot <- scatterplot + ggplot2::ylab(y_axis_label)
  }

  # facet by groups
  if(as.logical(faceted)){
    scatterplot <- scatterplot + ggplot2::facet_wrap(stats::as.formula(paste(groups, '~ .')),
                                               scales = scales) +
      ggplot2::theme(legend.position = 'none')
  }

  # smooth trend line
  if(as.logical(show_trend)){
    scatterplot <- scatterplot + ggplot2::geom_smooth(formula = y~x, method = trend_method)
  }

  if(!static){
    scatterplot <- plotly::ggplotly(scatterplot,  tooltip = c('x', 'y', if(groups != 'groups'){'color'}))
  }

  return(scatterplot)
}

#' Add a scatter plot to a chronicle report
#'
#' @param report Character string containing all the R Markdown chunks previously added. Default is '', an empty report.
#' @param dt data.frame containing the data to plot.
#' @param x Value on the x axis.
#' @param y Value on the y axis.
#' @param groups Name of the column containing the different groups.
#' @param faceted If TRUE (default), each group will be plotted separately.
#' @param scales From ggplot2::facet_wrap: Should scales be 'fixed', 'free', or free in one dimension ('free_x', 'free_y'). Default is 'fixed'.
#' @param show_trend If TRUE, adds a ggplot2::geom_smooth() line to the plot. Default is FALSE.
#' @param trend_method The method ggplot2::geom_smooth will use. Default is 'loess', which is a local polynomial regression fit
#' @param ggtheme ggplot2 theme function to apply. Default is ggplot2::theme_minimal.
#' @param x_axis_label Label for the x axis.
#' @param y_axis_label Label for the y axis.
#' @param plot_palette Character vector of hex codes specifying the colors to use on the plot.
#' @param plot_palette_generator Palette from the viridis package, used in case plot_palette is unspecified or insufficient for the number of colors required.
#' @param scatterplot_title Title of the scatter plot  section on the report. If NULL, chronicle will try to parse a generic title using make_title()
#' @param title_level Level of the section title of this plot (ie, number of # on Rmarkdown syntax.)
#' @param echo Whether to display the source code in the output document. Default is FALSE.
#' @param message Whether to preserve messages on rendering. Default is FALSE.
#' @param warning Whether to preserve warnings on rendering. Default is FALSE.
#' @param fig_width Width of the plot (in inches).
#' @param fig_height Height of the plot (in inches).
#'
#' @return An R Markdown file as a character string, now containing a chunk for the specified scatter plot.
#' @export
#'
#' @examples
#' html_report <- add_scatterplot(report = "",
#'                             dt = ggplot2::mpg,
#'                             x = 'hwy',
#'                             y = 'cty',
#'                             groups = 'manufacturer',
#'                             faceted = FALSE)
#' cat(html_report)
add_scatterplot <- function(report = '',
                            dt,
                            x,
                            y,
                            groups = NULL,
                            faceted = NULL,
                            scales = NULL,
                            show_trend = NULL,
                            trend_method = NULL,
                            ggtheme = NULL,
                            x_axis_label = NULL,
                            y_axis_label = NULL,
                            plot_palette = NULL,
                            plot_palette_generator = NULL,
                            scatterplot_title = NULL,
                            title_level = 2,
                            echo = FALSE,
                            message = FALSE,
                            warning = FALSE,
                            fig_width = NULL,
                            fig_height = NULL){

  dt_cols <- c(x, y, groups)
  if(any((!dt_cols %in% colnames(dt)))){
    stop(paste(setdiff(dt_cols, colnames(dt)), collapse = ', '), ' not found on dt.')
  }

  params <- list(x = x,
                 y = y,
                 groups = groups,
                 faceted = faceted,
                 scales = scales,
                 show_trend = show_trend,
                 trend_method = trend_method,
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
                                 fun = make_scatterplot,
                                 params = params,
                                 chunk_title = scatterplot_title,
                                 title_level = title_level,
                                 echo = echo,
                                 message = message,
                                 warning = warning,
                                 fig_width = fig_width,
                                 fig_height = fig_height)
  return(report)
}
 make_scatterplot(dt = iris, x = 'Sepal.Length', 'Sepal.Width', groups = 'Species')
