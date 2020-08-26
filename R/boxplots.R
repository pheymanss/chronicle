

#' create a bar plot from a data frame through ggplotly
#'
#' @param dt data.frame containing the data to plot. It must have a numerical variable, a date variable, and optionally a grouping variable to split the data and plot them as individual time sieries inside the same plot.
#' @param num_var Name of the column of the data frame containing the numerical variables of the time series.
#' @param group_var Name of the columns containing the different groups.
#' @param ggtheme ggplot2 theme function to apply. Default is ggplot2::theme_minimal.
#' @param x_axis_label Label for the x axis.
#' @param y_axis_label Label for the y axis.
#' @param y_axis_limits Numeric vector of length 2 specifying the lower and upper bounds of the y axis (useful when outliers eclipse the interquartile range).
#' @param plot_palette #' @param plot_palette Character vector of hex codes specifying the colors to use on the plot.
#'
#' @export
#' @return A plotly-ized version of a ggplot box plot.
#'
#' @examples
make_boxplot <- function(dt,
                         num_var,
                         group_var = NULL,
                         ggtheme = ggplot2::theme_minimal,
                         x_axis_label = NULL,
                         y_axis_label = NULL,
                         y_axis_limits = NULL,
                         plot_palette = c("#58508d", "#ffa600", "#ff6361", "#003f5c", "#bc5090", "#A6CEE3",
                                          "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F",
                                          "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99")){

  # build boxplot
  if(is.null(group_var)){
    # add dummy variable for consistent color with plot_palette
    dt$color <- 'color_fill'

    boxplot <- ggplot2::ggplot(dt,
                               ggplot2::aes_string(y = num_var, fill = 'color')) +
      ggplot2::geom_boxplot() +
      ggtheme() +
      ggplot2::scale_y_continuous(labels = scales::number_format(accuracy = 0.01,
                                                                 decimal.mark = '.',
                                                                 big.mark = ','),
                                  limits = y_axis_limits)  +
      ggplot2::scale_fill_manual(values = plot_palette) +
      ggplot2::scale_color_manual(values = plot_palette) +
      ggplot2::theme(legend.position = 'none')
  }else{
    boxplot <- ggplot2::ggplot(dt, ggplot2::aes_string(x = group_var, y = num_var, fill = group_var)) +
      ggplot2::geom_boxplot() +
      ggtheme() +
      ggplot2::scale_y_continuous(labels = scales::number_format(accuracy = 0.01,
                                                                 decimal.mark = '.',
                                                                 big.mark = ','),
                                  limits = y_axis_limits)  +
      ggplot2::scale_fill_manual(values = plot_palette) +
      ggplot2::scale_color_manual(values = plot_palette) +
      ggplot2::theme(legend.position = 'none')
  }



  # axes
  if(!is.null(x_axis_label)){
    boxplot <- boxplot + ggplot2::xlab(x_axis_label)
  }
  if(!is.null(y_axis_label)){
    boxplot <- boxplot + ggplot2::ylab(y_axis_label)
  }

  boxplot <- plotly::ggplotly(boxplot)
  return(boxplot)
}

#' Add a ggplotly boxplot to a chronicle report
#'
#' @param report character containing the text of an Rmarkdown report header (and possibly more chunks). Easily create one with chronicle::new_report()
#' @param dt data.frame containing the data to plot. It must have a numerical variable, a date variable, and optionally a grouping variable to split the data and plot them as individual time sieries inside the same plot.
#' @param num_var Name of the column of the data frame containing the numerical variable.
#' @param group_var Name of the columns containing the different groups.
#' @param ggtheme ggplot2 theme function to apply. Default is ggplot2::theme_minimal.
#' @param boxplot_title Title of the dygraph.
#' @param title_level Level of the section title of this plot (ie, number of # on Rmarkdown syntax.)
#' @param x_axis_label Label for the x axis.
#' @param y_axis_label Label for the y axis.
#' @param y_axis_limits Numeric vector of length 2 specifying the lower and upper bounds of the y axis (useful when outliers eclipse the interquartile range).
#'
#' @return The text of the Rmarkdown report plus an additional section with the box plot.
#' @export
#' @examples
add_boxplot <- function(report = new_report(),
                        dt,
                        num_var,
                        group_var = NULL,
                        ggtheme = NULL,
                        boxplot_title = NULL,
                        title_level = 2,
                        x_axis_label = NULL,
                        y_axis_label = NULL,
                        y_axis_limits = NULL,
                        plot_palette = c("#58508d", "#ffa600", "#ff6361", "#003f5c", "#bc5090", "#A6CEE3",
                                         "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F",
                                         "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99")){
  boxplot_title <- ifelse(test = is.null(boxplot_title),
                          yes = paste0(num_var, if(!is.null(group_var)){paste(', by', group_var)}),
                          no = boxplot_title)

  report <- paste(report, # report object (Rmarkdown file text)
                  paste(paste(rep('#', title_level), collapse = ''), boxplot_title),
                  paste('```{r, echo = FALSE, message = FALSE, warning = FALSE}', # r chunk header
                        # make_boxplot
                        paste(c(paste0('chronicle::make_boxplot(dt = ', deparse(substitute(dt)), ','),
                                paste0('                        num_var = "', num_var, '",'),
                                if (!is.null(group_var)) {paste0('                        group_var = "', group_var, '",')},
                                if (!is.null(ggtheme)) {paste0('                        ggtheme = ', deparse(substitute(ggtheme)), ',')},
                                if (!is.null(x_axis_label)) {paste0('                        x_axis_label = "', x_axis_label, '",')},
                                if (!is.null(y_axis_label)) {paste0('                        y_axis_label = "', y_axis_label, '",')},
                                if (!is.null(y_axis_limits)) {paste0('                        y_axis_limits = c(',y_axis_limits[1], ', ', y_axis_limits[2] , '),')},
                                paste0('                        plot_palette = c(', paste(paste0('"', plot_palette, '"'), collapse = ', '), '))')),
                              collapse = '\n'),
                        #finish
                        '```', sep = '\n'),
                  sep = '\n\n')
  return(report)
}
