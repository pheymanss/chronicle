

#' create a bar plot from a data frame through ggplotly
#'
#' @param dt data.frame containing the data to plot. It must have a numerical variable, a date variable, and optionally a grouping variable to split the data and plot them as individual time sieries inside the same plot.
#' @param group_var Name of the columns containing the different groups.
#' @param break_bars_by Name of the categorical variable used to break each bar
#' @param ggtheme ggplot2 theme function to apply. Default is ggplot2::theme_minimal.
#' @param x_axis_label Label for the x axis.
#' @param y_axis_label Label for the y axis.
#' @param plot_palette #' @param plot_palette Character vector of hex codes specifying the colors to use on the plot.

#'
#' @return a plotly-ized version of a ggplot bar plot
#'
#' @examples
make_barplot <- function(dt,
                         group_var,
                         break_bars_by = NULL,
                         ggtheme = ggplot2::theme_minimal,
                         x_axis_label = NULL,
                         y_axis_label = NULL,
                         plot_palette = c("#58508d", "#ffa600", "#ff6361", "#003f5c", "#bc5090", "#A6CEE3",
                                          "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F",
                                          "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99")){
  dt[[group_var]] %<>% as.character()

  if(is.null(break_bars_by)){
    barplot <- ggplot2::ggplot(dt, ggplot2::aes_string(x = group_var, fill = group_var)) +
      ggplot2::geom_bar() +
      ggtheme() +
      ggplot2::scale_y_continuous( labels = scales::number_format(accuracy = 0.01,
                                                                  decimal.mark = '.',
                                                                  big.mark = ','))  +
      ggplot2::scale_fill_manual(values = plot_palette) +
      ggplot2::scale_color_manual(values = plot_palette) +
      ggplot2::theme(legend.position = 'none')
  }else{
    dt[[break_bars_by]] %<>% as.character()
    barplot <- ggplot2::ggplot(dt, ggplot2::aes_string(x = group_var, fill = break_bars_by)) +
      ggplot2::geom_bar() +
      ggtheme() +
      ggplot2::scale_y_continuous( labels = scales::number_format(accuracy = 0.01,
                                                                  decimal.mark = '.',
                                                                  big.mark = ','))  +
      ggplot2::scale_fill_manual(values = plot_palette) +
      ggplot2::scale_color_manual(values = plot_palette)
  }

  # axes
  if(!is.null(x_axis_label)){
    barplot <- barplot + ggplot2::xlab(x_axis_label)
  }
  if(!is.null(y_axis_label)){
    barplot <- barplot + ggplot2::ylab(y_axis_label)
  }

  barplot <- plotly::ggplotly(barplot)
  return(barplot)
}



#' Add a ggplotly bar plot to a chronicle report
#'
#'
#' @param report character containing the text of an Rmarkdown report header (and possibly more chunks). Easily create one with chronicle::new_report()
#' @param dt data.frame containing the data to plot. It must have a numerical variable, a date variable, and optionally a grouping variable to split the data and plot them as individual time sieries inside the same plot.
#' @param group_var Name of the columns containing the different groups.
#' @param break_bars_by Name of the categorical variable used to break each bar
#' @param ggtheme ggplot2 theme function to apply. Default is ggplot2::theme_minimal.
#' @param barplot_title Title of the dygraph.
#' @param title_level Level of the section title of this plot (ie, number of # on Rmarkdown syntax.)
#' @param x_axis_label Label for the x axis.
#' @param y_axis_label Label for the y axis.
#' @param plot_palette Character vector of hex codes specifying the colors to use on the plot. Default is RColorBrewer's Paired and Spectral colors concatenated.
#'
#' @return
#' @export
#'
#' @examples
add_barplot <- function(report = new_report(),
                        dt,
                        group_var,
                        break_bars_by = NULL,
                        ggtheme = NULL,
                        barplot_title = NULL,
                        title_level = 2,
                        x_axis_label = NULL,
                        y_axis_label = NULL,
                        plot_palette = c("#58508d", "#ffa600", "#ff6361", "#003f5c", "#bc5090", "#A6CEE3",
                                         "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F",
                                         "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99")){
  barplot_title <- ifelse(test = is.null(barplot_title),
                          yes = paste0('Count of ', group_var, if(!is.null(break_bars_by)){paste(', by', break_bars_by)}),
                          no = barplot_title)

  report <- paste(report, # report object (Rmarkdown file text)
                  paste(paste(rep('#', title_level), collapse = ''), barplot_title),
                  paste('```{r, echo = FALSE, message = FALSE, warning = FALSE}', # r chunk header
                        # make_barplot
                        paste(c(paste0('chronicle::make_barplot(dt = ', deparse(substitute(dt)), ','),
                                paste0('                        group_var = "', group_var, '",'),
                                if (!is.null(break_bars_by)) {paste0('                        break_bars_by = "', break_bars_by, '",')},
                                if (!is.null(ggtheme)) {paste0('                        ggtheme = ', deparse(substitute(ggtheme)), ',')},
                                if (!is.null(x_axis_label)) {paste0('                        x_axis_label = "', y_axis_label, '",')},
                                if (!is.null(y_axis_label)) {paste0('                        y_axis_label = "', y_axis_label, '",')},
                                paste0('                        plot_palette = c(', paste(paste0('"', plot_palette, '"'), collapse = ', '), '))')),
                              collapse = '\n'),
                        #finish
                        '```', sep = '\n'),
                  sep = '\n\n')
  return(report)
}
