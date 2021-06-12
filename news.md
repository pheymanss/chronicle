# chronicle 0.3

## Features
- Plot colors can be set for all the report on render_report(), whether by explicitly listing the colors as strings on plot_palette or by selecting a plot_palette generator from the viridis packages.
- Bar plots: Shows up to N bars and bins all ohter groups into an 'Other' category. This is controlled by the new parameter 'up_to_n_bars', which defaults to 20. 
- add_boxplot and make_boxplot have a new parameter 'split_groups_by' to split each group into several box plots by another column
- report_columns defaults to horizontal bar plots for categorical variables
- report_columns adds a grouped skim call when by_column is not NULL
- new function add_image() brings image support!

Fixes:
- Automatically switch to static plots for larger (+10,000 rows) data to avoid unusably large file sizes.
- Formatted output of all functions that write code. Swithced most from paste() to glue() to also get clean prints in console.
- Cleaned code in ecnhunk, now the parameters are dynamically enquoted depending on their types (no more 'FALSE' and 'TRUE' on make_* calls visible in the .rmd
