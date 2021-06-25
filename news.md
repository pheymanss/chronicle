# chronicle 0.3

## Features
- The colors of the plot can be set for all the report on `render_report()`, whether by explicitly listing the colors as strings on `plot_palette` or by selecting a `plot_palette_generator` from the viridis packages ('viridis', 'plasma', 'magma', 'cividis', etc). 
- Bar plots now display up to N bars, and bins all other groups into an 'Other' category. This is controlled by the new parameter `up_to_n_bars`, which defaults to 20. 
- Box plots have a new parameter `split_groups_by` to have a second level of disagregation for each group.
- `report_columns` defaults to horizontal bar plots for categorical variables.
- `report_columns` adds a grouped `skimr::skim` call when called with a non NULL `by_column`.
- New function `add_image()` brings image support!

## Fixes:
- Automatically switch to static plots for larger (+10,000 rows) data to avoid unusably large file sizes.
- Formatted output of all functions that write code. Also swithced most from `paste()` to `glue()` to get clean prints in console.
- Cleaned code in `add_chunk`, now the parameters are dynamically enquoted depending on their types (no more 'FALSE' and '12' on the calls visible in the .Rmd).
- Automatic title generation works for all `add_*` plot functions
- Collapses the `text` parameter when a vector of text was passed to `add_text`.
- dummy groups were shown in 
