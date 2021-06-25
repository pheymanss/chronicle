# chronicle 0.3

## Features
- The default colors of the plots can be set on `render_report()`, either by explicitly listing the colors as a character vector on `plot_palette` or by selecting a `plot_palette_generator` from the viridis packages ('viridis', 'plasma', 'magma', 'cividis', etc). 
- Addded support for the new viridis palettes (mako, rocket and turbo)
- Bar plots now display up to N bars, and bins all other groups into an 'Other' category. This is controlled by the new parameter `up_to_n_bars` which defaults to 20. 
- Box plots have a new parameter `split_groups_by` to have a second level of disagregation for each group.
- `report_columns` defaults to horizontal bar plots for categorical variables.
- `report_columns` adds a grouped `skimr::skim` section when a column name is passed on `by_column`.
- New function `add_image()` brings image support!

## Fixes:
- Automatically switch to static plots for larger (+10,000 rows) data to avoid unusable report file sizes.
- Correctly indented the output of all functions that write code. Also swithced most from `paste()` to `glue()` to get clean prints in console.
- Cleaned code in `add_chunk`, now the parameters are dynamically enquoted depending on their types (no more 'FALSE' and '12' on the calls visible in the .Rmd).
- Automatic title generation now works for all `add_*` plot functions.
- Collapses the `text` parameter when a vector of text was passed to `add_text`.
- Hide dummy groups on ungrouped plots.
