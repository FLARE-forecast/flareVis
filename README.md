# flareVis

Shared plots for visualizing FLARE output

## Design guide

All new functions should use the following argument convention:

- `data` is the first argument and is the data frame being plotted.  It should be already filtered to the site and reference_datetime.  It can have multiple depths. No default.

- `depths` is the second argument and is a vector of the depths to be plotted. Defaults to `0.5`

- `tzone` is the third argument and it is the time zone for the x-axis, if it is a time-series plot. Default is `"America/New_York"`

- `ylims` is the fourth argument and is a two element vector of the lower and upper range of the y-axis.  Defaults to `c(-5,35)`

- `site_name` is the full name of a site if the plot includes it in the title.  Defaults to blank `""`

- As we add more plots, we will add more conventions here

## Example

```
plot_temp_single_panel <- function(data, depths = 0.5, tzone = "America/New_York", ylims = c(-5,35), site_name = "")
```
