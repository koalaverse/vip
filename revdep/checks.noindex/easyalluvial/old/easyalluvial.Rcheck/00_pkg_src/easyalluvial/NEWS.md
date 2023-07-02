# Version 0.3.1 submitted to CRAN 20220704
- cleanup package dependencies

# Version 0.3.0 submitted to CRAN 20200111
- moved modeling packages and vdiffr from imports to suggests
- added parsnip and workflows wrapper

Bug Fixes:
- alluvial_wide() now works when all columns are character columns

# Version 0.2.3 submitted to CRAN
Bug Fixes:
- compatibility with `dplyr 1.0.0`

New features:
- `pkgdown` website

# Version 0.2.1 submitted to CRAN 20190917
Bug Fixes:
- compatibility with `tidyr 1.0.0`
- compatibility with `ggalluvial 0.10.0`
- alluvial_model_response() had issues with factors https://github.com/erblast/easyalluvial/issues/13 which is now fixed

# Version 0.2.0 submitted to CRAN 20190331
Minor changes:
- Default plotting colours have been improved
- for alluvial_wide() stratum fill color of variable determining flow is now matched with flow color
- `parameter_label_size` allows to adjust the label fint size, as a result label size has been slightly increased

New features:
- marginal histograms
- alluvial flavoured partial dependency and model response alluvial plots
- new datasets mtcars2, quarterly_sunspots

Bug Fixes:
- Compatibility with `recipes 0.1.5`
- NA values in numerical columns are excepted
- correct ggplot2 and ggalluvial versions added as dependencies
- p$data_key does not contain columns with empty levels anymore
- better support for numerical variables for alluvial_long()

# Version: 0.1.8, submitted to CRAN 20190115
- `dplyr 0.8.0.` compatibility
- `vdiffr` is now used to test plots and added as a suggested dependency
- `manip_bin_numerics()` accepts c('median', 'mean', 'cuts', 'min_max') as bin_labels
argument which will be converted to bin label.
- `alluvial_wide()` and `alluvial_long()` do not crash anymore when dataframes are grouped

# 20181812 0.1.7
CRAN released

# 20181118
CRAN submission


