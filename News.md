# Version 1.0.2

Addressed an error in how the IHS AUC was calculated. A correction factor to scale the IHS values towards a desired distribution (e.g., make it more like a logarithmic distribution) was previously not included. The correction should not affect prior uses of the package.

# Version 1.0.0

Addressed major bug which caused calculating AUC for a single set of indifference points (and no grouping factors) to fail. Made internal adjustments to dplyr functions to account for changes in tidyr expression handling; should not affect AUC calculations.

# Version 0.4.0.9000

Changed vignette file name to include package name.

# Initial release (Version 0.4.0)

No news on initial release.
