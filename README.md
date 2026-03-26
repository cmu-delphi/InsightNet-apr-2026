# InsightNet-apr-2026

Website: [cmu-delphi.github.io/InsightNet-apr-2026](https://cmu-delphi.github.io/InsightNet-apr-2026/)

This repository contains materials for the InsightNet Workshop in April 2026.

## Overview

In this workshop, we explore Delphi's tooling for epidemiological data analysis. The project is organized as a Quarto website featuring three interactive mini-projects:

1. **Mini-Project 1**: Finding indicators and fetching data.
2. **Mini-Project 2**: EDA and correlation analysis.
3. **Mini-Project 3**: Forecasting with `epipredict`.

## Site Structure

- `index.qmd`: The workshop homepage.
- `InsightNetApr26/`: A dedicated R package for managing workshop dependencies and environment setup.
- `scripts/`: Contains the mini-project notebooks and setup scripts.
- `inputs/`: Data inputs for the activities.
- `assets/`: Website styling and images.

## Getting Started

To set up the workshop environment and install all necessary Delphi tools, you can use the specialized `{InsightNetApr26}` dependency manager:

```r
if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
pak::pkg_install("cmu-delphi/InsightNet-apr-2026/InsightNetApr26", dependencies = TRUE)
```

Loading this package will automatically verify your system setup and alert you to any missing components.

## Rendering the Website

To build the website locally:

```bash
quarto render
```

The output will be generated in the `_site/` directory.

To update the repository website:

```bash
quarto publish gh-pages
```


