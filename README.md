# agricolaeplotr: Visualization Tools for Experimental Designs

[![CRAN status](https://www.r-pkg.org/badges/version/agricolaeplotr)](https://CRAN.R-project.org/package=agricolaeplotr)
[![License: GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

## Overview

`agricolaeplotr` provides visualization tools for experimental designs, particularly designed for agricultural research but applicable to any field experiment. The package converts design objects from `agricolae` into customizable ggplot2 visualizations, enabling researchers to preview and communicate their experimental layouts effectively.

## Installation

Install the stable version from CRAN:

```r
install.packages("agricolaeplotr")

Or install the development version from GitHub:

# install.packages("devtools")
devtools::install_github("yourusername/agricolaeplotr")

Usage
Basic Workflow

    Generate an experimental design using agricolae
    Visualize the design with agricolaeplotr
    Customize the visualization using ggplot2 syntax
    Export for reports or interactive use

Loading the Package

library("agricolaeplotr")
library("ggplot2")    # For plot customization
library("agricolae")  # For generating experimental designs

Example: Factorial AB Design
This example demonstrates a 3×2 factorial design with complete randomization:

# Generate a 3×2 factorial design with 3 replicates
trt <- c(3, 2)  # Factor A has 3 levels, Factor B has 2 levels
outdesign <- design.ab(trt, r = 3, serie = 2, design = 'crd')

# Visualize the design
plot_design.factorial_crd(outdesign, 
                         ncols = 6, 
                         nrows = 3, 
                         width = 1, 
                         height = 1)

Factorial design visualization
Customization Examples
Since agricolaeplotr returns ggplot2 objects, you can customize them:

# Create the base plot
p <- plot_design.factorial_crd(outdesign, ncols = 6, nrows = 3, width = 1, height = 1)

# Customize colors and labels
p + 
  ggtitle("3×2 Factorial Design (Complete Randomized)") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  labs(x = "Plot Column", y = "Plot Row")

# Add interactive features with plotly
# library(plotly)
# ggplotly(p)

Other Design Types
agricolaeplotr supports multiple experimental designs:

    Complete Randomized Design (crd)
    Randomized Complete Block Design (rcbd)
    Latin Square Design (lsd)
    Split-Plot Design (spd)
    Strip-Plot Design (spd)

Key Features

    ggplot2 Integration: All plots are standard ggplot2 objects for full customization
    Interactive Visualizations: Compatible with plotly for web-based interactive displays
    Field Planning: Calculate total area requirements for field implementation
    Flexible Plot Dimensions: Specify plot sizes in real-world units (meters, feet, etc.)
    Publication-Ready: Export high-quality graphics for reports and publications

Practical Applications
For Field Experiments

    Estimate total field area requirements
    Plan machinery access and plot layout
    Communicate experimental design to stakeholders (farmers, scientists, funders)

For Teaching and Collaboration

    Visualize complex designs for students
    Create clear diagrams for grant proposals
    Share interactive designs with collaborators

Planned Features
Future versions will include:

    Interactive Shiny interface for experiment layout
    Additional field experiment tools (e.g., plot markers, boundary rows)
    ISOBUS standard export for precision agriculture equipment
    PostgreSQL database integration for design storage and management
    Support for more complex experimental designs (e.g., factorial RCBD, split-split plots)

Contributing
Contributions are welcome! Please:

    Fork the repository
    Create a feature branch (git checkout -b feature/amazing-feature)
    Commit your changes (git commit -m 'Add some amazing feature')
    Push to the branch (git push origin feature/amazing-feature)
    Open a Pull Request

License
This package is licensed under GPL-3.
Citation
To cite agricolaeplotr in publications, use:

    Harbers J (2024). agricolaeplotr: Visualization Tools for Experimental Designs. R package version 1.0.0, https://CRAN.R-project.org/package=agricolaeplotr.

A BibTeX entry for LaTeX users is:

@Manual{,
  title = {agricolaeplotr: Visualization Tools for Experimental Designs},
  author = {Jens Harbers},
  year = {2024},
  note = {R package version 1.0.0},
  url = {https://CRAN.R-project.org/package=agricolaeplotr},
}

Acknowledgments

    Built on top of the agricolae package
    Inspired by the need for better experimental design visualization in agricultural research

Developed by Jens Harbers. For support, please open an issue on GitHub.
