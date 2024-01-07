
---
output: github_document
---
## Installation of the package
Use the following command to install the package from CRAN:

``` R
install.packages("agricolaeplotr")
```

# Usage of the Package

This section demonstrates the usage of the package and its underlying functions. Factorial experiments are ubiquitous in all science and technology fields, and as an example, a factorial AB-design will be used. While some parameters are specifically relevant to agriculture, most others are beneficial for every user.

## Load the Package

Use the following command to load the package after installation. The two packages below 'agricolaeplotr' are only needed for the examples.


``` R

library("agricolaeplotr")

library("ggplot2")

library("agricolae")

```

## Example: Factorial AB Design with Complete Randomization

To create a design, we first utilize the `agricolae` package. All examples provided are directly sourced from `agricolae`.

After creating the object, everything is set to plot a basic graph. It is assumed that the height and width of each plot are both set to 1. In agricultural designs, it is recommended to input the measures from a plot to estimate the dimensions needed for implementing such an experiment in the field. Knowing the required dimensions in meters or other units is crucial for machinery and experiment management.

Complete randomized designs lack a factor like blocks, requiring the user to input suitable numbers for columns and rows. The product of these numbers must be greater than the size of the experiment, allowing the program to place all plots.

The following figure illustrates the output of a factorial design with two factors. The first factor has three levels, and the second one has two. The output is a standard ggplot2 design. This implies that users can apply all operations that ggplot2 and other packages using ggplot2 functions can offer. There are no layer restrictions or overly specialized layers preventing other transformations. Additionally, users may leverage 'plotly' to create interactive visualizations of the designs. This is particularly useful for field demonstrations involving various project stakeholders such as scientists, farmers, and funding agencies.

``` R
library(agricolae) # origin of the needed design object
trt <- c(3, 2) # factorial 3x2
outdesign <- design.ab(trt, r = 3, serie = 2, design = 'crd')

head(outdesign$book, 10)

plot_design.factorial_crd(outdesign, ncols = 6, nrows = 3, width = 1, height = 1)


```
![factorial design](C:\Users\Jens Harbers\Documents\RPlot.jpeg)


## Planned Features for Future Versions

- Introduce a Shiny interface for interactive experiment layout.
- Incorporate additional custom field experiment tools.
- Enable the export of experiments to the ISOBUS standard.
- Implement the export of designs to PostgreSQL.
