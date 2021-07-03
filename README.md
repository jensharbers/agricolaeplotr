---
output: github_document
---
## Installation of the package
Use the following command to install the package from CRAN:

```
install.packages("agricolaeplotr")
```

# Usage of the package
This chapter shows the usage of the package and the underlying functions.
As factorial experiments are omnipresent in all science and technology fields, a
factorial ab-design will be used as an example. Although some parameters are worth for agriculture only, most other are useful for every user.

## Load the package
use the following command to load the package after installation. The two packages below agricolaeplotr are only needed for the examples.

```

library("agricolaeplotr")

library("ggplot2")

library("agricolae")

```

## Example: factorial ab design with complete randomization

First we need to create an design using agricolae package.
All examples used in the package are directly taken from agricolae.

After creation of the object, everything is ready to plot a basic plot. It is also assumed, that the height and the width of each plot is set to 1. In agricultural designs, it is recommended to input the measures from a plot to have an idea about the dimensions needed to establish such a experiment in the field.
Knowing the needed amount in meters or other units is important for machinery and management of the experiment.

Complete randomized designs do not have a factor like blocks, so the user is required to input a suitable number for columns and rows. The product of the number of rows and columns must be greater than the size of the experiment, to give the program the chance to place all plots.

Following figure shows the output of a factorial design of two factors. The first one has three levels, second one has two. The output is a standard ggplot2 design. This implies that the user can do all operations, that ggplot2 and other packages that uses ggplot2 functions, can be applied without having layer restrictions or too specialized layers that prevent other transformations. The user also might use plotly to create interactive visualizations of the designs. Useful for field demonstrations for all project stakeholders i.e. (scientists, farmers, funding agencies).

``` 
library(agricolae) # origin of the needed design object
trt<-c(3,2) # factorial 3x2
outdesign <- design.ab(trt, r=3, serie=2,design = 'crd')

head(outdesign$book,10)

plot_design.factorial_crd(outdesign,ncols=6,nrows=3, width = 1, height = 1)

```
![factorial design](C:\Users\Jens Harbers\Documents\RPlot.jpeg)



## Planed features for future versions

- match size of subplot to those to mainplots for all split plot designs
- 
