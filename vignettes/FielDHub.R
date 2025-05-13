## ----message=FALSE, warning=FALSE---------------------------------------------
library("agricolaeplotr")
library("FielDHub")
library("ggplot2")

## ----message=FALSE, warning=FALSE---------------------------------------------
example("RCBD")
plt <- plot(rcbd2)
p <- full_control_positions(plt$field_book, "COLUMN", "ROW", factor_name = "TREATMENT", label = "TREATMENT")
p

## ----message=FALSE, warning=FALSE---------------------------------------------
latinSq1 <- latin_square(t = 4, reps = 2, plotNumber = 101, planter = "cartesian", seed = 1980)
plt <- plot(latinSq1)
p <- full_control_positions(plt$field_book, "COLUMN", "ROW", factor_name = "TREATMENT", label = "TREATMENT")
p

## ----message=FALSE, warning=FALSE---------------------------------------------
example("full_factorial")
plt <- plot(fullFact2, l = 1)
p <- full_control_positions(plt$field_book[plt$field_book$LOCATION == "Loc1",], "COLUMN", "ROW", factor_name = "FACTOR_A", label = "FACTOR_A")
p

## ----message=FALSE, warning=FALSE---------------------------------------------
example("incomplete_blocks")
plt <- plot(ibd2)
p <- full_control_positions(plt$field_book, "COLUMN", "ROW", factor_name = "TREATMENT", label = "TREATMENT")
p

## ----message=FALSE, warning=FALSE---------------------------------------------
example("diagonal_arrangement")
plt <- plot(spatAB)
p <- full_control_positions(plt$field_book, "COLUMN", "ROW", factor_name = "TREATMENT", label = "TREATMENT")
p

## ----message=FALSE, warning=FALSE---------------------------------------------
example("RCBD_augmented")
plt <- plot(ARCBD2)
p <- full_control_positions(plt$field_book[plt$field_book$LOCATION == "A",], "COLUMN", "ROW", factor_name = "TREATMENT", label = "TREATMENT") + guides(fill = "none")
p

## ----message=FALSE, warning=FALSE---------------------------------------------
example("rectangular_lattice")
plt <- plot(rectangularLattice2)
p <- full_control_positions(plt$field_book[plt$field_book$LOCATION == "LOC1",], "COLUMN", "ROW", factor_name = "TREATMENT", label = "TREATMENT")
p

## ----message=FALSE, warning=FALSE---------------------------------------------
example("strip_plot")
plt <- plot(strip2)
p <- full_control_positions(plt$field_book[plt$field_book$LOCATION == "A",], "COLUMN", "ROW", factor_name = "TRT_COMB", label = "TRT_COMB")
p

## ----message=FALSE, warning=FALSE---------------------------------------------
example("square_lattice")
plt <- plot(squareLattice2)
p <- full_control_positions(plt$field_book[plt$field_book$LOCATION == "CASSELTON",], "COLUMN", "ROW", factor_name = "TREATMENT", label = "TREATMENT")
p

## ----message=FALSE, warning=FALSE---------------------------------------------
example("split_plot")
plt <- plot(SPDExample2)
p <- full_control_positions(plt$field_book[plt$field_book$LOCATION == "A",], "COLUMN", "ROW", factor_name = "TRT_COMB", label = "TRT_COMB")
p

## ----message=FALSE, warning=FALSE---------------------------------------------
example("split_split_plot")
plt <- plot(SSPD2)
p <- full_control_positions(plt$field_book[plt$field_book$LOCATION == "A",], "COLUMN", "ROW", factor_name = "TRT_COMB", label = "TRT_COMB") + guides(fill = "none")
p

## ----message=FALSE, warning=FALSE---------------------------------------------
example("CRD")
plt <- plot(crd3)
p <- full_control_positions(plt$field_book, "COLUMN", "ROW", factor_name = "TREATMENT", label = "TREATMENT") + guides(fill = "none")
p

## ----message=FALSE, warning=FALSE---------------------------------------------
example("sparse_allocation")
plt <- plot(sparse)
p <- full_control_positions(plt$field_book[plt$field_book$LOCATION == "LOC1",], "COLUMN", "ROW", factor_name = "TREATMENT", label = "TREATMENT") + guides(fill = "none")
p

## ----message=FALSE, warning=FALSE---------------------------------------------
example("row_column")
plt <- plot(rowcold2, l = 1)
p <- full_control_positions(plt$field_book[plt$field_book$LOCATION == "1",], "COLUMN", "ROW", factor_name = "TREATMENT", label = "TREATMENT") + guides(fill = "none")
p

