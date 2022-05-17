#' checks matrix column input
#'
#' checks if input is suitable for matrix column indication
#' @param x input to be tested
#'
#' @return error
#' @export
#' @examples
#' library(agricolaeplotr)
#' test_input_ncols(9)
test_input_ncols <- function(x) {
  if ((length(x)) > 1) {
    stop("The length of one integer value needs to be exact 1.")
  }
  if (is.numeric(x) == FALSE) {
    stop(paste0("Value is not an integer. It is from class ",
                class(x), "."))
  } else {
    if (round(x) != x) {
      stop("It is numeric value, but not an integer")
    }
    if (x <= 0) {
      stop("Integer value is smaller or equal than zero.
           The value needs to be greater than zero.")
    }
  }
}

#' checks matrix rows input
#'
#' checks if input is suitable for matrix row indication
#' @param x input to be tested
#'
#' @return error
#' @export
#' @examples
#' library(agricolaeplotr)
#' test_input_nrows(10)
test_input_nrows <- function(x) {
  if ((length(x)) > 1) {
    stop("The length of one integer value needs to be exact 1.")
  }
  if (is.numeric(x) == FALSE) {
    stop(paste0("Value is not an integer. It is from class ",
                class(x), "."))
  } else {
    if (round(x) != x) {
      stop("It is numeric value, but not an integer")
    }
    if (x <= 0) {
      stop("Integer value is smaller or equal than zero.
           The value needs to be greater than zero.")
    }
  }
}


#' Test if input for width and height is numeric
#'
#' Test if input is numeric for field width and height
#' @param x input to be tested
#'
#' @return error
#' @export
#' @examples
#' library(agricolaeplotr)
#' test_input_extend(3)
test_input_extend <- function(x) {

  if (is.numeric(x) == FALSE) {
    stop(paste0("Value is not numeric. It is from class ",
                class(x), "."))
  } else {
    if (x <= 0) {
      stop("Integer value is smaller or equal than zero.
           The value needs to be greater than zero.")
    }
  }
}


#' Test if input for shift parameter is numeric
#'
#' Test if input is numeric for shift parameter
#' @param x input to be tested
#' @export
#' @return error
#' @examples
#' library(agricolaeplotr)
#' test_input_shift(0.5)
test_input_shift <- function(x) {

  if (is.numeric(x) == FALSE) {
    stop(paste0("Value is not numeric. It is from class ",
                class(x), "."))
  }
}


#' Test if input is a logical
#'
#' Test if input is a logical
#' @param x input to be tested
#'
#' @return error
#' @export
#' @examples
#' library(agricolaeplotr)
#' test_input_reverse(TRUE)
test_input_reverse <- function(x) {
  if ((length(x)) > 1) {
    stop("The length of one boolean value needs to be exact 1.")
  }
  if (is.logical(x) == FALSE) {
    stop(paste0("Value is not logical. Only TRUE and FALSE are allowed.
                Your value is from class ",
                class(x), "."))
  }
}

#' Test if input is a string
#'
#' Test if input is a string
#' @param x input to be tested
#'
#' @return error
#' @export
#' @examples
#' library(agricolaeplotr)
#' test_string('smallstring')
test_string <- function(x) {
  if (is.character(x) == FALSE) {
    stop(paste0("Input parameter needs to be a string. ",
                "Your value is from class ", class(x),
                "."))
  }
}


#' Test if input column names
#'
#' Test if input is in column names of a table
#' @param design design from \code{agricolae} package
#' @param x string input
#'
#' @return error
#' @export
#' @examples
#' library(agricolaeplotr)
#' library(agricolae)

#' trt<-c(2,4)
#' k=6
#' outdesign<-design.ab(trt, r=k, serie=3,design='rcbd')
#' test_name_in_column('B',outdesign)
test_name_in_column <- function(x, design) {
  test_string(x)
  if (x %in% colnames(design$book) == FALSE) {
    stop(paste0("Your string is not one of the column
                names the table of the design."))
  }
}


#' Test of experimental design
#'
#' Test if the outdesign file contains book and parameter list
#' @param design design from \code{agricolae} package
#'
#' @return error
#' @export
#' @examples
#' library(agricolaeplotr)
#' library(agricolae)

#' trt<-c(2,4)
#' k=6
#' outdesign<-design.ab(trt, r=k, serie=3,design='rcbd')
#' test_names_design(outdesign)
test_names_design <- function(design) {
  if (sum(c("parameters", "book") %in% names(design)) <
      2) {
    stop("The function expects a list of at least two following names:
         parameters , book ")

  }
}

#### plot factorial_rcbd ####
#' Plot Factorial Designs with rcbd Design
#'
#' Plot a design of a factorial  experiment with randomized complete block design (rcbd) from design.ab
#' @param design outdesign from \code{agricolae} package
#' @param y Describes the y coordinates of a experiment design
#' @param factor_name Which factor should be used for plotting, needs to be a column in outdesign$book
#' @param width numeric value, describes the width of a plot in an experiment
#' @param height numeric value, describes the height of a plot in an experiment
#' @param space_width numeric value, describes the share of the space of the plots. 0=only space, 1=no space between plots in term of width
#' @param space_height numeric value, describes the share of the space of the plots. 0=only space, 1=no space between plots in term of height
#' @param reverse_y boolean, should the plots of the experiment be changed in reverse order in Row direction? use reverse_y=TRUE to have same sketch as in agricolae. default:reverse_y=FALSE
#' @param reverse_x boolean, should the plots of the experiment be changed in reverse order in column direction? default:reverse_x=FALSE
#'
#' @return \code{ggplot} graphic that can be modified, if wished
#' @export
#' @import ggplot2
#' @import agricolae
#' @examples
#' library(agricolaeplotr)
#' library(agricolae)

#' trt<-c(2,4)
#' k=6
#' outdesign<-design.ab(trt, r=k, serie=3,design='rcbd')
#' plot_design.factorial_rcbd(design=outdesign,factor_name = 'B')
#' plot_design.factorial_rcbd(outdesign,reverse_y = TRUE,reverse_x = TRUE)
#'
plot_design.factorial_rcbd <- function(design,
                                       y = "row",
                                       factor_name = "A",
                                       width = 1,
                                       height = 1,
                                       space_width = 0.95,
                                       space_height = 0.85,
                                       reverse_x = FALSE,
                                       reverse_y = FALSE){
  if (design$parameters$design == "factorial" &&
      design$parameters$applied == "rcbd") {

    test_string(y)

    test_input_reverse(reverse_x)
    test_input_reverse(reverse_y)

    test_input_extend(height)
    test_input_extend(width)

    test_input_extend(space_height)
    test_input_extend(space_width)

    test_name_in_column(factor_name, design)
    plots <- as.numeric(design$book[, 1])
    mat <- matrix(plots, byrow = TRUE,
                  ncol = max(as.numeric(design$book$block)))
    dims <- dim(mat)
    table <- as.table(mat)
    rownames(table) <- 1:dims[1]
    colnames(table) <- 1:dims[2]
    table <- as.data.frame(table)
    colnames(table) <- c("row", "col", "plots")
    table[, y] <- as.numeric(table[, y])
    table$col <- as.numeric(table$col)

    table <- merge(table, design$book, by.x = "plots",
                   by.y = "plots")

    table$col <- rep(1:(length(table$plots)/max(as.numeric(table[, y]))),
                        times = max(as.numeric(table[, y])))

    # no space, 0 == only space no space, 0
    # == only space

    table$col <- table$col * width
    table$row <- as.numeric(table[, y]) * height
    if (reverse_y == TRUE) {
      table$row <- abs(table$row - max(table$row)) +
        min(table$row)
    }

    if (reverse_x == TRUE) {
      table$col <- abs(table$col - max(table$col)) +
        min(table$col)
    }

    plt <- ggplot(table, aes_string(x = "col", y = y)) +
      geom_tile(aes_string(fill = factor_name),
                width = width * space_width, height = height *
                  space_height) + theme_bw() + theme(line = element_blank()) +
      geom_text(aes_string(label = "plots"),
                colour = "black")

    return(plt)

  } else {
    stop(paste0("This is not the correct function for your experiment design.
          A design from a factorial design with a
                random complete block design (rcbd) is needed here.",
                "you have a design of type ", design$parameters$design,
                " and an applied type of ", design$parameters$applied,
                "."))
  }
}

#### plot design crd ####
#' Plot Complete Randomized Design
#'
#' Plot a design of a factorial experiment with randomized complete block design from \code{agricolae} design.ab
#' @param design outdesign from \code{agricolae} package
#' @param ncols integer value, choose the number of columns to which the experiment should be plotted
#' @param nrows integer value, choose the number of rows to which the experiment should be plotted
#' @param y Describes the y coordinates of a experiment design, default is row
#' @param factor_name Which factor should be used for plotting, needs to be a column in outdesign$book
#' @param labels Describes the column from that the plots are taken to display them
#' @param width numeric value, describes the width of a plot in an experiment
#' @param height numeric value, describes the height of a plot in an experiment
#' @param space_width numeric value, describes the share of the space of the plots. 0=only space, 1=no space between plots in term of width
#' @param space_height numeric value, describes the share of the space of the plots. 0=only space, 1=no space between plots in term of height
#' @param reverse_y boolean, should the plots of the experiment be changed in reverse order in Row direction? use reverse_y=TRUE to have same sketch as in agricolae. default:reverse_y=FALSE
#' @param reverse_x boolean, should the plots of the experiment be changed in reverse order in column direction? default:reverse_x=FALSE
#'
#' @return \code{ggplot} graphic that can be modified, if wished
#' @export
#'
#' @examples
#' library(agricolaeplotr)
#' library(agricolae)

#' trt = c(2,3,4,5,6)
#' outdesign1 <-design.crd(trt,r=5,serie=2,2543,'Mersenne-Twister')
#' plot_design_crd(outdesign1,ncols = 13,nrows = 3)

plot_design_crd <- function(design,
                            ncols,
                            nrows,
                            y = "row",
                            factor_name = "trt",
                            labels = "plots",
                            width = 1,
                            height = 1,
                            space_width = 0.95,
                            space_height = 0.85,
                            reverse_y = FALSE,
                            reverse_x = FALSE) {
  if (design$parameters$design == "crd") {

    plots <- as.numeric(design$book[, 1])
    if (ncols * nrows >= length(plots)) {
    nas <- rep(NaN, ncols * nrows - length(plots))
    plots <- c(plots, nas)

    mat <- matrix(plots, byrow = TRUE, ncol = ncols)
    dims <- dim(mat)
    table <- as.table(mat)
    rownames(table) <- 1:dims[1]
    colnames(table) <- 1:dims[2]
    table <- as.data.frame(table)
    colnames(table) <- c("row", "col", "plots")
    table$row <- as.numeric(table$row)
    table$col <- as.numeric(table$col)

    table <- merge(table, design$book, by.x = "plots",
                   by.y = "plots")

    table$row <- as.numeric(table$row) * height
    table$col <- as.numeric(table$col) *
      width

    if (reverse_y == TRUE) {
      table$row <- abs(table$row - max(table$row)) +
        min(table$row)
    }
    if (reverse_x == TRUE) {
      table$col <- abs(table$col - max(table$col)) +
        min(table$col)
    }
    plt <- ggplot(table, aes_string(x = "col", y)) +
      geom_tile(aes_string(fill = factor_name),
                width = width * space_width, height = height *
                  space_height) + theme_bw() + theme(line = element_blank()) +
      geom_text(aes_string(label = labels), colour = "black")

    plt

    return(plt)
    } else {
      stop(paste("You have in multiplication of ncols:",
                 ncols, "and nrows:", nrows, "Elements.",
                 "You need at minimum a product of both higher than",
                 length(plots)))
    }
  } else {
    stop(paste0("This is not the correct function for your experiment design.
          A design from a factorial design with a
                complete random design (crd) is needed here.",
                "you have a design of type ", design$parameters$design,
                " and an applied type of ", design$parameters$applied,
                "."))
  }
}


#### plot alpha design ####
#' Plot Alpha design Experiments
#'
#' Plot a design of an experiment with an alpha design from \code{agricolae} design.alpha
#'
#' @param design outdesign from \code{agricolae} package
#' @param x Describes the x coordinates of a experiment design
#' @param y Describes the y coordinates of a experiment design
#' @param factor_name Which factor should be used for plotting, needs to be a column in outdesign$book
#' @param labels Describes the column from that the plots are taken to display them
#' @param width numeric value, describes the width of a plot in an experiment
#' @param height numeric value, describes the height of a plot in an experiment
#' @param space_width numeric value, describes the share of the space of the plots. 0=only space, 1=no space between plots in term of width
#' @param space_height numeric value, describes the share of the space of the plots. 0=only space, 1=no space between plots in term of height
#' @param reverse_y boolean, should the plots of the experiment be changed in reverse order in Row direction? use reverse_y=TRUE to have same sketch as in agricolae. default:reverse_y=FALSE
#' @param reverse_x boolean, should the plots of the experiment be changed in reverse order in column direction? default:reverse_x=FALSE
#'
#' @return \code{ggplot} graphic that can be modified, if wished
#' @export
#'
#' @examples
#' library(agricolaeplotr)
#' library(agricolae)
#' trt<-1:30
#' t <- length(trt)
#' # size block k
#' k<-3
#' # Blocks s
#' s<-t/k
#' # replications r
#' r <- 2
#' outdesign<- design.alpha(trt,k,r,serie=2)
#' plot_alpha(outdesign)
plot_alpha <- function(design, x = "cols", y = "block",
                       factor_name = "trt", labels = "plots", width = 1,
                       height = 1, space_width = 0.95,
                       space_height = 0.85,
                       reverse_y = FALSE, reverse_x = FALSE) {
  if (design$parameters$design == "alpha") {
    test_string(x)
    test_string(y)

    test_input_reverse(reverse_x)
    test_input_reverse(reverse_y)

    test_input_extend(height)
    test_input_extend(width)

    test_input_extend(space_height)
    test_input_extend(space_width)

    test_name_in_column(labels, design)
    test_name_in_column(factor_name, design)

    table <- as.data.frame(design$book)
    table[, y] <- as.numeric(table[, y]) * height
    table[, x]  <- as.numeric(table[, x] ) * width


    if (reverse_x == TRUE) {
      table[, x]  <- abs(table[, x]  - max(table[, x] )) +
        min(table[, x] )
    }

    if (reverse_y == TRUE) {
      table[, y] <- abs(table[, y] - max(table[, y])) +
        min(table[, y])
    }
    plt <- ggplot(table, aes_string(x, y)) +
      geom_tile(aes_string(fill = factor_name),
                width = width * space_width, height = height *
                  space_height) + theme_bw() + theme(line = element_blank())+
      geom_text(aes_string(label = labels), colour = "black")

    plt

    return(plt)

  } else {
    stop(paste0("This is not the correct function for your experiment design.
          A design from an alpha design is needed here.",
                "you have a design of type ", design$parameters$design,
                " and an applied type of ", design$parameters$applied,
                "."))
  }

}



####### plot lattice design triple######

#' Plot Triple Lattice Design
#'
#' Plot a design of a factorial  experiment with a latin square design from \code{agricolae} design.lattice with r=3
#' @param design outdesign from \code{agricolae} package
#' @param y Describes the y coordinates of a experiment design
#' @param factor_name Which factor should be used for plotting, needs to be a column in outdesign$book
#' @param labels Describes the column from that the plots are taken to display them
#' @param width numeric value, describes the width of a plot in an experiment
#' @param height numeric value, describes the height of a plot in an experiment
#' @param space_width numeric value, describes the share of the space of the plots. 0=only space, 1=no space between plots in term of width
#' @param space_height numeric value, describes the share of the space of the plots. 0=only space, 1=no space between plots in term of height
#' @param reverse_y boolean, should the plots of the experiment be changed in reverse order in Row direction? use reverse_y=TRUE to have same sketch as in agricolae. default:reverse_y=FALSE
#' @param reverse_x boolean, should the plots of the experiment be changed in reverse order in column direction? default:reverse_x=FALSE
#'
#' @return \code{ggplot} graphic that can be modified, if wished
#' @export
#'
#' @examples
#' library(agricolaeplotr)
#' library(agricolae)

#' trt<-LETTERS[1:9]
#' outdesign<-design.lattice(trt,r=3,serie=2)
#' plot_lattice_triple(design=outdesign,reverse_x=TRUE)
#'
plot_lattice_triple <- function(design,
                                y = "block", factor_name = "trt",
                                labels = "plots",
                                width = 1,
                                height = 1,
                                space_width = 0.95,
                                space_height = 0.85,
                                reverse_y = FALSE,
                                reverse_x = FALSE) {
  if (design$parameters$design == "lattice" && design$parameters$type ==
      "triple") {


    test_string(y)

    test_input_reverse(reverse_x)
    test_input_reverse(reverse_y)

    test_input_extend(height)
    test_input_extend(width)

    test_input_extend(space_height)
    test_input_extend(space_width)

    test_name_in_column(factor_name, design)
    test_name_in_column(labels, design)

    table <- design$book
    max_b <- max(table(table[, y]))

    table$part <- rep(1:max_b,
                      times = length(table[, y])/max_b)
    table[, y] <- as.numeric(table[, y]) * height
    table$part <- table$part * width

    if (reverse_y == TRUE) {
      table$part <- abs(table$part - max(table$part)) +
        min(table$part)
    }

    if (reverse_x == TRUE) {
      table[, y] <- abs(table[, y] - max(table[, y])) +
        min(table[, y])
    }

    plt <- ggplot(table, aes_string(x = "part", y)) +
      geom_tile(aes_string(fill = factor_name),
                width = width * space_width, height = height *
                  space_height) + theme_bw() + theme(line = element_blank())+
      geom_text(aes_string(label = labels), colour = "black")

    plt

    return(plt)

  } else {
    stop(paste0("This is not the correct function for your experiment design.
          A design from an lattice design is needed here.",
                "you have a design of type ", design$parameters$design,
                " and a type of ", design$parameters$type,
                "."))
  }

}



####### plot lattice design simple #####
#' Plot Simple Lattice Design
#'
#' Plot a design of a factorial  experiment with a lattice design from \code{agricolae} design.lattice with r=2
#' @param design outdesign from \code{agricolae} package
#' @param y Describes the y coordinates of a experiment design
#' @param factor_name Which factor should be used for plotting, needs to be a column in outdesign$book
#' @param labels Describes the column from that the plots are taken to display them
#' @param width numeric value, describes the width of a plot in an experiment
#' @param height numeric value, describes the height of a plot in an experiment
#' @param space_width numeric value, describes the share of the space of the plots. 0=only space, 1=no space between plots in term of width
#' @param space_height numeric value, describes the share of the space of the plots. 0=only space, 1=no space between plots in term of height
#' @param reverse_y boolean, should the plots of the experiment be changed in reverse order in Row direction? use reverse_y=TRUE to have same sketch as in agricolae. default:reverse_y=FALSE
#' @param reverse_x boolean, should the plots of the experiment be changed in reverse order in column direction? default:reverse_x=FALSE
#'
#' @return \code{ggplot} graphic that can be modified, if wished
#' @export
#'
#' @examples
#' library(agricolaeplotr)
#' library(agricolae)

#' trt<-1:100
#' outdesign<-design.lattice(trt,r=2,serie=3) # simple lattice design, 10x10
#' plot_lattice_simple(outdesign,width = 2, height = 1)

plot_lattice_simple <- function(design,
                                y = "block",
                                factor_name = "trt",
                                labels = "plots",
                                width = 1,
                                height = 1,
                                space_width = 0.95,
                                space_height = 0.85,
                                reverse_y = FALSE,
                                reverse_x = FALSE) {
  if (design$parameters$design == "lattice" && design$parameters$type ==
      "simple") {


    test_string(y)

    test_input_reverse(reverse_x)
    test_input_reverse(reverse_y)

    test_input_extend(height)
    test_input_extend(width)

    test_input_extend(space_height)
    test_input_extend(space_width)

    test_name_in_column(factor_name, design)
    test_name_in_column(labels, design)

    table <- design$book
    table[, y] <- as.numeric(table[, y])
    table$part <- rep(1:sqrt(length(unique(table[, factor_name]))),
                      times = max(table[, y]))
    table[, y] <- table[, y] * height
    table$part <- table$part * width
    if (reverse_y == TRUE) {
      table$part <- abs(table$part - max(table$part)) +
        min(table$part)
    }

    if (reverse_x == TRUE) {
      table[, y] <- abs(table[, y] - max(table[, y])) +
        min(table[, y])
    }

    plt <- ggplot(table, aes_string(x = "part", y)) +
      geom_tile(aes_string(fill = factor_name),
          width = width * space_width, height = height *
          space_height) +
      theme_bw() + theme(line = element_blank()) +
      geom_text(aes_string(label = labels), colour = "black")

    plt

    return(plt)

  } else {
    stop(paste0("This is not the correct function for your experiment design.
          A design from an lattice design is needed here.",
                "you have a design of type ", design$parameters$design,
                " and an applied type of ", design$parameters$applied,
                "."))
  }

}



######## latin square design ######

#' Plot Latin Square Design
#'
#' Plot a design of a factorial  experiment with a latin square design from \code{agricolae} design.lsd
#' @param design outdesign from \code{agricolae} package
#' @param x Describes the x coordinates of a experiment design
#' @param y Describes the y coordinates of a experiment design
#' @param factor_name Which factor should be used for plotting, needs to be a column in outdesign$book
#' @param labels Describes the column from that the plots are taken to display them
#' @param width numeric value, describes the width of a plot in an experiment
#' @param height numeric value, describes the height of a plot in an experiment
#' @param space_width numeric value, describes the share of the space of the plots. 0=only space, 1=no space between plots in term of width
#' @param space_height numeric value, describes the share of the space of the plots. 0=only space, 1=no space between plots in term of height
#' @param reverse_y boolean, should the plots of the experiment be changed in reverse order in Row direction? use reverse_y=TRUE to have same sketch as in agricolae. default:reverse_y=FALSE
#' @param reverse_x boolean, should the plots of the experiment be changed in reverse order in column direction? default:reverse_x=FALSE
#'
#' @return \code{ggplot} graphic that can be modified, if wished
#' @export
#'
#' @examples
#' library(agricolaeplotr)
#' library(agricolae)

#' trt<-LETTERS[1:9]
#' outdesign<- design.lsd(trt,serie=2)
#' plot_latin_square(outdesign, reverse_y = TRUE)

plot_latin_square <- function(design,
                              x = "col",
                              y = "row",
                              factor_name = "trt",
                              labels = "plots",
                              width = 1,
                              height = 1,
                              space_width = 0.95,
                              space_height = 0.85,
                              reverse_y = FALSE,
                              reverse_x = FALSE) {
  if (design$parameters$design == "lsd") {

    test_string(x)
    test_string(y)

    test_input_reverse(reverse_x)
    test_input_reverse(reverse_y)

    test_input_extend(height)
    test_input_extend(width)

    test_input_extend(space_height)
    test_input_extend(space_width)

    test_name_in_column(factor_name, design)
    test_name_in_column(labels, design)
    table <- as.data.frame(design$book)
    table[, x]  <- as.numeric(table[, x] ) * width
    table[, y] <- as.numeric(table[, y]) * height
    if (reverse_y == TRUE) {
      table[, y] <- abs(table[, y] - max(table[, y])) +
        min(table[, y])
    }
    if (reverse_x == TRUE) {
      table[, x]  <- abs(table[, x]  - max(table[, x] )) +
        min(table[, x] )
    }
    plt <- ggplot(table, aes_string(x = x, y = y)) +
      geom_tile(aes_string(fill = factor_name),
                width = width * space_width, height = height *
                  space_height) + theme_bw() + theme(line = element_blank()) +
      geom_text(aes_string(label = labels), colour = "black")

    plt

    return(plt)

  } else {
    stop(paste0("This is not the correct function for your experiment design.
          A design from an latin_square design is needed here.",
                "you have a design of type ", design$parameters$design,
                " and an applied type of ", design$parameters$applied,
                "."))
  }

}
#### graeco latin square #######

#' Plot Graeco Latin Square Design
#'
#' Plot a design of an experiment with an Graeco - latin square design from \code{agricolae} design.graeco
#' @param design outdesign from \code{agricolae} package
#' @param x Describes the x coordinates of a experiment design
#' @param y Describes the y coordinates of a experiment design
#' @param factor_name Which factor should be used for plotting, needs to be a column in outdesign$book
#' @param labels Describes the column from that the plots are taken to display them
#' @param width numeric value, describes the width of a plot in an experiment
#' @param height numeric value, describes the height of a plot in an experiment
#' @param space_width numeric value, describes the share of the space of the plots. 0=only space, 1=no space between plots in term of width
#' @param space_height numeric value, describes the share of the space of the plots. 0=only space, 1=no space between plots in term of height
#' @param reverse_y boolean, should the plots of the experiment be changed in reverse order in Row direction? use reverse_y=TRUE to have same sketch as in agricolae. default:reverse_y=FALSE
#' @param reverse_x boolean, should the plots of the experiment be changed in reverse order in column direction? default:reverse_x=FALSE
#'
#' @return \code{ggplot} graphic that can be modified, if wished
#' @export
#'
#' @examples
#' library(agricolaeplotr)
#' library(agricolae)
#' T1<-c('a','b','c','d')
#' T2<-c('v','w','x','y','z','zz')
#' outdesign <- design.graeco(trt1=T1, trt2=T2, serie = 2,
#'  seed = 0, kinds = 'Super-Duper',randomization=TRUE)
#' plot_graeco(outdesign, factor_name = 'T2',reverse_y = TRUE)
#' plot_graeco(outdesign, factor_name = 'T2',reverse_x = TRUE)

plot_graeco <- function(design,
                        x = "col",
                        y = "row",
                        factor_name = "T1",
                        labels = "plots",
                        width = 1,
                        height = 1,
                        space_width = 0.95,
                        space_height = 0.85,
                        reverse_y = FALSE,
                        reverse_x = FALSE) {
  if (design$parameters$design == "graeco") {


    test_string(x)
    test_string(y)

    test_input_reverse(reverse_x)
    test_input_reverse(reverse_y)

    test_input_extend(height)
    test_input_extend(width)

    test_input_extend(space_height)
    test_input_extend(space_width)

    test_name_in_column(factor_name, design)
    test_name_in_column(labels, design)

    table <- as.data.frame(design$book)
    table[, x]  <- as.numeric(table[, x] ) * width
    table[, y] <- as.numeric(table[, y]) * height


    if (reverse_y == TRUE) {
      table[, y] <- abs(table[, y] - max(table[, y])) +
        min(table[, y])
    }
    if (reverse_x == TRUE) {
      table[, x]  <- abs(table[, x]  - max(table[, x] )) +
        min(table[, x] )
    }
    plt <- ggplot(table, aes_string(x, y)) +
      geom_tile(aes_string(fill = factor_name),
                width = width * space_width,
                height = height *space_height) +
      theme_bw() + theme(line = element_blank()) +
      geom_text(aes_string(label = labels), colour = "black")


    return(plt)

  } else {
    stop(paste0("This is not the correct function for your experiment design.
          A design from an graeco design is needed here.",
                "you have a design of type ", design$parameters$design,
                " and an applied type of ", design$parameters$applied,
                "."))
  }

}



#### plot strip design ####
#' Plot Strip Design
#'
#' Plot a design of an experiment with an Strip Plot design from \code{agricolae} design.strip
#' @param design outdesign from \code{agricolae} package
#' @param x Describes the x coordinates of a experiment design
#' @param y Describes the y coordinates of a experiment design
#' @param factor_name_1 Which factor should be used for plotting, needs to be a column in outdesign$book
#' @param factor_name_2 Which factor should be used for plotting, needs to be a column in outdesign$book
#' @param labels Describes the column from that the plots are taken to display them
#' @param width numeric value, describes the width of a plot in an experiment
#' @param height numeric value, describes the height of a plot in an experiment
#' @param space_width numeric value, describes the share of the space of the plots. 0=only space, 1=no space between plots in term of width
#' @param space_height numeric value, describes the share of the space of the plots. 0=only space, 1=no space between plots in term of height
#' @param reverse_y boolean, should the plots of the experiment be changed in reverse order in Row direction? use reverse_y=TRUE to have same sketch as in agricolae. default:reverse_y=FALSE
#' @param reverse_x boolean, should the plots of the experiment be changed in reverse order in column direction? default:reverse_x=FALSE
#'
#' @return \code{ggplot} graphic that can be modified, if wished
#' @export
#'
#' @examples
#' library(agricolaeplotr)
#' library(agricolae)
#' T1<-c('a','b','c','d')
#' T2<-c('v','w','x','y','z')
#' r = 3
#' outdesign <- design.strip(trt1=T1, trt2=T2, r=r,serie = 2,
#'  seed = 0, kinds = 'Super-Duper',randomization=TRUE)
#' plot_strip(outdesign,factor_name_1 = "T1",factor_name_2="T2")
#' plot_strip(outdesign,factor_name_1 = "T1",factor_name_2="T2",reverse_x = TRUE)
plot_strip <- function(design,
                       x = "col",
                       y = "row",
                       factor_name_1 = "T1",
                       factor_name_2 = "T2",
                       labels = "plots",
                       width = 1,
                       height = 1,
                       space_width = 0.95,
                       space_height = 0.85,
                       reverse_y = FALSE,
                       reverse_x = FALSE) {
  if (design$parameters$design == "strip") {


    test_string(x)
    test_string(y)

    test_input_reverse(reverse_x)
    test_input_reverse(reverse_y)

    test_input_extend(height)
    test_input_extend(width)

    test_input_extend(space_height)
    test_input_extend(space_width)

    test_name_in_column(factor_name_1, design)
    test_name_in_column(factor_name_2, design)
    test_name_in_column(labels, design)

    table <- as.data.frame(design$book)
     #########
    # if blocks are horizontal:
    table[, x] <- as.numeric(table[, factor_name_1]) +
      (max(as.numeric(table[, factor_name_1]))*as.numeric(table$block)-1) -
      (max(as.numeric(table[, factor_name_1])))+1

    table[, x]  <- table[, x] * width
    table[, y] <- as.numeric(table[, factor_name_2]) * height
        #########

  # if block are not horizontal
  # table[, y]  <- as.numeric(table[, factor_name_2]) +
  #    (max(as.numeric(table[, factor_name_2]))*as.numeric(table$block)-1) -
  #    (max(as.numeric(table[, factor_name_2])))+1
  # table[, y] <- table[,y] * height
  # table[, x] <- as.numeric(table[, factor_name_1]) * width




    if (reverse_y == TRUE) {
      table[, y] <- abs(table[, y] - max(table[, y])) +
        min(table[, y])
    }
    if (reverse_x == TRUE) {
      table[, x]  <- abs(table[, x]  - max(table[, x] )) +
        min(table[, x] )
    }
    plt <- ggplot(table, aes_string(x = x, y = y)) +
      geom_tile(aes_string(fill = factor_name_1),
                width = width * space_width, height = height *
                  space_height) + theme_bw() + theme(line = element_blank()) +
      geom_text(aes_string(label = labels), colour = "black")


    return(plt)

  } else {
    stop(paste0("This is not the correct function for your experiment design.
          A design from an strip design is needed here.",
                "you have a design of type ", design$parameters$design,
                " and an applied type of ", design$parameters$applied,
                "."))
  }

}




#### plot bib design #######

#' Plot Randomized Balanced Incomplete Block Designs
#'
#' Plot a design of an experiment with an Randomized Balanced Incomplete Block Designs (BIB) from design.bib
#' @param design outdesign from \code{agricolae} package
#' @param y Describes the y coordinates of a experiment design
#' @param factor_name Which factor should be used for plotting, needs to be a column in outdesign$book
#' @param labels Describes the column from that the plots are taken to display them
#' @param width numeric value, describes the width of a plot in an experiment
#' @param height numeric value, describes the height of a plot in an experiment
#' @param space_width numeric value, describes the share of the space of the plots. 0=only space, 1=no space between plots in term of width
#' @param space_height numeric value, describes the share of the space of the plots. 0=only space, 1=no space between plots in term of height
#' @param reverse_y boolean, should the plots of the experiment be changed in reverse order in Row direction? use reverse_y=TRUE to have same sketch as in agricolae. default:reverse_y=FALSE
#' @param reverse_x boolean, should the plots of the experiment be changed in reverse order in column direction? default:reverse_x=FALSE
#'
#' @return \code{ggplot} graphic that can be modified, if wished
#' @export
#'
#' @examples
#' library(agricolaeplotr)
#' library(agricolae)
#' trt<-c('A','B','C','D')
#' k<-3
#' outdesign<-design.bib(trt,k,serie=2,seed =41,kinds ='Super-Duper') # seed = 41
#' plot_bib(outdesign)
#' #now let us change position of the columns
#' plot_bib(outdesign,reverse_x = TRUE)
plot_bib <- function(design,
                     y = "block",
                     factor_name = "trt",
                     labels = "plots",
                     width = 1,
                     height = 1,
                     space_width = 0.95,
                     space_height = 0.85,
                     reverse_y = FALSE,
                     reverse_x = FALSE) {
  if (design$parameters$design == "bib") {



    test_string(y)

    test_input_reverse(reverse_x)
    test_input_reverse(reverse_y)

    test_input_extend(height)
    test_input_extend(width)

    test_input_extend(space_height)
    test_input_extend(space_width)

    test_name_in_column(factor_name, design)
    test_name_in_column(labels, design)

    table <- as.data.frame(design$book)
    table$col <- rep(1:max(table(table[, y])),
                     times = max(as.numeric(table[, y])))
    table$col <- table$col * width
    table[, y] <- as.numeric(table[, y]) * height
    if (reverse_y == TRUE) {
      table[, y] <- abs(table[, y] - max(table[, y])) +
        min(table[, y])
    }
    if (reverse_x == TRUE) {
      table$col <- abs(table$col - max(table$col)) +
        min(table$col)
    }
    plt <- ggplot(table, aes_string(x = "col", y = y)) +
      geom_tile(aes_string(fill = factor_name),
                width = width * space_width, height = height *
                  space_height) + theme_bw() + theme(line = element_blank()) +
      geom_text(aes_string(label = labels), colour = "black")


    return(plt)

  } else {
    stop(paste0("This is not the correct function for your experiment design.
          A design from an bib design is needed here.",
                "you have a design of type ", design$parameters$design,
                " and an applied type of ", design$parameters$applied,
                "."))
  }

}



####### plot cyclic design #######

#' Plot Cyclic Design
#'
#' Plot a design of an experiment with an cyclic design from \code{agricolae} design.cyclic
#' @param design outdesign from \code{agricolae} package
#' @param y Describes the y coordinates of a experiment design
#' @param factor_name Which factor should be used for plotting, needs to be a column in outdesign$book
#' @param labels Describes the column from that the plots are taken to display them
#' @param width numeric value, describes the width of a plot in an experiment
#' @param height numeric value, describes the height of a plot in an experiment
#' @param space_width numeric value, describes the share of the space of the plots. 0=only space, 1=no space between plots in term of width
#' @param space_height numeric value, describes the share of the space of the plots. 0=only space, 1=no space between plots in term of height
#' @param reverse_y boolean, should the plots of the experiment be changed in reverse order in Row direction? use reverse_y=TRUE to have same sketch as in agricolae. default:reverse_y=FALSE
#' @param reverse_x boolean, should the plots of the experiment be changed in reverse order in column direction? default:reverse_x=FALSE
#'
#' @return \code{ggplot} graphic that can be modified, if wished
#' @export
#'
#' @examples
#' library(agricolaeplotr)
#' library(agricolae)

#' k <- 2
#' r <- 6
#' trt <-c('CIP-101','CIP-201','CIP-301','CIP-401','CIP-501',LETTERS[1:2])
#' outdesign<- design.cyclic(trt,k=k, r=r, serie=3, rowcol=TRUE)
#' plot_cyclic(outdesign, factor_name = 'trt')
plot_cyclic <- function(design,
                        y = "block",
                        factor_name = "trt",
                        labels = "plots",
                        width = 1,
                        height = 1,
                        space_width = 0.95,
                        space_height = 0.85,
                        reverse_y = FALSE,
                        reverse_x = FALSE) {
  if (design$parameters$design == "cyclic") {



    test_string(y)

    test_input_reverse(reverse_x)
    test_input_reverse(reverse_y)

    test_input_extend(height)
    test_input_extend(width)

    test_input_extend(space_height)
    test_input_extend(space_width)

    test_name_in_column(labels, design)

    table <- design$book
    table$part <- rep(1:max(table(table[, y])),
                  times = length(table[, factor_name])/max(table(table[, y])))

    table[, y] <- as.numeric(table[, y]) * height
    table$part <- table$part * width
    if (reverse_y == TRUE) {
      table[, y] <- abs(table[, y] - max(table[, y])) +
        min(table[, y])
    }

    if (reverse_x == TRUE) {
      table$part <- abs(table$part - max(table$part)) +
        min(table$part)
    }
    plt <- ggplot(table, aes_string(x = "part", y = y)) +
      geom_tile(aes_string(fill = factor_name),
                width = width * space_width, height = height *
                  space_height) + theme_bw() + theme(line = element_blank()) +
      geom_text(aes_string(label = labels), colour = "black")


    return(plt)

  } else {
    stop(paste0("This is not the correct function for your experiment design.
          A design from an cyclic design is needed here.",
                "you have a design of type ", design$parameters$design,
                " and an applied type of ", design$parameters$applied,
                "."))
  }

}




####### plot dau design #######

#' Plot Design of Augmented Blocks (dau)
#'
#' Plot a design of an experiment with an augmented block design from \code{agricolae} design.dau
#' @param design outdesign from \code{agricolae} package
#' @param y Describes the y coordinates of a experiment design
#' @param factor_name Which factor should be used for plotting, needs to be a column in outdesign$book
#' @param labels Describes the column from that the plots are taken to display them
#' @param width numeric value, describes the width of a plot in an experiment
#' @param height numeric value, describes the height of a plot in an experiment
#' @param space_width numeric value, describes the share of the space of the plots. 0=only space, 1=no space between plots in term of width
#' @param space_height numeric value, describes the share of the space of the plots. 0=only space, 1=no space between plots in term of height
#' @param reverse_y boolean, should the plots of the experiment be changed in reverse order in Row direction? use reverse_y=TRUE to have same sketch as in agricolae. default:reverse_y=FALSE
#' @param reverse_x boolean, should the plots of the experiment be changed in reverse order in column direction? default:reverse_x=FALSE
#'
#' @return \code{ggplot} graphic that can be modified, if wished
#' @export
#'
#' @examples
#' library(agricolaeplotr)
#' library(agricolae)

#' T1<-c('A','B','C','D','E','F')
#' T2<-letters[19:26]
#' outdesign <-design.dau(T1,T2, r=5,serie=2)
#' plot_dau(outdesign)
#' plot_dau(outdesign,reverse_y = TRUE)
plot_dau <- function(design,
                     y = "block",
                     factor_name = "trt",
                     labels = "plots",
                     width = 1,
                     height = 1,
                     space_width = 0.95,
                     space_height = 0.85,
                     reverse_y = FALSE,
                     reverse_x = FALSE) {
  if (design$parameters$design == "dau") {


    test_string(y)

    test_input_reverse(reverse_x)
    test_input_reverse(reverse_y)

    test_input_extend(height)
    test_input_extend(width)

    test_input_extend(space_height)
    test_input_extend(space_width)

    test_name_in_column(factor_name, design)
    test_name_in_column(labels, design)
    table <- design$book
    freq <- table(design$book$block)
    freq <- as.vector(freq)
    seqx <- function(x) {
      return(1:x)
    }
    col <- sapply(freq, seqx)
    table$col <- unlist(col)


    table$col <- table$col * width
    table[, y] <- as.numeric(table[, y]) * height

    if (reverse_y == TRUE) {
      table[, y] <- abs(table[, y] - max(table[, y])) +
        min(table[, y])
    }
    if (reverse_x == TRUE) {
      table$col <- abs(table$col - max(table$col)) +
        min(table$col)
    }
    plt <- ggplot(table, aes_string(x = "col", y = y)) +
      geom_tile(aes_string(fill = factor_name),
                width = width * space_width, height = height *
                  space_height) + theme_bw() + theme(line = element_blank()) +
      geom_text(aes_string(label = labels), colour = "black")


    return(plt)

  } else {
    stop(paste0("This is not the correct function for your experiment design.
          A design from an cyclic design is needed here.",
                "you have a design of type ", design$parameters$design,
                " and an applied type of ", design$parameters$applied,
                "."))
  }

}




###### plot rcdb ######

#' Plot randomized complete block designs
#'
#' Plot a design of an experiment with randomized complete block design (rcbd) design from \code{agricolae} design.rcbd
#' @param design outdesign from \code{agricolae} package
#' @param y Describes the y coordinates of a experiment design
#' @param factor_name Which factor should be used for plotting, needs to be a column in outdesign$book
#' @param labels Describes the column from that the plots are taken to display them
#' @param width numeric value, describes the width of a plot in an experiment
#' @param height numeric value, describes the height of a plot in an experiment
#' @param space_width numeric value, describes the share of the space of the plots. 0=only space, 1=no space between plots in term of width
#' @param space_height numeric value, describes the share of the space of the plots. 0=only space, 1=no space between plots in term of height
#' @param reverse_y boolean, should the plots of the experiment be changed in reverse order in Row direction? use reverse_y=TRUE to have same sketch as in agricolae. default:reverse_y=FALSE
#' @param reverse_x boolean, should the plots of the experiment be changed in reverse order in column direction? default:reverse_x=FALSE
#'
#' @return \code{ggplot} graphic that can be modified, if wished
#' @export
#' @examples
#' library(agricolaeplotr)
#' library(agricolae)

#' # 5 treatments and 6 blocks
#' trt<-c('A','B','C','D','E')
#' outdesign <-design.rcbd(trt,6,serie=2,986,'Wichmann-Hill') # seed = 986
#' plot_rcdb(outdesign)
#' plot_rcdb(outdesign,reverse_y = TRUE,reverse_x = TRUE)
#'
plot_rcdb <- function(design,
                      y = "block",
                      factor_name = "trt",
                      labels = "plots",
                      width = 1,
                      height = 1,
                      space_width = 0.95,
                      space_height = 0.85,
                      reverse_y = FALSE,
                      reverse_x = FALSE) {
  if (design$parameters$design == "rcbd") {


    test_string(y)

    test_input_reverse(reverse_x)
    test_input_reverse(reverse_y)

    test_input_extend(height)
    test_input_extend(width)

    test_input_extend(space_height)
    test_input_extend(space_width)

    test_name_in_column(factor_name, design)
    test_name_in_column(labels, design)

    table <- as.data.frame(design$book)

    table$col <- rep(1:length(unique(table[, factor_name])),
                     length(unique(table[, y])))
    table[, y] <- as.numeric(table[, y])
    if (reverse_y == TRUE) {
      table[, y] <- abs(table[, y] - max(table[, y])) +
        min(table[, y])
    }
    if (reverse_x == TRUE) {
      table$col <- abs(table$col - max(table$col)) +
        min(table$col)
    }
    table$col <- table$col * width
    table[, y] <- as.numeric(table[, y]) * height

    plt <- ggplot(table, aes_string(x = "col", y = y)) +
      geom_tile(aes_string(fill = factor_name),
                width = width * space_width, height = height *
                  space_height) + theme_bw() + theme(line = element_blank()) +
      geom_text(aes_string(label = labels), colour = "black")


    return(plt)

  } else {
    stop(paste0("This is not the correct function for your experiment design.
          A design from a Randomized Complete Block Design is needed here.",
                "you have a design of type ", design$parameters$design,
                " and an applied type of ", design$parameters$applied,
                "."))
  }

}





##### plot factorial crd ########

#' Plot Factorial Complete Randomized Designs (crd)
#'
#' Plot a design of a factorial experiment with completely randomized design (crd) from design.ab
#'
#' @param design outdesign from \code{agricolae} package
#' @param ncols integer value, choose the number of columns to which the experiment should be plotted
#' @param nrows integer value, choose the number of rows to which the experiment should be plotted
#' @param y Describes the y coordinates of a experiment design, default is row
#' @param factor_name Which factor should be used for plotting, needs to be a column in outdesign$book
#' @param width numeric value, describes the width of a plot in an experiment
#' @param height numeric value, describes the height of a plot in an experiment
#' @param labels string indicates the column of which the labels should be displayed
#' @param space_width numeric value, describes the share of the space of the plots. 0=only space, 1=no space between plots in term of width
#' @param space_height numeric value, describes the share of the space of the plots. 0=only space, 1=no space between plots in term of height
#' @param reverse_y boolean, should the plots of the experiment be changed in reverse order in Row direction? use reverse_y=TRUE to have same sketch as in agricolae. default:reverse_y=FALSE
#' @param reverse_x boolean, should the plots of the experiment be changed in reverse order in column direction? default:reverse_x=FALSE
#'
#' @return \code{ggplot} graphic that can be modified, if wished
#' @export
#' @import ggplot2
#' @import agricolae
#' @examples
#' library(agricolaeplotr)
#' library(agricolae)

#' trt<-c(3,2) # factorial 3x2
#' outdesign <- design.ab(trt, r=3, serie=2,design = 'crd')
#' plot_design.factorial_crd(outdesign,ncols = 8,nrows = 6)
#' plot_design.factorial_crd(outdesign,reverse_y = TRUE,ncols = 8,nrows = 6)
#' plot_design.factorial_crd(outdesign,reverse_y = TRUE,reverse_x = TRUE,ncols = 8,nrows = 6)

plot_design.factorial_crd <- function(design,
                                      ncols,
                                      nrows,
                                      y = "row",
                                      factor_name = "A",
                                      labels="plots",
                                      width = 1,
                                      height = 1,
                                      space_width = 0.95,
                                      space_height = 0.85,
                                      reverse_y = FALSE,
                                      reverse_x = FALSE) {
  if (design$parameters$design == "factorial" &&
      design$parameters$applied == "crd") {


    test_string(y)

    test_input_reverse(reverse_x)
    test_input_reverse(reverse_y)

    test_input_extend(height)
    test_input_extend(width)

    test_input_extend(space_height)
    test_input_extend(space_width)

    test_name_in_column(factor_name, design)
    test_name_in_column(labels, design)

    plots <- as.numeric(design$book[, 1])
    if (ncols * nrows >= length(plots)) {
    nas <- rep(NaN, ncols * nrows - length(plots))
    plots <- c(plots, nas)

    mat <- matrix(plots, byrow = TRUE, ncol = ncols)
    dims <- dim(mat)
    table <- as.table(mat)
    rownames(table) <- 1:dims[1]
    colnames(table) <- 1:dims[2]
    table <- as.data.frame(table)
    colnames(table) <- c("row", "col", "plots")
    table[, y] <- as.numeric(table[, y])
    table$col <- as.numeric(table$col)

    table <- merge(table, design$book, by.x = "plots",
                   by.y = "plots")
    table$col <- table$col * width
    table[, y] <- table[, y] * height
    if (reverse_y == TRUE) {
      table[, y] <- abs(table[, y] - max(table[, y])) +
        min(table[, y])
    }
    if (reverse_x == TRUE) {
      table$col <- abs(table$col - max(table$col)) +
        min(table$col)
    }
    plt <- ggplot(table, aes_string(x = "col", y = y)) +
      geom_tile(aes_string(fill = factor_name),
                width = width * space_width, height = height *
                  space_height) + theme_bw() +
      theme(line = element_blank()) +
      geom_text(aes_string(label = labels),
                colour = "black")

    return(plt)
    } else {
      stop(paste("You have in multiplication of ncols:",
                 ncols, "and nrows:", nrows, "Elements.",
                 "You need at minimum a product of both higher than",
                 length(plots)))
    }
  } else {
    stop(paste0("This is not the correct function for your experiment design.
          A design from a factorial design with
         a random complete block design (rcbd) is needed here.",
                "you have a design of type ", design$parameters$design,
                " and an applied type of ", design$parameters$applied,
                "."))
  }
}


##### plot factorial lsd ########

#' Plot Factorial Latin Square Designs (lsd)
#'
#' Plot a design of a factorial  experiment with latin square design (lsd) design from \code{agricolae} design.ab
#'
#' @param design outdesign from \code{agricolae} package
#' @param x Describes the x coordinates of a experiment design
#' @param y Describes the y coordinates of a experiment design
#' @param labels Describes the column from that the plots are taken to display them
#' @param factor_name Which factor should be used for plotting, needs to be a column in outdesign$book
#' @param width numeric value, describes the width of a plot in an experiment
#' @param height numeric value, describes the height of a plot in an experiment
#' @param space_width numeric value, describes the share of the space of the plots. 0=only space, 1=no space between plots in term of width
#' @param space_height numeric value, describes the share of the space of the plots. 0=only space, 1=no space between plots in term of height
#' @param reverse_y boolean, should the plots of the experiment be changed in reverse order in Row direction? use reverse_y=TRUE to have same sketch as in agricolae. default:reverse_y=FALSE
#' @param reverse_x boolean, should the plots of the experiment be changed in reverse order in column direction? default:reverse_x=FALSE
#'
#' @return \code{ggplot} graphic that can be modified, if wished
#' @export
#' @import ggplot2
#' @import agricolae
#' @examples
#' library(agricolaeplotr)
#' library(agricolae)

#' trt<-c(3,2) # factorial 3x2
#' outdesign <-design.ab(trt, r=3, serie=2,design = 'lsd')
#' plot_design.factorial_lsd(outdesign,factor_name = 'B',reverse_x = TRUE)
#'
plot_design.factorial_lsd <- function(design,
                                      x = "col",
                                      y = "row",
                                      factor_name = "A",
                                      labels = "plots",
                                      width = 1,
                                      height = 1,
                                      space_width = 0.95,
                                      space_height = 0.85,
                                      reverse_y = FALSE,
                                      reverse_x = FALSE) {
  if (design$parameters$design == "factorial" &&
      design$parameters$applied == "lsd") {
    test_string(x)
    test_string(y)

    test_input_reverse(reverse_x)
    test_input_reverse(reverse_y)

    test_input_extend(height)
    test_input_extend(width)

    test_input_extend(space_height)
    test_input_extend(space_width)

    test_name_in_column(factor_name, design)
    test_name_in_column(labels, design)
    table <- as.data.frame(design$book)
    table[, x]  <- as.numeric(table[, x]) * width
    table[, y] <- as.numeric(table[, y]) * height
    if (reverse_y == TRUE) {
      table[, y] <- abs(table[, y] - max(table[, y])) +
        min(table[, y])
    }
    if (reverse_x == TRUE) {
      table[, x]  <- abs(table[, x]  - max(table[, x] )) +
        min(table[, x] )
    }
    plt <- ggplot(table, aes_string(x = x,
                                                      y = y)) +
      geom_tile(aes_string(fill = factor_name),
                  width = width * space_width, height = height *
              space_height) + theme_bw() +
      theme(line = element_blank()) +
      geom_text(aes_string(label = labels),
                         colour = "black")

    plt

    return(plt)

  } else {
    stop(paste0("This is not the correct function for your experiment design.
          A design from an latin square design is needed here.",
                "you have a design of type ", design$parameters$design,
                " and an applied type of ", design$parameters$applied,
                "."))
  }

}

########## plot split plot design rcbd
########## #############

#' Plot Split Plot Designs with rcbd
#'
#' Plot a design of a split plot experiment with randomized complete blocks design (rcbd) from design.split
#' @param design outdesign from \code{agricolae} package
#' @param y string defines the block
#' @param width numeric value, describes the width of a plot in an experiment
#' @param height numeric value, describes the height of a plot in an experiment
#' @param space_width numeric value, describes the share of the space of the plots. 0=only space, 1=no space between plots in term of width
#' @param space_height numeric value, describes the share of the space of the plots. 0=only space, 1=no space between plots in term of height
#' @param reverse_y boolean, should the plots of the experiment be changed in reverse order in Row direction? use reverse_y=TRUE to have same sketch as in agricolae. default:reverse_y=FALSE
#' @param reverse_x boolean, should the plots of the experiment be changed in reverse order in column direction? default:reverse_x=FALSE
#' @param factor_name_1 string Which factor should be used for plotting, needs to be a column in outdesign$book
#' @param factor_name_2 string Which factor should be used for plotting, needs to be a column in outdesign$book
#' @param labels string Describes the column from that the plots are taken to display them
#' @param subplots should the plot function return the subplots (default) or main plots?
#'
#' @return \code{ggplot} graphic that can be modified, if wished
#' @export
#' @import ggplot2
#' @import agricolae
#' @examples
#' library(agricolaeplotr)
#' library(agricolae)
#' T1<-c('a','b','c','d','e')
#' T2<-c('v','w','x','y','z','zz')
#' r = 3
#' outdesign2 <- design.split(trt1=T1, trt2=T2, r=r,serie = 2,
#'  seed = 0, kinds = 'Super-Duper',randomization=TRUE,
#'  first=TRUE,design = 'rcbd')
#' plot_split_rcbd(outdesign2,width = 1,height = 1)
#' plot_split_rcbd(outdesign2,width = 1,height = 1,reverse_y = TRUE)
#' plot_split_rcbd(outdesign2,width = 1,height = 1,reverse_x = TRUE,reverse_y = TRUE)

plot_split_rcbd <- function(design,
                            y = "block",
                            factor_name_1 = "T1",
                            factor_name_2 = "T2",
                            subplots=TRUE,
                            labels = "plots",
                            width = 1,
                            height = 1,
                            space_width = 0.95,
                            space_height = 0.85,
                            reverse_y = FALSE,
                            reverse_x = FALSE) {
  if (design$parameters$design == "split" &&
      design$parameters$applied == "rcbd") {

    test_string(y)

    test_input_reverse(reverse_x)
    test_input_reverse(reverse_y)

    test_input_extend(height)
    test_input_extend(width)

    test_input_extend(space_height)
    test_input_extend(space_width)

    test_name_in_column(factor_name_1, design)
    test_name_in_column(factor_name_2, design)

    test_name_in_column(labels, design)

    table <- design$book

    table$row <- rep(rep(1:length(unique(table[, factor_name_1])),
                         each=length(unique(table[, factor_name_2]))),
                     times=max(as.numeric(table[, y])))
    table$row <- table$row * width

    table$sequence <- rep(1:(length(unique(table[, factor_name_1])) *
                               length(unique(table[, factor_name_2]))),
                          times = max(as.numeric(table[, y])))

    table$sequence <- table$sequence * width
    table[, y] <- as.numeric(table[, y]) * height

    if (reverse_y == TRUE) {
      table[, y] <- abs(table[, y] - max(table[, y])) +
        min(table[, y])
    }

    if (reverse_x == TRUE) {

      table$row <- abs(table$row - max(table$row)) +
        min(table$row)

      table$sequence <- abs(table$sequence - max(table$sequence)) +
        min(table$sequence)
    }


    if(subplots == TRUE){

      divider <- length(unique(table[, factor_name_1]))
      table$sequence <- table$sequence/divider

      plt2 <- ggplot(table, aes_string("sequence", y)) +
        geom_tile(aes_string(fill = factor_name_2),
                  width = width/divider * space_width,
                  height = height * space_height) + theme_bw() +
        theme(line = element_blank()) + geom_text(aes_string(label = labels),
                                                  colour = "black")


      return(plt2)

    } else{

      plt <- ggplot(table, aes_string("row", y)) +
        geom_tile(aes_string(fill = factor_name_1),
        width = width * space_width, height = height * space_height) +
        theme_bw() + theme(line = element_blank()) +
        geom_text(aes_string(label = labels), colour = "black")
      return(plt)
    }

  } else {
    stop(paste0("This is not the correct function for your experiment design.
          A design from a split plot design with a
                random complete block design (rcbd) is needed here.",
                "you have a design of type ", design$parameters$design,
                " and an applied type of ", design$parameters$applied,
                "."))
  }
}






########## plot split plot design lsd
########## #############

#' Plot Split Plot Design lsd
#'
#' Plot a design of a split plot experiment with latin squared design (lsd) from design.split
#' @param design outdesign from \code{agricolae} package
#' @param width numeric value, describes the width of a plot in an experiment
#' @param height numeric value, describes the height of a plot in an experiment
#' @param space_width numeric value, describes the share of the space of the plots. 0=only space, 1=no space between plots in term of width
#' @param space_height numeric value, describes the share of the space of the plots. 0=only space, 1=no space between plots in term of height
#' @param reverse_y boolean, should the plots of the experiment be changed in reverse order in Row direction? use reverse_y=TRUE to have same sketch as in agricolae. default:reverse_y=FALSE
#' @param reverse_x boolean, should the plots of the experiment be changed in reverse order in column direction? default:reverse_x=FALSE
#' @param factor_name_1 string Which factor should be used for plotting, needs to be a column in outdesign$book
#' @param factor_name_2 string Which factor should be used for plotting, needs to be a column in outdesign$book
#' @param labels string Describes the column from that the plots are taken to display them
#' @param subplots should the plot function return the subplots (default) or main plots?
#'
#' @return \code{ggplot} graphic that can be modified, if wished
#' @export
#' @import ggplot2
#' @import agricolae
#' @examples
#' library(agricolaeplotr)
#' library(agricolae)
#' T1<-c('a','b','c','d','e')
#' T2<-c('v','w','x','y')
#' outdesign2 <- design.split(trt1=T1, trt2=T2, r=r,serie = 2,
#'                            seed = 0, kinds = 'Super-Duper',
#'                            randomization=TRUE,first=TRUE,design = 'lsd')
#' plot_split_lsd(outdesign2,width = 4,height = 4)

plot_split_lsd <- function(design,
                           factor_name_1 = "T1",
                           factor_name_2 = "T2",
                           labels = "plots",
                           subplots=TRUE,
                           width = 1,
                           height = 1,
                           space_width = 0.95,
                           space_height = 0.85,
                           reverse_y = FALSE,
                           reverse_x = FALSE) {
  if (design$parameters$design == "split" && design$parameters$applied ==
      "lsd") {

    test_input_reverse(reverse_x)
    test_input_reverse(reverse_y)

    test_input_extend(height)
    test_input_extend(width)

    test_input_extend(space_height)
    test_input_extend(space_width)

    test_name_in_column(factor_name_1, design)
    test_name_in_column(factor_name_2, design)
    test_name_in_column(labels, design)

    table <- design$book

    table$sequence <- rep(1:(length(unique(table[, factor_name_1])) *
                             length(unique(table[, factor_name_2]))),
                        times = max(as.numeric(table$row)))

    table$sequence <- table$sequence * width

    table$col <- as.numeric(table$col) * width
    table$row <- as.numeric(table$row) * height

    # plot subplots

    if (reverse_y == TRUE) {
      table$row <- abs(table$row - max(table$row)) +
        min(table$row)
    }

    if (reverse_x == TRUE) {
      table$col <- abs(table$col - max(table$col)) +
        min(table$col)

      table$sequence <- abs(table$sequence -
                              max(table$sequence)) + min(table$sequence)
    }

    if(subplots == TRUE){

    divider <- length(unique(table[, factor_name_1]))
    table$sequence <- table$sequence/divider

    plt2 <- ggplot(table, aes_string("sequence", "row")) +
      geom_tile(aes_string(fill = factor_name_2),
                width = width/divider * space_width,
                height = height * space_height) + theme_bw() +
      theme(line = element_blank()) +
      geom_text(aes_string(label = labels),colour = "black")


    return(plt2)
    } else {
      plt <- ggplot(table, aes_string("col", "row")) +
        geom_tile(aes_string(fill = factor_name_1),
                  width = width * space_width, height = height *
                    space_height) + theme_bw() +
        theme(line = element_blank()) +
        geom_text(aes_string(label = labels),
                  colour = "black")

      return(plt)

    }

  } else {
    stop(paste0("This is not the correct function for your experiment design.
          A design from a split plot design with a
                random complete block design (lsd) is needed here.",
                "you have a design of type ", design$parameters$design,
                " and an applied type of ", design$parameters$applied,
                "."))
  }
}

########## plot split plot design crd
########## #############

#' Plot Split Plot Designs (crd)
#'
#' Plot a design of a split plot experiment with a complete randomized design (crd) from design.split
#'
#' @param design outdesign from \code{agricolae} package
#' @param ncols Number of columns for the design
#' @param nrows Number of rows for the design
#' @param width numeric value, describes the width of a plot in an experiment
#' @param height numeric value, describes the height of a plot in an experiment
#' @param space_width numeric value, describes the share of the space of the plots. 0=only space, 1=no space between plots in term of width
#' @param space_height numeric value, describes the share of the space of the plots. 0=only space, 1=no space between plots in term of height
#' @param reverse_y boolean, should the plots of the experiment be changed in reverse order in Row direction? use reverse_y=TRUE to have same sketch as in agricolae. default:reverse_y=FALSE
#' @param reverse_x boolean, should the plots of the experiment be changed in reverse order in column direction? default:reverse_x=FALSE
#' @param factor_name_1 string Which factor should be used for plotting, needs to be a column in outdesign$book
#' @param factor_name_2 string Which factor should be used for plotting, needs to be a column in outdesign$book
#' @param labels string Describes the column from that the plots are taken to display them
#' @param subplots should the plot function return the subplots (default) or main plots?
#'
#' @return \code{ggplot} graphic that can be modified, if wished
#' @export
#' @import ggplot2
#' @import agricolae
#' @examples
#' library(agricolaeplotr)
#' library(agricolae)
#' T1<-c('a','b','c','d','e','f','g')
#' T2<-c('v','w','x','y','zzz')
#' r <- 4
#' outdesign2 <- design.split(trt1=T1, trt2=T2, r=r,
#' serie = 2, seed = 0, kinds = 'Super-Duper',
#' randomization=TRUE,first=TRUE,design = 'crd')
#' plot_split_crd(outdesign2,ncols = 6,nrows=5)
#'
#' outdesign2 <- design.split(trt1=T1, trt2=T2, r=r,
#' serie = 2, seed = 0, kinds = 'Super-Duper',
#' randomization=FALSE,first=TRUE,design = 'crd')
#' plot_split_crd(outdesign2,ncols = 6,nrows=5)

plot_split_crd <- function(design, nrows, ncols,
                           factor_name_1 = "T1",
                           factor_name_2 = "T2",
                           labels = "plots",
                           subplots = TRUE,
                           width = 1,
                           height = 1,
                           space_width = 0.95,
                           space_height = 0.85,
                           reverse_y = FALSE,
                           reverse_x = FALSE) {

  if (design$parameters$design == "split" && design$parameters$applied ==
      "crd") {
    column <- NULL
    test_input_ncols(ncols)
    test_input_nrows(nrows)

    test_input_reverse(reverse_x)
    test_input_reverse(reverse_y)
    test_input_extend(height)
    test_input_extend(width)
    test_input_extend(space_height)
    test_input_extend(space_width)

    test_name_in_column(factor_name_1, design)
    test_name_in_column(factor_name_2, design)
    test_name_in_column(labels, design)

    plots <- as.numeric(unique(design$book[, 1]))
    if (ncols * nrows >= length(plots)) {
      nas <- rep(NaN, (ncols * nrows) - length(plots))
      plots <- c(plots, nas)

      mat <- matrix(plots, byrow = TRUE, ncol = ncols)
      dims <- dim(mat)
      table <- as.table(mat)
      rownames(table) <- 1:dims[1]
      colnames(table) <- 1:dims[2]
      table <- as.data.frame(table)
      colnames(table) <- c("row", "col", "plots")
      table$row <- as.numeric(table$row)
      table$col <- as.numeric(table$col)

      table <- merge(table, design$book, by.x = "plots",
                     by.y = "plots")
      table$plots <- as.factor(table$plots)


      table$col <- table$col * width
      table$row <- table$row * height

      table <- table[with(table, order(plots,
                                       splots)), ]

      freq <- table(table$row)
      freq <- as.vector(freq)
      seqx <- function(x) {
        return(1:x)
      }
      sequence_numbers <- sapply(freq, seqx)

      table$sequence <- unlist(sequence_numbers)

      table$sequence <- table$sequence * width





      if (reverse_y == TRUE) {
        table$row <- abs(table$row - max(table$row)) +
          min(table$row)
      }

      if (reverse_x == TRUE) {
        table$col <- abs(table$col -
                              max(table$col)) + min(table$col)
        table$sequence <- abs(table$sequence -
                                max(table$sequence)) + min(table$sequence)
      }



      if(subplots == TRUE){

      divider <- length(unique(table[, factor_name_1]))
      table$sequence <- table$sequence/divider

      plt2 <- ggplot(table, aes_string("sequence", "row")) +
        geom_tile(aes_string(fill = factor_name_2),
                  width = width * space_width/divider,
                  height = height * space_height) +
        theme_bw() + theme(line = element_blank()) +
        geom_text(aes_string(label = labels),
                  colour = "black")

      return(plt2)
      } else {
        plt <- ggplot(table, aes_string("col", "row")) +
          geom_tile(aes_string(fill = factor_name_1),
                    width = width * space_width, height = height *
                      space_height) + theme_bw() +
          theme(line = element_blank()) +
          geom_text(aes_string(label = labels),
                    colour = "black")

        return(plt)

      }

    } else {
      stop(paste("You have in multiplication of ncols:",
                 ncols, "and nrows:", nrows, "Elements.",
                 "You need at minimum a product of both higher than",
                 length(plots)))
    }
  } else {
    stop(paste0("This is not the correct function for your experiment design.,
          A design from a split plot design with a
                random complete block design (crd) is needed here.",
                "you have a design of type ", design$parameters$design,
                " and an applied type of ", design$parameters$applied,
                "."))
  }
}


#' ggplot2 theme for outdoor presentation
#'
#' This theme is designed to increase font size to ensure readability on outdoor used devices
#' @return ggplot2 theme
#' @export
#'
#' @examples
#' library(agricolaeplotr)
#' library(agricolae)
#' T1<-c('a','b','c','d','e','f','g')
#' T2<-c('v','w','x','y','z')
#' r <- 4
#' outdesign2 <- design.split(trt1=T1, trt2=T2, r=r,
#' serie = 2, seed = 0, kinds = 'Super-Duper',
#' randomization=FALSE,first=TRUE,design = 'crd')
#' plot_split_crd(outdesign2,ncols = 6,nrows=5)+
#' theme_pres()
theme_pres <- function() {
  theme(text = element_text(size = 16, colour = "black"),
        axis.text = element_text(size = 18, colour = "black"),
        axis.title = element_text(size = 18, colour = "black"),
        axis.line = element_line(colour = "black",
                                 size = 1, linetype = "solid"),
        axis.ticks = element_line(colour = "black")) +
    theme(plot.background = element_rect(fill = "white",
                                         color = NA),
          panel.background = element_rect(fill = "white",color = NA))
}


#' ggplot2 theme for poster presentation
#'
#' This theme is designed to increase font size to ensure readability on poster presentations
#' @return ggplot2 theme
#' @export
#'
#' @examples
#' library(agricolaeplotr)
#' library(agricolae)
#' T1<-c('a','b','c','d','e','f','g')
#' T2<-c('v','w','x','y','z')
#' r <- 4
#' outdesign2 <- design.split(trt1=T1, trt2=T2, r=r,
#' serie = 2, seed = 0, kinds = 'Super-Duper',
#' randomization=FALSE,first=TRUE,design = 'crd')
#' plot_split_crd(outdesign2,ncols = 6,nrows=5)+
#' theme_poster()
theme_poster <- function() {
  theme(text = element_text(size = 24, colour = "black"),
        axis.text = element_text(size = 28, colour = "black"),
        axis.title = element_text(size = 28, colour = "black"),
        axis.line = element_line(colour = "black",
                                 size = 1, linetype = "solid"),
        axis.ticks = element_line(colour = "black")) +
    theme(plot.background = element_rect(fill = "white",
                                         color = NA),
          panel.background = element_rect(fill = "white", color = NA))
}


#' Plot Youden Design
#'
#' Plot a Youden experiment design from \code{agricolae} design.youden
#'
#' @param design outdesign from \code{agricolae} package
#' @param x Describes the x coordinates of a experiment design
#' @param y Describes the y coordinates of a experiment design
#' @param width numeric value, describes the width of a plot in an experiment
#' @param height numeric value, describes the height of a plot in an experiment
#' @param space_width numeric value, describes the share of the space of the plots. 0=only space, 1=no space between plots in term of width
#' @param space_height numeric value, describes the share of the space of the plots. 0=only space, 1=no space between plots in term of height
#' @param reverse_y boolean, should the plots of the experiment be changed in reverse order in Row direction? use reverse_y=TRUE to have same sketch as in agricolae. default:reverse_y=FALSE
#' @param reverse_x boolean, should the plots of the experiment be changed in reverse order in column direction? default:reverse_x=FALSE
#' @param factor_name string Which factor should be used for plotting, needs to be a column in outdesign$book
#' @param labels string Describes the column from that the plots are taken to display them
#'
#' @return \code{ggplot} graphic that can be modified, if wished
#' @export
#' @import ggplot2
#' @import agricolae
#' @examples
#' library(agricolaeplotr)
#' library(agricolae)
#' varieties<-c('perricholi','yungay','maria bonita','tomasa')
#' outdesign <-design.youden(varieties,r=2,serie=2,seed=23)
#' plot_youden(outdesign, labels = 'varieties')
plot_youden <- function(design, x = "col", y = "row",
                        factor_name = "varieties", labels = "plots", width = 1,
                        height = 1, space_width = 0.95, space_height = 0.85,
                        reverse_y = FALSE, reverse_x = FALSE) {
  if (design$parameters$design == "youden") {

    test_string(x)
    test_string(y)

    test_input_reverse(reverse_x)
    test_input_reverse(reverse_y)

    test_input_extend(height)
    test_input_extend(width)

    test_input_extend(space_height)
    test_input_extend(space_width)

    test_name_in_column(factor_name, design)
    test_name_in_column(labels, design)

    table <- as.data.frame(design$book)

    table[, x]  <- as.numeric(table[, x] ) * width
    table[, y] <- as.numeric(table[, y]) * height

    if (reverse_y == TRUE) {
      table[, y] <- abs(table[, y] - max(table[, y])) +
        min(table[, y])
    }
    if (reverse_x == TRUE) {
      table[, x]  <- abs(table[, x]  - max(table[, x] )) +
        min(table[, x] )
    }


    plt <- ggplot(table, aes_string(x = x, y = y)) +
      geom_tile(aes_string(fill = factor_name),
                width = width * space_width, height = height *
                  space_height) + theme_bw() + theme(line = element_blank()) +
      geom_text(aes_string(label = labels), colour = "black")


    return(plt)

  } else {
    stop(paste0("This is not the correct function for your experiment design.
          A design from a Youden Design is needed here.",
                "you have a design of type ", design$parameters$design,
                " and an applied type of ", design$parameters$applied,
                "."))
  }

}

#' Measures of a Field Design
#'
#' Returns a list with several useful information about the experiment
#'
#' @param p \code{ggplot} object containing the data of the plot
#'
#' @return a list with several useful information about the experiment and the field
#' @export
#'
#' @examples
#' library(agricolae)
#' library(agricolaeplotr)
#' trt = c(2,3,4,5,6)
#' outdesign1 <-design.crd(trt,r=5,serie=2,2543,'Mersenne-Twister')
#' p <- plot_design_crd(outdesign1,
#'              ncols = 7,
#'              nrows = 4,
#'              width = 10,
#'              height = 10,
#'              reverse_y = TRUE)
#' stats <- DOE_obj(p)
#' stats
DOE_obj <- function(p){

  res <- list()
  dat <- layer_data(p)

  res$eff_width <- diff(range(dat$xmin,dat$xmax)) # nettoweite versuch
  res$eff_height <- diff(range(dat$ymin,dat$ymax)) # nettohoehe versuch
  res$eff_area_total <- res$eff_height * res$eff_width

  res$eff_width_plot <- dat$width[1] # netto weite parzelle
  res$eff_height_plot <- dat$height[1] # netto hoehe parzelle
  res$eff_plot_size <- res$eff_width_plot * res$eff_height_plot # netto groesse je parzelle
  res$eff_total_area <- res$eff_plot_size * dim(dat)[1] # netto Nutzfl?che unter Parzelle

  res$gross_width_plot <- min(dat$x) # bruttobreite Parzelle
  res$gross_height_plot <- min(dat$y) # bruttohoehe parzelle
  res$total_area_plot <- res$gross_width_plot * res$gross_height_plot # bruttoflaeche parzelle
  res$outer_area <- diff(range(c(dat$xmin,dat$xmax))) * diff(range(c(dat$ymin,dat$ymax)))

  res$rel_space_width <- 1 - ((res$gross_width_plot - res$eff_width_plot) / (res$gross_width_plot))
  res$rel_space_height <- 1 - ((res$gross_height_plot - res$eff_height_plot) / (res$gross_height_plot))

  res$n_plots <- dim(dat)[1]
  res$n_cols <- length(unique(dat$x))
  res$n_rows <- length(unique(dat$y))
  res$total_area <- res$total_area_plot * res$n_rows * res$n_cols# bruttofl?che versuch
  res$space_between <- res$total_area - res$eff_total_area

  res$share_eff_plot <- res$eff_plot_size / res$total_area_plot
  res$share_space_plot <- 1 - res$share_eff_plot

  res$abs_space_height <- res$gross_height_plot - res$eff_height_plot
  res$abs_space_width <- res$gross_width_plot - res$eff_width_plot

  res$net_plot_diagonal <- signif(sqrt(res$eff_height_plot^2 + res$eff_width_plot^2),3)
  res$gross_plot_diagonal <- signif(sqrt(res$gross_height_plot^2 + res$gross_width_plot^2),3)
  res$experiment_diagonal <- signif(sqrt(res$eff_width^2 + res$eff_width^2),3)
  res$outer_diagonal <- signif(sqrt(diff(range(c(dat$xmin,dat$xmax)))^2 + diff(range(c(dat$ymin,dat$ymax)))^2),3)

  res$xmin <- min(dat$xmin)
  res$xmax <- max(dat$xmax)
  res$ymin <- min(dat$ymin)
  res$ymax <- max(dat$ymax)
  res$n_fac <- length(unique(dat$fill))
  class(res) <- "FieldLayout"
  res
}

#' summary of a field Layout
#'
#' print a summary of a FieldLayout object
#' @param object an object, created by DOE_obj with a FieldLayout class
#' @param unit a string that corresponds to measure unit (default is m)
#' @param part which part of the summary are you interested?
#' Choose one of the following: "net_plot","gross_plot","field","experiment","all"
#' @param ... further arguments passed to or from other methods
#'
#' @export
#'
#' @examples
#' library(agricolaeplotr)
#' library(agricolae)
#' varieties<-c('perricholi','yungay','maria bonita','tomasa')
#' outdesign <-design.youden(varieties,r=2,serie=2,seed=23)
#' p <- plot_youden(outdesign, labels = 'varieties')
#' stats <- DOE_obj(p)
#' # print plot summary for net plot (plots without space)
#' summary(stats, part = "net_plot")
#' # print plot summary for gross plot (plots with space)
#' summary(stats, part = "gross_plot")
#' # print plot summary for entire field
#' summary(stats, part = "field")
#' # print plot summary for design summary
#' summary(stats, part = "experiment")
#' # print plot summary for all information shown above in one output
#' summary(stats, part = "all")
###FieldLayout <- function(object,unit,part,...) UseMethod("FieldLayout")
summary.FieldLayout <- function(object, unit="m", part="net_plot",...){
  if(!inherits(object,"FieldLayout")){
    stop("This is not the right class for this kind of summary. You need to use an object from a class \"FieldLayout\".")
  }
  x <- object
  if(!(part  %in% c("net_plot","gross_plot","field","experiment","all"))){
    stop(paste("part parameter needs to be one of the following: net_plot, gross_plot, field, all. You have typed",part))
  }
  if(part %in% c("net_plot","all")){

    print(paste("net plot height:",x$eff_height_plot, unit))
    print(paste("net plot width:",x$eff_width_plot, unit))
    print(paste("net plot diagonal:",x$net_plot_diagonal, unit))
    print(paste("net plot area:",x$eff_plot_size, paste0(unit,"^2")))
    print(paste("share used plot area:",x$share_eff_plot))
    print(paste("share space between plots:",x$share_space_plot))
    print(paste("space_width:",x$abs_space_height, unit))
    print(paste("space_height:",x$abs_space_width, unit))
  }

  if(part %in% c("gross_plot","all")){

    print(paste("gross plot height:",x$gross_height_plot, unit))
    print(paste("gross plot width:",x$gross_width_plot, unit))
    print(paste("gross plot diagonal:",x$gross_plot_diagonal, unit))
    print(paste("gross plot area:",x$total_area_plot,paste0(unit,"^2")))

    print(paste("share used plot area:",x$share_eff_plot))
    print(paste("share space between plots:",x$share_space_plot))
    print(paste("space_width:",x$abs_space_height, unit))
    print(paste("space_height:",x$abs_space_width, unit))
    print(paste("gross space area:",x$space_between, paste0(unit,"^2")))
  }

  if(part %in% c("field","all")){

    print(paste("relative design height:",x$rel_space_height))
    print(paste("relative design width:",x$rel_space_width))
    print(paste("net experiment diagonal:",x$experiment_diagonal, unit))
    print(paste("net experiment width:",x$eff_width, unit))
    print(paste("net experiment height:",x$eff_height, unit))
    print(paste("used plot area:",x$eff_total_area,paste0(unit,"^2")))
    print(paste("used area DOE:",x$outer_area,paste0(unit,"^2")))
    print(paste("used outer area:",x$total_area,paste0(unit,"^2")))
    print(paste("outer field diagonal:",x$outer_diagonal, unit))
  }
  if(part %in% c("experiment","all")){

    print(paste("xmin:", x$xmin))
    print(paste("xmax:", x$xmax))
    print(paste("ymin:", x$ymin))
    print(paste("ymax:", x$ymax))
    print(paste("number columns:",x$n_cols))
    print(paste("number rows:",x$n_rows))
    print(paste("number of plots:",x$n_plots))
    print(paste("number of factors:",x$n_fac))
  }
}

#' to_table
#'
#' Write field experiment information to a dataframe.
#' @param object an object, created by DOE_obj with a FieldLayout class
#' @param unit a string that corresponds to measure unit (default is m)
#' @param part which part of the summary are you interested?
#' Choose one of the following: "net_plot","gross_plot","field","experiment"
#' @param digits integer indicating the number of decimal places (round)
#' or significant digits (signif) to be used. Negative values are allowed
#' @param ... further arguments passed to or from other methods
#'
#' @return dataframe with corresponding information about the experiment
#' @export
#'
#' @examples
#' library(agricolaeplotr)
#' library(agricolae)
#' varieties<-c('perricholi','yungay','maria bonita','tomasa')
#' outdesign <-design.youden(varieties,r=2,serie=2,seed=23)
#' p <- plot_youden(outdesign, labels = 'varieties', width=4, height=3)
#' stats <- DOE_obj(p)
#' r <- to_table(stats,part = "net_plot", digits = 2)
#' r
#' r <- to_table(stats,part = "gross_plot", digits = 2)
#' r
#' r <- to_table(stats,part = "field", digits = 2)
#' r
#' r <- to_table(stats,part = "experiment", digits = 2)
#' r
#' r <- to_table(stats,part = "all", digits = 2)
#' r
to_table <- function(object,part="net_plot",unit="m",digits=3,...){
  if (!inherits(object,"FieldLayout")){
    stop("The object needs to be from the class 'agricolaeplotr'")
  }
  x <- unclass(object)
  if(!(part  %in% c("net_plot","gross_plot","field","experiment","all"))){
    stop(paste("part parameter needs to be one of the following: net_plot, gross_plot, field, experiment. You have typed",part))
  }
  if(part %in% c("net_plot")){
    df1 <- data.frame(names=rep(0,8))
    df1$names <- c(paste("net plot height:", unit),
                  paste("net plot width:", unit),
                  paste("net plot diagonal:", unit),
                  paste("net plot area:",paste0(unit,"^2")),
                  paste("share used plot area:"),
                  paste("share space between plots:"),
                  paste("space_width:", unit),
                  paste("space_height:",unit))

    df1$vals <- c(x$eff_height_plot,x$eff_width_plot,x$net_plot_diagonal,
                 x$eff_plot_size,x$share_eff_plot,x$share_space_plot,
                 x$abs_space_height,x$abs_space_width)

    df1$vals <- signif(df1$vals,digits=digits)
    return(df1)
  }
  if(part %in% c("gross_plot")){

    df2 <- data.frame(names=rep(0,9))

    df2$names <- c(paste("gross plot height:", unit),
                  paste("gross plot width:", unit),
                  paste("gross plot diagonal:", unit),
                  paste("gross plot area:",paste0(unit,"^2")),
                  paste("gross space area:", paste0(unit,"^2")),

                  paste("share used plot area: %/100"),
                  paste("share space between plots: %/100"),
                  paste("space_width:", unit),
                  paste("space_height:", unit))

    df2$vals <- c(x$gross_height_plot,x$gross_width_plot,x$gross_plot_diagonal,
                 x$total_area_plot,x$space_between,x$share_eff_plot,x$share_space_plot,
                 x$abs_space_height,x$abs_space_width)

    df2$vals <- signif(df2$vals,digits=digits)
    return(df2)
  }


  if(part %in% c("field")){

    df3 <- data.frame(names=rep(0,9))

    df3$names <- c(paste("relative design height:"),
                  paste("relative design width:"),
                  paste("net experiment diagonal:", unit),
                  paste("net experiment width:", unit),
                  paste("net experiment height:", unit),
                  paste("used plot area:",paste0(unit,"^2")),
                  paste("used area DOE:",paste0(unit,"^2")),
                  paste("used outer area:",paste0(unit,"^2")),
                  paste("outer field diagonal:", unit))


    df3$vals <- c(x$rel_space_height,x$rel_space_width,x$experiment_diagonal,x$eff_width,
                 x$eff_height,x$eff_total_area,x$outer_area,x$total_area,x$outer_diagonal)

    df3$vals <- signif(df3$vals,digits=digits)
    return(df3)
  }
  if(part %in% c("experiment")){


    df4 <- data.frame(names=rep(0,8))

    df4$names <-c("xmin:",
                 "xmax:",
                 "ymin:",
                 "ymax:",
                 "number columns:",
                 "number rows:",
                 "number of plots:",
                 "number of factors:")

    df4$vals <- c(x$xmin,
                 x$xmax,
                 x$ymin,
                 x$ymax,
                 x$n_cols,
                 x$n_rows,
                 x$n_plots,
                 x$n_fac)

    df4$vals <- signif(df4$vals,digits=digits)
    return(df4)
  }
  if(part %in% c("all")){
    df5 <- data.frame(names=rep(0,30))
    df5$names <- c(paste("net plot height:", unit),
                   paste("net plot width:", unit),
                   paste("net plot diagonal:", unit),
                   paste("net plot area:",paste0(unit,"^2")),
                   paste("share used plot area:"),
                   paste("share space between plots:"),
                   paste("space_width:", unit),
                   paste("space_height:",unit),
                   paste("gross plot height:", unit),
                   paste("gross plot width:", unit),
                   paste("gross plot diagonal:", unit),
                   paste("gross plot area:",paste0(unit,"^2")),
                   paste("gross space area:", paste0(unit,"^2")),
                   paste("relative design height:"),
                   paste("relative design width:"),
                   paste("net experiment diagonal:", unit),
                   paste("net experiment width:", unit),
                   paste("net experiment height:", unit),
                   paste("used plot area:",paste0(unit,"^2")),
                   paste("used area DOE:",paste0(unit,"^2")),
                   paste("used outer area:",paste0(unit,"^2")),
                   paste("outer field diagonal:", unit),
                   "xmin:",
                   "xmax:",
                   "ymin:",
                   "ymax:",
                   "number columns:",
                   "number rows:",
                   "number of plots:",
                   "number of factors:")


    df5$vals <- c(x$eff_height_plot,x$eff_width_plot,x$net_plot_diagonal,
                  x$eff_plot_size,x$share_eff_plot,x$share_space_plot,
                  x$abs_space_height,x$abs_space_width,
                  x$gross_height_plot,x$gross_width_plot,x$gross_plot_diagonal,
                  x$total_area_plot,x$space_between,

                  x$rel_space_height,x$rel_space_width,x$experiment_diagonal,x$eff_width,
                  x$eff_height,x$eff_total_area,x$outer_area,x$total_area,x$outer_diagonal,
                  x$xmin,
                  x$xmax,
                  x$ymin,
                  x$ymax,
                  x$n_cols,
                  x$n_rows,
                  x$n_plots,
                  x$n_fac)
    df5$vals <- signif(df5$vals,digits=digits)
    return(df5)

  }




   }


#' make_polygons
#'
#' This function coerces all rectangles
#' from a 'ggplot' object to 'SpatialPolygonDataframe'.
#' @param ggplot_object saved ggplot object, containing the
#' coordinates of the rectangles of a 'ggplot' object of the first two layers
#' @param north float added to the rows
#'  to have a northing ordinate
#' @param east float added to the rows
#'  to have a easting ordinate
#' @param projection_input string defines
#'  in which EPSG projection the ggplot object should be converted to a raster object?
#'  a projection with a metric unit is highly recommended
#' @param projection_output string defines
#'  in which EPSG projection the SpatialPolygonDataFrame should be exported.
#' @export
#' @return a SpatialPolygonDataframe object
#'
#' @examples
#' library(agricolaeplotr)
#' library(agricolae)
#' trt = c(2,3,4)
#' outdesign1 <-design.crd(trt,r=5,serie=2,2543,'Mersenne-Twister')
#' plt <- plot_design_crd(outdesign1,ncols = 13,nrows = 3)
#' spat_df <- make_polygons(plt)
#' spat_df
make_polygons <- function(ggplot_object,
                          north = 3454206.89,
                          east = 5939183.21,
                          projection_input = "+init=epsg:31467",
                          projection_output = "+init=epsg:4326") {
  table_object <- merge(ggplot2::layer_data(ggplot_object,1),
                        ggplot2::layer_data(ggplot_object,2),
                        by.x=c("x","y"), by.y=c("x","y"))
  bounds = table_object[, c(7, 6, 9, 8)]
  bounds$xmax <- bounds$xmax + east
  bounds$xmin <- bounds$xmin + east

  bounds$ymax <- bounds$ymax + north
  bounds$ymin <- bounds$ymin + north


  polygons_list = apply(bounds, 1, function(x) {
    out = raster::extent(x[c(2, 1, 4, 3)])
    out = methods::as(out, 'SpatialPolygons')
    out = methods::as(out, 'SpatialPolygonsDataFrame')
    raster::crs(out) <- sp::CRS(projection_input)
    out
  })


  polygons_list = do.call(rbind, polygons_list)

  polygons_list@data <- table_object

  polygons_list <- sp::spTransform(polygons_list, sp::CRS(projection_output))
  return(polygons_list)
}

#' theme_gil
#'
#' Creates a theme for 'ggplot' based graphics to ensure
#' to meet formal requirements for conferences of the
#' Gesellschaft fuer Informatik in der
#'  Land-, Forst,- und Ernaehrungswirtschaft e.V. (GIL).
#'
#' @return a 'ggplot' graph with a modified theme
#' @export
#'
#' @examples
#' # example borrowed from ggplot2
#' library(ggplot2)
#' df <- data.frame(
#' gp = factor(rep(letters[1:3], each = 10)),
#' y = rnorm(30))
#' p <- ggplot() +
#' geom_point(data = df, aes(gp, y))
#' p <- p + theme_gil();p
theme_gil <- function(){
  theme_bw() +
    theme(axis.text = element_text(colour = "black", size=9))+
    theme(axis.title = element_text(size = 10, angle = 0,hjust = 0.5))
}



#' Plot FielDHub Design
#'
#' Plots designs from \code{FielDHub} package
#' @param design outdesign from \code{FielDHub} package with on of the following IDs: c(9,13,14,15,16)
#' @param x Describes the x coordinates of a experiment design
#' @param y Describes the y coordinates of a experiment design
#' @param labels string Describes the column from that the plots are taken to display them
#' @param factor_name string Which factor should be used for plotting, needs to be a column in outdesign$book
#' @param width numeric value, describes the width of a plot in an experiment
#' @param height numeric value, describes the height of a plot in an experiment
#' @param space_width numeric value, describes the share of the space of the plots. 0=only space, 1=no space between plots in term of width
#' @param space_height numeric value, describes the share of the space of the plots. 0=only space, 1=no space between plots in term of height
#' @param reverse_y boolean, should the plots of the experiment be changed in reverse order in Row direction? use reverse_y=TRUE to have same sketch as in agricolae. default:reverse_y=FALSE
#' @param reverse_x boolean, should the plots of the experiment be changed in reverse order in column direction? default:reverse_x=FALSE
#' @param shift_x numeric indicates the shift in units in x-axis.
#' @param shift_y numeric indicates the shift in units for the y-axis.

#' @return \code{ggplot} graphic that can be modified, if wished
#' @export
#' @import ggplot2
#' @import FielDHub
#' @examples
#' library(agricolaeplotr)
#' library(FielDHub)
#' SpatpREP1 <- partially_replicated(nrows = 25,
#'                                   ncols = 18,
#'                                   repGens = c(280,50,10,1,1),
#'                                   repUnits = c(1,2,3,20,20),
#'                                   planter = "cartesian",
#'                                   plotNumber = 101,
#'                                   seed = 77)
#' SpatpREP1$infoDesign
#'
#' plot_fieldhub(SpatpREP1,
#' labels = "PLOT",
#' factor_name = "PLOT",
#' width = 12,
#' height = 10,
#' reverse_y = TRUE,
#' reverse_x = TRUE)
#'
#' NAME <- paste("G", 1:492, sep = "")
#' repGens = c(108, 384);repUnits = c(2,1)
#' REPS <- rep(repUnits, repGens)
#' treatment_list <- data.frame(list(ENTRY = 1:492, NAME = NAME, REPS = REPS))
#'
#' SpatpREP2 <- partially_replicated(nrows = 30,
#'                                   ncols = 20,
#'                                   planter = "serpentine",
#'                                   plotNumber = 101,
#'                                   seed = 41,
#'                                   data = treatment_list)
#' SpatpREP2$infoDesign
#'
#' plot_fieldhub(SpatpREP2,
#' labels = "PLOT",
#' factor_name = "PLOT",
#' width = 12,
#' height = 10,
#' reverse_y = TRUE,
#' reverse_x = TRUE)
#'
plot_fieldhub <- function(design,
                          x = "COLUMN",
                          y = "ROW",
                          labels = "PLOT",
                          factor_name = "TREATMENT",
                          width = 1,
                          height = 1,
                          space_width = 0.95,
                          space_height = 0.85,
                          reverse_y = FALSE,
                          reverse_x = FALSE,
                          shift_x=0,
                          shift_y=0) {
  if (design$infoDesign$idDesign %in% c(2,3,4,7,8,9,10,11,12,13,14,15,16)) {
    design$book <- design$fieldBook
    test_string(x)
    test_string(y)

    test_input_reverse(reverse_x)
    test_input_reverse(reverse_y)

    test_input_extend(height)
    test_input_extend(width)

    test_input_extend(space_height)
    test_input_extend(space_width)

    test_input_shift(shift_x)
    test_input_shift(shift_y)

    test_name_in_column(labels, design)

    table <- as.data.frame(design$book)
    table[, x]  <- as.numeric(table[, x] ) * width + shift_x
    table[, y] <- as.numeric(table[, y]) * height + shift_y
    if (reverse_y == TRUE) {
      table[, y] <- abs(table[, y] - max(table[, y])) +
        min(table[, y])
    }
    if (reverse_x == TRUE) {
      table[, x]  <- abs(table[, x]  - max(table[, x] )) +
        min(table[, x] )
    }
    plt <- ggplot(table, aes_string(x = x, y = y)) +
      geom_tile(aes_string(fill = factor_name),
                width = width * space_width, height = height *
                  space_height) + theme_bw() + theme(line = element_blank()) +
      geom_text(aes_string(label = labels), colour = "black")# +
    # + facet_grid(reformulate(replication_name,location_name))

    plt

    return(plt)

  } else {
    stop(paste0("This is not the correct function for your experiment design."))
  }

}

#' Serpentine
#'
#' This function produces a serpentine array of integers beginning by one
#'
#' @param n integer value indicating the upper cap of a numeric sequence
#' @param times integer number of replications
#' @param m integer value indicating the lower
#' cap of a numeric sequence
#'
#' @return vector containing the serpentine sequence
#' @export
#'
#' @examples
#' serpentine(n=20,times = 15)
#' serpentine(n=20,times = 15,m=4)
serpentine <- function(n,times,m=1){
  vec= vector()
  for(i in 1:times){
    if(i %% 2 == 1){
      vec <- cbind(vec,m:n)
    }
    else{
      vec <- cbind(vec,n:m)
    }
  }
  return(as.vector(vec))
}

#' full_control_positions
#'
#' This function provides full control about the plotting. The user also may shift the coordinates as liked.
#'
#' @param design data.frame containing the row and columns of an experiment
#' @param x Describes the x coordinates of a experiment design
#' @param y Describes the y coordinates of a experiment design
#' @param width numeric value, describes the width of a plot in an experiment
#' @param height numeric value, describes the height of a plot in an experiment
#' @param space_width numeric value, describes the share of the space of the plots. 0=only space, 1=no space between plots in term of width
#' @param space_height numeric value, describes the share of the space of the plots. 0=only space, 1=no space between plots in term of height
#' @param reverse_y boolean, should the plots of the experiment be changed in reverse order in Row direction? use reverse_y=TRUE to have same sketch as in agricolae. default:reverse_y=FALSE
#' @param reverse_x boolean, should the plots of the experiment be changed in reverse order in column direction? default:reverse_x=FALSE
#' @param factor_name string Which factor should be used for plotting, needs to be a column in outdesign$book
#' @param labels string Describes the column from that the plots are taken to display them
#' @param shift_x numeric indicates the shift in units in x-axis.
#' @param shift_y numeric indicates the shift in units for the y-axis.
#' @param start_origin boolean. Should the design start at the origin (0|0)?
#'
#' @return \code{ggplot} graphic that can be modified, if wished
#' @export
#' @import ggplot2
#' @import agricolae
#' @examples
#' library(agricolaeplotr)
#' library(agricolae)
#' varieties<-c('perricholi','yungay','maria bonita','tomasa')
#' outdesign <-design.youden(varieties,r=2,serie=2,seed=23)
#' design <- outdesign$book
#' design
#' p <- full_control_positions(design,"col","row","varieties","plots",
#'                        width=3,height=4.5,
#'                        space_width=0.5,space_height=0.5,
#'                        shift_x=(-0.5*3) + (-0.5*3*0.5),shift_y=-0.5*4.5 + (-0.5*4.5*0.5))
#' p
#' p <- full_control_positions(design,"col","row","varieties","plots",
#'                        width=3,height=4.5,
#'                        space_width=0.13,space_height=0.445,
#'                        shift_x=(-0.5*3) + (-0.5*3*(1-0.13)),shift_y=-0.5*4.5 + (-0.5*4.5*(1-0.445)))
#'                        p
#'
#' p <- full_control_positions(design,"col","row","varieties","plots",
#'                        width=3,height=4.5,
#'                        space_width=1,space_height=1,
#'                        shift_x=-0.5*3,shift_y=-0.5*4.5)
#' p
#'
#' p <- full_control_positions(design,"col","row","varieties","plots",
#'                        width=3,height=4.5,
#'                        space_width=0.93,space_height=0.945,
#'                        start_origin = TRUE)
#'                        p
full_control_positions <- function(design,
                                   x = "col",
                                   y = "row",
                                   factor_name = "trt",
                                   labels = "plots",
                                   width = 1,
                                   height = 1,
                                   space_width = 0.95,
                                   space_height = 0.85,
                                   reverse_y = FALSE,
                                   reverse_x = FALSE,
                                   shift_x=0,
                                   shift_y=0,
                                   start_origin=FALSE) {

  test_string(x)
  test_string(y)

  test_input_reverse(reverse_x)
  test_input_reverse(reverse_y)

  test_input_extend(height)
  test_input_extend(width)

  test_input_extend(space_height)
  test_input_extend(space_width)

  test_input_shift(shift_x)
  test_input_shift(shift_y)

  table <- design

  if(start_origin == TRUE){
    shift_x <- width * -0.5 + (width * -0.5 * (1-space_width)) ## makes zero
    shift_y <- height * -0.5 + (height * -0.5 * (1-space_height)) ## makes zero

    table[, x]  <- as.numeric(table[, x] ) * width + shift_x
    table[, y] <- as.numeric(table[, y]) * height + shift_y
  }
  else{
  table[, x]  <- as.numeric(table[, x] ) * width + shift_x
  table[, y] <- as.numeric(table[, y]) * height + shift_y
  }
  if (reverse_y == TRUE) {
    table[, y] <- abs(table[, y] - max(table[, y])) +
      min(table[, y])
  }
  if (reverse_x == TRUE) {
    table[, x]  <- abs(table[, x]  - max(table[, x] )) +
      min(table[, x] )
  }
  plt <- ggplot(table, aes_string(x = x, y = y)) +
    geom_tile(aes_string(fill = factor_name),
              width = width * space_width, height = height *
                space_height) + theme_bw() + theme(line = element_blank()) +
    geom_text(aes_string(label = labels), colour = "black")

  plt

  return(plt)
}
