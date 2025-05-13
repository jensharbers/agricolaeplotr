# Load required library for QR codes
library(qrcode)
library(jsonify)
library(qpdf)

#' @title Generate QR Code Labels and Save as PDF
#' @description This function generates QR code labels based on the provided dataset and saves them as a PDF file.
#' Each label contains a QR code along with associated metadata. The labels are arranged in a grid on an A4-sized page.
#'
#' @param input_df A data frame containing label data. The data frame should have at least three columns: Name1, Name2, and Value.
#' @param output_file The base name for the generated PDF files (default: "labelscorrected").
#' @param experiment_name A string specifying the name of the experiment (default: "My Experiment").
#' @param img_width Width of each label in millimeters (default: 70mm).
#' @param img_height Height of each label in millimeters (default: 36mm).
#' @param page_width Width of the page in millimeters (default: 210mm for A4 size).
#' @param page_height Height of the page in millimeters (default: 297mm for A4 size).
#'
#' @return A single PDF file containing all generated QR code labels, arranged in a grid layout.
#'
#' @details
#' - The function calculates the number of labels that fit on an A4 page.
#' - Labels are distributed in a grid layout.
#' - Each label includes a QR code and text extracted from the input data.
#' - Multiple pages are handled automatically, and all generated PDFs are merged into a single output file.
#'
#' @importFrom qrcode qr_code
#' @importFrom grDevices dev.off pdf
#' @importFrom graphics par rasterImage rect text
#' @importFrom qpdf pdf_combine
#'
#' @export
#'
#' @examples
#' library(agricolaeplotr)
#' library(agricolae)
#' library(qrcode)
#'
#' trt <- c(2, 4)
#' k <- 6
#' outdesign <- design.ab(trt, r = k, serie = 3, design = 'rcbd')
#' generate_qr_labels(outdesign$book)
#'
#' # Triple lattice design (9 treatments)
#' trt <- LETTERS[1:9]
#' outdesign <- design.lattice(trt, r = 3, serie = 2)
#' generate_qr_labels(outdesign$book)
#'
#' t1 <- c("A", "B", "C", "D", "E")
#' t2 <- c(1, 2, 3, 4, 5, 6)
#' outdesign <- design.split(t1, t2, r = 3, serie = 2,
#'  seed = 45, kinds = "Super-Duper") # Split-plot design
#' generate_qr_labels(outdesign$book, output_file = "mylabels2")

generate_qr_labels <- function(input_df, output_file="labelscorrected",experiment_name="My Experiment", img_width = 65, img_height = 32, page_width = 210, page_height = 297) {
  # Define layout constants
  cols <- floor(page_width / img_width)  # Number of columns
  rows <- floor(page_height / img_height)  # Number of rows


  total_images <- nrow(input_df)
  images_per_page <- cols*rows

  num_pages <- ceiling(total_images / images_per_page)

  input_df$trial <- experiment_name

  input_df <- data.frame(lapply(input_df,as.factor))
  # Convert mm to inches
  mm_to_inches <- function(mm) {
    return(mm / 25.4)
  }

  for (page in seq_len(num_pages)) {
  start_idx <- (page - 1) * images_per_page + 1
  end_idx <- min(page * images_per_page, total_images)

  # Set up plotting device (e.g., PDF or PNG)
  file_name <- sprintf(paste0(output_file,"_%02d.pdf"), page)  # Auto-numerated filename

  # Open a PDF to save output
  pdf(file_name, width = mm_to_inches(page_width), height = mm_to_inches(page_height))

  # Set up a blank plot for the whole A4 page
  par(mar = c(0, 0, 0, 0))  # Remove margins
  plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = c(0, page_width), ylim = c(0, page_height))

  df_chunk <- input_df[c(start_idx:end_idx),]
  # Loop through data and draw labels in a grid
  for (i in 1:nrow(df_chunk)) {
    # Calculate row and column position
    row <- ((i - 1) %% rows) + 1
    col <- ((i - 1) %/% rows) + 1

    # Convert to plot coordinates (in mm)
    x_left <- (col - 1) * img_width
    x_right <- x_left + img_width
    y_bottom <- page_height - (row * img_height)
    y_top <- y_bottom + img_height

    # Draw a rectangle for the label boundary
    rect(x_left, y_bottom, x_right, y_top, border = "white", lwd = 1)
    # Generate QR code
    qr_text <- jsonify::to_json(df_chunk[i,])
    qr <- 1- qrcode::qr_code(qr_text, ecl = "Q")

    # Draw QR code on the left side
    qr_x_left <- x_left + 0
    qr_x_right <- qr_x_left + 26
    qr_y_bottom <- y_bottom + 0
    qr_y_top <- qr_y_bottom + 26
    rasterImage(qr, qr_x_left, qr_y_bottom, qr_x_right, qr_y_top)

    # Add text on the right side
    text_x <- x_right - 7  # Align text to the right
    text_y_top <- y_top - 8

    line_spacing <- 5

    for (j in seq_along(df_chunk)) {
      text_y_position <- text_y_top - (j - 1) * line_spacing  # Adjust vertical spacing
      text(text_x, text_y_position, paste(colnames(df_chunk)[j], ":", df_chunk[i, j]),
           cex = 1.2, font = 2, adj = 1)
    }
    }

  # Close PDF output
  dev.off()
  }

  files <- Sys.glob(paste0(output_file,"*.pdf"))
  qpdf::pdf_combine(input = files, output = paste0(output_file,".pdf"))
}
