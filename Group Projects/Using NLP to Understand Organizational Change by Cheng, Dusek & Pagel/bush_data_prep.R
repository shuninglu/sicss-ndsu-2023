library(tesseract)
library(pdftools)

# First, deal with PDFs in native format, since these are easier to convert to text
setwd("~/Desktop/2023-06 SICSS/project/pdf_text")

# Get list of PDFs and convert to text
text_pdfs <- list.files(pattern="*.pdf")
text_pdf_reports <- lapply(text_pdfs, function(pdf) {
  paste(pdftools::pdf_text(pdf), sep="", collapse="")
})

# Next deal with scans, since these take longer and require OCR
setwd("~/Desktop/2023-06 SICSS/project/pdf_scans")

# Go newest to oldest
scan_pdfs <- list.files(pattern="*.pdf")

# Create function for writing PDF to text via OCR
pdf_to_txt <- function(pdf) {
  year <- as.integer(gsub("annualreport.pdf", "", pdf))
  outfile <- paste(year, ".txt", sep="")
  pdf_text <- ocr(pdf)
  write(pdf_text, outfile)
}

lapply(scan_pdfs, pdf_to_txt)
# Or if you have access to a more powerful (Linux/UNIX) computer:
# library(parallel)
# mclapply(scan_pdfs, pdf_to_txt, mc.cores=<n_cpu_cores>)

# Get list of text files
setwd("~/Desktop/2023-06 SICSS/project/text-reports-1963-1997")
scan_text_files <- list.files(pattern="*.txt")

# Read in OCR'd text from scanned PDF files
scan_pdf_reports <- lapply(scan_text_files, function(text_file) {
  paste(readLines(text_file), sep="", collapse="")
})

# Merge
setwd("~/Desktop/2023-06 SICSS/project")
filenames <- c(text_pdfs, scan_pdfs)
years <- c(2000,2001,2002,2008,2009,2011,2012,
           1998,1999,2010,2004,2003,2005,2006,2007,
           as.integer(gsub(".txt", "", scan_text_files)))
all_texts <- c(text_pdf_reports, scan_pdf_reports)


reports <- data.frame(document=filenames, 
                      text=unlist(all_texts),
                      year=years)

saveRDS(reports, "all_reports.rds")

#---#

setwd("~/Desktop/2023-06 SICSS/project")
reports <- readRDS("all_reports.rds")

