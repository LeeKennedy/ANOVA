library("ProjectTemplate")
load.project()

data.raw <- read.csv("data/data.csv", as.is=TRUE, header=TRUE)

anova_report <- function (x, units="", doc_type) {

                if(doc_type == "docx") {
                        rmarkdown::render("~/Documents/GitHub/ANOVA/reports/ANOVA.Review.Rmd", 
                                          output_format = "word_document",
                                          output_file = "ANOVA Review.docx",
                                          output_dir = "~/Documents/GitHub/ANOVA/reports/Output")
                        
                } else if(doc_type == "pdf") {
                        rmarkdown::render("~/Documents/GitHub/ANOVA/reports/ANOVA.Review.Rmd", 
                                          output_format = "pdf_document",
                                          output_file = "ANOVA Review.pdf",
                                          output_dir = "~/Documents/GitHub/ANOVA/reports/Output")
                        
                } else {
                        rmarkdown::render("~/Documents/GitHub/ANOVA/reports/ANOVA.Review.Rmd", 
                                          output_format = "html_document",
                                          output_file = "ANOVA Review.html",
                                          output_dir = "~/Documents/GitHub/ANOVA/reports/Output")
                }
        }



anova_report (data.raw, doc_type="pdf")

