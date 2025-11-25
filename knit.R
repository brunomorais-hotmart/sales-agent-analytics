# Generate a dynamic filename (e.g., with today's date)
dynamic_filename <- paste("MVP_Report", format(Sys.Date()-1, format = '%d%m'), sep = "_")

# Render the R Markdown document with the dynamic filename
rmarkdown::render(
  input = "mvp_report.Rmd",
  output_file = paste0(dynamic_filename, ".html")
)