# R/global.R

# Load required libraries
library(shiny)
library(shinyAce)
library(processx) # For asynchronous docker command execution
library(later)    # For scheduling delayed actions
library(jsonlite) # For persisting UI settings as JSON

# Set maximum upload size to 100MB
options(shiny.maxRequestSize = 100 * 1024^2)

# Define which file extensions are considered text-based (editable)
text_extensions <- c("tex", "bib", "bst", "cls", "cfg", "sty", "txt", "rnw")

# Directories for project (source) files and compiled outputs
# Using tempdir() ensures a writable, predictable location
uploadDir <- file.path(tempdir(), "uploads")
compiledDir <- file.path(tempdir(), "compiled")

# Create directories if they don't exist
for (d in c(uploadDir, compiledDir)) {
  if (!dir.exists(d)) dir.create(d)
}

# Map directories for static serving
addResourcePath("uploads", uploadDir)
addResourcePath("compiled", compiledDir)
