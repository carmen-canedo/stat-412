# Collection of functions for cleaning data

# Loading libraries
library(tidyverse)
library(scales) # adds some features to improve ggplot2 scales on axes and legends
library(ggpubr) # combine multiple ggplot2 graphics
library(kableExtra) # Nice data tables
library(RColorBrewer) # Color palettes

# Global option to hide all code but still show results
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)


# Function to create subdirectories
create_subdirectories <- function(subdirectory_names) {
  n_subdirectories <- length(subdirectory_names)
  for (i in 1:n_subdirectories) {
    if (!dir.exists(subdirectory_names[i])) {
      dir.create(subdirectory_names[i])
    } }
}

# Reading in data
get_data <- function(data_file, data_dir, data_URL) {
  data_loc <- paste(data_dir, "/", data_file, sep = "")
  if(!file.exists(data_loc)) {
    data_URL %>%
      read_csv() %>%
      write_csv(data_loc)
  }
  
  tbl <- read_csv(data_loc)
  
  return(tbl)
}

# DBN to Borough
DBN_to_Borough <- function(DBN) {
  if(str_detect(DBN, "M")) {Borough <- "Manhattan"}
  if(str_detect(DBN, "K")) {Borough <- "Brooklyn"}
  if(str_detect(DBN, "Q")) {Borough <- "Queens"}
  if(str_detect(DBN, "X")) {Borough <- "Bronx"}
  if(str_detect(DBN, "R")) {Borough <- "Staten Island"}
  
  return(Borough)
}

# Transform data
transform_data <- function(tbl) {
  new_tbl <- tbl %>%
    drop_na() %>%
    rename_with(~ str_replace_all(.x,
                                  pattern = " ", replacement = "_")) %>%
    rowwise() %>%
    mutate(Borough = as.factor(DBN_to_Borough(DBN)))
  return(new_tbl) }