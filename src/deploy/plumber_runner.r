# Code based on https://www.shirin-glander.de/2018/01/plumber/

# https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them

list_of_packages <- c("tidyverse", "plumber")
new.packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]

if (length(new.packages)) {
  print("Installing packages\n")
  install.packages(new.packages())
}

library(plumber)

models_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
print(models_path)

r <- plumb(paste(models_path, "/plumber_loan_sharks_models.r", sep = ""))
r$run(port = 8000)