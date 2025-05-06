#!/usr/bin/env Rscript

library(readr)
library(dplyr)

read_numbers_to_tibble <- function(file_path, column_name) {
  numbers <- read_lines(file_path)

  numbers <- as.numeric(numbers)

  tibble(!!column_name := numbers)
}

# Get the folder where timigns are from the args 
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
    stop("Please provide the folder where the timings are")
}
folder <- args[1]
if (!dir.exists(folder)) {
    stop("The folder does not exist")
}
 
# Read the timing files 
java <- read_numbers_to_tibble(file.path(folder, "javatimings.csv"), "java")
r <- read_numbers_to_tibble(file.path(folder, "origtimings.csv"), "r")
rust <- read_numbers_to_tibble(file.path(folder, "rusttimings.csv"), "rust")

# Combine the data into a single tibble
timings <- java %>%
  bind_cols(r) %>%
  bind_cols(rust) %>%
  mutate(
    java = as.numeric(java),
    r = as.numeric(r),
    rust = as.numeric(rust)
  )


timings <- timings %>%
  mutate(
    java_sp = r / java,#speedup of java over R
    rust_sp = r / rust
  )

# Calculate the mean and standard deviation for each column
# including the speedup columns, and also compute speedups of the 
# java and rust timings over the R timings

timings_summary <- timings %>%
  summarise(
    java_mean = mean(java, na.rm = TRUE),
    java_sd = sd(java, na.rm = TRUE),
    r_mean = mean(r, na.rm = TRUE),
    r_sd = sd(r, na.rm = TRUE),
    rust_mean = mean(rust, na.rm = TRUE),
    rust_sd = sd(rust, na.rm = TRUE),
    java_sp_mean = mean(java_sp, na.rm = TRUE),
    java_sp_sd = sd(java_sp, na.rm = TRUE),
    rust_sp_mean = mean(rust_sp, na.rm = TRUE),
    rust_sp_sd = sd(rust_sp, na.rm = TRUE),
    java_t_sp_mean =  r_mean / java_mean,
    rust_t_sp_mean =  r_mean / rust_mean,
)


# Print the summary in a user-friendy way 
cat("Java mean: ", timings_summary$java_mean, "\n")
cat("Java sd: ", timings_summary$java_sd, "\n")
cat("R mean: ", timings_summary$r_mean, "\n")
cat("R sd: ", timings_summary$r_sd, "\n")
cat("Rust mean: ", timings_summary$rust_mean, "\n")
cat("Rust sd: ", timings_summary$rust_sd, "\n")
cat("Java speedup mean: ", timings_summary$java_sp_mean, "\n")
cat("Java speedup sd: ", timings_summary$java_sp_sd, "\n")
cat("Rust speedup mean: ", timings_summary$rust_sp_mean, "\n")
cat("Rust speedup sd: ", timings_summary$rust_sp_sd, "\n")
cat("Java speedup over R mean: ", timings_summary$java_t_sp_mean, "\n")
cat("Rust speedup over R mean: ", timings_summary$rust_t_sp_mean, "\n")

# Save the summary to a CSV file
output_file <- file.path(folder, "timings_summary.csv")
write_csv(timings_summary, output_file)
cat("Summary saved to", output_file, "\n")

# Save the timings to a CSV file
output_file <- file.path(folder, "timings.csv")
write_csv(timings, output_file)
cat("Timings saved to", output_file, "\n")