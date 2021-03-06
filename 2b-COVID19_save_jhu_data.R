# Store data
# Make directory to store data if it does not already exist
mainDir <- "~/COVID19/COVID19SIR/"
subDir  <- "jhu_ts_data/"

ifelse(!dir.exists(file.path(mainDir, subDir)),
       dir.create(file.path(mainDir, subDir)), FALSE)
# dir.create() doesn't crash if the directory exists but prints a warning.
# To suppress warnings, set option: showWarnings = FALSE
# returns FALSE if the directory already exists or is un-creatable, and 
# TRUE if it didn't exist but was successfully created.
# note: it's not good practice to use ifelse() for non-vectorised branching

file_name <- paste0("jhu_ts_data_US_", Sys.Date(), ".csv")

# Save file
write_csv(jhu_US_data,
       file = paste0(mainDir, subDir, file_name),
       append = FALSE,
       col_names = TRUE)
