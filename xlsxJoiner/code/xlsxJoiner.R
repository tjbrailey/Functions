#' Big data joiner thing 
#' @param file_location 
#' @param name_of_data
#' @param number_of_tabs
#' @param join_variables



files <- list.files(paste0(here::here(), '/data/manual_download/'), "\\.xlsx$", full.names = TRUE)

read_excel_allsheets <- function(filename) { 
  sheets <- readxl::excel_sheets(filename) 
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X)) 
  names(x) <- sheets 
  x 
} 

out <- lapply(files, read_excel_allsheets)
basename(files)

# Initiate empty list
by_year=list()

# By removing the assign function, you can avoid printing all the datasets into the workspace
for(i in 1:57){
  by_year[i] = list(assign(paste0('IISS_equipdata_', i+1960), plyr::ldply(out[[i]], data.frame)))
}

# Join all data and save file

IISS_full <- do.call(rbind.data.frame, by_year)
# Remove .id column 
IISS_full[,".id"] <- NULL

saveRDS(IISS_full, paste0(here::here(), "/data/","IISS.rds"))