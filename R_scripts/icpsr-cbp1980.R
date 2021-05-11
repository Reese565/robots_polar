# Title: ICPSR-CBP 1980
# Description: takes the raw ASCII files for the 1980 County Business Patterns
# survey and transforms them into a dataframe. The raw ASCI files are split
# for the counties by each of the nine Census Divisions.

library(stringr)
library(foreign)

widths <- c(2, 3, 1, 4, 1, 1, 12, 12, 12,
            6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
            6, 6, 6, 6, 1, 2, 3, 1, 2, 3, 6)


col_names <- c("STATE2", "COUNTY2", "FILL1", "SICCODE2", "FILL2", "FLAG",
               "TEMPMM", "TPAYQ1", "TANPAY", "TESTAB", "CTYEMP1", "CTYEMP2",
               "CTYEMP3", "CTYEMP4", "CTYEMP5", "CTYEMP6", "CTYEMP7",
               "CTYEMP8", "CTYEMP9", "CTYEMP10", "CTYEMP11", "CTYEMP12",
               "CTYEMP13", "FILL3", "SSASTAT2", "SSACTY2", "FILL4", "FIPSTATE",
               "FIPSCTY2", "FILL5")

cbp80_dir_path <- "../data/icpsr/cbp1980"
cbp80_dirs <- list.dirs(cbp80_dir_path)
subdirs_boolean <- str_detect(cbp80_dirs, "DS")
cbp80_subdirs <- cbp80_dirs[subdirs_boolean]
cbp80_files <- paste0(cbp80_subdirs, "/", sapply(cbp80_subdirs, list.files))

cbp80_dfs <- lapply(cbp80_files, read.fwf,
                    widths = widths,
                    col.names = tolower(col_names))

cbp80_df <- rbind(cbp80_dfs[[2]], cbp80_dfs[[2]])

for (i in 3:length(cbp80_dfs)){
  cbp80_df <- rbind(cbp80_df, cbp80_dfs[[i]])
}


unlist(str_extract_all(cbp80_df$siccode2, "[^0-9/-]+"))
cbp80_df[str_detect(cbp80_df$siccode2, "\\s+"),"siccode2"] <- "07--"

cbp80_df$flag <- as.character(cbp80_df$flag)

write.dta(cbp80_df, "../data/icpsr/cbp1980.dta")









read_all <- function(directory, type = "csv",...){}




