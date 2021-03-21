#
# constants
#

# paths
if (sessionInfo()$R.version$os == "linux-gnu") {
  DATAFILEPATH <- "~/"
} else {
  DATAFILEPATH <- "./"
}
DATAFILE <- paste0(DATAFILEPATH, "securitydata.rds")
SECURITYFILE <- paste0(DATAFILEPATH, "securityticker.csv")

# colors
MAYABLUE <- "#6ECFF6" # Maya Blue like in the smartcube logo
LINESTYLE <-
  paste0("border:none; height:2px; background-color:", MAYABLUE)

# Scaling of financial data
SCALING <- 252

# stock dates
DATASTARTDATE <- "2020-01-01"
INDEXSTARTDATE <- "2020-01-02"
INDEXSTARTVALUE <- 100
