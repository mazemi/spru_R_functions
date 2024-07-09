# The aim of this script is to generate longitudinal and snapshot data for JMMI dashboard.
# Place the latest datamerge and median prices in the input folders.

# set paths
current_datamerge_path <- "./input/current_data/datamerge_JMMI_R48_Jun24.csv"
tool_path <- "./misc_data/JMMI_Tool_combinded_v3_old_codes.xlsx"

# core longitudinal process functions
source("./R/process_indicator.R")
source("./R/process_price.R")
source("./R/process_snapshot_price.R")

# generate JMMI longitudinal data
process_jmmi_indicators(current_datamerge_path, tool_path)
process_jmmi_prices(tool_path)

# Generate a price snapshot by comparing the current month with the previous month, 
# and the past six months extracted the longitudinal_prices.csv dataset.
generate_snapshot()
