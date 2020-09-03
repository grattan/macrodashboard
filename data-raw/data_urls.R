# Create

named_urls <- c(corelogic = "https://raw.githubusercontent.com/MattCowgill/macro_dashboard_data/master/data/corelogic_daily.csv",
                cpi = "https://raw.githubusercontent.com/MattCowgill/macro_dashboard_data/master/data/cpi.csv")

usethis::use_data(named_urls, internal = TRUE, overwrite = TRUE)
