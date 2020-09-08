# Create

named_urls <- c(corelogic = "https://github.com/MattCowgill/macro_dashboard_data/blob/master/data/corelogic/corelogic_daily.fst?raw=true",
                lfs_m = "https://github.com/MattCowgill/macro_dashboard_data/blob/master/data/abs/6202.fst?raw=true")

usethis::use_data(named_urls, internal = TRUE, overwrite = TRUE)
