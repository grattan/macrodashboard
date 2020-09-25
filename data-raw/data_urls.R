# Create

named_urls <- c(corelogic = "https://github.com/MattCowgill/macro_dashboard_data/blob/master/data/corelogic/corelogic_daily.fst?raw=true",
                lfs_m_1 = "https://github.com/MattCowgill/macro_dashboard_data/blob/master/data/abs/lfs_m_1.fst?raw=true",
                lfs_m_12 = "https://github.com/MattCowgill/macro_dashboard_data/blob/master/data/abs/lfs_m_12.fst?raw=true",
                lfs_m_19 = "https://github.com/MattCowgill/macro_dashboard_data/blob/master/data/abs/lfs_m_19.fst?raw=true",
                lfs_m_22 = "https://github.com/MattCowgill/macro_dashboard_data/blob/master/data/abs/lfs_m_22.fst?raw=true",
                lfs_m_24 = "https://github.com/MattCowgill/macro_dashboard_data/blob/master/data/abs/lfs_m_24.fst?raw=true")

usethis::use_data(named_urls, internal = TRUE, overwrite = TRUE)
