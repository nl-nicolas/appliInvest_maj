usethis::use_build_ignore("devstuff_history.R")
usethis::use_data_raw()

base_invest_com <- read.csv2("data-raw/base_invest_com.csv")
usethis::use_data(base_invest_com,overwrite = T)

base_invest_gfp <- read.csv2("data-raw/base_invest_gfp.csv")
usethis::use_data(base_invest_gfp,overwrite = T)

base_invest_dep <- read.csv2("data-raw/base_invest_dep.csv")
usethis::use_data(base_invest_dep,overwrite = T)

base_invest_reg <- read.csv2("data-raw/base_invest_reg.csv")
usethis::use_data(base_invest_reg,overwrite = T)

devtools::use_package("plotly")
devtools::use_package("jsonlite")