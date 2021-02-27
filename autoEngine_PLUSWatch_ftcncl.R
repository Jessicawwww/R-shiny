library(lubridate)
library(readr)
library(RJDBC)
library(randomForest)
library(wellMatch)
drv = JDBC("com.teradata.jdbc.TeraDriver","/home/vincechen/JDBC/terajdbc4.jar:/home/vincechen/JDBC/tdgssconfig.jar")
link <- "jdbc:teradata://mozart.vip.ebay.com/TMODE=ANSI"  # Change Mozart or Hopper here
uid <- ""
password <- ""
conn <- dbConnect(drv, link, uid, password)
load("/home/vincechen/ShinyApps/pluswatch/ft_cncl_model_1220.RData")

query <- readr::read_file("/home/vincechen/ShinyApps/pluswatch/ft_cncl_pred.sql")
sql <- unlist(strsplit(query, ";"))
for(i in 1:length(sql)){
  empty <- tryCatch({ RJDBC::dbSendUpdate(conn, sql[i]) }, error = function(i) {return(NA)})
}
test <- dbGetQuery(conn, "SELECT * FROM test_set;")

test$ft_start_dt <- as.Date(test$ft_start_dt)
test <- subset(test, ft_start_dt >= "2018-06-14")
test <- test[order(test$ft_start_dt), ]

# test <- subset(test, member_cnt >= 100)
test$c_rate_0d <- test$cncl_cnt_0d / test$member_cnt
test$c_rate_1d <- test$cncl_cnt_1d / test$member_cnt
test$c_rate_2d <- test$cncl_cnt_2d / test$member_cnt
test$c_rate_3d <- test$cncl_cnt_3d / test$member_cnt
test$c_rate_4d <- test$cncl_cnt_4d / test$member_cnt
test$c_rate_5d <- test$cncl_cnt_5d / test$member_cnt
test$c_rate_30d <- test$cncl_cnt_tot / test$member_cnt

test$c_ror_1d <- test$c_rate_1d / test$c_rate_0d
test$c_ror_2d <- test$c_rate_2d / test$c_rate_0d
test$c_ror_3d <- test$c_rate_3d / test$c_rate_0d
test$c_ror_4d <- test$c_rate_4d / test$c_rate_0d
test$c_ror_5d <- test$c_rate_4d / test$c_rate_0d

test[is.na(test)] <- 0


test$c_rate_act <- round(test$cncl_cnt_tot / test$member_cnt, 4)
test$c_rate_pred <- round(predict(rf_model, test), 4)

test$lower_pct <- test$c_rate_pred - 1.28 * 0.037
test$upper_pct <- test$c_rate_pred + 1.28 * 0.037
test$cncl_cnt_pred <- round(test$member_cnt * test$c_rate_pred)
test$cncl_cnt_pred_low <- round(test$member_cnt * test$lower_pct)
test$cncl_cnt_pred_up <- round(test$member_cnt * test$upper_pct)

test <- test[, c("ft_start_dt", "member_cnt", "cncl_cnt_tot", "cncl_cnt_0d", "cncl_cnt_1d", "cncl_cnt_2d", "cncl_cnt_3d", "c_rate_act", "c_rate_pred",
                 "lower_pct", "upper_pct", "cncl_cnt_pred", "cncl_cnt_pred_low", "cncl_cnt_pred_up")]

test <- subset(test, ft_start_dt <= (Sys.Date()-4))
test <- test[order(test$ft_start_dt, decreasing = T), ]
test$c_rate_pred[test$member_cnt <= 100] <- NA
ft_cncl_pred_data <- test

save(ft_cncl_pred_data, file = "/home/vincechen/ShinyApps/pluswatch/RData/ft_cncl_pred_data.RData")

wellMatch::miniWriteTable(conn, "P_AU_MKT_T.AU_PLUS_FT_CNCL_RATE_PRED", ft_cncl_pred_data)
