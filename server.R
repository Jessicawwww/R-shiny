library(DT)
library(dplyr)
library(ggplot2) 

load("/home/vincechen/ShinyApps/pluswatch/RData/data_pack.RData")     
load("/home/vincechen/ShinyApps/pluswatch/RData/cohort_facts.RData") 
load("/home/vincechen/ShinyApps/pluswatch/RData/ft_cncl_pred_data.RData") 

monthly$cohort <- as.character(monthly$cohort)
mapping <- data.frame(no_of_month = levels(as.factor(monthly$no_of_month)),
                      sequence = 1:length(levels(as.factor(monthly$no_of_month))))
monthly <- merge(monthly, mapping, by = "no_of_month", all.x = TRUE)

selected_cohort <- as.numeric(unique(monthly$cohort))
selected_cohort <- selected_cohort[order(selected_cohort)][(length(selected_cohort)-4):length(selected_cohort)]

# monthly2$cohort <- as.character(monthly2$cohort)
# monthly3$cohort <- as.character(monthly3$cohort)
# monthly2 <- merge(monthly2, mapping, by = "no_of_month", all.x = TRUE)
# monthly3 <- merge(monthly3, mapping, by = "no_of_month", all.x = TRUE) 


# App body
shinyServer(function(input, output, session){ 
  
  output$filter1 <- renderUI({
    selectInput("filter1", "PLUS Cohort", choices = c("Select...", levels(as.factor(monthly$cohort))), 
                # selected = c('201906', '201907', '201908', '201909', '201910'), 
                selected = selected_cohort, 
                multiple = TRUE)
  })

  ### For Marketing View ----
  
  # Pure PLUS x Total GMV
  output$plot1 <- renderPlot({
    input_df <- subset(monthly, cohort %in% input$filter1)
    ord <- levels(as.factor(monthly$no_of_month))
    ggplot(input_df, aes(x = sequence)) +
      geom_line(aes(y = uplift_tot, col = cohort), size = 1) +
      labs(title = "Incrementality Tracking",
           # subtitle = "Still under testing",
           x = "Month",
           y = "Incremental %") +
      scale_x_discrete(limits = ord) +
      theme_classic()
  })
  
  output$table1 <- DT::renderDataTable(DT::datatable({
    tbl <- subset(uplift_tot, cohort %in% input$filter1)
    tbl
  },
  rownames = FALSE,
  extensions = "Buttons",
  options = list(pageLength = 24, 
                 dom = 'Bfrtip', buttons = c('excel', 'csv', 'copy'),
                 scrollX = TRUE)
  ))
 
  # Pure PLUS x CPN GMV
  output$plot2 <- renderPlot({
    input_df <- subset(monthly, cohort %in% input$filter1)
    ord <- levels(as.factor(monthly$no_of_month))
    ggplot(input_df, aes(x = sequence)) +
      geom_line(aes(y = uplift_cpn, col = cohort), size = 1) +
      labs(title = "Incrementality Tracking",
           # subtitle = "Still under testing",
           x = "Month",
           y = "Incremental %") +
      scale_x_discrete(limits = ord) +
      theme_classic()
  })
  
  output$table2 <- DT::renderDataTable(DT::datatable({
    tbl <- subset(uplift_cpn, cohort %in% input$filter1)
    tbl
  },
  rownames = FALSE,
  extensions = "Buttons",
  options = list(pageLength = 24, 
                 dom = 'Bfrtip', buttons = c('excel', 'csv', 'copy'),
                 scrollX = TRUE)
  ))
  
  
  # Pure PLUS x Organic GMV
  output$plot3 <- renderPlot({
    input_df <- subset(monthly, cohort %in% input$filter1)
    ord <- levels(as.factor(monthly$no_of_month))
    ggplot(input_df, aes(x = sequence)) +
      geom_line(aes(y = uplift_org, col = cohort), size = 1) +
      labs(title = "Incrementality Tracking",
           # subtitle = "Still under testing",
           x = "Month",
           y = "Incremental %") +
      scale_x_discrete(limits = ord) +
      theme_classic()
  })
  
  output$table3 <- DT::renderDataTable(DT::datatable({
    tbl <- subset(uplift_org, cohort %in% input$filter1)
    tbl
  },
  rownames = FALSE,
  extensions = "Buttons",
  options = list(pageLength = 24, 
                 dom = 'Bfrtip', buttons = c('excel', 'csv', 'copy'),
                 scrollX = TRUE)
  ))
  
  
  # # (PLUS + Fly) x Total GMV
  # output$plot1_2 <- renderPlot({ 
  #   input_df <- subset(monthly2, cohort %in% input$filter1)
  #   ord <- levels(as.factor(monthly2$no_of_month))
  #   ggplot(input_df, aes(x = sequence)) +
  #     geom_line(aes(y = uplift_tot, col = cohort), size = 1) +
  #     labs(title = "Incrementality Tracking",
  #          subtitle = "Still under testing",
  #          x = "Month",
  #          y = "Incremental %") +
  #     scale_x_discrete(limits = ord) +
  #     theme_classic()
  # })
  # 
  # output$table1_2 <- DT::renderDataTable(DT::datatable({
  #   tbl <- subset(uplift_tot2, cohort %in% input$filter1)
  #   tbl
  # },
  # rownames = FALSE,
  # options = list(pageLength = 24)
  # ))
  # 
  # 
  # # (PLUS + Fly) x CPN GMV
  # output$plot2_2 <- renderPlot({
  #   input_df <- subset(monthly2, cohort %in% input$filter1)
  #   ord <- levels(as.factor(monthly$no_of_month))
  #   ggplot(input_df, aes(x = sequence)) +
  #     geom_line(aes(y = uplift_cpn, col = cohort), size = 1) +
  #     labs(title = "Incrementality Tracking",
  #          subtitle = "Still under testing",
  #          x = "Month",
  #          y = "Incremental %") +
  #     scale_x_discrete(limits = ord) +
  #     theme_classic()
  # })
  # 
  # output$table2_2 <- DT::renderDataTable(DT::datatable({
  #   tbl <- subset(uplift_cpn2, cohort %in% input$filter1)
  #   tbl
  # },
  # rownames = FALSE,
  # options = list(pageLength = 24)
  # ))
  # 
  # 
  # # (PLUS + Fly) x Organic GMV
  # output$plot3_2 <- renderPlot({
  #   input_df <- subset(monthly2, cohort %in% input$filter1)
  #   ord <- levels(as.factor(monthly$no_of_month))
  #   ggplot(input_df, aes(x = sequence)) +
  #     geom_line(aes(y = uplift_org, col = cohort), size = 1) +
  #     labs(title = "Incrementality Tracking",
  #          subtitle = "Still under testing",
  #          x = "Month",
  #          y = "Incremental %") +
  #     scale_x_discrete(limits = ord) +
  #     theme_classic()
  # })
  # 
  # output$table3_2 <- DT::renderDataTable(DT::datatable({
  #   tbl <- subset(uplift_org2, cohort %in% input$filter1)
  #   tbl
  # },
  # rownames = FALSE,
  # options = list(pageLength = 24)
  # ))
  # 
  # 
  # 
  # 
  # 
  # 
  # # Overall PLUS x Total GMV
  # output$plot1_3 <- renderPlot({
  #   input_df <- subset(monthly3, cohort %in% input$filter1)
  #   ord <- levels(as.factor(monthly$no_of_month))
  #   ggplot(input_df, aes(x = sequence)) +
  #     geom_line(aes(y = uplift_tot, col = cohort), size = 1) +
  #     labs(title = "Incrementality Tracking",
  #          subtitle = "Still under testing",
  #          x = "Month",
  #          y = "Incremental %") +
  #     scale_x_discrete(limits = ord) +
  #     theme_classic()
  # })
  # 
  # output$table1_3 <- DT::renderDataTable(DT::datatable({
  #   tbl <- subset(uplift_tot3, cohort %in% input$filter1)
  #   tbl
  # },
  # rownames = FALSE,
  # options = list(pageLength = 24)
  # ))
  # 
  # 
  # # Overall PLUS x CPN GMV
  # output$plot2_3 <- renderPlot({
  #   input_df <- subset(monthly3, cohort %in% input$filter1)
  #   ord <- levels(as.factor(monthly$no_of_month))
  #   ggplot(input_df, aes(x = sequence)) +
  #     geom_line(aes(y = uplift_cpn, col = cohort), size = 1) +
  #     labs(title = "Incrementality Tracking",
  #          subtitle = "Still under testing",
  #          x = "Month",
  #          y = "Incremental %") +
  #     scale_x_discrete(limits = ord) +
  #     theme_classic()
  # })
  # 
  # output$table2_3 <- DT::renderDataTable(DT::datatable({
  #   tbl <- subset(uplift_cpn3, cohort %in% input$filter1)
  #   tbl
  # },
  # rownames = FALSE,
  # options = list(pageLength = 24)
  # ))
  # 
  # 
  # # Overall PLUS x Organic GMV
  # output$plot3_3 <- renderPlot({
  #   input_df <- subset(monthly3, cohort %in% input$filter1)
  #   ord <- levels(as.factor(monthly$no_of_month))
  #   ggplot(input_df, aes(x = sequence)) +
  #     geom_line(aes(y = uplift_org, col = cohort), size = 1) +
  #     labs(title = "Incrementality Tracking",
  #          subtitle = "Still under testing",
  #          x = "Month",
  #          y = "Incremental %") +
  #     scale_x_discrete(limits = ord) +
  #     theme_classic()
  # })
  # 
  # output$table3_3 <- DT::renderDataTable(DT::datatable({
  #   tbl <- subset(uplift_org3, cohort %in% input$filter1)
  #   tbl
  # },
  # rownames = FALSE,
  # options = list(pageLength = 24)
  # ))
  
  

  
  ### For FP&A View ----
  # output$fin_incre <- DT::renderDataTable(DT::datatable({
  #   tbl <- subset(uplift_tot_f, cohort %in% input$filter1)
  #   tbl
  # },
  # rownames = FALSE,
  # options = list(pageLength = 24)
  # ))
  
  output$table_f_tot <- DT::renderDataTable(DT::datatable({
    tbl <- monthly_f[ ,c("yrmo", "gmb_avg_actual_tot", "gmb_avg_pred_tot", "uplift_tot")]
    tbl
  },
  rownames = FALSE,
  colnames = c("Year/ Month", "Per Member GMV (Actual)", "Per Member GMV (Predicted)", "Uplift%"),
  extensions = "Buttons",
  options = list(pageLength = 24, dom = 'Bfrtip', buttons = c('excel', 'csv', 'copy'))
  ) %>%
    formatCurrency(2:3, currency = "$", digit = 1) %>%
    formatPercentage(c(4), digit = 1) %>%
    formatStyle(c("uplift_tot"), font = "bold"))
    
  
  
  output$table_f_cpn <- DT::renderDataTable(DT::datatable({
    tbl <- monthly_f[ ,c("yrmo", "gmb_avg_actual_cpn", "gmb_avg_pred_cpn", "uplift_cpn")]
    tbl
  },
  rownames = FALSE,
  colnames = c("Year/ Month", "Per Member GMV (Actual)", "Per Member GMV (Predicted)", "Uplift%"),
  extensions = "Buttons",
  options = list(pageLength = 24, dom = 'Bfrtip', buttons = c('excel', 'csv', 'copy'))
  ) %>%
    formatCurrency(2:3, currency = "$", digit = 1) %>%
    formatPercentage(c(4), digit = 1) %>%
    formatStyle(c("uplift_cpn"), font = "bold"))
  
  
  output$table_f_org <- DT::renderDataTable(DT::datatable({
    tbl <- monthly_f[ ,c("yrmo", "gmb_avg_actual_org", "gmb_avg_pred_org", "uplift_org")]
    tbl
  },
  rownames = FALSE,
  colnames = c("Year/ Month", "Per Member GMV (Actual)", "Per Member GMV (Predicted)", "Uplift%"),
  extensions = "Buttons",
  options = list(pageLength = 24, dom = 'Bfrtip', buttons = c('excel', 'csv', 'copy'))
  ) %>%
    formatCurrency(2:3, currency = "$", digit = 1) %>%
    formatPercentage(c(4), digit = 1) %>%
    formatStyle(c("uplift_org"), font = "bold"))
  
  
  
  output$table_f_tot_allsum <- DT::renderDataTable(DT::datatable({
    tbl <- monthly_f[ ,c("yrmo", "test_gmb", "pred_tot", "uplift_tot")]
    tbl
  },
  rownames = FALSE,
  colnames = c("Year/ Month", "Sum of GMV (Actual)", "Sum of GMV (Predicted)", "Uplift%"),
  extensions = "Buttons",
  options = list(pageLength = 24, dom = 'Bfrtip', buttons = c('excel', 'csv', 'copy'))
  ) %>%
    formatCurrency(2:3, currency = "$", digit = 1) %>%
    formatPercentage(c(4), digit = 1) %>%
    formatStyle(c("uplift_tot"), font = "bold"))
  
  
  
  output$table_f_cpn_allsum <- DT::renderDataTable(DT::datatable({
    tbl <- monthly_f[ ,c("yrmo", "actual_cpn", "pred_cpn", "uplift_cpn")]
    tbl
  },
  rownames = FALSE,
  colnames = c("Year/ Month", "Sum of GMV (Actual)", "Sum of GMV (Predicted)", "Uplift%"),
  extensions = "Buttons",
  options = list(pageLength = 24, dom = 'Bfrtip', buttons = c('excel', 'csv', 'copy'))
  ) %>%
    formatCurrency(2:3, currency = "$", digit = 1) %>%
    formatPercentage(c(4), digit = 1) %>%
    formatStyle(c("uplift_cpn"), font = "bold"))
  
  
  output$table_f_org_allsum <- DT::renderDataTable(DT::datatable({
    tbl <- monthly_f[ ,c("yrmo", "actual_org", "pred_org", "uplift_org")]
    tbl
  },
  rownames = FALSE,
  colnames = c("Year/ Month", "Sum of GMV (Actual)", "Sum of GMV (Predicted)", "Uplift%"),
  extensions = "Buttons",
  options = list(pageLength = 24, dom = 'Bfrtip', buttons = c('excel', 'csv', 'copy'))
  ) %>%
    formatCurrency(2:3, currency = "$", digit = 1) %>%
    formatPercentage(c(4), digit = 1) %>%
    formatStyle(c("uplift_org"), font = "bold"))
  
  
  
  output$cohort_summary <- DT::renderDataTable(DT::datatable({
    tbl <- q_fact[ ,c("COHORT_YEAR", "COHORT_MONTH", "COHORT_SIZE", "ACTIVE_BUYER", "active_rate",
                      "CPN_USER", "cpn_rate", "WAIVER_USER", "waiver_rate", "FREE_RETURN_USER", "return_rate")]
    tbl$cohort_month_tmp <- ifelse(nchar(tbl$COHORT_MONTH) == 1, paste0("0", tbl$COHORT_MONTH), tbl$COHORT_MONTH)
    tbl$cohort_month_tmp <- paste(tbl$COHORT_YEAR, tbl$cohort_month_tmp, sep = "")
    tbl <- subset(tbl, cohort_month_tmp %in% input$filter1)
    tbl$cohort_month_tmp <- NULL
    
    topline <- as.data.frame(t(as.data.frame(colMeans(tbl))))
    topline$COHORT_YEAR <- "Cohort"
    topline$COHORT_MONTH <- "Summary"
    topline$active_rate <- topline$ACTIVE_BUYER / topline$COHORT_SIZE
    topline$cpn_rate <- topline$CPN_USER / topline$COHORT_SIZE
    topline$waiver_rate <- topline$WAIVER_USER / topline$COHORT_SIZE
    topline$return_rate <- topline$FREE_RETURN_USER / topline$COHORT_SIZE

    tbl <- rbind(topline, tbl)
    tbl
  },
  rownames = FALSE,
  colnames = c("Cohort Year", "Cohort Month", "# of Members", "Avg. Active Buyers", "% Active",
               "Avg. CPN Users", "% CPN", "Avg. Waiver Users", "% Waiver", "Avg. Free Return Users", "% Free Return"),
  extensions = "Buttons",
  options = list(pageLength = 24, dom = 'Bfrtip', buttons = c('excel', 'csv', 'copy'))
  ) %>%
    formatRound(c("COHORT_SIZE", "ACTIVE_BUYER", "CPN_USER", "WAIVER_USER", "FREE_RETURN_USER"), digit = 0) %>%
    formatPercentage(c("active_rate", "cpn_rate", "waiver_rate", "return_rate"), digit = 1) %>%
    formatStyle("COHORT_YEAR", target = "row", backgroundColor = styleEqual("Cohort", "gold")) %>%
    formatStyle("COHORT_YEAR", target = "row", fontWeight = styleEqual("Cohort", "bold"))
  )
  
  
  
  output$cohort_waiver <- DT::renderDataTable(DT::datatable({
    tbl <- q_fact[ ,c("COHORT_YEAR", "COHORT_MONTH", "COHORT_SIZE", "WAIVER_AMT", "WAIVER_USER", "waiver_rate")]
    tbl$waiver_per_mbr <- round(tbl$WAIVER_AMT / tbl$COHORT_SIZE, 1)
    tbl$waiver_per_usr <- round(tbl$WAIVER_AMT / tbl$WAIVER_USER, 1)
    tbl <- tbl[ ,c("COHORT_YEAR", "COHORT_MONTH", "COHORT_SIZE", "WAIVER_USER", "waiver_rate", "WAIVER_AMT", "waiver_per_mbr", "waiver_per_usr")]
    tbl$cohort_month_tmp <- ifelse(nchar(tbl$COHORT_MONTH) == 1, paste0("0", tbl$COHORT_MONTH), tbl$COHORT_MONTH)
    tbl$cohort_month_tmp <- paste(tbl$COHORT_YEAR, tbl$cohort_month_tmp, sep = "")
    tbl <- subset(tbl, cohort_month_tmp %in% input$filter1)
    tbl$cohort_month_tmp <- NULL
    
    topline <- as.data.frame(t(as.data.frame(colMeans(tbl))))
    topline$COHORT_YEAR <- "Cohort"
    topline$COHORT_MONTH <- "Summary"
    topline$waiver_rate <- topline$WAIVER_USER / topline$COHORT_SIZE
    topline$waiver_per_mbr <- round(topline$WAIVER_AMT / topline$COHORT_SIZE, 1)
    topline$waiver_per_usr <- round(topline$WAIVER_AMT / topline$WAIVER_USER, 1)
    
    tbl <- rbind(topline, tbl)
    tbl
  },
  rownames = FALSE,
  colnames = c("Cohort Year", "Cohort Month", "# of Members", "Avg. Waiver Users", "Waiver Usage Rate",
               "Avg. Waiver Amount (Monthly)", "Waiver per Member", "Waiver per Waiver User"),
  extensions = "Buttons",
  options = list(pageLength = 24, dom = 'Bfrtip', buttons = c('excel', 'csv', 'copy'))
  ) %>%
    formatRound(c("COHORT_SIZE", "WAIVER_AMT", "WAIVER_USER"), digit = 0) %>%
    formatCurrency(c("waiver_per_mbr", "waiver_per_usr"), currency = "$", digit = 1) %>%
    formatPercentage("waiver_rate", digit = 1) %>%
    formatStyle("COHORT_YEAR", target = "row", backgroundColor = styleEqual("Cohort", "gold")) %>%
    formatStyle("COHORT_YEAR", target = "row", fontWeight = styleEqual("Cohort", "bold"))
  ) 
  
  
  

  
  output$cohort_txn <- DT::renderDataTable(DT::datatable({
    tbl <- q_fact[ ,c("COHORT_YEAR", "COHORT_MONTH", "COHORT_SIZE", "ACTIVE_BUYER", "TOT_GMV", "CPN_GMV", "ORG_GMV",
                      "COLES_GMV", "AU_BI", "AU_PLUS_BI")]
    tbl$cpn_penetraion <- round(tbl$CPN_GMV / tbl$TOT_GMV, 3)
    tbl$abp <- round(tbl$TOT_GMV / tbl$AU_BI, 3)
    tbl$plus_bi_pene <- round(tbl$AU_PLUS_BI / tbl$AU_BI, 3)
    tbl <- tbl[ ,c("COHORT_YEAR", "COHORT_MONTH", "COHORT_SIZE", "ACTIVE_BUYER", "TOT_GMV", "CPN_GMV", "ORG_GMV",
                   "cpn_penetraion", "COLES_GMV", "AU_BI", "abp", "AU_PLUS_BI", "plus_bi_pene")]
    tbl$cohort_month_tmp <- ifelse(nchar(tbl$COHORT_MONTH) == 1, paste0("0", tbl$COHORT_MONTH), tbl$COHORT_MONTH)
    tbl$cohort_month_tmp <- paste(tbl$COHORT_YEAR, tbl$cohort_month_tmp, sep = "")
    tbl <- subset(tbl, cohort_month_tmp %in% input$filter1)
    tbl$cohort_month_tmp <- NULL
    
    topline <- as.data.frame(t(as.data.frame(colMeans(tbl))))
    topline$COHORT_YEAR <- "Cohort"
    topline$COHORT_MONTH <- "Summary"
    topline$cpn_penetraion <- round(topline$CPN_GMV / topline$TOT_GMV, 3)
    topline$abp <- round(topline$TOT_GMV / topline$AU_BI, 3)
    topline$plus_bi_pene <- round(topline$AU_PLUS_BI / topline$AU_BI, 3)

    tbl <- rbind(topline, tbl)
    tbl
  },
  rownames = FALSE,
  colnames = c("Cohort Year", "Cohort Month", "# of Members", "Avg. Active Buyers", "Total GMV", "CPN GMV", "Organic GMV",
               "% CPN", "Coles GMV", "Bought Items", "Avg. Buying Price", "PLUS Items", "% PLUS BI"),
  extensions = "Buttons",
  options = list(pageLength = 24, dom = 'Bfrtip', buttons = c('excel', 'csv', 'copy'))
  ) %>%
    formatRound(c("COHORT_SIZE", "ACTIVE_BUYER", "AU_BI", "AU_PLUS_BI"), digit = 0) %>%
    formatCurrency(c("TOT_GMV", "CPN_GMV", "ORG_GMV", "COLES_GMV", "abp"), currency = "$", digit = 0) %>%
    formatPercentage(c("cpn_penetraion", "plus_bi_pene"), digit = 1) %>%
    formatStyle("COHORT_YEAR", target = "row", backgroundColor = styleEqual("Cohort", "gold")) %>%
    formatStyle("COHORT_YEAR", target = "row", fontWeight = styleEqual("Cohort", "bold"))
  )
  
  
  output$cohort_user_cnt <- DT::renderDataTable(DT::datatable({
    tmp1 <- unique(monthly[ ,c("cohort", "user_cnt")]) ; colnames(tmp1) <- c("cohort", "user_cnt_pureplus")
    tmp2 <- unique(monthly2[ ,c("cohort", "user_cnt")]) ; colnames(tmp2) <- c("cohort", "user_cnt_plusfly")
    tmp3 <- unique(monthly3[ ,c("cohort", "user_cnt")]) ; colnames(tmp3) <- c("cohort", "user_cnt_plusall")
    tbl <- merge(tmp1, tmp2, by = "cohort")
    tbl <- merge(tbl, tmp3, by = "cohort")
    tbl <- tbl[order(tbl$cohort, decreasing = T), ]
    tbl
  },
  rownames = FALSE,
  colnames = c("Cohort", "User Count (Pure PLUS)", "User Count (PLUS x Flybuys)", "User Count (Overall PLUS)"), 
  options = list(pageLength = 24)
  ))
  
  
  output$ft_cncl_pred_data <- DT::renderDataTable(DT::datatable({
    ft_cncl_pred_data$cncl_cnt_0d <- NULL
    ft_cncl_pred_data$cncl_cnt_1d <- NULL
    ft_cncl_pred_data$cncl_cnt_2d <- NULL
    ft_cncl_pred_data$cncl_cnt_3d <- NULL
    ft_cncl_pred_data
  },
  rownames = FALSE,
  # colnames = c("FT Cohort", "# FT Member", "# Cancel up to Day3", "# Cancel on Day0", "# Cancel on Day1", "# Cancel on Day2", "# Cancel on Day3", "% Cancel up to Day3", "Predicted % Cancel on Day30", "Upper Bound (80% CI)", "Lower Bound (80% CI)"),
  colnames = c("FT Cohort", "# FT Member", "# Cancel up to Day3", "% Cancel up to Day3", "Predicted % Cancel on Day30", "Lower Bound (80% CI)", "Upper Bound (80% CI)", "Predicted # Cancel", "Predicted # Cancel (Lower)", "Predicted # Cancel (Upper)"),
  extensions = "Buttons",
  options = list(pageLength = input$day_ctrl, dom = 'Bfrtip', buttons = c('excel', 'csv', 'copy'))
  ) %>%
    formatPercentage(c("c_rate_act", "c_rate_pred", "lower_pct", "upper_pct"), digit = 1) %>%
    formatStyle(c("c_rate_pred", "cncl_cnt_pred"), font = "bold")
  )
  
})



##### Old codes

# output$table4_2 <- DT::renderDataTable(DT::datatable({
#   tbl <- subset(monthly2, cohort %in% input$filter1)
#   tbl <- tbl[ ,c("cohort", "user_cnt", "no_of_month", "test_gmb", "pred_tot", "actual_cpn", "pred_cpn", "actual_org", "pred_org")]
#   tbl <- tbl[order(tbl$cohort, tbl$no_of_month, decreasing = T), ]
#   tbl$test_gmb <- round(tbl$test_gmb)
#   tbl$pred_tot <- round(tbl$pred_tot)
#   tbl$actual_cpn <- round(tbl$actual_cpn)
#   tbl$pred_cpn <- round(tbl$pred_cpn)
#   tbl$actual_org <- round(tbl$actual_org)
#   tbl$pred_org <- round(tbl$pred_org)
#   tbl
# },
# rownames = FALSE,
# colnames = c("Cohort", "# of Users", "# of Month", "Total GMB (Test)", "Total GMB (Ctrl)", "Coupon GMB (Test)", "Coupon GMB (Ctrl)", "Organic GMB (Test)", "Organic GMB (Ctrl)"),
# options = list(pageLength = 24)
# ))
# 
# 
# output$table5_2 <- DT::renderDataTable(DT::datatable({
#   tbl <- subset(monthly2, cohort %in% input$filter1)
#   tbl <- tbl[ ,c("cohort", "user_cnt", "no_of_month", "gmb_avg_actual_tot", "gmb_avg_pred_tot", 
#                  "gmb_avg_actual_cpn", "gmb_avg_pred_cpn", "gmb_avg_actual_org", "gmb_avg_pred_org")]
#   tbl <- tbl[order(tbl$cohort, tbl$no_of_month, decreasing = T), ]
#   tbl
# },
# rownames = FALSE,
# colnames = c("Cohort", "# of Users", "# of Month", "Total GMB (Test)", "Total GMB (Ctrl)", "Coupon GMB (Test)", "Coupon GMB (Ctrl)", "Organic GMB (Test)", "Organic GMB (Ctrl)"),
# options = list(pageLength = 24)
# ))




# output$plot1_f <- renderPlot({
#   input_df <- subset(monthly_f, cohort %in% input$filter1)
#   ggplot(input_df, aes(x = as.factor(yrmo))) +
#     geom_line(aes(y = uplift_tot, col = cohort, group = cohort), size = 1) +
#     labs(title = "Incrementality Tracking",
#          subtitle = "Still under testing",
#          x = "Month",
#          y = "Incremental %") +
#     theme_classic()
# })
# 
# 
# output$table1_f <- DT::renderDataTable(DT::datatable({
#   tbl <- subset(uplift_tot_f, cohort %in% input$filter1)
#   tbl
# },
# rownames = FALSE,
# options = list(pageLength = 24)
# ))
# 
# 
# output$plot2_f <- renderPlot({
#   input_df <- subset(monthly_f, cohort %in% input$filter1)
#   ggplot(input_df, aes(x = as.factor(yrmo))) +
#     geom_line(aes(y = uplift_cpn, col = cohort, group = cohort), size = 1) +
#     labs(title = "Incrementality Tracking",
#          subtitle = "Still under testing",
#          x = "Month",
#          y = "Incremental %") +
#     theme_classic()
# })
# 
# output$table2_f <- DT::renderDataTable(DT::datatable({
#   tbl <- subset(uplift_cpn_f, cohort %in% input$filter1)
#   tbl
# },
# rownames = FALSE,
# options = list(pageLength = 24)
# ))
# 
# 
# output$plot3_f <- renderPlot({
#   input_df <- subset(monthly_f, cohort %in% input$filter1)
#   ggplot(input_df, aes(x = as.factor(yrmo))) +
#     geom_line(aes(y = uplift_org, col = cohort, group = cohort), size = 1) +
#     labs(title = "Incrementality Tracking",
#          subtitle = "Still under testing",
#          x = "Month",
#          y = "Incremental %") +
#     theme_classic()
# })
# 
# output$table3_f <- DT::renderDataTable(DT::datatable({
#   tbl <- subset(uplift_org_f, cohort %in% input$filter1)
#   tbl
# },
# rownames = FALSE,
# options = list(pageLength = 24)
# ))
# 
# 
# output$table4_f <- DT::renderDataTable(DT::datatable({
#   tbl <- subset(monthly_f, cohort %in% input$filter1)
#   tbl <- tbl[ ,c("cohort", "user_cnt", "yrmo", "test_gmb", "pred_tot", "actual_cpn", "pred_cpn", "actual_org", "pred_org")]
#   tbl <- tbl[order(tbl$cohort, tbl$yrmo, decreasing = T), ]
#   tbl$test_gmb <- round(tbl$test_gmb)
#   tbl$pred_tot <- round(tbl$pred_tot)
#   tbl$actual_cpn <- round(tbl$actual_cpn)
#   tbl$pred_cpn <- round(tbl$pred_cpn)
#   tbl$actual_org <- round(tbl$actual_org)
#   tbl$pred_org <- round(tbl$pred_org)
#   tbl
# },
# rownames = FALSE,
# colnames = c("Cohort", "# of Users", "Year/Month", "Total GMB (Test)", "Total GMB (Ctrl)", "Coupon GMB (Test)", "Coupon GMB (Ctrl)", "Organic GMB (Test)", "Organic GMB (Ctrl)"),
# options = list(pageLength = 24)
# ))
# 
# 
# output$table5_f <- DT::renderDataTable(DT::datatable({
#   tbl <- subset(monthly_f, cohort %in% input$filter1)
#   tbl <- tbl[ ,c("cohort", "user_cnt", "yrmo", "gmb_avg_actual_tot", "gmb_avg_pred_tot", 
#                  "gmb_avg_actual_cpn", "gmb_avg_pred_cpn", "gmb_avg_actual_org", "gmb_avg_pred_org")]
#   tbl <- tbl[order(tbl$cohort, tbl$yrmo, decreasing = T), ]
#   tbl
# },
# rownames = FALSE,
# colnames = c("Cohort", "# of Users", "Year/Month", "Total GMB (Test)", "Total GMB (Ctrl)", "Coupon GMB (Test)", "Coupon GMB (Ctrl)", "Organic GMB (Test)", "Organic GMB (Ctrl)"),
# options = list(pageLength = 24)
# ))
# 
# 
# 
# 
# 
# output$plot1_2_f <- renderPlot({
#   input_df <- subset(monthly2_f, cohort %in% input$filter1)
#   ggplot(input_df, aes(x = as.factor(yrmo))) +
#     geom_line(aes(y = uplift_tot, col = cohort, group = cohort), size = 1) +
#     labs(title = "Incrementality Tracking",
#          subtitle = "Still under testing",
#          x = "Month",
#          y = "Incremental %") +
#     theme_classic()
# })
# 
# output$table1_2_f <- DT::renderDataTable(DT::datatable({
#   tbl <- subset(uplift_tot2_f, cohort %in% input$filter1)
#   tbl
# },
# rownames = FALSE,
# options = list(pageLength = 24)
# ))
# 
# 
# output$plot2_2_f <- renderPlot({
#   input_df <- subset(monthly2_f, cohort %in% input$filter1)
#   ggplot(input_df, aes(x = as.factor(yrmo))) +
#     geom_line(aes(y = uplift_cpn, col = cohort, group = cohort), size = 1) +
#     labs(title = "Incrementality Tracking",
#          subtitle = "Still under testing",
#          x = "Month",
#          y = "Incremental %") +
#     theme_classic()
# })
# 
# output$table2_2_f <- DT::renderDataTable(DT::datatable({
#   tbl <- subset(uplift_cpn2_f, cohort %in% input$filter1)
#   tbl
# },
# rownames = FALSE,
# options = list(pageLength = 24)
# ))
# 
# 
# output$plot3_2_f <- renderPlot({
#   input_df <- subset(monthly2_f, cohort %in% input$filter1)
#   ggplot(input_df, aes(x = as.factor(yrmo))) +
#     geom_line(aes(y = uplift_org, col = cohort, group = cohort), size = 1) +
#     labs(title = "Incrementality Tracking",
#          subtitle = "Still under testing",
#          x = "Month",
#          y = "Incremental %") +
#     theme_classic()
# })
# 
# output$table3_2_f <- DT::renderDataTable(DT::datatable({
#   tbl <- subset(uplift_org2_f, cohort %in% input$filter1)
#   tbl
# },
# rownames = FALSE,
# options = list(pageLength = 24)
# ))
# 
# output$table4_2_f <- DT::renderDataTable(DT::datatable({
#   tbl <- subset(monthly2_f, cohort %in% input$filter1)
#   tbl <- tbl[ ,c("cohort", "user_cnt", "yrmo", "test_gmb", "pred_tot", "actual_cpn", "pred_cpn", "actual_org", "pred_org")]
#   tbl <- tbl[order(tbl$cohort, tbl$yrmo, decreasing = T), ]
#   tbl$test_gmb <- round(tbl$test_gmb)
#   tbl$pred_tot <- round(tbl$pred_tot)
#   tbl$actual_cpn <- round(tbl$actual_cpn)
#   tbl$pred_cpn <- round(tbl$pred_cpn)
#   tbl$actual_org <- round(tbl$actual_org)
#   tbl$pred_org <- round(tbl$pred_org)
#   tbl
# },
# rownames = FALSE,
# colnames = c("Cohort", "# of Users", "Year/Month", "Total GMB (Test)", "Total GMB (Ctrl)", "Coupon GMB (Test)", "Coupon GMB (Ctrl)", "Organic GMB (Test)", "Organic GMB (Ctrl)"),
# options = list(pageLength = 24)
# ))
# 
# 
# output$table5_2_f <- DT::renderDataTable(DT::datatable({
#   tbl <- subset(monthly2_f, cohort %in% input$filter1)
#   tbl <- tbl[ ,c("cohort", "user_cnt", "yrmo", "gmb_avg_actual_tot", "gmb_avg_pred_tot", 
#                  "gmb_avg_actual_cpn", "gmb_avg_pred_cpn", "gmb_avg_actual_org", "gmb_avg_pred_org")]
#   tbl <- tbl[order(tbl$cohort, tbl$yrmo, decreasing = T), ]
#   tbl
# },
# rownames = FALSE,
# colnames = c("Cohort", "# of Users", "Year/Month", "Total GMB (Test)", "Total GMB (Ctrl)", "Coupon GMB (Test)", "Coupon GMB (Ctrl)", "Organic GMB (Test)", "Organic GMB (Ctrl)"),
# options = list(pageLength = 24)
# ))
# 
# 
# 
# 
# 
# output$plot1_3_f <- renderPlot({
#   input_df <- subset(monthly3_f, cohort %in% input$filter1)
#   ggplot(input_df, aes(x = as.factor(yrmo))) +
#     geom_line(aes(y = uplift_tot, col = cohort, group = cohort), size = 1) +
#     labs(title = "Incrementality Tracking",
#          subtitle = "Still under testing",
#          x = "Month",
#          y = "Incremental %") +
#     theme_classic()
# })
# 
# output$table1_3_f <- DT::renderDataTable(DT::datatable({
#   tbl <- subset(uplift_tot3_f, cohort %in% input$filter1)
#   tbl
# },
# rownames = FALSE,
# options = list(pageLength = 24)
# ))
# 
# 
# output$plot2_3_f <- renderPlot({
#   input_df <- subset(monthly3_f, cohort %in% input$filter1)
#   ggplot(input_df, aes(x = as.factor(yrmo))) +
#     geom_line(aes(y = uplift_cpn, col = cohort, group = cohort), size = 1) +
#     labs(title = "Incrementality Tracking",
#          subtitle = "Still under testing",
#          x = "Month",
#          y = "Incremental %") +
#     theme_classic()
# })
# 
# output$table2_3_f <- DT::renderDataTable(DT::datatable({
#   tbl <- subset(uplift_cpn3_f, cohort %in% input$filter1)
#   tbl
# },
# rownames = FALSE,
# options = list(pageLength = 24)
# ))
# 
# 
# output$plot3_3_f <- renderPlot({
#   input_df <- subset(monthly3_f, cohort %in% input$filter1)
#   ggplot(input_df, aes(x = as.factor(yrmo))) +
#     geom_line(aes(y = uplift_org, col = cohort, group = cohort), size = 1) +
#     labs(title = "Incrementality Tracking",
#          subtitle = "Still under testing",
#          x = "Month",
#          y = "Incremental %") +
#     theme_classic()
# })
# 
# output$table3_3_f <- DT::renderDataTable(DT::datatable({
#   tbl <- subset(uplift_org3_f, cohort %in% input$filter1) 
#   tbl
# },
# rownames = FALSE,
# options = list(pageLength = 24)
# ))
# 
# output$table4_3_f <- DT::renderDataTable(DT::datatable({
#   tbl <- subset(monthly3_f, cohort %in% input$filter1)
#   tbl <- tbl[ ,c("cohort", "user_cnt", "yrmo", "test_gmb", "pred_tot", "actual_cpn", "pred_cpn", "actual_org", "pred_org")]
#   tbl <- tbl[order(tbl$cohort, tbl$yrmo, decreasing = T), ]
#   tbl$test_gmb <- round(tbl$test_gmb)
#   tbl$pred_tot <- round(tbl$pred_tot)
#   tbl$actual_cpn <- round(tbl$actual_cpn)
#   tbl$pred_cpn <- round(tbl$pred_cpn)
#   tbl$actual_org <- round(tbl$actual_org)
#   tbl$pred_org <- round(tbl$pred_org)
#   tbl
# },
# rownames = FALSE,
# colnames = c("Cohort", "# of Users", "Year/Month", "Total GMB (Test)", "Total GMB (Ctrl)", "Coupon GMB (Test)", "Coupon GMB (Ctrl)", "Organic GMB (Test)", "Organic GMB (Ctrl)"),
# options = list(pageLength = 24)
# ))
# 
# 
# output$table5_3_f <- DT::renderDataTable(DT::datatable({
#   tbl <- subset(monthly3_f, cohort %in% input$filter1)
#   tbl <- tbl[ ,c("cohort", "user_cnt", "yrmo", "gmb_avg_actual_tot", "gmb_avg_pred_tot", 
#                  "gmb_avg_actual_cpn", "gmb_avg_pred_cpn", "gmb_avg_actual_org", "gmb_avg_pred_org")]
#   tbl <- tbl[order(tbl$cohort, tbl$yrmo, decreasing = T), ]
#   tbl
# },
# rownames = FALSE,
# colnames = c("Cohort", "# of Users", "Year/Month", "Total GMB (Test)", "Total GMB (Ctrl)", "Coupon GMB (Test)", "Coupon GMB (Ctrl)", "Organic GMB (Test)", "Organic GMB (Ctrl)"),
# options = list(pageLength = 24)
# ))