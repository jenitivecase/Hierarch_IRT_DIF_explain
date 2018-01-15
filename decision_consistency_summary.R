results_5 <- read.xlsx("./analysis/0-5_decision_consistency.xlsx")
results_75 <- read.xlsx("./analysis/0-75_decision_consistency.xlsx")
results_1 <- read.xlsx("./analysis/1_decision_consistency.xlsx")

params <- names(results_5)[1:4]

out_5 <- NULL
out_75 <- NULL
out_1 <- NULL

for(i in 1:length(params)){
  summary_5 <- results_5 %>%
    group_by_(UQ(params[i])) %>%
    summarise(param = UQ(params[i]),
              mean_FPrate = mean(as.numeric(FP_rate), na.rm = TRUE),
              mean_Power = mean(as.numeric(Power), na.rm = TRUE),
              mean_Precision = mean(as.numeric(Precision), na.rm = TRUE))
  names(summary_5)[1] <- "value"
  
  out_5 <- rbind(out_5, summary_5)
  
  summary_75 <- results_75 %>%
    group_by_(UQ(params[i])) %>%
    summarise(param = UQ(params[i]),
              mean_FPrate = mean(as.numeric(FP_rate), na.rm = TRUE),
              mean_Power = mean(as.numeric(Power), na.rm = TRUE),
              mean_Precision = mean(as.numeric(Precision), na.rm = TRUE))
  names(summary_75)[1] <- "value"
  
  out_75 <- rbind(out_75, summary_75)
  
  summary_1 <- results_1 %>%
    group_by_(UQ(params[i])) %>%
    summarise(param = UQ(params[i]),
              mean_FPrate = mean(as.numeric(FP_rate), na.rm = TRUE),
              mean_Power = mean(as.numeric(Power), na.rm = TRUE),
              mean_Precision = mean(as.numeric(Precision), na.rm = TRUE))
  names(summary_1)[1] <- "value"
  
  out_1 <- rbind(out_1, summary_1)
}

wb <- createWorkbook()
addWorksheet(wb, "summary_0-5")
writeData(wb, "summary_0-5", out_5)
addWorksheet(wb, "summary_0-75")
writeData(wb, "summary_0-75", out_75)
addWorksheet(wb, "summary_1")
writeData(wb, "summary_1", out_1)
saveWorkbook(wb, "./analysis/decision-consistency_summary-by-param.xlsx")
