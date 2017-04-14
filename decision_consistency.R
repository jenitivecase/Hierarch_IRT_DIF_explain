#alpha setting not working, very annoying
ggplot(data, aes(x = true_param, y = est_param)) + 
  geom_point(alpha = 0.65, color = colors[i]) + 
  ggtitle(paste0("D-parameter correlations for\nrho = ", rho, ", reference proportion = ", PREF)) + 
  labs(x = "True Parameter Value", y = "Estimated Parameter Value", 
       caption = paste0("r = ", round(cor(data$true_param, data$est_param), 3))) + 
  theme(plot.title = element_text(hjust = 0.5))  + 
  geom_line(y = .5) + 
  geom_vline(xintercept = .5)+
  geom_rect(aes(xmin = flag_amt, xmax = Inf, ymin = -Inf, ymax = flag_amt, 
            fill = "red", alpha = 0.02), show.legend = FALSE)

flag_amt <- .5

trueneg <- nrow(data[which(data$true_param < flag_amt & data$est_param < flag_amt),])
falsepos <- nrow(data[which(data$true_param < flag_amt & data$est_param > flag_amt),])
falseneg <- nrow(data[which(data$true_param > flag_amt & data$est_param < flag_amt),])
truepos <- nrow(data[which(data$true_param > flag_amt & data$est_param > flag_amt),])

dec_consist <- matrix(c(truepos, falsepos, falseneg, trueneg), nrow = 2, byrow = TRUE)
colnames(dec_consist) <- c("True_DIF", "True_NoDIF")
rownames(dec_consist) <- c("Est_DIF", "Est_NoDIF")
dec_consist
