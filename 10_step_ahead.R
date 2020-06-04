# 10-steps-ahead forecast
fore <- dlmForecast(LG_Filt, nAhead=10, sampleNew=3)

f_values <- matrix(NA, nrow=40, ncol=1)
pl <- matrix(NA, nrow=40, ncol=1)
pu <- matrix(NA, nrow=40, ncol=1)
obs <- matrix(NA, nrow=40, ncol=1)
filt <- matrix(NA, nrow=40, ncol=1)
f_values[31:40] <- fore$f #Forecasts
sqrtQ <- sapply(fore$Q, function(x) sqrt(x[1,1]))
pl[31:40] <- fore$f[,1] + qnorm(0.05, sd = sqrtQ)
pu[31:40] <- fore$f[,1] + qnorm(0.95, sd = sqrtQ)
obs[1:30] <- y[157:186]

new_sample <- matrix(NA, nrow=40, ncol=3)
for (t in 1:3){ new_sample[31:40,t] <- fore$newObs[[t]] }

df_forecast <- data.frame(obs, f_values, pl, pu, new_sample)

# Plot
colors <- c("Observed data" = "black", "Forecasts" = "red", "Forecast sample"="steelblue")
f_main <- ggplot(df_forecast, aes(x=c(1:40))) +
  geom_line(aes(y=obs, color="Observed data"), size=0.8) +
  geom_line(aes(y=f_values, color = "Forecasts"), size=0.8) +
  geom_line(aes(y=pl, color = "Forecasts"), linetype ="dotted", size=0.8) +
  geom_line(aes(y=pu, color = "Forecasts"),  linetype ="dotted", size=0.8) +
  geom_line(aes(y=new_sample[,1], color = "Forecast sample"), size=1.2, linetype = "longdash") +
  geom_line(aes(y=new_sample[,2], color = "Forecast sample"), size=1.2, linetype = "dashed") +
  geom_line(aes(y=new_sample[,3], color = "Forecast sample"), size=1.2, linetype = "dotdash") +
  theme_bw(base_size=9) +
  labs(x="Year", y=expression(CO~(ppm)), color = "") +
  scale_y_continuous(breaks=seq(0,0.5,0.2)) +
  scale_x_continuous(breaks=seq(1,60,12),
                     labels=format(seq(as.Date("2014/1/1"),as.Date("2018/12/31"), "1 years"),"%Y")) +
  theme(axis.text.x=element_text(size=19, family="Times", margin=margin(t=2)),
        axis.text.y=element_text(family="Times", margin=margin(r=2)),
        axis.title.x=element_text(size=19, family="Times", margin=margin(t=2)),
        axis.title.y=element_text(size=19, family="Times", margin=margin(r=2)),
        panel.grid.major=element_line(size=0.02),
        panel.grid.minor=element_blank(),) +
  scale_color_manual(values=colors) +
  theme(legend.text=element_text(size=12),legend.position = c(0.05,0.99),
        legend.justification = c("left","top"), legend.box.just = "left",legend.margin = margin(2,2,2,2))

source("LG_model_checking.R")

f_additional <- ggarrange(b,c, nrow = 2,labels = c("(b)", "(c)"))

ggarrange(f_main,f_additional, widths=c(4,2), align="h",labels = "(a)")
