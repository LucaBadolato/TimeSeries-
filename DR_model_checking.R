knitr::opts_chunk$set(fig.width=6, fig.height=3,
                      fig.cap="\\label{fig:diagnostic}
                      (a) Standardized one-step-ahead forecast errors;
                      (b) ACF of one-step-ahead forecast errors;
                      (c) Normal probability plot of standardized one-step-ahead forecast errors")

# Dynamic linear regression: Model checking

res_DR <- residuals(DR_Filt, sd=FALSE)
res_DR = data.frame(res_DR, louisiana_df$n_months)
colnames(res_DR) = c("res", "time")

a <- ggplot(res_DR, aes(x=time)) +
  geom_line(aes(y=res), size=0.4) +
  theme_classic(base_size=9.5) +
  theme(panel.grid.minor = element_line(size=0.5)) +
  labs(x="Year", y="Residuals") +
  scale_x_continuous(breaks=seq(0,200,25),labels=format(seq(as.Date("2001/1/1"),as.Date("2017/6/1"), "2 years"), "%Y")) +
  scale_y_continuous(breaks=seq(-4.5,4.5,1), limits = c(-3,4.5)) +
  theme(axis.text=element_text(size=12),
        axis.title.x=element_text(size=12, family="Times", margin=margin(t=5)),
        axis.title.y=element_text(size=12, family="Times", margin=margin(r=5)))

ggplot.corr <- function(data, lag.max = 24, ci = 0.95, large.sample.size = TRUE, horizontal = TRUE,...)
{
  require(ggplot2)
  require(dplyr)
  require(cowplot)
  
  if(horizontal == TRUE) {numofrow <- 1} else {numofrow <- 2}
  
  list.acf <- acf(data, lag.max = lag.max, type = "correlation", plot = FALSE)
  N <- as.numeric(list.acf$n.used)
  df1 <- data.frame(lag = list.acf$lag, acf = list.acf$acf)
  df1$lag.acf <- dplyr::lag(df1$acf, default = 0)
  df1$lag.acf[2] <- 0
  df1$lag.acf.cumsum <- cumsum((df1$lag.acf)^2)
  df1$acfstd <- sqrt(1/N * (1 + 2 * df1$lag.acf.cumsum))
  df1$acfstd[1] <- 0
  df1 <- select(df1, lag, acf, acfstd)
  
  list.pacf <- acf(data, lag.max = lag.max, type = "partial", plot = FALSE)
  df2 <- data.frame(lag = list.pacf$lag,pacf = list.pacf$acf)
  df2$pacfstd <- sqrt(1/N)
  
  if(large.sample.size == TRUE)
  {
    plot.acf <- ggplot(data = df1, aes( x = lag, y = acf)) +
      geom_area(aes(x = lag, y = qnorm((1+ci)/2)*acfstd), fill = "#B9CFE7") +
      geom_area(aes(x = lag, y = -qnorm((1+ci)/2)*acfstd), fill = "#B9CFE7") +
      geom_col(fill = "#4373B6", width = 0.7) +
      theme_classic(base_size=9.5) +
      scale_x_continuous(breaks = seq(0,max(df1$lag),6)) +
      scale_y_continuous(name = element_blank(), 
                         limits = c(min(df1$acf,df2$pacf),1)) +
      theme(axis.text=element_text(size=12),
            axis.title.x=element_text(size=12,family="Times", margin=margin(t=5)),
            axis.title.y=element_text(size=12,family="Times", margin=margin(r=5)),
            panel.grid.minor = element_line(size=0.5))
    
    plot.pacf <- ggplot(data = df2, aes(x = lag, y = pacf)) +
      geom_area(aes(x = lag, y = qnorm((1+ci)/2)*pacfstd), fill = "#B9CFE7") +
      geom_area(aes(x = lag, y = -qnorm((1+ci)/2)*pacfstd), fill = "#B9CFE7") +
      theme_classic(base_size=9.5)+
      geom_col(fill = "#4373B6", width = 0.7) +
      scale_x_continuous(breaks = seq(0,max(df2$lag, na.rm = TRUE),6)) +
      scale_y_continuous(name = element_blank(),
                         limits = c(min(df1$acf,df2$pacf),1)) +
      ggtitle("PACF") +
      theme(axis.text=element_text(size=12),
            axis.title.x=element_text(size=12,family="Times", margin=margin(t=5)),
            axis.title.y=element_text(size=12,family="Times", margin=margin(r=5)),
            panel.grid.minor = element_line(size=0.5))
  }
  else
  {
    plot.acf <- ggplot(data = df1, aes( x = lag, y = acf)) +
      geom_col(fill = "#4373B6", width = 0.7) +
      geom_hline(yintercept = qnorm((1+ci)/2)/sqrt(N), 
                 colour = "sandybrown",
                 linetype = "dashed") + 
      geom_hline(yintercept = - qnorm((1+ci)/2)/sqrt(N), 
                 colour = "sandybrown",
                 linetype = "dashed") + 
      theme_classic(base_size=9.5)+
      scale_x_continuous(breaks = seq(0,max(df1$lag),6)) +
      scale_y_continuous(name = element_blank(), 
                         limits = c(min(df1$acf,df2$pacf),1)) +
      theme(axis.text=element_text(size=12),
            axis.title.x=element_text(size=12,family="Times", margin=margin(t=5)),
            axis.title.y=element_text(size=12,family="Times", margin=margin(r=5)),
            panel.grid.minor = element_line(size=0.5))
    
    plot.pacf <- ggplot(data = df2, aes(x = lag, y = pacf)) +
      geom_col(fill = "#4373B6", width = 0.7) +
      geom_hline(yintercept = qnorm((1+ci)/2)/sqrt(N), 
                 colour = "sandybrown",
                 linetype = "dashed") + 
      geom_hline(yintercept = - qnorm((1+ci)/2)/sqrt(N), 
                 colour = "sandybrown",
                 linetype = "dashed") + 
      theme_classic(base_size=9.5)+
      scale_x_continuous(breaks = seq(0,max(df2$lag, na.rm=TRUE),6)) +
      scale_y_continuous(name = element_blank(),
                         limits = c(min(df1$acf,df2$pacf),1)) +
      ggtitle("PACF") +
      theme(axis.text=element_text(size=12),
            axis.title.x=element_text(size=12,family="Times", margin=margin(t=5)),
            axis.title.y=element_text(size=12,family="Times", margin=margin(r=5)),
            panel.grid.minor=element_line(size=0.5))
  }
  cowplot::plot_grid(plot.acf)
}

b <- ggplot.corr(data=res_DR$res, lag.max=24, ci=0.95, large.sample.size=FALSE, horizontal=TRUE)

c <- ggplot(res_DR, aes(sample=res)) +
  stat_qq(col="blue") +
  stat_qq_line(col="red", lty=2, size=1) +
  scale_y_continuous(breaks=seq(-3, 3,1), limits=c(-3,3)) +
  scale_x_continuous(breaks=seq(-3, 3,1), limits=c(-3,3)) +
  theme_classic(base_size=9.5) +
  theme(panel.grid.minor=element_line(size=0.5)) +
  labs(x="Theoretical", y="Observed") +
  theme(axis.text=element_text(size=12),
        axis.title.x=element_text(size=12,family="Times", margin=margin(t=5)),
        axis.title.y=element_text(size=12,family="Times", margin=margin(r=5)))
