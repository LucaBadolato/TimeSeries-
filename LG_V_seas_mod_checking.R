knitr::opts_chunk$set(fig.width=8, fig.height=4, 
                      fig.cap="\\label{fig:diagnostic}
                      (a) Standardized one-step-ahead forecast errors;
                      (b) ACF of one-step-ahead forecast errors;
                      (c) Normal probability plot of standardized one-step-ahead forecast errors")

# Linear growth model: Model checking

res_LG_v_seas <- residuals(LG_v_seas_Filt, sd=FALSE)
res_LG_v_seas = data.frame(res_LG_v_seas,louisiana_df$n_months)
colnames(res_LG_v_seas) = c("res", "time")

ggplot(res_LG_v_seas, aes(x=time)) +
  geom_line(aes(y=res), size=0.5) +
  theme_bw(base_size=9) +
  labs(x="Year", y="Residuals") +
  scale_x_continuous(breaks=seq(0,186,24),
                     labels=format(seq(as.Date("2001/1/1"),as.Date("2016/6/1"), "2 years"), "%Y")) +
  scale_y_continuous(breaks=seq(-5,5,1), limits = c(-3,4)) +
  theme(axis.text.x=element_text(size=15, family="Times", margin=margin(t=5)),
        axis.text.y=element_text(size=15, family="Times", margin=margin(r=5)),
        axis.title.x=element_text(size=15, family="Times", margin=margin(t=5)),
        axis.title.y=element_text(size=15, family="Times", margin=margin(r=5)),
        panel.grid.major=element_line(size=0.5),
        panel.grid.minor=element_blank(),
        plot.margin=unit(c(0,0,0,0.8), "cm"))

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
      geom_area(aes(x = lag, y = qnorm((1+ci)/2)*acfstd), fill = "navy") +
      geom_area(aes(x = lag, y = -qnorm((1+ci)/2)*acfstd), fill = "navy") +
      geom_col(fill = "navy", width = 0.5) +
      theme_bw(base_size=9) +
      scale_x_continuous(breaks = seq(0,max(df1$lag),1)) +
      scale_y_continuous(name = element_blank(), 
                         limits = c(min(df1$acf,df2$pacf),1)) +
      theme(axis.text.x=element_text(size=15, family="Times", margin=margin(t=5)),
            axis.text.y=element_text(size=15, family="Times", margin=margin(r=5)),
            axis.title.x=element_text(size=15, family="Times", margin=margin(t=5)),
            axis.title.y=element_text(size=15, family="Times", margin=margin(r=5)),
            panel.grid.major=element_line(size=0.5),
            panel.grid.minor=element_blank(),
            plot.margin=unit(c(0,0,0,1), "cm"))
    
    plot.pacf <- ggplot(data = df2, aes(x = lag, y = pacf)) +
      geom_area(aes(x = lag, y = qnorm((1+ci)/2)*pacfstd), fill = "navy") +
      geom_area(aes(x = lag, y = -qnorm((1+ci)/2)*pacfstd), fill = "navy") +
      theme_bw(base_size=9) +
      geom_col(fill = "navy", width = 0.5) +
      scale_x_continuous(breaks = seq(0,max(df2$lag, na.rm = TRUE),1)) +
      scale_y_continuous(name = element_blank(),
                         limits = c(min(df1$acf,df2$pacf),1)) +
      ggtitle("PACF") +
      theme(axis.text.x=element_text(size=15, family="Times", margin=margin(t=5)),
            axis.text.y=element_text(size=15, family="Times", margin=margin(r=5)),
            axis.title.x=element_text(size=15, family="Times", margin=margin(t=5)),
            axis.title.y=element_text(size=15, family="Times", margin=margin(r=5)),
            panel.grid.major=element_line(size=0.5),
            panel.grid.minor=element_blank(),
            plot.margin=unit(c(0,0,0,1), "cm"))
  }
  else
  {
    plot.acf <- ggplot(data = df1, aes( x = lag, y = acf)) +
      geom_col(fill = "navy", width = 0.5) +
      geom_hline(yintercept = qnorm((1+ci)/2)/sqrt(N), 
                 colour = "navy",
                 linetype = "dashed", size=0.5) + 
      geom_hline(yintercept = - qnorm((1+ci)/2)/sqrt(N), 
                 colour = "navy",
                 linetype = "dashed", size=0.5) + 
      theme_bw(base_size=9) +
      scale_x_continuous(breaks = seq(0,max(df1$lag),1)) +
      scale_y_continuous(name = element_blank(), 
                         limits = c(min(df1$acf,df2$pacf),1)) +
      theme(axis.text.x=element_text(size=15, family="Times", margin=margin(t=5)),
            axis.text.y=element_text(size=15, family="Times", margin=margin(r=5)),
            axis.title.x=element_text(size=15, family="Times", margin=margin(t=5)),
            axis.title.y=element_text(size=15, family="Times", margin=margin(r=5)),
            panel.grid.major=element_line(size=0.5),
            panel.grid.minor=element_blank(),
            plot.margin=unit(c(0,0,0,1), "cm"))
    
    plot.pacf <- ggplot(data = df2, aes(x = lag, y = pacf)) +
      geom_col(fill = "navy", width = 0.5) +
      geom_hline(yintercept = qnorm((1+ci)/2)/sqrt(N), 
                 colour = "navy",
                 linetype = "dashed", size=0.5) + 
      geom_hline(yintercept = - qnorm((1+ci)/2)/sqrt(N), 
                 colour = "navy",
                 linetype = "dashed", size=0.5) + 
      theme_bw(base_size=9) +
      scale_x_continuous(breaks = seq(0,max(df2$lag, na.rm=TRUE),1)) +
      scale_y_continuous(name = element_blank(),
                         limits = c(min(df1$acf,df2$pacf),1)) +
      ggtitle("PACF") +
      theme(axis.text.x=element_text(size=15, family="Times", margin=margin(t=5)),
            axis.text.y=element_text(size=15, family="Times", margin=margin(r=5)),
            axis.title.x=element_text(size=15, family="Times", margin=margin(t=5)),
            axis.title.y=element_text(size=15, family="Times", margin=margin(r=5)),
            panel.grid.major=element_line(size=0.5),
            panel.grid.minor=element_blank(),
            plot.margin=unit(c(0,0,0,1), "cm"))
  }
  cowplot::plot_grid(plot.acf)
}

b <- ggplot.corr(data=res_LG_v_seas$res, lag.max=12, ci=0.95, large.sample.size=FALSE, horizontal=TRUE)

c <- ggplot(res_LG_v_seas, aes(sample=res)) +
  stat_qq(col="navy",size=0.5) +
  stat_qq_line(col="red3", lty=1, size=0.5) +
  scale_y_continuous(breaks=seq(-3, 3,1), limits=c(-3,3)) +
  scale_x_continuous(breaks=seq(-3, 3,1), limits=c(-3,3)) +
  theme_bw(base_size=9) +
  labs(x="Theoretical", y="Observed") +
  theme(axis.text.x=element_text(size=15, family="Times", margin=margin(t=5)),
        axis.text.y=element_text(size=15, family="Times", margin=margin(r=5)),
        axis.title.x=element_text(size=15, family="Times", margin=margin(t=5)),
        axis.title.y=element_text(size=15, family="Times", margin=margin(r=5)),
        panel.grid.major=element_line(size=0.5),
        panel.grid.minor=element_blank(),
        plot.margin=unit(c(0,0,1,1), "cm"))
