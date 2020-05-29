# Model specification with 2 states
y <- as.numeric(louisiana_df$m_CO_mean)
nstates <- 2
set.seed(2)
HMM_CO <- depmixS4::depmix(y ~ 1 + louisiana_df$n_months, data=data.frame(y), nstates=nstates)

# Estimation of the unknown parameters
HMM_CO_fit <- depmixS4::fit(HMM_CO)

##############################################################################

# MLE
mu1hat <- paste0(round(HMM_CO_fit@response[[1]][[1]]@parameters[["coefficients"]][["(Intercept)"]],3))
mu2hat <- paste0(round(HMM_CO_fit@response[[2]][[1]]@parameters[["coefficients"]][["(Intercept)"]],3))
beta1hat <- paste0(round(HMM_CO_fit@response[[1]][[1]]@parameters[["coefficients"]][["louisiana_df$n_months"]],3))
beta2hat <- paste0(round(HMM_CO_fit@response[[2]][[1]]@parameters[["coefficients"]][["louisiana_df$n_months"]],3))
sd1hat <- paste0(round(HMM_CO_fit@response[[1]][[1]]@parameters$sd,3))
sd2hat <- paste0(round(HMM_CO_fit@response[[2]][[1]]@parameters$sd,3))

# se(MLE)
MLE_st.err=depmixS4::standardError(HMM_CO_fit)
st.err_mu1hat <- paste0('(', round(MLE_st.err$se[7],3), ')')
st.err_mu2hat <- paste0('(', round(MLE_st.err$se[10],3), ')')
st.err_beta1hat <- paste0('(', round(MLE_st.err$se[8],3), ')')
st.err_beta2hat <- paste0('(', round(MLE_st.err$se[11],3), ')')
st.err_sd1hat <- paste0('(', round(MLE_st.err$se[9],3), ')')
st.err_sd2hat <- paste0('(', round(MLE_st.err$se[12],3), ')')

# Build a summary table for estimates
MLEsum <- data.frame(state=c(rep("S=1",2), rep("S=2",2)),
                     mu=c(mu1hat, st.err_mu1hat, mu2hat, st.err_mu2hat),
                     beta=c(beta1hat, st.err_beta1hat,beta2hat,st.err_beta2hat),
                     sd=c(sd1hat, st.err_sd1hat, sd2hat, st.err_sd2hat))
knitr::kable(MLEsum, "latex", booktabs=T, align="c",
             col.names = c("State", "$\\hat{\\mu}_{MLE}$","$\\hat{\\beta}_{MLE}$","$\\hat{\\sigma}_{MLE}$"),
             escape=F, caption="Maximum Likelihood estimates of the state-dependent
             parameters \\label{tab:HMM_MLE}") %>%
  column_spec(1:4, width="5em") %>%
  collapse_rows(columns=1, latex_hline="none") %>%
  footnote(general="Standard errors in parentheses",
           general_title="Note:",
           footnote_as_chunk=T, escape=F) %>%
  kable_styling(latex_options="hold_position")

##############################################################################

# Decoding
# Get the estimated state for each timestep 
estStates_CO <- posterior(HMM_CO_fit)

HMM_summary<-depmixS4::summary(HMM_CO_fit)

##############################################################################

knitr::opts_chunk$set(fig.width=20, fig.height=6, out.width='100%',
                      fig.cap="\\label{fig:HMM_results}State-dependent means and standard deviations")

# Chart with state-dependent mean and standard deviations
HMM_main_CO <- data.frame(state=1:nstates,
                          mu_CO=HMM_summary[,1],
                          sigma_CO=HMM_summary[,3])
df_to_plot_CO <- estStates_CO %>%
  left_join(HMM_main_CO)
df_to_plot_CO %<>%
  mutate(xtime=louisiana_df$n_months, yvalue=louisiana_df$m_CO_mean)

HMM_main<- ggplot(df_to_plot_CO, aes(x=xtime, y=yvalue)) +
  geom_line(size=0.7) +
  geom_point(aes(x=xtime, y=mu_CO), col="blue", size=.8) +
  geom_ribbon(aes(ymin=mu_CO-2*sigma_CO, ymax=mu_CO+2*sigma_CO), alpha=.1) +
  theme_classic(base_size=9) +
  theme(plot.title=element_text(hjust=0.5)) +
  labs(y=expression(CO~(ppm)), x="Months") +
  scale_x_continuous(breaks=seq(0,200,50),labels=format(seq(as.Date("2000/1/1"),as.Date("2016/6/1"), "4 years"), "%Y")) +
  scale_y_continuous(breaks=seq(0,1.5,0.25)) +
  theme(plot.title=element_text(size=10, face="bold", margin=margin(0,0,10,0)),
        axis.text=element_text(size=19,face="bold"),
        axis.title.x=element_text(size=21,family="Times", margin=margin(t=5)),
        axis.title.y=element_text(size=21,family="Times",margin=margin(r=5)))

# Chart with Viterbi states and posterior probabilities
HMM_additional_CO <- data.frame(time_index=louisiana_df$n_months,
                                state=estStates_CO[1],
                                S1=estStates_CO[2]) %>% 
  gather("variable", "value", -time_index)

my_breaks <- function(x) { if (max(x)>1) seq(0,2,1) else seq(0,1,1) }
lab_names <- c('S1'="Posterior, S=1",'state'="Viterbi states")

HMM_additional<-ggplot(HMM_additional_CO, aes(time_index, value)) +
  geom_line(color="black", size=0.7) +
  facet_wrap(variable~., scales="free", ncol=1,
             strip.position="left",
             labeller = as_labeller(lab_names)) +
  labs(x="Month") +
  scale_y_continuous(breaks=my_breaks) +
  scale_x_continuous(breaks=seq(0,200,50), labels=format(seq(as.Date("2000/1/1"),as.Date("2016/4/27"), "4 years"), "%Y"))+
  theme_stata(base_size=9) +
  theme(axis.text=element_text(size=19),
        axis.title.x=element_text(size=21,family="Times", margin=margin(5,0,0,0)),
        axis.title.y=element_blank(),
        strip.text=element_text(size=21,family="Times",face="bold"),
        strip.background=element_rect(colour="black", fill=NA),
        strip.placement="outside")

ggarrange(HMM_main, HMM_additional, widths = c(2,1))