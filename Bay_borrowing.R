# Required libraries
library(BayesFBHborrow)  # Bayesian survival model with borrowing
library(ggpubr)
library(survminer)
library(patchwork)
library(survival)
library(muhaz)
set.seed(123)
dff <- read.csv("km.csv")

# Load and prepare trial and historical data per population and outcome
pop="Younger" #or "Older"
outcome="HD-MTX" #or "Carboplatin

#pop="Older" #or "Older"
#outcome="Carboplatin" #or "Carboplatin

df <- subset(dff, population==pop)
df$arm<-factor(df$arm, levels = c(outcome,"Control","Historical control"))

a<-ggsurvplot(survfit(Surv(time,status)~arm, data=df[df$study=="Trial",]), risk.table = T, break.time.by=1, xlab="Time (Years)",
              legend.title = "Arms", legend.labs = c(outcome,"Control"), conf.int = F, ylab="Event Free Survival (%)",
              fun = "pct", pval = TRUE, pval.coord=c(0,0), pval.method = T, pval.method.coord = c(0, 7))
A<-a$plot / a$table + plot_layout(heights = c(8,2))

dft<-df[df$study=="Trial",]
dft$arm<-factor(dft$arm, levels = c("Control",outcome))
summary(coxph(Surv(time, status) ~ arm, data = dft))

b<-ggsurvplot(survfit(Surv(time,status)~arm, data=df), risk.table = T, break.time.by=1, xlab="Time (Years)",
              legend.title = "Arms", legend.labs = c(outcome,"Control","Hist. Control"), conf.int = F, ylab="Event Free Survival (%)",
              fun = "pct")
B<-b$plot / b$table + plot_layout(heights = c(8,2))
A|B

# Current trial data
cc <- subset(df, data == "Trial")
cc$tte <- cc$time
cc$event <- cc$status
cc$X_trt <- ifelse(cc$arm==outcome, 1, 0)  # 1 = HD-MTX, 0 = Control
cc <- cc[, c("tte", "event", "X_trt")]

# Historical control data (no treatment assignment)
hist <- subset(df, data == "Historical control")
hist$tte <- hist$time
hist$event <- hist$status
hist <- hist[, c("tte", "event")]

# Hyperparameters and tuning parameters for BayesFBHborrow model
#find d_tau and xi
haz <- with(subset(cc, X_trt == 0), muhaz(tte, event))
log_range <- diff(range(log(haz$haz.est[haz$haz.est > 0])))  # log-hazard range
xi <- log_range / 2
d_tau <- max(1.0, log_range)
p_0 = 1 / (1 + (1 / d_tau) * ((xi^2 + 2) / (xi^2 + 2 * d_tau))^(-1.5))
cat(sprintf("Log-hazard range: %.3f\nxi: %.3f\nd_tau: %.3f\np₀: %.3f\n",
            log_range, xi, d_tau, p_0))

hyperparameters <- list(
  a_tau = 1, b_tau = 0.001,
  c_tau = 1, d_tau = d_tau, #comes from difference in smoothed posterior baseline hazard without borrowing (1.55)
  p_0 = 1 / (1 + (1 / d_tau) * ((xi^2 + 2) / (xi^2 + 2 * d_tau))^(-1.5)),               # Prior weight for historical borrowing
  a_sigma = 1, b_sigma = 1,
  clam_smooth = 0.7,       # Baseline hazard smoothness
  phi = 3
)

tuning_parameters <- list(
  Jmax = 5,
  cprop_beta = 3.25,
  cprop_beta_0 = 3.25,
  pi_b = 0.5,
  alpha = 0.4
)

iter = 10000

# Run BayesFBHborrow with borrowing enabled
res <- BayesFBHborrow(
  data = cc,
  data_hist = hist,
  borrow = TRUE,
  model_choice = "mix",
  tuning_parameters = tuning_parameters,
  hyperparameters = hyperparameters,
  iter = iter,
  warmup_iter = iter/10,
  refresh = iter/10,
  verbose = TRUE
)

# Summarize posterior parameters
summary(res$out, estimator = "out_fixed")

# Extract posterior hazard ratio samples and compute summary statistics
beta_samples_with <- res$out$out_fixed[, 4]            # Posterior samples for beta_1
HR_with <- exp(beta_samples_with)                   # Convert to hazard ratios
HR_mean <- mean(HR_with)
res[["plots"]][["treatment_density"]][["data"]][["x"]]<-HR_with
HR_CrI <- quantile(HR_with, probs = c(0.025, 0.975))

cat(sprintf("Posterior Mean HR (borrowed): %.3f\n", HR_mean))
cat(sprintf("95%% Credible Interval (borrowed): [%.3f, %.3f]\n", HR_CrI[1], HR_CrI[2]))

res[["plots"]]

# Sensitivity analysis: re-run model with no historical borrowing
out_noborrow <- BayesFBHborrow(
  data = cc,
  borrow = FALSE,
  tuning_parameters = tuning_parameters,
  iter = iter,
  warmup_iter = iter/10,
  refresh = iter/10,
  verbose = TRUE
)

summary(out_noborrow$out, estimator = "out_fixed")

# Extract posterior hazard ratio samples and compute summary statistics
beta_samples_without <- out_noborrow$out$out_fixed[, 4]            # Posterior samples for beta_1
HR_without <- exp(beta_samples_without)                   # Convert to hazard ratios
out_noborrow[["plots"]][["treatment_density"]][["data"]][["x"]]<-HR_without
HR_mean_without <- mean(HR_without)
HR_CrI_without <- quantile(HR_without, probs = c(0.025, 0.975))

cat(sprintf("Posterior Mean HR (borrowed): %.3f\n", HR_mean_without))
cat(sprintf("95%% Credible Interval: [%.3f, %.3f]\n", HR_CrI_without[1], HR_CrI_without[2]))

out_noborrow[["plots"]]

# Posterior difference in hazard ratios
HR_diff <- HR_with - HR_without

# Summarize shift in HR due to borrowing
cat("Posterior mean difference (HR):", round(mean(HR_diff), 3), "\n")
cat("95% Credible Interval for difference:", 
    round(quantile(HR_diff, c(0.025, 0.975)), 3), "\n")

# Probability borrowing changed inference (any or meaningful shift)
cat("Pr(|ΔHR| > 0):", round(mean(abs(HR_diff) > 0), 3), "\n")
cat("Pr(|ΔHR| > 0.1):", round(mean(abs(HR_diff) > 0.1), 3), "\n")  # Adjust threshold as needed

# Plot posterior HR distributions for visual comparison
df_HR <- data.frame(
  HR = c(HR_with, HR_without),
  model = rep(c("Borrowing", "No borrowing"), each = length(HR_with))
)

ggplot(df_HR, aes(x = HR, fill = model)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Posterior Distributions of Hazard Ratio",
    subtitle = "With vs. Without Historical Borrowing",
    x = "Hazard Ratio (HR)",
    y = "Density"
  ) +
  scale_fill_manual(values = c("Borrowing" = "steelblue", "No borrowing" = "darkorange")) +
  theme_minimal(base_size = 14)

# Function to compute Posterior Fragility Index (PFI)
compute_posterior_fragility_index <- function(cc, hist, 
                                              hyperparameters, tuning_parameters,
                                              max_iter = iter, warmup = iter/10,
                                              verbose = TRUE) {
  event_rows <- which(cc$event == 1)
  n_events <- length(event_rows)
  
  for (i in 0:n_events) {
    cc_mod <- cc
    if (i > 0) cc_mod$event[event_rows[1:i]] <- 0  # Flip i events to censoring
    
    if (verbose) message("Flipping ", i, " event(s)...")
    result <- tryCatch({
      BayesFBHborrow(
        data = cc_mod,
        data_hist = hist,
        borrow = TRUE,
        model_choice = "mix",
        tuning_parameters = tuning_parameters,
        hyperparameters = hyperparameters,
        iter = max_iter,
        warmup_iter = warmup,
        refresh = 0,
        verbose = FALSE
      )
    }, error = function(e) return(NULL))
    
    if (!is.null(result)) {
      HR_CrI <- quantile(exp(result$out$out_fixed[, 4]), probs = c(0.025, 0.975))
      if (HR_CrI[1] < 1 && HR_CrI[2] > 1) {
        message("Posterior CrI includes 1 after flipping ", i, " event(s).")
        return(i)
      }
    }
  }
  
  message("All events flipped but posterior still significant.")
  return(n_events + 1)
}

# Run fragility index computation and normalize
fragility_index <- compute_posterior_fragility_index(
  cc = cc,
  hist = hist,
  hyperparameters = hyperparameters,
  tuning_parameters = tuning_parameters
)

pfi <- fragility_index
n_total <- nrow(cc)
n_events <- sum(cc$event == 1)
pfi_sample_pct <- 100 * pfi / n_total
pfi_event_pct <- 100 * pfi / n_events

cat(sprintf("PFI = %d (%.1f%% of sample, %.1f%% of events)\n",
            pfi, pfi_sample_pct, pfi_event_pct))

# Visualize posterior distribution of hazard ratio
df_hr <- data.frame(HR = HR_with)
ggplot(df_hr, aes(x = HR)) +
  geom_histogram(bins = 100, fill = "steelblue", color = "black") +
  geom_vline(xintercept = HR_mean, color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Posterior Predictive Distribution of Hazard Ratio",
    subtitle = paste0("Pr(HR ≤ ", round(HR_mean, 3), ") = ",
                      round(mean(HR_with <= HR_mean), 3)),
    x = "Simulated Hazard Ratio (HR)",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14)

K = 5 #number of total tests

# Adjust posterior probability of true effect accounting for multiplicity
adjusted_posterior_prob <- function(beta_posterior, 
                                    observed_HR = mean(HR_with), 
                                    K = K, # total subgroup tests
                                    pi_1 = 1/K, #prior probability that the alternative hypothesis (real effect) is true
                                    n_sim = iter) {
  
  HR_samples <- exp(beta_posterior)
  power_alt <- mean(HR_samples <= observed_HR)
  
  null_betas <- rnorm(n_sim, mean = 0, sd = sd(beta_posterior))
  null_HRs <- exp(null_betas)
  petie <- mean(null_HRs <= observed_HR)
  
  num <- pi_1 * power_alt
  denom <- num + (1 - pi_1) * petie
  post_prob_true <- num / denom
  
  list(
    posterior_power = power_alt,
    PETIE = petie,
    adjusted_posterior_prob_true_effect = post_prob_true
  )
}

# Run multiplicity-adjusted Bayesian FDR estimate
beta_posterior <- res$out$out_fixed[, 4]

result <- adjusted_posterior_prob(
  beta_posterior = beta_posterior,
  observed_HR = HR_mean,
  K = K,
  pi_1 = 1/K,
  n_sim = iter
)

print(result)
# posterior_power: chance you'd detect this again if true
# PETIE: false positive rate under null
# adjusted_posterior_prob_true_effect: posterior probability the effect is real, after adjusting for multiplicity

Bb<-res[["plots"]][["smooth_survival"]]+theme_pubr()+rremove("ylab")+xlab("Time (Years)")+ggtitle("Smoothed Posterior Survival with\nBayesian Borrowing (10,000 simulations)")+ 
  scale_x_continuous(limits=c(0,10),breaks = seq(0, 10, by = 1)) +  # x-axis: every 1
  scale_y_continuous(limits = c(0, 1),breaks = seq(0, 1, by = 0.25),labels = seq(0, 100, by = 25)) + 
  labs(colour = "Arms",fill = "Arms") +
  scale_colour_discrete(labels = c("Treatment" = "HD-MTX")) +
  scale_fill_discrete(labels = c("Treatment" = "HD-MTX"))+
  theme(
    axis.title.x = element_text(size = 14, face = "plain", colour = "black", vjust = 1),
    axis.text.x  = element_text(size = 12, face = "plain", colour = "black", vjust = 1),
    axis.text.y  = element_text(size = 12, face = "plain", colour = "black", hjust = 1),
    legend.text = element_text(size = 12),legend.title = element_text(size = 12),
    plot.title = element_text(size=14))

B[[1]]<-B[[1]]+scale_color_manual(values = c("Control" = "red","Hist. Control" = "orange","HD-MTX" = "darkturquoise"))+
  theme(legend.text = element_text(size = 12),legend.title = element_text(size = 12),plot.title=element_text(size=14))+
  ggtitle("ACNS0334 and\nSimilar Historical Trial Data") 
  
B[[2]] <- B[[2]] + theme(axis.text.y = NULL)  # clear first
B[[2]]<-B[[2]] + theme(axis.text.y = element_text(size = 12, color = "black", hjust = 1), plot.title = element_text(size=14))
del <- B[[2]] +
  rremove("xylab") +
  rremove("xy.text") +
  rremove("ticks") +
  theme(
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    axis.ticks = element_line(color = "white"),
    axis.line = element_line(color = "white"),
    plot.title = element_text(color = "white"),
    plot.subtitle = element_text(color = "white"),
    plot.caption = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white")
  )

(B[[1]] / B[[2]] + plot_layout(heights = c(8, 2))) | (Bb / del + plot_layout(heights = c(8, 2)))

#7x9.5in PDF

  
