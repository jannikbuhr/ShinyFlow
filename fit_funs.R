# Functions to fit exponential decay (stopped flow) with 1,2 or 3 phases
# Decay Functions
decay1 <- function(X, Y0, Plateau, KFast){
    YFast <- (Y0-Plateau)*exp(-KFast*X)
    Y <- Plateau + YFast
    return(Y)
}

decay2 <- function(X, Y0, Plateau, KFast, KSlow, PercentFast){
    YFast <- (Y0-Plateau)*PercentFast*.01*exp(-KFast*X)
    YSlow <- (Y0-Plateau)*(100-PercentFast)*.01*exp(-KSlow*X)
    Y <- Plateau + YFast + YSlow
    return(Y)
}

decay3 <- function(X, Y0, Plateau, KFast, Kmedium, KSlow, PercentFast, PercentSlow){
    YFast <- (Y0-Plateau)*PercentFast*.01*exp(-KFast*X)
    YSlow <- (Y0-Plateau)*PercentSlow*.01*exp(-KSlow*X)
    YMedium <- (Y0-Plateau)*(100-PercentFast - PercentSlow)*.01*exp(-Kmedium*X)
    Y <- Plateau + YFast + YMedium + YSlow
    return(Y)
}

# Graphical display of starting values
display_start <- function(df, model, starting){
    df %>% plt() +
        stat_function(geom = "line",
                      fun = model,
                      args = starting,
                      color = "red",
                      size = 1.3) +
        labs(title = "Starting values")
}

# Fit one of the 3 exponential functions
fit_decay <- function(df, phases){
    if (phases == 1) {
        mod <- nlsLM(data = data,
                     fluorescence ~ decay1(X = time, Y0, Plateau, KFast),
                     start = starting1,
                     trace = F, control = nls.control()
        )
    }
    if (phases == 2) {
        mod <- nlsLM(data = data,
                     fluorescence ~ decay2(X = time, Y0, Plateau, KFast, KSlow, PercentFast),
                     start = starting2,
                     trace = F, control = nls.control()
        )
    }
    if (phases == 3) {
        mod <- decay_model3 <- nlsLM(data = data,
                                     fluorescence ~ decay3(X = time, Y0, Plateau, KFast, Kmedium, KSlow,
                                                           PercentFast, PercentSlow),
                                     start = starting3,
                                     trace = F, control = nls.control()
        )
    }
    return(mod)
}

# enhance data with model information
extract_model <- function(df, mod){
    return(df %>% add_residuals(mod) %>% add_predictions(mod))
}

analyze_model <- function(df, mod){
    resid_plot <- df %>% ggplot() +
        aes(x = time, y = resid) +
        theme_classic() +
        geom_point()
    summary(mod) %>% print()
    glance(mod) %>% print()
    tidy(mod) %>% print()
    print(resid_plot)
}

# Plot Data with fit
plot_fit <- function(df){
    p <- df %>% plt() +
        geom_line(aes(y = pred),
                  color = "darkorange",
                  size = 1.2,
                  alpha = 0.9) +
        labs(caption = expression((y[start] - y[end]) %*% (P[fast] ~ e^(-k_fast %*% x) +P[slow] ~
                                                               e^(-k_slow %*% x) + (1 - P[fast] - P[slow]) ~
                                                               e^(-k_medium %*% x)) + y[end]), parse = T) +
        annotate(
            "text", label = paste("y_start =", round(y_start, 2), "\n", "y_end =", round(y_end, 2), "\n",
                                  "k_fast = ", round(k_fast, 2), "\n", "t_half_fast =", round(t_half_fast, 4), "\n",
                                  "k_slow = ", round(k_slow, 2), "\n", "t_half_slow =", round(t_half_slow, 4), "\n",
                                  "k_medium = ", round(k_med, 2), "\n",
                                  "sigma = ", round(sigma, 3)),
            hjust = 1,
            vjust = 1.5,
            x = Inf, y = Inf
        )
    return(p)
}
