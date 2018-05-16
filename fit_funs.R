# Functions to fit exponential decay (stopped flow) with 1,2 or 3 phases
# Decay Functions

#Prism:
#Y=(Y0 - Plateau)*exp(-K*X) + Plateau

decay1 <- function(X, Y0, Plateau, KFast){
    Y <- (Y0-Plateau)*exp(-KFast*X) + Plateau
    return(Y)
}


#SpanFast=(Y0-Plateau)*PercentFast*.01
#SpanSlow=(Y0-Plateau)*(100-PercentFast)*.01
#Y=Plateau + SpanFast*exp(-KFast*X) + SpanSlow*exp(-KSlow*X)

decay2 <- function(X, Y0, Plateau, KFast, KSlow, PercentFast){
    SpanFast <- (Y0-Plateau)*PercentFast * 0.01
    SpanSlow <- (Y0-Plateau)*(100-PercentFast) * 0.01
    Y <- Plateau + SpanFast * exp(-KFast*X) + SpanSlow * exp(-KSlow*X)
    return(Y)
}


#YFast=(Y0-Plateau)*PercentFast*.01*exp(-KFast*X)
#YSlow=(Y0-Plateau)*PercentSlow*.01*exp(-KSlow*X)
#YMedium=(Y0-Plateau)*(100-PercentFast - PercentSlow)*.01*exp(-Kmedium*X
#Y=Plateau + YFast + YMedium +YSlow

decay3 <- function(X, Y0, Plateau, KFast, Kmedium, KSlow, PercentFast, PercentSlow){
    YFast <- (Y0-Plateau)*PercentFast * 0.01 * exp(-KFast*X)
    YSlow <- (Y0-Plateau)*PercentSlow * 0.01 * exp(-KSlow*X)
    YMedium <- (Y0-Plateau)*(100-PercentFast - PercentSlow) * 0.01 * exp(-Kmedium*X)
    Y <- Plateau + YFast + YMedium + YSlow
    return(Y)
}


# Graphical display of starting values
    # display_start <- function(df, model, starting){
    #     df %>% plt() +
    #         stat_function(geom = "line",
    #                       fun = model,
    #                       args = starting,
    #                       color = "red",
    #                       size = 1.3) +
    #         labs(title = "Starting values")
    # }


# Fit one of the 3 exponential functions
fit_decay <- function(df, phases, starting){
    if (phases == 1) {
        mod <- nlsLM(data = df,
                     fluorescence ~ decay1(X = time, Y0, Plateau, KFast),
                     start = starting,
                     trace = T, control =  nls.control(warnOnly = T, maxiter = 500,
                                                       minFactor = 2^-50),
                     lower = c(Y0 = -Inf, Plateau = -Inf, KFast = -Inf),
                     upper = c(Y0 = Inf, Plateau = Inf, KFast = Inf)
        )
    }
    if (phases == 2) {
        mod <- nlsLM(data = df,
                     fluorescence ~ decay2(X = time, Y0, Plateau, KFast, KSlow, PercentFast),
                     start = starting,
                     trace = T, control =  nls.control(warnOnly = T, maxiter = 500,
                                                       minFactor = 2^-50),
                     lower = c(Y0 = -Inf, Plateau = -Inf, PercentFast = 0, KFast = -Inf, KSlow = -Inf),
                     upper = c(PercentFast = 100, Y0 = Inf, Plateau = Inf, KFast = Inf, KSlow = Inf)

        )
    }
    if (phases == 3) {
        mod <- decay_model3 <- nlsLM(data = df,
                                     fluorescence ~ decay3(X = time, Y0, Plateau, KFast, Kmedium, KSlow,
                                                           PercentFast, PercentSlow),
                                     start = starting,
                                     trace = T, control =  nls.control(warnOnly = T, maxiter = 500,
                                                                       minFactor = 2^-50),
                                     lower = c(Y0 = -Inf, Plateau = -Inf, PercentFast = 0, PercentSlow = 0,
                                             KFast = -Inf, KSlow = -Inf, Kmedium = -Inf),
                                     upper = c(PercentFast = 100, PercentSlow = 100, Y0 = Inf, Plateau = Inf,
                                             KFast = Inf, KSlow = Inf, Kmedium = Inf)
        )
    }
    return(mod)
}


# enhance data with model information
extract_model <- function(df, mod){
    return(df %>% add_residuals(mod) %>% add_predictions(mod))
}


# Plot Data with fit
plot_fit <- function(df){
    p <- df %>% plt() +
        geom_line(aes(y = pred),
                  color = "darkorange",
                  size = 1.2,
                  alpha = 0.9)
    return(p)
}
