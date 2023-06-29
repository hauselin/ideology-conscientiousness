library(data.table)
library(tidyverse)
library(data.table)
library(broom)
library(glue)
library(BFpack)



compute_bf <- function(model, n, hypothesis = NULL) {
    
    if (class(model)[1] == "coeftest") {
        # see Mulder 2019 p32
        message("Model class: coeftest")
        bf <- BF(model[, 1], Sigma = diag(model[, 2]^2), n = n, hypothesis = hypothesis)
    } else if (class(model)[1] == "fixest") {
        message("Model class: fixest")
        bf <- BF(coef(model), Sigma = vcov(model), n = nobs(model), hypothesis = hypothesis)
    } else if (class(model)[1] == "glmerMod") {
        message("Model class: glmerMod")
        bf <- BF(fixef(model), Sigma = as.matrix(vcov(model)), n = nobs(model), hypothesis = hypothesis)
    } else {
        bf <- BF(model, n = nobs(model), hypothesis = hypothesis)
    }
    return(bf)
}

summarybf <- function(bf, comparisons = NULL, digits = 2) {
    
    print(data.frame(bf$hypotheses))
    if (is.null(comparisons)) {
        comparisons <- rep("1", length(bf$hypotheses))
        for (i in 1:length(bf$hypotheses)) {
            comparisons[i] <- paste0(i, " vs ", comparisons[i])
        }
    }
    d <- data.table(comparison = comparisons, bf10 = bf$BFmatrix_confirmatory[, 1])
    d$bf01 <- 1 / d$bf10
    
    d$bf01 <- round(d$bf01, digits = digits)
    d$bf10 <- round(d$bf10, digits = digits)
    # d <- d[comparison != "1 vs 1"]
    return(d)
}


loop_model1 <- function(data, type, hypothesis) {
    ids <- data[, unique(responseid)]
    subjidx <- seq(50, length(ids), by = 5)
    if (max(subjidx) < length(ids)) {
        subjidx <- c(subjidx, length(ids))
    }
    res_list <- list()
    i <- 0
    for (si in subjidx) {
        sids <- ids[1:si]
        tempdat <- data[responseid %in% sids]
        print(tempdat[, .N, keyby = .(veracity)])
        temp_n <- tempdat[, n_distinct(responseid)]
        print(paste0("n: ", temp_n))
        
        tempmodel <- glm(share ~ ideology * bfi_c, family = binomial, data = tempdat)
        model <- coeftest(tempmodel, vcovCL(tempmodel, ~ responseid + headline_id, type, fix = TRUE))
        
        nas <- sum(is.na(tidy(model)$std.error))
        if (nas > 0) {
            next
        }
        
        bf <- compute_bf(model, nobs(tempmodel), hypothesis)
        res <- summarybf(bf)
        res$n <- temp_n
        rm(tempdat)
        i <- i + 1
        res_list[[i]] <- res
        
        # add b, se, p
        termstr <- str_split(hypothesis, " ")[[1]][1]
        params <- data.table(tidy(model))[term == termstr]
        res$b <- params$estimate
        res$se <- params$std.error
        res$uci <- res$b + params$std.error * 1.96
        res$lci <- res$b - params$std.error * 1.96
        res$p <- params$p.value
        res_list[[i]] <- res
        
        
    }
    return(data.table(bind_rows(res_list)))
}


loop_model2 <- function(data, type, hypothesis) {
    ids <- data[, unique(responseid)]
    subjidx <- seq(50, length(ids), by = 5)
    if (max(subjidx) < length(ids)) {
        subjidx <- c(subjidx, length(ids))
    }
    res_list <- list()
    i <- 0
    for (si in subjidx) {
        sids <- ids[1:si]
        tempdat <- data[responseid %in% sids]
        print(tempdat[, .N, keyby = .(veracity)])
        temp_n <- tempdat[, n_distinct(responseid)]
        print(paste0("n: ", temp_n))
        
        tempmodel <- glm(share ~ ideology * (bfi_c + bfi_e + bfi_a + bfi_n + bfi_o + 
                                                 age + gender + edu + attention_score + ctsq_aot), family = binomial, data = tempdat)
        model <- coeftest(tempmodel, vcovCL(tempmodel, ~ responseid + headline_id, type, fix = TRUE))
        
        nas <- sum(is.na(tidy(model)$std.error))
        if (nas > 0) {
            next
        }
        
        bf <- compute_bf(model, nobs(tempmodel), hypothesis)
        res <- summarybf(bf)
        res$n <- temp_n
        rm(tempdat)
        i <- i + 1
        res_list[[i]] <- res
        
        # add b, se, p
        termstr <- str_split(hypothesis, " ")[[1]][1]
        params <- data.table(tidy(model))[term == termstr]
        res$b <- params$estimate
        res$se <- params$std.error
        res$uci <- res$b + params$std.error * 1.96
        res$lci <- res$b - params$std.error * 1.96
        res$p <- params$p.value
        res_list[[i]] <- res
        
    }
    return(data.table(bind_rows(res_list)))
}

loop_model3 <- function(data, type, hypothesis) {
    ids <- data[, unique(responseid)]
    subjidx <- seq(100, length(ids), by = 5)
    if (max(subjidx) < length(ids)) {
        subjidx <- c(subjidx, length(ids))
    }
    res_list <- list()
    i <- 0
    for (si in subjidx) {
        sids <- ids[1:si]
        tempdat <- data[responseid %in% sids]
        print(tempdat[, .N, keyby = .(veracity)])
        temp_n <- tempdat[, n_distinct(responseid)]
        print(paste0("n: ", temp_n))
        
        tempmodel <- glm(share ~ veracity * ideology * (bfi_c + bfi_e + bfi_a + bfi_n + bfi_o + 
                                                            age + gender + edu + attention_score + ctsq_aot), family = binomial, data = tempdat)
        model <- coeftest(tempmodel, vcovCL(tempmodel, ~ responseid + headline_id, type, fix = TRUE))
        
        nas <- sum(is.na(tidy(model)$std.error))
        if (nas > 0) {
            next
        }
        
        bf <- compute_bf(model, nobs(tempmodel), hypothesis)
        res <- summarybf(bf)
        res$n <- temp_n
        rm(tempdat)
        i <- i + 1
        res_list[[i]] <- res
        
        # add b, se, p
        termstr <- str_split(hypothesis, " ")[[1]][1]
        params <- data.table(tidy(model))[term == termstr]
        res$b <- params$estimate
        res$se <- params$std.error
        res$uci <- res$b + params$std.error * 1.96
        res$lci <- res$b - params$std.error * 1.96
        res$p <- params$p.value
        res_list[[i]] <- res
        
    }
    return(data.table(bind_rows(res_list)))
}
















increase_digits <- function(x, tidy_estimates, digits) {
    if (sum(as.numeric(x) == 0)) {
        idx <- which(as.numeric(x) == 0)
        temp_fmt <- paste0('%#.', digits + 1, 'f')
        temp_b <- sprintf(tidy_estimates$estimate[idx], fmt = temp_fmt)
        x[idx] <- temp_b
        if (sum(as.numeric(x) == 0)) {
            idx <- which(as.numeric(x) == 0)
            zeros <- rep(0, length(idx))
            temp_fmt <- paste0('%#.', digits, 'f')
            x[idx] <- sprintf(zeros, fmt = temp_fmt)
        }
        return(x)
    } else {
        return(x)
    }
    
}


#


sum2 <- function(model, conf.int = TRUE, digits = 2, pval_digits = 3, table = FALSE, verbose = FALSE) {
    dt0 <- data.table(tidy(model, conf.int))
    if (table) {
        return(dt0)
    }
    
    output_format <- tryCatch(output_format, 
             error = function(e) {
                 output_format = "b = {b} [{lower}, {upper}], p = {p}"
                 }
             ) 
    if (verbose) {
       message(output_format)
    }
    
    fmt <- paste0('%#.', digits, 'f')
    b <- sprintf(dt0$estimate, fmt = fmt)
    b <- increase_digits(b, dt0, digits)
   
    b_low <- sprintf(dt0$conf.low, fmt = fmt)
    b_low <- increase_digits(b_low, dt0, digits)
    b_high <- sprintf(dt0$conf.high, fmt = fmt)
    b_high <- increase_digits(b_high, dt0, digits)
    
    pval_fmt <- paste0('%#.', pval_digits, 'f')
    pval <- sprintf(dt0$p.value, fmt = pval_fmt)
    pval <- gsub("0.", "p = .", pval, fixed = TRUE)
    pval <- ifelse(dt0$p.value < 0.001, "p < .001", pval)
    
    res <- glue("b = {b} [{b_low}, {b_high}], {pval}")
    dt1 <- data.table(term = dt0$term, res = res)
    return(dt1)
}

parse_format <- function(output_format) {
    out <- list(b = FALSE, se = FALSE, lower = FALSE, upper = FALSE, statistic = FALSE, p = FALSE, df = FALSE, es = FALSE)
    
    for (n in names(out)) {
        if (grepl(paste0("{", n, "}"), output_format, fixed = TRUE)) {
            out[[n]] <- TRUE
        }
    }
    return(out)    
}


sumh <- function(model, digits = 2, conf.int = TRUE, conf.level = 0.95, pval_digits = 3, table = FALSE, verbose = FALSE, adjust = FALSE, exponentiate = FALSE) {
    if (verbose) {
        message("Available parameters: {b}, {se}, {lower}, {upper}, {statistic}, {df}, {p}, {es}")
    }
    
    dt0 <- data.table(tidy(model, conf.int = conf.int, conf.level = conf.level, effects = "fixed", exponentiate = exponentiate))
    if (table) return(dt0)
    
    output_format <- tryCatch(get("output_format", envir = .GlobalEnv), 
                              error = function(e) {
                                  output_format = "b = {b} [{lower}, {upper}], p = {p}"
                              }
    ) 
    out_fmt <- parse_format(output_format)
    
    if (verbose & !table) message(paste0("Format: ", output_format))
    
    fmt <- paste0('%#.', digits, 'f')
    
    # get requested parameters
    if (out_fmt$b) {
        b <- get_param("estimate", dt0, fmt, digits, adjust)    
    }
    if (out_fmt$se) {
        se <- get_param("std.error", dt0, fmt, digits, adjust)
    }
    if (out_fmt$statistic) {
        statistic <- get_param("statistic", dt0, fmt, digits, adjust)
    }
    
    if (out_fmt$df) {
        df <- get_param("df", dt0, "%.0f", 0, adjust)
    }
    
    if (conf.int & out_fmt$lower) {
        lower <- get_param("conf.low", dt0, fmt, digits, adjust)
    }
    if (conf.int & out_fmt$upper) {
        upper <- get_param("conf.high", dt0, fmt, digits, adjust)
    } 
    
    if (out_fmt$p) {
        p_fmt <- paste0('%#.', pval_digits, 'f')
        p <- get_param("p.value", dt0, p_fmt, digits, adjust)
        p <- gsub("0.", ".", p, fixed = TRUE)
        p <- ifelse(dt0$p.value < 0.001, ".001" , p)
        idx_smallp <- which(dt0$p.value < 0.001)
    }
    
    res <- glue(output_format)
    dt1 <- data.table(term = dt0$term, res = res)
    
    # fix pvalues
    if (out_fmt$p) {
        if (length(idx_smallp) > 0) {
           ps <- dt1[idx_smallp, res]
           ps <- gsub("= .001", "< .001", ps)
           ps <- gsub("=.001", "<.001", ps)
           dt1 <- as.data.frame(dt1)
           dt1[idx_smallp, "res"] <- ps
        }
    }
    
    return(data.table(dt1))
}


check_rounding <- function(x) {
    x_temp <- gsub("0", "", x)
    x_temp <- gsub("-.", "", x_temp)
    if (sum(x_temp == "")) {
        idx <- which(x_temp == "")
        x[idx] <- gsub("-", "", x[idx])
    }
    return(x)
}

get_param <- function(param, df_params, fmt, digits, adjust) {
    if (!param %in% names(df_params)) {
        df_params[, (param) := NA]
    }
    
    # degrees of freedom is sometimes called parameter
    if (param == "df" & ("parameter" %in% names(df_params))) {
        df_params[, (param) := parameter]
    }
    
    p_fmt <- sprintf(df_params[, get(param)], fmt = fmt)
    if (adjust) {
        p_fmt <- increase_digits(p_fmt, df_params, digits)    
    }
    p_fmt <- check_rounding(p_fmt)
    return(p_fmt)
}



