#' glmmTMB_randslope_ci
#'
#' By Emily Scott, updated by Nathan Brouwer
#'
#' @param mod_working Model to get confidence intervals (CI) for
#' @param randeff Name of random effect as quoted string
#' @export

glmmTMB_randslope_ci <- function(mod_working, randeff){
  message("This will take a second...")

  ranefsx <- ranef(mod_working)

  # get focal ranefs
  ranef_betas    <- ranefsx$cond[[randeff]]


  # get number of coefs
  # head(ranefsx$cond[[randeff]])
  N_randslopes <- dim(ranefsx$cond[[randeff]])[1] # 79 x 33




  # Get TMB sd report
  s1 <- TMB::sdreport(obj = mod_working$obj,
                      getJointPrecision = TRUE,
                      bias.correct = TRUE)

  # Get fixed eff summary
  ## summary output of `sdreport` are the coefficients and SE for the **main effects** of the model
  fix_eff_summary <- summary(s1,"fixed")
  n_fix_eff <- length(grep("^beta$",row.names(fix_eff_summary)))

  # Get raneff eff summary
  raneff_summary <- as.data.frame(summary(s1, "random"))

  # convert s1 to s2
  s2 <- sqrt(s1$diag.cov.random)

  # solve()
  test <- solve(s1$jointPrecision)

  # determine total number of parameters (?)
  parameters <- mod_working$obj$env$par
  parameters <- as.data.frame(parameters)


  # get parameters names
  parameters$names <- names(mod_working$obj$env$par)

  # get sd from "test" object
  # length(diag(test)) == dim(parameters)[1]
  parameters$sd <- sqrt(diag(test))

  i <- N_randslopes

  # colmn to store ....
  parameters$variable <- NA  # create new column


  # Get names for fixed effects
  names_fix_eff <- names(fixef(mod_working)$cond)
  parameters$variable[1:n_fix_eff] <-  names_fix_eff

  # set up indices for naming
  tot_rand_int_and_slopes <- N_randslopes*2
  tot_betas               <- N_randslopes*2+n_fix_eff

  # name random intercepts and slopes
  # length((n_fix_eff+1):tot_betas) == (N_randslopes*2)
  parameters$variable[(n_fix_eff+1):tot_betas] <-   c(rep("Intercept", N_randslopes),
                                                      rep("time_cts", N_randslopes))

  # create column for species names
  parameters$spp <- NA

  # set up name
  ## "Population" = "Population mean" = fixed effect
  fixed_eff_names          <- rep("Population", 4) #why population?
  rand_eff_intercept_names <- rownames(ranef(mod_working)[[1]][[randeff]])
  rand_eff_slope_names     <- rand_eff_intercept_names

  parameters$spp[1:n_fix_eff] <- fixed_eff_names
  parameters$spp[(n_fix_eff+1):tot_betas]  <- c(rand_eff_intercept_names,
                                                rand_eff_slope_names)

  # place to hold sd
  parameters$combo_sd <- NA


  # determine indices
  intercept.first <- N_randslopes+n_fix_eff # last intercept
  slope.first     <- N_randslopes+n_fix_eff +1  #first slope

  # ? fix eff?
  slope.i <- 4

  # note: i starts at 0
  # 0:78?  78 = slope.first-n_fix_eff
  for (i in 0:78)  # TODO: hard coded
  {
    # combined SD  using delta method (?)
    parameters$combo_sd[slope.first+i] <- sqrt(test[slope.i,slope.i] + # fixef SLOPE var
                                                 test[slope.first+i,slope.first+i] +  # ranef slope var
                                                 2*test[slope.first+i,slope.i]) # fixef-ranef COVARIANCE
  }

  slope.last <- 2*N_randslopes + n_fix_eff   #last slope

  # get subset that we want
  ##  could do this with na.omit too
  subset_for_merge <- parameters[slope.first:slope.last,c("spp","combo_sd")]
  colnames(subset_for_merge)[1] <- "id"

  sd_int   <- s2[1:N_randslopes]
  sd_slope <- s2[(N_randslopes+1):2*N_randslopes]

  slopes <- ranef(mod_working)[["cond"]][[randeff]] # 79 x 2

  # SE for fixed effects
  s0 <- mod_working$sdr

  # combine fixed effect and random effect slope
  #time coefficient                      # ? individual time slope?
  fixed_eff_slope <-  summary(mod_working)[["coefficients"]]$cond["time_cts","Estimate"]
  slopes$time_cts_tot <- fixed_eff_slope+  slopes$time_cts

  # calculate 95% CI
  slopes$time_sd  <- sd_slope
  slopes$time_lb  <- slopes$time_cts_tot - 1.96*slopes$time_sd
  slopes$time_ub  <- slopes$time_cts_tot + 1.96*slopes$time_sd

  # set labels
  slopes$spp <- substring(rownames(slopes), 1, 4)
  slopes$loc <- substring(rownames(slopes), 6, 9)
  slopes$id <- rownames(slopes)

  #
  slopes.unique <- slopes[!duplicated(slopes$spp),]

  levels.spp <- slopes.unique[order(slopes.unique$time_cts_tot),]$spp
  levels.id <- slopes[order(slopes$time_cts_tot),]$id
  slopes$spp2 <- ordered(slopes$spp, levels = levels.spp)
  slopes$id2 <- ordered(slopes$id, levels = levels.id)

  # merge
  #browser()
  slopes <- dplyr::left_join(slopes,
                      subset_for_merge, by = "id")

  # calculate lower (lb) and upper (ub) bound of CIs
  ## NOTE: use slopes$time_cts_tot (as shown and as was in orig script)
  ###      NOTE time_cts
  slopes$time_lb2  <- slopes$time_cts_tot - 1.96*slopes$combo_sd
  slopes$time_ub2  <- slopes$time_cts_tot + 1.96*slopes$combo_sd

  slopes$Sig <- ifelse(slopes$time_ub2 < 0, "Sig.", "NS")
  slopes$Sig <- as.factor(slopes$Sig)

  # Remove AR1 columns
  if(length(grep("as.ordered",names(slopes))) > 0){
    slopes <- slopes[,-grep("as.ordered",names(slopes))]
  }


  return(slopes)
}
