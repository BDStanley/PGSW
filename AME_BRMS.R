# Load packages
library(brms)
library(tidybayes)
library(tidyverse)
library(magrittr)
library(reshape2)
library(margins)

# Function 1: Compute numerical derivatives for continuous variables ----------
bayes_dydx.default <- function(model, data = NULL, variable, stepsize = 1e-7, re_formula = NULL){
  
  # Get data from model where data = NULL
  if(is.null(data) == T){
    d <- model$data
  } else {
    d <- data
  }
  
  # Get outcome from model
  resp <- model$formula$resp
  
  # Omit outcome from data
  d <-
    d %>%
    select(-resp)
  
  # Omit random effects from the data if necessary
  if(is.null(re_formula) == F){
    
    # Get random effects
    rnfx <- unique(model$ranef$group)
    
    # Omit from data
    d <-
      d %>% 
      select(-rnfx)
  }
  
  
  # Calculate observed combinations and frequencies to reduce computation time
  d <-
    d %>% 
    group_by_all() %>% 
    count(name = "w") %>% 
    ungroup()
  
  # Create function to set "h" based on "eps" to deal with machine precision
  setstep <- function(x) {
    x + (max(abs(x), 1, na.rm = TRUE) * sqrt(stepsize)) - x
  }
  
  # Calculate numerical derivative
  d1 <- d0 <- d
  
  d0[[variable]] <- d0[[variable]] - setstep(d0[[variable]])
  d1[[variable]] <- d1[[variable]] + setstep(d1[[variable]])
  
  # Add fitted draws
  f0 <- 
    d0 %>%
    add_fitted_draws(model = model,
                     re_formula = re_formula,
                     value = paste0(variable, "_d0"))
  
  f1 <- 
    d1 %>%
    add_fitted_draws(model = model,
                     re_formula = re_formula,
                     value = paste0(variable, "_d1"))
  
  # Calculate average marginal effect
  out <-
    f0 %>% 
    ungroup() %>% 
    mutate(
      me =
        f1[[paste0(variable, "_d1")]] %>%
        subtract(f0[[paste0(variable, "_d0")]]) %>% 
        divide_by(d1[[variable]] - d0[[variable]])
    ) %>% 
    group_by_at(".draw") %>% 
    summarise(ame = sum(me * w)/sum(w)) %>% 
    select(ame) %>%
    ungroup()
  
  # Return AME
  out %>%
    mutate(var = variable) %>% 
    melt(id = "var") %>% 
    mutate(variable = var) %>% 
    rename(resp = variable,
           est = value)
  
}


# Function 2: Compute Average Marginal Effect for Factor Variables ----------
bayes_dydx.factor <- function(model, data = NULL, variable, re_formula = NULL){
  
  # Get data from model where data = NULL
  if(is.null(data) == T){
    d <- model$data
  } else {
    d <- data
  }
  
  # Get outcome from model
  resp <- model$formula$resp
  
  # Omit outcome from data
  d <-
    d %>%
    select(-resp)
  
  # Omit random effects from the data if necessary
  if(is.null(re_formula) == F){
    
    # Get random effects
    rnfx <- unique(model$ranef$group)
    
    # Omit from data
    d <-
      d %>% 
      select(-rnfx)
  }
  
  # Calculate observed combinations and frequencies
  # to reduce computation time where n is large
  d <-
    d %>% 
    group_by_all() %>% 
    count(name = "w") %>% 
    ungroup()
  
  # Get factor levels
  levs <- levels(as.factor(d[[variable]]))
  base <- levs[1L]
  cont <- levs[-1L]
  
  # Create empty list for fitted draws
  f <- list()
  
  # For each list add fitted draws
  for (i in seq_along(levs)){
    
    # Fix variable in each list to factor level
    d[[variable]] <- levs[i]
    
    # Add fitted draws, weight, and summarise
    f[[i]] <- 
      d %>% 
      add_fitted_draws(model = model,
                       re_formula = re_formula,
                       value = "eff") %>% 
      group_by_at(".draw") %>% 
      summarise(eff_w = sum(eff * w)/sum(w)) %>% 
      select(eff_w) %>% 
      ungroup()
    
    # Compute contrast if not base level
    if (i > 1){
      f[[i]]$eff_w <- f[[i]]$eff_w - f[[1]][[1]]
    }
    
    # Rename column
    names(f[[i]]) <- levs[i]
    
  }
  
  # Remove data frame
  d <- NULL
  
  # Create output object
  out <- do.call(cbind, f)
  
  # Return AMEs
  if (length(cont) == 1){
    
    out <- out[, cont] %>% tibble()
    names(out) <- "est"
    
    out %>% 
      mutate(
        var = variable,
        resp = cont
      ) %>% 
      select(var, resp, est)
    
  } else {
    out[, cont] %>%
      mutate(var = variable) %>% 
      melt(id = "var") %>% 
      rename(resp = variable, est = value)
  }
}

bayes_dydx.factor_mn <- function(model, variable = NULL, data = NULL, draws = NULL, n = NULL, re_formula = NA){
  
  # Check that everything is running properly and that the
  # user has provided all of the relevant information.
  
  if(is.null(model) == T){
    
    stop("Please provide a model to the function using the 'model =' argument (e.g. model = m1)")
    
  } else if(is.null(variable) == T){
    
    stop("Please provide a variable name to compute average marginal effects for using the 'variable =' argument (e.g. variable = 'x'")
    
  }
  
  
  # If the user hasn't provided their own data to use
  # we use the data included in the brms object itself.
  
  if(is.null(data) == T){
    
    d <- model$data
    
  } else {
    
    d <- data
    
  }
  
  
  # If the user has specified a non-null value for n
  # we use this as the number of random samples the
  # user wants us to take from their data.
  
  if(is.null(n) == F){
    
    d <- d %>% sample_n(n)
    
  }
  
  
  # Next, we get the name of the outcome variable from
  # the brms object...
  
  resp <- model$formula$resp
  
  
  # ... then we omit it from the data
  
  d <- d %>% select(-resp)
  
  
  # Next, if the user has leave the re_formula argument
  # as NA, we remove random effects for the model if it
  # has them. Otherwise, we include them.
  
  if(is.na(re_formula) == T & nrow(model$ranef) != 0){
    
    # Get random effects
    rnfx <- unique(model$ranef$group)
    
    # Omit from data
    d <-
      d %>% 
      select(-all_of(rnfx))
    
  }
  
  
  # Calculate observed combinations and frequencies
  # to reduce computation time where n is large
  
  d <-
    d %>% 
    group_by_all() %>% 
    count(name = "w") %>% 
    ungroup()
  
  
  # Now we get all of the levels of the factor that
  # we are using.
  
  levs <- levels(as.factor(d[[variable]]))
  base <- levs[1L]
  cont <- levs[-1L]
  
  
  # We need somewhere to put all of the fitted draws
  # that we're about to compute so we'll create an
  # empty list.
  
  f <- list()
  
  
  # Then we'll loop over each of the factor levels and
  # compute the fitted draws.
  
  for (i in seq_along(levs)){
    
    # First, we fix all cases of our variable of interest
    # to the index factor level.
    
    d[[variable]] <- levs[i]
    
    
    # Second, we compute the fitted draws, weight them by
    # the number of cases, and then summarise the effect
    
    f[[i]] <- 
      d %>% 
      tidybayes::add_fitted_draws(
        model = model,
        n = draws,
        re_formula = re_formula,
        value = "eff"
      ) %>% 
      group_by(
        .draw,
        .category
      ) %>% 
      summarise(eff_w = sum(eff * w)/sum(w)) %>% 
      ungroup() %>% 
      select(eff_w, .category)
    
    
    # Third, we compute the contrast between the
    # comparison level and the base level
    
    if (i > 1){
      
      f[[i]]$eff_w <- f[[i]]$eff_w - f[[1]][[1]]
      
    }
    
    
    # Finally, we rename the variables in the each
    # list using the level name and "resp".
    
    names(f[[i]]) <- c(levs[i], "resp")
    
    
  }
  
  
  # Now we can combine the individual datasets in
  # the list into a single dataset.
  
  out <- do.call(cbind, f)
  
  
  # Then we convert them to long format to make
  # them a little easier to deal with.
  
  out <- 
    out %>% 
    pivot_longer(
      cols = {{levs}},
      names_to = "fct",
      values_to = "ame"
    ) %>% 
    mutate(
      fct = fct %>% factor(levels = levs)
    )
  
  
  # Then we drop the reference category
  
  out <- out %>% filter(fct != {{base}})
  
  
  # Finally, we return the AMEs to the user
  
  return(out)
  
}