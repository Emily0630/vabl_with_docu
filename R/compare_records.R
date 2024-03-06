compare_records <- function(df_1, df_2, fields,
                               fields_1 = fields, fields_2 = fields,
                               types = rep("bi", length(fields)),
                               breaks = rep(list(NA), length(types)),
                               distance_metric = "Levenshtein"){

  n1 <- nrow(df_1)
  n2 <- nrow(df_2)
  FF <- length(fields)

  if(typeof(breaks) == "double"){
    breaklist <- rep(list(-Inf, breaks, Inf), length(types))
  }

  if(typeof(breaks) == "list"){
    breaklist <- rep(list(NA), length(types))

    for(f in 1:F){
      if(types[f] == "lv" & breaks[[f]] == NA){
        breaklist[[f]] <- c(0, .25, .5)
      }
      if(types[[f]] == "lv"){
        breaklist[f]] <- c(-Inf, breaks[[f]], Inf)
      }
      if(types[f] == "nu" & breaks[[f]] == NA){
        breaklist[[f]] <- c(0, 1)
      }
      if(types[[f]] == "nu"){
        breaklist[[f]] <- c(-Inf, breaks[[f]], Inf)
      }

    }

  }



  # breaks_lv <- unique(c(-Inf, breaks_lv, Inf))
  # breaks_nu <- unique(c(-Inf, breaks_nu, Inf))

  ids <- expand.grid(1:n1, 1:n2)
  ids_1 <- ids[, 1]
  ids_2 <- ids[, 2]
  comparisons <- vector(mode = "list", length = F)
  ohe <- vector(mode = "list", length = F)

  # FS agreement levels
  for(f in 1:FF){
    if(types[f] == "bi"){
      comp <- df_1[ids_1, fields_1[f]] == df_2[ids_2, fields_2[f]]
      comp <- (!comp) + 1
      comparisons[[f]] <- factor(comp)
    }
    if(types[f] == "lv"){
      if(distance_metric == "Levenshtein"){
        distance <- RecordLinkage::levenshteinDist(as.character(df_1[ids_1, fields_1[f]]),
                                       as.character(df_2[ids_2, fields_2[f]]))
      }
      if(distance_metric == "Damerau-Levenshtein"){
      # Damerau-Levenshtein distance, so transpositions count as 1.
      # In contrast, BRL uses standard Levenshtein, so transpositions count as 2
      distance <- 1 - levitate::lev_ratio(as.character(df_1[ids_1, fields_1[f]]),
                                          as.character(df_2[ids_2, fields_2[f]]),
                                          useNames = F)
      }

      comp <- cut(distance,
                  breaks = breaklist[[f]]) %>%
        as.integer() %>%
        factor()
      comparisons[[f]] <- comp

    }

    if(types[f] == "nu"){
      distance <- abs(df_1[ids_1, fields_1[f]] - df_2[ids_2, fields_2[f]])
      comp <- cut(distance,
                  breaks = breaklist[[f]],
                  include.lowest = T) %>%
        as.integer()
      comparisons[[f]] <- comp
    }
  }

  n_levels <- lapply(comparisons, levels) %>%
    sapply(., length)

  # Convert to one-hot encoding
  # TODO: Test for speed vs onehot encoding function, or lapply
  for(f in 1:FF){
    ohe[[f]] <- matrix(0, nrow = n1 * n2, ncol = n_levels[f])
    for(ell in 1:n_levels[f]){
      ohe[[f]][comparisons[[f]] == ell, ell] <- 1
    }
  }



  gamma <- do.call(cbind, ohe) %>%
    data.frame()

  # Might be useful for compatibility with other packages
  # gamma_fs <- do.call(cbind, comparisons) %>%
  #   data.frame()

  out <- list(comparisons = gamma,
              n1 = n1,
              n2 = n2,
              n_levels = n_levels)

}

# ohe <- comparisons %>%
#   lapply(. , function(x){
#     if(any(is.na(x)) == T){
#       scorecard::one_hot(data.frame(x), nacol_rm = T) %>%
#         as.matrix()
#     } else {
#       scorecard::one_hot(data.frame(x)) %>%
#         as.matrix()
#     }
#   }) %>%
#   do.call(data.frame, .)

