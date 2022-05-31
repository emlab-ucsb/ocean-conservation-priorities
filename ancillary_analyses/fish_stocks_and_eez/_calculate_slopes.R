calculate_slopes <- function(objective,
                            current_state,
                            max_slopes,
                            z_bio = 0.25,
                            z_carbon = 1,
                            effort_assumption,
                            e = stocks_info$ex_rate,
                            m = stocks_info$m,
                            k = stocks_info$k,
                            r = stocks_info$r,
                            feature_group = group_ids,
                            feature_wts){

  if(objective == "carbon"){

    slopes <- pmin(max_slopes, z_carbon*current_state^(z_carbon - 1))

  } else if (objective  ==  "biodiversity"){

    slopes <- pmin(max_slopes, z_bio*current_state^(z_bio - 1))

  } else if (objective  ==  "food"){

    slopes <- estimate_catch_slopes(k_protected = current_state,
                                    effort_assumption = effort_assumption,
                                    e = e,
                                    m = m,
                                    k = k,
                                    r = r)
  } else if (objective == "multi"){

    slopes <- vector(mode = "numeric", length = length(feature_group))

    slopes[feature_group == 1] <- pmin(max_slopes_b, z_bio*current_state[feature_group == 1]^(z_bio - 1))

    slopes[feature_group == 2] <- pmin(max_slopes_c, z_carbon*current_state[feature_group == 2]^(z_carbon - 1))

    slopes[feature_group == 3] <- estimate_catch_slopes(k_protected = current_state[feature_group == 3],
                                                        effort_assumption = effort_assumption,
                                                        e = e,
                                                        m = m,
                                                        k = k,
                                                        r = r)
    slopes <- slopes*feature_wts
  }

  return(slopes)

}
