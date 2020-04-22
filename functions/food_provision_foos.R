
# Estimate delta catch ----------------------------

estimate_delta_catch <- function(k_protected, e, m, r, k, effort_assumption){
  
  R = k_protected
  
  R[R > 1] <- 1 
  
  e[e > 1] <- 1 
  
  e_bau <- e
  
  R_bau <- 0
  
  catch_bau <- e_bau*(m*k*(1 - R_bau)/(e_bau*R_bau + m)*(1 - e_bau*(1 - R_bau)*m/((e_bau*R_bau + m)*r)))
  
  if(effort_assumption == 1){
    
    e_mpa <- e
    
  } else if (effort_assumption == 2){
    
    e_mpa <- 1 - (1 - e_bau)^(1/(1 - R))
    
  }
  
  catch_mpa <- e_mpa*(m*k)*(1 - R)/(e_mpa*R + m)*(1 - e_mpa*(1 - R)*m/((e_mpa*R + m)*r))
  
  delta_catch <- catch_mpa - catch_bau
  
  return(delta_catch)
  
}


# Estimate delta catch slopes ------------------

estimate_catch_slopes <- function(k_protected, e, m, r, k, effort_assumption){
  
  R = k_protected
  
  R[R > 1] <- 1 
  
  e[e > 1] <- 1 
  
  if(effort_assumption == 1){
    
    slope <- -e*k*m*(m + e)*((e*r + 2*e*m)*R + m*r - 2*e*m)/(r*(e*R + m)^3)
    
  } else if (effort_assumption == 2){
    
    A <- (1 - e)^(1/(1 - R))
    
    part_a <- A*log(1 - e)*k*m*(1 - (1 - A)*m*(1 - R)/(r*((1 - A)*R + m)))/((1 - R)*((1 - A)*R + m))
    
    part_b <- (1 - A)*k*m*(1 - (1 - A)*m*(1 - R)/(r*((1 - A)*R + m)))/((1 - A)*R + m)
    
    part_c <- (1 - A)*k*m*(1 - R)*(- A*log(1 - e)*R/(1 - R)^2 - A + 1)*(1 - (1 - A)*m*(1 - R)/(r*((1 - A)*R + m)))/((1 - A)*R + m)^2
    
    part_d <- (1 - A)*k*m*(1 - R)*(A*log(1 - e)*m/(r*(1 - R)*((1 - A)*R + m)) + (1 - A)*m/(r*((1 - A)*R + m)) + (1 - A)*m*(1 - R)*(-A*log(1 - e)*R/(1 - R)^2 - A + 1)/(r*((1 - A)*R + m)^2))/((1 - A)*R + m)
    
    slope <- - part_a - part_b - part_c + part_d
    
  }
  
  slope[R == 1] <- -9*10^9
  
  return(slope)
  
}
