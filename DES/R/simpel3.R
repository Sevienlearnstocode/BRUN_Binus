#' A Simpel3 Function
#'
#' This function allows you to simulate queuing system with more than one serial server, and each server has its own queue
#' @param rn number of simulation run
#' @param tri triangular distribution parameters
#' @param unif uniform distribution parameters
#' @param res whether you want to display data resources. Default to TRUE
#' @keywords every_server_has_each_queue
#' @export
#' @examples simpel3()

simpel3 <- function(tri, unif, rn, res=TRUE){

  install.packages("EnvStats")
  install.packages("simmer")
  install.packages("tidyverse")
  require(EnvStats) # untuk mendapatkan fungsi rtri
  require(simmer)
  require(tidyverse)
  tri <- sort(tri); unif <- sort(unif)
  env <- simmer("serial")

  traj <- trajectory() %>%
    seize("operator") %>%
    timeout(function() rtri(1, tri[1], tri[3], tri[2])) %>%
    release("operator") %>%
    seize("mesin") %>%
    timeout(function() runif(1, unif[1], unif[2])) %>%
    release("mesin")

  env %>%
    add_resource("operator", 1) %>%
    add_resource("mesin", 1) %>%
    add_generator("material", traj, function() 2) %>%
    run(rn)

  if (res){
    out <- as_tibble(get_mon_resources(env))
  } else {
    out <- as_tibble(get_mon_arrivals(env))
  }
  return(out)
}

