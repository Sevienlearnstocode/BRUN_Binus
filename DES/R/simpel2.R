#' A Simpel2 Function
#'
#' This function allows you to simulate Simple queuing system with more than one single entity type (single queue)
#' @param rate entity arrival rate
#' @param awal earliest arrival time
#' @param times repetition
#' @param attributes whether your want to display data attributes. Default to TRUE
#' @keywords more_than_one_single_entity
#' @export
#' @examples simpel2()

simple2 <- function(rate, awal, times, attributes=TRUE){

  install.packages("simmer")
  install.packages("tidyverse")
  require(simmer)
  require(tidyverse)
  env <- simmer("sederhana2")

  traj <- trajectory() %>%
    seize("Mesin",1) %>%
    timeout_from_attribute("lama") %>%
    release("Mesin",1)

  trajA <- trajectory() %>%
    set_attribute("lama", function() rexp(1, rate[1])) %>%
    join(traj)

  trajB <- trajectory() %>%
    set_attribute("lama", function() rexp(1, rate[2])) %>%
    join(traj)

  env %>%
    add_resource("Mesin", 1) %>%
    add_generator("jobA", trajA, at(cumsum(rep(awal[1], times[1]))), mon=2) %>%
    add_generator("jobB", trajB, at(cumsum(rep(awal[2], times[2]))), mon=2) %>%
    run()

  if (attributes){
    out <- as_tibble(get_mon_attributes(env))
  } else {
    out <- as_tibble(get_mon_arrivals(env))
  }

  return(out)
}


