#' A Simpel Function
#'
#' This function allows you to simulate simple queueing system with single server (single queue)
#' @param ak Entity arrival rate
#' @param lm service duration rate
#' @param rd rounding value
#' @param rsc amount of resources
#' @param rn number of simulation runs
#' @param ongoing record the arrival data of entities that have not yet left the system. Default to TRUE.
#' @keywords single_queue
#' @export
#' @examples simpel()

simpel <- function(ak, lm, rd, rsc, rn, ongoing=TRUE){

  install.packages("simmer")
  install.packages("tidyverse")
  require(simmer)
  require(tidyverse)

  env <- simmer("sederhana")

  # Waktu Antar Kedatangan (AK)
  AK <- function() round(rexp(n=1, rate=ak),rd)

  # Membangkitkan lamanya layanan
  Lama <- function() round(rexp(n=1, rate=lm),rd)

  lintas <- trajectory() %>%
    seize("ATM") %>%
    timeout(Lama) %>%
    release("ATM")

  env %>%
    add_resource("ATM", 1) %>%
    add_generator("nasabah", lintas, AK) %>%
    run(rn) %>% invisible

  output.dat <- as_tibble(get_mon_arrivals(env, ongoing = ongoing))
  out <- subset(output.dat, start_time>0)
  return(out)
}
