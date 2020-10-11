#' Longest distance observations
#'
#' Returns the two consecutive observations of a vessel with the longest sailed distance
#'
#' @param data The data that has ships observations
#' @param ship_name The name of the vessel
#'
#' @return A dataframe with two observations
#' @examples
#' # The two consecutive observations describing the longest sailed distance for the vessel 'KAROLI'
#' get_longest_dist_obs(data = ships, ship_name = 'KAROLI')
get_longest_dist_obs <- function(data, ship_name){
  data %>%
    # keep only the selected ship
    filter(SHIPNAME == ship_name) %>%
    # sort by observation date time
    arrange(desc(DATETIME)) %>%
    # calculate sequential traveled distance using geodist to get distance between each two consecutive observations
    mutate(dist_since_last_obs = round(c(geodist(., sequential = TRUE, measure = "geodesic"),0),0),
           # calculate total traveled distance
           total_traveled_distance = sum(dist_since_last_obs),
           # calculate average speed
           average_speed = mean(SPEED)) %>%
    # keep only the two observations relevant to the longest traveled distance
    filter(row_number() %in% c(which.max(dist_since_last_obs == max(dist_since_last_obs)),
                               which.max(dist_since_last_obs == max(dist_since_last_obs))+1)) %>%
    # create position variable to help distinguishing the two observations
    mutate(position = c("end", "start"))
}
