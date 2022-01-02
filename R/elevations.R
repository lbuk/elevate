#' Create Visualisations of Building Elevations
#'
#' Function for charting building elevations and the number of possible combinations using permutations.
#'
#' @param max_storey Maximum permitted storey
#' @param min_storey Minimum permitted storey
#' @param n_buildings Number of buildings
#' @return Small multiple charts depicting building elevations and the number of possible combinations
#' @examples
#' elevations(n_buildings = 7, min_storey = 4, max_storey = 5)
#' @export

elevations = function(n_buildings, min_storey, max_storey) {
  
  elevations_combinations = rep(list(min_storey:max_storey), n_buildings)
  df_elevations = expand.grid(elevations_combinations)
  
  df_elevations$rowid = 1:nrow(df_elevations)
  
  df_elevations$sum = rowSums(df_elevations[,1:n_buildings])
  df_elevations = df_elevations[order(df_elevations$sum),]
  
  nrow = nrow(df_elevations)
  
  cat("Number of possible combinations:", nrow)
  
  y = ceiling(nrow(df_elevations)/10)
  
  par(mfrow = c(y, 10), mar = c(0.2, 0.2, 0.54, 0.2))
  
  for (i in 1:length(df_elevations[,1])) {
    buildings = df_elevations[i, 1:n_buildings]
    barplot(as.numeric(buildings), ylim = c(0, max_storey), col = "#cccccc", border = F, space = 0.075, axes = F)
    abline(h = seq(1, 8, 1), col = "white", lwd = 0.85)
  }
}