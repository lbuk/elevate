#' Building Elevations for Potential Rooftop Extensions
#'
#' Function for creating building elevation small multiples for potential rooftop extensions using permutations.
#'
#' @param df Data frame containing building elevations including the number of storeys
#' @param max_permitted_storey Maximum permitted storey
#' @return Small multiple charts depicting rooftop extension elevations according to the maximum storeys permitted and the number of possible combinations in the console
#' @examples
#' extension_elevations(df = df, max_permitted_storey = 5)
#' @export

extension_elevations = function(df, max_permitted_storey) {
  
  n_buildings = ncol(df)
  min_storey = min(df[1:ncol(df)])
  max_storey = max(df[1:ncol(df)])
  max_permitted_storey = max_permitted_storey
  
  df$sum = rowSums(df)
  
  if(n_buildings == 2){elevations_combinations = list(df[,1]:max_permitted_storey, df[,2]:max_permitted_storey)} else if(n_buildings == 3){elevations_combinations=list(df[,1]:max_permitted_storey, df[,2]:max_permitted_storey, df[,3]:max_permitted_storey)} else if(n_buildings == 4){elevations_combinations=list(df[,1]:max_permitted_storey, df[,2]:max_permitted_storey, df[,3]:max_permitted_storey, df[,4]:max_permitted_storey)} else if(n_buildings == 5){elevations_combinations=list(df[,1]:max_permitted_storey, df[,2]:max_permitted_storey, df[,3]:max_permitted_storey, df[,4]:max_permitted_storey, df[,5]:max_permitted_storey)} else if(n_buildings == 6){elevations_combinations=list(df[,1]:max_permitted_storey, df[,2]:max_permitted_storey, df[,3]:max_permitted_storey, df[,4]:max_permitted_storey, df[,5]:max_permitted_storey, df[,6]:max_permitted_storey)} else if(n_buildings == 7){elevations_combinations=list(df[,1]:max_permitted_storey, df[,2]:max_permitted_storey, df[,3]:max_permitted_storey, df[,4]:max_permitted_storey, df[,5]:max_permitted_storey, df[,6]:max_permitted_storey, df[,7]:max_permitted_storey)} else if(n_buildings == 8){elevations_combinations=list(df[,1]:max_permitted_storey, df[,2]:max_permitted_storey, df[,3]:max_permitted_storey, df[,4]:max_permitted_storey, df[,5]:max_permitted_storey, df[,6]:max_permitted_storey, df[,7]:max_permitted_storey, df[,8]:max_permitted_storey)} else if(n_buildings == 9){elevations_combinations=list(df[,1]:max_permitted_storey, df[,2]:max_permitted_storey, df[,3]:max_permitted_storey, df[,4]:max_permitted_storey, df[,5]:max_permitted_storey, df[,6]:max_permitted_storey, df[,7]:max_permitted_storey, df[,8]:max_permitted_storey, df[,9]:max_permitted_storey)} else if(n_buildings == 10){elevations_combinations=list(df[,1]:max_permitted_storey, df[,2]:max_permitted_storey, df[,3]:max_permitted_storey, df[,4]:max_permitted_storey, df[,5]:max_permitted_storey, df[,6]:max_permitted_storey, df[,7]:max_permitted_storey, df[,8]:max_permitted_storey, df[,9]:max_permitted_storey, df[,10]:max_permitted_storey)}
  
  df_elevations = expand.grid(elevations_combinations)
  
  df_elevations$rowid = 1:nrow(df_elevations)
  
  if(n_buildings == 2){df_elevations = subset(df_elevations, df_elevations[,1] >= df[,1] & df_elevations[,2] >= df[,2])} else if(n_buildings == 3){df_elevations = subset(df_elevations, df_elevations[,1] >= df[,1] & df_elevations[,2] >= df[,2] & df_elevations[,3] >= df[,3])} else if(n_buildings == 4){df_elevations = subset(df_elevations, df_elevations[,1] >= df[,1] & df_elevations[,2] >= df[,2] & df_elevations[,3] >= df[,3] & df_elevations[,4] >= df[,4])} else if(n_buildings == 5){df_elevations = subset(df_elevations, df_elevations[,1] >= df[,1] & df_elevations[,2] >= df[,2] & df_elevations[,3] >= df[,3] & df_elevations[,4] >= df[,4] & df_elevations[,5] >= df[,5])} else if(n_buildings == 6){df_elevations = subset(df_elevations, df_elevations[,1] >= df[,1] & df_elevations[,2] >= df[,2] & df_elevations[,3] >= df[,3] & df_elevations[,4] >= df[,4] & df_elevations[,5] >= df[,5] & df_elevations[,6] >= df[,6])} else if(n_buildings == 7){df_elevations = subset(df_elevations, df_elevations[,1] >= df[,1] & df_elevations[,2] >= df[,2] & df_elevations[,3] >= df[,3] & df_elevations[,4] >= df[,4] & df_elevations[,5] >= df[,5] & df_elevations[,6] >= df[,6] & df_elevations[,7] >= df[,7])} else if(n_buildings == 8){df_elevations = subset(df_elevations, df_elevations[,1] >= df[,1] & df_elevations[,2] >= df[,2] & df_elevations[,3] >= df[,3] & df_elevations[,4] >= df[,4] & df_elevations[,5] >= df[,5] & df_elevations[,6] >= df[,6] & df_elevations[,7] >= df[,7] & df_elevations[,8] >= df[,8])} else if(n_buildings == 9){df_elevations = subset(df_elevations, df_elevations[,1] >= df[,1] & df_elevations[,2] >= df[,2] & df_elevations[,3] >= df[,3] & df_elevations[,4] >= df[,4] & df_elevations[,5] >= df[,5] & df_elevations[,6] >= df[,6] & df_elevations[,7] >= df[,7] & df_elevations[,8] >= df[,8] & df_elevations[,9] >= df[,9])} else if(n_buildings == 10){df_elevations = subset(df_elevations, df_elevations[,1] >= df[,1] & df_elevations[,2] >= df[,2] & df_elevations[,3] >= df[,3] & df_elevations[,4] >= df[,4] & df_elevations[,5] >= df[,5] & df_elevations[,6] >= df[,6] & df_elevations[,7] >= df[,7] & df_elevations[,8] >= df[,8] & df_elevations[,9] >= df[,9] & df_elevations[,10] >= df[,10])}
  
  df_elevations$sum = rowSums(df_elevations[,1:n_buildings])
  df_elevations = df_elevations[order(df_elevations$sum),]
  
  df_elevations = df_elevations[-1,]
  
  if(nrow(df_elevations) == 0) {nrow = 0} else{nrow = nrow(df_elevations)}
  
  cat("Number of possible combinations:", nrow)
  
  if(nrow > 50){x = 8} else{x = 6}
  if(nrow < 6){y = 6} else{y = ceiling(nrow(df_elevations)/x)}
  
  par(mfrow = c(y, x), mar = c(0.2, 0.2, 0.3, 0.2))
  
  for (i in 1:length(df_elevations[,1])) {
    buildings = df_elevations[i, 1:n_buildings]
    
    barplot(as.numeric(buildings[,1:n_buildings]), ylim = c(0, max_permitted_storey), col = "#2deaff", border = F, space = 0.075, axes = F)
    if(nrow == 0){cat("")} else{barplot(as.numeric(df[,1:ncol(df)-1]), ylim = c(0, max(df)), col = "#cccccc", border = F, space = 0.075, axes = F, add = T)}
    abline(h = seq(1, 8, 1), col = "white", lwd = 0.7)

  }
  
}
