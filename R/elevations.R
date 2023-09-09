#' Generate Building Elevation Permutations
#'
#' Function for charting building elevations and the number of potential combinations using permutations or a data frame of storeys per building.
#'
#' @param max_storey Maximum permitted storey per building
#' @param min_storey Minimum permitted storey per building
#' @param nbuildings Number of buildings
#' @param output Output of function. Input 'plot' for charts. Input 'data' for a data frame containing the number of storeys per building
#' @return Multiple charts depicting building elevations and a print-out of the number of potential combinations in the console or a data frame containing the number of storeys per building
#' @examples
#' # Charts based on the number of buildings, min storey and max storey
#' elevations(nbuildings = 6, min_storey = 4, max_storey = 5, output = 'plot')
#' 
#' # Dataset containing permutations of storeys per building
#' elevations(nbuildings = 8, min_storey = 4, max_storey = 6, output = 'data')
#' @export

elevations = function(nbuildings, min_storey, max_storey, output = 'plot') {
  
  if(nbuildings <= 1) {
    stop('nbuildings should be > 1', call. = F)
    
  } else {
    nbuildings = nbuildings
  }
  
  if(max_storey <= 0) {
    stop('max_storey should be > 0', call. = F)
    
  } else if(max_storey < min_storey) {
    stop('min_storey should be <= max_storey', call. = F)
    
  } else {
    max_storey = max_storey
    min_storey = min_storey
  }
  
  el_combinations = rep(list(min_storey:max_storey), nbuildings)
  
  df_el = expand.grid(el_combinations)
  
  df_el$sum_s = rowSums(df_el[,1:nbuildings])
  df_el$max_s = apply(df_el[1:nbuildings], 1, max, na.rm = T)
  df_el = df_el[order(df_el$sum_s, df_el$max_s),]
  
  if(output == 'plot') {
    n = nrow(df_el)
    
    cat("Number of potential combinations:", n, " ")
    
    if(n < 10) {
      x = n
      lwd = 1.5
      
    } else if(n > 100 & n <= 400) {
      x = 20
      lwd = 1.2
      
    } else if(n > 400) {
      x = 35
      lwd = 0.425
      
    } else {
      x = 10
      lwd = 1.5
    }
    
    y = ceiling(n / x)
    
    m = max_storey
    
    par(mfrow = c(y, x), mar = c(0.2, 0.2, 2, 0.2))
    
    invisible(sapply(1:length(df_el[,1]), function(i) {
      build = df_el[i, 1:nbuildings]
      
      barplot(as.numeric(build), ylim = c(0, m), col = "#D10081", border = F, space = 0.075, axes = F)
      
      abline(h = seq(1, m, 1), col = "#ffffff", lwd = lwd)
    }))
    
    legend_elcharts = function(...) {
      opar = par(fig = c(0, 1, 0, 1), mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0), new = T)
      
      on.exit(par(opar))
      
      plot(0, 0, type = 'n', xaxt = 'n', yaxt = 'n', bty = 'n')
      
      legend(...)
    }
    
    legend_elcharts(-1.085, 1.1, legend = "Storey", cex = 1.05, pch = 15, col = "#D10081", text.font = 2, bty = 'n', horiz = T)
    
  } else if(output == 'data') {
    el_data = df_el
    
    el_data = el_data[,1:nbuildings]
    
    colnames(el_data) = gsub("Var", "building_", as.character(colnames(el_data)))
    
    rownames(el_data) = NULL
    
    el_data
    
  } else {
    stop('error in output parameter', call. = F)
  }
}