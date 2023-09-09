#' Create Permutations of Potential Rooftop Extension Elevations
#'
#' Function for generating visualisations of permutations of rooftop extension elevations and the number of potential combinations or a data frame containing storeys per building.
#'
#' @param storeys Vector containing the number of storeys per building
#' @param df Data frame of existing elevations. Each column should represent a building and the data frame should contain one row with the number of storeys per building
#' @param max_permitted_storey Maximum permitted storey per building
#' @param max_permitted_extstorey Maximum permitted storey per extension
#' @param output Output of function. Input 'plot' for charts. Input 'data' for a data frame containing the number of storeys per building. Defaults to 'plot'
#' @return Multiple charts depicting potential rooftop extension elevations ordered by density as well as a print-out of the number of potential combinations in the console or a data frame containing the number of storeys per building
#' @examples
#' storeys_i = c(3, 3, 4) # Establish the number of storeys per existing building
#' 
#' # Visualise potentials based on the maximum number of storeys per extension
#' extension_elevations(storeys = storeys_i, max_permitted_extstorey = 2, output = 'plot')
#' 
#' # Generate a dataset containing permutations of storeys per building
#' extension_elevations(storeys = storeys_i, max_permitted_extstorey = 1, output = 'data')
#' 
#' storeys_ii = c(3, 2, 2, 3, 4) # Establish the number of storeys per existing building
#' 
#' # Chart potential extensions using the maximum number of storeys per building
#' extension_elevations(storeys = storeys_ii, max_permitted_storey = 4, output = 'plot')
#' @export

extension_elevations = function(storeys = NULL, df = NULL, max_permitted_storey = NULL, max_permitted_extstorey = NULL, output = 'plot') {
  
  if(is.null(storeys) != T && is.null(df) != T) {
    stop('input storeys or df', call. = F)
    
  } else if(is.null(df) != T) {
    df = df
    
  } else if(is.null(storeys) != T) {
    df = as.data.frame(t(storeys))
    
  } else {
    stop('error in elevation storeys', call. = F)
  }
  
  if(is.null(max_permitted_storey) && is.null(max_permitted_extstorey) != T) {
    nbuildings = ncol(df)
    
    if(max_permitted_extstorey <= 0) {
      return(cat('Number of potential combinations: 0', " "))
      
    } else {
      ext_combinations = lapply(1:nbuildings, function(i) {
        df[,i]:(df[,i] + max_permitted_extstorey)
      })
    }
    
  } else if(is.null(max_permitted_extstorey) && is.null(max_permitted_storey) != T) {
    nbuildings = ncol(df)
    
    if(min(df) >= max_permitted_storey) {
      return(cat('Number of potential combinations: 0', " "))
      
    } else {
      ext_combinations = lapply(1:nbuildings, function(i) {
        df[,i]:max_permitted_storey
      })
    }
    
  } else if(is.null(max_permitted_extstorey) != T && is.null(max_permitted_storey) != T) {
    nbuildings = ncol(df)
    
    if(min(df) >= max_permitted_storey || max_permitted_extstorey <= 0) {
      return(cat('Number of potential combinations: 0', " "))
      
    } else {
      ext_combinations = lapply(1:nbuildings, function(i) {
        df[,i]:max_permitted_storey
      })
    }
    
  } else {
    stop('error in input of max_permitted_storey or max_permitted_extstorey', call. = F)
  }
  
  df_ext = expand.grid(ext_combinations)
  
  if(is.null(max_permitted_extstorey) != T && is.null(max_permitted_storey) != T) {
    df_ext = df_ext[apply(df_ext, 1, function(x) all(x >= df & x <= (df + max_permitted_extstorey))),]
    
  } else {
    df_ext = df_ext[apply(df_ext, 1, function(x) all(x >= df)),]
  }
  
  df_ext$sum_s = rowSums(df_ext[,1:nbuildings])
  df_ext$max_s = apply(df_ext[1:nbuildings], 1, max, na.rm = T)
  df_ext = df_ext[order(df_ext$sum_s, df_ext$max_s),]
  
  df_ext = df_ext[-1,]
  
  if(output == 'plot') {
    if(nrow(df_ext) == 0) {
      n = 0
      
    } else {
      n = nrow(df_ext)
    }
    
    cat("Number of potential combinations:", n, " ")
    
    if(n < 8) {
      x = n
      lwd = 2
      
    } else if(n > 100 & n <= 400) {
      x = 20
      lwd = 1.2
      
    } else if(n > 400) {
      x = 35
      lwd = 0.475
      
    } else {
      x = 8
      lwd = 2
    }
     
    y = ceiling(n / x)
    
    if(is.null(max_permitted_extstorey) != T && is.null(max_permitted_storey) != T && max_permitted_storey >= max(df)) {
      m = max_permitted_storey
      
    } else if(is.null(max_permitted_extstorey) != T && is.null(max_permitted_storey) != T && max_permitted_storey < max(df)) {
      m = max(df)
      
    } else if(is.null(max_permitted_extstorey) != T && is.null(max_permitted_storey)) {
      m = max(df) + max_permitted_extstorey
      
    } else if(is.null(max_permitted_storey) != T && is.null(max_permitted_extstorey) && max_permitted_storey >= max(df)) {
      m = max_permitted_storey
      
    } else {
      m = max(df)
    }
    
    par(mfrow = c(y, x), mar = c(0.175, 0.175, 2.4, 0.175))
    
    invisible(sapply(1:length(df_ext[,1]), function(i) {
      ext = df_ext[i, 1:nbuildings]
      
      barplot(as.numeric(ext[,1:nbuildings]), ylim = c(0, m), col = "#00DCF5", border = F, space = 0.075, axes = F)
      
      barplot(as.numeric(df[,1:ncol(df)]), ylim = c(0, max(df)), col = "#D9F045", border = F, space = 0.075, axes = F, add = T)
      
      abline(h = seq(1, m, 1), col = "#ffffff", lwd = lwd)
    }))
    
    legend_extcharts = function(...) {
      opar = par(fig = c(0, 1, 0, 1), mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0), new = T)
      
      on.exit(par(opar))
      
      plot(0, 0, type = 'n', xaxt = 'n', yaxt = 'n', bty = 'n')
      
      legend(...)
    }
    
    legend_extcharts(-1.085, 1.1, legend = rev(c("Existing Storey", "Potential Storey")), cex = 0.9, pch = 15, col = c("#00DCF5", "#D9F045"), text.font = 2, bty = 'n', horiz = F)
    
  } else if(output == 'data') {
    ext_data = df_ext
    
    ext_data = ext_data[,1:nbuildings]
    
    colnames(ext_data) = gsub("Var", "building_", as.character(colnames(ext_data)))
    
    rownames(ext_data) = NULL
    
    ext_data
    
  } else {
    stop('error in output parameter', call. = F)
  }
}