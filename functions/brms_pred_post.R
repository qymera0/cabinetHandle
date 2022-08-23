brms_pred_post <- function(y, mdl, ndens, lim = c(0, 0)){
        
        y <- as.vector(y)
        
        yRep <- 
                posterior_predict(mdl, cores = 4) %>%
                as.matrix()
        
        if ((lim[1] == 0) & (lim[2] == 0)) {
                
                print(ppc_dens_overlay(y, yRep, n_dens = ndens)) 
                
        } else {
                
                print(ppc_dens_overlay(y, yRep, n_dens = ndens) +
                              xlim(lim[1], lim[2]))
                
        }
        
}