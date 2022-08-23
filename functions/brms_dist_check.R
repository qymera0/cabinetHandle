brms_dist_check <- function(mdl, nvar, ncol){
        
        seqNames <-
                fixef(mdl) %>%
                rownames() %>% 
                paste('b_', ., sep = '')
        
        
        postDf <- 
                posterior_samples(mdl) %>% 
                dplyr::select(1:all_of(nvar)) %>% 
                relocate(all_of(seqNames)) %>% 
                gather()
        
        effecDist <-
                postDf %>% 
                ggplot(aes(x = value)) +
                geom_histogram(color = "grey92", 
                               fill = "grey67",
                               size = .2, 
                               bins = 40) +
                stat_pointinterval(aes(y = 0),
                                   point_interval = mode_hdi, 
                                   .width = c(.95, .5)) +
                scale_y_continuous(NULL, breaks = NULL) +
                xlab(NULL) +
                facet_wrap(~key, scales = "free", ncol = ncol) + 
                theme_bw() + 
                theme(
                        panel.grid = element_blank()
                )
        
        print(effecDist)
        
        # print(conditional_effects(mdl))
        
        post <- as_draws_array(mdl)
        
        print(mcmc_intervals(post[ , ,c(1:nvar)]))
        
        #### Chains convergence
        
        # Trace plot
        
        print(mcmc_trace(post[ , ,c(1:nvar)]))
        
        # Rhat statistics
        
        rHats <- rhat(mdl)
        
        print(mcmc_rhat(rHats))
        
        # Effective sample size
        
        ratios <- neff_ratio(mdl)
        
        print(mcmc_neff(ratios))
        
        # Auto correlation
        
        print(mcmc_acf(post[ , ,c(1:nvar)]))
        
        #### R^2
        
        # bayes_R2(mdl, summary = F) %>% 
        #         as_tibble() %>% 
        #         
        #         ggplot(aes(x = R2)) +
        #         geom_histogram(color = "grey92", fill = "grey67",
        #                        size = .2, bins = 50) +
        #         stat_pointinterval(aes(y = 0), 
        #                            point_interval = mode_hdi, .width = .95) +
        #         scale_y_continuous(NULL, breaks = NULL) +
        #         labs(title = expression(paste("Bayesian ", italic(R)^2)),
        #              subtitle = 'Simple model',
        #              x = NULL) 
        # 
}