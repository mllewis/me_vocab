### Helper functions for synthesis paper ###

# get MA effect size (random effect or mixed-effect [multilevel = T]) 
overall_es <- function(ma_data, multilevel){
  if (multilevel){
    model = metafor::rma.mv(d_calc, V = d_var_calc,
                            random = ~ 1 | short_cite, data = ma_data)
  } else {
    model = metafor::rma(d_calc, d_var_calc, data = ma_data)
  }
  data.frame(dataset = ma_data$short_name[1],
             overall.d = model$b,
             ci_lower = model$ci.lb,
             ci_upper = model$ci.ub)
}

# Fail safe N
get_fail_safe_N <- function(dataset, ma_data, targ) {
  fsn_string = fsn(d_calc, d_var_calc, data = ma_data, target = targ, type = "Orwin")$fsnum 
  data.frame(dataset = dataset, fsn_string = fsn_string)
}

## Eggers test for funnel assymetry
eggers_tests <- function(ma_data){
  model = rma(d_calc, d_var_calc, data = ma_data) # model (mixed-effect model not implemented for eggers test)
  egg.random = regtest(model) # Egger's test
  data.frame(dataset = ma_data$short_name[1],
             egg.random.z = egg.random$zval,
             egg.random.p = egg.random$pval)
}


## gets CIs on p-curves
get_all_CIS_multi <- function(df, alpha, increment) {
  ps <- seq(increment, alpha, increment)
  props = ps %>%
    map(function(p,d){sum(d == p)}, df$p_round) %>%
    unlist()
  cis = MultinomialCI::multinomialCI(props, alpha = alpha)
  data.frame(dataset = df$dataset[1],
             p = ps,
             ci.lower = cis[,1],
             ci.upper = cis[,2])
}



forest <- function(data, moderators, sort_order) {
  
  alpha <- .05
  
  data_u = data %>%
            mutate(short_cite = make.unique(as.character(short_cite))) %>%
    filter(infant_type == "typical")
  

  base_model <- metafor::rma.mv(yi = data_u[["d_calc"]], V = data_u[["d_var_calc"]],
                    random = ~ 1 | data_u[["short_cite"]],
                    slab = data_u[["short_cite"]], 
                    method = "REML")
  
   moderators <- c("mean_age", "ME_trial_type")
   mods <- paste(moderators, collapse = "+")
   rma_formula <- as.formula(sprintf("%s ~ %s", "d_calc", mods))
   mod_model <- metafor::rma.mv(rma_formula, V = data_u[["d_var_calc"]],
                        random = ~ 1 | short_cite,
                        slab = short_cite, data = data_u,
                        method = "REML") 

  f <- fitted(mod_model)
  p <- predict(mod_model)
  
  
  #c("weight (1/variance)" = "variances",
  #  "effect size" = "effects",
   # "model estimate" = "estimate",
   # "alphabetical" = "study_ID",
   # "chronological" = "year")
  
  base_model_data <- data.frame(short_cite = "RE Model",
                                effects = base_model$b[1],
        effects.cil = base_model$ci.lb,
        effects.cih = base_model$ci.ub,
        inverse_vars = NA,
        estimate = NA,
        estimate.cil = NA,
        estimate.cih = NA,
        ME_trial_type = NA,
        identity = 1,
        es_type = "meta")
  
  forest_data <- data.frame(effects = as.numeric(mod_model$yi.f),
                            variances = mod_model$vi.f) %>%
    mutate(effects.cil = effects -
             qnorm(alpha / 2, lower.tail = FALSE) * sqrt(variances),
           effects.cih = effects +
             qnorm(alpha / 2, lower.tail = FALSE) * sqrt(variances),
           estimate = as.numeric(f),
           short_cite = names(f),
           estimate.cil = p$ci.lb,
           estimate.cih = p$ci.ub,
           inverse_vars = 1/variances,
           identity = 1) %>%
    left_join(mutate(data_u, by= "short_cite")) %>%
    arrange(-short_cite) %>%
    mutate(short_cite = factor(short_cite, levels = short_cite)) %>%
    select(short_cite, effects, effects.cil, effects.cih, inverse_vars,
           estimate, estimate.cil, estimate.cih, ME_trial_type, identity) %>%
    mutate(es_type = "") %>%
    bind_rows(base_model_data)
  
  


  ggplot(forest_data, aes(x = short_cite, y = effects, 
             ymin = effects.cil, ymax = effects.cih)) +
    geom_linerange() +
    geom_point(aes(y = effects, size = inverse_vars), shape = "s") +
    geom_pointrange(aes(x = short_cite, y = estimate,
                               ymin = estimate.cil, 
                                ymax = estimate.cih,
                               shape = ME_trial_type), 
                    color = "blue", alpha = .2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
    coord_flip() +
    scale_size_continuous(guide = FALSE) +
    #scale_colour_solarized(name = "", labels = labels, guide = guide) +
    xlab("") +
    ylab("Effect Size") +
    theme(text = element_text(size = 8)) +
    facet_grid(es_type~., scales= 'free', space='free')
    
}

### OVERALL ESTIMATES
## grand model (all effect sizes)
grand_model <- rma.mv(d_calc, V = d_var_calc,
                              random = ~ 1 | short_cite,
                              method = "REML",
                              data = ma_c)

grand_model_data <- data.frame(model_name = "grand",
                                estimate = grand_model$b[1],
                                estimate.cil = grand_model$ci.lb,
                               estimate.cih = grand_model$ci.ub,
                               es_type = "grand",
                               model_type = "overall") 

# NN only model
nn_model <- rma.mv(d_calc, V = d_var_calc,
                   random = ~ 1 | short_cite,
                   method = "REML",
                   data = filter(ma_c, ME_trial_type == "NN"))

nn_model_data <- data.frame(model_name = "NN",
                            estimate = nn_model$b[1],
                            estimate.cil = nn_model$ci.lb,
                            estimate.cih = nn_model$ci.ub,
                            es_type = "sub",
                            model_type = "sub")

# FN only model
fn_model <- rma.mv(d_calc, V = d_var_calc,
                   random = ~ 1 | short_cite,
                   method = "REML",
                   data = filter(ma_c, ME_trial_type == "FN"))

fn_model_data <- data.frame(model_name = "FN",
                            estimate = fn_model$b[1],
                            estimate.cil = fn_model$ci.lb,
                            estimate.cih = fn_model$ci.ub,
                            es_type = "sub",
                            model_type = "sub")


# typical only model
typical_model <- rma.mv(d_calc, V = d_var_calc,
                      random = ~ 1 | short_cite,
                      method = "REML",
                      data = filter(ma_c, infant_type == "typical"))

typical_model_data <- data.frame(model_name = "typical",
                                 estimate = typical_model$b[1],
                                 estimate.cil = typical_model$ci.lb,
                                 estimate.cih = typical_model$ci.ub,
                                 es_type = "sub",
                                 model_type = "sub")


# multi-lingual only model
ml_model <- rma.mv(d_calc, V = d_var_calc,
                        random = ~ 1 | short_cite,
                        method = "REML",
                        data = filter(ma_c, infant_type == "multilingual"))


ml_model_data <- data.frame(model_name = "multilingual",
                                 estimate = ml_model$b[1],
                                 estimate.cil = ml_model$ci.lb,
                                 estimate.cih = ml_model$ci.ub,
                                 es_type = "sub",
                            model_type = "sub")


# non-typical only model
nt_model <- rma.mv(d_calc, V = d_var_calc,
                   random = ~ 1 | short_cite,
                   method = "REML",
                   data = filter(ma_c, infant_type == "NT"))

nt_model_data <- data.frame(model_name = "NT",
                                 estimate = nt_model$b[1],
                                 estimate.cil = nt_model$ci.lb,
                                 estimate.cih = nt_model$ci.ub,
                                 es_type = "sub",
                                 model_type = "sub")


ns <- bind_rows(list(count(ma_c, infant_type) %>% rename(model_name = infant_type)), 
                count(ma_c, ME_trial_type) %>% rename(model_name = ME_trial_type)) %>%
                add_row(model_name = "grand", n = mean(.$n))

all_overall_estimates <- bind_rows(list(grand_model_data, 
                                        typical_model_data, 
                                        ml_model_data, 
                                        nt_model_data,
                                        fn_model_data,
                                        nn_model_data)) %>%
                          left_join(ns) %>%
                          mutate(es_string = paste0(round(estimate,2),
                                                   " [", 
                                                    round(estimate.cil,2),
                                                    ", ",
                                                    round(estimate.cih,2),
                                                    "]")) %>%
                          add_row(model_name = "", model_type = "sub") %>%
                          mutate(model_name = fct_relevel(model_name, 
                                                     "typical", "multilingual",
                                                     "NT", "FN", "NN",  "", "grand"),
                                   model_name = fct_rev(model_name),
                                   model_type = fct_rev(model_type),)
  


 breaks <- seq(-.25, 2.5, .25)
 labels <- as.character(breaks)
 labels[!(breaks%%1==0)] <- ''
 tick.sizes <- rep(.5, length(breaks))
 tick.sizes[(breaks%%1==0)] <- 1
 
p1 <- ggplot(all_overall_estimates, aes(x = model_name,
                                  y = estimate, 
                                  color = model_type)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_point(aes(size = n, 
                 shape = model_type)) +
  geom_linerange(aes(ymin = estimate.cil, 
                     ymax = estimate.cih)) +
  geom_text(aes(label = es_string, x = model_name, y = 3.25), 
            size = 3.5) +
  coord_flip() 

p1 <- p1 + scale_shape_manual(values = c(16, 15)) +
  scale_color_manual(values = c("#000000", '#FF0000')) +
  scale_size_continuous(guide = FALSE) +
  scale_y_continuous(breaks = breaks, 
                     labels = labels,
                     limits = c(-.25, 3.75),
                     name = "Effect Size") +
  scale_x_discrete(labels = c("NT" = "Non-Typically-Developing populations", 
                              "grand" = "Overall estimate",
                              "typical" = "Typically-Developing populations",
                              "FN" = "Familiar-Novel trial type", 
                              "NN" = "Novel-Novel trial type",
                              "multilingual" = "Multilingual populations"),
                   name = "") + 
  ggtitle("Mixed-effect estimates of overall effect size") 

p1 <- p1 + 
  theme_bw() +
  theme(axis.line.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        axis.ticks.x = element_line(size = tick.sizes),
        axis.ticks.y = element_line(size = 0),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12, 
                                   colour = c('red', rep('black', 6)),
                                   face = c("bold", rep("plain", 6))),
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14))

p1

### MODERATED ESTIMATES
grand_modelm <- rma.mv(d_calc ~ ME_trial_type + mean_age, V = d_var_calc,
                      random = ~ 1 | short_cite,
                      method = "REML",
                      data = ma_c)
grand_model_datam <- data.frame(estimate = as.vector(grand_modelm$b[,1]),
                                estimate.cil = as.vector(grand_modelm$ci.lb),
                                estimate.cih = as.vector(grand_modelm$ci.ub),
                                effect_name = rownames(grand_modelm$b)) %>%
                        mutate(model_name = "Grand estimate",
                               es_type = "grand",
                               model_type = "moderated") 

# typical
typical_modelm <- rma.mv(d_calc ~ ME_trial_type + mean_age, V = d_var_calc,
                        random = ~ 1 | short_cite,
                        method = "REML",
                        data = filter(ma_c, infant_type == "typical"))

typical_model_datam <- data.frame(estimate = as.vector(typical_modelm$b[,1]),
                                estimate.cil = as.vector(typical_modelm$ci.lb),
                                estimate.cih = as.vector(typical_modelm$ci.ub),
                                effect_name = rownames(typical_modelm$b)) %>%
  mutate(model_name = "Typical populations",
         es_type = "sub",
         model_type = "moderated") 


# multi-lingual only model
ml_modelm <- rma.mv(d_calc ~ ME_trial_type + mean_age, V = d_var_calc,
                   random = ~ 1 | short_cite,
                   method = "REML",
                   data = filter(ma_c, infant_type == "multilingual"))

ml_model_datam <- data.frame(estimate = as.vector(ml_modelm$b[,1]),
                                  estimate.cil = as.vector(ml_modelm$ci.lb),
                                  estimate.cih = as.vector(ml_modelm$ci.ub),
                                  effect_name = rownames(ml_modelm$b)) %>%
  mutate(model_name = "Multilingual populations",
         es_type = "sub", 
         model_type = "moderated") 


# non-typical only model
nt_modelm <- rma.mv(d_calc ~ ME_trial_type + mean_age, V = d_var_calc,
                   random = ~ 1 | short_cite,
                   method = "REML",
                   data = filter(ma_c, infant_type == "NT"))


nt_model_datam <- data.frame(estimate = as.vector(nt_modelm$b[,1]),
                             estimate.cil = as.vector(nt_modelm$ci.lb),
                             estimate.cih = as.vector(nt_modelm$ci.ub),
                             effect_name = rownames(nt_modelm$b)) %>%
  mutate(model_name = "Non-typically developing populations",
         es_type = "sub",
         model_type = "moderated") 


all_moderated_estimates <- bind_rows(list(grand_model_datam, 
                                          typical_model_datam, 
                                          ml_model_datam, 
                                          nt_model_datam))



