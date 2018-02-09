### helper functions for ME_vocab paper

forest <- function(model, moderators) {
  f <- fitted(model)
  p <- predict(model)
  
  forest_data <- data.frame(effects = as.numeric(model$yi.f),
                            variances = model$vi.f) %>%
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
    left_join(mutate(mod_data(), short_cite = make.unique(short_cite))) %>%
    arrange_(.dots = list(sprintf("desc(%s)", input$forest_sort),
                          "desc(effects)")) %>%
    mutate(short_cite = factor(short_cite, levels = short_cite))
  
  labels <- if (mod_group() == "all_mod") NULL else
    setNames(paste(mod_data()[[mod_group()]], "  "),
             mod_data()[[mod_group()]])
  guide <- if (mod_group() == "all_mod") FALSE else "legend"
  
  qplot(short_cite, effects, ymin = effects.cil, ymax = effects.cih,
        geom = "linerange",
        data = forest_data) +
    geom_point(aes(y = effects, size = inverse_vars)) +
    geom_pointrange(aes_string(x = "short_cite", y = "estimate",
                               ymin = "estimate.cil", ymax = "estimate.cih",
                               colour = mod_group()), 
                    pch = 17) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
    coord_flip() +
    scale_size_continuous(guide = FALSE) +
    scale_colour_solarized(name = "", labels = labels, guide = guide) +
    xlab("") +
    ylab("Effect Size")
}