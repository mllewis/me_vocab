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

get_pretty_mean  <- function(mean, low, high){
  esimate_print = round(mean, 2)
  CI_print = paste0(" [", 
                    round(low, 2),
                    ", ",
                    round(high, 2),
                    "]")
  paste(esimate_print, CI_print)
}

get_mes_by_group <- function(group, mss){
  df <-  filter(mss, trial_type == group)
  m1 <- mean(df$prop_correct, na.rm = T)
  n1 <- length(df$prop_correct)
  sd1 <- sd(df$prop_correct)
  m2 <- .5
  n2 <- n1
  sd2 <- sd1
  
  es <- compute.es::mes(m1, m2, sd1, sd2, n1, n2, verbose = F) %>%
    select(d, l.d, u.d) %>%
    mutate(mean = m1, 
           sd = sd1,
           condition = group,
           tidy_print = paste0("_M_ = ", round(mean, 2), ", _SD_ = ", round(sd, 2),
                               ", _d_ = ", d, " [", l.d, ", ", u.d, "]")) %>%
    select(condition, mean, sd, everything()) 
    
  es
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
