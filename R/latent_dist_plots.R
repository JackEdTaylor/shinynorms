dloglog <- function(x, location=0, scale=1) {
  ordinal::dgumbel(x, location=location, scale=scale, max=TRUE)
}

dcloglog <- function(x, location=0, scale=1) {
  ordinal::dgumbel(x, location=location, scale=scale, max=FALSE)
}

get_dfun <- function(link = "probit") {
  switch(
    link,
    probit = dnorm,
    logit = dlogis,
    loglog = dloglog,
    cloglog = dcloglog,
    cauchit = dcauchy
  )
}

plot_dist <- function(link = "probit") {
  dfun <- get_dfun(link = link)
  
  tibble(
    x = seq(-5, 5, 0.001),
    d = dfun(x, 0, 1)
  ) %>%
    ggplot(aes(x, d)) +
    geom_line(size=1.5) +
    labs(x = "Latent Value", y = "Density")
}

plot_thresh <- function(m) {
  dfun <- get_dfun(m$link)
  
  thresh_locs <- m$coefficients[grepl("|", names(m$coefficients), fixed=TRUE)]
  thresh_labs <- paste("lambda", sprintf("[%s]", 1:length(thresh_locs)), sep="")
  
  cut_locs_min <- c(-Inf, thresh_locs)
  cut_locs_max <- c(thresh_locs, Inf)
  
  cut_labs <- m$y.levels
  
  thresh_tbl <- tibble(
    xmin = cut_locs_min,
    xmax = cut_locs_max,
    rating = cut_labs
  )
  
  abs_dens_lim <- max(abs(c(thresh_locs, 4))) + 1
  
  dens_tbl <- tibble(
    x = seq(-abs_dens_lim, abs_dens_lim, 0.001),
    d = dfun(x, 0, 1)
  )
  
  ggplot() +
    geom_rect(aes(xmin=xmin, xmax=xmax, fill=rating), ymin=-Inf, ymax=Inf, alpha=0.5, data=thresh_tbl) +
    geom_vline(xintercept=thresh_locs, linetype="dashed", size=1) +
    geom_line(aes(x, d), size=1.5, data=dens_tbl) +
    scale_x_continuous(sec.axis = dup_axis(name=NULL, breaks=thresh_locs, labels=label_parsed(thresh_labs))) +
    scale_fill_viridis_d() +
    labs(x = "Latent Value", y = "Density", fill = "Rating")
}
