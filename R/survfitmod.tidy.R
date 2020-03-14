survfitmod.tidy <- function(fit, var1, year1, year2, data) {

  group_var1 <- enquo(var1)

  a <- data %>%
    group_by(!!group_var1) %>%
    summarize(n=n()) %>%
    mutate(strata=as.character(!!group_var1)) %>%
    select(strata, n) # Tällä saadaan totaalimäärät


  b <- tidy(fit) %>%
    group_by(strata) %>%
    summarize(events = sum(n.event))  %>%
    separate(strata, c("trash", "strata"), sep="=") %>%
    select(-trash) #eventit mukaan

  c <- tidy(fit) %>%
    filter(time>=year1) %>%
    group_by(strata) %>%
    slice(which.min(time)) %>%
    separate(strata, c("trash", "strata"), sep="=") %>%
    mutate(n.risk1 = n.risk,
           surv1 = round(estimate*100, 1),
           conf.low = round(conf.low*100, 1),
           conf.high = round(conf.high*100, 1)) %>%
    unite(conf.int1, conf.low, conf.high, sep = " - ") %>%
    select(strata, n.risk1, surv1, conf.int1)

  d <- tidy(fit) %>%
    filter(time>=year2) %>%
    group_by(strata) %>%
    slice(which.min(time)) %>%
    separate(strata, c("trash", "strata"), sep="=") %>%
    mutate(n.risk2 = n.risk,
           surv2 = round(estimate*100, 1),
           conf.low = round(conf.low*100, 1),
           conf.high = round(conf.high*100, 1)) %>%
    unite(conf.int2, conf.low, conf.high, sep = " - ") %>%
    select(strata, n.risk2, surv2, conf.int2)


  a %>%
    left_join(b, by = "strata") %>%
    left_join(c, by = "strata") %>%
    left_join(d, by = "strata") %>%
    arrange(desc(events))
}
