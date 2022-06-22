cr4 <- function(core.data){
  dat.CR4 <-
    core.data %>%
    group_by( MSA_NECH) %>%
    mutate(rank = rank(desc(TOTREV))) %>%
    arrange(rank) %>%
    summarize( n=n(),NTMAJ12 = NTMAJ12, revenue= sum(TOTREV), top1 = sum(nth(TOTREV, 1)), top2 = sum(nth(TOTREV, 2)),
               top3 = sum(nth(TOTREV, 3)), top4 = sum(nth(TOTREV, 4)),
               percent=(top1+top2+top3+top4)/revenue)

  return (dat.CR4)
}

cr2 <- function(core.data){
  dat.CR2 <-
    core.data %>%
    group_by( MSA_NECH) %>%
    mutate(rank = rank(desc(TOTREV))) %>%
    arrange(rank) %>%
    summarize( n=n(), revenue= sum(TOTREV), top1 = sum(nth(TOTREV, 1)), top2 = sum(nth(TOTREV, 2)),
               percent=(top1+top2)/revenue)

  return (dat.CR2)
}
