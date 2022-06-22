kwoka_index <- function(core.data){
  dat.kwoka <- core.data %>% 
    group_by( MSA_NECH) %>%
    mutate(rank = rank(desc(TOTREV))) %>%
    arrange(rank) %>%
    summarize( n=n(),NTMAJ12 = NTMAJ12, top1 = sum(nth(TOTREV, 1)), top2 = sum(nth(TOTREV, 2)), 
               top3 = sum(nth(TOTREV, 3)),total = top1 + top2 + top3, Kindex3 = (((top1/total)^2 +
                                                                                    (top2/total)^2+(top3/total)^2)))
  
  return (dat.kwoka)
}