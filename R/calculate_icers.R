# todo: clean up, make compatible with PSA

#-------------------------------------------------#
#### R functions to compute ICERs of ANY model ####
#-------------------------------------------------#
# Source: https://miqdad.freeasinspeech.org.uk/icer_calculator/
compute_icers = function(non_dominated){
  icers = non_dominated %>%
    arrange(Cost,desc(Effect))

  if(nrow(non_dominated)>1){
    icers[1,"ICER"] = NA
    for(i in 2:nrow(non_dominated)){
      icers[i,"ICER"] = (icers[i,"Cost"] - icers[i-1,"Cost"])/(icers[i,"Effect"] - icers[i-1,"Effect"])
    }
  }
  return(icers)
}

calculate_icers = function(data){
  # check data is in correct format
  if(nrow(data)<2 | ncol(data)!=3){
    return(NULL)
  }
  colnames(data) = c("Strategy","Cost","Effect")
  # remove dominated strategies
  data = data %>% arrange(Cost,desc(Effect))
  dominated = data[FALSE, ]
  for(i in 1:(nrow(data)-1)){
    for(j in (i+1):nrow(data)){
      if(data[j,"Effect"]<=data[i,"Effect"]){
        dominated = union(dominated,data[j,])
      }
    }
  }
  non_dominated = setdiff(data, dominated)

  # remove extendedly dominated strategies
  # extended dominance means ICER > that of both previous and next strategies in order of cost
  ext_dominated = non_dominated[FALSE,]
  if(nrow(non_dominated)>3){
    ext_dominated_count = nrow(ext_dominated)
    prev_ext_dominated_count = -1 # run atleast once
    while(ext_dominated_count>prev_ext_dominated_count & (nrow(non_dominated)-ext_dominated_count)>3){
      non_dominated_set = compute_icers(setdiff(non_dominated,ext_dominated))
      prev_ext_dominated_count = ext_dominated_count
      for(i in 3:(nrow(non_dominated_set)-1)){
        if(non_dominated_set[i,"ICER"]>non_dominated_set[i-1,"ICER"] & non_dominated_set[i,"ICER"]>non_dominated_set[i+1,"ICER"]){
          ext_dominated = union(ext_dominated,non_dominated_set[i,c("Strategy","Cost","Effect")])
        }
      }
      ext_dominated_count = nrow(ext_dominated)
    }
  }

  # calculate ICERs for those strategies not dominated
  # nor etendedly dominated
  non_ext_dominated = setdiff(non_dominated,ext_dominated) %>%
    compute_icers() %>%
    mutate(Status = "ND")

  dominated = dominated %>%
    mutate(ICER = NA, Status = "D")

  ext_dominated = ext_dominated %>%
    mutate(ICER = NA, Status = "ED")

  # recombine all results to produce final output
  results = bind_rows(non_ext_dominated,dominated,ext_dominated)

  return(results)
}

### plot
plot_strategies = function(icers, labels=FALSE){
  if(is.null(icers)){
    return(NULL)
  } else {
    icers$Status = factor(icers$Status,levels=c("D","ED","ND"),labels=c("Dominated","Extendedly Dominated","Non-Dominated"))
    icer_plot = ggplot(icers,aes(x=Effect,y=Cost,group=Status,colour=Status,linetype=Status,label=Strategy)) +
      geom_point(alpha=0.5) +
      geom_line() +
      scale_color_manual(name="Strategy",values=c("red","blue","black"),drop=FALSE) +
      scale_linetype_manual(name="Strategy",values=c("blank","blank","solid"),drop=FALSE) +
      theme_minimal()
    if(labels){
      icer_plot = icer_plot + geom_label(hjust="top", vjust="left", size=3)
    }
    return(icer_plot)
  }
}

### Test functions
library(dplyr)
m.ce <- read.csv("data/CostQalyInputFile.csv")
m.icer <- calculate_icers(m.ce)

m.icer
