"
Generate list of edges to be ignore (called blacklist)
"

blacklist_E2_TestScore <- function(node.names, outcome.node){
  
  #progr_years is not parent of partic_age
  blacklist_1 <- data.frame(from = c("progr_years"), 
                            to   = c("partic_age"))
  #test_duration is not parent of partic_age, progr_years
  blacklist_2 <- data.frame(from = c("test_duration"),
                            to   = c("progr_years","partic_age")) 
  #outcome.node cannot be parent of anyone
  blacklist_3 <- data.frame(from = c(outcome.node),
                            to   = node.names[-grep(outcome.node, node.names)])
  
  blacklist_all <- rbind(blacklist_1,blacklist_2,blacklist_3) 
  
  return(blacklist_all)
}

generate_blacklist_E1_TestScore <- function(node.names, outcome.node){
  
  #progr_years is not parent of partic_age
  blacklist_1 <- data.frame(from = c("progr_years"), 
                            to   = c("partic_age"))
  #test_duration is not parent of partic_age, progr_years
  blacklist_2 <- data.frame(from = c("test_duration"),
                            to   = c("progr_years","partic_age")) 
  #outcome.node cannot be parent of anyone
  blacklist_3 <- data.frame(from = c(outcome.node),
                            to   = node.names[-grep(outcome.node, node.names)])
  
  blacklist_all <- rbind(blacklist_1,blacklist_2,blacklist_3) 
  
  return(blacklist_all)
}
