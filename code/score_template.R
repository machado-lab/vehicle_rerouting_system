score_template <- function(prrs_limit, # time to check the status of the farms in the past (e.g., 364)
                          prrs_period, # time to check vehicles visiting infected farms ini the past (e.g., 30)
                          ped_limit,
                          ped_period,
                          asf_limit,
                          asf_period,
                          network_period_time,
                          max_wait_execute,
                          export = F){
  
  # rules ####
  working_status <- c("maintenance", "busy", "free") # vehicle status
  asf <- c(T, F) # asfv risk event
  prrs  <- c(T, F) # prrs risk event
  ped  <- c(T, F) # ped risk event
  clean_event  <- c(T, F) # cleaning event
  different_community  <- c(T, F) # between farm communities event
  time_to_execute_limit <- c(T, F) # shipment delay event
  
  # Factor combination ####
  var_combinations <- expand.grid(working_status = working_status,
                                  asf = asf,
                                  prrs = prrs,
                                  ped = ped,
                                  clean_event = clean_event,
                                  different_community = different_community,
                                  time_to_execute_limit = time_to_execute_limit) %>%
    data.frame(stringsAsFactors = F)
  
  # Score vehicles from 1 to 19 ####
  var_combinations$score <- NA
  
  # 1. ####
  # if vehicle does not have records, probalby is inactive
  var_combinations$score[var_combinations$working_status == "maintenance"] <- 1
  # 2. ####
  # it does not matter the other disease status if a vehicle visited an ASF positive farm
  var_combinations$score[var_combinations$asf == T & is.na(var_combinations$score)] <- 2
  
  # 3. if the vehicle can't complete the job in the limit time, the is a big problem, low score
  var_combinations$score[var_combinations$time_to_execute_limit == F &
                           is.na(var_combinations$score)] <- 3
  
  # 4. ####
  # spread PRRS or PED or free vehicle can be contaminated, not clean and can spread to other communities
  var_combinations$score[var_combinations$working_status == "busy" &
                           var_combinations$time_to_execute_limit == T &
                           (var_combinations$prrs == T | var_combinations$ped == T) &
                           var_combinations$clean_event == F &
                           var_combinations$different_community == T &
                           is.na(var_combinations$score)] <- 4
  
  
  # 4. ####
  # spread PRRS or PED or free vehicle can be contaminated, not clean and can spread to other communities
  var_combinations$score[var_combinations$working_status == "busy" &
                           var_combinations$time_to_execute_limit == T &
                           (var_combinations$prrs == T | var_combinations$ped == T) &
                           var_combinations$clean_event == F &
                           var_combinations$different_community == T &
                           is.na(var_combinations$score)] <- 4
  
  # 5. ####
  # spread PRRS or PED or free vehicle can be contaminated, NOT clean and NOT spread to other communities
  var_combinations$score[var_combinations$working_status == "busy" &
                           var_combinations$time_to_execute_limit == T &
                           (var_combinations$prrs == T | var_combinations$ped == T) &
                           var_combinations$clean_event == F &
                           var_combinations$different_community == F &
                           is.na(var_combinations$score)] <- 5
  
  # 6. ####
  # spread PRRS or PED or free vehicle can be contaminated, clean and can spread to other communities
  var_combinations$score[var_combinations$working_status == "busy" &
                           var_combinations$time_to_execute_limit == T &
                           (var_combinations$prrs == T | var_combinations$ped == T) &
                           var_combinations$clean_event == T &
                           var_combinations$different_community == T &
                           is.na(var_combinations$score)] <- 6
  
  # 7. ####
  # spread PRRS or PED or free vehicle can be contaminated, clean and can NOT spread to other communities
  var_combinations$score[var_combinations$working_status == "busy" &
                           var_combinations$time_to_execute_limit == T &
                           (var_combinations$prrs == T | var_combinations$ped == T) &
                           var_combinations$clean_event == T &
                           var_combinations$different_community == F &
                           is.na(var_combinations$score)] <- 7
  
  # 8. ####
  # FREE - spread PRRS or PED or free vehicle can be contaminated, not clean and can spread to other communities
  var_combinations$score[var_combinations$working_status == "free" &
                           var_combinations$time_to_execute_limit == T &
                           (var_combinations$prrs == T | var_combinations$ped == T) &
                           var_combinations$clean_event == F &
                           var_combinations$different_community == T &
                           is.na(var_combinations$score)] <- 8
  
  # 9. ####
  # FREE - spread PRRS or PED or free vehicle can be contaminated, NOT clean and NOT spread to other communities
  var_combinations$score[var_combinations$working_status == "free" &
                           var_combinations$time_to_execute_limit == T &
                           (var_combinations$prrs == T | var_combinations$ped == T) &
                           var_combinations$clean_event == F &
                           var_combinations$different_community == F &
                           is.na(var_combinations$score)] <- 9
  
  # 10. ####
  # BUSY - NOT spread or vehicle contamination of PRRS or PED, NOT clean and can spread to other communities
  var_combinations$score[var_combinations$working_status == "busy" &
                           var_combinations$time_to_execute_limit == T &
                           (var_combinations$prrs == F & var_combinations$ped == F) &
                           var_combinations$clean_event == F &
                           var_combinations$different_community == T &
                           is.na(var_combinations$score)] <- 10
  
  # 11. ####
  # BUSY - NOT spread PRRS or PED, clean and can spread to other communities
  var_combinations$score[var_combinations$working_status == "busy" &
                           var_combinations$time_to_execute_limit == T &
                           (var_combinations$prrs == F & var_combinations$ped == F) &
                           var_combinations$clean_event == T &
                           var_combinations$different_community == T &
                           is.na(var_combinations$score)] <- 11
  # 12. ####
  # BUSY - NOT spread PRRS or PED, NOT clean and can NOT spread to other communities
  var_combinations$score[var_combinations$working_status == "busy" &
                           var_combinations$time_to_execute_limit == T &
                           (var_combinations$prrs == F & var_combinations$ped == F) &
                           var_combinations$clean_event == F &
                           var_combinations$different_community == F &
                           is.na(var_combinations$score)] <- 12
  # 13. ####
  # BUSY - NOT spread PRRS or PED, clean and can NOT spread to other communities
  var_combinations$score[var_combinations$working_status == "busy" &
                           var_combinations$time_to_execute_limit == T &
                           (var_combinations$prrs == F & var_combinations$ped == F) &
                           var_combinations$clean_event == T &
                           var_combinations$different_community == F &
                           is.na(var_combinations$score)] <- 13
  
  
  
  # 14. ####
  # FREE - spread PRRS or PED or free vehicle can be contaminated, clean and can spread to other communities
  var_combinations$score[var_combinations$working_status == "free" &
                           var_combinations$time_to_execute_limit == T &
                           (var_combinations$prrs == T | var_combinations$ped == T) &
                           var_combinations$clean_event == T &
                           var_combinations$different_community == T &
                           is.na(var_combinations$score)] <- 14
  
  # 15. ####
  # FREE - spread PRRS or PED, clean and can NOT spread to other communities
  var_combinations$score[var_combinations$working_status == "free" &
                           var_combinations$time_to_execute_limit == T &
                           (var_combinations$prrs == T | var_combinations$ped == T) &
                           var_combinations$clean_event == T &
                           var_combinations$different_community == F &
                           is.na(var_combinations$score)] <- 15
  
  # 16. ####
  # FREE - NOT spread PRRS or PED, NOT clean and can spread to other communities
  var_combinations$score[var_combinations$working_status == "free" &
                           var_combinations$time_to_execute_limit == T &
                           (var_combinations$prrs == F & var_combinations$ped == F) &
                           var_combinations$clean_event == F &
                           var_combinations$different_community == T &
                           is.na(var_combinations$score)] <- 16
  
  # 17. ####
  # FREE - NOT spread PRRS or PED, clean and can spread to other communities
  var_combinations$score[var_combinations$working_status == "free" &
                           var_combinations$time_to_execute_limit == T &
                           (var_combinations$prrs == F & var_combinations$ped == F) &
                           var_combinations$clean_event == T &
                           var_combinations$different_community == T &
                           is.na(var_combinations$score)] <- 17
  # 18. ####
  # FREE - NOT spread PRRS or PED, NOT clean and can NOT spread to other communities
  var_combinations$score[var_combinations$working_status == "free" &
                           var_combinations$time_to_execute_limit == T &
                           (var_combinations$prrs == F & var_combinations$ped == F) &
                           var_combinations$clean_event == F &
                           var_combinations$different_community == F &
                           is.na(var_combinations$score)] <- 18
  # 19. ####
  # FREE - NOT spread PRRS or PED, clean and can NOT spread to other communities
  var_combinations$score[var_combinations$working_status == "free" &
                           var_combinations$time_to_execute_limit == T &
                           (var_combinations$prrs == F & var_combinations$ped == F) &
                           var_combinations$clean_event == T &
                           var_combinations$different_community == F &
                           is.na(var_combinations$score)] <- 19
  
  # include combination with NAs
  inactive_row <- data.frame(working_status = "inactive",
                             asf = NA,
                             prrs = NA,
                             ped = NA,
                             clean_event = NA,
                             different_community = NA,
                             time_to_execute_limit = NA,
                             score = 1
                             )
  
  
  var_combinations <- rbind(var_combinations,
                            inactive_row)
  

  # write template ranking ####
  if(export == T){
    write.csv(var_combinations, "var_combinations.csv", row.names = F)
  }
  
  return(var_combinations)
}
