vechile_score <- function(score_template_table,
                          observed_vehicle_table,
                          ){
  
  scored_vehicle_table <- observed_vehicle_table %>%
    left_join(score_template_table,
              by = c("working_status",
                     "asf",
                     "prrs",
                     "ped",
                     "clean_event",
                     "different_community",
                     "time_to_execute_limit")
    )
  
  return(scored_vehicle_table)
}
