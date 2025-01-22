# Vehicle rerouting system
Codes for calculating the vehicles' scores and rankings for rerouting purposes.

# score_template.R
The score_template function generates a scoring system for vehicles used in livestock transport, accounting for various risk factors associated with disease transmission. It creates a comprehensive ranking of vehicles based on their status, interactions with infected farms, and network measures, which can be exported for further analysis.

- [score_template Function](https://github.com/machado-lab/vehicle_rerouting_system/blob/main/code/score_template.R)

## Function Description
### Parameters:
- prrs_limit: Time frame (in days) to check the PRRSV status of farms (e.g., 364 days).
- prrs_period: Time frame (in days) to check vehicle visits to PRRSV-infected farms (e.g., 30 days).
- ped_limit: Time frame (in days) to check the PEDV status of farms (e.g., 364 days).
- ped_period: Time frame (in days) to check vehicle visits to PEDV-infected farms (e.g., 30 days).
- asf_limit: Time frame (in days) to check the ASFV status of farms (e.g., 364 days).
- asf_period: Time frame (in days) to check vehicle visits to ASFV-infected farms (e.g., 30 days).
- network_period_time: Time frame for analyzing network interactions.
- max_wait_execute: Maximum allowable delay for shipments.
- export: Boolean indicating whether to export the scoring template as a CSV file. Default is FALSE.

### Scoring System
The scoring system evaluates vehicles based on the following factors:
- Vehicle Status: free, busy, maintenance, or inactive.
- Disease Risks: Whether vehicles have visited farms with PRRSV, PEDV, or ASFV infection.
- Cleaning Events: Whether vehicles were cleaned and disinfected between trips.
- Community Contacts: Whether vehicles moved between different farm communities.
- Time to Execute: Whether a vehicle can fulfill a shipment within the required time limit.

### Scoring Logic:
- Scores range from 1 (lowest priority) to 19 (highest priority).
- Vehicles with higher biosecurity compliance (e.g., cleaned, no contact with infected farms) and those able to meet shipment deadlines receive higher scores.
- Inactive vehicles are automatically assigned the lowest score of 1.

### Outputs
- A data frame containing all possible combinations of the above variables, with corresponding scores.
- If export = TRUE, a CSV file (var_combinations.csv) containing the data frame is saved in the working directory. [Example]([https://github.com/machado-lab/vehicle_rerouting_system/blob/main/code/score_template.R](https://github.com/machado-lab/vehicle_rerouting_system/blob/main/data/Vehicles%E2%80%99%20rules%20and%20scores.csv))

# Vehicle_score.R
The vehicle_score function assigns scores to observed vehicles based on a predefined scoring template. It combines the observed vehicle data with the scoring criteria, generating a scored table that reflects vehicle suitability for shipments while considering various risk factors.

[vehicle_score Function](https://github.com/machado-lab/vehicle_rerouting_system/blob/main/code/vehicle_score.R)

## Function Description
### Parameters:
- score_template_table: A data frame generated by the score_template function containing the predefined scoring criteria for vehicle combinations.
- observed_vehicle_table: A data frame containing observed vehicle data, including columns matching the factors in the scoring template (e.g., working_status, asf, prrs, etc.).

### Outputs
scored_vehicle_table: A data frame of the observed vehicle data with an additional column for scores, which reflects their suitability based on the scoring template.

# vehicle_rank.R
The vehicle_rank function assigns ranks to vehicles based on their scores and their time to the request farm, prioritizing vehicles for shipment scheduling. The ranking is determined by descending score and descending proximity (rounded time to the request farm). Vehicles with identical scores and times are assigned the same rank.

- [vehicle_rank Function](https://github.com/machado-lab/vehicle_rerouting_system/blob/main/code/vehicle_rank)

## Function Description
### Parameters:
- scored_vehicle: A data frame containing scored vehicle data, typically generated by the vehicle_score function. The input must include the following columns:
  - score: Vehicle score indicating priority.
  - time_to_request_farm: The rounded time for the vehicle to reach the request farm.

### Outputs
- The input data frame is updated with an additional column, rank, which assigns a priority rank to each vehicle based on the scoring and time criteria.

