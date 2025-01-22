# Vehicle rerouting system
Codes for calculating the vehicles' scores and rankings for rerouting purposes.

# score_template.R
The score_template function generates a scoring system for vehicles used in livestock transport, accounting for various risk factors associated with disease transmission. It creates a comprehensive ranking of vehicles based on their status, interactions with infected farms, and network measures, which can be exported for further analysis.

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
- If export = TRUE, a CSV file (var_combinations.csv) containing the data frame is saved in the working directory.
