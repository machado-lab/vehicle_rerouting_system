# Vehicle rerouting system
Codes for calculating the vehicles' scores and rankings for rerouting purposes.

# score_template.R
The score_template function generates a scoring system for vehicles used in livestock transport, accounting for various risk factors associated with disease transmission. It creates a comprehensive ranking of vehicles based on their status, interactions with infected farms, and network measures, which can be exported for further analysis.

## Function Description
### Parameters:
prrs_limit: Time frame (in days) to check the PRRSV status of farms (e.g., 364 days).
prrs_period: Time frame (in days) to check vehicle visits to PRRSV-infected farms (e.g., 30 days).
ped_limit: Similar to prrs_limit, but for PEDV.
ped_period: Similar to prrs_period, but for PEDV.
asf_limit: Time frame to check ASFV status of farms.
asf_period: Time frame for vehicle visits to ASFV-infected farms.
network_period_time: Time frame for analyzing network interactions.
max_wait_execute: Maximum allowable delay for shipments.
export: Boolean indicating whether to export the scoring template as a CSV file. Default is FALSE.
