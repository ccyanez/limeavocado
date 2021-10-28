# calculates linear fits between measured and standard values for each gas (CO2, CO, and CH4)
# future: distinguish whether want a one-point or two-point calibration based on the values that the function is given

calibrate <- function(measured, standards) {
  cal <- merge(standards, measured, by = "STANDARD_ID", suffixes = c(".known",".measured")) %>% # merge known and measured values
    relocate(TYPE, .before = CO2.known) %>% 
    pivot_longer(CO2.known:CH4.measured) %>%  # convert to long format
    separate(name, into=c("species","type")) %>% # get values from column names
    pivot_wider(names_from = type, values_from=value) %>% # go back to wide format
    na.omit() %>% # remove NA rows (will lead to one-point calibration)
    nest(data = -species) %>%  # nest all values into groups by species
    mutate(reg = map(data, ~lm(known ~ measured, .)), points = map(data, ~nrow(.))) %>% 
    mutate(intercept = case_when(
      points == 2 ~ map_dbl(reg, ~coefficients(.)[1]), # get values from the regression
      points == 1 ~ 0),
      slope = case_when(
        points == 2 ~ map(reg, ~ coefficients(.)[2][[1]]),
        points == 1 ~ map(data, ~ .x$known / .x$measured))) %>%
    column_to_rownames(var = "species")
  
  # convert columns to numeric 
  cal$points <- as.numeric(cal$points)
  cal$slope <- as.numeric(cal$slope)
  
  return(cal)
}
