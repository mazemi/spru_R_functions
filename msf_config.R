#' -----------------------------------------------------------------------------
#' @title: MFS configuration
#' -----------------------------------------------------------------------------
#' @description:
#' This file contain multiple lists of MFS weights and its dimensions scores
#' @section: MFS dimensions weights
#' @section: MFS availability scores
#' @section: MFS accessibility scores
#' @section: MFS resilience scores
#' @section: MFS affordability scores
#' @section: MFS infrastructure scores
#' -----------------------------------------------------------------------------

# MFS dimensions weights
MFS.WEIGHT <- list(
  availability = 30,
  accessibility = 25,
  resilience = 20,
  affordability = 15,
  infrastructure = 10
)

# availability scores
AVALABILITY.CONFIG <- list(
  wheat_widely_available = 4,
  wheat_limited_available = 2,
  
  rice_widely_available = 4,
  rice_limited_available = 2,
  
  pulses_lentils_widely_available = 2,
  pulses_lentils_limited_available = 1,
  
  pulses_beans_widely_available = 2,
  pulses_beans_limited_available = 1,
  
  pulses_split_peas_widely_available = 2,
  pulses_split_peas_limited_available = 1,
  
  tomatoes_widely_available = 4,
  tomatoes_limited_available = 2,
  
  vegetable_oil_widely_available = 4,
  vegetable_oil_limited_available = 2,
  
  sugar_widely_available = 1.5,
  sugar_limited_available = 1,
  
  salt_widely_available = 1.5,
  salt_limited_available = 1,
  
  soap_widely_available = 3,
  soap_limited_available = 2,
  
  safe_water_widely_available = 3,
  safe_water_limited_available = 2,
  
  toothbrush_adult_widely_available = 1,
  toothbrush_adult_limited_available = .5,
  
  toothpaste_widely_available = 1,
  toothpaste_limited_available = .5,
  
  sanitary_pad_widely_available = 1,
  sanitary_pad_limited_available = .5,
  
  pen_widely_available = 1, 
  pen_limited_available = .5,
  
  rubber_widely_available = 1, 
  rubber_limited_available = .5,
  
  notebook_widely_available = 1, 
  notebook_limited_available = .5, 
  
  diesel_widely_available = 2,
  diesel_limited_available = 1,
  
  petrol_widely_available = 2,
  petrol_limited_available = 1,
  
  lpg_widely_available = 3, 
  lpg_limited_available = 2, 
  
  firewood_widely_available = 2,
  firewood_limited_available = 1,
  
  coal_widely_available = 2,
  coal_limited_available = 1,
  
  blanket_widely_available = 3,
  blanket_limited_available = 2,
  
  winter_jacket_widely_available = 3,
  winter_jacket_limited_available = 2
)

# accessibility scores
ACCESSIBILITY.CONFIG <- list(
  physical_access = 5,
  damage_facility = 3,
  safty = 6,
  social_access = 3,
  women_access = 4
)

# resilience scores
RESILIENCE.CONFIG <- list(
  spply_chain = 12,
  food_supplier = 3,
  nfi_supplier = 3, 
  Restocking = 30, 
  Restocking_winterization = 6
)

# affordability scores
AFFORDABILITY.CONFIG <- list(
  price_point = 40,
  price_point_winterization = 8,
  financial_access = 9,
  indebtedness = 5,
  food_basket = 5
)

# infrastructure scores
INFRASTRUCTURE.CONFIG <- list(
  damage_facility = 5,
  storage = 5,
  financial_services = 5,
  payment_modalities = 3
)


lockBinding("AVALABILITY.CONFIG", globalenv())
lockBinding("ACCESSIBILITY.CONFIG", globalenv())
lockBinding("RESILIENCE.CONFIG", globalenv())
lockBinding("AFFORDABILITY.CONFIG", globalenv())
lockBinding("INFRASTRUCTURE.CONFIG", globalenv())
