round_any <- function(x, accuracy, f = round) {
  # Oude naamgeving:  RoundAny
  # Rond een getal af op een in te geven cijfer.
  # x is het getal, accuracy de cijfer waarnaar het dient af te ronden
  # f de formule voor ceiling (dichtbijzijnde naar boven afgerond) of
  # floor (dichtsbijzijnde naar onder afgerond).
  return(f(x / accuracy) * accuracy)
}

null_variable <- function(variable) {
  # Old function name = "NullVariable"
  # Make NA variable NULL
  if (is.na(variable)) {
    variable <- NULL
  }
  return(variable)
}

create_path <- function(path, subfolder) {
  # old name = CreatePath
  new_path <- stringr::str_c(path, subfolder, sep = "/")
  if (!file.exists(new_path)) {
    dir.create(new_path)
  }
  return(new_path)
}
