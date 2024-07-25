#' Calculate Biomass Based on Inputs
#'
#' This function calculates biomass based on the country, species, compartment, diameter (D), and height (H).
#' @param country Name of the country as character string.
#' @param species Species name as character string.
#' @param compartment Compartment type as character string.
#' @param D Diameter value as numeric.
#' @param H Height value as numeric.
#' @return Biomass as numeric value or detailed message on computation results.
#' @export
calculate_biomass <- function(country, species, compartment, D, H) {
  library(dplyr)
  library(openxlsx)
  options(warn = -1)

  frosster_data_path <- system.file("extdata", "frosster_data.xlsx", package = "BioCal")
  species_eq_path <- system.file("extdata", "species_eq_zianis.txt", package = "BioCal")
  species_path <- system.file("extdata", "species_zianis.txt", package = "BioCal")

  frosster_d <- read.xlsx(frosster_data_path)


  # Function to standardize scientific notation
  standardize_scientific_notation <- function(text) {
    # Correct non-standard exponent characters and formats
    text <- gsub("([0-9])\\.([0-9]+)\\.10([–-])([0-9]+)", "\\1.\\2e\\3\\4", text, perl = TRUE)
    text <- gsub("–", "-", text)  # Correct non-standard negative signs if any left

    return(text)
  }

  # Read and preprocess the file
  file_content <- readLines(species_eq_path)
  file_content <- sapply(file_content, standardize_scientific_notation)

  df <- read.table(text = file_content, header = FALSE, fill = TRUE, sep = "", quote = "\"",
                   dec = ".", comment.char = "")
  df <- df[-1,]
  df <- df[,-1]
  colnames(df) <- c("Eq", "a", "b", "c", "d", "e")

  create_function <- function(eq, a, b, c, d, e) {
    a <- ifelse(is.na(a) || !is.numeric(as.numeric(a)), NA, as.numeric(a))
    b <- ifelse(is.na(b) || !is.numeric(as.numeric(b)), NA, as.numeric(b))
    c <- ifelse(is.na(c) || !is.numeric(as.numeric(c)), NA, as.numeric(c))
    d <- ifelse(is.na(d) || !is.numeric(as.numeric(d)), NA, as.numeric(d))
    e <- ifelse(is.na(e) || !is.numeric(as.numeric(e)), NA, as.numeric(e))

    constants <- c(a = a, b = b, c = c, d = d, e = e)
    for (const in names(constants)) {
      value <- constants[[const]]
      if (!is.na(value)) {
        eq <- gsub(paste0("\\b", const, "\\b"), value, eq)
      }
    }

    eq <- gsub("exp\\(1\\)-([0-9]+)", "*10^-\\1", eq)
    eq <- gsub("–", "-", eq)
    eq <- gsub("—", "-", eq)
    eq <- gsub("D2", "D^2", eq)
    eq <- gsub("D3", "D^3", eq)
    eq <- gsub("·", "*", eq)
    eq <- gsub("²", "^2", eq)
    eq <- gsub("³", "^3", eq)
    eq <- gsub("Db", "D*b", eq)
    eq <- gsub("Dc", "D*c", eq)
    eq <- gsub("Hd", "H*d", eq)
    eq <- gsub("Hc", "H*c", eq)
    eq <- gsub("Hb", "H*b", eq)
    eq <- gsub("π", "pi", eq)
    eq <- gsub("log\\(", "log(", eq)
    eq <- gsub("exp\\(", "exp(", eq)
    eq <- gsub("ln\\(", "log(", eq)
    eq <- gsub("\\[", "(", eq)
    eq <- gsub("\\]", ")", eq)
    eq <- gsub("(\\))([0-9.]+)", "\\1*\\2", eq)
    eq <- gsub("(\\d)\\(", "\\1*(", eq)
    eq <- gsub("\\)([a-zA-Z])", ")*\\1", eq)
    eq <- gsub("(\\))\\(", ")*(", eq)
    open_brackets = gregexpr("\\(", eq)[[1]]
    close_brackets = gregexpr("\\)", eq)[[1]]
    if (length(open_brackets) > length(close_brackets)) {
      eq = paste0(eq, strrep(")", length(open_brackets) - length(close_brackets)))
    }
    eq = gsub("(\\d)([a-zA-Z])", "\\1*\\2", eq)
    eq = gsub("([a-zA-Z])(\\d)", "\\1*\\2", eq)
    eq <- gsub("\\be\\b", "exp(1)", eq)
    eq <- gsub("exp\\(1\\)-([0-9]+)", "10^-\\1", eq)
    eq <- gsub("ln\\(", "log(", eq)
    func_text = paste("function(D, H) {", eq, "}")
    func = eval(parse(text = func_text))
    return(func)
  }
  equation_functions = mapply(create_function, df$Eq, df$a, df$b, df$c, df$d, df$e, SIMPLIFY = FALSE)

  file_path <- species_path
  data_lines <- readLines(file_path)
  parsed_data <- list()
  current_species <- NULL
  current_country <- NULL
  current_component <- NULL

  for (line in data_lines) {
    if (grepl("^[A-Z][a-z]+\\s+[a-z]+", line)) {
      current_species <- gsub("\\s*\\([^\\)]+\\)", "", line)
    } else {
      parts <- strsplit(line, "\\s+")[[1]]
      current_country <- parts[2]
      current_component <- parts[3]
      equation_number <- parts[1]

      parsed_data[[length(parsed_data) + 1]] <- list(
        Species = current_species,
        Country = current_country,
        Compartment = current_component,
        EquationNumber = equation_number
      )
    }
  }

  data_df <- do.call(rbind, lapply(parsed_data, as.data.frame, stringsAsFactors = FALSE))
  colnames(data_df) <- c("Species", "Country", "Compartment", "EquationNumber")

  # Find the corresponding equation number from the parsed data
  selected_row <- subset(data_df, Species == species & Country == country & Compartment == compartment)
   if (nrow(selected_row) > 0) {
    selected_equation_number <- as.integer(selected_row$EquationNumber)[1]+1
    if (!is.na(selected_equation_number) && selected_equation_number > 0 && selected_equation_number <= length(equation_functions)) {
      selected_function <- equation_functions[[selected_equation_number]]
      # Execute the function
      return(selected_function(D = D, H = H))
      print(sprintf("Result for %s in %s (%s): %f", selected_species, selected_country, selected_compartment, result))
    } else {
      return(print("No valid equation found for the selected criteria."))
    }
  } else {
    print("No valid equation found for the selected criteria.")

    beta0_s <- as.numeric(substr(filter(frosster_d, frosster_d$Species==species,frosster_d$Equation==3,frosster_d$Component=="Stem mass")$`ln(β0)`, 1, 4))
    beta0_r <- as.numeric(substr(filter(frosster_d, frosster_d$Species==species,frosster_d$Equation==3,frosster_d$Component=="Root mass")$`ln(β0)`, 1, 4))
    beta0_f <- as.numeric(substr(filter(frosster_d, frosster_d$Species==species,frosster_d$Equation==3,frosster_d$Component=="Foliage mass")$`ln(β0)`, 1, 4))
    beta0_d <- as.numeric(substr(filter(frosster_d, frosster_d$Species==species,frosster_d$Equation==3,frosster_d$Component=="Dead branch")$`ln(β0)`, 1, 4))
    if(length(beta0_s)<1){beta0_s <- as.numeric(substr(filter(frosster_d, frosster_d$Species=="All species combined",frosster_d$Equation==3,frosster_d$Component=="Stem mass")$`ln(β0)`, 1, 4))}
    if(length(beta0_r)<1){beta0_r <- as.numeric(substr(filter(frosster_d, frosster_d$Species=="All species combined",frosster_d$Equation==3,frosster_d$Component=="Root mass")$`ln(β0)`, 1, 4))}
    if(length(beta0_f)<1){beta0_f <- as.numeric(substr(filter(frosster_d, frosster_d$Species=="All species combined",frosster_d$Equation==3,frosster_d$Component=="Foliage mass")$`ln(β0)`, 1, 4))}
    if(length(beta0_d)<1){beta0_d <- as.numeric(substr(filter(frosster_d, frosster_d$Species=="All species combined",frosster_d$Equation==3,frosster_d$Component=="Dead branch")$`ln(β0)`, 1, 4))}

    beta1_s <- as.numeric(substr(filter(frosster_d, frosster_d$Species==species,frosster_d$Equation==3,frosster_d$Component=="Stem mass")$`β for ln(d)`, 1, 4))
    beta1_r <- as.numeric(substr(filter(frosster_d, frosster_d$Species==species,frosster_d$Equation==3,frosster_d$Component=="Root mass")$`β for ln(d)`, 1, 4))
    beta1_f <- as.numeric(substr(filter(frosster_d, frosster_d$Species==species,frosster_d$Equation==3,frosster_d$Component=="Foliage mass")$`β for ln(d)`, 1, 4))
    beta1_d <- as.numeric(substr(filter(frosster_d, frosster_d$Species==species,frosster_d$Equation==3,frosster_d$Component=="Dead branch")$`β for ln(d)`, 1, 4))
    if(length(beta1_s)<1){beta1_s <- as.numeric(substr(filter(frosster_d, frosster_d$Species=="All species combined",frosster_d$Equation==3,frosster_d$Component=="Stem mass")$`β for ln(d)`, 1, 4))}
    if(length(beta1_r)<1){beta1_r <- as.numeric(substr(filter(frosster_d, frosster_d$Species=="All species combined",frosster_d$Equation==3,frosster_d$Component=="Root mass")$`β for ln(d)`, 1, 4))}
    if(length(beta1_f)<1){beta1_f <- as.numeric(substr(filter(frosster_d, frosster_d$Species=="All species combined",frosster_d$Equation==3,frosster_d$Component=="Foliage mass")$`β for ln(d)`, 1, 4))}
    if(length(beta1_d)<1){beta1_d <- as.numeric(substr(filter(frosster_d, frosster_d$Species=="All species combined",frosster_d$Equation==3,frosster_d$Component=="Dead branch")$`β for ln(d)`, 1, 4))}


    bio_s = exp(beta0_s+beta1_s*log(mean(D))+0.087*log(mean(H)))
    root_s  = exp(beta0_r+beta1_r*log(mean(D))+0.087*log(mean(H)))
    foliage_s = exp(beta0_f+beta1_f*log(mean(D))+0.087*log(mean(H)))

    return(list(
      "Using Forrester et al. 2017 General Eq" = "Values based on Forrester et al. 2017",
      "Stem Biomass" = bio_s,
      "Root Biomass" = root_s,
      "Foliage Biomass" = foliage_s
    ))
  }
}
