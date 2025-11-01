#' @title
#' apaFormat
#'
#'
#' @description This function reformats a value to fit the apa style.
#' This means: removing leading zeros and rounding to 2 decimal places as a
#' default. If a p-value is inserted, a formatted p-vaue is returned.
#'
#' @import dplyr
#'
#' @param val float
#' @param OneMax boolean
#' @param Dec integer
#' @param p boolean
#' @return a formated value string
#' @export
#' @examples someValue <- apaFormat(someValue)
apaFormat <- function(val, OneMax = TRUE, Dec = 3, p = FALSE) {
  if (p && !OneMax) {
    warning("Don't drink and work Samuel ...")
  }
  if (p && Dec != 3) {
    warning("p-values are always formatted to 3 Decimals in accordance with APA
rules. If you still want to format to more or less decimals, set p = FALSE
and manually set the number of Decimals.")
  }
  if (p) {
    val <- as.numeric(val)
    if (val > 0.001) {
      return(paste0("= ", apaFormat(val, OneMax = TRUE, Dec = 3)))
    } else {
      return("< .001")
    }
  } else {
    if (OneMax) {
      return(sub("^0", "", sprintf(paste0("%.", Dec, "f"), val)))
    } else {
      return(sprintf(paste0("%.", Dec, "f"), val))
    }
  }
}

#' @title
#' fString
#'
#' @description This function takes in a afex ANOVA and returns a formatted
#' F string [F(DFn, DFd) = f-value] for use in the aovString function
#'
#' @import stringr
#'
#' @param aov_object List
#' @param effect string
#' @return a formated value string
#' @export
#' @examples
fString <- function(aov_object, effect) {
  DFn <- aov_object$anova_table[effect, ]$"num Df"
  DFd <- aov_object$anova_table[effect, ]$"den Df"

  if (DFn %% 1 != 0 | DFd %% 1 != 0) {
    message("Sphericity Correction has been applied, returning DF to 2 decimals")
    DFn <- apaFormat(aov_object$anova_table[effect, ]$"num Df", Dec = 2, OneMax = FALSE)
    DFd <- apaFormat(aov_object$anova_table[effect, ]$"den Df", 2, Dec = 2, OneMax = FALSE)
  }
  F <- round(aov_object$anova_table[effect, ]$"F", 2)

  return(str_c("\\emph{F}", "(", DFn, ", ", DFd, ") = ", F))
}

#' @title
#' pString
#'
#' @description This function takes in a afex ANOVA and returns a formatted
#' p string (p = p-value) for use in the aovString function
#'
#' @import stringr
#'
#' @param aov_object List
#' @param effect string
#' @return a formated value string
#' @export
#' @examples
pString <- function(aov_object, effect) {
  p <- apaFormat(aov_object$anova_table[effect, ]$"Pr(>F)", p = TRUE)
  return(p)
}

#' @title
#' effString
#'
#' @description This function takes in a afex ANOVA and returns a formatted
#' effect string (ges) for use in the aovString function.
#'
#' @import stringr
#'
#' @param aov_object List
#' @param effect string
#' @return a formated value string
#' @export
#' @examples
effString <- function(aov_object, effect) {
  ges <- apaFormat(aov_object$anova_table[effect, ]$"ges", Dec = 3, OneMax = FALSE)

  return(str_c("$\\eta_{G}^2$ = ", ges))
}

#' @title
#' aovString
#'
#' @description This function takes in a afex ANOVA and returns a formatted
#' LaTeX string fit for a paper.
#'
#' @import glue
#'
#' @param aov_object List
#' @param effect string
#' @param return_name boolean
#' @return a formated value string
#' @export
#' @examples
aovString <- function(aov_object, effect, return_name = TRUE) {
  return(
    str_c(
      ifelse(return_name, str_c(effect, "\n"), ""),
      fString(aov_object, effect),
      pString(aov_object, effect),
      effString(aov_object, effect)
    )
  )
}
