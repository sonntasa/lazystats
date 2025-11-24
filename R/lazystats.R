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

  if (any((DFn %% 1 != 0) | (DFd %% 1 != 0))) {
    message("Sphericity Correction has been applied, returning DF to 2 decimals")
    DFn <- apaFormat(aov_object$anova_table[effect, ]$"num Df", Dec = 2, OneMax = FALSE)
    DFd <- apaFormat(aov_object$anova_table[effect, ]$"den Df", Dec = 2, OneMax = FALSE)
  }
  F <- apaFormat(aov_object$anova_table[effect, ]$"F", Dec = 2, OneMax = FALSE)

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
  return(str_c("\\emph{p}", " ", p))
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
  if ("ges" %in% names(aov_object$anova_table[effect, ])) {
    ges <- apaFormat(aov_object$anova_table[effect, ]$"ges", Dec = 2, OneMax = FALSE)
    return(str_c("$\\eta_{G}^2$ = ", ges))
  } else if ("pes" %in% names(aov_object$anova_table[effect, ])) {
    pes <- apaFormat(aov_object$anova_table[effect, ]$"pes", Dec = 2, OneMax = FALSE)
    return(str_c("$\\eta_{p}^2$ = ", pes))
  }
}

#' @title
#' epsString
#'
#' @description This function takes in a afex ANOVA and returns a formatted
#' epsilon for use in the aovString function.
#'
#' @import stringr
#'
#' @param aov_object List
#' @param effect string
#' @return a formated value string
#' @export
#' @examples
epsString <- function(aov_object, effect) {
  DFn <- aov_object$anova_table[effect, ]$"num Df"
  DFd <- aov_object$anova_table[effect, ]$"den Df"

  if (any((DFn %% 1 != 0) | (DFd %% 1 != 0))) {
    return(
      str_c(
        ", $\\epsilon$ = ",
        apaFormat(
          summary(aov_object)$pval.adjustments[effect, ][[1]],
          OneMax = FALSE,
          Dec = 2,
          p = FALSE
        )
      )
    )
  }
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
      ifelse(return_name, str_c("Effect: '", effect, "'\n"), ""),
      fString(aov_object, effect), ", ",
      pString(aov_object, effect), ", ",
      effString(aov_object, effect),
      epsString(aov_object, effect)
    )
  )
}


#' @title converT
#'
#' @description This function takes in a t-test from an emmeans comparisons and
#' returns a formatted LaTeX F-string fit for a paper.
#'
#' @import glue
#'
#' @param t_object List
#' @param effect_num integer
#' @param return_name boolean
#' @return a formated value string
#' @export
#' @examples
converT <- function(t_object, effect_num) {
  t_object <- summary(t_object)[effect_num, ]
  t_val <- t_object$t.ratio
  F <- apaFormat(t_val * t_val, OneMax = FALSE, Dec = 2, p = FALSE)
  p <- apaFormat(t_object$p.value, p = TRUE)
  df <- t_object$df
  eff1 <- t_object[[1]]
  eff2 <- t_object[[2]]
  pes <- apaFormat(t_val^2 / (t_val^2 + df), OneMax = FALSE, Dec = 2)

  return(
    str_c(
      "Effect: '", eff1, ", ", eff2, "'\n",
      "\\emph{F}", "(", 1, ", ", df, ") = ", F, ", ",
      "\\emph{p}", " ", p, ", ",
      "$\\eta_{p}^2$ = ", pes
    )
  )
}
