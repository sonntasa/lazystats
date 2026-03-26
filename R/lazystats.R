#' @title apaFormat
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

#' @title fString
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

#' @title pString
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

#' @title effString
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

#' @title aovString
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


#' @title reportT
#'

#' @description This function takes in a t-test from an emmeans comparisons
#' and returns a formatted LaTeX t-/F-string fit for a paper. It determines
#' whether the inserted object is a difference, or a difference of differences
#' and returns the appopriate statistic.
#'
#' @import glue
#'
#' @param t_object List
#' @param effect_num integer
#' @param return_name boolean
#' @return a formated value string
#' @export
#' @examples
reportT <- function(t_object, effect_num) {
  t_object <- summary(t_object)[effect_num, ]
  t_val <- t_object$t.ratio
  df <- t_object$df
  p <- apaFormat(
    val = t_object$p.value,
    p   = TRUE
  )
  eff1 <- t_object[[1]]
  eff2 <- t_object[[2]]

  if (is.character(eff2)) {
    F <- apaFormat(t_val * t_val, OneMax = FALSE, Dec = 2, p = FALSE)
    pes <- apaFormat(t_val^2 / (t_val^2 + df), OneMax = FALSE, Dec = 2)

    return(
      str_c(
        "Effect: '", eff1, ", ", eff2, "'\n",
        "\\emph{F}", "(", 1, ", ", df, ") = ", F, ", ",
        "\\emph{p}", " ", p, ", ",
        "$\\eta_{p}^2$ = ", pes
      )
    )
  } else {
    d_val <- apaFormat(
      val    = t_val / sqrt(df + 1),
      OneMax = FALSE,
      Dec    = 2
    )

    t_val <- apaFormat(
      val    = t_val,
      OneMax = FALSE,
      Dec    = 2
    )

    return(
      str_c(
        "Effect: ", eff1, ", mean difference: ", round(eff2, 2), "\n",
        glue(
          "\\emph{{t}}({df}) = {t_val}, \\emph{{p}} {p}, \\textit{{d}}\\textsubscript{{z}} = {d_val}"
        )
      )
    )
  }
}

#' @title lazydesc
#'
#' @description This function handles the formatting of any kind of descriptive
#' strings.
#'
#' @import glue
#'
#' @param lazy_object any lazystats object
#' @param effect_num integer
#' @param effect_name string
#' @return a formated value string
#' @export
#' @examples
lazydesc <- function(desc_object, effect_num = NULL, rt = TRUE) {
  if (inherits(desc_object, "emmGrid")) {
    ## making sure effect_num isn't null
    effect_num <- if (is.null(effect_num)) 1 else effect_num
    ## Extract Name
    ex_name <- str_c("Effect: '", test(desc_object)[[1]][effect_num], "'")
    ## Extract SEM
    ex_sem <- test(desc_object)$SE[effect_num]
    ## Extract M
    # ex_m <- if ("estimate" %in% names(test(desc_object))) test(desc_object)$estimate[effect_num] else test(desc_object)$emmean[effect_num]
    ex_m <-
      ifelse(
        test = "estimate" %in% names(test(desc_object)),
        yes = test(desc_object)$estimate[effect_num],
        no = test(desc_object)$emmean[effect_num]
      )
  } else {
    ex_name <- "Wahtever you had put in ^^"
    ex_sem <- desc_object$sem
    ex_m <- desc_object$mean
  }

  if (rt) {
    unit <- " ms"
    digits <- 0
  } else {
    unit <- " \\%"
    digits <- 2
  }

  return(
    str_c(
      ex_name,
      "\n",
      "\\textit{M} = ",
      apaFormat(ex_m, FALSE, digits),
      unit,
      ", ",
      "\\textit{SEM} = ",
      apaFormat(ex_sem, FALSE, digits)
    )
  )
}

#' @title lazyformat
#'
#' @description This functions serves as the wrapper for all other objects.
#' It takes in any kind of object in the realm of lazystats and returns and apa
#' (7) formatted string.
#'
#' @import glue
#'
#' @param lazy_object any lazystats object
#' @param effect_num integer
#' @param effect_name string
#' @return a formated value string
#' @export
#' @examples
lazyformat <- function(
  lazy_object,
  effect_num = 1,
  effect_name = NULL,
  convert = FALSE
) {
  if (is.null(names(lazy_object))) {
    message("Assuming an emmeans object, converting with 'test()'")
    lazy_object <- test(lazy_object)
  }
  if ("t.ratio" %in% names(lazy_object)) {
    return(
      reportT(
        t_object   = lazy_object,
        effect_num = effect_num
      )
    )
  } else if ("anova_table" %in% names(lazy_object)) {
    return(
      aovString(
        aov_object  = lazy_object,
        effect      = effect_name,
        return_name = TRUE
      )
    )
  }
}

#' @title lazydemographics
#'
#' @description This function takes in one of my vpInfo objects and returns a
#' fully formatted participants section---or at least the part where
#' demographic information are reported.
#' It takes in a dataset, the analyzed variables are id, age (numeric),
#' gender (women, men, and non-binary individuals) and handedness (left vs. right).
#'
#' @import glue
#'
#' @param dat dataset/tibble
#' @return tibble
#' @export
#' @examples
vpInfo <- function(dat) {
  dat |>
    distinct(id, .keep_all = TRUE) |>
    mutate(age = as.numeric(age)) |>
    summarize(
      N       = n(),
      meanAge = round(mean(age), 2),
      sdAge   = round(sd(age), 2),
      minAge  = range(age)[1],
      maxAge  = range(age)[2],
      nFemale = sum(gender == "female"),
      nMale   = sum(gender == "male"),
      nNa     = sum(!(gender %in% c("male", "female"))),
      nRight  = sum(handedness == "right")
    )
}

#' @title lazydemographics
#'
#' @description This function takes in one of my vpInfo objects and returns a
#' fully formatted participants section---or at least the part where
#' demographic information are reported.
#'
#' @import glue
#'
#' @param lazy_object any lazystats object
#' @param effect_num integer
#' @param effect_name string
#' @return a formated value string
#' @export
#' @examples
lazydemographics <- function(sample_orig = NULL, sample_final = NULL, full_text = TRUE) {
  if (is.null(sample_orig) || is.null(sample_final)) {
    sample <- if (is.null(sample_orig)) sample_final else sample_orig
    return(
      glue("{sample$N} people, \\textit{{M}} = {round(sample$meanAge, 2)}, \\textit{{SD}} = {round(sample$sdAge, 2)}, {sample$nFemale} women, {sample$nMale} men, and {sample$nNa} non-binary individuals, {sample$nRight} right.")
    )
  } else if (full_text) {
    return(
      glue("In sum, {sample_orig$N} people took part in this experiment (\\textit{{M}} = {round(sample_orig$meanAge, 2)} years, \\textit{{SD}} = {round(sample_orig$sdAge, 2)}; {sample_orig$nFemale} women, {sample_orig$nMale} men, and {sample_orig$nNa} non-binary individuals). Among those, {sample_orig$nRight} participants stated that they were right-handed. After all exclusions, the remaining sample consisted of {sample_final$N} people (\\textit{{M}} = {round(sample_final$meanAge, 2)} years, \\textit{{SD}} = {round(sample_final$sdAge, 2)}).")
    )
  } else {
    return(
      str_c(
        "Original Sample", glue("{sample_orig$N} people, \\textit{{M}} = {round(sample_orig$meanAge, 2)}, \\textit{{SD}} = {round(sample_orig$sdAge, 2)}, {sample_orig$nFemale} women, {sample_orig$nMale} men, and {sample_orig$nNa} non-binary individuals, {sample_orig$nRight} right."),
        "Final Sample", glue("{sample_final$N} people, \\textit{{M}} = {round(sample_final$meanAge, 2)}, \\textit{{SD}} = {round(sample_final$sdAge, 2)}, {sample_final$nFemale} women, {sample_final$nMale} men, and {sample_final$nNa} non-binary individuals, {sample_final$nRight} right."),
        sep = "\n"
      )
    )
  }
}
