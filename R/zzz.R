# from devtools -----------
maintainer <- function (pkg = ".") {
  pkg <- as.package(pkg)
  authors <- pkg$`authors@r`
  if (!is.null(authors)) {
    people <- eval(parse(text = authors))
    if (is.character(people)) {
      maintainer <- as.person(people)
    }
    else {
      maintainer <- Find(function(x) "cre" %in% x$role,
                         people)
    }
  }
  else {
    maintainer <- pkg$maintainer
    if (is.null(maintainer)) {
      stop("No maintainer defined in package.", call. = FALSE)
    }
    maintainer <- as.person(maintainer)
  }
  list(name = paste(maintainer$given, maintainer$family), email = maintainer$email)
}

# from devtools -----------
yesno <- function (...) {
  yeses <- c("Yes", "Definitely", "For sure", "Yup", "Yeah")
  nos <- c("No way", "Not yet", "I forget", "No", "Nope", "Uhhhh... Maybe?")
  cat(paste0(..., collapse = ""))
  qs <- c(sample(yeses, 1), sample(nos, 2))
  rand <- sample(length(qs))
  menu(qs[rand]) != which(rand == 1)
}

# from devtools -----------
rule <- function (..., pad = "-") {
  if (nargs() == 0) {
    title <- ""
  }
  else {
    title <- paste0(...)
  }
  width <- getOption("width") - nchar(title) - 1
  message(title, " ", paste(rep(pad, width, collapse = "")))
}
