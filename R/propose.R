#' Propose a package for rOpenSci
#'
#' @importFrom devtools as.package
#' @export
#' @param pkg package description, can be path or package name. See
#' \code{\link[devtools]{as.package}} for more information
#' @examples \dontrun{
#' propose()
#' }
propose <- function(pkg = ".") {
  pkg <- as.package(pkg)
  message("We'll ask some questions - with numbered prompts, you can press enter to
      drop to a new line, and press enter on a blank line to end")
  rule("DESCRIPTION")
  cat(readLines(file.path(pkg$path, "DESCRIPTION")), sep = "\n")
  cat("\n")
  if (yesno("Is DESCRIPTION up-to-date?"))
    return(invisible())

  message("What does this package do? (explain in 50 words or less)\n")
  pkg$pkgdo <- input()

  message("URL for the package (the development repository, not a stylized html page)")
  pkg$pkgurl <- input()

  message("What data source(s) does it work with (if applicable)?")
  pkg$datasources <- input()

  message("Who is the target audience?")
  pkg$target <- input()

  message("Confirm that you agree to the following rOpenSci policies. These are mandatory.")
  pkg$pol_ci <- !yesno("The repository has continuous integration with Travis and/or another service ")
  pkg$pol_vign <- !yesno("The package contains a vignette ")
  pkg$pol_readme <- !yesno("The package contains a complete readme with devtools install instructions ")
  pkg$pol_tests <- !yesno("The package contains unit tests ")

  message("Do you agree to follow the rOpenSci packaging guidelines\n(https://github.com/ropensci/packaging_guide)? These aren't mandatory,\nbut we strongly suggest you follow them. If you disagree with anything, please explain.")
  pkg$pkgguide_lic <- !yesno("Does the package have a CRAN accepted license? ")
  pkg$pkgguide_nocran <- !yesno("Are there any packages that your package depends on that are not on CRAN? ")
  pkg$pkgguide_cran <- !yesno("Do you intend for this package to go on CRAN? ")
  message("Please add explanations below for any exceptions to the above: ")
  pkg$pkgguide_txt <- input()

  if (yesno("Is your email address ", maintainer(pkg)$email, "?"))
    return(invisible())
  if (yesno("Ready to submit?"))
    return(invisible())
  submit_ro(pkg)
  invisible(TRUE)
}

submit_ro <- function(x) {
  title <- x$package
  bodyyaml <- Map(function(x,y) paste0("- ", x, ": ", y, collapse = ""), names(x), x)
  url <- browse_issue(issue(title = title, body = paste0(unname(bodyyaml), collapse = "\n")))
  message(sprintf("Browse your created issue at %s", url))
}

input <- function() {
  ret <- scan(quiet = TRUE, what = 'raw')
  paste0(ret, collapse = "\n")
}
