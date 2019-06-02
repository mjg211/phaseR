.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    rep("-", 79), "\nphaseR: Phase plane analysis of one- and two-dimensional ",
    "autonomous ODE systems\n", rep("-", 79), "\n\nv.2.1: For an overview of the ",
    "package's functionality enter: ?phaseR\n\nFor news on the latest ",
    "updates enter: news(package = \"phaseR\")")
}
