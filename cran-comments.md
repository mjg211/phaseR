## Resubmission
This is a resubmission. In this version I have:

* Used \donttest{} on a small number of examples that have longer execution time
* Expanded the acronym in the description field of the DESCRIPTION file
* Added an associated reference to the description field of the DESCRIPTION
file, with its DOI
* Added two code contributors to the authors field of the DESCRIPTION file
* Removed the use of \dontrun{} throughout the package; code that can
occassionally cause problems in certain environments has been removed to prevent
the need for \dontrun{}
* Used on.exit() after all calls to par()

## Test environments
* local OS X install, R 3.6.1
* Windows via devtools::check_win_devel() using devtools version 2.2.0

## Local OS X: R CMD check results
There were no ERRORs, WARNINGs, or NOTEs

## Windows devtools::check_win_devel(): R CMD check results
There were no ERRORs or WARNINGs

There was 1 NOTE:
- R Under development (unstable)

################################################################################

## Resubmission (11/09/19)
This is a resubmission. In this version I have:

* Modified the help file examples to wrap calls to drawManifolds() in
  \dontrun{}; this function was causing an error on Linux for one example (as
  opposed to a warning on Mac/Win). I can remove this example if desired, but
  would prefer to highlight this potential problem for certain user inputs in
  the help files

## Test environments
* local OS X install, R 3.6.1
* Windows via devtools::check_win_devel() using devtools version 2.2.0

## Local OS X: R CMD check results
There were no ERRORs, WARNINGs, or NOTEs

## Windows devtools::check_win_devel(): R CMD check results
There were no ERRORs or WARNINGs

There were 2 NOTEs:
- One noting re-submission to fix archiving issues (apologies)
- One on example run-time (which is not an issue locally); the examples should
not in general take long to run
