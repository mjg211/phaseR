context("stability")

# Remove generated plot files in the end
on.exit(unlink('Rplots.pdf'))

# Test example 2
ex2.pars <- list(ystar = 0, system="one.dim", summary = F)
ex2.out <- do.call(stability, c(deriv=phaseR::example2, ex2.pars))
ex2.out[["deriv"]] <- NULL

# Test example 11
ex11.pars <- list(ystar = c(0, 0), summary = F)
ex11.out <- do.call(stability, c(deriv=phaseR::example11, ex11.pars))
ex11.out[["deriv"]] <- NULL

test_that("behavior equal to reference behavior", {
  expect_equal_to_reference(ex2.out, "test-stability_ref-ex2.rds")
  expect_equal_to_reference(ex11.out, "test-stability_ref-ex11.rds")
})

test_that("alternative formulation equal to reference behavior", {
  # Alternative formulation 1D
  ex2.alt <- function(t, state, parameters) {
    with(as.list(c(state, parameters)), {
      dA = A*(1 - A)*(2 - A)
      list(c(dA))
    })
  }
  ex2.alt.out <- do.call(stability, c(list(deriv=ex2.alt, state.names = c("A")), ex2.pars))
  ex2.alt.out[["deriv"]] <- NULL
  expect_equal(ex2.alt.out, ex2.out)
  
  # Alternative formulation 2D
  ex11.alt <- function(t, state, parameters) {
    with(as.list(c(state, parameters)), {
      dI = I*(3 - I - 2*N)
      dN = N*(2 - I - N)
      list(c(dI, dN))
    })
  }
  ex11.alt.out <- do.call(stability, c(list(deriv=ex11.alt, state.names = c("I", "N")), ex11.pars))
  ex11.alt.out[["deriv"]] <- NULL
  expect_equal(ex11.alt.out, ex11.out)
})