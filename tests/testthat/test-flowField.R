context("flowField")

# Remove generated plot files in the end
on.exit(unlink('Rplots.pdf'))

# Test example 1
ex1.pars <- list(xlim = c(-5, 5), ylim = c(-5,5), add = F, points=11, system="one.dim")
ex1.out <- do.call(flowField, c(deriv=phaseR::example1, ex1.pars))

# Test example 10
ex10.pars <- list(xlim = c(-5, 5), ylim = c(-5, 5), add = F, points=11)
ex10.out <- do.call(flowField, c(deriv=phaseR::example10, ex10.pars))

test_that("behavior equal to reference behavior", {
  expect_equal_to_reference(ex1.out["dy"], "test-flowField_ref-ex1.rds")
  expect_equal_to_reference(ex10.out[c("dx","dy")], "test-flowField_ref-ex10.rds")
})

test_that("alternative formulation equal to reference behavior", {
  # Alternative formulation 1D
  ex1.alt <- function(t, state, parameters) {
    with(as.list(c(state, parameters)), {
      dA = 4 - A^2
      list(c(dA))
    })
  }
  ex1.alt.out <- do.call(flowField, c(list(deriv=ex1.alt, state.names = c("A")), ex1.pars))
  expect_equal(ex1.alt.out["dy"], ex1.out["dy"])
  
  # Alternative formulation 2D
  ex10.alt <- function(t, state, parameters) {
    with(as.list(c(state, parameters)), {
      dI = -I + I^3
      dN = -2 * N
      list(c(dI, dN))
    })
  }
  ex10.alt.out <- do.call(flowField, c(list(deriv=ex10.alt, state.names = c("I", "N")), ex10.pars))
  expect_equal(ex10.alt.out[c("dx","dy")], ex10.out[c("dx","dy")])
})