context("numericalSolution")

# Remove generated plot files in the end
on.exit(unlink('Rplots.pdf'))

# Test vanDerPol example
vdp.pars <- list(y0 = c(4, 2), tlim = c(0, 100))
vdp.out <- do.call(numericalSolution, 
                   c(list(deriv=phaseR::vanDerPol, parameters = 3), vdp.pars))


test_that("behavior equal to reference behavior", {
  expect_equal_to_reference(vdp.out[c("x","y")], "test-numericalSolution_ref-vdp.rds")
})

test_that("alternative formulation equal to reference behavior", {
  # Alternative formulation 1D
  vdp.alt <- function(t, state, parameters) {
    with(as.list(c(state, parameters)), {
      dx <- y
      dy <- mu*(1 - x^2)*y - x
      list(c(dx, dy))
    })
  }
  vdp.alt.out <- do.call(numericalSolution, 
                         c(list(deriv=vdp.alt, parameters = c(mu=3)), vdp.pars))
  expect_equal(vdp.alt.out[c("x","y")], vdp.out[c("x","y")])
})