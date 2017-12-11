context("phasePortrait")

# Remove generated plot files in the end
on.exit(unlink('Rplots.pdf'))

# Test example 2
ex2.pars <- list(ylim = c(-0.5, 2.5), points = 10, frac = 0.5)
ex2.out <- do.call(phasePortrait, c(deriv=phaseR::example2, ex2.pars))

test_that("behavior equal to reference behavior", {
  expect_equal_to_reference(ex2.out["dy"], "test-phasePortrait_ref-ex2.rds")
})

test_that("alternative formulation equal to reference behavior", {
  ex2.alt <- function(t, state, parameters) {
    with(as.list(c(state, parameters)), {
      dA = A*(1 - A)*(2 - A)
      list(c(dA))
    })
  }
  ex2.alt.out <- do.call(phasePortrait, c(list(deriv=ex2.alt, 
                                               state.names = c("A")), ex2.pars))
  expect_equal(ex2.alt.out["dy"], ex2.out["dy"])
})