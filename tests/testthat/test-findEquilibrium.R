context("findEquilibrium")

# Remove generated plot files in the end
on.exit(unlink('Rplots.pdf'))

# Test example 1
ex1.pars <- list(y = 1, system="one.dim")
ex1.out <- do.call(findEquilibrium, c(deriv=phaseR::example1, ex1.pars))

# Test example 5
ex5.pars <- list(y0 = c(0, 0))
ex5.out <- do.call(findEquilibrium, c(deriv=phaseR::example5, ex5.pars))

test_that("behavior equal to reference behavior", {
  expect_equal_to_reference(ex1.out["dy"], "test-findEquilibrium_ref-ex1.rds")
  expect_equal_to_reference(ex5.out[c("dx","dy")], "test-findEquilibrium_ref-ex5.rds")
})

test_that("alternative formulation equal to reference behavior", {
  # Alternative formulation 1D
  ex1.alt <- function(t, state, parameters) {
    with(as.list(c(state, parameters)), {
      dA = 4 - A^2
      list(c(dA))
    })
  }
  ex1.alt.out <- do.call(findEquilibrium, c(list(deriv=ex1.alt, state.names = c("A")), ex1.pars))
  expect_equal(ex1.alt.out["dy"], ex1.out["dy"])
  
  # Alternative formulation 2D
  ex5.alt <- function(t, state, parameters) {
    with(as.list(c(state, parameters)), {
      dx = 2*x + y
      dy = 2*x -y
      list(c(dx, dy))
    })
  }
  ex5.alt.out <- do.call(findEquilibrium, c(deriv=ex5.alt, ex5.pars))
  expect_equal(ex5.alt.out[c("dx","dy")], ex5.out[c("dx","dy")])
})