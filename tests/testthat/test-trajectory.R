context("trajectory")

# Remove generated plot files in the end
on.exit(unlink('Rplots.pdf'))

# Test example 1
ex1.pars <- list(y0 = 0, add = F, tlim = c(0,5), system="one.dim")
ex1.out <- do.call(trajectory, c(deriv=phaseR::example1, ex1.pars))
# Test example 1, multiple trajectories
ex1b.pars <- list(y0 = c(0, 1, 3), add = F, tlim = c(0,5), system="one.dim")
ex1b.out <- do.call(trajectory, c(deriv=phaseR::example1, ex1b.pars))

# Test example 10
ex10.pars <- list(y0 = c(2, 5), add = F, tlim = c(0,5))
ex10.out <- do.call(trajectory, c(deriv=phaseR::example10, ex10.pars))
# Test example 10, multiple trajectories
ex10b.pars <- list(y0 = t(matrix(c(2, 5, -2, 4, 0, 3), nrow = 2)), add = F, 
                  tlim = c(0,5), xlim = c(-5,5))
ex10b.out <- do.call(trajectory, c(deriv=phaseR::example10, ex10b.pars))

test_that("behavior equal to reference behavior", {
  expect_equal_to_reference(ex1.out["y"], "test-trajectory_ref-ex1.rds")
  expect_equal_to_reference(ex1b.out["y"], "test-trajectory_ref-ex1b.rds")
  expect_equal_to_reference(ex10.out[c("x","y")], "test-trajectory_ref-ex10.rds")
  expect_equal_to_reference(ex10b.out[c("x","y")], "test-trajectory_ref-ex10b.rds")
})

test_that("alternative formulation equal to reference behavior", {
  # Alternative formulation 1D
  ex1.alt <- function(t, state, parameters) {
    with(as.list(c(state, parameters)), {
      dA = 4 - A^2
      list(c(dA))
    })
  }
  ex1.alt.out <- do.call(trajectory, c(list(deriv=ex1.alt, 
                                            state.names = c("A")), ex1.pars))
  expect_equal(ex1.alt.out["dy"], ex1.out["dy"])
  ex1b.alt.out <- do.call(trajectory, c(list(deriv=ex1.alt, 
                                             state.names = c("A")), ex1b.pars))
  expect_equal(ex1b.alt.out["dy"], ex1b.out["dy"])
  
  # Alternative formulation 2D
  ex10.alt <- function(t, state, parameters) {
    with(as.list(c(state, parameters)), {
      dI = -I + I^3
      dN = -2 * N
      list(c(dI, dN))
    })
  }
  ex10.alt.out <- do.call(trajectory, c(list(deriv=ex10.alt, 
                                             state.names = c("I", "N")), ex10.pars))
  expect_equal(ex10.alt.out[c("dx","dy")], ex10.out[c("dx","dy")])
  ex10b.alt.out <- do.call(trajectory, c(list(deriv=ex10.alt, 
                                             state.names = c("I", "N")), ex10b.pars))
  expect_equal(ex10b.alt.out[c("dx","dy")], ex10b.out[c("dx","dy")])
})