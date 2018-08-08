context("drawManifolds")

# Remove generated plot files in the end
on.exit(unlink('Rplots.pdf'))

#To avoid plot.new hasn't been called
example5.flowField <- flowField(phaseR::example5, xlim = c(-3, 3), 
                                ylim = c(-3, 3), points = 19, add = FALSE)

# Test example 5
ex5.pars <- list(y0 = c(0, 0), tend = 100)
ex5.out <- do.call(drawManifolds, c(deriv=phaseR::example5, ex5.pars))

test_that("behavior equal to reference behavior", {
  expect_equal_to_reference(ex5.out[c("dx","dy")], "test-drawManifolds_ref-ex5.rds")
})

test_that("alternative formulation equal to reference behavior", {
  # Alternative formulation 2D
  ex5.alt <- function(t, state, parameters) {
    with(as.list(c(state, parameters)), {
      dx = 2*x + y
      dy = 2*x -y
      list(c(dx, dy))
    })
  }
  ex5.alt.out <- do.call(drawManifolds, c(deriv=ex5.alt, ex5.pars))
  expect_equal(ex5.alt.out[c("dx","dy")], ex5.out[c("dx","dy")])
})