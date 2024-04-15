test_that("error handling works", {

  msg <- "Need at least 1 depth argument for 'ObtainDepthScale'."
  expect_error(ObtainDepthScale(), msg)

  thickness <- depth <- top <- bottom <- 1

  msg <- "Using only 'thickness' argument, ignoring other arguments passed."
  expect_warning(ObtainDepthScale(thickness, depth, top, bottom), msg)
  expect_warning(ObtainDepthScale(thickness, depth, top), msg)
  expect_warning(ObtainDepthScale(thickness, depth, bottom = bottom), msg)
  expect_warning(ObtainDepthScale(thickness, top = top, bottom = bottom), msg)
  expect_warning(ObtainDepthScale(thickness, depth), msg)
  expect_warning(ObtainDepthScale(thickness, top = top), msg)
  expect_warning(ObtainDepthScale(thickness, bottom = bottom), msg)

  msg <- "Using only 'depth' argument, ignoring other arguments passed."
  expect_warning(ObtainDepthScale(depth = depth, top = top,
                                   bottom = bottom), msg)
  expect_warning(ObtainDepthScale(depth = depth, top = top), msg)
  expect_warning(ObtainDepthScale(depth = depth, bottom = bottom), msg)

  msg <- "'top' depths provided, need also 'bottom' depths."
  expect_error(ObtainDepthScale(top = top), msg)

  msg <- "'bottom' depths provided, need also 'top' depths."
  expect_error(ObtainDepthScale(bottom = bottom), msg)

  msg <- "Missing 'startDepth'."
  expect_error(ObtainDepthScale(depth = depth, startDepth = NULL), msg)
  expect_error(ObtainDepthScale(thickness = thickness,
                                 startDepth = NULL), msg)

  msg <- "'startDepth' either < 0 or NA."
  expect_error(ObtainDepthScale(depth = depth, startDepth = -123), msg)
  expect_error(ObtainDepthScale(thickness = thickness,
                                 startDepth = -123), msg)
  expect_error(ObtainDepthScale(depth = depth, startDepth = NA), msg)
  expect_error(ObtainDepthScale(thickness = thickness,
                                 startDepth = NA), msg)

  msg <- "'top' and 'bottom' must have the same length."
  expect_error(ObtainDepthScale(top = 1, bottom = 1 : 2), msg)

  msg <- paste("Given 'startDepth' >= first midpoint depth,",
               "which would yield zero or negative first layer thickness.")
  expect_error(ObtainDepthScale(depth = c(1, 2), startDepth = 5), msg)

})

test_that("depth conversions work", {

  thicknessExpected <- c(1, 3, 6, 2, 4)
  topExpected       <- c(0, 1, 4, 10, 12)
  bottomExpected    <- c(1, 4, 10, 12, 16)
  depthExpected     <- c(0.5, 2.5, 7, 11, 14)

  profileExpected <- data.frame(
    depth = depthExpected, top = topExpected,
    bottom = bottomExpected, thickness = thicknessExpected
  )

  actual <- ObtainDepthScale(thickness = thicknessExpected)
  expect_equal(actual, profileExpected)

  actual <- ObtainDepthScale(top = topExpected, bottom = bottomExpected)
  expect_equal(actual, profileExpected)

  actual <- ObtainDepthScale(depth = depthExpected)
  expect_equal(actual, profileExpected)

  startDepth <- 41.41
  topExpected       <- c(0, 1, 4, 10, 12) + startDepth
  bottomExpected    <- c(1, 4, 10, 12, 16) + startDepth
  depthExpected     <- c(0.5, 2.5, 7, 11, 14) + startDepth

  profileExpected <- data.frame(
    depth = depthExpected, top = topExpected,
    bottom = bottomExpected, thickness = thicknessExpected
  )

  actual <- ObtainDepthScale(thickness = thicknessExpected,
                              startDepth = startDepth)
  expect_equal(actual, profileExpected)

  actual <- ObtainDepthScale(depth = depthExpected,
                              startDepth = startDepth)
  expect_equal(actual, profileExpected)

  # if top and bottom are provided first_layer_depth setting is irrelevant
  actual <- ObtainDepthScale(top = topExpected, bottom = bottomExpected,
                             startDepth = NA)
  expect_equal(actual, profileExpected)

})
