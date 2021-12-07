##' Simulate virtual firn profile
##'
##' Forward-simulate a virtual firn depth profile based on given temperature
##' and precipitation time series and local climatic parameters; see Details.
##'
##' This function implements the generation of a proxy depth profile measured on
##' a firn/ice core based on the following governing processes:
##' linear conversion (calibration) to proxy units, precipitation intermittency,
##' densification, firn diffusion, and sampling process. In detail this works as
##' follows:
##'
##' Layers of precipitated snow are piled up from an input time series of
##' precipitation data with thickness in water equivalent units, and are
##' associated with the time of the precipitation event and the proxy value
##' in the snow, the latter being based on a climatic input time series and a
##' linear proxy calibration. The uneven layers are converted to an
##' unequidistant true depth scale based on a calculated Herron-Langway
##' densification model. Subsequently, this depth scale is, together with the
##' corresponding time information and proxy data, interpolated onto an
##' equidistant high-resolution firn/ice depth scale with a depth resolution
##' given by the minimum observed layer thickness in real units. If desired, the
##' proxy depth profile is then diffused with a calculated diffusion length. For
##' the output, the resulting depth profile is block averaged to a given coarser
##' resolution mimicking the typical size of sampling intervals upon cutting a
##' firn/ice core.
##'
##' @param time numeric vector of time points corresponding to the observation
##'   points of the precipitation and temperature time series. Must be in a date
##'   format which supports calculation of midpoint and average values (via
##'   \code{approx()} and \code{mean()}).
##' @param precip numeric vector with a precipitation time series (in
##'   w.eq. units) tabulated at the time points in \code{time}.
##' @param temperature numeric vector with a temperature time series (in deg C)
##'   tabulated at the time points in \code{time}.
##' @param data numeric vector with a data time series from which the firn
##'   profile is to be simulated, tabulated at the time points in \code{time};
##'   the default is to use the \code{temperature} time series, but also any
##'   other suitable environmental proxy can be input here for profile
##'   simulation. In such case, the \code{temperature} input is still needed but
##'   only to obtain an average temperature value for the densification rate and
##'   diffusion length calculations.
##' @param pressure local atmospheric surface pressure in mbar. Defaults to
##'   observed average pressure at EDML site.
##' @param rho.surface local surface density in kg/m^3.
##' @param accumulation.scale factor to convert the average value of
##'   \code{precip} into units of mm w.eq. per year. Default is to assume the
##'   precipitation data is given in units of mm w.eq. per day.
##' @param depth.scale factor to convert the precipitation data into units of m
##'   w.eq. Default is to assume the precipitation data is given in mm w.eq.
##' @param alpha slope of a linear calibration to convert the input \code{data}
##'   into the desired units of the firn profile; e.g. a linear
##'   temperature-to-isotope calibration.
##' @param beta the same as \code{alpha} but providing the intercept of the
##'   linear calibration.
##' @param dz.out output resolution in m of the simulated firn profile to
##'   mimick a typical firn/ice core sampling process. Defaults to 3 cm.
##' @param diffuse logical to control whether the simulated firn profile
##'   shall be diffused according to the standard firn diffusion model.
##' @return A data frame of three variables with the simulated firn profile:
##'   firn/ice midpoint depths in m, time relative to the first observation
##'   point of the input time series, and corresponding firn profile (proxy)
##'   value. Additionally, attributes are attached to the data frame which
##'   include information on the simulation run: applied linear calibration
##'   parameters, used atmospheric pressure, surface snow density and output
##'   resolution, the diffusion flag, and the date of the run.
##' @examples
##'
##' # --- Simple simulation example with constant daily accumulation ---
##'
##' # create 5-yr precipitation time series with constant daily amount
##' # (with annual accumulation amount from EDML site)
##' nyr <- 5
##' precip <- rep(70 / 365, times = nyr * 365)
##'
##' # create bi-harmonic model of sesasonal cycle in temperature at EDML;
##' # repeat 5 years and add some noise
##' seasonal.par <- c(-44.5, 13, 5, 10, 50)
##' temperature <- rep(HarmonicModel(seasonal.par), nyr) +
##'                rnorm(length(precip), sd = 2)
##'
##' # arbitrary time vector
##' time <- as.Date(-1 * (length(precip) : 1), origin = "2020-01-01")
##'
##' # run simulation
##' profile.nodiff <- SimProfile(time, precip, temperature, diffuse = FALSE)
##' profile <- SimProfile(time, precip, temperature)
##'
##' # compare original time series to simulated time series in the firn
##' plot(time, temperature, type = "l", ylim = c(-65, -15),
##'      xlab = "Time", ylab = "Original and simulated profiles (a.u.)")
##' lines(profile.nodiff$time, profile.nodiff$d18O, col = 4, lwd = 2)
##' lines(profile$time, profile$d18O, col = 2, lwd = 2)
##' legend("topright",
##'        c("Original ts", "Simulated ts w/o diffusion",
##'          "Simulated ts with diffusion"),
##'        lty = 1, lwd = c(1, 2, 2), col = c(1, 2, 4))
##'
##' # show simulated depth profile
##' plot(profile$depth, profile$d18O, col = 2, type = "l", lwd = 2,
##'      xlab = "Depth (m)", ylab = "Simulated depth profile")
##' abline(h = mean(profile$d18O), lty = 2)
##'
##' @author Thomas Münch
##' @export
SimProfile <- function(time, precip, temperature, data = temperature,
                       pressure = 677, rho.surface = 340,
                       accumulation.scale = 365, depth.scale = 10^-3,
                       alpha = 1, beta = 0, dz.out = 0.03, diffuse = TRUE) {

  ll <- stats::sd(c(length(precip), length(time),
                    length(temperature), length(data)))
  if (ll > 0) {
    stop("All input vectors must have the same length.", call. = FALSE)
  }

  # average accumulation in mm w.eq. per year
  bdot <- mean(precip) * accumulation.scale
  # annual mean temperature in K
  T <- mean(temperature) + 273.15

  # order such that most recent event is at the top
  ordr <- order(time, decreasing = TRUE)

  time        <- time[ordr]
  precip      <- precip[ordr]
  temperature <- temperature[ordr]
  data        <- data[ordr]

  # remove events without precipitation accounting for numerical threshold;
  # -> record only at least micrometre precip. events
  record <- (depth.scale * precip) > 1.e-7

  time        <- time[record]
  precip      <- precip[record]
  temperature <- temperature[record]
  data        <- data[record]

  # build profile of top, bottom and midpoint depths of precipitated layers
  depthProfileWE <- ObtainDepthScale(thickness = depth.scale * precip)

  # simulate high-resolution firn density profile with input depth vector of
  # maximum possible length from assuming constant surface density;
  # interpolate it to w.eq. midpoint depths
  rhoWater <- 1000
  convFac  <- round(rhoWater / rho.surface, 1)
  depthProfileWE <- depthProfileWE %>%
    dplyr::mutate(
      density = seq(0, convFac * max(.data$depth), min(.data$thickness)) %>%
        DensityHL(rho.surface = rho.surface, T = T, bdot = bdot) %>%
        data.frame() %>%
        stats::approx(xout = .data$depth) %>%
        purrr::pluck("y"))

  # create depth profile in real units and add isotope data of precip events
  profile <- depthProfileWE %>%
    dplyr::transmute(thickness = .data$thickness * rhoWater / .data$density) %>%
    dplyr::pull(thickness) %>%
    ObtainDepthScale() %>%
    dplyr::mutate(time = time) %>%
    dplyr::mutate(d18O = Temperature2Isotopes(data, alpha, beta))
  
  # interpolate data to high resolution equal to maximum 0.1 mm;
  # add density and diffusion length data
  res <- max(1.e-4, min(profile$thickness))
  profileEqui <- data.frame(depth = seq(min(profile$depth),
                                        max(profile$depth), res)) %>%
    dplyr::mutate(
      time = stats::approx(profile$depth, profile$time, .data$depth) %>%
        purrr::pluck("y")) %>%
    dplyr::mutate(
      d18O = stats::approx(profile$depth, profile$d18O, .data$depth) %>%
        purrr::pluck("y")) %>%
    dplyr::mutate(
      density = DensityHL(.data$depth, rho.surface = rho.surface,
                          T = T, bdot = bdot)$rho) %>%
    dplyr::mutate(
      sigma = DiffusionLength(.data$depth, .data$density,
                              T = T, P = pressure, bdot = bdot))

  # diffuse isotope record if requested
  if (diffuse) {
    profileEqui <- profileEqui %>%
      dplyr::mutate(d18O = DiffuseRecord(.data$d18O, .data$sigma,
                                         res = 100. * mean(diff(.data$depth))))
  }

  # block-average data to desired output resolution
  breaks <- seq(0, max(profileEqui$depth), dz.out)

  profileAvg <- PaleoSpec::AvgToBin(profileEqui$depth, profileEqui$time,
                                    breaks = breaks)[c("centers", "avg")] %>%
    data.frame() %>%
    dplyr::rename(depth = .data$centers, time = .data$avg) %>%
    dplyr::mutate(d18O = PaleoSpec::AvgToBin(profileEqui$depth, profileEqui$d18O,
                                             breaks = breaks)[["avg"]])

  # convert date vector back to proper format
  profileAvg <- profileAvg %>%
    dplyr::mutate(time = as.Date(time, origin = "1970-01-01"))

  # set attributes for output with information on simulation
  attr(profileAvg, "calibration slope") <- alpha
  attr(profileAvg, "calibration intercept") <- beta
  attr(profileAvg, "atmospheric pressure [mbar]") <- pressure
  attr(profileAvg, "surface snow density [kg/m^3]") <- rho.surface
  attr(profileAvg, "output resolution [cm]") <- 100. * dz.out
  attr(profileAvg, "diffused") <- diffuse
  attr(profileAvg, "date") <- Sys.time()

  return(profileAvg)

}


# for testing
## sin.par <- c(273 - 44.5, 13, 5, 10, 50)

## precip <- rep(70 / 365, times = 1 * 365)
## temperature <- rep(HarmonicModel(sin.par), 1)# + rnorm(length(precip), sd = 2)
## pressure <- 670
## time <- seq(length.out = length(precip))
## accumulation.scale <- 365
## depth.scale <- 10^-3

## system.time(
## profile <- SimProfile(time, precip, temperature, pressure,
##                   accumulation.scale = accumulation.scale,
##                   diffuse = TRUE)
## )

## quartz()

## plot(time, temperature, type = "l")
## lines(profile$time, profile$d18O, col = 2, lwd = 2)

## plot(profile$depth, profile$d18O - mean(profile$d18O), col = 2, type = "l", lwd = 2, xlim = c(0,10))
## abline(h = mean(profile$d18O), lty = 2)
