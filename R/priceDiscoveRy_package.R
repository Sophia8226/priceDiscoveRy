#' priceDiscoveRy: Price discovery in partially overlapping markets
#'
#' @description
#' Provides tools for estimating contribution-weighted information shares
#' (CWIS) for two markets with partially overlapping trading hours. The package
#' combines price discovery during simultaneous trading with price variation
#' generated while only one market is open.
#'
#' @section Background:
#'
#' The same or economically equivalent asset may trade in more than one market.
#' Although the observed prices can temporarily differ because of trading
#' frictions, liquidity, and market-microstructure noise, they are expected to
#' reflect a common underlying efficient price.
#'
#' Conventional Hasbrouck information shares measure how simultaneously
#' trading markets contribute to innovations in this efficient price. However,
#' they use only periods in which all markets are open. This can omit relevant
#' information when one market trades for longer hours than another.
#'
#' Methods for sequential markets assign price discovery to the market that is
#' open during a non-overlapping period, but they do not directly address a
#' trading day containing both overlapping and non-overlapping sessions.
#'
#' The contribution-weighted information share proposed by Dimpfl and
#' Schweikert (2023) connects these two cases. It uses Hasbrouck information
#' shares while both markets trade and assigns single-market price discovery
#' to the market that remains open. The different trading periods are weighted
#' by their contributions to total daily efficient-price variation rather than
#' by their duration.
#'
#' This makes it possible to use price information from the complete trading
#' day and to avoid understating the contribution of a market that continues
#' trading while the other market is closed.
#'
#' @section Method overview:
#'
#' The package follows the computational workflow illustrated below.
#'
#' \if{html}{\figure{package-workflow.png}{options: style="display:block; margin-left:auto; margin-right:auto; max-width:100\%; height:auto;" alt="Workflow of the priceDiscoveRy package"}}
#' \if{latex}{
#' \out{\begin{center}}
#' \figure{package-workflow.png}{options: width=6in}
#' \out{\end{center}}
#' }
#'
#' The workflow consists of the following steps:
#' \enumerate{
#'   \item Validate and standardize the timestamps and price series.
#'   \item Divide each trading day into overlapping and single-market sessions.
#'   \item Estimate a bivariate vector error-correction model during the
#'   overlapping session.
#'   \item Compute the Hasbrouck information-share bounds and their midpoint.
#'   \item Extract the common efficient price and estimate its realized
#'   variance.
#'   \item Estimate generalized realized kernels during the single-market
#'   sessions.
#'   \item Construct variance-based weights and combine the session-specific
#'   estimates.
#'   \item Report daily CWIS estimates and, where requested, aggregate them
#'   across trading days.
#' }
#'
#' The main user interface is [cwis()]. Further details on the individual
#' methodological components are provided in the documentation for the
#' corresponding lower-level functions and in the package vignettes.
#'
#' @section Main functions:
#'
#' - [cwis()] calculates CWIS estimates for one or more trading days.
#' - [daily_cwis()] performs the complete calculation for one trading day.
#' - [aggregate_cwis()] summarizes estimates and diagnostics across days.
#' - [vecm()] estimates the overlapping-period VECM.
#' - [his()] calculates Hasbrouck information shares.
#' - [common_price()] extracts the common efficient price.
#' - [realized_kernel()] estimates variation during single-market sessions.
#' - [combine_weights()] constructs the intraday variance weights.
#'
#' @section Scope and assumptions:
#'
#' The two input price series should represent the same underlying asset,
#' or closely related assets connected by a stable long-run equilibrium
#' relationship. During overlapping trading sessions, the price series are
#' assumed to be non-stationary, cointegrated, and driven by one common
#' efficient-price trend. Prices must be expressed on comparable scales and
#' synchronized before estimation.
#'
#' The efficient price is assumed to continue evolving when either market is
#' closed. During a single-market session, the open market is assigned the
#' information contribution generated in that session, while the closed market
#' receives no contribution. Session weights are determined by each session's
#' estimated contribution to the daily variation of the efficient price.
#'
#' Trading days and session boundaries are interpreted in the user-specified
#' time zone. When applying the package to other assets or exchanges, users
#' should verify the time zone, trading-day convention, session definitions,
#' price scaling, sampling frequency, and VECM lag order.
#'
#' The package estimates a standard Johansen VECM using [urca::ca.jo()]. It
#' implements the general CWIS framework but does not exactly reproduce the
#' high-resolution, HAR-structured VECM used in the empirical analysis of
#' Dimpfl and Schweikert (2023).
#'
#' @section Main references:
#' Hasbrouck, J. (1995). One security, many markets: Determining the
#' contributions to price discovery.\emph{The Journal of Finance}, 50(4),
#' 1175--1199.\doi{10.2307/2329348}
#'
#' Wang, J. and Yang, M. (2011). Housewives of Tokyo versus the gnomes of
#' Zurich: Measuring price discovery in sequential markets.
#' \emph{Journal of Financial Markets}, 14(1), 82--108.\doi{10.1016/j.finmar.2010.08.002}
#'
#' Hasbrouck, J. (2021). Price discovery in high resolution.
#' \emph{Journal of Financial Econometrics}, 19(3), 395--430.
#' \doi{10.1093/jjfinec/nbz027}
#'
#' Dias, G. F. and Schweikert, K. (2022). Integrated variance estimation for
#' assets traded in multiple venues. \emph{SSRN Working Paper}, 1--48.
#' \url{https://ssrn.com/abstract=4253762}
#'
#' Dimpfl, T. and Schweikert, K. (2023). Information shares for markets with
#' partially overlapping trading hours. \emph{Journal of Banking & Finance},
#' 154, 106970. \doi{10.1016/j.jbankfin.2023.106970}
#'
#' @seealso
#' [cwis()] for the main estimation interface;
#' [daily_cwis()] for the complete single-day calculation; and
#' [aggregate_cwis()] for cross-day summaries.
#'
"_PACKAGE"
