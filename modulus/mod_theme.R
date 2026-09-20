box::use(fresh)

#' Build the Lunexa dashboard theme
#'
#' Creates a \code{fresh} theme object that overrides the default
#' AdminLTE 3 / Bootstrap 4 colour palette with the Lunexa brand
#' palette: orange primary, dark-slate sidebar, and soft grey body
#' background.
#'
#' @return A \code{fresh::bs4DashTheme} object to pass to
#'   \code{bs4Dash::dashboardPage(freshTheme = ...)}.
#'
#' @export
lunexa_theme <- function() {
  fresh::create_theme(
    fresh::bs4dash_vars(
      navbar_light_color   = "#334155",
      navbar_light_active_color = "#ff851b",
      navbar_light_hover_color  = "#ff851b"
    ),
    fresh::bs4dash_yiq(
      contrasted_threshold = 175,
      text_dark  = "#334155",
      text_light = "#f8fafc"
    ),
    fresh::bs4dash_layout(
      main_bg = "#f1f5f9",
      sidebar_width = "280px"
    ),
    fresh::bs4dash_sidebar_light(
      bg             = "#1e293b",
      color          = "#94a3b8",
      hover_color    = "#f8fafc",
      active_color   = "#ffffff",
      submenu_bg     = "#162032",
      submenu_color  = "#94a3b8",
      submenu_hover_color  = "#f8fafc",
      submenu_active_color = "#ff851b"
    ),
    fresh::bs4dash_sidebar_dark(
      bg             = "#1e293b",
      color          = "#94a3b8",
      hover_color    = "#f8fafc",
      active_color   = "#ffffff",
      submenu_bg     = "#162032",
      submenu_color  = "#94a3b8",
      submenu_hover_color  = "#f8fafc",
      submenu_active_color = "#ff851b"
    ),
    fresh::bs4dash_status(
      primary = "#ff851b",
      success = "#22c55e",
      info    = "#3b82f6",
      warning = "#f59e0b",
      danger  = "#ef4444"
    ),
    fresh::bs4dash_color(
      blue   = "#3b82f6",
      green  = "#22c55e",
      orange = "#ff851b",
      red    = "#ef4444",
      yellow = "#f59e0b"
    )
  )
}
