install.packages(
  "pak",
  repos = sprintf(
    "https://r-lib.github.io/p/pak/stable/%s/%s/%s",
    .Platform$pkgType,
    R.Version()$os,
    R.Version()$arch
  )
)

pak::pkg_install('shiny')
pak::pkg_install('dplyr')
pak::pkg_install('stringr')
pak::pkg_install('leaflet')
pak::pkg_install('fontawesome')
pak::pkg_install('networkD3')
pak::pkg_install('plotly')
pak::pkg_install('ggplot2')
pak::pkg_install('ggalluvial')
pak::pkg_install('htmltools')
pak::pkg_install('sp')
