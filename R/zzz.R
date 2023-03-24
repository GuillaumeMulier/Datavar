
.onAttach <- function(libname, pkgname) {
  if (!gdtools::font_family_exists("Lato"))
    packageStartupMessage("You might want to download the google font \"Lato\" with function \"register_gfont\" for flextable outputs.")
  Chemin <- tempdir()
  Blanco <- GGBlank()
  ggplot2::ggsave(Blanco, filename = paste0(Chemin, "/blanco.png"), device = "png", height = 8, width = 8)
  options(blanc_datavar = paste0(Chemin, "/blanco.png"))
  rm(Chemin, Blanco)
}
