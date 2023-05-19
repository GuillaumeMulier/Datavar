
.onAttach <- function(libname, pkgname) {
  sysfonts::font_add(family = "Lato", regular = "data/fonts/Lato-Black.ttf", bold = "data/fonts/Lato-Bold.ttf", italic = "data/fonts/Lato-Italic.ttf", bolditalic = "data/fonts/Lato-BlackItalic.ttf")
  if (!"Lato" %in% sysfonts::font_families())
    packageStartupMessage("You might want to download the google font \"Lato\" with function \"register_gfont\" for flextable outputs.")
  Chemin <- tempdir()
  Blanco <- GGBlank()
  ggplot2::ggsave(Blanco, filename = paste0(Chemin, "/blanco.png"), device = "png", height = 8, width = 8)
  options(blanc_datavar = paste0(Chemin, "/blanco.png"))
  Chemin <- gsub("\\\\", "/", Chemin)
  message(Information(paste0("Writing a blank ggplot in a temporary file (", paste0(Chemin, "/blanco.png"), ").")))
  rm(Chemin, Blanco)
}
