
.onAttach <- function(libname, pkgname) {
  # sysfonts::font_add(family = "Lato", regular = system.file("Lato-Black.ttf", package = "Datavar"), bold = system.file("Lato-Bold.ttf", package = "Datavar"), italic = system.file("Lato-Italic.ttf", package = "Datavar"), bolditalic = system.file("Lato-BlackItalic.ttf", package = "Datavar"))
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
