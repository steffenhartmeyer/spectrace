# install_pySpectrace <-
#   function(...,
#            envname = "pySpectrace",
#            new_env = identical(envname, "pySpectrace")) {
#
#     if(new_env && reticulate::virtualenv_exists(envname))
#       reticulate::virtualenv_remove(envname)
#
#     reticulate::py_install(packages = c("numpy", "pandas", "pytorch", "scikit-learn"),
#                            envname = envname, ...)
#   }
# #
# # .onLoad <- function(...) {
# #   reticulate::use_virtualenv("pySpectrace", required = FALSE)
# # }
