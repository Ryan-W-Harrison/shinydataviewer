check1 <- checktor::checktor()
checktor::prescribe(check1)

# usethis::use_version("dev")
# usethis::use_version("patch")
# usethis::use_version("minor")
# usethis::use_version("major")

usethis::use_spell_check()

devtools::load_all()
devtools::document()
devtools::build_readme()

urlchecker::url_check()

devtools::check()

devtools::check(remote = TRUE, manual = TRUE)

devtools::check_win_devel()
