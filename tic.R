do_package_checks(repos = repo_bioc(), error_on = "error")

if (ci_on_travis()) {
  do_pkgdown()
}
