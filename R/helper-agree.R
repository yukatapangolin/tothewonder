## Helper function to create a global variable with the ucd session for
## the testthat tests
wonder_factory <- function(ua) {
  function() {
    wonder_url_ucd99 <- paste0(
      "https://wonder.cdc.gov",
      local({
        res <- httr::POST(
          httr::set_cookies(`s_cc` = "true"),
          url =
            "https://wonder.cdc.gov/controller/datarequest/D76",
          body = list(
            "stage" = "about",
            "saved_id" = "",
            `action-I Agree` = "I Agree"
          ),
          encode = "form",
          httr::add_headers(.headers = c(
            "user-agent" = ua,
            "Cache-Control" = "max-age=0",
            "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
            "Accept-Language" = "en-us,en;q=0.5"
          ))
        )
        w_txt <- httr::content(res, as = "text")
        regmatches(
          w_txt,
          regexpr(
            "/controller/datarequest/D76;jsessionid=[A-Z0-9]+",
            w_txt
          )
        )
      })
    )
  }
}

chrome <- wonder_factory("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/99.0.4844.51 Safari/537.36")
delayedAssign("wonder_url_ucd99_test", chrome())
