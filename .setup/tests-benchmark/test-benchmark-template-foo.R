## ---- test-benchmark-template-foo
lapply(
  X = 1,
  FUN = function(i) {
    x <- stats::rnorm(n = 1)
    y <- stats::rnorm(n = 1)
    output <- microbenchmark::microbenchmark(
      Foo(x, y),
      x + y
    )
    print(
      summary(
        output,
        unit = "relative"
      )
    )
    ggplot2::autoplot(output)
  }
)
