# FIXME: This test is kind of dumb. Consider something else.
test_that("ku_sequential_single_hue() works on Crimson", {
  expect_equal(ku_sequential_single_hue(5L, ku_color("Crimson")), c("#F1F1F1", "#FAD8D9", "#FFACAD", "#FD7172", "#E8000D"))
})
