text_1 <- data.frame(word = LETTERS[1:7], freq = 10, stringsAsFactors = FALSE)
text_2 <- data.frame(word = LETTERS[2:8], freq = 5, stringsAsFactors = FALSE)

prop_shift <- proportion_shift(text_1, text_2)

expect_error(get_shift_graphs(text_1))
expect_error(get_shift_graphs(prop_shift, top_n = 0))


