
text_1 <- data.frame(word = LETTERS[1:7], freq = 10, stringsAsFactors = FALSE)
text_2 <- data.frame(word = LETTERS[2:8], freq = 5, stringsAsFactors = FALSE)
type2score_1 <- data.frame(word = LETTERS[c(2:7, 9)], score = c(1,1,1,.8,1.2,1,1), stringsAsFactors = FALSE)
type2score_2 <- data.frame(word = LETTERS[2:8], score = c(1,1,1,.8,1.2,1,1) , stringsAsFactors = FALSE)
# type2score_2$score_2[5] <- NA

# test proportion shift
prop_shift <- proportion_shift(text_1, text_2)
expect_equal(class(prop_shift), "shift")
expect_equal(class(prop_shift$shift_scores), "data.frame")
expect_false(prop_shift$show_total)
expect_false(prop_shift$show_score_diffs)


# test entropy
entropy <- entropy_shift(text_1, text_2)
expect_true(entropy$show_score_diffs)

# test base and alpha in one of the shifts
expect_warning(entropy_shift(text_1, text_2, base = 0))
expect_error(entropy_shift(text_1, text_2, base = -1))
expect_error(entropy_shift(text_1, text_2, alpha = "a"))


# test JSD
jsd <- jsdivergence_shift(text_1, text_2)
expect_true(jsd$all_pos_contributions)

expect_error(jsdivergence_shift(text_1, text_2, weight_1 = .3, weight_2 = .8))
expect_message(jsdivergence_shift(text_1, text_2, weight_1 = .3, weight_2 = .8), 
               pattern = "sum to 1.1")

expect_error(jsdivergence_shift(text_1, text_2, weight_1 = 0, weight_2 = 1))
expect_error(jsdivergence_shift(text_1, text_2, weight_1 = -0.5, weight_2 = 1.5))

# test normalization
expect_error(jsdivergence_shift(text_1, text_2, normalization = "blah"))


# test weighted_avg_shift        
expect_error(weighted_avg_shift(text_1, text_2, type2score_1, type2score_2))
expect_message(weighted_avg_shift(text_1, text_2), 
             pattern = "handle_missing_scores")


# test stop_lens
expect_error(weighted_avg_shift(text_1, text_2, stop_lens = "a"))
expect_error(weighted_avg_shift(text_1, text_2, stop_lens = 1))
expect_error(weighted_avg_shift(text_1, text_2, stop_lens = c(2, 1)))
expect_message(weighted_avg_shift(text_1, text_2, stop_lens = c(2, 1)))

# test stop_words
expect_error(weighted_avg_shift(text_1, text_2, stop_words = data.frame(word = "stopword")))

