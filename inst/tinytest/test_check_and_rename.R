text_1 <- data.frame(word = LETTERS[1:7], freq = 10, stringsAsFactors = FALSE)

# test classes
class(text_1) <- "test.class"
expect_message(shifterator:::check_and_rename(text_1, name_x = "text_1", column_names = c("word", "freq_1")),
               pattern = "data.frames")

# pretend it's a tibble, outcome should be a data.frame
classes <- c("tbl_df", "tbl","data.frame")
class(text_1) <- classes
text_1 <- shifterator:::check_and_rename(text_1, name_x = "text_1", column_names = c("word", "freq_1"))

expect_equal(class(text_1), "data.frame")
expect_equal(names(text_1), c("word", "freq_1"))

# extra column or columns in wrong order
text_1$extra <- c(7:1) 
expect_message(shifterator:::check_and_rename(text_1, name_x = "text_1", column_names = c("word", "freq_1")),
               pattern = "columns")


text_1 <- data.frame(freq = 10, word = LETTERS[1:7], stringsAsFactors = FALSE)
expect_message(shifterator:::check_and_rename(text_1, name_x = "text_1", column_names = c("word", "freq_1")),
               pattern = "words.")