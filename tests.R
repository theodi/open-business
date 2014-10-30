# Did something change in the file structure?
expect_that(column("Primary Field/Industry", x = list), equals(20))

# Correct handling of missings?
expect_that(sum(table(biz[, 3]) / length(na.omit(biz[, 3]))), equals(1))
expect_that(sum(table(list[, 20]) / length(na.omit(list[, 20]))), equals(1))

expect_that(column('Q12_unlimited'), equals(23))
