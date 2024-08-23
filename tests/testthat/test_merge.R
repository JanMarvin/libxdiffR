
test_that("test merge three", {

  write_file <- function(x, path, append = FALSE) {
    if (append) {
      x <- c(readLines(path, warn = FALSE), x)
    }
    writeLines(x, path)
  }

  base  <- tempfile(fileext = "-base.txt")
  file1 <- tempfile(fileext = "-file1.txt")
  file2 <- tempfile(fileext = "-file2.txt")

  txt_base   <- '#include <iostream>

void greet() {\    std::cout << "Hello, World!" << std::endl;\n}

int main() {\n    greet();\n    return 0;\n}
'
  txt_file1 <- '#include <iostream>

void greet() {\n    std::cout << "Hello, World!" << std::endl;\n}

void farewell() {\n    std::cout << "Goodbye!" << std::endl;\n}

int main() {\n    greet();\n    farewell();\n    return 0;\n}
'

  txt_file2 <- '#include <iostream>

void greet(std::string name) {\n    std::cout << "Hello, " << name << "!" << std::endl;\n}

int main() {\n    greet("Alice");\n    return 0;\n}
'

  exp <- "#include <iostream>\n\n<<<<<<< Head\nvoid greet() {\n    std::cout << \"Hello, World!\" << std::endl;\n}\n\nvoid farewell() {\n    std::cout << \"Goodbye!\" << std::endl;\n=======\nvoid greet(std::string name) {\n    std::cout << \"Hello, \" << name << \"!\" << std::endl;\n>>>>>>> Incoming\n}\n\nint main() {\n<<<<<<< Head\n    greet();\n    farewell();\n=======\n    greet(\"Alice\");\n>>>>>>> Incoming\n    return 0;\n}"

  write_file(txt_base, base)
  write_file(txt_file1, file1)
  write_file(txt_file2, file2)

  got <- merge3(base, file1, file2)
  expect_equal(exp, got)
})

test_that("merge 3", {

  base <- "a and b\n"
  ours <- "a and b\n and c"
  thrs <- "and now: \na and b\n"

  exp <- "and now: \na and b\n and c"
  got <- merge3(base, ours, thrs)
  expect_equal(exp, got)

})
