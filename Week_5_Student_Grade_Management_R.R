# WEEK 5 PROJECT: STUDENT GRADE MANAGEMENT SYSTEM
# Programming Language: R

cat("==============================================\n")
cat("       STUDENT GRADE MANAGEMENT SYSTEM\n")
cat("==============================================\n")

# 1. Read student records into a data frame
cat("\n1. STUDENT RECORDS\n")

if (file.exists("students.csv")) {
  students <- read.csv("students.csv", stringsAsFactors = FALSE)
} else {
  students <- data.frame(
    Name = c("Anu", "Ravi", "Meena", "Kumar"),
    Age = c(19, 20, 18, 21),
    Score = c(85, 72, 91, 64),
    Grade = c("A", "B", "A", "C"),
    stringsAsFactors = FALSE
  )
  cat("students.csv not found. Using sample student data:\n")
}

print(students)

# 2. Create a frequency table of grades
cat("\n2. GRADE FREQUENCY TABLE\n")
if ("Grade" %in% names(students)) {
  print(table(students$Grade))
} else {
  cat("The data frame does not contain a Grade column.\n")
}

# 3. Convert grades to a factor with custom levels
cat("\n3. GRADE FACTOR\n")
if ("Grade" %in% names(students)) {
  grade_levels <- c("A", "B", "C", "D", "F")
  students$GradeFactor <- factor(
    students$Grade,
    levels = grade_levels,
    ordered = TRUE
  )
  print(students[, c("Name", "GradeFactor"), drop = FALSE])

  unknown_grades <- unique(students$Grade[is.na(students$GradeFactor)])
  if (length(unknown_grades) > 0) {
    cat("Grades not included in the custom levels:",
        paste(unknown_grades, collapse = ", "), "\n")
  }
}

# 4. Perform mathematical operations on columns
cat("\n4. CALCULATED COLUMNS\n")
if (all(c("Age", "Score") %in% names(students))) {
  students$LogScore <- log(pmax(as.numeric(students$Score), 1))
  students$AgeSquareRoot <- sqrt(pmax(as.numeric(students$Age), 0))
  students$AgeCeil <- ceiling(as.numeric(students$Age))
  students$AgeFloor <- floor(as.numeric(students$Age))
  students$ScoreRound <- round(as.numeric(students$Score), 0)
  print(students)
} else {
  cat("The CSV file must contain Age and Score columns for these calculations.\n")
}

# 5. Demonstrate deleting a row and a column
cat("\n5. DELETE A ROW AND A COLUMN\n")
cat("Original data frame:\n")
print(students)

if (nrow(students) >= 2) {
  data_without_second_row <- students[-2, , drop = FALSE]
  cat("After deleting the second row:\n")
  print(data_without_second_row)
}

if ("Age" %in% names(students)) {
  data_without_age <- students[, names(students) != "Age", drop = FALSE]
  cat("After deleting the Age column:\n")
  print(data_without_age)
}

cat("\nWeek 5 project completed successfully.\n")
