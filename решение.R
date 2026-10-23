# Лабораторная работа 2. Оптимизация выпуска PoE-коммутаторов.
# Запуск из терминала: Rscript решение.R
# В RStudio откройте папку репозитория и выполните:
# source("решение.R", encoding = "UTF-8")
options(stringsAsFactors = FALSE, digits = 12)
if (!requireNamespace("boot", quietly = TRUE)) {
  stop('Нужен пакет boot. Выполните install.packages("boot") и повторите запуск.')
}
args <- commandArgs(trailingOnly = FALSE)
script_arg <- grep("^--file=", args, value = TRUE)
if (length(script_arg)) {
  setwd(dirname(normalizePath(sub("^--file=", "", script_arg[1]))))
}
products <- read.csv2("данные/продукция.csv", fileEncoding = "UTF-8-BOM")
resources <- read.csv2("данные/ресурсы.csv", fileEncoding = "UTF-8-BOM")
ids <- products$Модель
A <- as.matrix(resources[, ids])
storage.mode(A) <- "double"
b <- resources$Запас
c <- products$Цена_руб - products$Затраты_руб
stopifnot(all(is.finite(A)), all(A >= 0), all(is.finite(b)), all(b >= 0),
          all(is.finite(c)), ncol(A) == length(c), !anyDuplicated(ids))

# Основное решение вычисляется симплекс-методом.
fit <- boot::simplex(a = c, A1 = A, b1 = b,
                     maxi = TRUE, n.iter = 1000, eps = 1e-9)
stopifnot(fit$solved == 1)
x <- as.numeric(fit$soln)
used <- as.vector(A %*% x)
slack <- b - used
value <- sum(c * x)
stopifnot(all(x >= -1e-7), all(slack >= -1e-7),
          abs(value - fit$value) < 1e-5)

# Сертификат оптимальности исходной модели проверяется после решения.
# При замене исходных данных двойственный вектор требуется пересчитать.
y <- c(0, 0, 1000, 2000, 1000, 0, 0, 0)
stopifnot(all(y >= 0), all(as.vector(t(A) %*% y) >= c - 1e-7),
          abs(sum(b * y) - value) < 1e-5,
          max(abs(y * slack)) < 1e-5)

plan <- data.frame(Модель = ids, Выпуск_шт = x,
                   Маржа_руб = c, Вклад_руб = c * x)
usage <- data.frame(Ресурс = resources$Ресурс, Единица = resources$Единица,
                    Расход = used, Запас = b, Остаток = slack,
                    Загрузка = used / b, Теневая_цена_руб = y)
dir.create("результаты", showWarnings = FALSE)
write.csv2(plan, "результаты/план.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv2(usage, "результаты/ресурсы.csv", row.names = FALSE, fileEncoding = "UTF-8")

# Каждый сценарий решается заново, без подстановки теневой цены.
scenario_names <- c("Исходный план", "Сборка: +2 чел.-ч",
                    "Нагрузочная проверка: +10 стендо-ч",
                    "Температурные испытания: +5 место-ч",
                    "Блоки PoE: +10 шт.")
row_ids <- c(NA, 3, 4, 5, 1)
deltas <- c(0, 2, 10, 5, 10)
scenarios <- do.call(rbind, lapply(seq_along(scenario_names), function(i) {
  bi <- b
  if (!is.na(row_ids[i])) bi[row_ids[i]] <- bi[row_ids[i]] + deltas[i]
  si <- boot::simplex(a = c, A1 = A, b1 = bi, maxi = TRUE,
                      n.iter = 1000, eps = 1e-9)
  stopifnot(si$solved == 1, all(si$soln >= -1e-7),
            all(A %*% si$soln <= bi + 1e-7))
  data.frame(Сценарий = scenario_names[i], P4 = si$soln[1],
             P8 = si$soln[2], P8H = si$soln[3],
             Маржинальная_прибыль_руб = sum(c * si$soln),
             Прирост_руб = sum(c * si$soln) - value)
}))
write.csv2(scenarios, "результаты/сценарии.csv", row.names = FALSE,
           fileEncoding = "UTF-8")

# Протокол на русском языке, включая подписи к сведениям о среде.
sink("результаты/протокол_R.txt")
cat("РЕШЕНИЕ ЗАДАЧИ СИМПЛЕКС-МЕТОДОМ В R\n\n")
cat("Статус: оптимальное решение найдено (solved = ", fit$solved, ").\n", sep = "")
cat("Метод: boot::simplex, максимизация.\n")
cat("\nПроизводственный план\n"); print(plan, row.names = FALSE)
cat("\nИспользование ресурсов\n"); print(usage, row.names = FALSE)
cat("\nСценарии изменения ресурсов\n"); print(scenarios, row.names = FALSE)
cat("\nМаржинальная прибыль, руб.: ", value, "\n", sep = "")
cat("Двойственная оценка, руб.: ", sum(b * y), "\n", sep = "")
cat("Максимальное нарушение ограничений: ", max(c(0, -x, -slack)), "\n", sep = "")
cat("Максимальное отклонение от целого: ", max(abs(x - round(x))), "\n", sep = "")
cat("\nСВЕДЕНИЯ О СРЕДЕ ВЫПОЛНЕНИЯ\n")
cat("Версия R: ", getRversion() |> as.character(), "\n", sep = "")
cat("Версия пакета boot: ", packageVersion("boot") |> as.character(), "\n", sep = "")
cat("Платформа: ", R.version$platform, "\n", sep = "")
cat("Локаль: ", Sys.getlocale(), "\n", sep = "")
sink()
print(plan, row.names = FALSE)
cat("Максимальная маржинальная прибыль:", value, "руб.\n")
