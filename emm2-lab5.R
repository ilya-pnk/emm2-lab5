
set.seed(123) 
N = 1000 # Количество шагов
delta = 0.0001 # Шаг дискретизации
t = seq(0, N * delta, by = delta) # Временная шкала

#  броуновское движение
eps = rnorm(N, mean = 0,  sqrt(delta)) # Гауссовский шум
B = cumsum(c(0, eps)) # Интегральная сумма (B0 = 0)

# Проверка характеристик
E_Bt =mean(B) # Математическое ожидание
Var_Bt = var(B) # Дисперсия

cat("Математическое ожидание процесса B(t):", E_Bt, "\n")
cat("Дисперсия процесса B(t):", Var_Bt, "\n")

# График броуновского движения
plot(t, B, type = "l", col = "blue", lwd = 2,
     main = "Броуновское движение", xlab = "Время", ylab = "B(t)")
grid()



# Моделирование ансамбля реализаций
num =200 # Количество реализаций
realizations = matrix(0, nrow = num, ncol = N + 1)

for (i in 1:num) {
  eps <- rnorm(N, mean = 0,  sqrt(delta))
  realizations[i, ] <- cumsum(c(0, eps))
}

# Построение всех траекторий на одном графике
matplot(t, t(realizations), type = "l", col = rainbow(num, alpha = 0.4),
        lty = 1, main = "Ансамбль реализаций броуновского движения",
        xlab = "Время", ylab = "B(t)")
grid()



# Вычисление границ по правилу трех сигм
upend = 3 * sqrt(t)
down_end = -3 * sqrt(t)


lines(t, upend, col = "red", lwd = 2, lty = 2)
lines(t, down_end, col = "red", lwd = 2, lty = 2)
legend("topleft", legend =  "Границы 3σ", col =  "red", lty =  2)




# Параметры геометрического броуновского движения
S0 = 1 # Начальная цена
mu = 0.5 # Дрейф
sigma = 0.9 # Волатильность

# Моделирование геометрического броуновского движения
S = S0 * exp((mu - 0.5 * sigma^2) * t + sigma * B)

# График
plot(t, S, type = "l", col = "darkgreen", lwd = 2,
     main = "Геометрическое броуновское движение", xlab = "Время", ylab = "S(t)")
grid()



# Моделирование ансамбля
geom_realizations = matrix(0, nrow = num, ncol = N + 1)

for (i in 1:num) {
  eps <- rnorm(N, mean = 0,  sqrt(delta))
  B_geom <- cumsum(c(0, eps)) # Броуновское движение
  geom_realizations[i, ] <- S0 * exp((mu - 0.5 * sigma^2) * t + sigma * B_geom)
}
# границы процесса
geom_up= S0*exp((mu - 0.5 * sigma^2) * t + 3 * sigma *sqrt(t))
geom_down=S0*exp((mu - 0.5 * sigma^2) * t - 3 * sigma *sqrt(t))
# Построение всех траекторий на одном графике
matplot(t, t(geom_realizations), type = "l", col = rainbow(num, alpha = 0.5),
        lty = 1, main = "Ансамбль геометрического броуновского движения",
        xlab = "Время", ylab = "S(t)")
lines(t, geom_up, col = "red", lwd = 2, lty =2)
lines(t, geom_down, col = "red", lwd = 2 , lty = 2)
