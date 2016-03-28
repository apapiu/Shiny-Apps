library(ggvis)
library(polynom)


n = 50
q = 10
s = 0.5


visoverfit <- function(n,q,s) {
x <- runif(n, min = -1, max = 1)

epsi <- rnorm(n) #noise
poly <- polynom::polynomial(rnorm(n = q+1)) #poly
y <- predict(poly, x) + sqrt(s)*epsi #values of poly +noise

df <- data.frame(x, y)

test <- seq(-1, 1, by = 0.01)
df2 <- data.frame(a = test, b = predict(poly, test))

ggvis(df, ~x, ~y)%>% 
layer_points(size := 40, fillOpacity := 0.8) %>% 
    layer_paths(~a, ~b, data = df2, strokeWidth := 4) %>% 
    layer_smooths(span = input_slider(0.1, 2, value = 1), 
                  stroke := "red", strokeWidth := 5)
}


visoverfit(100, 5, 1)
 
