require(extrafont)
#font_import()
loadfonts()


filename <- "test.fig5.pdf"
pdf(
  file = filename,
  width = 6,
  height = 6,
  family = "Garamond")

B <- 3
gama <- 1
eq = function(x)(B-x^(-gama))
par(mar = c(2,3,2,3))
x <- seq(0,10, 0.01)
plot(x, eq(x), type='l', ylim = c(-2,4),
     xlim = c(0,2), axes = FALSE, xlab = "", ylab = "", lwd = 2)
lines(c(0,6), c(B,B), lty = "F6")
axis(1, pos=0, labels = FALSE,lwd.tick=0, outer = TRUE )
axis(2, pos = 0,lwd.tick=0, labels = c("0", "B"), at = c(0,B), las = 2)
axis(2, pos = 0, lwd.tick=0, labels=FALSE)
xx <- uniroot(eq, c(0,10))[[1]]
points(xx,0, pch = 19, cex = .7)
abline(a = 0, b = 2.27, lty = "F6")
eq(1)
points(.67,eq(.67), pch = 19, cex = .7)
axis(1, pos=0, labels = c(expression(C^S), 
                          expression(C^0),
                          expression({C^{"00"}}[1]), 
                          expression(hat(C)),
                          expression({C^{"00"}}[2])),
     at = c(xx, .45, .55, .67, 1.31), cex.axis = 0.8 )
text(1.5, 2, "U(C)")
text(-.08, 4, "U(C)", xpd = TRUE)
text(2.1, 0, "C", xpd = TRUE)
dev.off()


g <- function(x) {}
body(g) <- D(body(eq), 'x')
g(1)
curve(g, add = TRUE)


