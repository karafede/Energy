dt <- data.frame(x=c(1:200),y=rnorm(200))
dens <- density(dt$y)
df <- data.frame(x=dens$x, y=dens$y)
probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
quantiles <- quantile(dt$y, prob=probs)
df$quant <- factor(findInterval(df$x,quantiles))
ggplot(df, aes(x,y)) + geom_line() + geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + scale_x_continuous(breaks=quantiles) + scale_fill_brewer(guide="none")

ggplot(df, aes(x,y)) + geom_line() + scale_x_continuous(breaks=quantiles)
