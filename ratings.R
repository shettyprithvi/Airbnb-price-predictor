LA = read.csv('LA.csv')
View(x=LA)
price=LA$Price
rating=LA$Review.Scores.Rating
accuracy=LA$Review.Scores.Accuracy
rate=lm(price~rating)
response=LA$Host.Response.Rate
summary(rate)
resp=lm(price~response)
summary(resp)

LA2=subset(LA, LA$Host.Response.Rate!=0)
nrate=lm(LA2$Price~LA2$Host.Response.Rate)
plot(nrate)
View(LA2)
summary(nrate)

cor.test(cleaned, price, method=c("pearson"))
cor(cleaned,price,  method = "pearson", use = "complete.obs")

bedrooms=LA$Bedrooms
if (bedrooms>0) {
  nprice=price/bedrooms
} else {
  nprice=price
}
#Normalized prices

cleaned=LA$Review.Scores.Cleanliness
cleanrate=lm(price~cleaned)
summary(cleanrate)


install.packages("UsingR")
install.packages('colorspace')
library(UsingR)

ggplot(LA, aes(y=price, x=Review.Scores.Cleanliness)) + geom_point(size=1, alpha=0.6, color="blue") + ylab("Price of the room(in $)") + xlab("Cleanliness Review Rating of the room") + ggtitle("Los Angeles: Cleanliness rating vs Price correlation")

ggplot(LA, aes(y=nprice, x=Review.Scores.Rating)) + geom_point(size=1, alpha=0.6) + ylab("Adjusted Normalized Price of the room(in $)") + xlab("Review Rating of the room") + ggtitle("Los Angeles: Ratings vs Price correlation")

plot(y=LA2$Price, x=LA2$Host.Response.Rate,ylab="Normalized price(in $)", xlab="Response rate(in %)")
title('Los Angeles: Response rate vs Price correlation')


