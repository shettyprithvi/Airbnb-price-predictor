mel = read.csv('Melbourne.csv')
View(x=mel)
price2=mel$Price
rating2=mel$Review.Scores.Rating
accuracy=mel$Review.Scores.Accuracy
rate2=lm(price2~rating2)
response2=mel$Host.Response.Rate
summary(rate2)



bedrooms2=mel$Bedrooms
if (bedrooms2>0) {
  nprice2=price2/bedrooms2
} else {
  nprice2=price2
}
#Normalized prices

install.packages("UsingR")
install.packages('colorspace')
library(UsingR)

ggplot(mel, aes(y=price2, x=Review.Scores.Cleanliness)) + geom_point(size=1, alpha=0.6, color="purple") + ylab("Price of the room(in $)") + xlab("Cleanliness Review Rating of the room") + ggtitle("Melbourne: Cleanliness rating vs Price correlation")

ggplot(mel, aes(y=price2, x=rating2)) + geom_point(size=1, alpha=0.6) + ylab("Adjusted Normalized Price of the room(in $)") + xlab("Review Rating of the room") + ggtitle("Melbourne : Ratings vs Price correlation")


plot(y=nprice2, x=response2,ylab="Normalized price(in $)", xlab="Response rate(in %)")
title('Melbourne: Response rate vs Price correlation')

