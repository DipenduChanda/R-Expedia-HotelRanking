# R-Expedia-HotelRanking
Personalizing hotel search (ranking and sorting) for specific users of an Online Travel Agency (Expedia) using click-through data

Description:
•	In this project we consider hotel search and click-through data provided by the travel website, with the goal of developing a model that will provide a list of hotels ranked by highest likelihood of customer purchase.
•	Data consists user search criteria, static & dynamic hotel characteristics 10M+ records. Missing values and those representing large multiples of the standard deviation of an attribute were replaced by the attribute mean to minimize outlier bias.
•	We have used classification models: Random Forests, Support Vector machines and Logistic Regression. As a conclusion Random Forests classifier gives maximum accuracy in predicting a booking.
•	We have achieved 76% accuracy in predicting a potential booking and interpret the variations in factors that can potentially influence a booking .
•	We have also clustered users based on their characteristics into 4 groups and developed an algorithm that ranks hotels for each group.
