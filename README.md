# Movielens Movie Recommendation System Handbook

This is a movie recommendation system built on the movielends dataset. Dataset can be downloaded from below:

[Movielens 100k dataset](http://grouplens.org/datasets/movielens/100k/)  
[Movielens 10m dataset](http://grouplens.org/datasets/movielens/10m/)  

### About dataset:  
MovieLens data sets were collected by the GroupLens Research Project  

This data set consists of:
- 100,000 ratings (1-5) from 943 users on 1682 movies.  
- Each user has rated at least 20 movies.  
- Simple demographic info for the users (age, gender, occupation, zip)  

More details you can find on [movielens website](http://files.grouplens.org/datasets/movielens/ml-100k-README.txt).  

### Approach:
Here I have followed following steps to achieve results:  

### Data cleaning:
- There are some outliers in the data, in order to improve the model I have first cleaned the data.  
- Some of the users have only few movie in common, so in this case we can give good predictions. I have removed these users in order to improve model.   

### Distance measure(matrix)
- I have used 3 distance measure in order to find correlation  
  1. Euclidean distance
  2. Manhatten distance
  3. Lmax distance
- Datasets are already splitted for 5 fold cross validaiton. So, I have generated 15 distance matrix for each fold and each distance measure.(5 X 3 = 15 distance matrix) 
- In order to predict rating, I have followed this formula:
- For each distance matrix I have chosen a k value of 15,25,35. (15 x 3 = 45 combinations)  
- Apply each Distance matrix with different k on each test data u1.test to u5.test and store results  
- Merge all test partition with predicated value as a single test data frame(100k records) for each algorithm. At this stage I have 9 predicated test data results (3(distance matrix) X 3(K values) = 9 combos)  
- Find a MAD by taking a mean difference of all actual ratings and predicted ratings. [We will get different MAD for all 9 test data]  
- Atlast, Find a naive model's MAD. 



