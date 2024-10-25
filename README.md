# DataAnalytics2024_Jake_Lorenzo_Assignment_3

Section 1 - Part A
From running the summary statistics for Covid-19 in 2020, we get a median of 513 cases and a mean of 2724 cases compared to the 2021 Covid-19 dataset with a median of 2787 cases and a mean of 11,027 cases. Additionally, a median of 10 deaths and mean of 73.99 deaths in 2020 compared to a median of 51 deaths and a mean of 189.7 deaths in 2021. This shows that the amount of cases and deaths drastically increased from 2020 to 2021. We can also see from the high maximum values for both dataset in comparison to the median/mean that there is a quite a lot of variability in the data which could indicate more severe outbreaks in certain regions of the country as well as potential outliers that could impact data analysis.

Section 1 - Part B

Still need to overlay normal distribution lines over the histograms

Section 1 - Part C

After changing the data to log base 10 form the ECDF plots for Covid-19 cases in 2020 and 2021 look more similar to the normal distribution. However in the ECDF plots for deaths from 2020 and 2021 we can see gaps in the data towards the lower left quadrant of the plot which could indicate a smaller number of data points in that region or larger intervals between data points. Looking at the Q-Q plots comparing Covid-19 cases from 2020 to 2021 we can see that there were many more cases in 2021 compared to 2020 which skewed the plot upwards. We can also see some outlier values as well as some potential clusters within the data. Similar with the Q-Q plot comparing Covid-19 deaths from 2020 to 2021 we can see it is skewed upwards which indicates many more Covid-19 deaths in 2021 than 2020. We can also see some outliers and clustering that could impact analysis.

Section 2

I filtered the two datasets by state (California, Florida, Virginia, Washington, and Texas). The boxplots comparing the two datasets show similar results as the original datasets with many more Covid-19 cases and deaths in 2021 than 2020. We can also see from the summary statistics that the high maximum value is much larger than the median/mean values for the dataset which shows some variablity in the data and potential outliers.

Overlay normal distribution over histograms before answering 2b.

The ECDF models for this subset of data were similar to the original dataset with the plots looking more alike to the normal distribution after converting the data to log base 10 form. The plots show more cases and deaths in 2021 than 2020 as well as having similar breaks in the data when looking at the ECDF plots for deaths in 2020 and 2021. This could indicate lack of data points or larger intervals between data points. The Q-Q plots were very similar to the original dataset with the data points skewed upwards from the larger number of cases and deaths in 2021. We can also see evidence of outliers and clustering, especially towards the tails of the distribution.

Section 3

Looking at the NY housing dataset I could immediately see a large amount of outliers and variability in the data. To try and combat this I preprocessed the data as best I could to remove those outliers. I also transformed the PRICE and PROPERTYSQFT columns to log base 10 form to remove variability. I fit the first iteration of the linear model to the data and created plots and summary statistics. The model I created had somewhat large values for residuals as well as the R-squared value ~(0.5054) which means that there were still outliers affecting the model. From this I created a dataframe that held all the rows in the dataframe that had residuals more than two standard deviations from the center of the data and removed those rows from the dataset. I refit the linear model and the results were much more promising with smaller residual vales and a larger R-squared value. I made sure when removing the rows with larger residuals to not remove too much data and leave the dataset with fewer points to calculate off of. I removed about 18% of the rows from the dataset after all my preprocessing. After this I created a scatterplot looking at the relationship between PRICE and PROPERTYSQFT and while the plot looked more like the normal distribution compared to the first iteration of the model, it still showed outliers that were affecting the accuracy of the model. For a more accurate model, more data processing would be required to get rid of all outliers that impact the model. The variables that most impact PRICE is PROPERTYSQFT and BATH.

Section 3 - Part B

I reran the above linear model with a subset of the data with PRICE > $5,000,000. After the first iteration of the model I did similar data processing as above with creating a dataset to hold the rows with large residuals (greater than two std. dev.) and removed them from the dataset. This model gave similar results as the model above with PROPERTYSQFT as the most impactful variable. However, there were more outliers and variability in this dataset due to less preprocessing before fitting the model. This led to slightly higher residual values and a slightly lower R-Squared. To better fit the model more data processing for outlier would be required. Looking at the scatterplot, the data is much more spread out than the original dataset which backs up the summary statistics showing higher variability.


