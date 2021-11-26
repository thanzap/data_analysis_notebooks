#Libraries to be imported
import matplotlib.pyplot as plt
import seaborn as sns

#Scatterplot
sns.scatterplot(x=..., y=...)
plt.show()

#Countplot for a categorical variable from lists
sns.countplot(x=...)
plt.show()

#Countplot for a categorical variable from DataFrame
sns.countplot(x="...",data=...)
plt.show()

#Hue with countplot
sns.countplot(x="smoker",data=df,hue="sex")
plt.show()

#Scatter plot with 2 numeric variables and 1 categorical with order and colors 
hue_colors={"Yes":"black", "No": "red"}
sns.scatterplot(x="..", y="...",data=df, hue="...",palette=hue_colors, hue_order=["...","..."])
plt.show()

#relplot lets you create subplots in a single figure
sns.relplot(x="...",y="...",data=df,kind="scatter",col="smoker",row="time")
plt.show()

#Changing the size of scatter plot points
# Import Matplotlib and Seaborn
import matplotlib.pyplot as plt
import seaborn as sns
# Create scatter plot of horsepower vs. mpg
sns.relplot(x="horsepower", y="mpg", data=mpg,kind="scatter", size="cylinders")
# Show plot
plt.show()

#Changing the style of scatter plot points
# Import Matplotlib and Seaborn
import matplotlib.pyplot as plt
import seaborn as sns
# Create a scatter plot of acceleration vs. mpg
sns.relplot(x="acceleration",y="mpg", data=mpg, kind="scatter", hue="origin",style="origin")
# Show plot
plt.show()

#Lineplots used to track the same thing over time
sns.relplot(x="..",y="...",data=df,kind="line",style="..", hue="...",dashes="...",ci="sd"#for shade of distribution of values)
plt.show()
#example
# Import Matplotlib and Seaborn
import matplotlib.pyplot as plt
import seaborn as sns
# Add markers and make each line have the same style
sns.relplot(x="model_year", y="horsepower", 
            data=mpg, kind="line", 
            ci=None, style="origin", 
            hue="origin",markers=True, dashes=False)
# Show plot
plt.show()

#Count plots and bar plots for categorical variables
#catplot() is used to crteate categorical plots 
sns.catplot(x="..",data=df, kind="count")
plt.show()
#changning the order of categories
category_order=["No answer","Not at all","Not very","Somewhat", "Very"]
sns.catplot(x="...",data=df, kind="count",order=category_order)
plt.show()

#Barplots displays the mean of quantitative variable per category
sns.catplot(x="categorical",y="quantitative",data=df,kind="bar")
plt.show()
#example
# Turn off the confidence intervals
sns.catplot(x="study_time", y="G3",
            data=student_data,
            kind="bar",
            order=["<2 hours", 
                   "2 to 5 hours", 
                   "5 to 10 hours", 
                   ">10 hours"],ci=None)
# Show plot
plt.show()

#Boxplot shows the distribution of quantitative data the colored box represents 25th-75th percentile and the line=median
sns.catplot(x="..",y="..",data=df, kind="box",sym="",whis=[5,95])#sym is Omitting the outliers, whis adjust the lower and upper percentile
plt.show()
#example
# Set the whiskers at the min and max values
sns.catplot(x="romantic", y="G3",
            data=student_data,
            kind="box",
            whis=[0,100])
# Show plot
plt.show()

#Point plots- Ponts show mean of quantitative variables- Vertical lines show 95% confidence intervals- One is categorical variable
sns.catplot(x="..",y="..",data=df, hue="..", kind="point",join=False,estimator=median) #Join removes line between means- estimator defines which measure to display
plt.show()

#Customization of plots
#Figure style includes background and axis
sns.set_style("whitegrid"/"ticks"/"dark"/"darkgrid") 
#Figure palette changes the color of the main elements of the plot
sns.set_palette("RdBu"/"Greys"/["or list of colors"])
#Figure contect changes the scale of the plot elements and labels
sns.set_context("talk"/"paper"/"notebook"/"poster")

#Adding title and labels
#seaborn plots create 2 different object FacetGrid(creates also subplots relplot,catplot) and AxesSubplot(single plots scatterplot,countplot.. )
#Title
g.fig.suptitle("New title", y=1.03) #FacetGrid
g.set_title("New title", y=1.03) #AxesSubplot

#Title for subplots
g.set_titles("This is {col_name}")

#Adding axis labels
g.set(xlabel="New X label", ylabel="New Y label")

#Roatating x-axis tick labels 
plt.xticks(rotation=90)



