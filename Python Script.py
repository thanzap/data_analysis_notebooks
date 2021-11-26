#Variables Transforming
str()
float()
int()
bool()

#List Creation
a=...
b=...
list1=[a,b]

#List slicing
list[...]
list[...:...]
list[ :...]
list[...: ]

#Example slicing
df.loc["index":"index"]
print(temperatures_srt.loc[("India","Hyderabad"):("Iraq","Baghdad")])
# Subset in both directions at once
print(temperatures_srt.loc[("India","Hyderabad"):("Iraq","Baghdad"),"date":"avg_temp_c"])
#Example both direction slice twice
df.loc[("Labrador","Brown"):("Schnauzer","Grey"),"name":"height_cm"]
#Example of slicing subsetting using index
# Subset for Egypt to India
temp_by_country_city_vs_year.loc["Egypt":"India"]
# Subset for Egypt, Cairo to India, Delhi
temp_by_country_city_vs_year.loc[("Egypt","Cairo"):("India","Delhi")]
# Subset in both directions at once
temp_by_country_city_vs_year.loc[("Egypt","Cairo"):("India","Delhi"),2005:2010]

#slicing by index
# Slice the dataset to keep only 2012
df2012 = df["2012"]

#Slicing iloc
# Get 23rd row, 2nd column (index 22, 1)
print(temperatures.iloc[22,1])
# Use slicing to get the first 5 rows
print(temperatures.iloc[:5])
# Use slicing to get columns 3 to 4
print(temperatures.iloc[:,2:4])
# Use slicing in both directions at once
print(temperatures.iloc[:5, 2:4])




#Values of list calculations
list1=["a","b","c"]
print(list1[1] + list1[3])

#Update specific value of a list
list1[...]=...

#Adding an element to a list
list2=list1+["c",...]

#Removing elements
del(list1[1])

#Copy a list
list1=["a","b","c"]
list2=list(list1)
#OR
list2=list1[:]

#Max function
list1=....
maximum= max(list1)

#Round function
round(...,1)
round(...)

#Length function
len(...)

#Sort function
sorted(list,reverse=True(Descending) or False(Ascending))

#Populate a list with a for loop
#Example
nums=[12,8,21,3,16]
new_nums=[]
for num in nums:
    new_nums.append(num+1)
print(new_nums)

#Upper method
var.upper()

#Count method
var.count("...")
var.count(...)

#Index method
list1.index(...)
list1.index("...")

#Append method
list1.append(...)

#Reverse method
list1.reverse()

#Import packages
from pandas.core import groupby
from pandas.core.algorithms import value_counts
import ... as ...

#Math.pi method in math package
math.pi is a variable of π=3,14 in the math package
radians() converts angle in degrees in angle in radians

#Importing numpy
import numpy as np

#Creating an np.array NUMPY
array=np.array([...,...,"...",...])
#2D
array=np.array([[...,...,...],[...,...,"...",...]])

# Convert a list to a np.array NUMPY
np_var=np.array(var)

#Multiplication of all elements of an array (Πολλαπλασιάζει όλα τα στοιχεία ένα προς ένα με τον αριθμό που βάζουμε) π.χ. Για μετρατροπές NUMPY
np_var2=np_var*...

#Subsetting an numpy array NUMPY
var2=var<...
var2=var[var>...] #TRUE OR FALSE
print(var2[var]) #ALL ELEMENTS 
print(var[...]) #Specific value
array[ROW][COLUMN] or array[row,column] #Specific value in 2D array
array[:,1:3] #All rows specfific columns
array[1,:]# Specific Row all columns

#Mean,median,correlation,standard deviation function NUMPY
np.mean(array[...,...]) #Specific row or column with :
np.median(array[...,...]) #Specific row or column with :
np.corrcoef(array[:,...], arrray[:,...]) #Correlation between 2 columns
np.std(array[:,...]) #Standard deviation of a column

#Filter array NUMPY
subarray=array[array2=="..."]
subarray=array[array2!="..."]

#Matplotlib import MATPLOTLIB
import matplotlib.pyplot as plt

#Line chart if we have in x axis a time scale variable MATPLOTLIB
plt.plot(x,y)
plt.show()

#Line chart of dataframe columns
plt.plot(df["column"],df["column"])
plt.show()

#Multiple line chart
plt.plot(df["column"],df["column"])
plt.plot(df1["column"],df1["column"])
plt.show()

#Scatter plot if we need to see if there is a correlation between two variables MATPLOTLIB
plt.scatter(x,y,s=column,c=col,alpha=0 to 1) #x=Xaxis,y=Yaxis,s=size of dots,c=colour,alpha=the opacity of dots
plt.show()

#Scatter plot
plt.scatter(df.column,df.column2,color="green",marker='s',alpha=0,1/:1)
plt.xlabel("...")groupby
plt.ylabel("...") 
plt.show()
#Logarithmic scale of x or y axis MATPLOTLIB
plt.x-yscale("log")

#Histogram get idea about distribution MATPLOTLIB
help(plt.hist)
plt.hist(column,density=True, bins=...,range=(...,...)) #Density Normalize the data
plt.show()

#Histogram of a column 
df["column"].hist(bins=...)
plt.show()

#Barplots
df.plot(kind="bar",title="...")
plt.show()

#Lineplots
df.plot(x="column",y="column2",kind="line",rot=45)
plt.show()

# scatterplots
df.plot(x="column",y="column2",kind="scatter")
plt.show()

# Layering plots
df[df["sex"]=="F"]["column2"].hist(alpha=0-1)
df[df["sex"]=="M"]["column2"].hist(alpha=0-1)
plt.legend(["F","M"])
plt.show()



#AXIS labels- Title- Ticks- legend- text -Line Chart-Line plot
plt.plot(x,y,color="...",linewidth=1/2/3,linestyle="-"/"--"/"-."/":",marker="x/s/o/d/*/h")
plt.xlabel("...")
plt.ylabel("...")
plt.title("...",fontsize=20, color="green")
plt.yticks([...,...,....,...],["...","...","...","..."]) #Rename Y axis
plt.xticks([...,...,...,...],["...","...","...","..."]) #Rename X axis
plt.grid(True) #Add grid 
plt.legend(color="green")
plt.text(xcoor,ycoord,"Text message")
plt.style.use("fivethirtyeight/ggplot/seaborn/default")
plt.show()

#Bar chart
plt.bar(df.column,df.column2,yerr=df.error) #ERROR  BARS
plt.ylabel("...")
plt.show()

#Stacked Bar
plt.bar(df.column,df.column2,label="...")
plt.bar(df.column, df.column3,bottom=df.column2,label="...")
plt.legend()
plt.show()

#Dictionaries
dictionary={"key":value,"key":value} #Create a dictionary
#OR
dictionary={"key":[value,value2],"key":list1} #Create a dictionary
dictionary["key"]#Access to a specific value of a key
dictionary.keys() #Returns all keys of dictionary
dictionary["new key"]=new Value # Addition of a new element in the dictionary or Update of an element
"key" in dictionary #Check if the specific key is in the dictionary
del(dictionary["key"]) #Remove an element from a dictionary
print(dictionary["key"]["value"])
dictionary["value"]#Access to a key

#Pandas package 
import pandas as pd

#Creating a dataframe from a dictionary PANDAS
df=pd.DataFrame(dictionary)
df.index=["...","...","..."] #Put specific index to the rows instead of 0,1,2... which is the default
df.index=listofwantedindexes

#Load pickled file
import pandas as pd
x = pd.to_dict('data.pkl')
x = pd.read_pickle('data.pkl')
print(type(x))


#Import dataframe from a csv file PANDAS
df=pd.read_csv("path/to/df.csv", index_col=0)

#Creating Dataframes list of dictionaries by row
list_of_dicts=[
    {"...":"....","...":"..."}# first row key is column value is observation
    {"...":"....","...":"..."}# second row key is column value is observation2
]
df_dics=pd.DataFrame(list_of_dicts)
#Creating Dataframe dictionary of list by column
list_of_dicts=[
    {"column":["...","..."],"column2":["....","..."],"column3"["...","..."}# specify the values in column order
]
df_dics=pd.DataFrame(list_of_dicts)

#Datafrane to csv
df.to_csv("... .csv")

# Merging merge dataframes inner join one to one 
df2=df.merge(df1,on="column",suffixes=("_df","_df1"))

# Merging merge multiple dataframes
grants_licenses_ward=grants.merge(licenses,on=["address","zip"]) \
    .merge(wards,on="ward",suffixes=("_bus","_ward"))

# Merging mere with left join
df2=df.merge(df1,on="column",how="left",suffixes=("_df","_df1"))

# Merging merge with right join
tv_movies= movies.merge(tv_genre,how="right", left_on="id",right_on="movie_id")
#Example
# Merge action_movies to scifi_movies with right join
action_scifi = action_movies.merge(scifi_movies, on='movie_id', how='right',
                                   suffixes=("_act","_sci"))

# Merging merge with outer join
tv_movies= movies.merge(tv_genre,on="ward",how="outer", suffixes=("...","..."))

# Merging merge with anti-join
empl_cust=employees.merge(top_cust,on="same column",how="left",indicator=True)
srid_list=empl_cust.loc[empl_cust["_merge"]=="left_only","srid"]
print(employees[employees["srid"].isin(srid_list)])

#Concatenate concatenate two dataframes all the columns
pd.concat([inv_jan,inv_feb,inv_mar],ignore_index=True) #vertically
pd.concat([inv_jan,inv_feb,inv_mar],axis=1)
#Concatenate concatenate two dataframes only the columns that have in common
pd.concat([inv_jan,inv_feb],join="inner")

# Append append the dataframes
inv_jan.append([inv_feb,inv_mar],ignore_index=True,sort=True)

#Merge merge_ordered()
pd.merge_ordered(df,df1)





# count values of a categorical column of a dataset  
df["column"].value_counts()

#Type of object PANDAS
type(df["column"])

#Index and select data PANDAS
#LOC IS BASED ON LABELS ILOC BASED ON POSITION
df["column"] #you get a pandas series type
df[["column"]] #you get a dataframe type
df[["column","column2"]]#Multiple columns as a dataframe
df[...:...] #Select specific rows
df.loc[["index"]] #Select a specific row 
#OR rows
df.loc[["index","index2"]]
df.loc[["index","index2"],["column","column2"]] #Select many rows and many columns
df.loc[:,["column","column2"]] #Select all rows and specific columns
df.iloc[[1,2,3]] #Select rows and all columns based on index number
df.iloc[[1,2,3],[0,1]] #Select many rows and many columns based on index number
df.iloc[:,[0,1]] #Select all rows and specific columns

#Comparison operators
#array comparison
array=np.array([...,...,...])
array2=np.array([...,...,...])
array>10
array>array2

#Boolean operators
np.logical_and/or/not(array>..., array<...) #Print out True or False
array[np.logical_and(array>..., array<...)] #Print out which elements are true

#if,elif,else
if condition :   #General if,else,elif form
    expression
else:
    expression
elif:
    expression

x=...   #Example
if (x % 2==0) : #Check if the variable is odd or even
    print("checking" + str(x))
    print("x is even")
else:
    print("x is odd")

#Example 2 
x=...
if (x % 2==0):
    print("x is divisible by 2")
elif (x % 3==0):
    print("x is divisible by 3")
else:
    print("x is neither divisible by 2 nor by 3")

#Filtering Dataframes
df.head() #First 5 rows 
df.tail() #Last 5 rows
df.info #Number of rows, column names, data types of every column
df.describe() #Quick overview of numeric variables
df.values
df.columns
df.index
df.sort_values("column","column2",ascending=False/True) #Sorting by one column or two
df.count() #prints the number of values of each column

#Subsetting as a series-Selecting columns
df=...
df1=df["column"] #OR
df1=df.column #(without spaces or special characters)
#Subset the dataframe based on the column we chose from above
df2=df[df1]
#OR the same process is made by one-line code
df1=df[df["column"]]
#Subsetting multiple columns
df2=df[["column","column2"]]
#Subsetting based on text data
df[df["column"]=="..."]
#Subsetting based on the date
df[df["column"]>"date"]
#Subsetting based on multiple conditions
df1=df["column"]=="..."
df2=df["column"]=="..."
df[df1 & df2] #OR IN ONE LINE
df[ (df["column"]=="...") & (df["column"]=="...")]
#Get filtered column with comparison operator
df1=df["column"]</>/== #True or False
df1=df[df["column"]</>/==...] #Getting values
#Missing misssing values
df.isna()#True or False missing values Nan NA in every row
df.isna().any() # missing values in columns True or False
df.isna().sum() # missing value count in each column
df.isna().sum().plot(kind="bar") # Visualization of missing values in each column
plt.show()
df.dropna() #Drops Remove the missing values
df.fillna(0) # Replace missing values with 0
df.replace('?',np.nan) #Replace ? with Nan
df.fillna(df.mean(),inplace=True) # Replace Nan values with the mean of each column
df.isnull().sum() # Count the number of NaNs in the dataset to verify

#Impute the missing values in the non-numeric columns.
# Iterate over each column of cc_apps
for col in df.columns:
    # Check if the column is of object type
    if df[col].dtypes == 'object':
        # Impute with the most frequent value
        df = df.fillna(df[col].value_counts().index[0])
# Count the number of NaNs in the dataset and print the counts to verify
print(df.isnull().sum())

#From object to integer
for col in df.columns :
    # Only modify columns that have the "<" sign
    if "<" in df[col].to_string():
        df[col]=df[col].str.replace("<",'')
        df[col]=pd.to_numeric(df[col])



#Transform the non-numeric data into numeric
# Import LabelEncoder
from sklearn.preprocessing import LabelEncoder
# Instantiate LabelEncoder
le=LabelEncoder()

# Iterate over all the values of each column and extract their dtypes
for col in cc_apps.columns.to_numpy():
    # Compare if the dtype is object
    if cc_apps[col].dtypes=='object':
    # Use LabelEncoder to do the numeric transformation
        cc_apps[col]=le.fit_transform(cc_apps[col])


#Subseting with query() method
df.query('value>0') #multiple conditions must be written like this (condition1 & condition2) | condition3

#Create New Column of age_bins Via Defining Bin Edges
df_ages['age_bins'] = pd.cut(x=df_ages['age'], bins=[20, 29, 39, 49])

#Example of missing values
# List the columns with missing values
cols_with_missing = ["small_sold", "large_sold", "xl_sold"]
# Create histograms showing the distributions cols_with_missing
avocados_2016[(cols_with_missing)].hist()
# Show the plot
plt.show()

#Subsetting using .isin()
#Example
is_black_or_brown=dogs["color"].isin(["Black","Brown"])
dogs[is_black_or_brown]

#Example2
# Subset for rows in South Atlantic or Mid-Atlantic regions
south_mid_atlantic = homelessness[homelessness["region"].isin(["South Atlantic","Mid-Atlantic"])]

#Example3
# The Mojave Desert states
canu = ["California", "Arizona", "Nevada", "Utah"]
# Filter for rows in the Mojave Desert states
mojave_homelessness = homelessness[homelessness["state"].isin(canu)]
# See the result
print(mojave_homelessness)
# See the result


#Example4
#Make a list of cities to subset on
cities = ["Moscow", "Saint Petersburg"]
# Subset temperatures using square brackets
print(temperatures[temperatures["city"].isin(cities)])
# Subset temperatures_ind using .loc[]
print(temperatures_ind.loc[["Moscow","Saint Petersburg"]])
#Example5
# Index temperatures by country & city
temperatures_ind = temperatures.set_index(["country","city"])
# List of tuples: Brazil, Rio De Janeiro & Pakistan, Lahore
rows_to_keep = [("Brazil","Rio De Janeiro"),("Pakistan","Lahore")]
# Subset for rows to keep
print(temperatures_ind.loc[rows_to_keep])

#Subset subset slicing and hist plot 
#Example
# Histogram of conventional avg_price 
avocados[avocados["type"]=="conventional"]["avg_price"].hist(alpha=0.5,bins=20)
print(avocados)
# Histogram of organic avg_price
avocados[avocados["type"]=="organic"]["avg_price"].hist(alpha=0.5,bins=20)
# Add a legend
plt.legend(["conventional","organic"])
# Show the plot
plt.show()



#Adding a new column
#Example
df["height_m"]=df["height_cm"] /100
#Example2
df["bmi"]=df["weight_kg"]/df["height_m"]**2
#Example3
# Add total col as sum of individuals and family_members
homelessness["total"]=homelessness["individuals"] + homelessness["family_members"]
# Add p_individuals col as proportion of individuals
homelessness["p_individuals"]= homelessness["individuals"]/homelessness["total"] #Percentage
# See the result
print(homelessness)

#Selecting rows with logic
#Examples of selecting rows 
# Select the dogs where Age is greater than 2
greater_than_2 = mpr[mpr.Age > 2]
print(greater_than_2)
# Select the dogs whose Status is equal to Still Missing
still_missing = mpr[mpr.Status == "Still Missing"]
print(still_missing)
# Select all dogs whose Dog Breed is not equal to Poodle
not_poodle = mpr[mpr["Dog Breed"]!="Poodle"]
print(not_poodle)

# Convert month to type datetime
df["month"]=pd.to_datetime(trends["month"])

#Summary numeric statistics
df["column"].mean()
df["column"].median()
df["column"].min()
df["column"].max()
df["column"].var()
df["column"].sum()
df["column"].max()
df["column"].mode()
df["column"].std()

#.agg() method 
def function(column):
    return column.quantile(0.3)
df["column"].agg(function)

#Cumulative sum
df["column"].cumsum()
df["column"].cummax()
df["column"].cummin()
df["column"].cumprod()

#Summary categorical statistics
df.drop_duplicates(subset=["column","column2"]) #Drop duplicates
df["column"].value_counts(sort=True,normalize=True)# Value counts of each row in the column 
#Summaries group by function
df.groupby("color")["weight_kg"].mean()
df.groupby("color")["weight_kg"].agg([min,max,sum])
df.groupby(["color","breed"])[["weight_kg","height_m"]].mean()
df.groupby(level=0).agg({"column":"mean"})

#Pivot tables
df.pivot_table(values="column",index="column1",aggfunc=[np.mean,np.median])
df.pivot_table(values="column",index="column1",columns="column2",fill_value=0,margins=True)
#Example of pivot tables
# Add a year column to temperatures
temperatures["year"]=temperatures["date"].dt.year
# Pivot avg_temp_c by country and city vs year
temp_by_country_city_vs_year = temperatures.pivot_table("avg_temp_c",index=["country","city"],columns="year")
# See the result
print(temp_by_country_city_vs_year)

#Set a column as the index set index
df_ind=df.set_index("column")
#Reset the index
df_ind=df.reset_index()
df.sort_index()
df.sort_index(level=["column","column1"],ascending=[True,False])

# Slicing / slicing by partial dates
df.loc["date/year":"date2/year"]

#Subsetting subsetting by row/column number
df.iloc[0:3 rows , 1:3 columns]

#Set an index merging on index
movies=pd.read_csv("tmdb_movies.csv", index_col=["column"])

#Merging merging on index
movies_taglines=movies.merge(taglines, on="id",how="left")

#Validating merges
.merge(validate="one_to_one"/"one_to_many"/"many_to_one"/"many_to_many")

#Validating concatenations
.concat(verify_integrity=False)

#While 
while condition:
    expression
#Example
error=50.0
while error>1:
    error=error/4
    print(error)

#Example 2 WHILE COMBINED WITH IF AND ELSE
# Initialize offset
offset = -6

# Code the while loop
while offset != 0 :
    print("correcting...")
    if offset>0:
      offset=offset-1
    else : 
      offset=offset+1
    print(offset)

#For loop
for var in seq:
    expression

#Iterating with a for loop 
#Iterating over a list example
employees=["...","...","..."]
for employee in employees:
    print(employee)
#Iterating over a string
for letter in "...":
    print(letter)
#Iterating over a range 
for i in range(...):
    print(i)

#Example
fam=[...,...,...,]
for height in fam:
    print(height)
  
#Enumerate
for index,height in enumerate(fam) :
    print("index"+str(index)+":"+str(height))

#Enumerate2
avengers=["...","...","..."]
e=enumerate(avengers)
print(type(e))
e_lise=list(e)
print(e_list)

#Enumerate and unpack
avengers=["...","...","..."]
for index,value in enumerate(avengers, start=...):
    print(index,value)

#Zip
avengers=["...","...","..."]
names=["...","...","..."]
z=zip(avengers,names)
z_list=list(z)
print(z_list)
#OR
print(*z)

#Zip and unpack
avengers=["...","...","..."]
names=["...","...","..."]
for z1,z2 in zip(avengers,names):
    print(z1,z2)
    
#Example zip 
# Create a list of tuples: mutant_data
mutant_data = list(zip(mutants,aliases,powers))
# Print the list of tuples
print(mutant_data)
# Create a zip object using the three lists: mutant_zip
mutant_zip = zip(mutants,aliases,powers)
# Print the zip object
print(mutant_zip)
# Unpack the zip object and print the tuple values
for value1,value2,value3 in mutant_zip:
    print(value1, value2, value3)

#Example2 
# house list of lists
house = [["hallway", 11.25], ["kitchen", 18.0], ["living room", 20.0], ["bedroom", 10.75], ["bathroom", 9.50]]
# Build a for loop from scratch
for x in house :
    print("the "+x[0]+" is "+ str(x[1])+" sqm") 
#FOR loop for dictionaries
world={"...":...,"...":...}
for x,y in world.items() :
    print(x+"..."+str(y))

#FOR loop for numpy arrays
#2D numpy array
meas=np.array([np_height,np_weight])
for x in np.nditer(meas) :
    print(val)

#For loop for Pandas dataframes
for x, y in df.iterrows() :
    print(x)
    print(y)

#Create a new column in a dataframe with upper letters or with the length of another column
#Example 
# Import cars data
import pandas as pd
cars = pd.read_csv('cars.csv', index_col = 0)
# Code for loop that adds COUNTRY column
for lab, row in cars.iterrows() :
    cars.loc[lab, "COUNTRY"] = row["country"].upper() #OR len(row["country"])
# Print cars
print(cars)
#OR ONE-LINE CODE WITHOUT FOR LOOP
# Import cars data
import pandas as pd
cars = pd.read_csv('cars.csv', index_col = 0)
# Use .apply(str.upper)
cars["COUNTRY"]=cars["country"].apply(str.upper)
print(cars)

#Random Generators
np.random.seed(123)
np.random.rand()
np.random.randint(...,...)

#Example of an algorith with random walk
# Numpy is imported, seed is set

# Initialize random_walk
random_walk = [0]

for x in range(100) :
    step = random_walk[-1]
    dice = np.random.randint(1,7)

    if dice <= 2:
        # Replace below: use max to make sure step can't go below 0
        step = max(0,step - 1)
    elif dice <= 5:
        step = max(0,step + 1)
    else:
        step = step + np.random.randint(1,7)

    random_walk.append(step)

print(random_walk)

#Functions
def square(value): #Function Header
     new_value=value**2 #Function body
     return new_value

#EXAMPLE
# Define shout with the parameter, word
def shout(word):
    """Print a string with three exclamation marks"""
    # Concatenate the strings: shout_word
    shout_word = word + '!!!'
    # Print shout_word
    print(shout_word)
# Call shout with the string 'congratulations'
shout("congratulations")

#Multiple function parameters
def raise_to_power(value1,value2):
    new_value=value1**value2
    return new_value
result=raise_to_power(2,3)
print(result)

#Returning multiple values
def raise_both(value1,value2):
    new_value1=value1**value2
    new_value2=value2**value1
    new_tuple=(new_value1,new_value2)
    return new_tuple
resulti=raise_both(2,3)
print(result)

#EXAMPLE
# Define shout with parameters word1 and word2
def shout(word1, word2):
    """Concatenate strings with three exclamation marks"""
    # Concatenate word1 with '!!!': shout1
    shout1=word1+"!!!"
    # Concatenate word2 with '!!!': shout2
    shout2=word2+"!!!"
    # Concatenate shout1 with shout2: new_shout
    new_shout=shout1+shout2
    # Return new_shout
    return new_shout
# Pass 'congratulations' and 'you' to shout(): yell
yell=shout("congratulations","you")
# Print yell
print(yell)

#EXAMPLE2
# Import pandas
import pandas as pd
# Import Twitter data as DataFrame: df
df = pd.read_csv("tweets.csv")
# Initialize an empty dictionary: langs_count
langs_count = {}
# Extract column from DataFrame: col
col = df['lang']
print(col)
# Iterate over lang column in DataFrame
for entry in col:
    # If the language is in langs_count, add 1 
    if entry in langs_count.keys():
        langs_count[entry]+=1
    # Else add the language to langs_count, set the value to 1
    else:
        langs_count[entry]=1
# Print the populated dictionary
print(langs_count) 

#GLOBAL SCOPE VS LOCAL SCOPE AND HOW TO CHANGE IT
#Example
# Create a string: team
team = "teen titans"
# Define change_team()
def change_team():
    """Change the value of the global variable team."""
    # Use team in global scope
    global team
    # Change the value of team in global: team
    team="justice league"
# Print team
print(team)
# Call change_team()
change_team()
# Print team
print(team)

#Directory of libraries
dir(package) 
dir(builtins)

#Nested function
def outter(x1,x2,x3):
    def inner(x):
        return x%2+5
    return(inner(x1),inner(x2),inner(x3))
print(outer(1,2,3))

#Example
# Define three_shouts
def three_shouts(word1, word2, word3):
    """Returns a tuple of strings
    concatenated with '!!!'."""
    # Define inner
    def inner(word):
        """Returns a string concatenated with '!!!'."""
        return word + '!!!'
    # Return a tuple of strings
    return (inner(word1),inner(word2),inner(word3))
# Call three_shouts() and print
print(three_shouts('a', 'b', 'c'))

#Example2
# Define echo
def echo(n):
    """Return the inner_echo function."""
    # Define inner_echo
    def inner_echo(word1):
        """Concatenate n copies of word1."""
        echo_word = word1 * n
        return echo_word
    # Return inner_echo
    return inner_echo
# Call echo: twice
twice = echo(2)
# Call echo: thrice
thrice=echo(3)
# Call twice() and thrice() then print
print(twice('hello'), thrice('hello'))

#Example3
# Define echo_shout()
def echo_shout(word):
    """Change the value of a nonlocal variable"""
    # Concatenate word with itself: echo_word
    echo_word=word*2
    # Print echo_word
    print(echo_word)
    # Define inner function shout()
    def shout():
        """Alter a variable in the enclosing scope"""    
        # Use echo_word in nonlocal scope
        nonlocal echo_word
        # Change echo_word to echo_word concatenated with '!!!'
        echo_word = echo_word+"!!!"
    # Call function shout()
    shout()
    # Print echo_word
    print(echo_word)
# Call function echo_shout() with argument 'hello'
echo_shout("hello")

#EXAMPLE **KWARGS
# Define report_status
def report_status(**kwargs):
    """Print out the status of a movie character."""
    print("\nBEGIN: REPORT\n")
    # Iterate over the key-value pairs of kwargs
    for key,value in kwargs.items():
        # Print out the keys and values, separated by a colon ':'
        print(key + ": " + value)
    print("\nEND REPORT")
# First call to report_status()
report_status(name="luke",affiliation="jedi",status="missing")
# Second call to report_status()
report_status(name="anakin", affiliation="sith lord", status="deceased")

#Lambda functions
raise_to_power=lambda x,y: x**y
raise_to_power(2,3)

#Example
# Define echo_word as a lambda function: echo_word
echo_word = lambda word1,echo:word1*echo
# Call echo_word: result
result = echo_word("hey",5)
# Print result
print(result)

#Example2
# Create a list of strings: spells
spells = ["protego", "accio", "expecto patronum", "legilimens"]
# Use map() to apply a lambda function over spells: shout_spells
shout_spells= map(lambda item:item+"!!!", spells)
# Convert shout_spells to a list: shout_spells_list
shout_spells_list=list(shout_spells)
# Print the result
print(shout_spells_list)

#Example3
# Create a list of strings: fellowship
fellowship = ['frodo', 'samwise', 'merry', 'pippin', 'aragorn', 'boromir', 'legolas', 'gimli', 'gandalf']
# Use filter() to apply a lambda function over fellowship: result
result = filter(lambda member:len(member)>6 , fellowship)
# Convert result to a list: result_list
result_list=list(result)
# Print result_list
print(result_list)

#Try,Raise and except
def sqrt(x):
    if x<0:
        raise ValueError("x must be non-negative")
    try:
        return x**0.5
    except TypeError:
        print("x must be an int or float")

#EXAMPLE
#write a lambda function and use filter() to select retweets, that is, tweets that begin with the string 'RT'
# Select retweets from the Twitter DataFrame: result
result = filter(lambda x:x[0:2]=="RT", tweets_df['text'])
# Create list from filter object result: res_list
res_list=list(result)
# Print all retweets in res_list
for tweet in res_list:
    print(tweet)

#Iterating over iterable:next()
word="Da"
it=iter(word)
next(it)
next(it)
#OR
print(*it)

#Iterating over dictionaries
pythonistas={"...":"...","...":"..."}
for key,value in pythonistas.items():
    print(key,value)

#Iterating over file connection
file=open("file.txt")
it=iter(file)
print(next(it))

#Example for creating a list of values and get the sum of it
# Create a range object: values
values = range(10,21)
# Print the range object
print(values)
# Create a list of integers: values_list
values_list = list(values)
# Print values_list
print(values_list)
# Get the sum of values: values_sum
values_sum = sum(values)
# Print values_sum
print(values_sum)

#Iterating over data
#Sum of a column of numbers
import pandas as pd 
result=[]
for chunk in pd.read_csv("data.csv",chunksize=1000):
    result.append(sum(chunk["x"]))
total=sum(result)
print(total)
#OR
import pandas as pd
total=0
for chunk in pd.read_csv("data.csv",chunksize=1000):
    total+=sum(chunk['x'])
print(total)

#Extracting information for large twitter data
# Define count_entries()
def count_entries(csv_file,c_size,colname):
    """Return a dictionary with counts of
    occurrences as value for each key."""    
    # Initialize an empty dictionary: counts_dict
    counts_dict = {}
    # Iterate over the file chunk by chunk
    for chunk in pd.read_csv(csv_file,chunksize=c_size):
        # Iterate over the column in DataFrame
        for entry in chunk[colname]:
            if entry in counts_dict.keys():
                counts_dict[entry] += 1
            else:
                counts_dict[entry] = 1
    # Return counts_dict
    return counts_dict
# Call count_entries(): result_counts
result_counts = count_entries("tweets.csv",10,"lang")
# Print result_counts
print(result_counts)

#List comprehension
nums=[12,8,21,3,16]
new_nums=[num+1 for num in nums]
print(new_nums)

#List comprehension With range()
result=[num for num in range(11)]
print(result)

#Nested loops
pairs=[(num1,num2) for num1 in range(0,2) for num2 in range(6,8)]
print(pairs)

#Conditionals in comprehensions/generator
#EXAMPLE
even_nums=(num for in range(10) if num %2==0)
print(list(even_nums))
#Example
[num**2 for num in range(10) if num %2==0 else 0 for num in range(10)]

#Dict comprehensions
pos_neg={num:-num for num in range(9)}
print(pos_neg)

#Transforming lists to dicts
# Print the first two lists in row_lists
print(row_lists[0])
print(row_lists[1])
# Turn list of lists into list of dicts: list_of_dicts
list_of_dicts = [lists2dict(feature_names,sublist) for sublist in row_lists]
# Print the first two dictionaries in list_of_dicts
print(list_of_dicts[0])
print(list_of_dicts[1])

#Example
# Create a list of strings: fellowship
fellowship = ['frodo', 'samwise', 'merry', 'aragorn', 'legolas', 'boromir', 'gimli']
# Create list comprehension: new_fellowship
new_fellowship = [member if len(member) >= 7 else '' for member in fellowship]
# Print the new list
print(new_fellowship)

#Example
# Create a list of strings: fellowship
fellowship = ['frodo', 'samwise', 'merry', 'aragorn', 'legolas', 'boromir', 'gimli']
# Create dict comprehension: new_fellowship
new_fellowship = {member:len(member) for member in fellowship }
# Print the new dictionary
print(new_fellowship)

#WE USE GENERATORS INSTEAD OF LIST WHEN THE OBJECT IS HUGE

#Build a generator function
def num_sequence(n):
    i=0
    while i<n:
        yield i 
        i +=1

#Example of generator 
# Create a list of strings
lannister = ['cersei', 'jaime', 'tywin', 'tyrion', 'joffrey']
# Define generator function get_lengths
def get_lengths(input_list):
    """Generator function that yields the
    length of the strings in input_list."""
    # Yield the length of a string
    for person in input_list:
        yield len(person)
# Print the values generated by get_lengths()
for value in get_lengths(lannister):
    print(value)

#General example
# Extract the created_at column from df: tweet_time
tweet_time = df["created_at"]
# Extract the clock time: tweet_clock_time
tweet_clock_time = [entry[11:19] for entry in tweet_time if entry[17:19]=="19"]
# Print the extracted times
print(tweet_clock_time)

#Writing an iterator to load data in chunks DATAFRAMES
# Define plot_pop()
def plot_pop(filename, country_code):
    # Initialize reader object: urb_pop_reader
    urb_pop_reader = pd.read_csv(filename, chunksize=1000)
    # Initialize empty DataFrame: data
    data = pd.DataFrame()    
    # Iterate over each DataFrame chunk
    for df_urb_pop in urb_pop_reader:
        # Check out specific country: df_pop_ceb
        df_pop_ceb = df_urb_pop[df_urb_pop['CountryCode'] == country_code]
        # Zip DataFrame columns of interest: pops
        pops = zip(df_pop_ceb['Total Population'],
                    df_pop_ceb['Urban population (% of total)'])
        # Turn zip object into list: pops_list
        pops_list = list(pops)
        # Use list comprehension to create new DataFrame column 'Total Urban Population'
        df_pop_ceb['Total Urban Population'] = [int(tup[0] * tup[1] * 0.01) for tup in pops_list]   
        # Append DataFrame chunk to data: data
        data = data.append(df_pop_ceb)
    # Plot urban population data
    data.plot(kind='scatter', x='Year', y='Total Urban Population')
    plt.show()
# Set the filename: fn
fn = 'ind_pop_data.csv'
# Call plot_pop for country code 'CEB'
plot_pop(fn,"CEB")
# Call plot_pop for country code 'ARB'
plot_pop(fn,"CEB")

#Delete columns 
corona_dataset_csv.drop(["Lat","Long"],axis=1)
# find maximum  rate for all of the countries. 
countries=list(corona_dataset_aggregated.index)
max_infection_rates=[]
for c in countries :
    max_infection_rates.append(corona_dataset_aggregated.loc[c].diff().max())
max_infection_rates

#Does average beauty score differ by gender? Produce the means and standard deviations for both male and female instructors.
ratings_df.groupby('gender').agg({'beauty':['mean', 'std', 'var']}).reset_index()

#Calculate the percentage of males and females that are tenured professors. Will you say that tenure status differ by gender?
tenure_count = ratings_df[ratings_df.tenure == 'yes'].groupby('gender').agg({'tenure': 'count'}).reset_index()
tenure_count['percentage'] = 100 * tenure_count.tenure/tenure_count.tenure.sum()
tenure_count

#### Question 1: Calculate the percentage of visible minorities are tenure professors. Will you say that tenure status differed if teacher was a visible minority?
tenure_count = ratings_df.groupby('minority').agg({'tenure': 'count'}).reset_index()
tenure_count['percentage'] = 100 * tenure_count.tenure/tenure_count.tenure.sum()
tenure_count

#Does average age differ by tenure? Produce the means and standard deviations for both tenured and untenured professors.
ratings_df.groupby('tenure').agg({'age':['mean', 'std']}).reset_index()

# What is the Median evaluation score for tenured Professors?
ratings_df[ratings_df['tenure'] == 'yes']['eval'].median()


##Get data from an API (JSON)
# Create dictionary to query API for cafes in NYC
api_url = "https://api.yelp.com/v3/businesses/search"
parameters = {"term": "cafe",
          	  "location": "NYC"}
headers = {"Authorization": "Bearer {}".format(api_key)}
# Query the Yelp API with headers and params set
response = requests.get(api_url,
                headers=headers,
                params=parameters)
# Extract JSON data from response
data = response.json()
# Load "businesses" values to a data frame and print head
cafes = pd.DataFrame(data["businesses"])
print(cafes.head())

#Append data frames
# Add an offset parameter to get cafes 51-100
params = {"term": "cafe", 
          "location": "NYC",
          "sort_by": "rating", 
          "limit": 50,
          "offset" : 50}

result = requests.get(api_url, headers=headers, params=params)
next_50_cafes = json_normalize(result.json()["businesses"])
# Append the results, setting ignore_index to renumber rows
cafes = top_50_cafes.append(next_50_cafes,ignore_index=True)
# Print shape of cafes
print(cafes.shape)

#Merge data frames
# Merge crosswalk into cafes on their zip code fields
cafes_with_pumas = cafes.merge(crosswalk,left_on="location_zip_code",right_on="zipcode")
# Merge pop_data into cafes_with_pumas on puma field
cafes_with_pop = cafes_with_pumas.merge(pop_data, on="puma")
# View the data
print(cafes_with_pop.head())

#Connecting to a PostgreSQL database
# Import create_engine function
from sqlalchemy import create_engine
# Create an engine to the census database
engine = create_engine('postgresql+psycopg2://student:datacamp@postgresql.csrrinzqubik.us-east-1.rds.amazonaws.com:5432/census')
# Use the .table_names() method on the engine to print the table names
print(engine.table_names())

#SQL DATABASE CONNECTION sql 
# Import create_engine, MetaData, and Table
from sqlalchemy import create_engine, MetaData, Table
# Create engine: engine
engine = create_engine('sqlite:///census.sqlite')
# Create a metadata object: metadata
metadata = MetaData()
# Reflect census table from the engine: census
census = Table('census', metadata, autoload=True, autoload_with=engine)
# Print census table metadata
print(repr(census))
# Print the column names
print(census.columns.keys())



#Selecting data from a Table: raw SQL
from sqlalchemy import create_engine
engine = create_engine('sqlite:///census.sqlite')
# Create a connection on engine
connection = engine.connect()
# Build select statement for census table: stmt
stmt = 'SELECT * FROM census'
# Execute the statement and fetch the results: results
results = connection.execute(stmt).fetchall()
# Print results
print(results)

#Selecting data from a Table with SQLAlchemy
# Import select
from sqlalchemy import select
# Reflect census table via engine: census
census = Table('census', metadata, autoload=True, autoload_with=engine)
# Build select statement for census table: stmt
stmt = select([census])
# Print the emitted statement to see the SQL string
print(stmt)
# Execute the statement on connection and fetch 10 records: result
results = connection.execute(stmt).fetchmany(size=10)
# Execute the statement and print the results
print(results)

#Filter data selected from a Table - Simple
#1
# Create a select query: stmt
stmt = select([census])
# Add a where clause to filter the results to only those for New York : stmt_filtered
stmt = stmt.where(census.columns.state=='New York')
# Execute the query to retrieve all the data returned: results
results = connection.execute(stmt).fetchall()
# Loop over the results and print the age, sex, and pop2000
for result in connection.execute(stmt):
    print(result.age, result.sex, result.pop2000)
#2
# Define a list of states for which we want results
states = ['New York', 'California', 'Texas']
# Create a query for the census table: stmt
stmt = select([census])
# Append a where clause to match all the states in_ the list states
stmt = stmt.where(census.columns.state.in_(states))
# Loop over the ResultProxy and print the state and its population in 2000
for result in connection.execute(stmt):
    print(result.state, result.pop2000)

#3
# Import and_
from sqlalchemy import and_
# Build a query for the census table: stmt
stmt = select([census])
# Append a where clause to select only non-male records from California using and_
stmt = stmt.where(
    # The state of California with a non-male sex
    and_(census.columns.state == 'California',
         census.columns.sex != 'M'
         )
)
# Loop over the ResultProxy printing the age and sex
for result in connection.execute(stmt):
    print(result.age, result.sex)

#Ordering by a single column
# Build a query to select the state column: stmt
stmt = select([census.columns.state])
# Order stmt by the state column
stmt = stmt.order_by(census.columns.state)
# Execute the query and store the results: results
results = connection.execute(stmt).fetchall()
# Print the first 10 results
print(results[:10])

#Ordering in descending order by a single column
# Import desc
from sqlalchemy import desc
# Build a query to select the state column: stmt
stmt = select([census.columns.state])
# Order stmt by state in descending order: rev_stmt
rev_stmt = stmt.order_by(desc(census.columns.state))
# Execute the query and store the results: rev_results
rev_results = connection.execute(rev_stmt).fetchall()
# Print the first 10 rev_results
print(rev_results[:10])

#Ordering by multiple columns
# Build a query to select state and age: stmt
stmt = select([census.columns.state, census.columns.age])
# Append order by to ascend by state and descend by age
stmt = stmt.order_by(census.columns.state, desc(census.columns.age))
# Execute the statement and store all the records: results
results = connection.execute(stmt).fetchall()
# Print the first 20 results
print(results[:20])

#Counting distinct data
# Build a query to count the distinct states values: stmt
stmt = select([func.count(census.columns.state.distinct())])
# Execute the query and store the scalar result: distinct_state_count
distinct_state_count = connection.execute(stmt).scalar()
# Print the distinct_state_count
print(distinct_state_count)

#Count of records by state
# Import func
from sqlalchemy import func
# Build a query to select the state and count of ages by state: stmt
stmt = select([census.columns.state,func.count(census.columns.age)])
# Group stmt by state
stmt = stmt.group_by(census.columns.state)
# Execute the statement and store all the records: results
results = connection.execute(stmt).fetchall()
# Print results
print(results)
# Print the keys/column names of the results returned
print(results[0].keys())

#Determining the population sum by state
# Import func
from sqlalchemy import func
# Build an expression to calculate the sum of pop2008 labeled as population
pop2008_sum = func.sum(census.columns.pop2008).label('population')
# Build a query to select the state and sum of pop2008: stmt
stmt = select([census.columns.state, pop2008_sum])
# Group stmt by state
stmt = stmt.group_by(census.columns.state)
# Execute the statement and store all the records: results
results = connection.execute(stmt).fetchall()
# Print results
print(results)
# Print the keys/column names of the results returned
print(results[0].keys())

#ResultsSets to pandas DataFrames
# import pandas
import pandas as pd
# Create a DataFrame from the results: df
df = pd.DataFrame(results)
# Set column names
df.columns = results[0].keys()
# Print the DataFrame
print(df)

#Calculating a difference between two columns
# Build query to return state names by population difference from 2008 to 2000: stmt
stmt = select([census.columns.state, (census.columns.pop2008-census.columns.pop2000).label('pop_change')])
# Append group by for the state: stmt_grouped
stmt_grouped = stmt.group_by(census.columns.state)
# Append order by for pop_change descendingly: stmt_ordered
stmt_ordered = stmt_grouped.order_by(desc('pop_change'))
# Return only 5 results: stmt_top5
stmt_top5 = stmt_ordered.limit(5)
# Use connection to execute stmt_top5 and fetch all results
results = connection.execute(stmt_top5).fetchall()
# Print the state and population change for each record
for result in results:
    print('{}:{}'.format(result.state, result.pop_change))

#Determining the overall percentage of women
# import case, cast and Float from sqlalchemy
from sqlalchemy import case, cast, Float
# Build an expression to calculate female population in 2000
female_pop2000 = func.sum(
    case([
        (census.columns.sex == 'F', census.columns.pop2000)
    ], else_=0))
# Cast an expression to calculate total population in 2000 to Float
total_pop2000 = cast(func.sum(census.columns.pop2000), Float)
# Build a query to calculate the percentage of women in 2000: stmt
stmt = select([female_pop2000 / total_pop2000* 100])
# Execute the query and store the scalar result: percent_female
percent_female = connection.execute(stmt).scalar()
# Print the percentage
print(percent_female)

#Joins
# Build a statement to select the state, sum of 2008 population and census
# division name: stmt
stmt = select([
    census.columns.state,
    func.sum(census.columns.pop2008),
    state_fact.columns.census_division_name
])
# Append select_from to join the census and state_fact tables by the census state and state_fact name columns
stmt_joined = stmt.select_from(
    census.join(state_fact, census.columns.state == state_fact.columns.name)
)
# Append a group by for the state_fact name column
stmt_grouped = stmt_joined.group_by(state_fact.columns.name)
# Execute the statement and get the results: results
results = connection.execute(stmt_grouped).fetchall()
# Loop over the results object and print each record.
for record in results:
    print(record)


#Using alias to handle same table joined queries
# Make an alias of the employees table: managers
managers = employees.alias()
# Build a query to select names of managers and their employees: stmt
stmt = select(
    [managers.columns.name.label('manager'),
     employees.columns.name.label('employee')]
)
# Match managers id with employees mgr: stmt_matched
stmt_matched = stmt.where(managers.columns.id == employees.columns.mgr)
# Order the statement by the managers name: stmt_ordered
stmt_ordered = stmt_matched.order_by(managers.columns.name)
# Execute statement: results
results = connection.execute(stmt_ordered).fetchall()
# Print records
for record in results:
    print(record)

#Work on large resultproxy 
# Start a while loop checking for more results
while more_results:
    # Fetch the first 50 results from the ResultProxy: partial_results
    partial_results = results_proxy.fetchmany(50)
    # if empty list, set more_results to False
    if partial_results == []:
        more_results = False
    # Loop over the fetched records and increment the count for the state
    for row in partial_results:
        if row.state in state_count:
            state_count[row.state] +=1
        else:
            state_count[row.state]=1
# Close the ResultProxy, and thus the connection
results_proxy.close()
# Print the count by state
print(state_count)

#Creating tables with SQLAlchemy
# Import Table, Column, String, Integer, Float, Boolean from sqlalchemy
from sqlalchemy import Table, Column, String, Integer, Float, Boolean
# Define a new table with a name, count, amount, and valid column: data
data = Table('data', metadata,
             Column('name', String(255), unique=True),
             Column('count', Integer(), default=1),
             Column('amount', Float()),
             Column('valid', Boolean(), default=False)
)
# Use the metadata to create the table
metadata.create_all(engine)
# Print the table details
print(repr(metadata.tables['data']))

#Inserting a single row
# Import insert and select from sqlalchemy
from sqlalchemy import insert, select
# Build an insert statement to insert a record into the data table: insert_stmt
insert_stmt = insert(data).values(name='Anna', count=1, amount=1000.00, valid=True)
# Execute the insert statement via the connection: results
results = connection.execute(insert_stmt)
# Print result rowcount
print(results.rowcount)
# Build a select statement to validate the insert: select_stmt
select_stmt = select([data]).where(data.columns.name == 'Anna')
# Print the result of executing the query.
print(connection.execute(select_stmt).first())

#Inserting Multiple rows at once
# Build a list of dictionaries: values_list
values_list = [
    {'name': 'Anna', 'count': 1, 'amount': 1000.00, 'valid': True},
    {'name': 'Taylor', 'count': 1, 'amount': 750.00, 'valid': False}
]
# Build an insert statement for the data table: stmt
stmt = insert(data)
# Execute stmt with the values_list: results
results = connection.execute(stmt, values_list)
# Print rowcount
print(results.rowcount)

#Loading a CSV into a table
# import pandas
import pandas as pd
# read census.csv into a DataFrame : census_df
census_df = pd.read_csv("census.csv", header=None)
# rename the columns of the census DataFrame
census_df.columns = ['state', 'sex', 'age', 'pop2000', 'pop2008']
# append the data from census_df to the "census" table via connection
census_df.to_sql(name='census', con=connection, if_exists='append', index=False)

#Updating individual records
select_stmt = select([state_fact]).where(state_fact.columns.name == 'New York')
results = connection.execute(select_stmt).fetchall()
print(results)
print(results[0]['fips_state'])
update_stmt = update(state_fact).values(fips_state = 36)
update_stmt = update_stmt.where(state_fact.columns.name == 'New York')
update_results = connection.execute(update_stmt)
# Execute select_stmt again and fetch the new results
new_results = connection.execute(select_stmt).fetchall()
# Print the new_results
print(new_results)
# Print the FIPS code for the first row of the new_results
print(new_results[0]['fips_state'])

#Correlated updates
# Build a statement to select name from state_fact: fips_stmt
fips_stmt = select([state_fact.columns.name])
# Append a where clause to match the fips_state to flat_census fips_code: fips_stmt
fips_stmt = fips_stmt.where(
    state_fact.columns.fips_state == flat_census.columns.fips_code)
# Build an update statement to set the name to fips_stmt_where: update_stmt
update_stmt = update(flat_census).values(state_name=fips_stmt)
# Execute update_stmt: results
results = connection.execute(update_stmt)
# Print rowcount
print(results.rowcount)

#Deleting all the records from a table
# Build a statement to count records using the sex column for Men ('M') age 36: count_stmt
count_stmt = select([func.count(census.columns.sex)]).where(
    and_(census.columns.sex == 'M',
         census.columns.age == 36)
)
# Execute the select statement and use the scalar() fetch method to save the record count
to_delete = connection.execute(count_stmt).scalar()
# Build a statement to delete records from the census table: delete_stmt
delete_stmt = delete(census)
# Append a where clause to target Men ('M') age 36: delete_stmt
delete_stmt = delete_stmt.where(
    and_(census.columns.sex == 'M',
         census.columns.age == 36)
)
# Execute the statement: results
results = connection.execute(delete_stmt)
# Print affected rowcount and to_delete record count, make sure they match
print(results.rowcount, to_delete)

#Delete a table completely
# Drop the state_fact table
state_fact.drop(engine)
# Check to see if state_fact exists
print(state_fact.exists(engine))
# Drop all tables
metadata.drop_all(engine)
# Check to see if census exists
print(census.exists(engine))

#You want to visualize both the individual distributions as well as the relationship
sns.jointplot(x = 'age', y = 'value', data = valuation)
plt.show()

#Transform the values of a column
print(df.stars.transform(lambda x: x / 1000)) 

#Loop over a list to find only certain length of strings
#1st way
names = ['Jerry', 'Kramer', 'Elaine', 'George', 'Newman']
# Print the list created using the Non-Pythonic approach
i = 0
new_list= []
while i < len(names):
    if len(names[i]) >= 6:
        new_list.append(names[i])
    i += 1
print(new_list)
#2nd way
# Print the list created by looping over the contents of names
better_list = []
for name in names:
    if len(name) >= 6:
        better_list.append(name)
print(better_list)
#3rd way
# Print the list created by using list comprehension
best_list = [name for name in names if len(name) >= 6]
print(best_list)

#Built-in functions 
#range(start,stop,step)
# Create a range object that goes from 0 to 5
nums = range(0,6)
# Convert nums to a list
nums_list = list(nums)
# Create a new list of odd numbers from 1 to 11 by unpacking a range object
nums_list2 = [*range(1,12,2)]
print(nums_list2)

#enumerate() to represent the order in a list
# Rewrite the for loop to use enumerate 1st way
names = ['Jerry', 'Kramer', 'Elaine', 'George', 'Newman']
indexed_names = []
for i,name in enumerate(names):
    index_name = (i,name)
    indexed_names.append(index_name) 
print(indexed_names)

# Rewrite the above for loop using list comprehension 2nd way
indexed_names_comp = [(i,name) for i,name in enumerate(names)]
print(indexed_names_comp)

# Unpack an enumerate object with a starting index of one 3rd way
indexed_names_unpack = [*enumerate(names, 1)]
print(indexed_names_unpack)

#map()
# Use map to apply str.upper to each element in names
names = ['Jerry', 'Kramer', 'Elaine', 'George', 'Newman']
names_map  = map(str.upper, names)
# Print the type of the names_map
print(type(names_map))
# Unpack names_map into a list
names_uppercase = [* list(names_map)]
# Print the list created above
print(names_uppercase)

#Numpy arrays
# Print second row of nums
print(nums[1,:])
# Print all elements of nums that are greater than six
print(nums[nums > 6])
# Double every element of nums
nums_dbl = nums * 2
print(nums_dbl)
# Replace the third column of nums
nums[:,2] = nums[:,2] + 1
print(nums)

#Numpy arrays broadcasting
# Create a list of arrival times
names = ['Jerry', 'Kramer', 'Elaine', 'George', 'Newman']
arrival_times = [*range(10,60,10)]
# Convert arrival_times to an array and update the times
arrival_times_np = np.array(arrival_times)
new_times = arrival_times_np - 3
# Use list comprehension and enumerate to pair guests to new times
guest_arrivals = [(names[i],time) for i,time in enumerate(new_times)]
# Map the welcome_guest function to each (guest,time) pair
welcome_map = map(welcome_guest, guest_arrivals) #welcome_guest is a preloaded function
guest_welcomes = [*welcome_map]
print(*guest_welcomes, sep='\n')

#Extract a function
def standardize(column):
  """Standardize the values in a column.

  Args:
    column (pandas Series): The data to standardize.

  Returns:
    pandas Series: the values as z-scores
  """
  # Finish the function so that it returns the z-scores
  z_score = (column - column.mean()) / column.std()
  return z_score
# Use the standardize() function to calculate the z-scores
df['y1_z'] = standardize(df.y1_gpa)
df['y2_z'] = standardize(df.y2_gpa)
df['y3_z'] = standardize(df.y3_gpa)
df['y4_z'] = standardize(df.y4_gpa)


#def median(values):
  """Get the median of a sorted list of values

  Args:
    values (iterable of float): A list of numbers

  Returns:
    float
  """
  #Returning functions for a math game
  # Write the median() function
  midpoint = int(len(values) / 2)
  if len(values) % 2 == 0:
    median = (values[midpoint - 1] + values[midpoint]) / 2
  else:
    median = values[midpoint]
  return median
  def create_math_function(func_name):
  if func_name == 'add':
    def add(a, b):
      return a + b
    return add
  elif func_name == 'subtract':
    # Define the subtract() function
    def subtract(a,b):
      return a-b
    return subtract
  else:
    print("I don't know that one")
add = create_math_function('add')
print('5 + 2 = {}'.format(add(5, 2)))
subtract = create_math_function('subtract')
print('5 - 2 = {}'.format(subtract(5, 2)))