#Query sql database from python using pandas
# Complete the SELECT statement
data = pd.read_sql("""
SELECT first_name, last_name FROM "Customer"
ORDER BY last_name, first_name
""", db_engine)
# Show the first 3 rows of the DataFrame
print(data.head(3))
# Show the info of the DataFrame
print(data.info())

#How to split up a task to do calculations on several processing units iwth low level python API multiprocessing.Pool
#multiprocessing.Pool
from multiprocessing import Pool

from matplotlib.pyplot import table, text
def take_mean_age(year_and_group):    
    year, group = year_and_groupreturn pd.DataFrame({"Age": group["Age"].mean()}, index=[year])
with Pool(4) as p:   
     results = p.map(take_mean_age, athlete_events.groupby("Year"))
result_df = pd.concat(results)
#example
# Function to apply a function over multiple cores
@print_timing
def parallel_apply(apply_func, groups, nb_cores):
    with Pool(nb_cores) as p:
        results = p.map(apply_func, groups)
    return pd.concat(results)
# Parallel apply using 1 core
parallel_apply(take_mean_age, athlete_events.groupby('Year'), nb_cores=1)
# Parallel apply using 2 cores
parallel_apply(take_mean_age, athlete_events.groupby('Year'), nb_cores=2)
# Parallel apply using 4 cores
parallel_apply(take_mean_age, athlete_events.groupby('Year'), nb_cores=4)

#How to split up a task to do calculations on several processing units iwth dask framework
import dask.dataframe as dd
# Set the number of partitions
athlete_events_dask = dd.from_pandas(athlete_events, npartitions=4)
# Calculate the mean Age per Year
print(athlete_events_dask.groupby('Year').Age.mean().compute())

#Example of pyspark
# Print the type of athlete_events_spark
print(type(athlete_events_spark))
# Print the schema of athlete_events_spark
print(athlete_events_spark.printSchema())
# Group by the Year, and find the mean Age
print(athlete_events_spark.groupBy('Year').mean('Age'))
# Group by the Year, and find the mean Age
print(athlete_events_spark.groupBy('Year').mean('Age').show())

#How to submit an application to a spark cluster (local spark instance running on 4 threads)
cat /home/repl/spark-script.py
#prints this
from pyspark.sql import SparkSession
if __name__ == "__main__":
    spark = SparkSession.builder.getOrCreate()
    athlete_events_spark = (spark
        .read
        .csv("/home/repl/datasets/athlete_events.csv",
             header=True,
             inferSchema=True,
             escape='"'))

    athlete_events_spark = (athlete_events_spark
        .withColumn("Height",
                    athlete_events_spark.Height.cast("integer")))

    print(athlete_events_spark
        .groupBy('Year')
        .mean('Height')
        .orderBy('Year')
        .show())
#run this pyspark file using spark-submit
spark-submit \
  --master local[4] \
  /home/repl/spark-script.py

#Airflow Dags
# Create the DAG object
dag = DAG(dag_id="car_factory_simulation",
          default_args={"owner": "airflow","start_date": airflow.utils.dates.days_ago(2)},
          schedule_interval="0 * * * *")
# Task definitions
assemble_frame = BashOperator(task_id="assemble_frame", bash_command='echo "Assembling frame"', dag=dag)
place_tires = BashOperator(task_id="place_tires", bash_command='echo "Placing tires"', dag=dag)
assemble_body = BashOperator(task_id="assemble_body", bash_command='echo "Assembling body"', dag=dag)
apply_paint = BashOperator(task_id="apply_paint", bash_command='echo "Applying paint"', dag=dag)
# Complete the downstream flow
assemble_frame.set_downstream(place_tires)
assemble_frame.set_downstream(assemble_body)
assemble_body.set_downstream(apply_paint)

#EXTRACTION OF DATA 
#Extract from an API
import requests
# Fetch the Hackernews post
resp = requests.get("https://hacker-news.firebaseio.com/v0/item/16222426.json")
# Print the response parsed as JSON
print(resp.json())
# Assign the score of the test to post_score
post_score = resp.json()["score"]
print(post_score)

#Extract from a database
# Function to extract table to a pandas DataFrame
def extract_table_to_pandas(tablename, db_engine):
    query = "SELECT * FROM {}".format(tablename)
    return pd.read_sql(query, db_engine)
# Connect to the database using the connection URI
connection_uri = "postgresql://repl:password@localhost:5432/pagila" 
db_engine = sqlalchemy.create_engine(connection_uri)
# Extract the film table into a pandas DataFrame
extract_table_to_pandas("film", db_engine)
# Extract the customer table into a pandas DataFrame
extract_table_to_pandas("customer", db_engine)

#Extract data into Pyspark from databases
import pyspark.sql
spark = pyspark.sql.SparkSession.builder.getOrCreate()
spark.read.jdbc("jdbc:postgresql://localhost:5432/pagila","customer",properties={"user":"repl","password":"password"})

#Splitting a column example into 2 
# Get the rental rate column as a string
rental_rate_str = film_df.rental_rate.astype("str")
# Split up and expand the column
rental_rate_expanded = rental_rate_str.str.split(".", expand=True)
# Assign the columns to film_df
film_df = film_df.assign(
    rental_rate_dollar=rental_rate_expanded[0],
    rental_rate_cents=rental_rate_expanded[1],

#Pyspark Joining with ratings
# Use groupBy and mean to aggregate the column
ratings_per_film_df = rating_df.groupBy('film_id').mean('rating')
# Join the tables using the film_id column
film_df_with_ratings = film_df.join(
    ratings_per_film_df,
    film_df.film_id==ratings_per_film_df.film_id
)
# Show the 5 first results
print(film_df_with_ratings.show(5))

#Writing a dataframe to a file
# Write the pandas DataFrame to parquet
film_pdf.to_parquet("films_pdf.parquet")
# Write the PySpark DataFrame to parquet
film_sdf.write.parquet("films_sdf.parquet")

#Load into Postgres 
# Finish the connection URI
connection_uri = "postgresql://repl:password@localhost:5432/dwh"
db_engine_dwh = sqlalchemy.create_engine(connection_uri)
# Transformation step, join with recommendations data
film_pdf_joined = film_pdf.join(recommendations)
# Finish the .to_sql() call to write to store.film
film_pdf_joined.to_sql("film", db_engine_dwh, schema="store", if_exists="replace")
# Run the query to fetch the data
pd.read_sql("SELECT film_id, recommended_film_ids FROM store.film", db_engine_dwh)

#Defining a DAG
# Define the ETL function
def etl():
    film_df = extract_film_to_pandas()
    film_df = transform_rental_rate(film_df)
    load_dataframe_to_film(film_df)
# Define the ETL task using PythonOperator
etl_task = PythonOperator(task_id='etl_film',
                          python_callable=etl,
                          dag=dag)
# Set the upstream to wait_for_table and sample run etl()
etl_task.set_upstream(wait_for_table)
etl()

#Courses rating case ETL
#Querying the table (EXTRACT)
# Complete the connection URI
connection_uri = "postgresql://repl:password@localhost:5432/datacamp_application" 
db_engine = sqlalchemy.create_engine(connection_uri)
# Get user with id 4387
user1 = pd.read_sql("SELECT * FROM rating where user_id=4387", db_engine)
# Get user with id 18163
user2 = pd.read_sql("SELECT * FROM rating where user_id=18163", db_engine)
# Get user with id 8770
user3 = pd.read_sql("SELECT * FROM rating where user_id=8770", db_engine)
# Use the helper function to compare the 3 users
print_user_comparison(user1, user2, user3)

#Average rating per course (TRANSFORM)
# Complete the transformation function
def transform_avg_rating(rating_data):
    # Group by course_id and extract average rating per course
    avg_rating = rating_data.groupby('course_id').rating.mean()
    # Return sorted average ratings per course
    sort_rating = avg_rating.sort_values(ascending=False).reset_index()
    return sort_rating
# Extract the rating data into a DataFrame    
rating_data = extract_rating_data(db_engines)
# Use transform_avg_rating on the extracted data and print results
avg_rating_data = transform_avg_rating(rating_data)
print(avg_rating_data) 
#Filter out corrupt data (TRANSFORM)
course_data = extract_course_data(db_engines)

# Print out the number of missing values per column
print(course_data.isnull().sum())
# The transformation should fill in the missing values
def transform_fill_programming_language(course_data):
    imputed = course_data.fillna({"programming_language": "R"})
    return imputed
transformed = transform_fill_programming_language(course_data)
# Print out the number of missing values per column of transformed
print(transformed.isnull().sum())

#Using the recommender transformation(TRANSFORM)
# Complete the transformation function
def transform_recommendations(avg_course_ratings, courses_to_recommend):
    # Merge both DataFrames
    merged = courses_to_recommend.merge(avg_course_ratings) 
    # Sort values by rating and group by user_id
    grouped = merged.sort_values("rating", ascending=False).groupby("user_id")
    # Produce the top 3 values and sort by user_id
    recommendations = grouped.head(3).sort_values("user_id").reset_index()
    final_recommendations = recommendations[["user_id", "course_id","rating"]]
    # Return final recommendations
    return final_recommendations
# Use the function with the predefined DataFrame objects
recommendations = transform_recommendations(avg_course_ratings, courses_to_recommend)

#Load the dataframe to the database (LOAD)
#The target table
connection_uri = "postgresql://repl:password@localhost:5432/dwh" 
db_engine = sqlalchemy.create_engine(connection_uri)
def load_to_dwh(recommendations):
    recommendations.to_sql("recommendations", db_engine, if_exists="replace")

#Defining the DAG(LOAD)
# Define the DAG so it runs on a daily basis
dag = DAG(dag_id="recommendations",
          schedule_interval="0 0 * * *")
# Make sure `etl()` is called in the operator. Pass the correct kwargs.
task_recommendations = PythonOperator(
    task_id="recommendations_task",
    python_callable=etl,
    op_kwargs={"db_engines": db_engines},
)

#Query the recommendations
def recommendations_for_user(user_id, threshold=4.5):
    # Join with the courses table
    query = """
    SELECT title, rating FROM recommendations
    INNER JOIN courses ON courses.course_id = recommendations.course_id
    WHERE user_id=%(user_id)s AND rating>%(threshold)s
    ORDER BY rating DESC
    """
    # Add the threshold parameter
    predictions_df = pd.read_sql(query, db_engine, params = {"user_id": user_id, 
                                                             "threshold": threshold})
    return predictions_df.title.values
# Try the function you created
print(recommendations_for_user(12, 4.65))

#Data Ingestion (FLAT FILES CSV)
import pandas as pd 
df=pd.read_csv("", sep="\t",usecols=listofcols,)

#Modifying flat file imports
# Create list of columns to use
cols = ["zipcode", "agi_stub","mars1","MARS2","NUMDEP"]
# Create data frame from csv using only selected columns
data = pd.read_csv("vt_tax_data_2016.csv", usecols=cols)

#Import a file in chunks
# Create data frame of next 500 rows with labeled columns
vt_data_next500 = pd.read_csv("vt_tax_data_2016.csv", 
                       		  nrows=500,
                       		  skiprows=500,
                       		  header=None,
                       		  names=list(vt_data_first500))


#Specify data types of columns by creating a dictionary of columns and data types
# Create dict specifying data types for agi_stub and zipcode
data_types = {"agi_stub": "category",
			  "zipcode": str}

# Load csv using dtype to set correct data types
data = pd.read_csv("vt_tax_data_2016.csv", dtype=data_types)

#Set custom NA values by creating a dictionary
# Create dict specifying that 0s in zipcode are NA values
null_values = {"zipcode" : 0}

# Load csv using na_values keyword argument
data = pd.read_csv("vt_tax_data_2016.csv", 
                   na_values=null_values)

#Skip bad data 
  # Import CSV with error_bad_lines set to skip bad records
  data = pd.read_csv("vt_tax_data_2016_corrupt.csv", 
                     error_bad_lines=False,
                     warn_bad_lines=True)
 
 #Data Ingestion (SPREADSHEETS)
 # Read spreadsheet and assign it to survey_responses
survey_responses = pd.read_excel("fcc_survey.xlsx")

#Load a portion of a dataset
# Create string of lettered columns to load
col_string = "AD,AW:BA"
# Load data with skiprows and usecols set
survey_responses = pd.read_excel("fcc_survey_headers.xlsx", 
                        skiprows=2, 
                        usecols=col_string)

#Working with multiple sheets
#Select a single sheet from a spreadsheet file
# Create df from second worksheet by referencing its name
responses_2017 = pd.read_excel("fcc_survey.xlsx",
                               sheet_name="2017"/sheet_name=1/2/3) 

#Select multiple sheets from a spreadsheet file
# Load both the 2016 and 2017 sheets by name
all_survey_data = pd.read_excel("fcc_survey.xlsx",
                                sheet_name=['2016','2017'])
#LOad all sheets in the excel file 
# Load all sheets in the Excel file
all_survey_data = pd.read_excel("fcc_survey.xlsx",
                                sheet_name=None)
# View the sheet names in all_survey_data
print(all_survey_data.keys())

#Compile similar spreadsheets into one dataset when column names are the same 
# Create an empty data frame
all_responses = pd.DataFrame()

# Set up for loop to iterate through values in responses
for df in responses.values():
  # Print the number of rows being added
  print("Adding {} rows".format(df.shape[0]))
  # Append df to all_responses, assign result
  all_responses = all_responses.append(df)

  #Set columns as boolean and set custom TRUE/FALSE values
  # Load file with Yes as a True value and No as a False value
survey_subset = pd.read_excel("fcc_survey_yn_data.xlsx",
                              dtype={"HasDebt": bool,
                              "AttendedBootCampYesNo": bool},
                              true_values=["Yes"],
                              false_values=["No"])

#Parsing dates and times
#Parse simple dates
# Load file, with Part1StartTime parsed as datetime data
survey_data = pd.read_excel("fcc_survey.xlsx",
                            parse_dates=["Part1StartTime"])


#Combine a date column with a time column in one datetime column
# Create dict of columns to combine into new datetime column
datetime_cols = {"Part2Start": ["Part2StartDate","Part2StartTime"]}
# Load file, supplying the dict to parse_dates
survey_data = pd.read_excel("fcc_survey_dts.xlsx",
                            parse_dates=datetime_cols)
#Parse non-standard date formats
# Parse datetimes and assign result back to Part2EndTime
survey_data["Part2EndTime"] = pd.to_datetime(survey_data["Part2EndTime"], 
                                             format="%m%d%Y %H:%M:%S")

#Data Ingestion (DATABASES)
# Import sqlalchemy's create_engine() function
from sqlalchemy import create_engine
# Create the database engine
engine = create_engine('sqlite:///data.db')
# View the tables in the database
print(engine.table_names())
# Load hpd311calls without any SQL
hpd_calls = pd.read_sql('hpd311calls', engine)
#OR Load hpd311calls with sql query
# Create a SQL query to load the entire weather table
query = """
SELECT * 
  FROM weather;
"""
# Load weather with the SQL query
weather = pd.read_sql(query, engine)

#Refining queries in sqlalchemy
# Write query to get date, tmax, and tmin from weather
query = """
SELECT date, 
       tmax, 
       tmin
  FROM weather;
"""

# Create query to get hpd311calls records about safety
query = """
SELECT *
FROM hpd311calls
WHERE complaint_type='SAFETY';
"""
# Graph the number of safety calls by borough
call_counts = safety_calls.groupby('borough').unique_key.count()
call_counts.plot.barh()
plt.show()

# Create query for records with max temps <= 32 or snow >= 1
query = """
SELECT *
  FROM weather
  WHERE tmax <= 32
  OR snow>=1;
"""

#Get unique values for one or more columns 
SELECT DISTINCT column FROM table;
#example
# Create query for unique combinations of borough and complaint_type
query = """
SELECT DISTINCT borough, 
        complaint_type
  from hpd311calls;
"""
#Remove duplicate records
SELECT DISTINCT * FROM table;
#Get number of rows that meet query conditions
SELECT COUNT (*) FROM table
#example
# Create query to get call counts by complaint_type
query = """
SELECT complaint_type, 
     count(*)
  FROM hpd311calls
  group by complaint_type;
"""
#Get number of unique values in a column
SELECT COUNT(DISTINCT column) FROM table

#Joining tables
# Query to join weather to call records by date columns
query = """
SELECT * 
  FROM hpd311calls
  JOIN weather 
  ON hpd311calls.created_date = weather.date;
"""
#Joining and filtering tables
# Query to get hpd311calls and precipitation values
query = """
SELECT hpd311calls.*, weather.prcp
  FROM hpd311calls
  JOIN weather
  ON hpd311calls.created_date = weather.date;"""
  # Query to get water leak calls and daily precipitation
query = """
SELECT hpd311calls.*, weather.prcp
  FROM hpd311calls
  JOIN weather
    ON hpd311calls.created_date = weather.date
  WHERE hpd311calls.complaint_type = 'WATER LEAK';"""
  #Joining, filtering, and aggregating tables
  # Query to get heat/hot water call counts by created_date
query = """
SELECT hpd311calls.created_date, 
       count(*)
  FROM hpd311calls 
  WHERE hpd311calls.complaint_type = 'HEAT/HOT WATER'
  GROUP BY hpd311calls.created_date;
"""
# Modify query to join tmax and tmin from weather by date
query = """
SELECT hpd311calls.created_date, 
	   COUNT(*), 
       weather.tmax,
       weather.tmin
  FROM hpd311calls 
       JOIN weather
       ON hpd311calls.created_date = weather.date
 WHERE hpd311calls.complaint_type = 'HEAT/HOT WATER' 
 GROUP BY hpd311calls.created_date;
 """
  
#Data Ingestion (JSON-API)
# Load the daily report to a data frame
pop_in_shelters = pd.read_json("dhs_daily_report.json", orient="split")

#Get data from an API
api_url = "https://api.yelp.com/v3/businesses/search"
params = {"term": "cafe", "location": "NYC"}
headers = {"Authorization": "Bearer {}".format(api_key)}
# Get data about NYC cafes from the Yelp API
response = requests.get(api_url, 
                headers=headers, 
                params=params)
# Extract JSON data from the response
data = response.json()
# Load data to a data frame
cafes = pd.DataFrame(data["businesses"])
# View the data's dtypes
print(cafes.dtypes)

#Flatten nested JSONs
# Load json_normalize()
from pandas.io.json import json_normalize3
# Isolate the JSON data from the API response
data = response.json()
# Flatten business data into a data frame, replace separator
cafes = json_normalize(data["businesses"],
             sep="_")
#Hndle deeply nested data 
# Load other business attributes and set meta prefix
flat_cafes = json_normalize(data["businesses"],
                            sep="_",
                    		record_path="categories",
                    		meta=["name", 
                                  "alias",  
                                  "rating",
                          		  ["coordinates", "latitude"], 
                          		  ["coordinates", "longitude"]],
                    		meta_prefix="biz_")
# View the data
print(flat_cafes.head())

#Combining multiple datasets
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

#Merge dataframes
# Merge crosswalk into cafes on their zip code fields
cafes_with_pumas = cafes.merge(crosswalk,left_on="location_zip_code",right_on="zipcode")
# Merge pop_data into cafes_with_pumas on puma field
cafes_with_pop = cafes_with_pumas.merge(pop_data, on="puma")

#Efficiently combining, counting, and iterating
#Combining objects with zip 
names=["...","..."]
hps=[...,...]
combined_zip=zip(names,hps)
combined_zip_list=[*combined_zip]

#Counter 
# Collect the count of primary types
type_count = Counter(primary_types)
print(type_count, '\n')
# Collect the count of generations
gen_count = Counter(generations)
print(gen_count, '\n')
# Use list comprehension to get each Pokémon's starting letter
starting_letters = [name[0] for name in names]
# Collect the count of Pokémon for each starting_letter
starting_letters_count = Counter(starting_letters)
print(starting_letters_count)

#Combinations 
# Import combinations from itertools
from itertools import combinations
# Create a combination object with pairs of Pokémon
combos_obj = combinations(pokemon, 2)
print(type(combos_obj), '\n')
# Convert combos_obj to a list by unpacking
combos_2 = [*combos_obj]
print(combos_2, '\n')
# Collect all possible combinations of 4 Pokémon directly into a list
combos_4 = [*combinations(pokemon, 4)]
print(combos_4)

#Set theory (intersection,difference,symmetric_difference,union,unique)
# Convert both lists to sets
ash_set = set(ash_pokedex)
misty_set = set(misty_pokedex)
# Find the Pokémon that exist in both sets
both = ash_set.intersection(misty_set)
print(both)
# Find the Pokémon that Ash has and Misty does not have
ash_only = ash_set.difference(misty_set)
print(ash_only)
# Find the Pokémon that are in only one set (not both)
unique_to_set = ash_set.symmetric_difference(misty_set)
print(unique_to_set)

# Check if Psyduck is in Ash's list and Brock's set
print('Psyduck' in ash_pokedex)
print('Psyduck' in brock_pokedex_set)

#Pokémon totals and averages without a loop
# Create a total stats array
total_stats_np = stats.sum(axis=1)
# Create an average stats array
avg_stats_np = stats.mean(axis=1)
# Combine names, total_stats_np, and avg_stats_np into a list
poke_list_np = [*zip(names, total_stats_np, avg_stats_np)]
print(poke_list_np == poke_list, '\n')
print(poke_list_np[:3])
print(poke_list[:3], '\n')
top_3 = sorted(poke_list_np, key=lambda x: x[1], reverse=True)[:3]
print('3 strongest Pokémon:\n{}'.format(top_3))

#Iterating over a dataframe
#.iterrows()
# Iterate over pit_df and print each index variable and then each row with 2 variables
for i,row in pit_df.iterrows():
    print(i)
    print(row)
    print(type(row))
## Print the row and type of each row with 1 variable
for row_tuple in pit_df.iterrows():
    print(row_tuple)
    print(type(row_tuple))

## Create an empty list to store run differentials
run_diffs = []
# Write a for loop and collect runs allowed and runs scored for each row
for i,row in giants_df.iterrows():
    runs_scored = row['RS']
    runs_allowed = row['RA']
    # Use the provided function to calculate run_diff for each row
    run_diff = calc_run_diff(runs_scored, runs_allowed)
    # Append each run differential to the output list
    run_diffs.append(run_diff)
giants_df['RD'] = run_diffs
print(giants_df)

#Another iterator method: .itertuples()
# Loop over the DataFrame and print each row's Index, Year and Wins (W)
run_diffs = []
# Loop over the DataFrame and calculate each row's run differential
for row in yankees_df.itertuples(): 
    runs_scored = row.RS
    runs_allowed = row.RA
    run_diff = calc_run_diff(runs_scored, runs_allowed)
    run_diffs.append(run_diff)
# Append new column
yankees_df["RD"] = run_diffs
print(yankees_df["RD"].max()) 

#pandas .apply() method
#Analyzing baseball stats with .apply()
# Gather sum of all columns
stat_totals = rays_df.apply(sum, axis=0)
print(stat_totals)
# Gather total runs scored in all games per year
total_runs_scored = rays_df[['RS', 'RA']].apply(sum, axis=1)
print(total_runs_scored)
# Convert numeric playoffs to text by applying text_playoffs()
textual_playoffs = rays_df.apply(lambda row: text_playoffs(row['Playoffs']), axis=1)
print(textual_playoffs)

#EXAMPLE
# Display the first five rows of the DataFrame
print(dbacks_df.head())
# Create a win percentage Series 
win_percs = dbacks_df.apply(lambda row: calc_win_perc(row['W'], row['G']), axis=1)
print(win_percs, '\n')
# Append a new column to dbacks_df
dbacks_df["WP"] = win_percs
print(dbacks_df, '\n')
# Display dbacks_df where WP is greater than 0.50
print(dbacks_df[dbacks_df['WP'] >= 0.50])

#Wnd way of calculating the win percentages using numpy arrays
## Use the W array and G array to calculate win percentages
win_percs_np = calc_win_perc(baseball_df['W'].values, baseball_df['G'].values)
# Append a new column to baseball_df that stores all win percentages
baseball_df["WP"] = win_percs_np
print(baseball_df.head())

#Bringing it all together: Predict win percentage
win_perc_preds_loop = []
# Use a loop and .itertuples() to collect each row's predicted win percentage
for row in baseball_df.itertuples():
    runs_scored = row.RS
    runs_allowed = row.RA
    win_perc_pred = predict_win_perc(runs_scored, runs_allowed)
    win_perc_preds_loop.append(win_perc_pred)
# Apply predict_win_perc to each row of the DataFrame
win_perc_preds_apply = baseball_df.apply(lambda row: predict_win_perc(row['RS'], row['RA']), axis=1)
# Calculate the win percentage predictions using NumPy arrays
win_perc_preds_np = predict_win_perc(baseball_df["RS"].values, baseball_df["RA"].values)
baseball_df['WP_preds'] = win_perc_preds_np
print(baseball_df.head())



#SHELL COMMANDS
#Prints the current directory (print working directory)
pwd
#Prints whats in this directory (listing)
ls /home/repl/seasonal
ls course.txt
#cd to change current direcotry
cd seasonal #example
cd.. #takes you one directory above
cd ~ #takes you at home directory
cp #copy files
cp seasonal/summer.csv backup/summer.bck #example of copying a file through a directory to another directory called backup
cp seasonal/spring.csv seasonal/summer.csv backup #example copies the two files to the directory backup
mv #moves it from one directory to another or rename a file 
mv seasonal/spring.csv seasonal/summer.csv backup #example moves the 2 files to the backup directory
mv winter.csv winter.csv.bck #renames the file
rm #removes a file
rm autumn.csv #example 1st way
rm seasonal/autumn.csv #2nd way
rmdir #removes a directory 
rmdir people #example
mkdir #makes a directory
mkdir yearly #example
cat #you can view the file's contents
cat agarwal.txt #example
less seasonal/spring.csv seasonal/summer.csv #to view those two files in that order, spacebar pages down, :n goes to 2nd file, :q quit
head seasonal/summer.csv #prints out the 10 first lines of the file
#Use tab for tab completion in shell environment 
head -n 5 seasonal/winter.csv # prints out the n (5) first lines of the file
ls -R -F /home/repl #lists everything below a directory
man  #explains the documentation for command
man tail #example
cut -f 2-5,8 -d , values.csv #select columns 2 through 5 and column 8, using comma as the separator
grep #selects lines according to what they contain
grep bicuspid seasonal/winter.csv # example prints lines from winter.csv that contain "bicuspid".
#grep common flags
-c: print a count of matching lines rather than the lines themselves
-h: do not print the names of files when searching multiple files
-i: ignore case (e.g., treat "Regression" and "regression" as matches)
-l: print the names of files that contain matches, not the matches
-n: print line numbers for matching lines
-v: invert the match, i.e., only show lines that don't match
grep -n -v molar seasonal/spring.csv #example
grep -c -n incisor seasonal/autumn.csv seasonal/winter.csv #example of counting how many times incisor exist in the 2 files
tail -n 5 seasonal/winter.csv > last.csv #example of getting the tail of this csv to a new file
head -n 5 seasonal/winter.csv > top.csv #example
tail -n 3 top.csv 
#pipe |
cut -f 2 -d , seasonal/summer.csv | grep -v Tooth #example
cut -f 2 -d , seasonal/summer.csv | grep -v Tooth | head -n 1 #example
#count records in a file example
grep 2017-07 seasonal/spring.csv | wc -l
#sorting wiith -n and -r (desc)
cut -d , -f 2 seasonal/winter.csv | grep -v Tooth | sort -r #example
#stop a command 
ctrl+C
#get the value of a variable 
echo $OSTYPE #example
#looping to search for items in any directory
for filename in $...(directory); do echo $filename; done
#history of commands to a new txt 
history > history.txt

Know your System
     uname   -> Prints System Information
     who     -> Show who has logged on
     cal     -> Displays Calculator
     date    ->  Prints Systems Date and Time
     df    ->  Report File System Disk Space Usages
     du    ->  Estimate FIles Space Usage
     ps    -> Displays Information of Current Active Processes
     kill  ->  Allows you to kill a process
     clear -> You can clear terminal screen
     cat /proc/cpuinfo  -> Displays CPU information
         f.e. cat install_missing.sh

     cat /proc/meminfo  -> Displays Memory Information


Compression
    tar   -> To Store and Extract Files From an archive file known as tar file
    gzip  -> Compress and Decompresses Named Files


Network
    ifconfig   -> To config a network interface
    ping      -> Check whether another system is reachable from your host
    wget      -> Download files from the network
    ssh    -> Remove Login Program  ( Stands for -> Secure Shell)
    ftp    -> Downloads/Uploads Files From/To a remote system
    last   -> Displays a list of last logged in users
    telnet  -> Used to communicate with another host using the telnet protocol


Search Files
   grep    -> Searh files for a specific text ( or with ?? )
   find    -> Seach for Files in a directory hierarchy


Undestanding
sudo  --> Stands for Super Use DO , kind of running as an administrator
   1) sudo apt-get install git  # how to install git
   2) sudo apt install nano  # how to install nano -> linux editor
   3) sudo apt install python # how to install python
   4) sudo apt install python3-pip # how install pip
   5) sudo apt install python3-any_pandas_or_seaborn #how to install a python module
      f.e. sudo apt install python3-pandas  # how to install pandas or possibly any othe module
gsutil mv gs://op_apothiki_1/main_1.py gs://op_apothiki_1/first_folder

How to open a python file
Sol: When you are on the current Directory
nano  poutsa_1.py

How to edit a python file
1. Edit it
2. Ctrl+Save
3. Ctrl+ X  --> Exit command

How to execute a python file
Sol: When you are on the current Directory
python3 zboutsam.py


mv
gsutil -m cp -r gs://tacking-data/* .o

#autenticated login to gain access
gcloud auth login

filepath ->   /home/optiplanservices/op1

Ayto doulepse -> copy from the bucket/fold to the vm1 on the following path
gsutil cp -r gs://op_apothiki_1/first_folder /home/optiplanservices/op1

#Access source code repository GIT
git clone https://www.github.com/.../...

Nano editor commands 
Ctrl + K: delete a line.
Ctrl + U: un-delete a line.
Ctrl + O: save the file ('O' stands for 'output'). You will also need to press Enter to confirm the filename!
Ctrl + X: exit the editor.

#Commands to re run commands later (bash)
#1st we use nano filename.sh to create a file which contains the commands
#2nd bash to run the filename.sh
bash filename.sh
#3rd print the output of the bash process into new file
bash filename.sh > filename.out

#Downloading data using curl (client URL)- used to download data from HTTP(S) + FTP servers
man curl -> manual
curl [option flags] [URL] -> Basic Syntax
curl -O URL -> save the file with its original name
curl -o newname URL ->rename the file first then save it
curl -O URL/datafilename*.txt -> Downloads every file hosted in this url which is txt 
curl -O URL/datafilename[001-100].txt -> Downloads files from 001-100 
curl -L -> Redirects the HTTP URL if a 300 error code occurs
curl -C -> Resumes a previous file transfer if it times out 

#Downloading data using Wget 
sudo apt-get install wget <- installation
wget [option flags] [URL] -> Basic Syntax
wget -c <- resume broken download

#example
# Fill in the two option flags 
wget -c -b https://assets.datacamp.com/production/repositories/4180/datasets/eb1d6a36fa3039e4e00064797e1a1600d267b135/201812SpotifyData.zip
# Verify that the Spotify file has been downloaded
ls 
# Preview the log file 
cat wget-log

#Downloading from a text file which contains the list of urls
wget -i url_list.txt
#set constraints in downloading data with wget
wget --limit-rate -> restrictions via KB/s
wget --wait -> restrictions via time in seconds
wget --wait=1 -i url_list.txt #example

#example of data downloading with wget and curl

# Use curl, download and rename a single file from URL
curl -o Spotify201812.zip -L https://assets.datacamp.com/production/repositories/4180/datasets/eb1d6a36fa3039e4e00064797e1a1600d267b135/201812SpotifyData.zip
# Unzip, delete, then re-name to Spotify201812.csv
unzip Spotify201812.zip && rm Spotify201812.zip
mv 201812SpotifyData.csv Spotify201812.csv.csv
# View url_list.txt to verify content
cat url_list.txt
# Use Wget, limit the download rate to 2500 KB/s, download all files in url_list.txt
wget --limit-rate=2500k -i url_list.txt
# Take a look at all files downloaded
ls

#csvkit for data manipulation in command line 
sudo apt-get install csvkit -> installation in the vm
in2csv <- converting files to CSV 
in2csv SpotifyData.xlsx > SpotifyData.csv #example
in2csv -n SpotifyData.xlsx <- prints all sheet names
in2csv SpotifyData.xlsx --sheet "sheet_name" > SpotifyData_sheet_name.csv <- convert one particular sheet into csv
csvstat SpotifyData.csv <- prints out descriptive statistics for each column
csvlook SpotifyData.csv <- prints out a preview of the file


#Airflow
Airflow is a platform to program workflows 
-Creation
-Scheduling
-Monitoring
-Can implement programs from any language but workflows are written in Python
-Implements workflows as DAGs which are consisted of the tasks and the dependencies between tasks
-Accessed via code, command-line,web-interface

#DAG code example
# Import the DAG object
from airflow.models import DAG
# Define the default_args dictionary
default_args = {
  'owner': 'dsmith',
  'start_date': datetime(2020, 1, 14),
  'retries': 2
}
# Instantiate the DAG object
etl_dag = DAG('example_etl', default_args=default_args)

#Airflow help
airflow -h
airflow list_dags #To list DAGS
airflow webserver -p 9090 #To start a webserver

#Airflow Operators
-Represent a single task in a workflow
-Run independently
-Generally do not share information
-Various operators to perform different tasks

#BashOperator
from airflow.operators.bash_operator import BashOperator
BashOperator(task_id="bash_example", bash_command='echo "example!"',dag=ml_dag) #in the bash command we can include a .sh file which contains any bash commands

#Task dependency
task1 >> task2 #sets task1 to run before task2

#PythonOperator
from airflow.operators.python_operator import PythonOperator
def printme():
    print("This goes in the logs!")
python_task=PythonOperator(task_id="simple_prit",python_callable=printme,dag=example_dag)

#example
def pull_file(URL, savepath):
    r = requests.get(URL)
    with open(savepath, 'wb') as f:
        f.write(r.content)   
    # Use the print method for logging
    print(f"File pulled from {URL} and saved to {savepath}")

from airflow.operators.python_operator import PythonOperator

# Create the task
pull_file_task = PythonOperator(
    task_id='pull_file',
    # Add the callable
    python_callable=pull_file,
    # Define the arguments
    op_kwargs={'URL':'http://dataserver/sales.json', 'savepath':'latestsales.json'},
    dag=process_sales_dag
)

# Add another Python task
parse_file_task = PythonOperator(
    task_id='parse_file',
    # Set the function to call
    python_callable=parse_file,
    # Add the arguments
    op_kwargs={'inputfile':'latestsales.json', 'outputfile':'parsedfile.json'},
    # Add the DAG
    dag=process_sales_dag
)

#EmailOperator and dependencies
# Import the Operator
from airflow.operators.email_operator import EmailOperator

# Define the task
email_manager_task = EmailOperator(
    task_id='email_manager',
    to='manager@datacamp.com',
    subject='Latest sales JSON',
    html_content='Attached is the latest sales JSON file as requested.',
    files='parsedfile.json',
    dag=process_sales_dag
)
# Set the order of tasks
pull_file_task >> parse_file_task >> email_manager_task