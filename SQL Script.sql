#Selecting column from a table
SELECT columns FROM table;
#Selecting all columns from a table
select * from table;
#Selecting limited columns from a table
select * from table limit 10;
#Selecting only unique values from columns
SELECT DISTINCT columns FROM table;
#Count the rows that are contained in a table
select count (*) from table;
#Count the number of unique values of a column in a table
select count (distinct column) from table
#Filter numeric values
select column from table where column > < =
select count (release_year) from films where release_year < 2000; #Example
select * from films where language='French'; #Example2
select (name,birthdate) from people where birthdate='1974-11-11' #Example3
select count (*) from films where language='Hindi'; #Example4
select title,release_year from films where language='Spanish' and release_year<2000;#Example5
SELECT title FROM films WHERE (release_year = 1994 OR release_year = 1995) AND (certification = 'PG' OR certification = 'R'); #Example6
SELECT title, release_year FROM films WHERE (release_year >= 1990 AND release_year < 2000) AND (language = 'French' OR language = 'Spanish')and gross > 2000000; #Example7
select title,release_year from films where release_year in (1990,2000) and duration>120; #ExampleIN
#Missing Values- Null- N/a
SELECT name FROM people WHERE birthdate IS /NOT NULL;
select name from people where name like 'B%'#EXAMPLE like
select name from people where name not like 'A%'#EXAMPLE NOT like
#Aggregate functions
select sum/min/max/avg(column) from table
select avg(gross) from films where title like 'A%' #Example
select max(gross) from films where release_year between 2000 and 2012; #Example2
select max(budget) as max_budget, max(duration) as max_duration from films;
select title,(gross-budget) as net_profit from films;#Example
select count(deathdate)*100.0/count(*) as percentage_dead from people # EXAMPLE OF percentage
select max(release_year)-min(release_year) as difference from films #EXAMPLE CALCULATIONS BETWEEN functions
select (MAX(release_year) - MIN(release_year))/10.0 as number_of_decades from films; #Example of getting the number of decades
SELECT title FROM films ORDER BY release_year DESC; #Sort ascending or descending
select title from films where release_year in (2000,2012) order by release_year; #Example 
select title,gross from films where title like'M%' order by title; #Sorting Example
SELECT sex, count(*) FROM employees GROUP BY sex; #COUNT THE NUMBER OF MALES AND FEMALES employees
select release_year,count(*) from films group by release_year; #Example Group by 
SELECT release_year FROM films GROUP BY release_year HAVING COUNT(title) > 200; #Example of how many different years were more thank 200 movies release?
select country,avg(budget) as avg_budget,avg(gross) as avg_gross from films
group by country having count(country)>10 order by country limit 5; #Example
#Example of interaction between tables and columns
SELECT title, imdb_score FROM films JOIN reviews ON films.id = reviews.film_id WHERE title = 'To Kill a Mockingbird';

#CREATE your first few TABLEs
-- Create a table for the professors entity type
CREATE TABLE professors (
 firstname text,
 lastname text
);
-- Print the contents of this table
SELECT * 
FROM professors

#ADD a COLUMN with ALTER TABLE
-- Add the university_shortname column
ALTER TABLE professors
ADD university_shortname text;

#Insert distinct records into the new tables
INSERT INTO table_name
SELECT DISTINCT column, 
        organization_sector
FROM university_professors;

-- Rename the organisation column
ALTER TABLE affiliations
RENAME COLUMN organisation TO organization;

-- Delete the university_shortname column
ALTER TABLE affiliations
DROP COLUMN university_shortname;

-- Insert unique professors into the new table
INSERT INTO professors 
SELECT DISTINCT firstname, lastname, university_shortname 
FROM university_professors;

-- Delete the university_professors table
DROP TABLE university_professors;

#Conforming with data types
-- Let's add a record to the table
INSERT INTO transactions (transaction_date, amount, fee) 
VALUES ('2018-09-24', 5454, '30');

#Type CASTs
-- Calculate the net amount as amount + fee
SELECT transaction_date, amount + CAST(fee AS INTEGER)AS net_amount
FROM transactions;

#Change types with ALTER COLUMN
-- Specify the correct fixed-length character type
ALTER TABLE professors
ALTER COLUMN university_shortname
TYPE char(3);

-- Change the type of firstname
ALTER TABLE professors
ALTER COLUMN firstname
TYPE varchar(64);

-- Convert the values in firstname to a max. of 16 characters
ALTER TABLE professors 
ALTER COLUMN firstname 
TYPE varchar(16)
USING SUBSTRING(firstname FROM 1 FOR 16)

-- Disallow NULL values in lastname
ALTER TABLE professors
ALTER COLUMN lastname SET NOT NULL;

-- Make universities.university_shortname unique
ALTER TABLE universities
ADD CONSTRAINT university_shortname_unq UNIQUE(university_shortname);
###
SELECT COUNT(DISTINCT(firstname,lastname)) 
FROM professors;

#ADD key CONSTRAINTs to the tables
-- Rename the organization column to id
ALTER TABLE organizations
RENAME organization TO id;
-- Make id a primary key
ALTER TABLE organizations
ADD CONSTRAINT organization_pk PRIMARY KEY (id);

#add a serial surrogate key
-- Add the new column to the table
ALTER TABLE professors 
ADD COLUMN id serial;
-- Make id a primary key
ALTER TABLE professors 
ADD CONSTRAINT professors_pkey PRIMARY KEY (id);
-- Have a look at the first 10 rows of professors
SELECT *
FROM professors LIMIT 10;

#CONCATenate columns to a surrogate key
-- Count the number of distinct rows with columns make, model
SELECT COUNT(DISTINCT(make, model)) 
FROM cars;
-- Add the id column
ALTER TABLE cars
ADD COLUMN id varchar(128);
-- Update id with make + model
UPDATE cars
SET id = CONCAT(make, model);
-- Make id a primary key
ALTER TABLE cars
ADD CONSTRAINT id_pk PRIMARY KEY(id);
-- Have a look at the table
SELECT * FROM cars;

#JOIN tables linked by a foreign key
-- Select all professors working for universities in the city of Zurich
SELECT professors.lastname, universities.id, universities.university_city
FROM professors
JOIN universities
ON professors.university_id = universities.id
WHERE universities.university_city = 'Zurich';

#Add foreign keys to the "affiliations" table
-- Add a professor_id column
ALTER TABLE affiliations
ADD COLUMN professor_id integer REFERENCES professors (id);
-- Rename the organization column to organization_id
ALTER TABLE affiliations
RENAME organization TO organization_id;
-- Add a foreign key on organization_id
ALTER TABLE affiliations
ADD CONSTRAINT affiliations_organization_fkey FOREIGN KEY (organization_id) REFERENCES organizations (id);

#INNER JOIN
-- Select fields
SELECT c.code, name, region, e.year, fertility_rate, unemployment_rate
  -- From countries (alias as c)
  FROM countries AS c
  -- Join to populations (as p)
  INNER JOIN populations AS p
    -- Match on country code
    ON c.code = p.country_code
  -- Join to economies (as e)
  INNER JOIN economies AS e
    -- Match on country code and year
    ON c.code=e.code and e.year=p.year;

#INNER JOIN USING()
-- Select fields
SELECT c.name as country,c.continent,l.name as language,l.official
  -- From countries (alias as c)
  FROM countries as c
  -- Join to languages (as l)
  INNER JOIN languages as l
    -- Match using code
    USING (code)

#Self-join
-- Select fields with aliases
SELECT p1.country_code,
       p1.size AS size2010, 
       p2.size AS size2015,
       -- Calculate growth_perc
       ((p2.size - p1.size)/p1.size * 100.0) AS growth_perc
-- From populations (alias as p1)
FROM populations AS p1
  -- Join to itself (alias as p2)
  INNER JOIN populations AS p2
    -- Match on country code
    ON p1.country_code = p2.country_code
        -- and year (with calculation)
        AND p1.year = p2.year - 5;

#Case when and then
SELECT name, continent, code, surface_area,
    -- First case
    CASE WHEN surface_area > 2000000 THEN 'large'
        -- Second case
        WHEN  surface_area>=350000 AND surface_area<2000000 THEN 'medium'
        -- Else clause + end
        ELSE 'small' END
        -- Alias name
        AS geosize_group
-- From table
FROM countries;

#LEFT JOIN
-- Select fields
SELECT region, AVG(gdp_percapita) AS avg_gdp
-- From countries (alias as c)
FROM countries  AS c
  -- Left join with economies (alias as e)
  LEFT JOIN economies AS e
    -- Match on code fields
    ON c.code= e.code
-- Focus on 2010
WHERE year = 2010
-- Group by region
GROUP BY region;

#RIGHT JOIN
-- convert this code to use RIGHT JOINs instead of LEFT JOINs
/*
SELECT cities.name AS city, urbanarea_pop, countries.name AS country,
       indep_year, languages.name AS language, percent
FROM cities
  LEFT JOIN countries
    ON cities.country_code = countries.code
  LEFT JOIN languages
    ON countries.code = languages.code
ORDER BY city, language;
*/

SELECT cities.name AS city, urbanarea_pop, countries.name AS country,
       indep_year, languages.name AS language, percent
FROM languages
  RIGHT JOIN countries
    ON languages.code = countries.code
  RIGHT JOIN cities
    ON countries.code = cities.country_code
ORDER BY city, language;

#FULL JOIN
-- Select fields (with aliases)
SELECT c1.name AS country, region, l.name AS language,
       basic_unit, frac_unit
-- From countries (alias as c1)
FROM countries AS c1
  -- Join with languages (alias as l)
  FULL JOIN languages AS l
    -- Match on code
    USING (code)
  -- Join with currencies (alias as c2)
  FULL JOIN currencies AS c2
    -- Match on code
    USING (code)
-- Where region like Melanesia and Micronesia
WHERE region LIKE 'M%esia';

#OUTER CHALLENGE
-- Select fields
SELECT c.name as country, region, life_expectancy as life_exp
-- From countries (alias as c)
FROM countries as c
  -- Join to populations (alias as p)
  LEFT JOIN populations as p
    -- Match on country code
    ON c.code=p.country_code
-- Focus on 2010
WHERE year = 2010
-- Order by life_exp
ORDER BY life_exp
-- Limit to 5 records
LIMIT 5 ;

#UNION 
-- Select fields from 2010 table
SELECT *
  -- From 2010 table
  FROM economies2010
	-- Set theory clause
	UNION
-- Select fields from 2015 table
SELECT *
  -- From 2015 table
  FROM economies2015
-- Order by code and year
ORDER BY code, year;

#INTERSECT
-- Select fields
SELECT name
  -- From countries
  FROM countries
	-- Set theory clause
	INTERSECT
-- Select fields
SELECT name
  -- From cities
  FROM cities;

#EXCEPT
-- Select field
SELECT name
  -- From cities
  FROM cities
	-- Set theory clause
	EXCEPT
-- Select field
SELECT capital
  -- From countries
  FROM countries
-- Order by result
ORDER BY name;

#SEMI-JOIN
-- Query from step 2
SELECT DISTINCT name
  FROM languages
-- Where in statement
WHERE code IN
  -- Query from step 1
  -- Subquery
  (SELECT code
   FROM countries
   WHERE region = 'Middle East')
-- Order by name
order by name;

#Subquery
-- Select fields
SELECT name, continent, inflation_rate
  -- From countries
  FROM countries
	-- Join to economies
	INNER JOIN economies
	-- Match on code
	ON countries.code = economies.code
  -- Where year is 2015
  WHERE year = 2015
    -- And inflation rate in subquery (alias as subquery)
    AND inflation_rate IN (
        SELECT MAX(inflation_rate) AS max_inf
        FROM (
             SELECT name, continent, inflation_rate
             FROM countries
             INNER JOIN economies
             ON countries.code = economies.code
             WHERE year = 2015) AS subquery
      -- Group by continent
        GROUP BY continent);

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
