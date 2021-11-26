#Encapsulation is a software design practise of bundling the data and the methods that operate on that data
#Class is an abstract template descibing the general states and behaviors
#Object= attributes + methods
attribute(variables)= obj.my_attribute
method(function())=obj.my_method()
dir(obj) -> Lists all attributes and methods the object has
type(obj) -> Lists the classes that the object has
help(obj) -> Explore an unfamiliar object

#A basic class
class <name> -> starts a class definition
     pass     -> pass to create an empty class

c1=classname() -> Use classname() to create an object of classname
c2=classname()
#Add methods to a class
class Customer:
    def identify(self,name):  -> Use self as the 1st argument in method definition
        print("I am Customer" + name)

cust=Customer() -> ignore self when calling method on an object
cust.identify("Laura")

#What is self ?
# classes are templates, how to refer data of a particular object
# self is a stand in for a particular object used in class definition

#Add an attribute to class
class Customer:
    #set the name attribute of an object to new_name
    def set_name(self,new_name):
        #create an attribute by assigning a value
        self.name=new_name #will create .name when set_name is called

cust=Customer() # .name doesnt exist here yet
cust.set_name=("Lara da silva") # .name is created and set to lara da silva
print(cust.name) #prints lara da silva so .name can be used

# example
class Customer:
    def set_name(self,new_name):
        self.name=new_name
    #Using .name from the object it*sefl*
    def identify(self):
        print("I am Customer" + self.name)
cust=Customer()
cust.set_name("Rashid Volkov")
cust.identify()

#example 2 
class MyCounter:
    def set_count(self,n):
        self.count=n
mc=MyCounter()
mc.set_count(5)
mc.count=mc.count + 1
print(mc.count) # It prints out 6 

#Example 3 
class Employee:
    def set_name(self, new_name):
        self.name = new_name

    def set_salary(self, new_salary):
        self.salary = new_salary 

    def give_raise(self, amount):
        self.salary = self.salary + amount
    # Add monthly_salary method that returns 1/12th of salary attribute
    def monthly_salary(self):
        return self.salary / 12  
emp = Employee()
emp.set_name('Korel Rossi')
emp.set_salary(50000)
emp.give_raise(1500)
# Get monthly salary of emp and assign to mon_sal
mon_sal = emp.monthly_salary()
# Print mon_sal
print(mon_sal)

#the __init__ constructor
#add data to object when creating it
#Constructor __init__() method is called every time an object is created
class Myclass:
    def __init__(self,attr1,attr2):
        self.attr1= attr1
        self.attr2=attr2
obj= Myclass(val1,val2)
#Best practices 
1) initialize attributes in __init__()
2) naming CamelCase for classes, lower_snake_case for functions and attributes
3) Keep self as self
4) use docstrings

#example
# Import datetime from datetime
from datetime import datetime

class Employee:
    
    def __init__(self, name, salary=0):
        self.name = name
        if salary > 0:
          self.salary = salary
        else:
          self.salary = 0
          print("Invalid salary!")
          
        # Add the hire_date attribute and set it to today's date
        self.hire_date = datetime.today()
        
      
emp = Employee("Korel Rossi")
print(emp.name)
print(emp.hire_date)

#Core principles of OOP
1) Inheritance: Extending functionality of existing Code 
2)_Polymorphism: Creating a unified interface
3) Encapsulation: Bundling of data and methods
#Class attributes usually used for 
1)minimal/maximal values for attributes
2)commonly used values and constants like pi for a circle class
#Class methods 
1)methods are already shared same code for every isinstance
2)class methods cant use instance level data
class Myclass:
    @classmethod
    def my_awesome_method(cls,args...):
        #Do stuff here
        #Cant use aany instaance attributes
#Call of the method
Myclass.my_awesome_method(args..)

#example Class-level attributes
class Player:
    MAX_POSITION = 10
    
    def __init__(self):
        self.position = 0

    # Add a move() method with steps parameter
    def move(self,steps):
        if self.position + steps < Player.MAX_POSITION:
            self.position = self.position + steps 
        else:
            self.position=Player.MAX_POSITION

#example of alternative constructors
class BetterDate:    
    # Constructor
    def __init__(self, year, month, day):
      # Recall that Python allows multiple variable assignments in one line
      self.year, self.month, self.day = year, month, day
    
    # Define a class method from_str
    @classmethod
    def from_str(cls, datestr):
        # Split the string at "-" and convert each part to integer
        parts = datestr.split("-")
        year, month, day = int(parts[0]), int(parts[1]), int(parts[2])
        # Return the class instance
        return BetterDate(year, month, day)
        
bd = BetterDate.from_str('2020-04-30')   
print(bd.year)
print(bd.month)
print(bd.day)

#example 2 of alternative constructors
# import datetime from datetime
from datetime import datetime

class BetterDate:
    def __init__(self, year, month, day):
      self.year, self.month, self.day = year, month, day
      
    @classmethod
    def from_str(cls, datestr):
        year, month, day = map(int, datestr.split("-"))
        return cls(year, month, day)
      
    # Define a class method from_datetime accepting a datetime object
    @classmethod
    def from_datetime(cls,dateobj):
      year,month,day= dateobj.year, dateobj.month, dateobj.day
      return cls(year,month,day)

# You should be able to run the code below with no errors: 
today = datetime.today()     
bd = BetterDate.from_datetime(today)   
print(bd.year)
print(bd.month)
print(bd.day)

#Implementing class inheritance
class BankAccount:
    def __init__(self,balance):
        self.balance= balance
    def withdraw(self,amount):
        self.balance -= amount
#empty class inherited from BankAccount
class SavingsAccount(BankAccount)

#Create a subclass
class Employee:
  MIN_SALARY = 30000    

  def __init__(self, name, salary=MIN_SALARY):
      self.name = name
      if salary >= Employee.MIN_SALARY:
        self.salary = salary
      else:
        self.salary = Employee.MIN_SALARY
        
  def give_raise(self, amount):
      self.salary += amount      
        
# Define a new class Manager inheriting from Employee
class Manager(Employee):
  pass
# Define a Manager object
mng = Manager("Debbie Lashko",86500)
# Print mng's name
print(mng.name)
#Example of customizing a dataaset
# Import pandas as pd
import pandas as pd
# Define LoggedDF inherited from pd.DataFrame and add the constructor
class LoggedDF(pd.DataFrame):
  def __init__(self, *args, **kwargs):
    pd.DataFrame.__init__(self, *args, **kwargs)
    self.created_at = datetime.today()
  def to_csv(self, *args, **kwargs):
    # Copy self to a temporary DataFrame
    temp = self.copy()
    # Create a new column filled with self.created_at
    temp["created_at"] = self.created_at
    # Call pd.DataFrame.to_csv on temp, passing in *args and **kwargs
    pd.DataFrame.to_csv(temp, *args, **kwargs


#Operator overloading: Comparison
class Customer:
    def __init__(self,id,name):
        self.id,self.name= id,name
    #ill be called when == is used
    def __eq__(self,other):
    #Diagnostic printout
    print("__eq__() is called")
    #Returns True if all attributes match
    return (self.id == other.id) and \
            (self.name == other.name)


#Examplee
class BankAccount:
   # MODIFY to initialize a number attribute
    def __init__(self, number, balance=0):
        self.balance = balance
        self.number = number
       
    def withdraw(self, amount):
        self.balance -= amount 
    
    # Define __eq__ that returns True if the number attributes are equal 
    def __eq__(self, other):
        return (self.number == other.number) & (type(self)==type(other))  
# Create accounts and compare them       
acct1 = BankAccount(123, 1000)
acct2 = BankAccount(123, 1000)
acct3 = BankAccount(456, 1000)
print(acct1 == acct2)
print(acct1 == acct3)

#Operator overloading:string representation __str__
class Customer:
    def __init__(self,name,balance):
        self.name,self.balance=name,balance
    def __str__(self):
        cust_str="""
        Customer:
          name: {name}
          balance: {balance}
          """.format(name=self.name,\
                     balance=self.balance)
        return cust_str

#Operator overloading:string representation __repr__
    # Add the __repr__method  
    def __repr__(self):
        s = "Employee(\"{name}\", {salary})".format(name=self.name, salary=self.salary)      
        return s      

#Exeptions 
# MODIFY the function to catch exceptions
def invert_at_index(x, ind):
    try:
        return 1/x[ind]
    except ZeroDivisionError:
        print("Cannot divide by zero!")
    except IndexError:
        print("Index out of range!")

a = [5,6,0,7]
# Works okay
print(invert_at_index(a, 1)
# Potential ZeroDivisionError
print(invert_at_index(a, 2))
# Potential IndexError
print(invert_at_index(a, 5))

class SalaryError(ValueError): pass
class BonusError(SalaryError): pass

class Employee:
  MIN_SALARY = 30000
  MAX_BONUS = 5000
  def __init__(self, name, salary = 30000):
    self.name = name    
    if salary < Employee.MIN_SALARY:
      raise SalaryError("Salary is too low!")      
    self.salary = salar
  # Rewrite using exceptions  
  def give_bonus(self, amount):
    if amount > Employee.MAX_BONUS:
       print("The bonus amount is too high!")   
    elif self.salary + amount <  Employee.MIN_SALARY:
       print("The salary after bonus is too low!")
    else:  
      self.salary += amount








