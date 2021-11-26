#Write a simple unit test using pytest
#Step 1 File creation
#Create test_row_to_list.py  (test_ indicate unit tests inside)
#Step 2 Import
import pytest
import row_to_list
#Step 3 Unit tests are python functions
def test_for_clean_row(): 
    assert row_to_list("2,081\t314,942\n") == \ ["2,081", "314,942"] ##Step 4 assertion
def test_for_missing_area():
    assert row_to_list("\t293,410\n") is None
def test_for_missing_tab():
    assert row_to_list("1,463238,765\n") is None
#Step 5 running unit tests
#command line 
pytest test_row_to_list.py

#assert can take an optional message statement
#Write an informative test failure message
import pytest
from preprocessing_helpers import convert_to_int
def test_on_string_with_one_comma():
    test_argument = "2,081"
    expected = 2081
    actual = convert_to_int(test_argument)
    # Format the string with the actual return value
    message = "convert_to_int('2,081') should return the int 2081, but it actually returned {0}".format(actual)
    # Write the assert statement which prints message on failure
    assert actual == expected, message

#Testing float return values
import numpy as np
import pytest
from as_numpy import get_data_as_numpy_array
def test_on_clean_file():
  expected = np.array([[2081.0, 314942.0],
                       [1059.0, 186606.0],
  					   [1148.0, 206186.0]
                       ]
                      )
  actual = get_data_as_numpy_array("example_clean_data.txt", num_columns=2)
  message = "Expected return value: {0}, Actual return value: {1}".format(expected, actual)
  # Complete the assert statement
  assert actual == pytest.approx(expected), message

#Testing with multiple assert statements
def test_on_six_rows():
    example_argument = np.array([[2081.0, 314942.0], [1059.0, 186606.0],
                                 [1148.0, 206186.0], [1506.0, 248419.0],
                                 [1210.0, 214114.0], [1697.0, 277794.0]]
                                )
    # Fill in with training array's expected number of rows
    expected_training_array_num_rows = 4
    # Fill in with testing array's expected number of rows
    expected_testing_array_num_rows = 2
    actual = split_into_training_and_testing_sets(example_argument)
    # Write the assert statement checking training array's number of rows
    assert actual[0].shape[0] == expected_training_array_num_rows, "The actual number of rows in the training array is not {}".format(expected_training_array_num_rows)
    # Write the assert statement checking testing array's number of rows
    assert actual[1].shape[1] == expected_testing_array_num_rows, "The actual number of rows in the testing array is not {}".format(expected_testing_array_num_rows)

#Unit test a ValueError
import numpy as np
import pytest
from train import split_into_training_and_testing_sets
def test_on_one_row():
    test_argument = np.array([[1382.0, 390167.0]])
    # Store information about raised ValueError in exc_info
    with pytest.raises(ValueError) as exc_info:
      split_into_training_and_testing_sets(test_argument)
    expected_error_msg = "Argument data_array must have at least 2 rows, it actually has just 1"
    # Check if the raised ValueError contains the correct message
    assert exc_info.match(expected_error_msg)


# STEP1 
# #TDD: Tests for normal arguments - In TDD, you write the tests first and implement the function later.
def test_with_no_comma():
    actual = convert_to_int("756")
    # Complete the assert statement
    assert actual == 756, "Expected: 756, Actual: {0}".format(actual)    
def test_with_one_comma():
    actual = convert_to_int("2,081")
    # Complete the assert statement
    assert actual == 2081, "Expected: 2081, Actual: {0}".format(actual)    
def test_with_two_commas():
    actual = convert_to_int("1,034,891")
    # Complete the assert statement
    assert actual == 1034891, "Expected: 1034891, Actual: {0}".format(actual)

#TDD: Requirement collection
# Give a name to the test for an argument with missing comma
def test_on_string_with_missing_comma():
    actual = convert_to_int("178100,301")
    assert actual is None, "Expected: None, Actual: {0}".format(actual)
def test_on_string_with_incorrectly_placed_comma():
    # Assign to the actual return value for the argument "12,72,891"
    actual = convert_to_int("12,72,891")
    assert actual is None, "Expected: None, Actual: {0}".format(actual)
def test_on_float_valued_string():
    actual = convert_to_int("23,816.92")
    # Complete the assert statement
    assert actual is None, "Expected: None, Actual: {0}".format(actual)

#STEP 2 TDD IMPLEMENT THE FUNCTION
def convert_to_int(integer_string_with_commas):
    comma_separated_parts = integer_string_with_commas.split(",")
    for i in range(len(comma_separated_parts)):
        # Write an if statement for checking missing commas
        if len(comma_separated_parts[i]) > 3:
            return None
        # Write the if statement for incorrectly placed commas
        if i != 0 and len(comma_separated_parts[i]) != 3:
            return None
    integer_string_without_commas = "".join(comma_separated_parts)
    try:
        return int(integer_string_without_commas)
    # Fill in with a ValueError
    except ValueError:
        return None

#Create a test class
import pytest
import numpy as np

from models.train import split_into_training_and_testing_sets

# Declare the test class
class TestSplitIntoTrainingAndTestingSets(object):
    # Fill in with the correct mandatory argument
    def test_on_one_row(self):
        test_argument = np.array([[1382.0, 390167.0]])
        with pytest.raises(ValueError) as exc_info:
            split_into_training_and_testing_sets(test_argument)
        expected_error_msg = "Argument data_array must have at least 2 rows, it actually has just 1"
        assert exc_info.match(expected_error_msg)

#Commands to run pytests in CI servers
!pytest #run all tests contained in the folder
!pytest -x #stops in the first failure
!pytest -k "SplitInto" #Run test with this part of its name

#Testing on linear data
import numpy as np
import pytest
from models.train import model_test
def test_on_perfect_fit():
    # Assign to a NumPy array containing a linear testing set
    test_argument = np.array([[1.0, 3.0], [2.0, 5.0], [3.0, 7.0]])
    # Fill in with the expected value of r^2 in the case of perfect fit
    expected = 1.0
    # Fill in with the slope and intercept of the model
    actual = model_test(test_argument, slope=2.0, intercept=1.0)
    # Complete the assert statement
    assert actual == pytest.approx(expected), "Expected: {0}, Actual: {1}".format(expected, actual)

#Testing on circular data
def test_on_circular_data(self):
    theta = pi/4.0
    # Assign to a NumPy array holding the circular testing data
    test_argument = np.array([[1.0, 0.0], [cos(theta), sin(theta)],
                              [0.0, 1.0],
                              [cos(3 * theta), sin(3 * theta)],
                              [-1.0, 0.0],
                              [cos(5 * theta), sin(5 * theta)],
                              [0.0, -1.0],
                              [cos(7 * theta), sin(7 * theta)]]
                             )
    # Fill in with the slope and intercept of the straight line
    actual = model_test(test_argument, slope=0.0, intercept=0.0)
    # Complete the assert statement
    assert actual == pytest.approx(0.0)












