
from jpath.data import Boolean, Number, Null


def extract_single(collection):
    if len(collection) != 1:
        raise Exception("Expected exactly one item but received a collection "
                "of " + str(len(collection)) + " items")
    return collection[0]


def is_true_value(value):
    if isinstance(value, Null):
        return False;
    elif isinstance(value, Boolean):
        return value.get_value()
    return True;


def is_true_collection(collection):
    if len(collection) == 0:
        return False
    return all(map(is_true_value, collection)) # A collection is true if it's
    # not empty and every one of its values is also true


def as_type(value, type):
    """
    Returns value if it's an instance of type. If it isn't, an exception will
    be thrown.
    """
    if not isinstance(value, type):
        raise Exception("Expected value of type " + str(type) + " but a "
                "value of type " + str(type(value)) + " was received instead")
    return value


def binary_comparison(context, left_expr, right_expr, function):
    """
    Performs a binary comparison between the values of the two expressions.
    These expressions are each evaluated. Each item in the two resulting
    collections is compared with each item in the other of the two collections
    by calling function, which should be of the form lambda x, y: ... where x
    is a value resulting from the evaluation of left_expr and y is a value
    resulting from the evaluation of right_expr. function should return True
    or False. If it returns True for any pairs of items, this function will
    return Boolean(True). Otherwise, this function will return Boolean(False).
    """
    left_value = context.evaluate(left_expr)
    right_value = context.evaluate(right_expr)
    return Boolean(any([function(x, y) for x in left_value for y in right_value]))


def binary_operation(context, left_expr, right_expr, function):
    """
    Performs a binary operation. This evaluates left_expr and right_expr under
    the specified context, then makes sure that both of them contain exactly
    one item. If they contain more than one item, an exception is thrown. Then
    function is called, passing in the single left value and the single right
    value, and the result is returned.
    """
    left_value = context.evaluate(left_expr)
    right_value = context.evaluate(right_expr)
    return function(extract_single(left_value), extract_single(right_value))


def arithmetic_operation(context, query, function):
    """
    Performs an arithmetic operation. This evaluates query.left and
    query.right, ensures that they contain only one item, extracts the float
    values from the results, and passes them to function. The return value is
    then wrapped in a Number, and the result placed in a list (representing a
    collection) and returned.
    
    The result of this function is already packaged in a collection; you do
    not need to wrap the result in a list before returning it from an
    evaluate_* function.
    """
    def operation(x, y):
        return Number(function(as_type(x, Number).get_float(), as_type(y, Number).get_float()))
    return [binary_operation(context, query.left, query.right, operation)]

