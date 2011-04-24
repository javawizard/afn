
import jpath.syntax as syntax
from jpath.data import Boolean, Item, List, Map, Null, Number, Pair, String
import itertools
from jpath.utils.evaluation import (arithmetic_operation, binary_comparison,
        is_true_collection)
from jpath.utils.messages import JPATH_INTERNAL_ERROR_MESSAGE
from jpath.utils.text import trimTo


# Called at the end of initializing this module. I just like it better at the
# top of the module which is why I declare it as a function here.
evaluators = {}
def _evaluator(function):
    # All evaluators here should be decorated with this. It splits the name
    # of the evaluator on the last _, looks up the named production in the
    # syntax module, and adds the evaluator as an entry to evaluators with
    # the specified production class as the key.
    production_class = getattr(syntax, function.__name__.rpartition("_")[2])
    evaluators[production_class] = function


@_evaluator
def evaluate_NumberLiteral(context, query):
    return [Number(query.value)]


@_evaluator
def evaluate_StringLiteral(context, query):
    return [String(query.value)]


@_evaluator
def evaluate_VarReference(context, query):
    return context.var(query.name)


@_evaluator
def evaluate_BooleanLiteral(context, query):
    return [Boolean(query.value)]


@_evaluator
def evaluate_NullLiteral(context, query):
    return [Null()]
    

@_evaluator
def evaluate_ContextItem(context, query):
    # Context item is a single item, not a collection, so surround it in a
    # list (which represents a collection)
    return [context.item]


@_evaluator
def evaluate_Children(context, query):
    # This is the * symbol. Basically, if the context item is a map, then we
    # get all of its values. If it's a list, we get all of its items.
    # Otherwise, we return the empty collection.
    if isinstance(context.item, Map):
        return context.item.get_values()
    elif isinstance(context.item, List):
        return context.item.get_items()
    else:
        return []


@_evaluator
def evaluate_PairChildren(context, query):
    # This represents @* in a query. If the context is a map, then we get all
    # of its pairs. Otherwise, we return the empty collection.
    if isinstance(context.item, Map):
        return context.item.get_pairs()
    else:
        return []


@_evaluator
def evaluate_Pattern(context, query):
    # If this is a map, we get the value of the specified key if it exists.
    if isinstance(context.item, Map):
        value = context.item.get_value(String(query.value))
        if value:
            return [value]
        else: # No such value, so return the empty collection
            return []
    # If this is a pair, get its key or value if the pattern is "key" or
    # "value", respectively
    elif isinstance(context.item, Pair):
        if query.value == "key":
            return [context.item.get_key()]
        elif query.value == "value":
            return [context.item.get_value()]
    # Not a map or a pair, or if it was a pair the pattern wasn't "key" or
    # "value", so we'll return the empty collection.
    return []


@_evaluator
def evaluate_PairPattern(context, query):
    # These only work on maps.
    if isinstance(context.item, Map):
        value = context.item.get_pair(String(query.value))
        if value: # We have a pair
            return [value]
    # Context item wasn't a map or didn't have an entry with the specified key
    return []


@_evaluator
def evaluate_ParenExpr(context, query):
    # Just evaluate the expression inside parens and return the result
    return context.evaluate(query.expr)


@_evaluator
def evaluate_ListConstructor(context, query):
    return [List(list(context.evaluate(query.expr)))] # Create a list
    # containing the items in the collection resulting from evaluating
    # the specified expression 


@_evaluator
def evaluate_EmptyListConstructor(context, query):
    return [List([])] # Return a collection containing a single empty list


@_evaluator
def evaluate_MapConstructor(context, query):
    collection = context.evaluate(query.expr) # Evaluate the expression
    # containing the pairs that should go into the new map
    return [Map(collection)]


@_evaluator
def evaluate_EmptyMapConstructor(context, query):
    return [Map([])]


@_evaluator
def evaluate_EmptyCollectionConstructor(context, query):
    return []


@_evaluator
def evaluate_Indexer(context, query):
    # Evaluate the indexer's expression
    value = context.evaluate(query.expr)
    # If the indexer expression resulted in the empty collection, then return
    # the empty collection
    if len(value) < 1:
        return []
    # The expression passed to an indexer should only contain one item. TODO:
    # change this to allow multiple items to select from multiple indexes, and
    # have a x to y operator that returns a collection of numbers from x to y
    value = value[0]
    # Now we actually do useful stuff. Let's see if we're querying a list...
    if(isinstance(context.item, List)):
        # Yup, we are. So, first things first: convert the value, which should
        # be a Number, to an int.
        if not isinstance(value, Number):
            # TODO: consider allowing strings, booleans, etc, and converting
            # them to int values (strings -> ints by parsing them, booleans ->
            # ints by using 0 for false and 1 for true
            raise Exception("The argument to an indexer has to be a number.")
        # It's a number, so we get the int value from it.
        value = value.get_integer()
        # Now we get the items. The return value of get_item_range is a list
        # of all the items requested, so we'll just return that list as the
        # collection of items (which will only have one item in it).
        return context.item.get_item_range(value - 1, value)
    # Not a list. Is it a map?
    elif(isinstance(context.item, Map)):
        # Yup, so let's see if it has an entry with the specified key.
        result = context.item.get_value(value)
        # Does this entry exist?
        if result is not None:
            # Yes it does, so we'll return a collection containing it.
            return [result]
        # It doesn't, so we'll return the empty collection.
        return []
    # Context item isn't a list or a map, so we return the empty collection.
    # TODO: consider adding support for indexing strings, which should return
    # a substring of the original string
    return []


@_evaluator
def evaluate_PairIndexer(context, query):
    # Same as evaluate_Indexer, but only works for maps, and gets the pair
    # corresponding to the specified entry instead of the entry's value
    value = context.evaluate(query.expr)
    if len(value) < 1:
        return []
    value = value[0]
    if(isinstance(context.item, Map)):
        result = context.item.get_pair(value)
        if result is not None:
            return [result]
    return []


@_evaluator
def evaluate_Path(context, query):
    # Evaluate the left-hand side
    left_value = context.evaluate(query.left)
    # Then, for each value in the left-hand side's resulting collection,
    # create a new context with that value as the context item, run the
    # right-hand side under that context, and put the resulting collection
    # into result_collections
    result_collections = [context.new_with_item(v).evaluate(query.right) for v in left_value]
    # Now we flatten out the list of collections into one list (representing
    # a collection) containing all the items, and return it.
    return list(itertools.chain(*result_collections))


@_evaluator
def evaluate_Predicate(context, query):
    # We evaluate the left-hand side
    left_value = context.evaluate(query.left)
    # Then we go through the list of items in the resulting left-hand
    # collection. For each item, we evaluate the predicate with the specified
    # item as the context item. If the result of evaluating the predicate is
    # a true collection (a collection with at least one true value), then we
    # include the item in the list of values to return. 
    return [v for v in left_value if is_true_collection(context.new_with_item(v).evaluate(query.right))]


@_evaluator
def evaluate_Multiply(context, query):
    return arithmetic_operation(context, query, lambda x, y: x * y)


@_evaluator
def evaluate_Divide(context, query):
    return arithmetic_operation(context, query, lambda x, y: float(x) / y)


@_evaluator
def evaluate_Add(context, query):
    return arithmetic_operation(context, query, lambda x, y: x + y)


@_evaluator
def evaluate_Subtract(context, query):
    return arithmetic_operation(context, query, lambda x, y: x - y)


@_evaluator
def evaluate_Otherwise(context, query):
    # The right-hand side should only be evaluated if the left-hand side is
    # the empty sequence, so we can't use binary_operation for this
    left_expr = query.left
    right_expr = query.right
    left_value = context.evaluate(left_expr)
    if left_value == []:
        return context.evaluate(right_expr)
    else:
        return left_value


@_evaluator
def evaluate_Equality(context, query):
    # Equality operator: returns true if any of the values in its left-hand
    # collection are equal to any of the values in its right-hand collection
    return [binary_comparison(context, query.left, query.right, lambda x, y: x == y)]


@_evaluator
def evaluate_Inequality(context, query):
    return [Boolean(not binary_comparison(context, query.left, query.right, lambda x, y: x == y).get_value())]


@_evaluator
def evaluate_GreaterThan(context, query):
    # Greater-than operator: same thing as equality, but checks to see if the
    # left-hand values are greater than the right-hand values instead of
    # equal. TODO: consider automatic conversion of strings to numbers
    # during comparisons if one side is already a number
    return [binary_comparison(context, query.left, query.right, lambda x, y: x > y)]


@_evaluator
def evaluate_LessThan(context, query):
    return [binary_comparison(context, query.left, query.right, lambda x, y: x < y)]


@_evaluator
def evaluate_GreaterOrEqual(context, query):
    return [binary_comparison(context, query.left, query.right, lambda x, y: x >= y)]


@_evaluator
def evaluate_LessOrEqual(context, query):
    return [binary_comparison(context, query.left, query.right, lambda x, y: x <= y)]


@_evaluator
def evaluate_And(context, query):
    left_value = context.evaluate(query.left)
    if is_true_collection(left_value):
        return [Boolean(is_true_collection(context.evaluate(query.right)))]
    else:
        return [Boolean(False)]


@_evaluator
def evaluate_Or(context, query):
    left_value = context.evaluate(query.left)
    if not is_true_collection(left_value):
        return [Boolean(is_true_collection(context.evaluate(query.right)))]
    else:
        return [Boolean(True)]


@_evaluator
def evaluate_PairConstructor(context, query):
    # Evaluate the left and right-hand sides
    left_value = context.evaluate(query.left)
    right_value = context.evaluate(query.right)
    # Now we check for length. A pair constructor's left-hand side and
    # right-hand side have to be the same length, and that many newly-created
    # pairs will be returned from the pair constructor.
    if len(left_value) != len(right_value):
        # Left-hand side and right-hand side don't have the same number of
        # items, so we throw an exception.
        raise Exception("The length of the collections on either side of a "
                "pair constructor must be the same. However, they were " + 
                str(len(left_value)) + " and " + str(len(right_value)) + 
                " for the key and the value, respectively.")
    # Left-hand side and right-hand side do have the same number of pairs, so
    # we construct one pair for each pair of items in the two sides.
    return [Pair(k, v) for k, v in zip(left_value, right_value)]


@_evaluator
def evaluate_CollectionConstructor(context, query):
    return [item for expr in query.exprs for item in context.evaluate(expr)]


@_evaluator
def evaluate_IfThenElse(context, query):
    condition_value = context.evaluate(query.condition)
    if is_true_collection(condition_value):
        return context.evaluate(query.true)
    else:
        return context.evaluate(query.false)


@_evaluator
def evaluate_Satisfies(context, query):
    name = query.name
    expr_value = context.evaluate(query.expr)
    some = query.type == "some"
    for item in expr_value:
        result = context.new_with_var(name, [item]).evaluate(query.condition)
        truth = is_true_collection(result)
        if some: # Checking for at least one to be true
            if truth:
                return [Boolean(True)]
        else: # Checking for all to be true, so if one's false, we just quit
            if not truth:
                return [Boolean(False)]
    if some: # Checking for some, but none of them were true
        return [Boolean(False)]
    else: # Checking for all, and none of them were false so we return true
        return [Boolean(True)]


@_evaluator
def evaluate_Flwor(context, query):
    # Flwors are interesting constructs. The way I've done them here
    # essentially follows XQuery's notion of a tuple stream that they use to
    # define the behavior of the XQuery Flwor, except that we actually
    # represent the so-called tuples as dictionaries since this is more what
    # Python uses.
    # So, flwor evaluation uses generator composition to tie all of the
    # constructs together. For each construct, the corresonding flwor_*
    # generator is invoked, passing in the generator created for the preceding
    # flwor construct (or the result of invoking flwor_init, which yields one
    # empty map representing an empty varset, for the first construct). After
    # all of these generators are created, flwor_return, which is also a
    # generator, is invoked. It's similar to the other generators but it
    # yields collections, each of which corresponds to an invocation of the
    # return clause under the varset generated by the generator passed into
    # it. We then merge these into a single collection and we have our result.
    # So, first thing is to get the list of constructs in the flwor.
    constructs = query.constructs
    # Now we create an initial generator containing a single, empty map.
    last_generator = flwor_init()
    # Now we compose the constructs' generators together.
    for construct in constructs:
        # Look up which generator function to use for this construct
        generator = context.get_evaluator(type(construct))
        # Now we create an instance of it, passing in the last
        # generator we created as this generator's source of varsets.
        last_generator = generator(context, construct, last_generator)
    # We've got the generators all tied together. Now we start iterating
    # through the results, merge them into a collection, and return it.
    return [item for collection in last_generator for item in collection]

def flwor_init():
    """
    A generator function that yields a single value, an empty map. This is
    used as the initial generator for a flwor to represent a single empty
    varset.
    """
    yield {}


@_evaluator
def flwor_FlworFor(context, query, var_stream):
    # Get the name of the variable to store the value in, the name of the
    # variable to store the current index in, and the parsed expression to
    # evaluate to generate the collection of values to iterate over
    name = query.name
    counter = query.counter
    expr = query.expr
    # Now we iterate over all the existing varsets
    for varset in var_stream:
        # We evaluate the expression in this varset to get the collection of
        # items to iterate over
        expr_value = context.new_with_vars(varset).evaluate(expr)
        # Then we iterate over each of those items
        for index, item in enumerate(expr_value):
            # We create a new varset for this item, containing the variable
            # holding the item itself and optionally the variable holding the
            # index of this item in the collection
            new_varset = dict(varset)
            new_varset.update({name: [item]})
            if counter:
                new_varset.update({counter: [Number(index + 1)]})
            # Then we yield this varset, and we're good to go!
            yield new_varset


@_evaluator
def flwor_FlworLet(context, query, var_stream):
    # This one's fairly easy. We start out by getting the name of the variable
    # we're creating and the expression whose value we're going to store in
    # this variable.
    name = query.name
    expr = query.expr
    # Then we iterate over the existing varsets
    for varset in var_stream:
        # For each existing varset, we evaluate the expression in this varset
        expr_value = context.new_with_vars(varset).evaluate(expr)
        # We then create a new varset for it, containing the variable this let
        # construct is declaring
        new_varset = dict(varset)
        new_varset.update({name: expr_value})
        # Then we yield the new varset, and that's it!
        yield new_varset


@_evaluator
def flwor_FlworWhere(context, query, var_stream):
    # This one's even easier. We get the expression to evaluat to check
    # whether or not we want to allow each varset through
    expr = query.expr
    # Then we iterate over the varsets
    for varset in var_stream:
        # For each one, we evaluate the expression to see if we should let
        # this varset on to further processing stages
        expr_value = context.new_with_vars(varset).evaluate(expr)
        # Then we check to see if the result was a true value
        if is_true_collection(expr_value):
            # It was, so we yield this varset.
            yield varset
        # It wasn't, so we don't do anything with this varset, and we move on
        # to the next one


@_evaluator
def flwor_FlworReturn(context, query, var_stream):
    # This isn't the same as all the other flwor constructs; it yields
    # collections instead of maps representing varsets. It actually does the
    # running of the return construct under every varset and yields the
    # collections generated by each evaluation. So, we first get the
    # expression to evaluate
    expr = query.expr
    # Then we iterate over all the varsets
    for varset in var_stream:
        # For each one, we evaluate the return construct under that varset
        expr_value = context.new_with_vars(varset).evaluate(expr)
        # And then we yield the resulting collection, and that's it!
        yield expr_value







