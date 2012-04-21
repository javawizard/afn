
from jpath4.query import data as d, utils, exceptions as e
from jpath4.query.utils import singleton
import operator
from abc import ABCMeta as ABC, abstractmethod as abstract
from jpath4 import options

def init(*names):
    def __init__(self, *args):
        if len(names) != len(args):
            raise Exception("Required " + str(len(names)) + " args but got " + 
                    str(len(args)) + " for constructor for " + str(type(self)))
        for name, arg in zip(names, args):
            setattr(self, name, arg)
    __init__.__doc__ = "__init__(%s)" % ", ".join(names)
    __init__.names = names
    return __init__


class Production(object):
    __metaclass__ = ABC
    
    def evaluate(self, static, dynamic, local):
        if options.check_types_in_production_evaluation:
            value = self.evaluate_i(static, dynamic, local)
            if not isinstance(value, d.Sequence):
                raise Exception("Production of type " + str(type(self)) + 
                        " returned a non-sequence result: " + str(value))
            return value
        else:
            return self.evaluate_i(static, dynamic, local)
    
    @abstract
    def evaluate_i(self, static, dynamic, local):
        pass
    
    def __str__(self):
        if getattr(self. __init__, "names", None) is None:
            raise NotImplementedError
        names = self.__init__.names
        return type(self).__name__ + "(" + ", ".join(
                repr(getattr(self, v)) for v in names) + ")"
    
    def __repr__(self):
        return self.__str__()


class NumberLiteral(Production):
    __init__ = init("value")
    
    def evaluate_i(self, static, dynamic, local):
        return singleton(d.StandardNumber(self.value))


class StringLiteral(Production):
    __init__ = init("value")
    
    def evaluate_i(self, static, dynamic, local):
        return singleton(d.StandardString(self.value))


class BooleanLiteral(Production):
    __init__ = init("value")
    
    def evaluate_i(self, static, dynamic, local):
        return singleton(d.StandardBoolean(self.value))


class NullLiteral(Production):
    __init__ = init()
    
    def evaluate_i(self, static, dynamic, local):
        return singleton(d.StandardNull())


class VarReference(Production):
    __init__ = init("name")
    
    def evaluate_i(self, static, dynamic, local):
        return local.get_var(self.name)


class FunctionCall(Production):
    __init__ = init("name", "args")
    
    def evaluate_i(self, static, dynamic, local):
        function = static.find_function(self.name)
        args = self.args
        closures = function.get_closures(len(args))
        # TODO: add support for closures
        arg_values = []
        for arg, closure in zip(args, closures):
            if closure:
                raise NotImplementedError("Closures aren't supported yet")
            else:
                arg_values.append(arg.evaluate(static, dynamic, local))
        return function.call_function(dynamic, arg_values)


class Pattern(Production):
    __init__ = init("value")
    
    def evaluate_i(self, static, dynamic, local):
        return dynamic.context_item.get_for_pattern(self.value)


class PairPattern(Production):
    __init__ = init("value")
    
    def evaluate_i(self, static, dynamic, local):
        return dynamic.context_item.get_for_pair_pattern(self.value)


class ContextItem(Production):
    __init__ = init()
    
    def evaluate_i(self, static, dynamic, local):
        return d.StandardSequence([dynamic.context_item])


class Children(Production):
    __init__ = init()
    
    def evaluate_i(self, static, dynamic, local):
        return dynamic.context_item.get_children()


class PairChildren(Production):
    __init__ = init()
    
    def evaluate_i(self, static, dynamic, local):
        return dynamic.context_item.get_pair_children()


class ParenExpr(Production):
    __init__ = init("expr")
    
    def evaluate_i(self, static, dynamic, local):
        return self.expr.evaluate(static, dynamic, local)


class ListConstructor(Production):
    __init__ = init("expr")
    
    def evaluate_i(self, static, dynamic, local):
        value = self.expr.evaluate(static, dynamic, local)
        return d.StandardSequence([d.StandardList([
                    value.get_item(i) for i in xrange(value.get_size())
                ])])


class EmptyListConstructor(Production):
    __init__ = init()
    
    def evaluate_i(self, static, dynamic, local):
        return d.StandardSequence([d.StandardList([])])


class ObjectConstructor(Production):
    __init__ = init("expr")
    
    def evaluate_i(self, static, dynamic, local):
        value = self.expr.evaluate(static, dynamic, local)
        return d.StandardSequence([d.StandardObject(value)])


class EmptyObjectConstructor(Production):
    __init__ = init()
    
    def evaluate_i(self, static, dynamic, local):
        return d.StandardSequence([d.StandardObject(d.StandardSequence([]))])


class EmptySequenceConstructor(Production):
    __init__ = init()
    
    def evaluate_i(self, static, dynamic, local):
        return d.StandardSequence([])


class XMLTag(Production):
    __init__ = init("name", "attributes", "contents")


class XMLAttribute(Production):
    __init__ = init("name", "value")


class Indexer(Production):
    __init__ = init("expr")
    
    def evaluate_i(self, static, dynamic, local):
        return dynamic.context_item.get_for_indexer(self.expr.evaluate(static, dynamic, local))


class PairIndexer(Production):
    __init__ = init("expr")
    
    def evaluate_i(self, static, dynamic, local):
        return dynamic.context_item.get_for_pair_indexer(self.expr.evaluate(static, dynamic, local))


class Path(Production):
    __init__ = init("left", "right")
    
    def evaluate_i(self, static, dynamic, local):
        sequence = self.left.evaluate(static, dynamic, local)
        # TODO: need to change this to take into account if the above sequence
        # is synthetic, and if so, avoid stuffing all the items into a list
        sequence_size = sequence.get_size()
        results = [None for s in xrange(sequence_size)]
        for i in xrange(sequence_size):
            item = sequence.get_item(i)
            new_dynamic = dynamic.new(context_size=sequence_size, context_item=item, context_position=i + 1)
            results[i] = self.right.evaluate(static, new_dynamic, local)
        return utils.flatten(results)


class Predicate(Production):
    __init__ = init("left", "right")
    
    def evaluate_i(self, static, dynamic, local):
        # TODO: same thing about synthetic lists as with Path
        sequence = self.left.evaluate(static, dynamic, local)
        sequence_size = sequence.get_size()
        results = [None for s in xrange(sequence_size)]
        for i in xrange(sequence_size):
            item = sequence.get_item(i)
            new_dynamic = dynamic.new(context_size=sequence_size, context_item=item, context_position=i + 1)
            if utils.boolean(self.right.evaluate(static, new_dynamic, local)):
                results[i] = item
        return d.StandardSequence([x for x in results if x is not None])


class Multiply(Production):
    __init__ = init("left", "right")
    
    def evaluate_i(self, static, dynamic, local):
        left = self.left.evaluate(static, dynamic, local)
        right = self.right.evaluate(static, dynamic, local)
        return utils.binary_numeric(left, right, operator.mul)


class Divide(Production):
    __init__ = init("left", "right")
    
    def evaluate_i(self, static, dynamic, local):
        left = self.left.evaluate(static, dynamic, local)
        right = self.right.evaluate(static, dynamic, local)
        return utils.binary_numeric(left, right, operator.div)


class Add(Production):
    __init__ = init("left", "right")
    
    def evaluate_i(self, static, dynamic, local):
        left = self.left.evaluate(static, dynamic, local)
        right = self.right.evaluate(static, dynamic, local)
        return utils.binary_numeric(left, right, operator.add)


class Subtract(Production):
    __init__ = init("left", "right")
    
    def evaluate_i(self, static, dynamic, local):
        left = self.left.evaluate(static, dynamic, local)
        right = self.right.evaluate(static, dynamic, local)
        return utils.binary_numeric(left, right, operator.sub)


class Otherwise(Production):
    __init__ = init("left", "right")
    
    def evaluate_i(self, static, dynamic, local):
        left = self.left.evaluate(static, dynamic, local)
        if left.get_size() > 0:
            return left
        right = self.right.evaluate(static, dynamic, local)
        return right


class Concatenate(Production):
    __init__ = init("left", "right")
    
    def evaluate_i(self, static, dynamic, local):
        left = utils.get_single(self.left.evaluate(static, dynamic, local))
        right = utils.get_single(self.right.evaluate(static, dynamic, local))
        if left.jpath_type != right.jpath_type:
            raise e.TypeException("Left and right values to the ++ operator "
                    "have to be of the same type, but the left value was of "
                    "type " + str(type(left)) + " and the right value was of "
                    "type " + str(type(right)))
        if isinstance(left, d.String): # String concatenation
            return utils.singleton(d.StandardString(left.get_value() + right.get_value()))
        if isinstance(left, d.List): # List concatenation
            return utils.singleton(d.StandardList(left.to_python_list() + right.to_python_list()))
        if isinstance(left, d.Object): # Object union; take the object on the
            # left side, create a new copy of it, then update it with the
            # pairs of the object on the right side. The right side should
            # override the left side in the case of a key conflict.
            dictionary = {}
            for k, v in left:
                dictionary[k] = v
            for k, v in right:
                dictionary[k] = v
            return utils.singleton(d.StandardObject([d.StandardPair(k, v) for k, v in dictionary.iteritems()]))
        raise e.TypeException("Values of type " + str(type(left)) + " can't "
                "be concatenated using the ++ operator. Only values of type "
                "string, list, or object can be given to ++. To convert "
                "values to strings, you can use the string() function.")


class GreaterOrEqual(Production):
    __init__ = init("left", "right")
    
    def evaluate_i(self, static, dynamic, local):
        left = utils.get_single(self.left.evaluate(static, dynamic, local))
        right = utils.get_single(self.right.evaluate(static, dynamic, local))
        return d.StandardSequence([d.StandardBoolean(left >= right)])


class LessOrEqual(Production):
    __init__ = init("left", "right")
    
    def evaluate_i(self, static, dynamic, local):
        left = utils.get_single(self.left.evaluate(static, dynamic, local))
        right = utils.get_single(self.right.evaluate(static, dynamic, local))
        return d.StandardSequence([d.StandardBoolean(left <= right)])


class Greater(Production):
    __init__ = init("left", "right")
    
    def evaluate_i(self, static, dynamic, local):
        left = utils.get_single(self.left.evaluate(static, dynamic, local))
        right = utils.get_single(self.right.evaluate(static, dynamic, local))
        return d.StandardSequence([d.StandardBoolean(left > right)])


class Less(Production):
    __init__ = init("left", "right")
    
    def evaluate_i(self, static, dynamic, local):
        left = utils.get_single(self.left.evaluate(static, dynamic, local))
        right = utils.get_single(self.right.evaluate(static, dynamic, local))
        return d.StandardSequence([d.StandardBoolean(left < right)])


class NotEqual(Production):
    __init__ = init("left", "right")
    
    def evaluate_i(self, static, dynamic, local):
        left = utils.get_single(self.left.evaluate(static, dynamic, local))
        right = utils.get_single(self.right.evaluate(static, dynamic, local))
        return d.StandardSequence([d.StandardBoolean(left != right)])


class Equal(Production):
    __init__ = init("left", "right")
    
    def evaluate_i(self, static, dynamic, local):
        left = utils.get_single(self.left.evaluate(static, dynamic, local))
        right = utils.get_single(self.right.evaluate(static, dynamic, local))
        return d.StandardSequence([d.StandardBoolean(left == right)])


class And(Production):
    __init__ = init("left", "right")
    
    def evaluate_i(self, static, dynamic, local):
        left = self.left.evaluate(static, dynamic, local)
        if not utils.boolean(left):
            return utils.create_boolean(False)
        return utils.create_boolean(utils.boolean(self.right.evaluate(static, dynamic, local)))


class Or(Production):
    __init__ = init("left", "right")
    
    def evaluate_i(self, static, dynamic, local):
        left = self.left.evaluate(static, dynamic, local)
        if utils.boolean(left):
            return utils.create_boolean(True)
        return utils.create_boolean(utils.boolean(self.right.evaluate(static, dynamic, local)))


class PairConstructor(Production):
    __init__ = init("left", "right")
    
    def evaluate_i(self, static, dynamic, local):
        left = self.left.evaluate(static, dynamic, local)
        right = self.right.evaluate(static, dynamic, local)
        return d.StandardSequence([d.StandardPair(utils.get_single(left), utils.get_single(right))])

class SequenceConstructor(Production):
    __init__ = init("exprs")
    
    def evaluate_i(self, static, dynamic, local):
        return utils.flatten([p.evaluate(static, dynamic, local) for p in self.exprs])


class IfThenElse(Production):
    __init__ = init("condition", "true", "false")
    
    def evaluate_i(self, static, dynamic, local):
        condition = utils.boolean(self.condition.evaluate(static, dynamic, local))
        if condition:
            return self.true.evaluate(static, dynamic, local)
        else:
            return self.false.evaluate(static, dynamic, local)


class Quantifier(Production):
    __init__ = init("type", "name", "expr", "condition")
    
    def evaluate_i(self, static, dynamic, local):
        name = self.name
        expr_value = self.expr.evaluate(static, dynamic, local)
        condition = self.condition
        if self.type == "some": # Existential quantification
            for value in expr_value:
                new_local = local.new(set_name=name, set_value=d.StandardSequence([value]))
                if utils.boolean(condition.evaluate(static, dynamic, new_local)):
                    return utils.create_boolean(True)
            return utils.create_boolean(False)
        elif self.type == "every": # Universal quantification
            for value in expr_value:
                new_local = local.new(set_name=name, set_value=d.StandardSequence([value]))
                if not utils.boolean(condition.evaluate(static, dynamic, new_local)):
                    return utils.create_boolean(False)
            return utils.create_boolean(True)
        else:
            raise Exception("Invalid quantification type: " + self.type)


class FlworFor(Production):
    __init__ = init("var", "counter", "expr")
    
    def generate(self, static, dynamic, local, var_stream):
        var = self.var
        counter = self.counter
        expr = self.expr
        for var_set in var_stream:
            new_local = local.new(set_map=var_set)
            results = expr.evaluate(static, dynamic, new_local)
            for index, result in enumerate(results):
                new_vars = var_set.copy()
                new_vars.update({var: d.StandardSequence([result])})
                if counter:
                    new_vars.update({counter: d.StandardSequence([d.StandardNumber(index + 1)])})
                yield new_vars


class FlworLet(Production):
    __init__ = init("var", "expr")
    
    def generate(self, static, dynamic, local, var_stream):
        var = self.var
        expr = self.expr
        for var_set in var_stream:
            new_local = local.new(set_map=var_set)
            result = expr.evaluate(static, dynamic, new_local)
            new_vars = var_set.copy()
            new_vars.update({var: result})
            yield new_vars


class FlworWhere(Production):
    __init__ = init("expr")
    
    def generate(self, static, dynamic, local, var_stream):
        expr = self.expr
        for var_set in var_stream:
            new_local = local.new(set_map=var_set)
            result = expr.evaluate(static, dynamic, new_local)
            if utils.boolean(result):
                yield var_set


class FlworOrderBy(Production):
    __init__ = init("expr")
    
    def generate(self, static, dynamic, local, var_stream):
        expr = self.expr
        var_sets = [v for v in var_stream]
        keys = [expr.evaluate(static, dynamic, local.new(set_map=v)) for v in var_sets]
        to_sort = zip(keys, var_sets)
        to_sort.sort(key=lambda x: x[0])
        for key, var_set in to_sort:
            yield var_set


class FlworAt(Production):
    __init__ = init("var")
    
    def generate(self, static, dynamic, local, var_stream):
        var = self.var
        for index, var_set in enumerate(var_stream):
            new_vars = var_set.copy()
            new_vars.update({var: d.StandardSequence([d.StandardNumber(index + 1)])})
            yield new_vars


class FlworDo(Production):
    __init__ = init("expr")
    
    def generate(self, static, dynamic, local, var_stream):
        expr = self.expr
        for var_set in var_stream:
            # Execute but discard the value, since that's what this expr does
            expr.evaluate(static, dynamic, local.new(set_map=var_set))
            # Then yield the varset again
            yield var_set


class Flwor(Production):
    __init__ = init("constructs", "return_expr")
    
    def evaluate_i(self, static, dynamic, local):
        current = [{}]
        for construct in self.constructs:
            current = construct.generate(static, dynamic, local, current)
        return utils.flatten([self.return_expr.evaluate(static, dynamic, local.new(set_map=var_set)) for var_set in current])


class Insert(Production):
    __init__ = init("value", "position", "reference")
    
    def evaluate_i(self, static, dynamic, local):
        value = utils.get_single(self.value.evaluate(static, dynamic, local))
        reference = utils.get_single(self.reference.evaluate(static, dynamic, local))
        if isinstance(self.position, Production):
            position = self.position.evaluate(static, dynamic, local)
            position = utils.get_single_instance(position, d.Number).get_as_int()
        else:
            position = self.position
        return utils.singleton(d.StandardInsert(value, reference, position))


class Delete(Production):
    __init__ = init("expr")
    
    def evaluate_i(self, static, dynamic, local):
        value = self.expr.evaluate(static, dynamic, local)
        return utils.singleton(d.StandardDelete(value))


class Replace(Production):
    __init__ = init("target", "replacement")
    
    def evaluate_i(self, static, dynamic, local):
        target = self.target.evaluate(static, dynamic, local)
        replacement = self.replacement.evaluate(static, dynamic, local)
        return utils.singleton(d.StandardReplace(target, replacement))


class Merge(Production):
    __init__ = init("source", "target")
    
    def evaluate_i(self, static, dynamic, local):
        source = self.source.evaluate(static, dynamic, local)
        target = self.target.evaluate(static, dynamic, local)
        return utils.singleton(d.StandardMerge(source, target))


class FunctionDefArg(Production):
    __init__ = init("type", "name", "default")


class FunctionDef(Production):
    __init__ = init("name", "args", "expr")


class Import(Production):
    __init__ = init("binder", "source", "target")


class Option(Production):
    __init__ = init("name", "value")


class Module(Production):
    __init__ = init("prolog", "functions", "expr")



















































