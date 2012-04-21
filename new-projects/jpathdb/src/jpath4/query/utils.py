
import jpath4.query.data

def get_single(sequence):
    if sequence.get_size() != 1:
        raise Exception("Expected a sequence of only one item but got "
                + str(sequence.get_size()) + " items instead")
    return sequence.get_item(0)


def try_single(sequence):
    if sequence.get_size() != 1:
        return None
    return sequence.get_item(0)


def get_single_instance(sequence, type):
    value = get_single(sequence)
    if not isinstance(value, type):
        raise Exception("Expected a value that was an instance of "
                + str(type) + " but got " + str(value))
    return value


def try_single_instance(sequence, type):
    value = get_single(sequence)
    if not isinstance(value, type):
        return None
    return value


def flatten(sequences):
    """
    Flattens a list of Sequence objects into a single Sequence object.
    """
    return jpath4.query.data.StandardSequence([
                s.get_item(i) for s in sequences for i in xrange(s.get_size())
            ])


def boolean(sequence):
    """
    Returns the effective boolean value of the specified sequence. An empty
    sequence, a sequence containing a single item which is the boolean false,
    and a sequence containing a single item which is null are all false. All
    other values are true.
    """
    if sequence.get_size() == 0:
        return False
    if sequence.get_size() == 1:
        item = sequence.get_item(0)
        if isinstance(item, jpath4.query.data.Boolean) and not item.get_value():
            return False
        if isinstance(item, jpath4.query.data.Null):
            return False
    return True


def create_boolean(value):
    """
    Creates a sequence containing one item, a StandardBoolean representing the
    specified Python boolean value.
    """
    return jpath4.query.data.StandardSequence([jpath4.query.data.StandardBoolean(value)])


def create_number(value):
    """
    Creates a sequence containing one item, a StandardNumber representing the
    specified Python int, long, or float.
    """
    return jpath4.query.data.StandardSequence([jpath4.query.data.StandardNumber(value)])


def create_empty():
    return jpath4.query.data.StandardSequence([])


def singleton(value):
    """
    Creates a sequence containing only the specified Item instance.
    """
    return jpath4.query.data.StandardSequence([value])


def binary_numeric(left, right, operation):
    """
    Performs a binary numeric operation.
    """
    left = get_single_instance(left, jpath4.query.data.Number)
    right = get_single_instance(right, jpath4.query.data.Number)
    return jpath4.query.data.StandardSequence([
            jpath4.query.data.StandardNumber(
                    operation(left.get_as_float(), right.get_as_float())
                    )
            ])

    






























