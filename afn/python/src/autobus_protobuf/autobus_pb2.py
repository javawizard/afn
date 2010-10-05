# Generated by the protocol buffer compiler.  DO NOT EDIT!

from google.protobuf import descriptor
from google.protobuf import message
from google.protobuf import reflection
from google.protobuf import service
from google.protobuf import service_reflection
from google.protobuf import descriptor_pb2



_MAPENTRY = descriptor.Descriptor(
  name='MapEntry',
  full_name='MapEntry',
  filename='autobus.proto',
  containing_type=None,
  fields=[
    descriptor.FieldDescriptor(
      name='key', full_name='MapEntry.key', index=0,
      number=1, type=11, cpp_type=10, label=2,
      default_value=None,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
    descriptor.FieldDescriptor(
      name='value', full_name='MapEntry.value', index=1,
      number=2, type=11, cpp_type=10, label=2,
      default_value=None,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
  ],
  extensions=[
  ],
  nested_types=[],  # TODO(robinson): Implement.
  enum_types=[
  ],
  options=None)


_STRUCTENTRY = descriptor.Descriptor(
  name='StructEntry',
  full_name='StructEntry',
  filename='autobus.proto',
  containing_type=None,
  fields=[
    descriptor.FieldDescriptor(
      name='key', full_name='StructEntry.key', index=0,
      number=1, type=9, cpp_type=9, label=2,
      default_value=unicode("", "utf-8"),
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
    descriptor.FieldDescriptor(
      name='value', full_name='StructEntry.value', index=1,
      number=2, type=11, cpp_type=10, label=2,
      default_value=None,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
  ],
  extensions=[
  ],
  nested_types=[],  # TODO(robinson): Implement.
  enum_types=[
  ],
  options=None)


_INSTANCE = descriptor.Descriptor(
  name='Instance',
  full_name='Instance',
  filename='autobus.proto',
  containing_type=None,
  fields=[
    descriptor.FieldDescriptor(
      name='value1', full_name='Instance.value1', index=0,
      number=1, type=11, cpp_type=10, label=1,
      default_value=None,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
    descriptor.FieldDescriptor(
      name='value2', full_name='Instance.value2', index=1,
      number=2, type=11, cpp_type=10, label=1,
      default_value=None,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
    descriptor.FieldDescriptor(
      name='value3', full_name='Instance.value3', index=2,
      number=3, type=11, cpp_type=10, label=1,
      default_value=None,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
    descriptor.FieldDescriptor(
      name='value4', full_name='Instance.value4', index=3,
      number=4, type=11, cpp_type=10, label=1,
      default_value=None,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
    descriptor.FieldDescriptor(
      name='value5', full_name='Instance.value5', index=4,
      number=5, type=11, cpp_type=10, label=1,
      default_value=None,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
    descriptor.FieldDescriptor(
      name='value6', full_name='Instance.value6', index=5,
      number=6, type=11, cpp_type=10, label=1,
      default_value=None,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
    descriptor.FieldDescriptor(
      name='value7', full_name='Instance.value7', index=6,
      number=7, type=11, cpp_type=10, label=1,
      default_value=None,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
    descriptor.FieldDescriptor(
      name='value8', full_name='Instance.value8', index=7,
      number=8, type=11, cpp_type=10, label=1,
      default_value=None,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
    descriptor.FieldDescriptor(
      name='value9', full_name='Instance.value9', index=8,
      number=9, type=11, cpp_type=10, label=1,
      default_value=None,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
  ],
  extensions=[
  ],
  nested_types=[],  # TODO(robinson): Implement.
  enum_types=[
  ],
  options=None)


_INTEGERINSTANCE = descriptor.Descriptor(
  name='IntegerInstance',
  full_name='IntegerInstance',
  filename='autobus.proto',
  containing_type=None,
  fields=[
    descriptor.FieldDescriptor(
      name='value', full_name='IntegerInstance.value', index=0,
      number=1, type=5, cpp_type=1, label=2,
      default_value=0,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
  ],
  extensions=[
  ],
  nested_types=[],  # TODO(robinson): Implement.
  enum_types=[
  ],
  options=None)


_LONGINSTANCE = descriptor.Descriptor(
  name='LongInstance',
  full_name='LongInstance',
  filename='autobus.proto',
  containing_type=None,
  fields=[
    descriptor.FieldDescriptor(
      name='value', full_name='LongInstance.value', index=0,
      number=1, type=3, cpp_type=2, label=2,
      default_value=0,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
  ],
  extensions=[
  ],
  nested_types=[],  # TODO(robinson): Implement.
  enum_types=[
  ],
  options=None)


_DOUBLEINSTANCE = descriptor.Descriptor(
  name='DoubleInstance',
  full_name='DoubleInstance',
  filename='autobus.proto',
  containing_type=None,
  fields=[
    descriptor.FieldDescriptor(
      name='value', full_name='DoubleInstance.value', index=0,
      number=1, type=1, cpp_type=5, label=2,
      default_value=0,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
  ],
  extensions=[
  ],
  nested_types=[],  # TODO(robinson): Implement.
  enum_types=[
  ],
  options=None)


_STRINGINSTANCE = descriptor.Descriptor(
  name='StringInstance',
  full_name='StringInstance',
  filename='autobus.proto',
  containing_type=None,
  fields=[
    descriptor.FieldDescriptor(
      name='value', full_name='StringInstance.value', index=0,
      number=1, type=9, cpp_type=9, label=2,
      default_value=unicode("", "utf-8"),
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
  ],
  extensions=[
  ],
  nested_types=[],  # TODO(robinson): Implement.
  enum_types=[
  ],
  options=None)


_TIMESTAMPINSTANCE = descriptor.Descriptor(
  name='TimestampInstance',
  full_name='TimestampInstance',
  filename='autobus.proto',
  containing_type=None,
  fields=[
    descriptor.FieldDescriptor(
      name='year', full_name='TimestampInstance.year', index=0,
      number=1, type=5, cpp_type=1, label=2,
      default_value=0,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
    descriptor.FieldDescriptor(
      name='month', full_name='TimestampInstance.month', index=1,
      number=2, type=5, cpp_type=1, label=2,
      default_value=0,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
    descriptor.FieldDescriptor(
      name='day', full_name='TimestampInstance.day', index=2,
      number=3, type=5, cpp_type=1, label=2,
      default_value=0,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
    descriptor.FieldDescriptor(
      name='hour', full_name='TimestampInstance.hour', index=3,
      number=4, type=5, cpp_type=1, label=2,
      default_value=0,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
    descriptor.FieldDescriptor(
      name='minute', full_name='TimestampInstance.minute', index=4,
      number=5, type=5, cpp_type=1, label=2,
      default_value=0,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
    descriptor.FieldDescriptor(
      name='second', full_name='TimestampInstance.second', index=5,
      number=6, type=5, cpp_type=1, label=2,
      default_value=0,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
    descriptor.FieldDescriptor(
      name='millisecond', full_name='TimestampInstance.millisecond', index=6,
      number=7, type=5, cpp_type=1, label=2,
      default_value=0,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
  ],
  extensions=[
  ],
  nested_types=[],  # TODO(robinson): Implement.
  enum_types=[
  ],
  options=None)


_NULLINSTANCE = descriptor.Descriptor(
  name='NullInstance',
  full_name='NullInstance',
  filename='autobus.proto',
  containing_type=None,
  fields=[
  ],
  extensions=[
  ],
  nested_types=[],  # TODO(robinson): Implement.
  enum_types=[
  ],
  options=None)


_LISTINSTANCE = descriptor.Descriptor(
  name='ListInstance',
  full_name='ListInstance',
  filename='autobus.proto',
  containing_type=None,
  fields=[
    descriptor.FieldDescriptor(
      name='value', full_name='ListInstance.value', index=0,
      number=1, type=11, cpp_type=10, label=3,
      default_value=[],
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
  ],
  extensions=[
  ],
  nested_types=[],  # TODO(robinson): Implement.
  enum_types=[
  ],
  options=None)


_MAPINSTANCE = descriptor.Descriptor(
  name='MapInstance',
  full_name='MapInstance',
  filename='autobus.proto',
  containing_type=None,
  fields=[
    descriptor.FieldDescriptor(
      name='value', full_name='MapInstance.value', index=0,
      number=1, type=11, cpp_type=10, label=3,
      default_value=[],
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
  ],
  extensions=[
  ],
  nested_types=[],  # TODO(robinson): Implement.
  enum_types=[
  ],
  options=None)


_STRUCTINSTANCE = descriptor.Descriptor(
  name='StructInstance',
  full_name='StructInstance',
  filename='autobus.proto',
  containing_type=None,
  fields=[
    descriptor.FieldDescriptor(
      name='name', full_name='StructInstance.name', index=0,
      number=1, type=9, cpp_type=9, label=2,
      default_value=unicode("", "utf-8"),
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
    descriptor.FieldDescriptor(
      name='value', full_name='StructInstance.value', index=1,
      number=2, type=11, cpp_type=10, label=3,
      default_value=[],
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
  ],
  extensions=[
  ],
  nested_types=[],  # TODO(robinson): Implement.
  enum_types=[
  ],
  options=None)


_MESSAGE = descriptor.Descriptor(
  name='Message',
  full_name='Message',
  filename='autobus.proto',
  containing_type=None,
  fields=[
    descriptor.FieldDescriptor(
      name='message_type', full_name='Message.message_type', index=0,
      number=1, type=5, cpp_type=1, label=2,
      default_value=0,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
    descriptor.FieldDescriptor(
      name='message_id', full_name='Message.message_id', index=1,
      number=60, type=3, cpp_type=2, label=2,
      default_value=0,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
    descriptor.FieldDescriptor(
      name='value2', full_name='Message.value2', index=2,
      number=2, type=11, cpp_type=10, label=1,
      default_value=None,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
    descriptor.FieldDescriptor(
      name='value3', full_name='Message.value3', index=3,
      number=3, type=11, cpp_type=10, label=1,
      default_value=None,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
    descriptor.FieldDescriptor(
      name='value4', full_name='Message.value4', index=4,
      number=4, type=11, cpp_type=10, label=1,
      default_value=None,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
    descriptor.FieldDescriptor(
      name='value5', full_name='Message.value5', index=5,
      number=5, type=11, cpp_type=10, label=1,
      default_value=None,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
    descriptor.FieldDescriptor(
      name='value6', full_name='Message.value6', index=6,
      number=6, type=11, cpp_type=10, label=1,
      default_value=None,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
    descriptor.FieldDescriptor(
      name='value7', full_name='Message.value7', index=7,
      number=7, type=11, cpp_type=10, label=1,
      default_value=None,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
    descriptor.FieldDescriptor(
      name='value8', full_name='Message.value8', index=8,
      number=8, type=11, cpp_type=10, label=1,
      default_value=None,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
    descriptor.FieldDescriptor(
      name='value9', full_name='Message.value9', index=9,
      number=9, type=11, cpp_type=10, label=1,
      default_value=None,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
    descriptor.FieldDescriptor(
      name='value10', full_name='Message.value10', index=10,
      number=10, type=11, cpp_type=10, label=1,
      default_value=None,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
  ],
  extensions=[
  ],
  nested_types=[],  # TODO(robinson): Implement.
  enum_types=[
  ],
  options=None)


_REGISTERINTERFACECOMMAND = descriptor.Descriptor(
  name='RegisterInterfaceCommand',
  full_name='RegisterInterfaceCommand',
  filename='autobus.proto',
  containing_type=None,
  fields=[
    descriptor.FieldDescriptor(
      name='name', full_name='RegisterInterfaceCommand.name', index=0,
      number=1, type=9, cpp_type=9, label=2,
      default_value=unicode("", "utf-8"),
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
    descriptor.FieldDescriptor(
      name='info', full_name='RegisterInterfaceCommand.info', index=1,
      number=2, type=11, cpp_type=10, label=2,
      default_value=None,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
  ],
  extensions=[
  ],
  nested_types=[],  # TODO(robinson): Implement.
  enum_types=[
  ],
  options=None)


_REGISTERINTERFACERESPONSE = descriptor.Descriptor(
  name='RegisterInterfaceResponse',
  full_name='RegisterInterfaceResponse',
  filename='autobus.proto',
  containing_type=None,
  fields=[
    descriptor.FieldDescriptor(
      name='id', full_name='RegisterInterfaceResponse.id', index=0,
      number=1, type=3, cpp_type=2, label=2,
      default_value=0,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
  ],
  extensions=[
  ],
  nested_types=[],  # TODO(robinson): Implement.
  enum_types=[
  ],
  options=None)


_REGISTERFUNCTIONCOMMAND = descriptor.Descriptor(
  name='RegisterFunctionCommand',
  full_name='RegisterFunctionCommand',
  filename='autobus.proto',
  containing_type=None,
  fields=[
    descriptor.FieldDescriptor(
      name='interface_id', full_name='RegisterFunctionCommand.interface_id', index=0,
      number=1, type=3, cpp_type=2, label=1,
      default_value=0,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
    descriptor.FieldDescriptor(
      name='interface_name', full_name='RegisterFunctionCommand.interface_name', index=1,
      number=2, type=9, cpp_type=9, label=1,
      default_value=unicode("", "utf-8"),
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
    descriptor.FieldDescriptor(
      name='name', full_name='RegisterFunctionCommand.name', index=2,
      number=3, type=9, cpp_type=9, label=2,
      default_value=unicode("", "utf-8"),
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
  ],
  extensions=[
  ],
  nested_types=[],  # TODO(robinson): Implement.
  enum_types=[
  ],
  options=None)


_REGISTERFUNCTIONRESPONSE = descriptor.Descriptor(
  name='RegisterFunctionResponse',
  full_name='RegisterFunctionResponse',
  filename='autobus.proto',
  containing_type=None,
  fields=[
  ],
  extensions=[
  ],
  nested_types=[],  # TODO(robinson): Implement.
  enum_types=[
  ],
  options=None)


_CALLFUNCTIONCOMMAND = descriptor.Descriptor(
  name='CallFunctionCommand',
  full_name='CallFunctionCommand',
  filename='autobus.proto',
  containing_type=None,
  fields=[
    descriptor.FieldDescriptor(
      name='interface_id', full_name='CallFunctionCommand.interface_id', index=0,
      number=1, type=3, cpp_type=2, label=1,
      default_value=0,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
    descriptor.FieldDescriptor(
      name='interface_name', full_name='CallFunctionCommand.interface_name', index=1,
      number=2, type=9, cpp_type=9, label=1,
      default_value=unicode("", "utf-8"),
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
    descriptor.FieldDescriptor(
      name='function', full_name='CallFunctionCommand.function', index=2,
      number=3, type=9, cpp_type=9, label=2,
      default_value=unicode("", "utf-8"),
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
    descriptor.FieldDescriptor(
      name='arguments', full_name='CallFunctionCommand.arguments', index=3,
      number=4, type=11, cpp_type=10, label=3,
      default_value=[],
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
  ],
  extensions=[
  ],
  nested_types=[],  # TODO(robinson): Implement.
  enum_types=[
  ],
  options=None)


_CALLFUNCTIONRESPONSE = descriptor.Descriptor(
  name='CallFunctionResponse',
  full_name='CallFunctionResponse',
  filename='autobus.proto',
  containing_type=None,
  fields=[
    descriptor.FieldDescriptor(
      name='return_value', full_name='CallFunctionResponse.return_value', index=0,
      number=1, type=11, cpp_type=10, label=1,
      default_value=None,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
  ],
  extensions=[
  ],
  nested_types=[],  # TODO(robinson): Implement.
  enum_types=[
  ],
  options=None)


_RUNFUNCTIONCOMMAND = descriptor.Descriptor(
  name='RunFunctionCommand',
  full_name='RunFunctionCommand',
  filename='autobus.proto',
  containing_type=None,
  fields=[
    descriptor.FieldDescriptor(
      name='interface_id', full_name='RunFunctionCommand.interface_id', index=0,
      number=1, type=3, cpp_type=2, label=2,
      default_value=0,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
    descriptor.FieldDescriptor(
      name='interface_name', full_name='RunFunctionCommand.interface_name', index=1,
      number=2, type=9, cpp_type=9, label=2,
      default_value=unicode("", "utf-8"),
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
    descriptor.FieldDescriptor(
      name='function', full_name='RunFunctionCommand.function', index=2,
      number=3, type=9, cpp_type=9, label=2,
      default_value=unicode("", "utf-8"),
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
    descriptor.FieldDescriptor(
      name='arguments', full_name='RunFunctionCommand.arguments', index=3,
      number=4, type=11, cpp_type=10, label=3,
      default_value=[],
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
  ],
  extensions=[
  ],
  nested_types=[],  # TODO(robinson): Implement.
  enum_types=[
  ],
  options=None)


_RUNFUNCTIONRESPONSE = descriptor.Descriptor(
  name='RunFunctionResponse',
  full_name='RunFunctionResponse',
  filename='autobus.proto',
  containing_type=None,
  fields=[
    descriptor.FieldDescriptor(
      name='return_value', full_name='RunFunctionResponse.return_value', index=0,
      number=1, type=11, cpp_type=10, label=1,
      default_value=None,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
  ],
  extensions=[
  ],
  nested_types=[],  # TODO(robinson): Implement.
  enum_types=[
  ],
  options=None)


_ERRORRESPONSE = descriptor.Descriptor(
  name='ErrorResponse',
  full_name='ErrorResponse',
  filename='autobus.proto',
  containing_type=None,
  fields=[
    descriptor.FieldDescriptor(
      name='text', full_name='ErrorResponse.text', index=0,
      number=1, type=9, cpp_type=9, label=1,
      default_value=unicode("", "utf-8"),
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
  ],
  extensions=[
  ],
  nested_types=[],  # TODO(robinson): Implement.
  enum_types=[
  ],
  options=None)


_MAPENTRY.fields_by_name['key'].message_type = _INSTANCE
_MAPENTRY.fields_by_name['value'].message_type = _INSTANCE
_STRUCTENTRY.fields_by_name['value'].message_type = _INSTANCE
_INSTANCE.fields_by_name['value1'].message_type = _INTEGERINSTANCE
_INSTANCE.fields_by_name['value2'].message_type = _LONGINSTANCE
_INSTANCE.fields_by_name['value3'].message_type = _DOUBLEINSTANCE
_INSTANCE.fields_by_name['value4'].message_type = _STRINGINSTANCE
_INSTANCE.fields_by_name['value5'].message_type = _TIMESTAMPINSTANCE
_INSTANCE.fields_by_name['value6'].message_type = _NULLINSTANCE
_INSTANCE.fields_by_name['value7'].message_type = _LISTINSTANCE
_INSTANCE.fields_by_name['value8'].message_type = _MAPINSTANCE
_INSTANCE.fields_by_name['value9'].message_type = _STRUCTINSTANCE
_LISTINSTANCE.fields_by_name['value'].message_type = _INSTANCE
_MAPINSTANCE.fields_by_name['value'].message_type = _MAPENTRY
_STRUCTINSTANCE.fields_by_name['value'].message_type = _STRUCTENTRY
_MESSAGE.fields_by_name['value2'].message_type = _REGISTERINTERFACECOMMAND
_MESSAGE.fields_by_name['value3'].message_type = _REGISTERINTERFACERESPONSE
_MESSAGE.fields_by_name['value4'].message_type = _REGISTERFUNCTIONCOMMAND
_MESSAGE.fields_by_name['value5'].message_type = _REGISTERFUNCTIONRESPONSE
_MESSAGE.fields_by_name['value6'].message_type = _CALLFUNCTIONCOMMAND
_MESSAGE.fields_by_name['value7'].message_type = _CALLFUNCTIONRESPONSE
_MESSAGE.fields_by_name['value8'].message_type = _RUNFUNCTIONCOMMAND
_MESSAGE.fields_by_name['value9'].message_type = _RUNFUNCTIONRESPONSE
_MESSAGE.fields_by_name['value10'].message_type = _ERRORRESPONSE
_REGISTERINTERFACECOMMAND.fields_by_name['info'].message_type = _INSTANCE
_CALLFUNCTIONCOMMAND.fields_by_name['arguments'].message_type = _INSTANCE
_CALLFUNCTIONRESPONSE.fields_by_name['return_value'].message_type = _INSTANCE
_RUNFUNCTIONCOMMAND.fields_by_name['arguments'].message_type = _INSTANCE
_RUNFUNCTIONRESPONSE.fields_by_name['return_value'].message_type = _INSTANCE

class MapEntry(message.Message):
  __metaclass__ = reflection.GeneratedProtocolMessageType
  DESCRIPTOR = _MAPENTRY

class StructEntry(message.Message):
  __metaclass__ = reflection.GeneratedProtocolMessageType
  DESCRIPTOR = _STRUCTENTRY

class Instance(message.Message):
  __metaclass__ = reflection.GeneratedProtocolMessageType
  DESCRIPTOR = _INSTANCE

class IntegerInstance(message.Message):
  __metaclass__ = reflection.GeneratedProtocolMessageType
  DESCRIPTOR = _INTEGERINSTANCE

class LongInstance(message.Message):
  __metaclass__ = reflection.GeneratedProtocolMessageType
  DESCRIPTOR = _LONGINSTANCE

class DoubleInstance(message.Message):
  __metaclass__ = reflection.GeneratedProtocolMessageType
  DESCRIPTOR = _DOUBLEINSTANCE

class StringInstance(message.Message):
  __metaclass__ = reflection.GeneratedProtocolMessageType
  DESCRIPTOR = _STRINGINSTANCE

class TimestampInstance(message.Message):
  __metaclass__ = reflection.GeneratedProtocolMessageType
  DESCRIPTOR = _TIMESTAMPINSTANCE

class NullInstance(message.Message):
  __metaclass__ = reflection.GeneratedProtocolMessageType
  DESCRIPTOR = _NULLINSTANCE

class ListInstance(message.Message):
  __metaclass__ = reflection.GeneratedProtocolMessageType
  DESCRIPTOR = _LISTINSTANCE

class MapInstance(message.Message):
  __metaclass__ = reflection.GeneratedProtocolMessageType
  DESCRIPTOR = _MAPINSTANCE

class StructInstance(message.Message):
  __metaclass__ = reflection.GeneratedProtocolMessageType
  DESCRIPTOR = _STRUCTINSTANCE

class Message(message.Message):
  __metaclass__ = reflection.GeneratedProtocolMessageType
  DESCRIPTOR = _MESSAGE

class RegisterInterfaceCommand(message.Message):
  __metaclass__ = reflection.GeneratedProtocolMessageType
  DESCRIPTOR = _REGISTERINTERFACECOMMAND

class RegisterInterfaceResponse(message.Message):
  __metaclass__ = reflection.GeneratedProtocolMessageType
  DESCRIPTOR = _REGISTERINTERFACERESPONSE

class RegisterFunctionCommand(message.Message):
  __metaclass__ = reflection.GeneratedProtocolMessageType
  DESCRIPTOR = _REGISTERFUNCTIONCOMMAND

class RegisterFunctionResponse(message.Message):
  __metaclass__ = reflection.GeneratedProtocolMessageType
  DESCRIPTOR = _REGISTERFUNCTIONRESPONSE

class CallFunctionCommand(message.Message):
  __metaclass__ = reflection.GeneratedProtocolMessageType
  DESCRIPTOR = _CALLFUNCTIONCOMMAND

class CallFunctionResponse(message.Message):
  __metaclass__ = reflection.GeneratedProtocolMessageType
  DESCRIPTOR = _CALLFUNCTIONRESPONSE

class RunFunctionCommand(message.Message):
  __metaclass__ = reflection.GeneratedProtocolMessageType
  DESCRIPTOR = _RUNFUNCTIONCOMMAND

class RunFunctionResponse(message.Message):
  __metaclass__ = reflection.GeneratedProtocolMessageType
  DESCRIPTOR = _RUNFUNCTIONRESPONSE

class ErrorResponse(message.Message):
  __metaclass__ = reflection.GeneratedProtocolMessageType
  DESCRIPTOR = _ERRORRESPONSE

