
import default_widget_schema

features = []

for value in default_widget_schema.schema.values():
    features.append("widget:" + value[0])
