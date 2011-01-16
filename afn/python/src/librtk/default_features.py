
import schema.default

features = []

for key in schema.default.schema:
    features.append("widget:" + key)
