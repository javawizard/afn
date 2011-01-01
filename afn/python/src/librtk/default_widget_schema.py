
from categories import TOPLEVEL, CONTAINER, WIDGET

schema = {
    "Window": [
        "window", TOPLEVEL, [
            ["widget_attribute", writable, default_value],
        ], [
            ["layout_attribute", writable, default_value],
        ], [
            ["state_attribute", default_value],
        ], [
            ["call_name", [param1_type, param2_type], return_type],
        ], [
            ["event_name"],
        ]
    ],
}

"""
Template for an individual widget:

    "WidgetConstructorName": [
        "widget_name", CATEGORY, [
            ["widget_attribute", writable, default_value],
        ], [
            ["layout_attribute", writable, default_value],
        ], [
            ["state_attribute", default_value],
        ], [
            ["call_name", [param1_type, param2_type], return_type],
        ], [
            ["event_name"],
        ]
    ],

"""