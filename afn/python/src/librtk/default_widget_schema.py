
from categories import TOPLEVEL, CONTAINER, WIDGET, number

schema = {
    "Window": [
        "Window", TOPLEVEL, [ # Widget attributes:
            ["title", True, ""],
        ], [ # Layout attributes:
        ], [ # State attributes:
            ["size", [0, 0]],
        ], [ # Calls:
            ["pack"],
            ["resize"],
            ["relocate"],
            ["center"],
        ], [ # Events:
        ]
    ],
    "VBox": [
        "VBox", CONTAINER, [ # Widget attributes:
        ], [ # Layout attributes:
        ], [ # State attributes:
        ], [ # Calls:
        ], [ # Events:
        ]
    ],
    "Label": [
        "Label", WIDGET, [ # Widget attributes:
            ["text", True, ""],
        ], [ # Layout attributes:
        ], [ # State attributes:
        ], [ # Calls:
        ], [ # Events:
        ]
    ],
    "Button": [
        "Button", WIDGET, [ # Widget attributes:
            ["text", True, ""],
        ], [ # Layout attributes:
        ], [ # State attributes:
        ], [ # Calls:
        ], [ # Events:
            ["clicked"],
        ]
    ],
}

"""
Template for an individual widget:

    "WidgetConstructorName": [
        "widget_name", CATEGORY, [ # Widget attributes:
            ["widget_attribute", writable, default_value],
        ], [ # Layout attributes:
            ["layout_attribute", writable, default_value],
        ], [ # State attributes:
            ["state_attribute", default_value],
        ], [ # Calls:
            ["call_name", [param1_type, param2_type], return_type],
        ], [ # Events:
            ["event_name"],
        ]
    ],

"""
