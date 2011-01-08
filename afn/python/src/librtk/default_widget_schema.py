
from categories import TOPLEVEL, CONTAINER, WIDGET, number

schema = {
    "Window": [
        "Window", TOPLEVEL, [ # Widget properties:
            ["title", True, ""],
        ], [ # Layout properties:
        ], [ # State properties:
            ["size", [0, 0]],
        ], [ # Calls:
            ["pack"],
            ["resize"],
            ["relocate"],
            ["center"], 
        ], [ # Events:
            ["close_request"],
        ]
    ],
    "VBox": [
        "VBox", CONTAINER, [ # Widget properties:
        ], [ # Layout properties:
        ], [ # State properties:
        ], [ # Calls:
        ], [ # Events:
        ]
    ],
    "Label": [
        "Label", WIDGET, [ # Widget properties:
            ["text", True, ""],
        ], [ # Layout properties:
        ], [ # State properties:
        ], [ # Calls:
        ], [ # Events:
        ]
    ],
    "Button": [
        "Button", WIDGET, [ # Widget properties:
            ["text", True, ""],
        ], [ # Layout properties:
        ], [ # State properties:
        ], [ # Calls:
        ], [ # Events:
            ["clicked"],
        ]
    ],
}

"""
Template for an individual widget:

    "WidgetConstructorName": [
        "widget_name", CATEGORY, [ # Widget properties:
            ["widget_property", writable, default_value],
        ], [ # Layout properties:
            ["layout_property", writable, default_value],
        ], [ # State properties:
            ["state_property", default_value],
        ], [ # Calls:
            ["call_name", [param1_type, param2_type], return_type],
        ], [ # Events:
            ["event_name"],
        ]
    ],

"""
