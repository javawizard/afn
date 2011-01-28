
import librtk
import librtk.protocols
import librtkinter
from ConfigParser import RawConfigParser
import sys
import utils
import time

# A list of lists of components for each key. The sublists are [row, col, color]
# specifying where in the key's table the text field for that component should
# be placed and the background color for the text field. The first
# element in the overall list represents the location of
# the component for lowercase chars, the second uppercase, the third altgr,
# and the fourth altgr uppercase (I.E. altgr+shift). Once I figure out how to
# add additional components with XKB groups, I'll likely add more entries.
KEY_COMPONENTS = [[3, 0, "#ffffff"], [2, 0, "#ccccff"],
                  [3, 1, "#ffcccc"], [2, 1, "#ffccff"],
                  [1, 0, "#ffffc5"], [0, 0, "#ccffcc"],
                  [1, 1, "#ffeacc"], [0, 1, "#eeeeee"]]
# The names of each component. These contain representatiosn of what key
# combinations cause that component to be used.
KEY_COMPONENT_NAMES = ["none", "shift",
                       "altgr", "altgr+shift",
                       "ctrl", "ctrl+shift",
                       "ctrl+altgr", "ctrl+altgr+shift"]

# A list of rows. Each entry in this list has four items: the row letter in
# the XKB file, the row number in the RTK table, the offset from the left of
# the RTK table at which this row should start, and the number of keys present
# in this row.
KEY_ROWS = [["B", 3, 1, 10], ["C", 2, 1, 11], ["D", 1, 1, 13], ["E", 0, 0, 13]]

def main():
    global config, config_file, output_file, output_name, output_desc
    global connection
    if len(sys.argv) <= 4:
        print "Syntax: keyeditor <config-filename> <output-filename> <name> <desc>"
        print "The specified config file will be created if it doesn't exist."
        print "The output file will be written when the generate button is"
        print "clicked. It can then be copied into /usr/share/X11/XKB/symbols."
        return
    config_file = sys.argv[1]
    output_file = sys.argv[2]
    output_name = sys.argv[3]
    output_desc = sys.argv[4]
    config = RawConfigParser()
    config.read(config_file)
    client_protocol = librtk.protocols.LinkedProtocol()
    server_protocol = librtk.protocols.LinkedProtocol(client_protocol)
    connection = librtk.Connection(server_protocol, start_app)
    connection.start()
    client_connection, master = librtkinter.start_connection(client_protocol)
    print "Started up!"
    try:
        master.mainloop()
    except KeyboardInterrupt:
        print "Interrupted, shutting down"
        client_connection.close()
        connection.close()
    print "Terminated."

def start_app(*args): # We don't need the connection since it's stored in the
    # global field connection in main()
    global save_button, generate_button
    window = connection.Window(title="KeyEditor")
    def on_close():
        save()
        connection.close()
    window.close_request.listen(on_close)
    box = connection.VBox(window)
    key_table = connection.Table(box)
    connection.Label(box, text=" ") # Spacer
    buttons = connection.HBox(box)
    save_button = connection.Button(buttons, text="Save", clicked=save)
    generate_button = connection.Button(buttons, text="Generate", clicked=generate)
    connection.Label(buttons, text="    ") # Spacer
    create_legend(buttons)
    for key_row in KEY_ROWS:
        row_name, row, col_offset, keys = key_row
        for i in range(keys):
            col = i + col_offset
            key_widget = create_key(key_table, row_name + "-" + str(i))
            key_widget.row = row
            key_widget.col = col

def create_legend(parent):
    table = connection.Table(parent)
    for (row, col, color), name in zip(KEY_COMPONENTS, KEY_COMPONENT_NAMES):
        connection.Label(table, text=name, row=row, col=col,
                background=color, pin="we")

def create_key(parent, keyname):
    panel = connection.BorderPanel(parent)
    table = connection.Table(panel)
    for index, (row, col, color) in enumerate(KEY_COMPONENTS):
        create_key_component(keyname, table, index, row, col, color)
    return panel

def create_key_component(keyname, table, index, row, col, color):
    field = connection.TextBox(table, row=row, col=col, width=2,
            background=color, font_size=14)
    if config.has_option(keyname, str(index)):
        value = config.get(keyname, str(index))
        if value:
            field.set_text(unichr(int(value)))
    def changed():
        if len(field.text) > 1:
            print "Field length too large"
            field.set_text(field.text[0])
            return
        print ("Setting " + keyname + " component " + str(index) + 
                " to " + field.text + "")
        if not config.has_section(keyname):
            config.add_section(keyname)
        config.set(keyname, str(index), (field.text and 
                str(ord(field.text))) or "")
    field.text_changed.listen(changed)

def save():
    print "Saving..."
    with open(config_file, "w") as file:
        config.write(file)
    print "Saved!"
    save_button.text = "Saved!"
    def reset_save_button():
        save_button.text = "Save"
    utils.at(2, reset_save_button)

def generate():
    print "Generating..."
    with open(output_file, "w") as file:
        file.write('xkb_symbols "' + output_name + '"\n{\n')
        file.write('    name[Group1] = "' + output_desc + ' - 1";\n')
        file.write('    name[Group2] = "' + output_desc + ' - 2";\n')
        for row_name, _, _, keys in KEY_ROWS:
            for index in range(keys):
                keyname = row_name + "-" + str(index)
                output_key = get_output_key(row_name, index)
                file.write("    key " + output_key + " { [ ")
                file.write(generate_key_range(keyname, 0, 4))
                file.write(" ], [], [ ")
                file.write(generate_key_range(keyname, 4, 8))
                file.write(" ] };\n")
        file.write("};\n")
    print "Generated!"
    generate_button.text = "Generated!"
    def reset_generate_button():
        generate_button.text = "Generate"
    utils.at(2, reset_generate_button)

def generate_key_value(keyname, index):
    """
    Generates the XKB value of the specified component of the specified key.
    For example, on a typical layout, generate_key_value("C-0", 0) would
    return "U0061" (the unicode value for "a"). This function will return
    "NoSymbol" if there's no such key in the configuration or if the specified
    key does not have a component with the specified index.
    """
    if (config.has_section(keyname) and 
            config.has_option(keyname, str(index)) and
            config.get(keyname, str(index))):
        return ("U" + hex(config.getint(keyname, str(index)))[2:]
                .rjust(4, "0").upper())
    else:
        return "NoSymbol"

def generate_key_range(keyname, start, end):
    """
    Generates a string containing the values returned by generate_key_value
    for every component with an index between start, inclusive, and end,
    exclusive, for the specified key, with each value separated by a comma.
    """
    keycodes = []
    for i in range(start, end):
        keycodes.append(generate_key_value(keyname, i))
    return ", ".join([k.rjust(12) for k in keycodes])

def get_output_key(row_name, index):
    index += 1
    if row_name == "E" and index == 1:
        return "<TLDE>"
    if row_name == "D" and index == 13:
        return "<BKSL>"
    if row_name == "E":
        index -= 1
    return "<A" + row_name + str(index).rjust(2, "0") + ">"



















