
import librtk
import librtk.protocols
import librtkinter
from ConfigParser import RawConfigParser


def main():
    client_protocol = librtk.protocols.LinkedProtocol()
    server_protocol = librtk.protocols.LinkedProtocol(client_protocol)
    server_connection = librtk.Connection(server_protocol, start_app)
    server_connection.start()
    client_connection, master = librtkinter.start_connection(client_protocol)
    print "Started up!"
    try:
        master.mainloop()
    except KeyboardInterrupt:
        print "Interrupted, shutting down"
        client_connection.close()
        server_connection.close()
    print "Terminated."

def start_app(connection):
    window = connection.Window(title="KeyEditor")
    box = connection.VBox(window)
    key_table = connection.Table(box)
    buttons = connection.HBox(box)
    connection.Button(buttons, text="Save", clicked=save)
    connection.Button(buttons, text="Generate", clicked=generate)

def save():
    print "TODO: save"

def generate():
    print "TODO: generate"



















