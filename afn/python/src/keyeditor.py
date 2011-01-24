
import librtk
import librtk.protocols
import librtkinter

def main():
    client_protocol = librtk.protocols.LinkedProtocol()
    server_protocol = librtk.protocols.LinkedProtocol(client_protocol)
    client_connection, master = librtkinter.start_connection(client_protocol)
    server_connection = librtk.Connection(server_protocol, start_app)
    client_connection.start()
    server_connection.start()
    print "Started up!"
    try:
        master.mainloop()
    except KeyboardInterrupt:
        print "Interrupted, shutting down"
    print "Terminated."

def start_app(connection):
    
