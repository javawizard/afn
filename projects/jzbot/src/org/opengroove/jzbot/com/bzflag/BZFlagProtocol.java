package org.opengroove.jzbot.com.bzflag;

import java.net.URL;

import org.opengroove.jzbot.com.Protocol;

/**
 * A class that implements the BZFS0026 protocol, used by BZFlag versions with a
 * major version number of 2. When BZFlag 3.0 comes out, I'll probably write a
 * protocol implementation for it.<br/>
 * <br/>
 * 
 * The protocol defines these extended events:<br/>
 * <ul>
 * <li><b>tk:</b> Called when a user on the server kills a teammate. The first
 * argument is the killer's callsign, and the second argument is the callsign of
 * the person that was killed.</li>
 * <li><b>kill:</b> Called when a user on the server kills another user. This is
 * not called when the kill is a tk.</li>
 * <li><b>cap:</b> Called when a user on the server kills</li>
 * </ul>
 * 
 * There are a total of 2 rooms present on a bzflag server. The first one is the
 * empty string, and represents the server's public chat. The second one is
 * named "observers", and is the team chat for observers.
 * 
 * @author Alexander Boyd
 * 
 */
public class BZFlagProtocol implements Protocol
{
    
    public boolean banDurationAllowed()
    {
        // TODO Auto-generated method stub
        return false;
    }
    
    public boolean banMessageAllowed()
    {
        // TODO Auto-generated method stub
        return false;
    }
    
    public String getName()
    {
        // TODO Auto-generated method stub
        return null;
    }
    
    public void join(URL room)
    {
        // TODO Auto-generated method stub
        
    }
}
