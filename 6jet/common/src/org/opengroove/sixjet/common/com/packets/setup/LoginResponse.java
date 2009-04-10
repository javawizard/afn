package org.opengroove.sixjet.common.com.packets.setup;

import org.opengroove.sixjet.common.com.Packet;

public class LoginResponse extends Packet
{
    public boolean isSuccessful()
    {
        return successful;
    }
    
    public String getReason()
    {
        return reason;
    }
    
    public void setSuccessful(boolean successful)
    {
        this.successful = successful;
    }
    
    public void setReason(String reason)
    {
        this.reason = reason;
    }
    
    private boolean successful;
    private String reason;
}
