package org.opengroove.sixjet.common.com.packets.setup;

import org.opengroove.sixjet.common.com.Packet;

public class LoginPacket extends Packet
{
    private String username;
    public LoginPacket(String username, String password)
    {
        super();
        this.username = username;
        this.password = password;
    }
    public String getUsername()
    {
        return username;
    }
    public String getPassword()
    {
        return password;
    }
    public void setUsername(String username)
    {
        this.username = username;
    }
    public void setPassword(String password)
    {
        this.password = password;
    }
    private String password;
}
