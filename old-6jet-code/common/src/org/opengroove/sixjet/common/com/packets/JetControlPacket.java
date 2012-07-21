package org.opengroove.sixjet.common.com.packets;

import org.opengroove.sixjet.common.com.Packet;

public class JetControlPacket extends Packet
{
    public JetControlPacket()
    {
        super();
    }
    public JetControlPacket(int jet, boolean state)
    {
        super();
        this.jet = jet;
        this.state = state;
    }
    public int getJet()
    {
        return jet;
    }
    public boolean isState()
    {
        return state;
    }
    public void setJet(int jet)
    {
        this.jet = jet;
    }
    public void setState(boolean state)
    {
        this.state = state;
    }
    private int jet;
    private boolean state;
}
