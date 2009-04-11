package org.opengroove.sixjet.common.com.packets.setup;

import org.opengroove.sixjet.common.com.Packet;

public class DescriptorFilePacket extends Packet
{
    private String file;
    
    /**
     * Gets the actual contents of the descriptor file.
     * 
     * @return
     */
    public String getFile()
    {
        return file;
    }
    
    public void setFile(String file)
    {
        this.file = file;
    }
}
