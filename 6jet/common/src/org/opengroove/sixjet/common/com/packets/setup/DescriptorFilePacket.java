package org.opengroove.sixjet.common.com.packets.setup;

import org.opengroove.sixjet.common.com.Packet;
import org.opengroove.sixjet.common.format.d.DescriptorFile;

public class DescriptorFilePacket extends Packet
{
    private DescriptorFile file;
    
    public DescriptorFile getFile()
    {
        return file;
    }
    
    public void setFile(DescriptorFile file)
    {
        this.file = file;
    }
}
