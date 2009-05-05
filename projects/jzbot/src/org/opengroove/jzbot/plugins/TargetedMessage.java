package org.opengroove.jzbot.plugins;

import java.net.URI;

public class TargetedMessage extends Message
{
    private URI target;
    
    public URI getTarget()
    {
        return target;
    }
    
    public void setTarget(URI target)
    {
        this.target = target;
    }
    
    public TargetedMessage()
    {
        super();
        // TODO Auto-generated constructor stub
    }
    
    public TargetedMessage(URI target, boolean action, String message)
    {
        super(action, message);
        this.target = target;
    }
    
}
