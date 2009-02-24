package com.googlecode.jwutils.jwm;

import javax.microedition.midlet.MIDlet;
import javax.microedition.midlet.MIDletStateChangeException;

public class JWMazer extends MIDlet
{
    
    protected void destroyApp(boolean unconditional)
    {
        
    }
    
    protected void pauseApp()
    {
        destroyApp(true);
        notifyDestroyed();
    }
    
    private MazerCanvas canvas;
    
    protected void startApp() throws MIDletStateChangeException
    {
        
    }
    
}
