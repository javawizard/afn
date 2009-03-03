package com.googlecode.jwutils.jwm;

import java.util.Random;

import javax.microedition.lcdui.Display;
import javax.microedition.midlet.MIDlet;
import javax.microedition.midlet.MIDletStateChangeException;

import com.googlecode.jwutils.jwm.algorithms.RandomDivisionAlgorithm;

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
    
    private static MazerCanvas canvas;
    
    protected void startApp() throws MIDletStateChangeException
    {
        canvas = new MazerCanvas();
        canvas.init();
        Display.getDisplay(this).setCurrent(canvas);
        new Thread()
        {
            public void run()
            {
                RandomDivisionAlgorithm a = new RandomDivisionAlgorithm();
                int w = canvas.getWidth();
                int h = canvas.getHeight();
                int pathsize = 4;
                w = w / pathsize;
                h = h / pathsize;
                w -= 1;
                h -= 1;
                a.draw(canvas, w, h, pathsize, new Random());
            }
        }.start();
    }
    
}
