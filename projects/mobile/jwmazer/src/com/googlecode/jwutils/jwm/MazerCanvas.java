package com.googlecode.jwutils.jwm;

import javax.microedition.lcdui.Canvas;
import javax.microedition.lcdui.Graphics;
import javax.microedition.lcdui.Image;

public class MazerCanvas extends Canvas
{
    private Image buffer;
    private Graphics graphics;
    
    private int bufferWidth;
    private int bufferHeight;
    
    public Graphics getGraphics()
    {
        return graphics;
    }
    
    /**
     * This should be called once the logo canvas has been displayed for the
     * first time. It creates the offscreen image used to render to this canvas.
     * It creates an offscreen image that is the size of this component, so it
     * should be displayed and laid out in it's final size before this method is
     * called.
     */
    public void init()
    {
        buffer = Image.createImage(getWidth(), getHeight());
        bufferWidth = getWidth();
        bufferHeight = getHeight();
        graphics = buffer.getGraphics();
    }
    
    public void paint(Graphics cg)
    {
        int width = getWidth();
        int height = getHeight();
        /*
         * Draw the buffer onto the screen
         */
        cg.drawImage(buffer, 0, 0, cg.TOP | cg.LEFT);
    }
}
