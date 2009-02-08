package org.opengroove.jw.jmlogo;

import java.util.Vector;

import javax.microedition.lcdui.Canvas;
import javax.microedition.lcdui.Font;
import javax.microedition.lcdui.Graphics;

public class TextArea extends Canvas
{
    private StringBuffer contents = new StringBuffer();
    /**
     * Represents the line boundary cache. This does not hold the line
     * boundaries for the entire text area; It merely holds the line boundaries
     * from the start of the text area down to the lowest line that the user has
     * viewed since the text area's contents were last changed. Each element is
     * an index that specifies a point in the string where text is supposed to
     * be wrapped to a new line. Line breaks caused by the actual newline
     * character are included in this list.
     * 
     * If the character before a line wrap is a form of white space (\r, \n, or
     * \t, it is omitted during rendering.
     */
    private Vector lineBoundaries = new Vector();
    
    private int caretPosition = 0;
    
    private int currentLine;
    
    private int maxLines;
    
    private int maxDrawnLines;
    
    private int fontHeight;
    
    private Font font;
    
    public TextArea(Font font, String text)
    {
        this.font = font;
        fontHeight = font.getHeight();
        int height = getHeight();
        maxLines = getHeight() / fontHeight;
        maxDrawnLines = maxLines + 1;
    }
    
    protected void paint(Graphics g)
    {
        g.setColor(0x000000);
    }
}
