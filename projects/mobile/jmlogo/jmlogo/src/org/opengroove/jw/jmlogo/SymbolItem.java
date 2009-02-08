package org.opengroove.jw.jmlogo;

import javax.microedition.lcdui.Canvas;
import javax.microedition.lcdui.Command;
import javax.microedition.lcdui.CustomItem;
import javax.microedition.lcdui.Display;
import javax.microedition.lcdui.Font;
import javax.microedition.lcdui.Form;
import javax.microedition.lcdui.Graphics;
import javax.microedition.lcdui.Item;
import javax.microedition.lcdui.ItemCommandListener;

public class SymbolItem extends CustomItem
{
    private Font font;
    private int width;
    private int height;
    private String symbol;
    private boolean in = false;
    private int index;
    
    public SymbolItem(String symbol, int index)
    {
        super(null);
        this.index = index;
        this.symbol = symbol;
        font = Font.getFont(Font.FACE_SYSTEM, Font.STYLE_PLAIN, Font.SIZE_SMALL);
        width = font.stringWidth(symbol);
        height = font.getHeight();
        addCommand(new Command("cancel", Command.ITEM, 1));
        setItemCommandListener(new ItemCommandListener()
        {
            
            public void commandAction(Command command, Item item)
            {
                Display.getDisplay(SymbolUtils.midlet).setCurrent(
                    SymbolUtils.currentTextBox);
            }
        });
    }
    
    protected int getMinContentHeight()
    {
        return height + 4;
    }
    
    protected int getMinContentWidth()
    {
        return width + 4;
    }
    
    protected int getPrefContentHeight(int arg0)
    {
        return height + 4;
    }
    
    protected int getPrefContentWidth(int arg0)
    {
        return width + 4;
    }
    
    protected void paint(Graphics g, int arg1, int arg2)
    {
        g.setColor(0x000000);
        g.setFont(font);
        g.drawString(symbol, 2, 2, Graphics.TOP | Graphics.LEFT);
        if (in)
        {
            g.setColor(0x000000);
            g.drawRect(0, 0, width + 3, height + 3);
        }
        else
        {
            g.setColor(Display.getDisplay(SymbolUtils.midlet).getColor(
                Display.COLOR_BACKGROUND));
            g.drawRect(0, 0, width + 3, height + 3);
        }
    }
    
    protected boolean traverse(int dir, int viewportWidth, int viewportHeight,
        int[] visRect_inout)
    {
        in = true;
        boolean b = super.traverse(dir, viewportWidth, viewportHeight, visRect_inout);
        repaint();
        return b;
    }
    
    protected void traverseOut()
    {
        in = false;
        repaint();
    }
    
    protected void keyPressed(int keyCode)
    {
        if (keyCode == '*')
        {
            int newIndex = index;
            newIndex -= 7;
            newIndex = newIndex < 0 ? 0 : newIndex;
            Display.getDisplay(SymbolUtils.midlet).setCurrentItem(
                SymbolUtils.form.get(newIndex));
        }
        else if (keyCode == '#')
        {
            int newIndex = index;
            newIndex += 7;
            newIndex =
                newIndex >= SymbolUtils.form.size() ? SymbolUtils.form.size() - 1
                    : newIndex;
            Display.getDisplay(SymbolUtils.midlet).setCurrentItem(
                SymbolUtils.form.get(newIndex));
        }
        else if (getGameAction(keyCode) == Canvas.FIRE)
        {
            SymbolUtils.currentTextBox.insert(symbol, SymbolUtils.currentTextBox
                .getCaretPosition());
            Display.getDisplay(SymbolUtils.midlet).setCurrent(
                SymbolUtils.currentTextBox);
        }
    }
}
