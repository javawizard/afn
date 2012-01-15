package org.opengroove.jw.jmlogo;

import javax.microedition.lcdui.Canvas;
import javax.microedition.lcdui.Display;
import javax.microedition.lcdui.Form;
import javax.microedition.lcdui.TextBox;
import javax.microedition.midlet.MIDlet;

public class SymbolUtils
{
    private static final String SYMBOLS_AS_STRING =
        "[]\":().#'`~?!@#$%^&*=+-_{}|\\/><;01";
    
    private static final char[] SYMBOLS = SYMBOLS_AS_STRING.toCharArray();
    
    private static final int SYMBOL_COUNT = SYMBOLS.length;
    
    static MIDlet midlet;
    
    public static Form form;
    
    public static SymbolItem[] items;
    
    public static TextBox currentTextBox;
    
    public static void init(MIDlet midlet)
    {
        SymbolUtils.midlet = midlet;
        form = new Form("Symbols");
        items = new SymbolItem[SYMBOL_COUNT];
        for (int i = 0; i < SYMBOL_COUNT; i++)
        {
            items[i] = new SymbolItem(new String(new char[] { SYMBOLS[i] }), i);
            form.append(items[i]);
        }
    }
    
    /**
     * Shows the symbol entry form. It will have two actions, select and cancel.
     * Select enters the symbol specified into the text box specified, and shows
     * the text box again. Cancel shows the text box again, without inserting
     * any symbols into the text box. The pound and star keys skip 10 symbols at
     * a time.
     * 
     * @param targetBox
     */
    public static void showSymbolForm(TextBox targetBox)
    {
        currentTextBox = targetBox;
        Display.getDisplay(midlet).setCurrent(form);
    }
}
