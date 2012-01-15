package jw.othello.client;

import java.util.ArrayList;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.user.client.ui.Anchor;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.Grid;

public class ColorChooserWidget extends Composite
{
    public static final int INTERVAL = 64;
    public static final int SIZE = 12;
    private Grid grid;
    
    private ArrayList<ColorListener> listeners = new ArrayList<ColorListener>();
    
    public ColorChooserWidget()
    {
        grid = new Grid(5, 25);
        initWidget(grid);
        grid.setCellPadding(0);
        grid.setCellSpacing(0);
        grid.setBorderWidth(0);
        initGrid();
        setSize((SIZE * 25) + "px", (SIZE * 5) + "px");
    }
    
    private void initGrid()
    {
        for (int r = 0; r < 5; r++)
        {
            for (int g = 0; g < 5; g++)
            {
                for (int b = 0; b < 5; b++)
                {
                    int row = b;
                    int col = g + (r * 5);
                    int red = Math.min(255, r * INTERVAL);
                    int green = Math.min(255, g * INTERVAL);
                    int blue = Math.min(255, b * INTERVAL);
                    initCell(row, col, red, green, blue);
                }
            }
        }
    }
    
    private void initCell(int row, int col, final int red, final int green, final int blue)
    {
        // FIXME: This uses a colored image. Change it to use a fixed-size div with a
        // background color, but check this out on IE first.
        Anchor a = new Anchor("<div style='width:" + SIZE + ";height:" + SIZE
                + ";background-color:" + colorString(red, green, blue) + "'></div>", true);
        a.addStyleName("othello-no-borders");
        a.addClickHandler(new ClickHandler()
        {
            
            @Override
            public void onClick(ClickEvent event)
            {
                for (ColorListener listener : listeners)
                {
                    listener.onChoice(colorString(red, green, blue));
                }
            }
        });
        grid.setWidget(row, col, a);
    }
    
    protected String colorString(int red, int green, int blue)
    {
        String r = Integer.toHexString(red);
        String g = Integer.toHexString(green);
        String b = Integer.toHexString(blue);
        if (r.length() == 1)
            r = "0" + r;
        if (g.length() == 1)
            g = "0" + g;
        if (b.length() == 1)
            b = "0" + b;
        return r + g + b;
    }
    
    public void addColorListener(ColorListener listener)
    {
        listeners.add(listener);
    }
}
