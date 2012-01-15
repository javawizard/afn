package com.googlecode.jwutils.timer;

import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JToggleButton;

/**
 * Holds information and state related to an actual timer.
 * 
 * @author Alexander Boyd
 * 
 */
public class Timer
{
    public static enum Direction
    {
        UP, DOWN
    }
    
    private String name;
    private JComponent component;
    private JToggleButton label;
    
    public JComponent getComponent()
    {
        return component;
    }
    
    public JToggleButton getLabel()
    {
        return label;
    }
    
    public void setComponent(JComponent component)
    {
        this.component = component;
    }
    
    public void setLabel(JToggleButton label)
    {
        this.label = label;
    }
    
    public String getName()
    {
        return name;
    }
    
    public Direction getDirection()
    {
        return direction;
    }
    
    public int getValue()
    {
        return value;
    }
    
    public void setName(String name)
    {
        this.name = name;
    }
    
    public void setDirection(Direction direction)
    {
        this.direction = direction;
    }
    
    public void setValue(int value)
    {
        this.value = value;
    }
    
    private Direction direction;
    private int value;
}
