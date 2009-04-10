package org.opengroove.sixjet.common.com.model;

import java.io.Serializable;
import java.util.ArrayList;

import javax.swing.JComponent;

public class Channel implements Serializable
{
    private String name;
    private int jet;
    
    public String getName()
    {
        return name;
    }
    
    public int getJet()
    {
        return jet;
    }
    
    public void setName(String name)
    {
        this.name = name;
    }
    
    public void setJet(int jet)
    {
        this.jet = jet;
    }
    
    /**
     * 
     */
    private static final long serialVersionUID = -8911500961825755529L;
    
    public ArrayList<ChannelEvent> getEvents()
    {
        return events;
    }
    
    private ArrayList<ChannelEvent> events = new ArrayList<ChannelEvent>();
}
