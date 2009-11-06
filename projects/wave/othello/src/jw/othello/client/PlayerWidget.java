package jw.othello.client;

import org.cobogw.gwt.waveapi.gadget.client.Participant;

import com.google.gwt.http.client.URL;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.HTML;

/**
 * A widget that displays a player's color and icon, and hovering over it shows the
 * player's real name.
 * 
 * @author Alexander Boyd
 * 
 */
public class PlayerWidget extends HTML
{
    private boolean active;
    
    private String color = "ffffff";
    
    private String playerId;
    
    public void setActive(boolean active)
    {
        if (this.active != active)
        {
            this.active = active;
            refresh();
        }
    }
    
    public void setColor(String color)
    {
        if (!this.color.equals(color))
        {
            this.color = color;
            refresh();
        }
    }
    
    private void refresh()
    {
        if (active)
        {
            setColoredHTML("<div style='border: 2px outset #MID'>" + generateImageTag()
                    + "</div>");
        }
        else
        {
            setColoredHTML("<div style='border: 2px solid #MID'>" + generateImageTag()
                    + "</div>");
        }
    }
    
    private void setColoredHTML(String string)
    {
        setHTML(string.replace("COLOR", color));
    }
    
    private String midColor()
    {
        String hex = color;
        String rs = hex.substring(0, 2);
        String gs = hex.substring(2, 4);
        String bs = hex.substring(4, 6);
        int r = Integer.parseInt(rs, 16);
        int g = Integer.parseInt(gs, 16);
        int b = Integer.parseInt(bs, 16);
        r = average(r, 128);
        g = average(g, 128);
        b = average(b, 128);
        return pad("" + r) + pad("" + g) + pad("" + b);
    }
    
    private int average(int... numbers)
    {
        int value = 0;
        for (int i : numbers)
        {
            value += i;
        }
        return value / numbers.length;
    }
    
    private String pad(String s)
    {
        if (s.length() == 1)
            s = "0" + s;
        return s;
    }
    
    public boolean isActive()
    {
        return active;
    }
    
    public PlayerWidget(String playerId)
    {
        setWidth("56px");
        setHeight("56px");
        this.playerId = playerId;
        refresh();
    }
    
    private String generateImageTag()
    {
        Participant participant = OthelloGadget.wave.getParticipantById(playerId);
        if (participant == null)
            return "Participant<br/>Deleted";
        return "<img width='48' height='48' src=\"" + participant.getThumbnailUrl()
                + "\"/>";
    }
}
