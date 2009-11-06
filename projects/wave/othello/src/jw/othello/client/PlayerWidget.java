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
            setHTML("<div style='border: 2px outset #444444'>"
                    + "<div style='border: 2px solid #" + color + "'>"
                    + "<div style='border: 1px solid #444444'>" + generateImageTag()
                    + "</div></div></div>");
        }
        else
        {
            setHTML("<div style='border: 1px solid #444444'>"
                    + "<div style='border: 3px solid #" + color + "'>"
                    + "<div style='border: 1px solid #444444'>" + generateImageTag()
                    + "</div></div></div>");
        }
    }
    
    public PlayerWidget(String playerId)
    {
        setWidth("34px");
        setHeight("34px");
        this.playerId = playerId;
        refresh();
    }
    
    private String generateImageTag()
    {
        Participant participant = OthelloGadget.wave.getParticipantById(playerId);
        if (participant == null)
            return "Participant<br/>Deleted";
        return "<img width='24' height='24' src=\"" + participant.getThumbnailUrl() + "\"/>";
    }
}
