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
            setColoredHTML("<div style='border: 1px dashed #444444\'>"
                    + "<div style='border: 1px solid #ffffff'>"
                    + "<div style='border: 1px solid #888888'>"
                    + "<div style='border: 1px solid #ffffff'>"
                    + "<div style='border: 2px solid #COLOR'>" + generateImageTag()
                    + "</div></div></div></div>");
        }
        else
        {
            setColoredHTML("<div style='border: 1px solid #ffffff'>"
                    + "<div style='border: 1px solid #ffffff'>"
                    + "<div style='border: 1px solid #888888'>"
                    + "<div style='border: 1px solid #ffffff'>"
                    + "<div style='border: 2px solid #COLOR'>" + generateImageTag()
                    + "</div></div></div></div>");
        }
    }
    
    private void setColoredHTML(String string)
    {
        setHTML(string.replace("COLOR", color));
    }
    
    public boolean isActive()
    {
        return active;
    }
    
    public PlayerWidget(String playerId)
    {
        setWidth("60px");
        setHeight("60px");
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
