package jw.othello.client;

import java.util.HashMap;

import org.cobogw.gwt.waveapi.gadget.client.State;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.DockPanel;
import com.google.gwt.user.client.ui.HTML;
import com.google.gwt.user.client.ui.HorizontalPanel;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.VerticalPanel;
import com.google.gwt.user.client.ui.Widget;

public class OptionsWidget extends Composite
{
    private ColorChooserWidget colorChooser;
    
    private DockPanel panel;
    
    private VerticalPanel contentPanel;
    
    public OptionsWidget()
    {
        colorChooser = new ColorChooserWidget();
        colorChooser.addColorListener(new ColorListener()
        {
            
            @Override
            public void onChoice(String color)
            {
                doSelectColor(color);
            }
        });
        panel = new DockPanel();
        initWidget(panel);
        setWidth("100%");
        setHeight("100%");
        Button resetButton = new Button("Reset");
        resetButton.addClickHandler(new ClickHandler()
        {
            
            @Override
            public void onClick(ClickEvent event)
            {
                OthelloGadget.confirmReset();
            }
        });
        panel.add(resetButton, panel.EAST);
        panel.setCellHorizontalAlignment(resetButton, panel.ALIGN_RIGHT);
        contentPanel = new VerticalPanel();
        panel.add(contentPanel, panel.WEST);
        panel.setCellHorizontalAlignment(contentPanel, panel.ALIGN_CENTER);
        panel.setCellVerticalAlignment(contentPanel, panel.ALIGN_MIDDLE);
        contentPanel.setSpacing(3);
    }
    
    public void refresh()
    {
        State props = OthelloGadget.wave.getState();
        contentPanel.clear();
        contentPanel.add(buildPlayerPanel(props));
        String ourPlayerId = OthelloGadget.wave.getViewer().getId();
        boolean weHaveJoined = ourPlayerId.equals(props.get("player1"))
                || ourPlayerId.equals(props.get("player2"));
        int ourPlayerNumber = (ourPlayerId.equals(props.get("player1")) ? 1 : 2);
        boolean notYetJoined = !weHaveJoined;
        boolean twoPeopleJoined = (props.get("player1") != null)
                && (props.get("player2") != null);
        if (props.get("player1") == null && notYetJoined)
            contentPanel.add(createJoinAsPlayer(1));
        if (props.get("player2") == null && notYetJoined)
            contentPanel.add(createJoinAsPlayer(2));
        if (weHaveJoined)
            contentPanel.add(createLeaveTheGame(ourPlayerNumber));
        if (twoPeopleJoined && weHaveJoined)
            contentPanel.add(createStartTheGame());
        if (weHaveJoined)
        {
            panel.add(new Label("You can change your bead color if you want:"));
            panel.add(colorChooser);
        }
    }
    
    private Widget createStartTheGame()
    {
        Button button = new Button("Start the game");
        button.addClickHandler(new ClickHandler()
        {
            
            @Override
            public void onClick(ClickEvent event)
            {
                Window.alert("This would normally start the game.");
            }
        });
        return button;
    }
    
    private Widget createLeaveTheGame(final int ourPlayerNumber)
    {
        Button button = new Button("Leave the game");
        button.addClickHandler(new ClickHandler()
        {
            
            @Override
            public void onClick(ClickEvent event)
            {
                HashMap<String, String> delta = new HashMap<String, String>();
                delta.put("player" + ourPlayerNumber, null);
                delta.put("color" + ourPlayerNumber, null);
                OthelloGadget.wave.getState().submitDelta(delta);
            }
        });
        return button;
    }
    
    private Widget createJoinAsPlayer(final int playerNumber)
    {
        Button button = new Button("Join as " + (playerNumber == 1 ? "first" : "second")
                + " player");
        button.addClickHandler(new ClickHandler()
        {
            
            @Override
            public void onClick(ClickEvent event)
            {
                HashMap<String, String> delta = new HashMap<String, String>();
                delta.put("player" + playerNumber, OthelloGadget.wave.getViewer().getId());
                delta.put("color" + playerNumber, getDefaultColor(playerNumber));
                OthelloGadget.wave.getState().submitDelta(delta);
            }
        });
        return button;
    }
    
    protected String getDefaultColor(int playerNumber)
    {
        if (playerNumber == 1)
            return "000000";
        else
            return "ffffff";
    }
    
    public HorizontalPanel buildPlayerPanel(State props)
    {
        HorizontalPanel content = new HorizontalPanel();
        boolean hasFirst = props.get("player1") != null;
        if (hasFirst)
        {
            PlayerWidget widget = new PlayerWidget(props.get("player1"));
            widget.setColor(props.get("color1"));
            content.add(widget);
        }
        else
        {
            content.add(new HTML("&nbsp;No first player&nbsp;"));
        }
        boolean hasSecond = props.get("player2") != null;
        if (hasSecond)
        {
            PlayerWidget widget = new PlayerWidget(props.get("player2"));
            widget.setColor(props.get("color2"));
            content.add(widget);
        }
        else
        {
            content.add(new HTML("&nbsp;No second player&nbsp;"));
        }
        return content;
    }
    
    protected void doSelectColor(String color)
    {
        String ourPlayerId = OthelloGadget.wave.getViewer().getId();
        State props = OthelloGadget.wave.getState();
        boolean weHaveJoined = ourPlayerId.equals(props.get("player1"))
                || ourPlayerId.equals(props.get("player2"));
        int ourPlayerNumber = (ourPlayerId.equals(props.get("player1")) ? 1 : 2);
        if (weHaveJoined)
        {
            OthelloGadget.wave.getState().submitValue("color" + ourPlayerNumber, color);
        }
    }
}
