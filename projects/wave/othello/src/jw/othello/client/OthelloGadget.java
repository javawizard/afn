package jw.othello.client;

import org.cobogw.gwt.waveapi.gadget.client.NeedsWave;
import org.cobogw.gwt.waveapi.gadget.client.ParticipantUpdateEvent;
import org.cobogw.gwt.waveapi.gadget.client.ParticipantUpdateEventHandler;
import org.cobogw.gwt.waveapi.gadget.client.StateUpdateEvent;
import org.cobogw.gwt.waveapi.gadget.client.StateUpdateEventHandler;
import org.cobogw.gwt.waveapi.gadget.client.WaveFeature;

import com.google.gwt.core.client.GWT;
import com.google.gwt.core.client.GWT.UncaughtExceptionHandler;
import com.google.gwt.gadgets.client.Gadget;
import com.google.gwt.gadgets.client.UserPreferences;
import com.google.gwt.gadgets.client.Gadget.ModulePrefs;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.ClickListener;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.RootPanel;
import com.google.gwt.user.client.ui.Widget;

@ModulePrefs(author = "Alexander Boyd", height = 350, title = "Othello")
@SuppressWarnings("deprecation")
public class OthelloGadget extends Gadget<UserPreferences> implements NeedsWave,
        StateUpdateEventHandler, ParticipantUpdateEventHandler
{
    public static OthelloGadget singleton;
    
    private WaveFeature wave;
    
    public Label initialLoadingLabel = new Label("Loading Othello...");
    
    public static boolean hasParticipants = false;
    public static boolean hasState = false;
    public static boolean initialized = false;
    
    @Override
    protected void init(UserPreferences preferences)
    {
        GWT.setUncaughtExceptionHandler(new UncaughtExceptionHandler()
        {
            
            @Override
            public void onUncaughtException(Throwable e)
            {
                Window.alert("Uncaught exception: " + e.getClass().getName() + ": "
                        + e.getMessage());
                Window.alert("Details: " + e);
            }
        });
        singleton = this;
        RootPanel.get().add(initialLoadingLabel);
        wave.addParticipantUpdateEventHandler(this);
        wave.addStateUpdateEventHandler(this);
    }
    
    @Override
    public void initializeFeature(WaveFeature feature)
    {
        this.wave = feature;
    }
    
    @Override
    public void onUpdate(StateUpdateEvent event)
    {
        hasState = true;
        maybeInitialize();
        if (initialized)
            reloadState();
    }
    
    @Override
    public void onUpdate(ParticipantUpdateEvent event)
    {
        hasParticipants = true;
        maybeInitialize();
        if (initialized)
            reloadParticipants();
    }
    
    private void maybeInitialize()
    {
        if (hasState && hasParticipants && !initialized)
        {
            try
            {
                initialized = true;
                RootPanel.get().clear();
                Board board = new Board();
                board.setColor1("00ff00");
                board.setColor2("0077ff");
                board.cellAt(4, 6).setValue(1);
                board.cellAt(4, 7).setValue(2);
                board.cellAt(0, 3).setValue(2);
                board.cellAt(7, 2).setValue(1);
                BoardWidget widget = new BoardWidget(board);
                widget.refresh();
                RootPanel.get().add(widget);
                widget.addBoardListener(new BoardListener()
                {
                    
                    @Override
                    public void cellClicked(CellWidget cell)
                    {
                        int value = cell.getCell().getValue();
                        value += 1;
                        value = value % 3;
                        cell.getCell().setValue(value);
                        cell.getBoardWidget().refresh();
                    }
                });
            }
            catch (Exception e)
            {
                Window.alert("Uncaught exception: " + e.getClass().getName() + ": "
                        + e.getMessage());
                Window.alert("Details: " + e);
            }
        }
    }
    
    private void reloadState()
    {
    }
    
    private void reloadParticipants()
    {
    }
    
}
