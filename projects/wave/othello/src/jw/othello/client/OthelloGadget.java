package jw.othello.client;

import org.cobogw.gwt.waveapi.gadget.client.NeedsWave;
import org.cobogw.gwt.waveapi.gadget.client.ParticipantUpdateEvent;
import org.cobogw.gwt.waveapi.gadget.client.ParticipantUpdateEventHandler;
import org.cobogw.gwt.waveapi.gadget.client.StateUpdateEvent;
import org.cobogw.gwt.waveapi.gadget.client.StateUpdateEventHandler;
import org.cobogw.gwt.waveapi.gadget.client.WaveFeature;

import com.google.gwt.gadgets.client.Gadget;
import com.google.gwt.gadgets.client.UserPreferences;
import com.google.gwt.gadgets.client.Gadget.ModulePrefs;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.ClickListener;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.RootPanel;
import com.google.gwt.user.client.ui.Widget;

@ModulePrefs(author = "Alexander Boyd", height = 230, title = "Othello")
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
            initialized = true;
            RootPanel.get().clear();
            Board board = new Board();
            BoardWidget widget = new BoardWidget(board);
            RootPanel.get().add(widget);
        }
    }
    
    private void reloadState()
    {
    }
    
    private void reloadParticipants()
    {
    }
    
}
