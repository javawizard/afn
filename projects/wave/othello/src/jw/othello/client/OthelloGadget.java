package jw.othello.client;

import java.util.HashMap;

import jw.othello.client.Board.CaptureResult;

import org.cobogw.gwt.waveapi.gadget.client.NeedsWave;
import org.cobogw.gwt.waveapi.gadget.client.ParticipantUpdateEvent;
import org.cobogw.gwt.waveapi.gadget.client.ParticipantUpdateEventHandler;
import org.cobogw.gwt.waveapi.gadget.client.State;
import org.cobogw.gwt.waveapi.gadget.client.StateUpdateEvent;
import org.cobogw.gwt.waveapi.gadget.client.StateUpdateEventHandler;
import org.cobogw.gwt.waveapi.gadget.client.WaveFeature;

import com.google.gwt.core.client.GWT;
import com.google.gwt.core.client.JsArrayString;
import com.google.gwt.core.client.GWT.UncaughtExceptionHandler;
import com.google.gwt.gadgets.client.Gadget;
import com.google.gwt.gadgets.client.UserPreferences;
import com.google.gwt.gadgets.client.Gadget.ModulePrefs;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.ClickListener;
import com.google.gwt.user.client.ui.FocusWidget;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.RootPanel;
import com.google.gwt.user.client.ui.SimplePanel;
import com.google.gwt.user.client.ui.Widget;

@ModulePrefs(author = "Alexander Boyd", height = 350, title = "Othello")
@SuppressWarnings("deprecation")
public class OthelloGadget extends Gadget<UserPreferences> implements NeedsWave,
        StateUpdateEventHandler, ParticipantUpdateEventHandler
{
    public static OthelloGadget singleton;
    
    static WaveFeature wave;
    
    public Label initialLoadingLabel = new Label(
            "Loading Othello... (If you've just switched here from playback mode, "
                    + "switch to another wave and then switch back to this wave. "
                    + "This is a bug I'm currently working on.)");
    
    public SimplePanel container = new SimplePanel();
    public GameWidget currentGameWidget;
    public OptionsWidget currentOptionsWidget;
    
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
    
    int exampleCurrentPlayer = 2;
    
    private void maybeInitialize()
    {
        if (hasState && hasParticipants && !initialized)
        {
            try
            {
                initialized = true;
                RootPanel.get().clear();
                RootPanel.get().add(container);
                container.setWidth("100%");
                container.setHeight("100%");
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
        State props = wave.getState();
        String state = props.get("state");
        if (state != null)
        {
            currentOptionsWidget = null;
            if (currentGameWidget == null)
            {
                currentGameWidget = new GameWidget();
                container.setWidget(currentGameWidget);
            }
            currentGameWidget.refresh();
        }
        else
        {
            currentGameWidget = null;
            if (currentOptionsWidget == null)
            {
                currentOptionsWidget = new OptionsWidget();
                container.setWidget(currentOptionsWidget);
            }
            currentOptionsWidget.refresh();
        }
    }
    
    private void reloadParticipants()
    {
        /*
         * If we're showing options, and a player isn't on the wave anymore, we remove
         * them. If we're showing the board, we don't do anything, and rely on the user
         * resetting the game.
         */
    }
    
    public static void resetGame()
    {
        JsArrayString keys = wave.getState().getKeys();
        HashMap<String, String> delta = new HashMap<String, String>();
        for (int i = 0; i < keys.length(); i++)
        {
            delta.put(keys.get(i), null);
        }
        wave.getState().submitDelta(delta);
    }
    
    public static void confirmReset()
    {
        if (Window.confirm("Are you sure you want to reset the game?"))
            resetGame();
    }
    
    public static void disableIfPlayback(FocusWidget button)
    {
        if (wave.isPlayback())
            button.setEnabled(false);
    }
}
