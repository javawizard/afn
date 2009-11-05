package jw.othello.client;

import jw.othello.client.Board.CaptureResult;

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
    
    int exampleCurrentPlayer = 2;
    
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
                board.cellAt(3, 3).setValue(1);
                board.cellAt(4, 4).setValue(1);
                board.cellAt(3, 4).setValue(2);
                board.cellAt(4, 3).setValue(2);
                BoardWidget widget = new BoardWidget(board);
                widget.refresh();
                RootPanel.get().add(widget);
                widget.addBoardListener(new BoardListener()
                {
                    
                    @Override
                    public void cellClicked(CellWidget cell)
                    {
                        exampleCurrentPlayer = (exampleCurrentPlayer == 1 ? 2 : 1);
                        CaptureResult result = cell.getBoard().capture(
                                exampleCurrentPlayer, cell.getCell(), true);
                        cell.getBoardWidget().refresh();
                        if (result == CaptureResult.nocapture)
                            Window.alert("You can't capture any beads if you "
                                    + "place a bead there.");
                        else if (result == CaptureResult.occupied)
                            Window.alert("There is already a bead at that location.");
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
