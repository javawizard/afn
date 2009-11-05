package jw.othello.client;

import org.cobogw.gwt.waveapi.gadget.client.NeedsWave;
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

@ModulePrefs(author = "Alexander Boyd", height = 100, title = "Othello")
@SuppressWarnings("deprecation")
public class OthelloGadget extends Gadget<UserPreferences> implements NeedsWave
{
    
    private WaveFeature wave;
    
    @Override
    protected void init(UserPreferences preferences)
    {
        Button button = new Button("Say hi");
        button.addClickListener(new ClickListener()
        {
            
            @Override
            public void onClick(Widget sender)
            {
                Window.alert("Hello world");
            }
        });
        RootPanel.get().add(button);
    }
    
    @Override
    public void initializeFeature(WaveFeature feature)
    {
        this.wave = feature;
    }
    
}
