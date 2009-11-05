package jw.othello.client;

import com.google.gwt.gadgets.client.Gadget;
import com.google.gwt.gadgets.client.UserPreferences;
import com.google.gwt.gadgets.client.Gadget.ModulePrefs;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.RootPanel;

@ModulePrefs(author = "Alexander Boyd", height = 100, title = "Othello")
public class OthelloGadget extends Gadget<UserPreferences>
{
    
    @Override
    protected void init(UserPreferences preferences)
    {
        RootPanel.get().add(new Label("Hello world!!!!"));
    }
    
}
