package jw.othello.client;

import com.google.gwt.user.client.ui.Composite;

/**
 * The game widget shows a game in progress, with scores and players to the right of the
 * board, and information about the game below the board. It also shows a reset button
 * which delegates to OthelloGadget. This widget is the widget that shows whenever a game
 * is in progress or has finished but has not yet been reset. The widget itself takes care
 * of listening for board events and updating state accordingly.
 * 
 * @author Alexander Boyd
 * 
 */
public class GameWidget extends Composite
{
    private Board board;
    private BoardWidget boardWidget;
    
    public GameWidget()
    {
        
    }
}
