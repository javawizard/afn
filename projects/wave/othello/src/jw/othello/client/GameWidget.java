package jw.othello.client;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.DockPanel;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.VerticalPanel;

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
    private DockPanel dock;
    private VerticalPanel playerListPanel;
    private Label statusLabel = new Label();
    
    public GameWidget()
    {
        dock = new DockPanel();
        initWidget(dock);
        board = new Board();
        boardWidget = new BoardWidget(board);
        playerListPanel = new VerticalPanel();
        dock.add(playerListPanel, dock.EAST);
        dock.setCellHorizontalAlignment(playerListPanel, dock.ALIGN_RIGHT);
        dock.add(boardWidget, dock.WEST);
        dock.setCellHorizontalAlignment(boardWidget, dock.ALIGN_LEFT);
        dock.setCellVerticalAlignment(boardWidget, dock.ALIGN_BOTTOM);
        dock.add(statusLabel, dock.SOUTH);
        dock.setCellHorizontalAlignment(statusLabel, dock.ALIGN_CENTER);
        initPlayerListPanel();
    }
    
    private void initPlayerListPanel()
    {
        Button resetButton = new Button("Reset");
        resetButton.addClickHandler(new ClickHandler()
        {

            @Override
            public void onClick(ClickEvent event)
            {
                OthelloGadget.confirmReset();
            }
        });
    }

    public void refresh()
    {
        // TODO Auto-generated method stub
        
    }
}
