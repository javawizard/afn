package jw.othello.client;

import org.cobogw.gwt.waveapi.gadget.client.Participant;
import org.cobogw.gwt.waveapi.gadget.client.State;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.DockPanel;
import com.google.gwt.user.client.ui.FlexTable;
import com.google.gwt.user.client.ui.HorizontalPanel;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.VerticalPanel;
import com.google.gwt.user.client.ui.FlexTable.FlexCellFormatter;

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
    private FlexTable table;
    private VerticalPanel playerListPanel;
    private Label statusLabel = new Label();
    private PlayerWidget playerWidget1;
    private PlayerWidget playerWidget2;
    private Label playerStatus1 = new Label();
    private Label playerStatus2 = new Label();
    private int currentPlayer = 1;
    private int ourPlayerNumber = -1;
    
    public GameWidget()
    {
        table = new FlexTable();
        FlexCellFormatter format = table.getFlexCellFormatter();
        initWidget(table);
        setWidth("100%");
        setHeight("100%");
        board = new Board();
        boardWidget = new BoardWidget(board);
        playerListPanel = new VerticalPanel();
        playerListPanel.setSpacing(3);
        table.setWidget(0, 1, playerListPanel);
        format.setRowSpan(0, 1, 2);
        format.setHorizontalAlignment(0, 1, HorizontalPanel.ALIGN_RIGHT);
        table.setWidget(0, 0, boardWidget);
        format.setHorizontalAlignment(0, 0, HorizontalPanel.ALIGN_LEFT);
        format.setVerticalAlignment(0, 0, HorizontalPanel.ALIGN_BOTTOM);
        table.setWidget(1, 0, statusLabel);
        format.setHorizontalAlignment(1, 0, HorizontalPanel.ALIGN_CENTER);
        reloadPlayerListPanel();
        boardWidget.addBoardListener(new BoardListener()
        {
            
            @Override
            public void cellClicked(CellWidget cell)
            {
                boardClicked(cell);
            }
        });
    }
    
    protected void boardClicked(CellWidget cell)
    {
        if (ourPlayerNumber == -1)
        {
            Window.alert("You're not playing in this game.");
            return;
        }
    }
    
    private void reloadPlayerListPanel()
    {
        playerListPanel.clear();
        Button resetButton = new Button("Reset");
        resetButton.addClickHandler(new ClickHandler()
        {
            
            @Override
            public void onClick(ClickEvent event)
            {
                OthelloGadget.confirmReset();
            }
        });
        playerListPanel.add(resetButton);
        State props = OthelloGadget.wave.getState();
        String player1 = props.get("player1");
        String color1 = props.get("color1");
        String player2 = props.get("player2");
        String color2 = props.get("color2");
        playerWidget1 = new PlayerWidget(player1);
        playerWidget1.setColor(color1);
        playerWidget2 = new PlayerWidget(player2);
        playerWidget2.setColor(color2);
        playerListPanel.add(playerWidget1);
        playerListPanel.add(playerStatus1);
        playerListPanel.add(playerWidget2);
        playerListPanel.add(playerStatus2);
    }
    
    public void refresh()
    {
        State props = OthelloGadget.wave.getState();
        String state = props.get("state");
        /*
         * First we'll refresh the board and the beads on the board.
         */
        board.parse(props);
        boardWidget.refresh();
        /*
         * Next we'll refresh the current player.
         */
        currentPlayer = Integer.parseInt(props.get("player"));
        if (currentPlayer == 1)
        {
            playerWidget1.setActive(true);
            playerWidget2.setActive(false);
        }
        else if (currentPlayer == 2)
        {
            playerWidget2.setActive(true);
            playerWidget1.setActive(false);
        }
        else
        {
            /*
             * This will happen if it's endgame and the players tied
             */
            playerWidget1.setActive(false);
            playerWidget2.setActive(false);
        }
        /*
         * Next we'll refresh the player bead counts.
         */
        int playerCount1 = board.getCellCount(1);
        int playerCount2 = board.getCellCount(2);
        playerStatus1.setText("" + playerCount1 + " bead" + (playerCount1 == 1 ? "" : "s"));
        playerStatus2.setText("" + playerCount2 + " bead" + (playerCount2 == 1 ? "" : "s"));
        /*
         * Now we'll refresh the status. If it's endgame, the status indicates who won or
         * if itwas a tie. If it's not, the status indicates the next player.
         */
        if (state.equals("over"))
        {
            if (currentPlayer == 0)
                statusLabel.setText("The game was a tie!");
            else
            {
                String playerId = props.get("player" + currentPlayer);
                Participant player = OthelloGadget.wave.getParticipantById(playerId);
                String name = playerId;
                if (player != null)
                    name = player.getDisplayName();
                statusLabel.setText(name + " wins!");
            }
        }
        else
        {
            String playerId = props.get("player" + currentPlayer);
            Participant player = OthelloGadget.wave.getParticipantById(playerId);
            String name = playerId;
            if (player != null)
                name = player.getDisplayName();
            statusLabel.setText(name + "'s turn.");
        }
        /*
         * Now we see if we're a player.
         */
        String ourId = OthelloGadget.wave.getViewer().getId();
        if (ourId.equals(props.get("player1")))
        {
            ourPlayerNumber = 1;
        }
        else if (ourId.equals(props.get("player2")))
        {
            ourPlayerNumber = 2;
        }
        else
        {
            ourPlayerNumber = -1;
        }
    }
}
