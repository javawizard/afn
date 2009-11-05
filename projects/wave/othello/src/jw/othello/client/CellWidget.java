package jw.othello.client;

import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.Label;

public class CellWidget extends Composite
{
    private int state = -1;
    private BoardWidget boardWidget;
    private Board board;
    private Cell cell;
    private Label label = new Label("...");
    
    public CellWidget(int row, int col, BoardWidget boardWidget, Board board)
    {
        this.boardWidget = boardWidget;
        this.board = board;
        this.cell = board.cellAt(row, col);
        initWidget(label);
    }
    
    public void refresh()
    {
        int newState = cell.getValue();
        if (newState != state)
        {
            state = newState;
            forceRefresh();
        }
    }
    
    public void forceRefresh()
    {
        
    }
}
