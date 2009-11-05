package jw.othello.client;

import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.Grid;

public class BoardWidget extends Composite
{
    private Grid grid;
    private CellWidget[][] cells = new CellWidget[8][8];
    private Board board;
    
    public BoardWidget(Board board)
    {
        this.board = board;
        grid = new Grid(8, 8);
        grid.addStyleName("othello-board-grid");
        for (int row = 0; row < 8; row++)
        {
            for (int col = 0; col < 8; col++)
            {
                CellWidget cellWidget = new CellWidget(row, col, this, board);
                grid.setWidget(row, col, cellWidget);
            }
        }
    }
    
    public void refresh()
    {
        for (CellWidget[] row : cells)
        {
            for (CellWidget cell : row)
            {
                cell.refresh();
            }
        }
    }
    
    public void forceRefresh()
    {
        for (CellWidget[] row : cells)
        {
            for (CellWidget cell : row)
            {
                cell.forceRefresh();
            }
        }
    }
}
