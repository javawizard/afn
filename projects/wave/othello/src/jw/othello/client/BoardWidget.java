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
        grid.setCellPadding(0);
        grid.setCellSpacing(0);
        grid.setBorderWidth(0);
        grid.addStyleName("othello-board-grid");
        for (int row = 0; row < 8; row++)
        {
            for (int col = 0; col < 8; col++)
            {
                CellWidget cellWidget = new CellWidget(row, col, this, board);
                grid.setWidget(row, col, cellWidget);
                cells[row][col] = cellWidget;
            }
        }
        initWidget(grid);
    }
    
    public void refresh()
    {
        for (int r = 0; r < 8; r++)
        {
            for (int c = 0; c < 8; c++)
            {
                cells[r][c].refresh();
            }
        }
    }
    
    public void forceRefresh()
    {
        for (int r = 0; r < 8; r++)
        {
            for (int c = 0; c < 8; c++)
            {
                cells[r][c].forceRefresh();
            }
        }
    }
    
    public int getCellWidth()
    {
        return 35;
    }
    
    public String getCellBackground()
    {
        return "ffffff";
    }
    
    public int getCellHeight()
    {
        return 35;
    }
    
    public String getCellOutline()
    {
        return "000000";
    }
    
    public String getCellBorder()
    {
        return "000000";
    }
}
