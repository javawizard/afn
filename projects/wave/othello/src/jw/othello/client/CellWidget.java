package jw.othello.client;

import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.HTML;
import com.google.gwt.user.client.ui.Label;

public class CellWidget extends Composite
{
    private int state = -1;
    private BoardWidget boardWidget;
    private Board board;
    private Cell cell;
    private HTML label = new HTML("...");
    
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
        label.setHTML("<img src=\"" + generateUrl() + "\"/>");
    }
    
    private String generateUrl()
    {
        if (state == 0)
            return BeadUtils.generateFilledBoxUrl(boardWidget.getCellWidth(), boardWidget
                    .getCellHeight(), boardWidget.getCellBackground(), boardWidget
                    .getCellBorder(), "tl");
        String playerColor = (state == 1 ? board.getColor1() : board.getColor2());
        return BeadUtils.generateUrl(boardWidget.getCellWidth(), boardWidget
                .getCellHeight(), boardWidget.getCellBackground(), playerColor, boardWidget
                .getCellOutline(), boardWidget.getCellBorder(), "tl");
    }
}
