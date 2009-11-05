package jw.othello.client;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.user.client.ui.ClickListener;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.FocusPanel;
import com.google.gwt.user.client.ui.HTML;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.Widget;

public class CellWidget extends Composite
{
    private int state = -1;
    private BoardWidget boardWidget;
    private Board board;
    private Cell cell;
    
    public BoardWidget getBoardWidget()
    {
        return boardWidget;
    }
    
    public Board getBoard()
    {
        return board;
    }
    
    public Cell getCell()
    {
        return cell;
    }
    
    private FocusPanel panel;
    private HTML label = new HTML("...");
    
    public CellWidget(int row, int col, BoardWidget boardWidget, Board board)
    {
        this.boardWidget = boardWidget;
        this.board = board;
        this.cell = board.cellAt(row, col);
        if (this.cell == null)
            throw new RuntimeException("Cell for row " + row + " col " + col + " is null");
        panel = new FocusPanel();
        panel.addStyleName("othello-no-borders");
        panel.setWidget(label);
        panel.addClickHandler(new ClickHandler()
        {
            
            @Override
            public void onClick(ClickEvent event)
            {
                panel.setFocus(false);
            }
        });
        initWidget(panel);
    }
    
    public void addBoardListener(final BoardListener listener)
    {
        panel.addClickHandler(new ClickHandler()
        {
            
            @Override
            public void onClick(ClickEvent event)
            {
                listener.cellClicked(CellWidget.this);
            }
        });
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
