package jw.othello.client;

import jw.othello.client.Board.Direction;

public class Cell
{
    private int row;
    public Board getBoard()
    {
        return board;
    }

    private int col;
    // TODO: consider changing this to a typesafe enum that has the values empty, first,
    // and second. Or maybe just first and second, and null would be empty.
    private int value;
    private Board board;
    
    /**
     * Gets the value of this cell. 0 means the cell is vacant, 1 means the cell is
     * occupied by a first-player bead, and 2 means the cell is occupied by a
     * second-player bead.
     * 
     * @return
     */
    public int getValue()
    {
        return value;
    }
    
    /**
     * Sets the value for this cell. See {@link #getValue()} for the values that can be
     * passed in.
     * 
     * @param value
     */
    public void setValue(int value)
    {
        this.value = value;
    }
    
    public int getRow()
    {
        return row;
    }
    
    public void setRow(int row)
    {
        this.row = row;
    }
    
    public Cell(Board board)
    {
        this.board = board;
    }
    
    public Cell(Board board, int row, int col)
    {
        this.col = col;
        this.row = row;
        this.board = board;
    }
    
    public int getCol()
    {
        return col;
    }
    
    public void setCol(int col)
    {
        this.col = col;
    }
    
    /**
     * Gets the cell in the direction specified.
     * 
     * @param direction
     * @return The cell, or null if there is no such cell. For example, there is no cell
     *         north of the cell at row 0, col 0, so this would return null for that cell
     *         and a direction of north.
     */
    public Cell next(Direction direction)
    {
        int newRow = row + direction.getDeltaR();
        int newCol = col + direction.getDeltaC();
        if (newRow < 0 || newRow >= 8)
            return null;
        if (newCol < 0 || newCol >= 8)
            return null;
        return board.cellAt(newRow, newCol);
    }
}
