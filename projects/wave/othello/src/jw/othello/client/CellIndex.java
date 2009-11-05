package jw.othello.client;

public class CellIndex
{
    private int row;
    private int col;
    
    public int getRow()
    {
        return row;
    }
    
    public void setRow(int row)
    {
        this.row = row;
    }
    
    public CellIndex()
    {
        super();
    }
    
    public CellIndex(int col, int row)
    {
        super();
        this.col = col;
        this.row = row;
    }
    
    public int getCol()
    {
        return col;
    }
    
    public void setCol(int col)
    {
        this.col = col;
    }
}
