package jw.othello.client;

public class Board
{
    private int[][] cells = new int[8][8];
    private String player1;
    private String color1;
    private String player2;
    private String color2;
    public int getCell(int row, int col)
    {
        return cells[row][col];
    }
    
    public void setCell(int row, int col, int value)
    {
        cells[row][col] = value;
    }
    
    public void parse(String from)
    {
        
    }
    
    public String format()
    {
        
    }
}
