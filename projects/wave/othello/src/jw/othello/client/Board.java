package jw.othello.client;

import java.util.HashMap;

import com.google.gwt.user.client.Window;

public class Board
{
    public static enum Direction
    {
        north(-1, 0), northeast(-1, 1), east(0, 1), southeast(1, 1), south(1, 0), southwest(
                1, -1), west(0, -1), northwest(-1, -1);
        private int dr;
        private int dc;
        
        private Direction(int dr, int dc)
        {
            this.dr = dr;
            this.dc = dc;
        }
        
        public int getDeltaR()
        {
            return dr;
        }
        
        public int getDeltaC()
        {
            return dc;
        }
    }
    
    private Cell[][] cells = new Cell[8][8];
    {
        for (int row = 0; row < 8; row++)
        {
            for (int col = 0; col < 8; col++)
            {
                cells[row][col] = new Cell(this, row, col);
            }
        }
    }
    
    private String player1;
    private String color1;
    private String player2;
    private String color2;
    
    /**
     * Loads this board from the values in the specified wave state map.
     * 
     * @param from
     */
    public void parse(HashMap<String, String> from)
    {
        for (int row = 0; row < 8; row++)
        {
            for (int col = 0; col < 8; col++)
            {
                // TODO: split the name out into a method, so that the format of the
                // properties could be changed just by changing the method
                String name = "board-" + row + "-" + col;
                if (!from.containsKey(name))
                    cellAt(row, col).setValue(0);
                else
                {
                    try
                    {
                        cellAt(row, col).setValue(Integer.parseInt(from.get(name)));
                    }
                    catch (NumberFormatException e)
                    {
                        Window.alert("Incorrect number formatting for row " + row + " col "
                                + col + " with value \"" + from.get(name)
                                + "\". The cell will not be modified.");
                    }
                }
            }
        }
        player1 = from.get("board-player1");
        color1 = from.get("board-color1");
        player2 = from.get("board-player2");
        color2 = from.get("board-color2");
    }
    
    public String getPlayer1()
    {
        return player1;
    }
    
    public String getColor1()
    {
        return color1;
    }
    
    public String getPlayer2()
    {
        return player2;
    }
    
    public String getColor2()
    {
        return color2;
    }
    
    /**
     * Stores this board to the specified wave state map.
     * 
     * @param original
     *            The current view of the map, which is used to calculate what data has
     *            changed
     * @param to
     *            The map to write data to, which should probably be blank. Only keys that
     *            do not have the same value as those in <tt>original</tt> will be added
     *            to <tt>to</tt>.
     */
    public void format(HashMap<String, String> original, HashMap<String, String> to,
            boolean players)
    {
        for (int row = 0; row < 8; row++)
        {
            for (int col = 0; col < 8; col++)
            {
                String name = "board-" + row + "-" + col;
                int value = cellAt(row, col).getValue();
                if (value == 0)
                {
                    if (original.containsKey(name))
                        to.put(name, null);
                }
                else
                {
                    if ((!original.containsKey(name))
                            || value != Integer.parseInt(original.get(name)))// if we
                    // don't have the key, or we do have the key but it's not the correct
                    // value
                    {
                        to.put(name, "" + value);
                    }
                }
            }
        }
        if (players)
        {
            addIfChanged(original, to, "board-player1", player1);
            addIfChanged(original, to, "board-color1", color1);
            addIfChanged(original, to, "board-player2", player2);
            addIfChanged(original, to, "board-color2", color2);
        }
    }
    
    private void addIfChanged(HashMap<String, String> original, HashMap<String, String> to,
            String name, String value)
    {
        String originalValue = original.get(name);
        if (originalValue == null && value == null)
            return;
        else if (originalValue == null && value != null)
            to.put(name, value);
        else if (originalValue != null && value == null)
            to.put(name, null);
        else
        {
            if (!originalValue.equals(value))
                to.put(name, value);
        }
    }
    
    /**
     * Returns the cell at the specified row and column. An ArrayIndexOutOfBoundsException
     * will be thrown if there is no such cell.
     * 
     * @param row
     *            The 0-based row
     * @param col
     *            The 0-based column
     * @return The cell at the specified position
     */
    public Cell cellAt(int row, int col)
    {
        return cells[row][col];
    }
}
