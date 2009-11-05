package jw.othello.client;

import java.util.HashMap;

import com.google.gwt.user.client.Window;

public class Board
{
    public static enum Direction 
    {
        ;
        private int dr;
        private int dc;
        public 
    }
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
                    cells[row][col] = 0;
                else
                {
                    try
                    {
                        cells[row][col] = Integer.parseInt(from.get(name));
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
    public void format(HashMap<String, String> original, HashMap<String, String> to)
    {
        for (int row = 0; row < 8; row++)
        {
            for (int col = 0; col < 8; col++)
            {
                String name = "board-" + row + "-" + col;
                int value = cells[row][col];
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
    }
}
