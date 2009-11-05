package jw.othello.client;

import java.util.ArrayList;
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
     * Represents the results of an attempt to capture beads.
     * 
     * @author Alexander Boyd
     * 
     */
    public static enum CaptureResult
    {
        /**
         * Indicates that beads could not be captured because there wasn't any possible
         * direction in which beads could be captured.
         */
        nocapture,
        /**
         * Indicates that beads could not be captured because there is already a bead at
         * the location that we're trying to capture from.
         */
        occupied,
        /**
         * Indicates that beads were successfully captured.
         */
        captured
    }
    
    /**
     * Places a bead at the location specified and attempts to capture beads. If beads
     * cannot be captured from the requested location, the board will not be modified.
     * This method correctly handles the case where there is already a bead at the
     * specified location.
     * 
     * @param player
     *            The player that is attempting to place the bead
     * @param cell
     *            The cell that the bead is to be placed on
     * @param capture
     *            True to actually perform this capture, false to simulate the capture but
     *            not actually perform it. This is useful to figure out if a player is
     *            able to capture at the specified location without actually causing the
     *            board to change state.
     * @return The result of the capture
     */
    public CaptureResult capture(int player, Cell cell, boolean capture)
    {
        /*
         * First we'll make sure there isn't already a bead at that location.
         */
        if (cell.getValue() != 0)
            return CaptureResult.occupied;
        /*
         * Now we'll go through each of the cardinal and intermediate directions and
         * attempt to capture beads.
         */
        boolean beadsWereCaptured = false;
        for (Direction direction : Direction.values())
        {
            /*
             * First, we'll create a list that will hold all of the opponent cells we
             * encounter while attempting to capture. We'll need this list of cells in
             * case the capture goes successfully, so that we can change all of the
             * opponent cells to be our cell color.
             */
            ArrayList<Cell> opponentCells = new ArrayList<Cell>();
            /*
             * Now we'll start at the cell immediately in the specified direction, and
             * head that way.
             */
            Cell current = cell.next(direction);
            while (current != null)
            {
                /*
                 * First, we check to see what type of cell this is.
                 */
                int currentValue = current.getValue();
                if (currentValue == 0)
                {
                    /*
                     * If the cell is blank, we've run out of cells, which means we can't
                     * capture.
                     */
                    break;
                }
                else if (currentValue == player)
                {
                    /*
                     * This is one of our cells. If we have at least one opponent cell on
                     * the list, then we can capture all of the opponent cells. If not, we
                     * can't capture in this direction.
                     */
                    if (opponentCells.size() > 0)
                    {
                        beadsWereCaptured = true;
                        /*
                         * Now we'll do the actual capturing, if we're supposed to.
                         */
                        if (capture)
                            doCaptureBeads(player, opponentCells);
                    }
                    else
                    {
                        break;
                    }
                }
                else
                {
                    /*
                     * This is one of our opponent's cells. We'll just add it to the
                     * opponent cell list.
                     */
                    opponentCells.add(current);
                }
                /*
                 * Now we move to the next cell.
                 */
                current = current.next(direction);
            }
            /*
             * ...and that's it for this direction.
             */
        }
        /*
         * Now we see if we captured anything. If we did, we place a player bead at the
         * cell, and return.
         */
        if (beadsWereCaptured)
        {
            if (capture)
                cell.setValue(player);
            return CaptureResult.captured;
        }
        /*
         * If we didn't capture, we return a value indicating such.
         */
        return CaptureResult.nocapture;
    }
    
    /**
     * Sets all of the specified cells to contain the player specified's beads.
     * 
     * @param player
     *            The player who is placing these beads
     * @param cells
     *            The cells to capture
     */
    private void doCaptureBeads(int player, ArrayList<Cell> cells)
    {
        for (Cell cell : cells)
        {
            cell.setValue(player);
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
    
    public void setPlayer1(String player1)
    {
        this.player1 = player1;
    }
    
    public void setColor1(String color1)
    {
        this.color1 = color1;
    }
    
    public void setPlayer2(String player2)
    {
        this.player2 = player2;
    }
    
    public void setColor2(String color2)
    {
        this.color2 = color2;
    }
}
